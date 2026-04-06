#lang racket/base

;; Coverage-guided testing loop with batched coverage feedback.
;;
;; Instead of checking coverage after every test, we generate a batch
;; of inputs, run them all, then check coverage once for the whole
;; batch. This amortizes the snapshot cost and makes the guidance loop
;; nearly as fast as plain rackcheck.
;;
;; Uses only rackcheck's public API — no private submodule access.

(require racket/match
         racket/random
         racket/stream
         racket/list
         "../prop.rkt"
         "../gen/shrink-tree.rkt"
         "config.rkt"
         "coverage.rkt"
         "corpus.rkt"
         "mutation.rkt"
         "shrinking.rkt")

(provide
 (struct-out guided-result)
 run-guided)

(struct guided-result
  (status iterations counterexample shrunk exception
   corpus seed coverage-summary new-coverage-bits)
  #:transparent)

;; Main entry point for guided testing.
;; `target` can be:
;;   - a path-string? → creates an instrumented namespace for that module
;;   - a target-coverage-info? → uses the caller's pre-built coverage tracking
;;   - #f → no coverage guidance (just runs tests)
(define (run-guided gconfig p target)
  (match-define (guided-config max-iters max-time-ms pop-size
                               mutation-rate seed verbose?) gconfig)

  (define g (property-gen p))
  (define f (property-proc p))

  ;; Set up coverage tracking.
  (define-values (instrumented-ns tci)
    (cond
      [(target-coverage-info? target)
       (values #f target)]
      [(or (string? target) (path? target))
       (make-instrumented-namespace target)]
      [else
       (values #f #f)]))

  ;; RNG setup
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed))
  (define caller-rng (current-pseudo-random-generator))

  (define corp (make-corpus))
  (define start-time (current-inexact-milliseconds))
  (define total-new-bits 0)
  (define batch-size (max 1 (min pop-size 100)))

  ;; Run property on a list of arguments.
  (define (test-input args)
    (with-handlers ([exn:fail? (lambda (e) (values #f e))])
      (parameterize ([current-pseudo-random-generator caller-rng]
                     [current-namespace
                      (or instrumented-ns (current-namespace))])
        (if (apply f args)
            (values #t #f)
            (values #f #f)))))

  ;; Generate a fresh input from the property's generator.
  (define (generate-fresh size)
    (define tree (g rng size))
    (values (shrink-tree-val tree) tree))

  ;; Mutate a corpus entry's input.
  ;; Returns (values new-input parent-entry mutation-hint).
  ;; mutation-hint is (list 'position idx) or #f.
  (define (mutate-from-corpus)
    (define entry (corpus-pick corp rng))
    (cond
      [entry
       (set-box! (corpus-entry-offspring-count entry)
                 (add1 (unbox (corpus-entry-offspring-count entry))))
       (define old-input (corpus-entry-input entry))
       (define hint (corpus-entry-mutation-hint entry))
       ;; If the parent has a mutation hint, use position-biased mutation
       ;; 70% of the time. Otherwise use random mutation.
       (define use-hint?
         (and hint
              (list? hint)
              (eq? (car hint) 'position)
              (< (random rng) 0.7)))
       ;; Input is always a list of argument values. Preserve arity.
       ;; Choose mutation strategy:
       ;;   40% dictionary-based (insert target constants)
       ;;   30% value mutation (random perturbation)
       ;;   30% structural (for list-of-lists inputs)
       (define input-len (if (list? old-input) (length old-input) 1))
       (define dict (if tci (target-coverage-info-dictionary tci) '()))
       (define strategy-roll (random rng))
       (define-values (new-input new-hint)
         (cond
           ;; Dictionary-based mutation (40% when dictionary available)
           [(and (< strategy-roll 0.4) (not (null? dict))
                 (list? old-input) (not (null? old-input)))
            (let ([idx (random 0 input-len rng)])
              (define v (list->vector old-input))
              (vector-set! v idx (mutate-with-dictionary (vector-ref v idx) dict rng))
              (values (vector->list v) #f))]
           ;; Multi-element list of lists → structural mutation
           [(and (< strategy-roll 0.7)
                 (list? old-input) (> input-len 1) (andmap list? old-input))
            (let ([idx (random 0 input-len rng)])
              (define v (list->vector old-input))
              (define elem (vector-ref v idx))
              (vector-set! v idx
                (if (list? elem)
                    (mutate-list-structurally elem rng)
                    (mutate-value elem rng)))
              (values (vector->list v) #f))]
           ;; Single or multi-element list → mutate one element's value
           [(and (list? old-input) (not (null? old-input)))
            (let ([idx (random 0 input-len rng)])
              (define v (list->vector old-input))
              (vector-set! v idx (mutate-value (vector-ref v idx) rng))
              (values (vector->list v) #f))]
           [else (values (mutate-value old-input rng) #f)]))
       (values new-input entry new-hint)]
      [else (values #f #f #f)]))

  ;; Compute size for iteration n
  (define (iter-size n)
    (min 1000 (expt (add1 (modulo n 50)) 2)))

  ;; --- The main batched loop ---
  (define (run-loop iteration)
    (cond
      [(>= iteration max-iters)
       (make-result 'passed iteration)]
      [(>= (current-inexact-milliseconds) (+ start-time max-time-ms))
       (make-result 'timed-out iteration)]
      [else
       (when (and verbose? (zero? (modulo iteration (* batch-size 10))))
         (define summary (and tci (coverage-summary tci)))
         (eprintf "guided: iteration ~a, corpus ~a, coverage ~a/~a (~a%)\n"
                  iteration (corpus-size corp)
                  (if summary (hash-ref summary 'covered) "?")
                  (if summary (hash-ref summary 'total) "?")
                  (if summary
                      (real->decimal-string (hash-ref summary 'percent) 1)
                      "?")))

       ;; Snapshot coverage before this batch
       (define snap (and tci (snapshot-target! tci)))

       ;; Generate and run a batch of inputs
       (define actual-batch-size
         (min batch-size (- max-iters iteration)))

       ;; Collect batch inputs, parents, trees, and mutation hints
       (define batch-inputs (make-vector actual-batch-size #f))
       (define batch-parents (make-vector actual-batch-size #f))
       (define batch-trees (make-vector actual-batch-size #f))
       (define batch-hints (make-vector actual-batch-size #f))
       (define batch-passed (make-vector actual-batch-size #t))
       (define batch-exns (make-vector actual-batch-size #f))
       (define failure-idx #f)

       (for ([i (in-range actual-batch-size)]
             #:break failure-idx)
         (define use-mutation?
           (and (> (corpus-size corp) 0)
                (< (random rng) mutation-rate)))

         (define-values (args parent-entry tree hint)
           (cond
             [use-mutation?
              (define-values (mutated parent mut-hint) (mutate-from-corpus))
              (if mutated
                  (values mutated parent #f mut-hint)
                  (let-values ([(a t) (generate-fresh (iter-size (+ iteration i)))])
                    (values a #f t #f)))]
             [else
              (define-values (a t) (generate-fresh (iter-size (+ iteration i))))
              (values a #f t #f)]))

         (vector-set! batch-inputs i args)
         (vector-set! batch-parents i parent-entry)
         (vector-set! batch-trees i tree)
         (vector-set! batch-hints i hint)

         (define-values (passed? exn) (test-input args))
         (vector-set! batch-passed i passed?)
         (vector-set! batch-exns i exn)

         (unless passed?
           (set! failure-idx i)))

       ;; If a failure was found, handle it immediately
       (cond
         [failure-idx
          (when verbose?
            (eprintf "guided: failure at iteration ~a\n"
                     (+ iteration failure-idx)))
          (define args (vector-ref batch-inputs failure-idx))
          (define tree (vector-ref batch-trees failure-idx))
          (define exn (vector-ref batch-exns failure-idx))
          (define shrunk
            (cond
              [tree
               (descend-shrinks (shrink-tree-shrinks tree)
                                args
                                (lambda (a)
                                  (let-values ([(p _) (test-input a)]) p)))]
              [else
               (shrink-failing-input
                args
                (lambda (a)
                  (let-values ([(p _) (test-input a)]) (not p)))
                100)]))
          (make-result 'falsified (+ iteration failure-idx)
                       args shrunk exn)]
         [tci
          (define batch-bitmap (compute-batch-bitmap! tci snap))
          (define global-bitmap (target-coverage-info-global-bitmap tci))
          (define interesting? (bitmap-has-new-coverage? batch-bitmap global-bitmap))

          (when interesting?
            (define new-bits (count-new-bits batch-bitmap global-bitmap))

            ;; Save the pre-merge global bitmap for per-input comparison
            (define pre-global (bytes-copy global-bitmap))
            (merge-bitmap! batch-bitmap global-bitmap)
            (set! total-new-bits (+ total-new-bits new-bits))

            ;; Identify which specific inputs triggered new coverage
            ;; by re-running each and checking against the pre-batch global.
            (define added 0)
            (for ([i (in-range actual-batch-size)])
              (when (vector-ref batch-passed i)
                (define per-snap (snapshot-target! tci))
                (with-handlers ([exn:fail? void])
                  (test-input (vector-ref batch-inputs i)))
                (define per-bitmap (compute-batch-bitmap! tci per-snap))
                ;; Check if this input has bits not in the pre-batch global
                (when (bitmap-has-new-coverage? per-bitmap pre-global)
                  (define per-bits (count-new-bits per-bitmap pre-global))
                  ;; Merge this input's bits into pre-global so subsequent
                  ;; inputs in the same batch are compared correctly
                  (merge-bitmap! per-bitmap pre-global)
                  (define entry
                    (corpus-entry (vector-ref batch-inputs i)
                                  #t
                                  per-bits
                                  (+ iteration i)
                                  (vector-ref batch-parents i)
                                  (box (exact->inexact per-bits))
                                  (box 0)
                                  (vector-ref batch-hints i)))
                  (corpus-add! corp entry)
                  (set! added (add1 added))
                  (define parent (vector-ref batch-parents i))
                  (when parent (corpus-boost-energy! parent per-bits)))))

            ;; Extend-with-options: take interesting inputs and try
            ;; extending each one with every dictionary entry. This
            ;; explores multiple directions from the coverage frontier.
            (define dict (if tci (target-coverage-info-dictionary tci) '()))
            (when (and (not (null? dict)) (> added 0))
              (define interesting-inputs
                (for/list ([i (in-range actual-batch-size)]
                           #:when (vector-ref batch-passed i))
                  (vector-ref batch-inputs i)))
              ;; Take up to 5 interesting inputs and extend each
              (define to-extend
                (if (> (length interesting-inputs) 5)
                    (take interesting-inputs 5)
                    interesting-inputs))
              (define ext-snap (snapshot-target! tci))
              (define ext-added 0)
              (for ([input (in-list to-extend)])
                ;; Generate extensions: for each arg, extend with dictionary
                (when (and (list? input) (not (null? input)))
                  (define extensions
                    (extend-with-dictionary (car input) dict rng))
                  ;; Run each extension
                  (for ([ext (in-list extensions)])
                    (define ext-input (cons ext (cdr input)))
                    (with-handlers ([exn:fail? void])
                      (test-input ext-input)))))
              ;; Check if extensions found new coverage
              (define ext-bitmap (compute-batch-bitmap! tci ext-snap))
              (when (bitmap-has-new-coverage? ext-bitmap global-bitmap)
                (define ext-new (count-new-bits ext-bitmap global-bitmap))
                (merge-bitmap! ext-bitmap global-bitmap)
                (set! total-new-bits (+ total-new-bits ext-new))
                (set! ext-added ext-new)
                ;; Re-run extensions individually to find which ones helped
                ;; (simplified: just add the interesting inputs with higher energy)
                (for ([input (in-list to-extend)])
                  (corpus-add! corp
                    (corpus-entry input #t ext-new (+ iteration actual-batch-size)
                                 #f (box (* 2.0 ext-new)) (box 0) #f))))
              (when (and verbose? (> ext-added 0))
                (eprintf "    extensions: ~a new bits from dictionary expansion\n"
                         ext-added)))

            (when verbose?
              (eprintf "  batch ~a-~a: ~a new bits, ~a/~a added to corpus (size ~a)\n"
                       iteration (+ iteration actual-batch-size -1)
                       new-bits added actual-batch-size
                       (corpus-size corp))))

          (unless interesting?
            ;; Decay parents that didn't produce interesting offspring
            (for ([i (in-range actual-batch-size)])
              (define parent (vector-ref batch-parents i))
              (when parent (corpus-decay-energy! parent))))

          (run-loop (+ iteration actual-batch-size))]

         ;; No instrumentation — just continue
         [else
          (run-loop (+ iteration actual-batch-size))])]))

  ;; Result constructor
  (define (make-result status iteration
                       [args #f] [shrunk #f] [exn #f])
    (guided-result status iteration args shrunk exn
                   corp seed
                   (if tci (coverage-summary tci) (hash))
                   total-new-bits))

  ;; Run
  (define result (run-loop 0))

  ;; Handle the case where run-loop returned void (failure was handled inline)
  (if (guided-result? result) result
      (make-result 'passed max-iters)))

;; Descend a rackcheck shrink tree.
(define (descend-shrinks trees last-failing-value pass?)
  (cond
    [(stream-empty? trees) last-failing-value]
    [else
     (define tree (stream-first trees))
     (define value (shrink-tree-val tree))
     (if (pass? value)
         (descend-shrinks (stream-rest trees) last-failing-value pass?)
         (descend-shrinks (shrink-tree-shrinks tree) value pass?))]))
