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
  (define (mutate-from-corpus)
    (define entry (corpus-pick corp rng))
    (cond
      [entry
       (set-box! (corpus-entry-offspring-count entry)
                 (add1 (unbox (corpus-entry-offspring-count entry))))
       (define old-input (corpus-entry-input entry))
       (define new-input
         (cond
           ;; List of lists (operation sequences) → structural mutation 70%
           [(and (list? old-input) (not (null? old-input))
                 (andmap list? old-input))
            (if (< (random rng) 0.7)
                (mutate-list-structurally old-input rng)
                ;; Fall back to single-element value mutation
                (let ([idx (random 0 (length old-input) rng)])
                  (define v (list->vector old-input))
                  (vector-set! v idx (mutate-value (vector-ref v idx) rng))
                  (vector->list v)))]
           ;; Plain list → element-level mutation
           [(and (list? old-input) (not (null? old-input)))
            (let ([idx (random 0 (length old-input) rng)])
              (define v (list->vector old-input))
              (vector-set! v idx (mutate-value (vector-ref v idx) rng))
              (vector->list v))]
           [else (mutate-value old-input rng)]))
       (values new-input entry)]
      [else (values #f #f)]))

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

       ;; Collect batch inputs, their parents, and their trees (for shrinking)
       (define batch-inputs (make-vector actual-batch-size #f))
       (define batch-parents (make-vector actual-batch-size #f))
       (define batch-trees (make-vector actual-batch-size #f))
       (define batch-passed (make-vector actual-batch-size #t))
       (define batch-exns (make-vector actual-batch-size #f))
       (define failure-idx #f)

       (for ([i (in-range actual-batch-size)]
             #:break failure-idx)
         (define use-mutation?
           (and (> (corpus-size corp) 0)
                (< (random rng) mutation-rate)))

         (define-values (args parent-entry tree)
           (cond
             [use-mutation?
              (define-values (mutated parent) (mutate-from-corpus))
              (if mutated
                  (values mutated parent #f)
                  (let-values ([(a t) (generate-fresh (iter-size (+ iteration i)))])
                    (values a #f t)))]
             [else
              (define-values (a t) (generate-fresh (iter-size (+ iteration i))))
              (values a #f t)]))

         (vector-set! batch-inputs i args)
         (vector-set! batch-parents i parent-entry)
         (vector-set! batch-trees i tree)

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
            (merge-bitmap! batch-bitmap global-bitmap)
            (set! total-new-bits (+ total-new-bits new-bits))

            ;; Add all inputs from this batch to the corpus
            (for ([i (in-range actual-batch-size)])
              (when (vector-ref batch-passed i)
                (define entry
                  (corpus-entry (vector-ref batch-inputs i)
                                #t
                                new-bits
                                (+ iteration i)
                                (vector-ref batch-parents i)
                                (box (exact->inexact new-bits))
                                (box 0)))
                (corpus-add! corp entry)))

            ;; Boost parent energy for parents in this batch
            (for ([i (in-range actual-batch-size)])
              (define parent (vector-ref batch-parents i))
              (when parent (corpus-boost-energy! parent new-bits)))

            (when verbose?
              (eprintf "  batch ~a-~a: ~a new coverage bits, corpus now ~a\n"
                       iteration (+ iteration actual-batch-size -1)
                       new-bits (corpus-size corp))))

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
