#lang racket/base

;; Coverage-guided property-based testing for rackcheck.
;;
;; Extends rackcheck with a batched coverage feedback loop: inputs are
;; generated in batches, run against the property, and the batch's
;; collective coverage is checked. Batches that trigger new coverage
;; are added to a corpus for further mutation.

(require racket/format
         "prop.rkt"
         "guided/config.rkt"
         "guided/coverage.rkt"
         "guided/corpus.rkt"
         "guided/guidance.rkt")

(provide
 make-guided-config
 guided-config?
 check-guided
 ;; For callers who want to set up their own instrumented namespace
 ;; and pass the tci directly to check-guided #:target
 make-instrumented-namespace
 target-coverage-info?
 target-coverage-info-num-points
 target-coverage-info-boxes
 target-coverage-info-global-bitmap
 snapshot-target!
 compute-batch-bitmap!
 bitmap-has-new-coverage?
 merge-bitmap!
 count-new-bits
 coverage-summary
 guided-result?
 guided-result-status
 guided-result-counterexample
 guided-result-shrunk
 guided-result-exception
 guided-result-iterations
 guided-result-corpus
 guided-result-seed
 guided-result-coverage-summary
 guided-result-new-coverage-bits
 corpus?
 corpus-entries
 corpus-size
 corpus-entry?
 corpus-entry-input
 corpus-entry-iteration
 corpus-entry-parent
 replay-input
 print-guided-result
 check-guided-property)

(define (check-guided prop
                      #:config [config (make-guided-config)]
                      #:target [target #f])
  (run-guided config prop
              (cond
                [(target-coverage-info? target) target]
                [(path? target) target]
                [(string? target) (string->path target)]
                [else #f])))

(define (replay-input p args)
  (define f (property-proc p))
  (with-handlers ([exn:fail? (lambda (e) e)])
    (apply f args)))

(define (print-guided-result res)
  (define status (guided-result-status res))
  (define summary (guided-result-coverage-summary res))
  (printf "Coverage-guided testing result:\n")
  (printf "  Status: ~a\n" status)
  (printf "  Iterations: ~a\n" (guided-result-iterations res))
  (printf "  Seed: ~a\n" (guided-result-seed res))
  (printf "  Corpus size: ~a\n" (corpus-size (guided-result-corpus res)))
  (printf "  New coverage bits: ~a\n" (guided-result-new-coverage-bits res))
  (printf "  Coverage: ~a/~a (~a%)\n"
          (hash-ref summary 'covered 0)
          (hash-ref summary 'total 0)
          (if (hash-ref summary 'percent #f)
              (real->decimal-string (hash-ref summary 'percent) 1)
              "?"))
  (case status
    [(falsified)
     (printf "  Counterexample: ~s\n" (guided-result-counterexample res))
     (when (guided-result-shrunk res)
       (printf "  Shrunk: ~s\n" (guided-result-shrunk res)))
     (when (guided-result-exception res)
       (printf "  Exception: ~a\n" (exn-message (guided-result-exception res))))]
    [(passed)
     (printf "  All iterations passed.\n")]
    [(timed-out)
     (printf "  Timed out.\n")]))

(define (check-guided-property prop
                               #:config [config (make-guided-config)]
                               #:target [target #f])
  (define res (check-guided prop #:config config #:target target))
  (case (guided-result-status res)
    [(falsified)
     (error 'check-guided-property
            "property ~a falsified after ~a iterations\n  counterexample: ~s\n  shrunk: ~s"
            (property-name prop)
            (guided-result-iterations res)
            (guided-result-counterexample res)
            (or (guided-result-shrunk res)
                (guided-result-counterexample res)))]
    [(timed-out)
     (printf "  ~ property ~a timed out after ~a iterations\n"
             (property-name prop)
             (guided-result-iterations res))]
    [(passed)
     (printf "  ✓ property ~a passed ~a guided iterations (corpus: ~a, coverage: ~a/~a)\n"
             (property-name prop)
             (guided-result-iterations res)
             (corpus-size (guided-result-corpus res))
             (hash-ref (guided-result-coverage-summary res) 'covered 0)
             (hash-ref (guided-result-coverage-summary res) 'total 0))]))
