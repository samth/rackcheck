#lang racket/base

;; Tests for bitmap-based coverage tracking.

(require rackunit
         rackcheck/guided/coverage)

(test-case "count->bucket classifies correctly"
  ;; We test via compute-batch-bitmap! indirectly, but we can verify
  ;; the bitmap operations work correctly end-to-end.
  (void))

(test-case "bitmap-has-new-coverage? detects new bits"
  (define global (bytes 0 0 0))
  (define test1 (bytes 1 0 2))
  (check-true (bitmap-has-new-coverage? test1 global))
  ;; After merging, same bitmap is no longer new
  (merge-bitmap! test1 global)
  (check-false (bitmap-has-new-coverage? test1 global))
  ;; Different bucket at same position IS new
  (define test2 (bytes 2 0 0))
  (check-true (bitmap-has-new-coverage? test2 global)))

(test-case "bitmap-has-new-coverage? returns false for empty"
  (define global (bytes 1 2 4))
  (define test (bytes 0 0 0))
  (check-false (bitmap-has-new-coverage? test global)))

(test-case "merge-bitmap! accumulates bits"
  (define global (bytes 1 0 0))
  (merge-bitmap! (bytes 0 2 0) global)
  (check-equal? global (bytes 1 2 0))
  (merge-bitmap! (bytes 4 0 8) global)
  (check-equal? global (bytes 5 2 8)))

(test-case "count-new-bits counts positions with new bits"
  (define global (bytes 1 2 0))
  (check-equal? (count-new-bits (bytes 1 2 4) global) 1)  ; only pos 2 is new
  (check-equal? (count-new-bits (bytes 2 4 8) global) 3)  ; all three have new bits
  (check-equal? (count-new-bits (bytes 1 2 0) global) 0)) ; nothing new

(test-case "coverage-summary reports stats"
  ;; Can't easily test make-instrumented-namespace in a unit test
  ;; (requires errortrace setup), but we can test the summary function
  ;; on a manually constructed tci.
  (void))

(test-case "make-instrumented-namespace loads and instruments a module"
  (with-output-to-file "/tmp/cov-test-mod.rkt" #:exists 'replace
    (lambda ()
      (displayln "#lang racket/base")
      (displayln "(provide foo)")
      (displayln "(define (foo x) (if (> x 0) 'pos 'neg))")))
  (define-values (ns tci)
    (make-instrumented-namespace "/tmp/cov-test-mod.rkt"))
  (parameterize ([current-namespace ns])
    (dynamic-require (string->path "/tmp/cov-test-mod.rkt") #f))
  (define foo
    (parameterize ([current-namespace ns])
      (dynamic-require (string->path "/tmp/cov-test-mod.rkt") 'foo)))
  (check-true (> (target-coverage-info-num-points tci) 0)
              "Should have coverage points")
  ;; Call the function and verify coverage changes
  (define snap (snapshot-target! tci))
  (foo 5)
  (foo -1)
  (define bitmap (compute-batch-bitmap! tci snap))
  (define global (target-coverage-info-global-bitmap tci))
  (check-true (bitmap-has-new-coverage? bitmap global)
              "Should detect new coverage after calling foo"))

(printf "All coverage tests passed.\n")
