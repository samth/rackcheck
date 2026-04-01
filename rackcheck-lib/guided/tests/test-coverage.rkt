#lang racket/base

;; Tests for coverage collection and diffing.

(require rackunit
         racket/set
         rackcheck/guided/coverage)

(test-case "diff-coverage computes positive deltas"
  (define before (hash '("f" 1 5) 3 '("f" 2 3) 0))
  (define after (hash '("f" 1 5) 5 '("f" 2 3) 2 '("f" 3 1) 1))
  (define d (diff-coverage before after))
  (check-equal? (hash-ref d '("f" 1 5)) 2)
  (check-equal? (hash-ref d '("f" 2 3)) 2)
  (check-equal? (hash-ref d '("f" 3 1)) 1))

(test-case "diff-coverage ignores zero/negative deltas"
  (define before (hash '("f" 1 5) 10))
  (define after (hash '("f" 1 5) 10))
  (define d (diff-coverage before after))
  (check-equal? (hash-count d) 0))

(test-case "coverage-signature extracts hit points"
  (define d (hash '("f" 1 5) 2 '("f" 2 3) 1))
  (define sig (coverage-signature d))
  (check-equal? (set-count sig) 2)
  (check-true (set-member? sig '("f" 1 5)))
  (check-true (set-member? sig '("f" 2 3))))

(test-case "new-coverage? detects novel points"
  (define sig (set '("f" 1 5) '("f" 2 3)))
  (define global (set '("f" 1 5)))
  (check-true (new-coverage? sig global))
  (check-false (new-coverage? sig (set '("f" 1 5) '("f" 2 3)))))

(test-case "count-crosses-threshold? detects power-of-2 crossings"
  (check-true (count-crosses-threshold? (hash '("f" 1 5) 1) (hash '("f" 1 5) 2)))
  (check-false (count-crosses-threshold? (hash '("f" 1 5) 2) (hash '("f" 1 5) 3)))
  (check-true (count-crosses-threshold? (hash '("f" 1 5) 2) (hash '("f" 1 5) 4))))

(test-case "coverage-sig-hash is deterministic"
  (define sig (set '("f" 1 5) '("f" 2 3)))
  (check-equal? (coverage-sig-hash sig) (coverage-sig-hash sig)))

(test-case "make-instrumented-namespace loads and instruments a module"
  (with-output-to-file "/tmp/cov-test-mod.rkt" #:exists 'replace
    (lambda ()
      (displayln "#lang racket/base")
      (displayln "(provide foo)")
      (displayln "(define (foo x) (if (> x 0) 'pos 'neg))")))
  (define-values (ns get-counts)
    (make-instrumented-namespace "/tmp/cov-test-mod.rkt"))
  (parameterize ([current-namespace ns])
    (dynamic-require (string->path "/tmp/cov-test-mod.rkt") #f))
  (define foo
    (parameterize ([current-namespace ns])
      (dynamic-require (string->path "/tmp/cov-test-mod.rkt") 'foo)))
  (define before (snapshot-coverage get-counts))
  (foo 5)
  (define after (snapshot-coverage get-counts))
  (define d (diff-coverage before after))
  (check-true (> (hash-count d) 0) "Should have coverage after calling foo"))

(printf "All coverage tests passed.\n")
