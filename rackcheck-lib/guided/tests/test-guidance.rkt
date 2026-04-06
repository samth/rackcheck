#lang racket/base

;; End-to-end tests for the guided testing loop.

(require rackunit
         rackcheck)

(define (write-target! path code)
  (with-output-to-file path #:exists 'replace
    (lambda () (display code))))

(test-case "guided check finds failure"
  (write-target! "/tmp/rackcheck-guidance-test-bug.rkt"
    #<<END
#lang racket/base
(provide buggy)
(define (buggy x)
  (cond
    [(< x 0) 'neg]
    [(< x 100) 'ok]
    [(< x 200) 'ok]
    [(and (>= x 200) (< x 210)) (error 'buggy "bug!")]
    [else 'ok]))
END
  )
  (define p
    (property ([x (gen:integer-in 0 1000)])
      (let ([buggy (dynamic-require (string->path "/tmp/rackcheck-guidance-test-bug.rkt") 'buggy)])
        (buggy x)
        #t)))
  (define res
    (check-guided p
      #:config (make-guided-config
                #:max-iterations 5000
                #:max-time-ms 10000
                #:seed 42)
      #:target "/tmp/rackcheck-guidance-test-bug.rkt"))
  (check-equal? (guided-result-status res) 'falsified))

(test-case "guided check passes for correct property"
  (write-target! "/tmp/rackcheck-guidance-test-ok.rkt"
    #<<END
#lang racket/base
(provide safe)
(define (safe x) (if (> x 0) 'pos 'neg))
END
  )
  (define p
    (property ([x (gen:integer-in -100 100)])
      (let ([safe (dynamic-require (string->path "/tmp/rackcheck-guidance-test-ok.rkt") 'safe)])
        (symbol? (safe x)))))
  (define res
    (check-guided p
      #:config (make-guided-config
                #:max-iterations 200
                #:max-time-ms 5000
                #:seed 42)
      #:target "/tmp/rackcheck-guidance-test-ok.rkt"))
  (check-equal? (guided-result-status res) 'passed))

(test-case "guided check reports coverage stats"
  (write-target! "/tmp/rackcheck-guidance-test-cov.rkt"
    #<<END
#lang racket/base
(provide branchy)
(define (branchy x)
  (cond
    [(< x 0) 'neg]
    [(< x 10) 'small]
    [(< x 100) 'medium]
    [else 'big]))
END
  )
  (define p
    (property ([x (gen:integer-in -100 200)])
      (let ([branchy (dynamic-require (string->path "/tmp/rackcheck-guidance-test-cov.rkt") 'branchy)])
        (symbol? (branchy x)))))
  (define res
    (check-guided p
      #:config (make-guided-config
                #:max-iterations 500
                #:max-time-ms 5000
                #:seed 42)
      #:target "/tmp/rackcheck-guidance-test-cov.rkt"))
  (check-equal? (guided-result-status res) 'passed)
  (define summary (guided-result-coverage-summary res))
  (check-true (hash? summary))
  (check-true (> (hash-ref summary 'total 0) 0)))

(printf "All guidance tests passed.\n")
