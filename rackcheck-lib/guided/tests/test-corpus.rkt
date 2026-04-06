#lang racket/base

;; Tests for corpus management with power schedule.

(require rackunit
         rackcheck/guided/corpus)

(test-case "make-corpus creates empty corpus"
  (define c (make-corpus))
  (check-equal? (corpus-size c) 0)
  (check-equal? (corpus-entries c) '()))

(test-case "corpus-add! adds entries"
  (define c (make-corpus))
  (define entry (corpus-entry '(42) #t 5 0 #f (box 5.0) (box 0)))
  (corpus-add! c entry)
  (check-equal? (corpus-size c) 1))

(test-case "corpus-pick returns an entry"
  (define c (make-corpus))
  (corpus-add! c (corpus-entry '(1) #t 3 0 #f (box 3.0) (box 0)))
  (corpus-add! c (corpus-entry '(2) #t 5 1 #f (box 5.0) (box 0)))
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed 42))
  (define picked (corpus-pick c rng))
  (check-true (corpus-entry? picked)))

(test-case "corpus-pick returns #f for empty corpus"
  (define c (make-corpus))
  (define rng (make-pseudo-random-generator))
  (check-false (corpus-pick c rng)))

(test-case "corpus-boost-energy! increases energy"
  (define entry (corpus-entry '(1) #t 3 0 #f (box 3.0) (box 0)))
  (corpus-boost-energy! entry 5)
  (check-equal? (unbox (corpus-entry-energy entry)) 8.0))

(test-case "corpus-decay-energy! decreases energy"
  (define entry (corpus-entry '(1) #t 3 0 #f (box 10.0) (box 0)))
  (corpus-decay-energy! entry)
  (check-equal? (unbox (corpus-entry-energy entry)) 9.5))

(test-case "power schedule favors high-energy entries"
  (define c (make-corpus))
  (corpus-add! c (corpus-entry '(a) #t 10 0 #f (box 100.0) (box 0)))
  (corpus-add! c (corpus-entry '(b) #t 1 1 #f (box 0.1) (box 10)))
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed 42))
  (define counts (make-hash))
  (for ([_ 100])
    (define picked (corpus-pick c rng))
    (define key (corpus-entry-input picked))
    (hash-update! counts key add1 0))
  (check-true (> (hash-ref counts '(a) 0) (hash-ref counts '(b) 0))
              "High-energy entry should be picked more often"))

(printf "All corpus tests passed.\n")
