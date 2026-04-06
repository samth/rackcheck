#lang racket/base

;; Corpus management with power-schedule entry selection.
;;
;; Each corpus entry tracks the input, its coverage bitmap, how many
;; new coverage bits it contributed, and an energy value that decays
;; when mutations of this entry fail to produce new coverage.

(require racket/random)

(provide
 (struct-out corpus-entry)
 make-corpus
 corpus?
 corpus-add!
 corpus-entries
 corpus-size
 corpus-pick
 corpus-boost-energy!
 corpus-decay-energy!)

(struct corpus-entry
  (input              ; the test input value(s)
   outcome            ; #t for pass, #f for fail
   new-bits-count     ; how many new coverage bits this entry contributed
   iteration          ; when it was found
   parent             ; parent corpus-entry or #f
   energy             ; (box real?) — power schedule energy, mutable
   offspring-count    ; (box exact-nonneg-integer?)
   mutation-hint)     ; #f or (list 'position index) — what mutation produced this
  #:transparent)

(struct corpus
  (entries-box)  ; (box (listof corpus-entry?))
  #:transparent)

(define (make-corpus)
  (corpus (box '())))

(define (corpus-entries c)
  (unbox (corpus-entries-box c)))

(define (corpus-size c)
  (length (corpus-entries c)))

(define (corpus-add! c entry)
  (set-box! (corpus-entries-box c) (cons entry (unbox (corpus-entries-box c)))))

;; Power-schedule selection: weight = energy / (1 + offspring-count).
;; Entries that produce interesting offspring get boosted; those that
;; don't get decayed. Floor at 0.01 to prevent starvation.
(define (corpus-pick c rng)
  (define entries (corpus-entries c))
  (cond
    [(null? entries) #f]
    [else
     (define weights
       (for/list ([e (in-list entries)])
         (max 0.01 (/ (unbox (corpus-entry-energy e))
                      (add1 (unbox (corpus-entry-offspring-count e)))))))
     (define total (apply + weights))
     (define target (* total (random rng)))
     (let loop ([entries entries] [weights weights] [acc 0.0])
       (cond
         [(null? (cdr entries)) (car entries)]
         [else
          (define w (car weights))
          (if (< target (+ acc w))
              (car entries)
              (loop (cdr entries) (cdr weights) (+ acc w)))]))]))

;; Boost: called when a mutation of this entry produced new coverage.
(define (corpus-boost-energy! entry new-bits)
  (define b (corpus-entry-energy entry))
  (set-box! b (+ (unbox b) new-bits)))

;; Decay: called when a mutation of this entry did NOT produce new coverage.
(define (corpus-decay-energy! entry)
  (define b (corpus-entry-energy entry))
  (set-box! b (* (unbox b) 0.95)))
