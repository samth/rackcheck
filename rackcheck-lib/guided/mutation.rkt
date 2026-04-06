#lang racket/base

;; Type-aware value mutation for coverage-guided testing.
;;
;; Mutations operate at the level of Racket values (not bytes), which is
;; a key advantage of building on a PBT framework rather than a byte-level
;; fuzzer. Each type has its own set of mutation strategies.

(require racket/contract/base
         racket/random
         racket/list)

(provide
 (contract-out
  [mutate-value (-> any/c pseudo-random-generator? any/c)]
  [splice-values (-> any/c any/c pseudo-random-generator? any/c)]
  [mutate-list-structurally (-> list? pseudo-random-generator? list?)])
 mutate-value-near
 mutate-with-dictionary
 extend-with-dictionary)

(define (exact-nonneg-integer? v) (and (exact-integer? v) (>= v 0)))

;; Dispatch mutation by type.
(define (mutate-value val rng)
  (cond
    [(boolean? val) (mutate-boolean val rng)]
    [(exact-integer? val) (mutate-integer val rng)]
    [(real? val) (mutate-real val rng)]
    [(char? val) (mutate-char val rng)]
    [(string? val) (mutate-string val rng)]
    [(bytes? val) (mutate-bytes val rng)]
    [(list? val) (mutate-list val rng)]
    [(vector? val) (mutate-vector val rng)]
    [(pair? val) (mutate-pair val rng)]
    ;; Fallback: return unchanged (we can't mutate arbitrary structs generically)
    [else val]))

;; Boolean mutation: always flip.
(define (mutate-boolean val rng)
  (not val))

;; Integer mutation strategies.
(define (mutate-integer val rng)
  (define strategies
    (list
     (lambda () 0)                              ; try zero
     (lambda () (add1 val))                     ; increment
     (lambda () (sub1 val))                     ; decrement
     (lambda () (- val))                        ; negate
     (lambda () (quotient val 2))               ; halve
     (lambda () (* val 2))                      ; double
     (lambda () (+ val (random -10 11 rng)))    ; small perturbation
     (lambda () (+ val (random -100 101 rng)))  ; medium perturbation
     (lambda () (random -1000 1001 rng))        ; random in moderate range
     (lambda () (expt 2 (random 0 32 rng)))     ; power of 2
     (lambda () (sub1 (expt 2 (random 1 32 rng)))) ; 2^n - 1
     (lambda () -1)                             ; boundary
     (lambda () 1)))                            ; boundary
  (define strategy (random-ref strategies rng))
  (strategy))

;; Real number mutation.
(define (mutate-real val rng)
  (define strategies
    (list
     (lambda () 0.0)
     (lambda () (+ val (* 0.01 (- (random rng) 0.5))))
     (lambda () (+ val (* 0.1 (- (random rng) 0.5))))
     (lambda () (- val))
     (lambda () (* val (+ 0.5 (random rng))))
     (lambda () (random rng))))
  ((random-ref strategies rng)))

;; Character mutation.
(define (mutate-char val rng)
  (define n (char->integer val))
  (define strategies
    (list
     (lambda () #\nul)
     (lambda () #\space)
     (lambda () #\newline)
     (lambda () (integer->char (modulo (add1 n) 256)))
     (lambda () (integer->char (modulo (sub1 n) 256)))
     (lambda () (integer->char (random 0 128 rng)))
     (lambda () (integer->char (random 0 256 rng)))))
  ((random-ref strategies rng)))

;; String mutation strategies.
(define (mutate-string val rng)
  (define len (string-length val))
  (define strategies
    (list
     ;; Empty string
     (lambda () "")
     ;; Insert a random char at a random position
     (lambda ()
       (define pos (random 0 (add1 len) rng))
       (define ch (integer->char (random 32 127 rng)))
       (string-append (substring val 0 pos)
                      (string ch)
                      (substring val pos)))
     ;; Delete a char (if non-empty)
     (lambda ()
       (if (zero? len) val
           (let ([pos (random 0 len rng)])
             (string-append (substring val 0 pos)
                            (substring val (add1 pos))))))
     ;; Replace a char (if non-empty)
     (lambda ()
       (if (zero? len) val
           (let ([pos (random 0 len rng)]
                 [ch (integer->char (random 32 127 rng))])
             (string-append (substring val 0 pos)
                            (string ch)
                            (substring val (add1 pos))))))
     ;; Duplicate string
     (lambda () (string-append val val))
     ;; Truncate
     (lambda ()
       (if (<= len 1) val
           (substring val 0 (random 1 len rng))))))
  ((random-ref strategies rng)))

;; Bytes mutation (similar to string but for byte strings).
(define (mutate-bytes val rng)
  (define len (bytes-length val))
  (define strategies
    (list
     (lambda () #"")
     ;; Insert a byte
     (lambda ()
       (define pos (random 0 (add1 len) rng))
       (define b (random 0 256 rng))
       (bytes-append (subbytes val 0 pos)
                     (bytes b)
                     (subbytes val pos)))
     ;; Delete a byte (if non-empty)
     (lambda ()
       (if (zero? len) val
           (let ([pos (random 0 len rng)])
             (bytes-append (subbytes val 0 pos)
                           (subbytes val (add1 pos))))))
     ;; Replace a byte (if non-empty)
     (lambda ()
       (if (zero? len) val
           (let ([pos (random 0 len rng)]
                 [b (random 0 256 rng)])
             (bytes-append (subbytes val 0 pos)
                           (bytes b)
                           (subbytes val (add1 pos))))))))
  ((random-ref strategies rng)))

;; List mutation strategies.
(define (mutate-list val rng)
  (define len (length val))
  (define strategies
    (list
     ;; Empty list
     (lambda () '())
     ;; Insert a mutated element at a random position
     (lambda ()
       (if (null? val) val
           (let* ([pos (random 0 (add1 len) rng)]
                  [elem (mutate-value (random-ref val rng) rng)]
                  [front (list-take val pos)]
                  [back (list-drop val pos)])
             (append front (list elem) back))))
     ;; Delete an element (if non-empty)
     (lambda ()
       (if (null? val) val
           (let ([pos (random 0 len rng)])
             (append (list-take val pos)
                     (list-drop val (add1 pos))))))
     ;; Replace an element with a mutation (if non-empty)
     (lambda ()
       (if (null? val) val
           (let ([pos (random 0 len rng)])
             (append (list-take val pos)
                     (list (mutate-value (list-ref val pos) rng))
                     (list-drop val (add1 pos))))))
     ;; Shuffle
     (lambda () (shuffle val rng))
     ;; Repeat an element
     (lambda ()
       (if (null? val) val
           (let ([elem (random-ref val rng)])
             (append val (list elem)))))))
  ((random-ref strategies rng)))

;; Vector mutation: convert to list, mutate, convert back.
(define (mutate-vector val rng)
  (list->vector (mutate-list (vector->list val) rng)))

;; Pair mutation (non-list pair): mutate car or cdr.
(define (mutate-pair val rng)
  (if (zero? (random 0 2 rng))
      (cons (mutate-value (car val) rng) (cdr val))
      (cons (car val) (mutate-value (cdr val) rng))))

;; Splice two values together (used for cross-corpus mutation).
;; For lists: take prefix of one, suffix of another.
;; For other types: randomly pick one and mutate it.
(define (splice-values a b rng)
  (cond
    [(and (list? a) (list? b) (not (null? a)) (not (null? b)))
     (define split-a (random 0 (add1 (length a)) rng))
     (define split-b (random 0 (add1 (length b)) rng))
     (append (list-take a split-a) (list-drop b split-b))]
    [(and (string? a) (string? b))
     (define split-a (random 0 (add1 (string-length a)) rng))
     (define split-b (random 0 (add1 (string-length b)) rng))
     (string-append (substring a 0 split-a) (substring b split-b))]
    [(and (bytes? a) (bytes? b))
     (define split-a (random 0 (add1 (bytes-length a)) rng))
     (define split-b (random 0 (add1 (bytes-length b)) rng))
     (bytes-append (subbytes a 0 split-a) (subbytes b split-b))]
    [else (mutate-value (if (zero? (random 0 2 rng)) a b) rng)]))

;; Helper: safe list take/drop
(define (list-take lst n)
  (cond
    [(or (zero? n) (null? lst)) '()]
    [else (cons (car lst) (list-take (cdr lst) (sub1 n)))]))

(define (list-drop lst n)
  (cond
    [(or (zero? n) (null? lst)) lst]
    [else (list-drop (cdr lst) (sub1 n))]))

;; Shuffle with explicit rng
(define (shuffle lst rng)
  (define vec (list->vector lst))
  (define len (vector-length vec))
  (for ([i (in-range (sub1 len) 0 -1)])
    (define j (random 0 (add1 i) rng))
    (define tmp (vector-ref vec i))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp))
  (vector->list vec))

;; ---------------------------------------------------------------------------
;; Position-biased mutation: mutate near a specific position.
;; Returns (values mutated-value actual-position-mutated).
;;
;; When a previous mutation at position P produced new coverage, future
;; mutations should focus near P — extending it, trying adjacent
;; characters, or making small edits in the same region.

(define (mutate-value-near val hint-pos rng)
  (cond
    [(string? val) (mutate-string-near val hint-pos rng)]
    [(list? val) (mutate-list-near val hint-pos rng)]
    [else (values (mutate-value val rng) 0)]))

;; String mutation biased around a position.
;; Tries mutations within ±3 characters of hint-pos.
(define (mutate-string-near val hint-pos rng)
  (define len (string-length val))
  (if (zero? len)
      (values (string (integer->char (random 32 127 rng))) 0)
      (let ()
  ;; Clamp hint to valid range
  (define pos (min hint-pos (sub1 len)))
  ;; Pick a nearby position (within ±3)
  (define nearby (max 0 (min (sub1 len) (+ pos (- (random 0 7 rng) 3)))))
  (define strategies
    (list
     ;; Insert a char right after the hint position
     (lambda ()
       (define insert-at (min (add1 pos) len))
       (define ch (integer->char (random 32 127 rng)))
       (values (string-append (substring val 0 insert-at)
                              (string ch)
                              (substring val insert-at))
               insert-at))
     ;; Replace the char at/near the hint position
     (lambda ()
       (define ch (integer->char (random 32 127 rng)))
       (values (string-append (substring val 0 nearby)
                              (string ch)
                              (substring val (min len (add1 nearby))))
               nearby))
     ;; Insert a copy of the char at hint-pos next to it (extend a pattern)
     (lambda ()
       (define ch (string-ref val pos))
       (define insert-at (min (add1 pos) len))
       (values (string-append (substring val 0 insert-at)
                              (string ch)
                              (substring val insert-at))
               insert-at))
     ;; Try a char that's close to the current one (±1 codepoint)
     (lambda ()
       (define old-ch (char->integer (string-ref val nearby)))
       (define new-ch (max 32 (min 126 (+ old-ch (if (zero? (random 0 2 rng)) 1 -1)))))
       (values (string-append (substring val 0 nearby)
                              (string (integer->char new-ch))
                              (substring val (min len (add1 nearby))))
               nearby))
     ;; Append a char at the end (grow the string)
     (lambda ()
       (define ch (integer->char (random 32 127 rng)))
       (values (string-append val (string ch)) len))))
  (define strategy (random-ref strategies rng))
  (strategy))))

;; List mutation biased around a position.
(define (mutate-list-near val hint-pos rng)
  (define len (length val))
  (cond
    [(zero? len) (values (list (mutate-value '() rng)) 0)]
    [else
     (define pos (min hint-pos (sub1 len)))
     (define strategies
       (list
        ;; Mutate the element at the hint position
        (lambda ()
          (define v (list->vector val))
          (vector-set! v pos (mutate-value (vector-ref v pos) rng))
          (values (vector->list v) pos))
        ;; Insert a mutated copy of the hint element next to it
        (lambda ()
          (define elem (mutate-value (list-ref val pos) rng))
          (define insert-at (min (add1 pos) len))
          (values (append (list-take val insert-at)
                          (list elem)
                          (list-drop val insert-at))
                  insert-at))
        ;; Replace a nearby element
        (lambda ()
          (define nearby (max 0 (min (sub1 len) (+ pos (- (random 0 5 rng) 2)))))
          (define v (list->vector val))
          (vector-set! v nearby (mutate-value (vector-ref v nearby) rng))
          (values (vector->list v) nearby))))
     ((random-ref strategies rng))]))

;; ---------------------------------------------------------------------------
;; Dictionary-based mutation: insert constants extracted from the target
;; source into the input. This is how AFL finds magic bytes and keywords.

;; Mutate a value using a dictionary entry. For strings, inserts or
;; replaces a substring with a dictionary entry. For lists, inserts a
;; dictionary entry as a new element.
(define (mutate-with-dictionary val dictionary rng)
  (cond
    [(null? dictionary) val]
    [(string? val)
     (define entry (random-ref dictionary rng))
     (define len (string-length val))
     (define strategies
       (list
        ;; Insert dictionary entry at a random position
        (lambda ()
          (define pos (random 0 (add1 len) rng))
          (string-append (substring val 0 pos) entry (substring val pos)))
        ;; Replace a portion with the dictionary entry
        (lambda ()
          (define pos (random 0 (max 1 len) rng))
          (define end (min len (+ pos (string-length entry))))
          (string-append (substring val 0 pos) entry (substring val end)))
        ;; Overwrite from a random position
        (lambda ()
          (define pos (random 0 (max 1 len) rng))
          (string-append (substring val 0 pos) entry))))
     ((random-ref strategies rng))]
    [(list? val)
     (define entry (random-ref dictionary rng))
     (define pos (random 0 (add1 (length val)) rng))
     (append (list-take val pos) (list entry) (list-drop val pos))]
    [else val]))

;; Extend a value with dictionary entries, producing multiple variants.
;; For strings: insert dictionary entries at various positions, and also
;; try concatenating pairs of entries to form plausible multi-token
;; sequences (like "#b" + "1" or "#(" + "1" + ")").
(define (extend-with-dictionary val dictionary rng)
  (cond
    [(string? val)
     (define extensions '())
     ;; Single entries: insert each at a random position
     (for ([entry (in-list dictionary)])
       (define pos (random 0 (add1 (string-length val)) rng))
       (set! extensions
             (cons (string-append (substring val 0 pos) entry (substring val pos))
                   extensions)))
     ;; Pairs: concatenate two dictionary entries and insert
     (for ([_ (in-range (min 20 (length dictionary)))])
       (define e1 (random-ref dictionary rng))
       (define e2 (random-ref dictionary rng))
       (define combined (string-append e1 e2))
       (define pos (random 0 (add1 (string-length val)) rng))
       (set! extensions
             (cons (string-append (substring val 0 pos) combined (substring val pos))
                   extensions)))
     ;; Triples: three random entries concatenated
     (for ([_ (in-range 20)])
       (define e1 (random-ref dictionary rng))
       (define e2 (random-ref dictionary rng))
       (define e3 (random-ref dictionary rng))
       (set! extensions
             (cons (string-append e1 e2 e3) extensions)))
     ;; Quads and quints: longer random dictionary concatenations
     (for ([_ (in-range 20)])
       (define n (+ 4 (random 0 3 rng)))
       (define parts (for/list ([_ (in-range n)]) (random-ref dictionary rng)))
       (set! extensions (cons (apply string-append parts) extensions)))
     ;; Insert pairs/triples into the EXISTING value at random positions
     (for ([_ (in-range 20)])
       (define n (+ 2 (random 0 3 rng)))
       (define combined (apply string-append
                               (for/list ([_ (in-range n)])
                                 (random-ref dictionary rng))))
       (define pos (random 0 (add1 (string-length val)) rng))
       (set! extensions
             (cons (string-append (substring val 0 pos) combined (substring val pos))
                   extensions)))
     extensions]
    [(list? val)
     (for/list ([entry (in-list dictionary)])
       (append val (list entry)))]
    [else (list val)]))

;; ---------------------------------------------------------------------------
;; Structural list mutation: operates at the element level of the list
;; (e.g., entire operations in a list-of-operations input) rather than
;; mutating individual values within elements.

(define (mutate-list-structurally lst rng)
  (define len (length lst))
  (cond
    [(< len 2) lst]
    [else
     (define strategies
       (list
        ;; Delete a contiguous chunk of 1-5 elements
        (lambda ()
          (define chunk-size (min (add1 (random 0 5 rng)) len))
          (define start (random 0 (max 1 (- len chunk-size -1)) rng))
          (append (list-take lst start)
                  (list-drop lst (min len (+ start chunk-size)))))

        ;; Duplicate a contiguous chunk (extend the sequence)
        (lambda ()
          (define chunk-size (min (add1 (random 0 5 rng)) len))
          (define start (random 0 (max 1 (- len chunk-size -1)) rng))
          (define chunk
            (list-take (list-drop lst start) (min chunk-size (- len start))))
          (define insert-pos (random 0 (add1 len) rng))
          (append (list-take lst insert-pos) chunk (list-drop lst insert-pos)))

        ;; Swap two elements
        (lambda ()
          (define i (random 0 len rng))
          (define j (random 0 len rng))
          (if (= i j) lst
              (let ([a (list-ref lst i)] [b (list-ref lst j)])
                (define v (list->vector lst))
                (vector-set! v i b)
                (vector-set! v j a)
                (vector->list v))))

        ;; Replace one element with a copy of another
        (lambda ()
          (define src (random 0 len rng))
          (define dst (random 0 len rng))
          (if (= src dst) lst
              (append (list-take lst dst)
                      (list (list-ref lst src))
                      (list-drop lst (add1 dst)))))

        ;; For list-of-lists: mutate just the first element (selector) of one tuple
        (lambda ()
          (define idx (random 0 len rng))
          (define elem (list-ref lst idx))
          (if (and (list? elem) (>= (length elem) 1) (integer? (car elem)))
              (append (list-take lst idx)
                      (list (cons (random 0 34 rng) (cdr elem)))
                      (list-drop lst (add1 idx)))
              lst))))
     ((random-ref strategies rng))]))
