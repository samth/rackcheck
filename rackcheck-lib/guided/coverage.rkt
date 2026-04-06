#lang racket/base

;; Coverage tracking for coverage-guided testing.
;;
;; Uses AFL-style bitmap coverage: each instrumented expression in the
;; target module maps to one byte in a bitmap. Each bit represents a
;; count bucket (1, 2, 3, 4-7, 8-15, 16-31, 32-127, 128+). New
;; coverage = any test that sets a bit not previously set in the global
;; bitmap.
;;
;; Coverage is checked per BATCH, not per test. A batch of N tests
;; runs between a single snapshot-before and snapshot-after, amortizing
;; the snapshot cost.
;;
;; Key optimization: after compilation, we extract just the target
;; file's count boxes from errortrace's execute-info hash. This is a
;; one-time O(all_points) scan. All subsequent operations are
;; O(target_points) with no allocation.

(require racket/path
         racket/fixnum
         racket/unsafe/ops
         errortrace/errortrace-lib)

(provide
 make-instrumented-namespace   ; -> (values namespace? target-coverage-info?)
 target-coverage-info?
 target-coverage-info-num-points
 snapshot-target!              ; -> fxvector?
 compute-batch-bitmap!         ; fxvector? -> bytes?
 bitmap-has-new-coverage?      ; bytes? bytes? -> boolean?
 merge-bitmap!                 ; bytes? bytes? -> void?
 count-new-bits                ; bytes? bytes? -> exact-nonneg-integer?
 coverage-summary              ; target-coverage-info? -> hash?
 target-coverage-info-global-bitmap
 target-coverage-info-boxes
 )

;; ---------------------------------------------------------------------------
;; Target coverage info: the result of instrumenting a module.

(struct target-coverage-info
  (boxes          ; (vectorof box?) — direct refs to errortrace count boxes
   num-points     ; exact-nonneg-integer?
   global-bitmap  ; bytes? — one byte per point, accumulates bucket bits
   snap-buffer    ; fxvector? — reusable buffer for snapshots
   batch-buffer)  ; bytes? — reusable buffer for batch bitmaps
  #:transparent)

;; ---------------------------------------------------------------------------
;; Namespace setup and target box extraction

(define (make-instrumented-namespace target-path)
  (define target (simplify-path
                  (if (path? target-path) target-path
                      (string->path target-path))))

  (define ns (make-base-namespace))

  (parameterize ([current-namespace ns])
    ;; Install load override FIRST
    (define orig-load/use-compiled (current-load/use-compiled))
    (define target-loaded? #f)
    (current-load/use-compiled
     (lambda (path expected-module)
       (if (and (path? path)
                (not target-loaded?)
                (equal? (simplify-path path) target))
           (begin
             (set! target-loaded? #t)
             (parameterize ([current-load-relative-directory (path-only path)])
               ((current-load) path expected-module)))
           (orig-load/use-compiled path expected-module))))

    ;; Set up errortrace from outer namespace
    (execute-counts-enabled #t)
    (current-compile (make-errortrace-compile-handler))

    ;; Load the target module — this triggers source compilation
    ;; through our override, which populates execute-info.
    (namespace-require (if (path? target-path)
                           target-path
                           (string->path target-path))))

  ;; Extract target boxes from execute-info.
  ;; execute-info is a hasheq: gensym -> (cons syntax-object (box count))
  ;; We filter to entries whose syntax-source matches the target path.
  (define boxes '())
  (hash-for-each execute-info
    (lambda (k v)
      (define stx (car v))
      (define bx (cdr v))
      (define src (syntax-source stx))
      (when (and (path? src) (equal? (simplify-path src) target))
        (set! boxes (cons bx boxes)))))
  (define boxes-vec (list->vector boxes))
  (define n (vector-length boxes-vec))

  (define tci
    (target-coverage-info
     boxes-vec
     n
     (make-bytes n 0)       ; global bitmap
     (make-fxvector n 0)    ; snapshot buffer
     (make-bytes n 0)))     ; batch bitmap buffer

  (values ns tci))

;; ---------------------------------------------------------------------------
;; AFL-style count bucket classification
;;
;; Maps an execution count delta to a single bit in a byte.
;; 8 buckets = 8 bits = one byte per expression.

(define (count->bucket delta)
  (cond
    [(<= delta 0) 0]
    [(= delta 1)  1]    ; bit 0
    [(= delta 2)  2]    ; bit 1
    [(= delta 3)  4]    ; bit 2
    [(<= delta 7) 8]    ; bit 3
    [(<= delta 15) 16]  ; bit 4
    [(<= delta 31) 32]  ; bit 5
    [(<= delta 127) 64] ; bit 6
    [else 128]))         ; bit 7

;; ---------------------------------------------------------------------------
;; Snapshot: read all target boxes into the reusable fxvector buffer.

(define (snapshot-target! tci)
  (define boxes (target-coverage-info-boxes tci))
  (define buf (target-coverage-info-snap-buffer tci))
  (define n (target-coverage-info-num-points tci))
  (for ([i (in-range n)])
    (unsafe-fxvector-set! buf i (unbox (vector-ref boxes i))))
  buf)

;; ---------------------------------------------------------------------------
;; Compute batch bitmap: for each expression, compute the delta between
;; the current count and the snapshot, classify into a bucket bit.

(define (compute-batch-bitmap! tci before-snap)
  (define boxes (target-coverage-info-boxes tci))
  (define buf (target-coverage-info-batch-buffer tci))
  (define n (target-coverage-info-num-points tci))
  (for ([i (in-range n)])
    (define current (unbox (vector-ref boxes i)))
    (define prev (unsafe-fxvector-ref before-snap i))
    (define delta (- current prev))
    (bytes-set! buf i (count->bucket delta)))
  buf)

;; ---------------------------------------------------------------------------
;; Check if the batch bitmap has any new coverage vs the global bitmap.
;; "New" = any bit set in test-byte that is NOT set in global-byte.

(define (bitmap-has-new-coverage? test-bitmap global-bitmap)
  (define n (bytes-length test-bitmap))
  (for/or ([i (in-range n)])
    (define test-byte (bytes-ref test-bitmap i))
    (define global-byte (bytes-ref global-bitmap i))
    ;; New bits = bits in test that aren't in global
    (not (zero? (bitwise-and test-byte (bitwise-xor test-byte
                                                     (bitwise-and test-byte global-byte)))))))

;; ---------------------------------------------------------------------------
;; Merge batch bitmap into global bitmap.

(define (merge-bitmap! test-bitmap global-bitmap)
  (define n (bytes-length test-bitmap))
  (for ([i (in-range n)])
    (bytes-set! global-bitmap i
                (bitwise-ior (bytes-ref global-bitmap i)
                             (bytes-ref test-bitmap i)))))

;; ---------------------------------------------------------------------------
;; Count how many positions have new bits (not already in global).

(define (count-new-bits test-bitmap global-bitmap)
  (define n (bytes-length test-bitmap))
  (for/sum ([i (in-range n)])
    (define test-byte (bytes-ref test-bitmap i))
    (define global-byte (bytes-ref global-bitmap i))
    (if (not (zero? (bitwise-and test-byte (bitwise-xor test-byte
                                                         (bitwise-and test-byte global-byte)))))
        1 0)))

;; ---------------------------------------------------------------------------
;; Coverage summary for reporting.

(define (coverage-summary tci)
  (define global (target-coverage-info-global-bitmap tci))
  (define n (target-coverage-info-num-points tci))
  (define covered
    (for/sum ([i (in-range n)])
      (if (> (bytes-ref global i) 0) 1 0)))
  (hash 'covered covered 'total n
        'percent (if (zero? n) 0
                     (exact->inexact (* 100.0 (/ covered n))))))
