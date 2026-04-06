#lang racket/base

;; Coverage collection and diffing using errortrace's execute-counts API.
;;
;; Strategy: errortrace's execute counts are cumulative and cannot be reset.
;; We snapshot counts before running a test, snapshot again after, and diff
;; to determine what the single test execution contributed.
;;
;; Coverage points are normalized to (list source-path position span) for
;; stability — we do not rely on syntax object identity.
;;
;; Namespace isolation: the target module must be loaded in a namespace
;; where errortrace's compile handler is active and the module is compiled
;; from source (not loaded from a .zo). We achieve this by:
;; 1. Creating a namespace via make-base-namespace (shares the racket
;;    ecosystem so transitive deps don't need recompilation).
;; 2. Setting up errortrace INSIDE that namespace (so the compile handler
;;    captures the correct module registry).
;; 3. Overriding current-load/use-compiled to force source loading for
;;    the specific target module, while all other modules load normally.

(require racket/contract/base
         racket/set
         racket/path
         errortrace/errortrace-lib)

(provide
 (contract-out
  [make-instrumented-namespace (-> path-string? (values namespace? procedure?))]
  [snapshot-coverage (-> procedure? hash?)]
  [diff-coverage (-> hash? hash? hash?)]
  [coverage-signature (-> hash? set?)]
  [new-coverage? (-> set? set? boolean?)]
  [count-crosses-threshold? (-> hash? hash? boolean?)]
  [coverage-sig-hash (-> set? exact-integer?)]))

;; Create a namespace with errortrace instrumentation for the given target
;; module. Returns two values:
;; - the namespace (use it to dynamic-require functions from the target)
;; - a thunk that retrieves the current execute counts from that namespace
;;
;; Key: we do NOT attach errortrace from the current namespace, because
;; doing so transitively brings in whatever errortrace depends on. If the
;; target module is among those transitive deps (e.g. racket/treelist),
;; it would already be loaded and wouldn't go through our source-load
;; override. Instead, we set up errortrace's parameters directly from
;; the outer namespace's already-loaded errortrace module.
(define (make-instrumented-namespace target-path)
  (define target (simplify-path
                  (if (path? target-path) target-path
                      (string->path target-path))))

  (define ns (make-base-namespace))

  (parameterize ([current-namespace ns])
    ;; Install the load override FIRST, before anything that might
    ;; transitively load the target module.
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

    ;; Set up errortrace instrumentation using the outer namespace's
    ;; errortrace module (already loaded there). These are parameters
    ;; so setting them affects the new namespace's compilation.
    (execute-counts-enabled #t)
    (current-compile (make-errortrace-compile-handler)))

  ;; get-counts thunk — reads execute counts from the shared execute-info
  (define (get-counts)
    (get-execute-counts))

  (values ns get-counts))

;; Normalize a syntax object to a stable coverage key.
(define (stx->coverage-key stx)
  (define src (syntax-source stx))
  (define pos (syntax-position stx))
  (define span (syntax-span stx))
  (and src pos span
       (list (if (path? src) (path->string src) (format "~a" src))
             pos
             span)))

;; Snapshot the current execute counts as a hash from coverage-key to count.
;; Takes a get-counts thunk from make-instrumented-namespace.
(define (snapshot-coverage get-counts)
  (define counts (get-counts))
  (for/fold ([h (hash)])
            ([entry (in-list counts)])
    (define key (stx->coverage-key (car entry)))
    (if key
        (hash-set h key (max (cdr entry) (hash-ref h key 0)))
        h)))

;; Compute the difference between two snapshots.
;; Returns a hash from coverage-key to delta (only positive deltas).
(define (diff-coverage before after)
  (for/fold ([h (hash)])
            ([(key count) (in-hash after)])
    (define prev (hash-ref before key 0))
    (define delta (- count prev))
    (if (> delta 0)
        (hash-set h key delta)
        h)))

;; Extract the set of coverage keys that were exercised (had positive delta).
(define (coverage-signature diff)
  (list->set (hash-keys diff)))

;; Does a coverage signature contain any point not in the global set?
(define (new-coverage? sig global-coverage)
  (not (subset? sig global-coverage)))

;; Did any execution count cross a power-of-2 boundary?
(define (count-crosses-threshold? before after)
  (for/or ([(key count) (in-hash after)])
    (define prev (hash-ref before key 0))
    (and (> count prev)
         (let loop ([p 1])
           (cond
             [(> p count) #f]
             [(and (> p prev) (<= p count)) #t]
             [else (loop (* p 2))])))))

;; Compute a stable hash for a coverage signature (for novelty comparison).
(define (coverage-sig-hash sig)
  (define sorted (sort (set->list sig)
                       (lambda (a b)
                         (string<? (format "~a" a) (format "~a" b)))))
  (equal-hash-code sorted))
