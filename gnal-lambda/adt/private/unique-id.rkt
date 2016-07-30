#lang racket/base

(provide unique-id id=?)

(require (file "../../../λ/private/unique-id.rkt")
         (only-in (file "../../../λ/private/boolean.rkt") true false)
         (only-in "conversions.rkt" rkt->boolean boolean->rkt)
         (only-in racket/match match*)
         (only-in racket/bool symbol=?)
         (only-in racket/set seteq set-add set-member?)
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     ))
(module+ test
  (require (submod "conversions.rkt" testing-forms)))

;; (define-type From (U Unique-Id-Internal-State Returned-From))
(struct returned-from (from) ; from : From
  #:property prop:procedure
  (λ (this) (returned-from this)))

(struct unique-id-internal-state (symbol)
  #:property prop:procedure
  (λ (this) (returned-from this)))

;; internal-state=? : Internal Internal -> Boolean
(define (internal-state=? a b)
  (match* [a b]
    [[(unique-id-internal-state as)
      (unique-id-internal-state bs)]
     (rkt->boolean (symbol=? as bs))]
    [[(returned-from _) _] (false)]
    [[_ (returned-from _)] (false)]
    [[ap bp]
     (from=? (ap) (bp) (seteq a b))]))

;; unique-id/internal : Internal -> Unique-ID
(define unique-id/internal (unique-id/internal-state internal-state=?))

;; unique-id/symbol : Symbol -> Unique-ID
(define (unique-id/symbol sym)
  (unique-id/internal (unique-id-internal-state sym)))

(define-syntax unique-id
  (lambda (stx)
    (syntax-parse stx
      [(unique-id)
       #:do [(define stuff
               (list (syntax-source stx)
                     (syntax-line stx)
                     (syntax-column stx)
                     (syntax-position stx)
                     (syntax-span stx)
                     ;(syntax-debug-info stx) ; requires Racket version 6.3 or later
                     (syntax-local-name)
                     (syntax-local-context)
                     (syntax-local-phase-level)))]
       #:with internal-id
       (generate-temporary
        (format-id #f "unique-id~a~a" (equal-hash-code stuff) (equal-secondary-hash-code stuff)))
       #'(unique-id/symbol 'internal-id)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set-add2 : (Setof Any) Any Any -> (Setof Any)
(define (set-add2 st a b)
  (set-add (set-add st a) b))

;; from=? : From From (Setof Any) -> Boolean
(define (from=? a b seen)
  (cond
    [(set-member? seen a) (false)]
    [(set-member? seen b) (false)]
    [(eq? a b) (true)]
    [else
     (match* [a b]
       [[(unique-id-internal-state as)
         (unique-id-internal-state bs)]
        (rkt->boolean (symbol=? as bs))]
       [[(returned-from ac)
         (returned-from bc)]
        (from=? ac bc (set-add2 seen a b))]
       [[(unique-id-internal-state _) (returned-from _)]
        (false)]
       [[(returned-from _) (unique-id-internal-state _)]
        (false)]
       [[ap bp]
        (from=? (ap) (bp) (set-add2 seen a b))])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define a (unique-id))
  (define b (unique-id))
  (define a* (λ (x) (a x)))
  (define b* (λ (x) (b x)))
  (define a** (λ (x) (a* (λ (y z) (x y z)))))
  (define b** (λ (x) (b* (λ (y z) (x y z)))))

  (check-false (id=? a b))
  (check-false (id=? b a))
  (check-false (id=? a b*))
  (check-false (id=? b a*))
  (check-false (id=? a b**))
  (check-false (id=? b a**))
  (check-false (id=? a* b))
  (check-false (id=? b* a))
  (check-false (id=? a* b*))
  (check-false (id=? b* a*))
  (check-false (id=? a* b**))
  (check-false (id=? b* a**))
  (check-false (id=? a** b))
  (check-false (id=? b** a))
  (check-false (id=? a** b*))
  (check-false (id=? b** a*))
  (check-false (id=? a** b**))
  (check-false (id=? b** a**))

  (check-true (id=? a a))
  (check-true (id=? b b))
  (check-true (id=? a a*))
  (check-true (id=? b b*))
  (check-true (id=? a a**))
  (check-true (id=? b b**))
  (check-true (id=? a* a))
  (check-true (id=? b* b))
  (check-true (id=? a* a*))
  (check-true (id=? b* b*))
  (check-true (id=? a* a**))
  (check-true (id=? b* b**))
  (check-true (id=? a** a))
  (check-true (id=? b** b))
  (check-true (id=? a** a*))
  (check-true (id=? b** b*))
  (check-true (id=? a** a**))
  (check-true (id=? b** b**))

  (define c-internal (λ () c-internal))
  (define c (unique-id/internal c-internal))
  (check-false (id=? a c))
  (check-false (id=? b c))
  (check-false (id=? c a))
  (check-false (id=? c b))
  (check-false (id=? c c))
  )


