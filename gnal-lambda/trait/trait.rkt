#lang racket/base

(provide define-trait
         type-implements
         trait-impl
         )

(require "../lc/adt.rkt"
         (file "../../λ/private/boolean.rkt")
         "../adt/private/unique-id.rkt"
         (only-in racket/base [begin -splice])
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     ))

(begin-for-syntax
  (struct trait-info (name internal-id make-impl-id xs))
  (define-syntax-class trait-id
    [pattern trait-id:id
             #:attr info (syntax-local-value #'trait-id (λ () '#f))
             #:fail-unless (trait-info? (attribute info))
             "expected an identifier with trait-info attached"])

  (define disappeared-use 'disappeared-use)
  (define (add-disappeared-uses stx ids)
    (syntax-property
     stx
     disappeared-use
     (append (stx-map syntax-local-introduce ids) (syntax-property stx disappeared-use))))
  )

(define-syntax define-trait
  (syntax-parser #:datum-literals (τ)
    [(define-trait trait:id
       (x:id τ)
       ...)
     #:with trait-internal (generate-temporary 'trait-internal)
     #:with Trait-Impl (generate-temporary 'Trait-Impl)
     #:with make-trait-impl (generate-temporary 'make-trait-impl)
     #:with given-τ (generate-temporary 'given-τ)
     #'(-splice
        (define trait-internal (unique-id))
        (define-adt Trait-Impl
          (make-trait-impl x ...))
        (define-syntax trait
          (trait-info 'trait #'trait-internal #'make-trait-impl (list #'x ...)))
        (define x
          (λ (given-τ)
            (match-adt Trait-Impl (given-τ trait-internal)
              [(make-trait-impl x ...) x])))
        ...)]))

(define-syntax type-implements
  (syntax-parser #:literals (trait-impl)
    [(type-implements
       (~and impl (trait-impl trait:id [x:id x-impl:expr] ...))
       ...)
     #'(λ (given-trait-internal)
          (trait-case given-trait-internal
            [trait impl]
            ...
            [else (error '"no typeclass implementation")]))]))

(define-syntax trait-impl
  (lambda (stx)
    (syntax-parse stx
      [(trait-impl trait:trait-id [x:id x-impl:expr] ...)
       #:with make-impl (trait-info-make-impl-id (attribute trait.info))
       #:with [trait-x-impl ...]
       (for/list ([trait-x (in-list (trait-info-xs (attribute trait.info)))]
                  [x (in-list (syntax->list #'[x ...]))]
                  [x-impl (in-list (syntax->list #'[x-impl ...]))])
         (unless (free-identifier=? x trait-x)
           (raise-syntax-error (syntax-e #'trait-impl)
                               (format "expected the identifier ~a" (syntax-e trait-x))
                               stx
                               x))
         x-impl)
       (add-disappeared-uses
        #'(make-impl trait-x-impl ...)
        #'[trait x ...])])))

(define-syntax trait-case
  (syntax-parser #:literals (else)
    [(trait-case given-trait-internal
       [else else-body:expr])
     #'else-body]
    [(trait-case given-trait-internal
       [match-trait:trait-id body:expr]
       rst ...+)
     #:with match-trait-internal (trait-info-internal-id (attribute match-trait.info))
     #'(match-adt Boolean (id=? given-trait-internal match-trait-internal)
         [(true) body]
         [(false) (trait-case given-trait-internal rst ...)])]))

