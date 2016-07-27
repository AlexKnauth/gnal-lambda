#lang racket/base

(provide define-adt match-adt)

(require "lc.rkt"
         "define.rkt"
         syntax/parse/define
         (only-in racket/base [begin -splice])
         (for-syntax racket/base racket/match syntax/stx))

(begin-for-syntax
  (struct adt-type-info (name variants))
  (define-syntax-class adt-type-id
    [pattern type-id:id
             #:attr info (syntax-local-value #'type-id)
             #:fail-unless (adt-type-info? (attribute info))
             "expected an identifier with adt-type-info attached"])
  (define-syntax-class variant-pat
    [pattern (variant:id field:id ...)
             #:attr field-count (length (syntax->list #'[field ...]))])
  (define-syntax-class variant-clause
    #:attributes (pat pat.variant pat.field-count [pat.field 1] body)
    [pattern [pat:variant-pat body:expr]])

  (define disappeared-use 'disappeared-use)
  (define (add-disappeared-uses stx ids)
    (syntax-property
     stx
     disappeared-use
     (append (stx-map syntax-local-introduce ids) (syntax-property stx disappeared-use))))
  )

(define-simple-macro
  (define-adt type:id
    (variant:id field:id ...)
    ...)
  #:with [do-variant-thing:id ...] (generate-temporaries #'[variant ...])
  (-splice
   (define-syntax type
     (adt-type-info 'type (list (list #'variant 'field ...) ...)))
   (define variant
     (λ (field ...)
       (λ (do-variant-thing ...)
         (do-variant-thing field ...))))
   ...))

(define-syntax match-adt
  (lambda (stx)
    (syntax-parse stx
      [(match-adt adt-type:adt-type-id v:expr clause:variant-clause ...)
       #:do [(define given-variant-clauses
               (syntax->list #'[clause ...]))
             (define type-variants
               (adt-type-info-variants (attribute adt-type.info)))]
       #:with [variant-matcher ...]
       (for/list ([type-variant (in-list type-variants)]
                  [given-clause (in-list given-variant-clauses)])
         (match-define (list-rest type-variant-id type-fields) type-variant)
         (define/syntax-parse clause:variant-clause given-clause)
         (unless (free-identifier=? #'clause.pat.variant type-variant-id)
           (raise-syntax-error (syntax-e #'match-adt)
                               (format "expected the variant ~a" (syntax-e type-variant-id))
                               stx
                               #'clause.pat.variant))
         (unless (= (attribute clause.pat.field-count) (length type-fields))
           (raise-syntax-error (syntax-e #'match-adt)
                               (format "expected ~v fields for the variant ~a"
                                       (length type-fields) (syntax-e #'clause.pat.variant))
                               stx
                               #'clause.pat))
         (syntax/loc #'clause
           (λ (clause.pat.field ...) clause.body)))
       ;; this uses #%app from "lc.rkt"
       (add-disappeared-uses
        #'(v variant-matcher ...)
        #'[adt-type clause.pat.variant ...])])))

