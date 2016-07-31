#lang gnal λ/adt

(provide List Listof empty cons)

(require "../gnal-lambda/show-adt.rkt"
         "../gnal-lambda/trait/trait.rkt"
         "private/boolean.rkt"
         "trait/show.rkt"
         "trait/equal.rkt"
         "trait/functor.rkt"
         )

;; A (Listof A) is one of:
;;  - (empty)
;;  - (cons A (Listof A))
(define-adt List
  (empty)
  (cons first rest))

(define map-List
  (λ (f lst)
    (match-adt List lst
      [(empty) (empty)]
      [(cons first rest)
       (cons (f first) (map-List f rest))])))

(define show-Listof
  (λ (A)
    (show-adt List
      (empty)
      (cons (show A) (show-Listof A)))))

(define equal?-Listof
  (λ (A)
    (λ (as bs)
      (match-adt List as
        [(empty)
         (match-adt List bs
           [(empty) (true)]
           [(cons b bs) (false)])]
        [(cons a as)
         (match-adt List bs
           [(empty) (false)]
           [(cons b bs)
            (match-adt Boolean ((equal? A) a b)
              [(true) ((equal?-Listof A) as bs)]
              [(false) (false)])])]))))

(define Listof
  (λ (A)
    (interp
      (trait-impl Functor
        [map map-List])
      (trait-impl Equal
        [equal? (equal?-Listof A)])
      (trait-impl Show
        [show (show-Listof A)]))))

