#lang gnal λ/adt

(provide Natural Natural-τ show-Natural natural=?
         zero succ zero?
         add1 + * ^
         quotient remainder
         n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10
         )

(require "../gnal-lambda/show-adt.rkt"
         "../gnal-lambda/trait/trait.rkt"
         "trait/show.rkt"
         "trait/equal.rkt"
         "private/unary-natural.rkt")

;; show-Natural : Natural -> Byte-String
(define show-Natural
  (show-adt Natural
    (zero)
    (succ show-Natural)))

(define Natural-τ
  (interp
    (trait-impl Show
      [show show-Natural])
    (trait-impl Equal
      [equal? natural=?])))

