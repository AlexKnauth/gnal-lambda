#lang gnal λ/adt

(provide Boolean Boolean-τ show-Boolean true false and or not boolean=?)

(require "../gnal-lambda/show-adt.rkt"
         "../gnal-lambda/trait/trait.rkt"
         "trait/show.rkt"
         "trait/equal.rkt"
         "private/boolean.rkt")

;; show-Boolean : Boolean -> Byte-String
(define show-Boolean
  (show-adt Boolean (true) (false)))

(define Boolean-τ
  (type-implements
    (trait-impl Show
      [show show-Boolean])
    (trait-impl Equal
      [equal? boolean=?])))

