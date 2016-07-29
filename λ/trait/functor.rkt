#lang gnal λ/adt

(provide Functor map)

(require "../../gnal-lambda/trait/trait.rkt")

(define-trait Functor
  (map τ))

