#lang gnal λ/adt

(provide Equal equal?)

(require "../../gnal-lambda/trait/trait.rkt")

(define-trait Equal
  (equal? τ))

