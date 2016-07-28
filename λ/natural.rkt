#lang gnal λ/adt

(provide Natural show-Natural zero succ)

(require "../gnal-lambda/show-adt.rkt"
         "private/natural.rkt")

;; show-Natural : Natural -> Byte-String
(define show-Natural
  (show-adt Natural
    (zero)
    (succ show-Natural)))

