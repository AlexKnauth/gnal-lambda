#lang gnal λ/adt

(provide Maybe show-Maybe none some)

(require "../gnal-lambda/show-adt.rkt"
         "private/maybe.rkt")

;; show-Maybe : [A -> Byte-String] -> [(Maybe A) -> Byte-String]
(define show-Maybe
  (λ (show-A)
    (show-adt Maybe (none) (some show-A))))

