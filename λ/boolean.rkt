#lang gnal λ/adt

(provide Boolean show-Boolean true false and or not boolean=?)

(require "../gnal-lambda/show-adt.rkt"
         "private/boolean.rkt")

;; show-Boolean : Boolean -> Byte-String
(define show-Boolean
  (show-adt Boolean (true) (false)))

