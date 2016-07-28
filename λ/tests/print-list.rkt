#lang gnal λ/io

(require "../../gnal-lambda/show-adt.rkt"
         "../byte.rkt")

;; A (Listof A) is one of:
;;  - (empty)
;;  - (cons A (Listof A))
(define-adt List
  (empty)
  (cons first rest))

(define show-List
  (λ (show-A)
    (show-adt List
      (empty)
      (cons show-A (show-List show-A)))))

(define print-List
  (λ (show-A)
    (λ (lst)
      (display-byte-string ((show-List show-A) lst)))))

(define print-listof-bit
  (print-List show-Bit))

(print-listof-bit (empty))
(print-listof-bit (cons (0-bit) (empty)))
(print-listof-bit (cons (1-bit) (empty)))
(print-listof-bit (cons (0-bit) (cons (0-bit) (empty))))
(print-listof-bit (cons (1-bit) (cons (0-bit) (empty))))
(print-listof-bit (cons (0-bit) (cons (1-bit) (empty))))
(print-listof-bit (cons (1-bit) (cons (1-bit) (empty))))

