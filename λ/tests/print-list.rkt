#lang gnal λ/io

(require "../../gnal-lambda/show-adt.rkt")

(define a-byte
  (byte (1-bit) (0-bit) (0-bit) (0-bit) (0-bit) (1-bit) (1-bit) (0-bit)))
(define b-byte
  (byte (0-bit) (1-bit) (0-bit) (0-bit) (0-bit) (1-bit) (1-bit) (0-bit)))

;; A (Listof A) is one of:
;;  - (empty)
;;  - (cons A (Listof A))
(define-adt List
  (empty)
  (cons first rest))

(define show-list
  (λ (show-element)
    (show-adt List
      (empty)
      (cons show-element (show-list show-element)))))

(define show-bit
  (λ (b)
    (b
     (λ () (byte-string1 a-byte))
     (λ () (byte-string1 b-byte)))))

(define print-list
  (λ (show-element)
    (λ (lst)
      (display-byte-string ((show-list show-element) lst)))))

(define print-listof-bit
  (print-list show-bit))

(print-listof-bit (empty))
(print-listof-bit (cons (0-bit) (empty)))
(print-listof-bit (cons (1-bit) (empty)))
(print-listof-bit (cons (0-bit) (cons (0-bit) (empty))))
(print-listof-bit (cons (1-bit) (cons (0-bit) (empty))))
(print-listof-bit (cons (0-bit) (cons (1-bit) (empty))))
(print-listof-bit (cons (1-bit) (cons (1-bit) (empty))))

