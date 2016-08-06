#lang gnal λ/adt

(provide List empty cons empty? list=?)

(require "boolean.rkt")

;; A (Listof A) is one of:
;;  - (empty)
;;  - (cons A (Listof A))
(define-adt List
  (empty)
  (cons first rest))

;; empty? : (Listof A) -> Boolean
(define empty?
  (λ (lst)
    (match-adt List lst
      [(empty) (true)]
      [(cons first rest) (false)])))

;; list=? : [A A -> Boolean] -> [(Listof A) (Listof A) -> Boolean]
(define list=?
  (λ (elem=?)
    (λ (as bs)
      (match-adt List as
        [(empty) (empty? bs)]
        [(cons a as)
         (match-adt List bs
           [(empty) (false)]
           [(cons b bs)
            (match-adt Boolean (elem=? a b)
              [(true) ((list=? elem=?) as bs)]
              [(false) (false)])])]))))

