#lang gnal λ/adt

;; --------------------------------
;; Pairs

(define-adt Pair
  (pair pair-fst pair-snd))

(define a (λ (x) x))
(define b (λ (x) x))

(match-adt Pair (pair a b) [(pair x y) x])
(match-adt Pair (pair a b) [(pair x y) y])

;; --------------------------------
;; Booleans

(define-adt Boolean
  (make-true)
  (make-false))
(define true (make-true))
(define false (make-false))

(match-adt Boolean true [(make-true) a] [(make-false) b])
(match-adt Boolean false [(make-true) a] [(make-false) b])

;; --------------------------------
;; Lists

(define-adt List
  (make-empty)
  (cons first rest))
(define empty (make-empty))

(match-adt List empty [(make-empty) a] [(cons x xs) b])
(match-adt List (cons a empty) [(make-empty) a]  [(cons x xs) b])
(match-adt List (cons a empty) [(make-empty) b]  [(cons x xs) x])
(match-adt List (match-adt List (cons a empty) [(make-empty) a]  [(cons x xs) xs])
  [(make-empty) b] [(cons x xs) a])

