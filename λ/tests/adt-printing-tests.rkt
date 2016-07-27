#lang gnal λ/io

;; --------------------------------
;; Pairs

(define-adt Pair
  (pair pair-fst pair-snd))

;; a-byte : Byte
;; b-byte : Byte
(define a-byte (byte (1-bit) (0-bit) (0-bit) (0-bit) (0-bit) (1-bit) (1-bit) (0-bit)))
(define b-byte (byte (0-bit) (1-bit) (0-bit) (0-bit) (0-bit) (1-bit) (1-bit) (0-bit)))
;; a : (IO Unit)
;; b : (IO Unit)
(define a (display-byte-string (byte-string1 a-byte)))
(define b (display-byte-string (byte-string1 b-byte)))

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

