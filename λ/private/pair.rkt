#lang gnal λ/adt

(provide Pair pair fst snd pair-ref/bit)

(require "byte.rkt")

(define-adt Pair
  (pair fst snd))

;; fst : (Pairof A B) -> A
(define fst
  (λ (p)
    (match-adt Pair p
      [(pair fst snd) fst])))

;; snd : (Pairof A B) -> B
(define snd
  (λ (p)
    (match-adt Pair p
      [(pair fst snd) snd])))

;; pair-ref/bit : (Pairof A B) (0-bit) -> A
;;                (Pairof A B) (1-bit) -> B
;;                (Pairof A A) Bit -> A
(define pair-ref/bit
  (λ (p b)
    (match-adt Bit b
      [(0-bit) (fst p)]
      [(1-bit) (snd p)])))

