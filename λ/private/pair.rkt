#lang gnal λ/adt

(provide Pair pair fst snd pair-ref/bit
         pair-set-fst pair-set-snd pair-set/bit)

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

;; pair-set-fst : (Pairof A B) C -> (Pairof C B)
(define pair-set-fst
  (λ (p v)
    (match-adt Pair p
      [(pair a b)
       (pair v b)])))

;; pair-set-snd : (Pairof A B) C -> (Pairof A C)
(define pair-set-snd
  (λ (p v)
    (match-adt Pair p
      [(pair a b)
       (pair a v)])))

;; pair-ref/bit : (Pairof A B) (0-bit) -> A
;;                (Pairof A B) (1-bit) -> B
;;                (Pairof A A) Bit -> A
(define pair-ref/bit
  (λ (p b)
    (match-adt Bit b
      [(0-bit) (fst p)]
      [(1-bit) (snd p)])))

;; pair-set/bit : (Pairof A B) (0-bit) C -> (Pairof C B)
;;                (Pairof A B) (1-bit) C -> (Pairof A C)
;;                (Pairof A A) Bit A -> (Pairof A A)
(define pair-set/bit
  (λ (p b v)
    (match-adt Bit b
      [(0-bit) (pair-set-fst p v)]
      [(1-bit) (pair-set-snd p v)])))

