#lang gnal λ/adt

(provide Natural zero succ zero? natural=?
         apply-n-times
         add1 + * ^
         ?sub1 ?∆
         quotient remainder
         n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10
         ;; private
         natural->rkt)

(require "boolean.rkt"
         "maybe.rkt")

;; A Natural is one of:
;;  - (zero)
;;  - (succ Natural)
;; zero : -> Natural
;; succ : Natural -> Natural
(define-adt Natural
  (zero)
  (succ n))

;; zero? : Natural -> Boolean
(define zero?
  (λ (n)
    (match-adt Natural n
      [(zero) (true)]
      [(succ n) (false)])))

;; natural=? : Natural Natural -> Boolean
(define natural=?
  (λ (a b)
    (match-adt Natural a
      [(zero) (zero? b)]
      [(succ a-1)
       (match-adt Natural b
         [(zero) (false)]
         [(succ b-1) (natural=? a-1 b-1)])])))

;; add1 : Natural -> Natural
(define add1 succ)

;; a few Naturals
(define n0 (zero))
(define n1 (add1 n0))
(define n2 (add1 n1))
(define n3 (add1 n2))
(define n4 (add1 n3))
(define n5 (add1 n4))
(define n6 (add1 n5))
(define n7 (add1 n6))
(define n8 (add1 n7))
(define n9 (add1 n8))
(define n10 (add1 n9))

;; apply-n-times : Natural [A -> A] A -> A
(define apply-n-times
  (λ (n f a)
    (match-adt Natural n
      [(zero) a]
      [(succ n-1) (apply-n-times n-1 f (f a))])))

;; + : Natural Natural -> Natural
(define +
  (λ (a b)
    (apply-n-times b add1 a)))

(define *
  (λ (a b)
    (apply-n-times b (λ (x) (+ x a)) n0)))

(define ^
  (λ (a b)
    (apply-n-times b (λ (x) (* x a)) n1)))

;; ?sub1 : Natural -> (Maybe Natural)
(define ?sub1
  (λ (a)
    (match-adt Natural a
      [(zero) (none)]
      [(succ a-1) (some a-1)])))

;; ?∆ : Natural Natural -> (Maybe Natural)
;; (?∆ a b) contains b - a when a < b, none otherwise
(define ?∆
  (λ (a b)
    (match-adt Natural a
      [(zero) (some b)]
      [(succ a-1)
       (opt-bind
        (?sub1 b)
        (λ (b-1)
          (?∆ a-1 b-1)))])))

;; quotient : Natural Natural -> Natural
(define quotient
  (λ (m n)
    (match-adt Maybe (?∆ n m)
      [(none) n0]
      [(some n∆m)
       (add1 (quotient n∆m n))])))

;; remainder : Natural Natural -> Natural
(define remainder
  (λ (m n)
    (match-adt Maybe (?∆ n m)
      [(none) m]
      [(some n∆m)
       (remainder n∆m n)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; private

(define natural->rkt
  (λ (zero* add1*)
    (λ (n)
      (apply-n-times n add1* zero*))))

