#lang gnal λ/adt

(provide Maybe none some none? some?
         opt-pure
         opt-bind
         opt-or
         opt-and
         )

(require "boolean.rkt")

;; A (Maybe A) is one of:
;;  - (none)
;;  - (some A)
;; none : -> (Maybe A)
;; some : A -> (Maybe A)
(define-adt Maybe
  (none)
  (some a))

;; none? : (Maybe A) -> Boolean
(define none?
  (λ (opt-v)
    (match-adt Maybe opt-v
      [(none) (true)]
      [(some v) (false)])))

;; some? : (Maybe A) -> Boolean
(define some?
  (λ (opt-v)
    (match-adt Maybe opt-v
      [(none) (false)]
      [(some v) (true)])))

;; opt-pure : A -> (Maybe A)
(define opt-pure some)

;; opt-bind : (Maybe A) [A -> (Maybe B)] -> (Maybe B)
(define opt-bind
  (λ (opt-a next)
    (match-adt Maybe opt-a
      [(none) (none)]
      [(some a) (next a)])))

;; opt-or : (Maybe A) (Maybe A) -> (Maybe A)
(define opt-or
  (λ (opt-a opt-b)
    (match-adt Maybe opt-a
      [(none) opt-b]
      [(some a) opt-a])))

;; opt-and : (Maybe A) (Maybe A) -> (Maybe A)
(define opt-and
  (λ (opt-a opt-b)
    (match-adt Maybe opt-a
      [(none) (none)]
      [(some a) opt-b])))

