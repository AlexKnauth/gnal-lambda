#lang racket/base

(require "conversions.rkt"
         (prefix-in b2 (file "../../../λ/private/base-2-natural.rkt"))
         (prefix-in b32 (file "../../../λ/private/base-32-natural.rkt"))
         (prefix-in b2 (file "../../../λ/private/pair-tree.rkt"))
         (prefix-in b32 (file "../../../λ/private/vector.rkt"))
         )

(define n 10000)
(define b2-nats (build-vector n rkt->b2-natural))
(define b32-nats (build-vector n rkt->b32-natural))

;; Base 2
(collect-garbage)
(define v2
  (time
   (for/fold ([acc b2empty-vector])
             ([a (in-range n)])
     (b2vector-conj acc a))))
(time
 (for ([a (in-vector b2-nats)])
   (b2vector-nth v2 a)))

;; Base 32
(collect-garbage)
(define v32
  (time
   (for/fold ([acc b32empty-vector])
             ([a (in-range n)])
     (b32vector-conj acc a))))
(time
 (for ([a (in-vector b32-nats)])
   (b32vector-nth v32 a)))

