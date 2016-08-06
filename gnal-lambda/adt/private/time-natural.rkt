#lang racket/base

(require "conversions.rkt"
         (prefix-in b1 (file "../../../λ/private/unary-natural.rkt"))
         (prefix-in b2 (file "../../../λ/private/base-2-natural.rkt"))
         (prefix-in b32 (file "../../../λ/private/base-32-natural.rkt"))
         )

(define b1-nats (build-vector 2000 rkt->natural))
(define b2-nats (build-vector 2000 rkt->b2-natural))
(define b32-nats (build-vector 2000 rkt->b32-natural))
(collect-garbage 'major)
(time
 (for* ([a (in-vector b1-nats)]
        [b (in-vector b1-nats)])
   (b1+ a b)))
(collect-garbage 'major)
(time
 (for* ([a (in-vector b2-nats)]
        [b (in-vector b2-nats)])
   (b2+ a b)))
(collect-garbage 'major)
(time
 (for* ([a (in-vector b32-nats)]
        [b (in-vector b32-nats)])
   (b32+ a b)))
