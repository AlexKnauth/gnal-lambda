#lang gnal λ/adt

;; A functional vector library inspired by Alexis King's data/pvector library.

(provide empty-vector
         make-vector
         build-vector
         ;; Equal
         vector=?
         ;; Countable
         vector-length
         ;; Collection
         vector-conj
         ;; Sequence
         vector-nth
         ;vector-set-nth
         vector-append
         )

(require "boolean.rkt"
         "natural.rkt"
         "v32.rkt"
         )

;; BRANCHING-FACTOR = 2^5 = 32
(define BRANCHING-FACTOR (^ n2 n5))
(define BRANCHING-FACTOR-1
  (match-adt Natural BRANCHING-FACTOR
    [(zero) (zero)]
    [(succ BRANCHING-FACTOR-1) BRANCHING-FACTOR-1]))

(define ??? (λ (f) (f)))
(define v32???
  (make-v32 ???))

;; A (Vectorof A) is a (vector-internal Natural (VNodeof A depth))
;; A (VNodeof A 0) is a
;;   (U ??? A)
;; A (VNodeof A 1) is a
;;   (vnode (VNodeof (V32of A) 0)
;;          (V32of? A))
;; A (VNodeof A 2) is a
;;   (vnode (VNodeof (V32of A) 1)
;;          (V32of? A))
;; A (VNodeof A (add1 d)) is a
;;   (vnode (VNodeof (V32of A) d)
;;          (V32of? A))
;; [] ; n = 0, q = 0, r = 0, d = 0
;;   = (vector-internal n0
;;       ???)
;; [a0] ; n = 1, q = 0, r = 1, d = 0
;;   = (vector-internal n1
;;       a0)
;; [a0 a1] ; n = 2, q = 0, r = 2, d = 1
;;   = (vector-internal n2
;;       (vnode
;;         ???
;;         (v32 a0 a1 ??? ...)))
;; [a0 ... a30] ; n = 31, q = 0, r = 31, d = 1
;;   = (vector-internal n31
;;       (vode
;;         ???
;;         (v32 a0 ... a30 ???)))
;; [a0 ... a31] ; n = 32, q = 1, r = 0, d = 1
;;   = (vector-internal n32
;;       (vnode
;;         (v32 a0 ... a31)
;;         (v32 ??? ...)))
;; [a0 ... a32] ; n = 33, q = 1, r = 1, d = 1
;;   = (vector-internal n33
;;       (vnode
;;         (v32 a0 ... a31)
;;         (v32 a32 ??? ...)))
;; [a0 ... a33] ; n = 34, q = 1, r = 2, d = 1
;;   = (vector-internal n34
;;       (vnode
;;         (v32 a0 ... a32)
;;         (v32 a32 a33 ??? ...)))
;; [a0 ... a62] ; n = 63, q = 1, r = 31, d = 1
;;   = (vector-internal n63
;;       (vnode
;;         (v32 a0 ... a31)
;;         (v32 a32 ... a62 ???)))
;; [a0 ... a63] ; n = 64, q = 2, r = 0, d = 2
;;   = (vector-internal n64
;;       (vnode
;;         (vnode
;;           ???
;;           (v32 (v32 a0 ... a31) (v32 a32 ... a63) ??? ...))
;;         (v32 ??? ...)))
;; [a0 ... a64] ; n = 65, q = 2, r = 1, d = 2
;;   = (vector-internal n65
;;       (vnode
;;         (vnode
;;           ???
;;           (v32 (v32 a0 ... a31) (v32 a32 ... a63) ??? ...))
;;         (v32 a64 ??? ...)))
;; [a0 ... a65] ; n = 66, q = 2, r = 2, d = 2
;;   = (vector-internal n66
;;       (vnode
;;         (vnode
;;           ???
;;           (v32 (v32 a0 ... a31) (v32 a32 ... a63) ??? ...))
;;         (v32 a64 a65 ??? ...)))
;; [a0 ... a94] ; n = 95, q = 2, r = 31, d = 2
;;   = (vector-internal n95
;;       (vnode
;;         (vnode
;;           ???
;;           (v32 (v32 a0 ... a31) (v32 a32 ... a63) ??? ...))
;;         (v32 a64 ... a94 ???)))
;; [a0 ... a95] ; n = 96, q = 3, r = 0, d = 2
;;   = (vector-internal n96
;;       (vnode
;;         (vnode
;;           ???
;;           (v32 (v32 a0 ... a31) (v32 a32 ... a63) (v32 a64 ... a95) ??? ...))
;;         (v32 ??? ...)))
;; [a0 ... a96] ; n = 97, q = 3, r = 1, d = 2
;;   = (vector-internal n97
;;       (vnode
;;         (vnode
;;           ???
;;           (v32 (v32 a0 ... a31) (v32 a32 ... a63) (v32 a64 ... a95) ??? ...))
;;         (v32 a96 ??? ...)))
;; [a0 ... a97] ; n = 98, q = 3, r = 2, d = 2
;;   = (vector-internal n98
;;       (vnode
;;         (vnode
;;           ???
;;           (v32 (v32 a0 ... a31) (v32 a32 ... a63) (v32 a64 ... a95) ??? ...))
;;         (v32 a96 a97 ??? ...)))
;; [a0 ... a991] ; n = 992, q = 31, r = 0, d = 2
;;   = (vector-internal n992
;;       (vnode
;;         (vnode
;;           ???
;;           (v32 (v32 a0 ... a31)
;;                (v32 a32 ... a63)
;;                ...
;;                (v32 a960 ... a991)
;;                ???)))
;;         (v32 ??? ...)))
;; [a0 ... a992] ; n = 993, q = 31, r = 1, d = 2
;;   = (vector-internal n993
;;       (vnode
;;         (vnode
;;           ???
;;           (v32 (v32 a0 ... a31)
;;                (v32 a32 ... a63)
;;                ...
;;                (v32 a960 ... a991)
;;                ???)))
;;         (v32 a992 ??? ...)))
;; [a0 ... a1022] ; n = 1023, q = 31, r = 31, qq = 0, qr = 31, d = 2
;;   = (vector-internal n1023
;;       (vnode
;;         (vnode
;;           ???
;;           (v32 (v32 a0 ... a31)
;;                (v32 a32 ... a63)
;;                ...
;;                (v32 a960 ... a991)
;;                ???)))
;;         (v32 a992 ... a1022 ???)))
;; [a0 ... a1023] ; n = 1024, q = 32, r = 0, qq = 1, qr = 0, d = 2
;;   = (vector-internal n1024
;;       (vnode
;;         (vnode
;;           (v32 (v32 a0 ... a31)
;;                (v32 a32 ... a63)
;;                ...
;;                (v32 a992 ... a1023)))
;;           (v32 ??? ...))
;;         (v32 ??? ...)))
;; [a0 ... a1024] ; n = 1025, q = 32, r = 1, qq = 1, qr = 0, d = 2
;;   = (vector-internal n1025
;;       (vnode
;;         (vnode
;;           (v32 (v32 a0 ... a31)
;;                (v32 a32 ... a63)
;;                ...
;;                (v32 a992 ... a1023)))
;;           (v32 ??? ...))
;;         (v32 a1024 ??? ...)))
;; [a0 ... a1055] ; n = 1056, q = 33, r = 0, qq = 1, qr = 1, d = 2
;;   = (vector-internal n1056
;;       (vnode
;;         (vnode
;;           (v32 (v32 a0 ... a31)
;;                (v32 a32 ... a63)
;;                ...
;;                (v32 a992 ... a1023)))
;;           (v32 (v32 a1024 ... n1055) ??? ...))
;;         (v32 ??? ...)))
;; [a0 ... a1056] ; n = 1057, q = 33, r = 1, qq = 1, qr = 1, d = 2
;;   = (vector-internal n1057
;;       (vnode
;;         (vnode
;;           (v32 (v32 a0 ... a31)
;;                (v32 a32 ... a63)
;;                ...
;;                (v32 a992 ... a1023)))
;;           (v32 (v32 a1024 ... n1055) ??? ...))
;;         (v32 n1056 ??? ...)))

(define-adt Vector
  (vector-internal length tree))

(define-adt VNode
  (vnode tree tail))

;; vector-length : (Vectorof A) -> Natural
(define vector-length
  (λ (v)
    (match-adt Vector v
      [(vector-internal length tree)
       length])))

(define empty-vector
  (vector-internal n0 ???))

;; vector-nth : (Vectorof A) Natural -> A
(define vector-nth
  (λ (v i)
    (match-adt Vector v
      [(vector-internal length node)
       (vnode-nth length node i)])))

;; vector-conj : (Vectorof A) A -> (Vectorof A)
(define vector-conj
  (λ (v a)
    (match-adt Vector v
      [(vector-internal length node)
       (vector-internal (add1 length) (vnode-conj length node a))])))

;; TODO
;; make-vector : Natural A -> (Vectorof A)
(define make-vector
  (λ (n v)
    (build-vector n (λ (i) v))))

;; build-vector : Natural [Natural -> A] -> (Vectorof A)
(define build-vector
  (λ (n f)
    (vector-internal n
      (build-vnode/acc n f n0 ???))))

;; vector-append : (Vectorof A) (Vectorof A) -> (Vectorof A)
(define vector-append
  (λ (a b)
    (match-adt Vector a
      [(vector-internal a-len a-tree)
       (match-adt Vector b
         [(vector-internal b-len b-tree)
          (vector-internal (+ a-len b-len)
            (vnode-append/acc a-len a-tree n0 b-len b-tree))])])))

;; vector=? : [A A -> Boolean] -> [(Vectorof A) (Vector A) -> Boolean]
(define vector=?
  (λ (elem=?)
    (λ (a b)
      (match-adt Vector a
        [(vector-internal n a)
         (match-adt Vector b
           [(vector-internal b-len b)
            (match-adt Boolean (natural=? n b-len)
              [(true)
               (vnode-elems=? elem=? a b n n0)]
              [(false)
               (false)])])]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; vlength->depth : Natural -> Natural
;; (vlength->depth 0) = 0
;; (vlength->depth 1) = 0
;; (vlength->depth 2) = 1
;; (vlength->depth 63) = 1
;; (vlength->depth 64) = 2
;; (vlength->depth 2047) = 2
;; (vlength->depth 2048) = 3
(define vlength->depth
  (λ (n)
    (match-adt Natural n
      [(zero) n0]
      [(succ n-1)
       (match-adt Natural n-1
         [(zero) n0]
         [(succ n-2)
          (add1 (vlength->depth (quotient n BRANCHING-FACTOR)))])])))

;; vnode-conj : Natural (VNodeof A old-d) A -> (VNodeof new-d)
(define vnode-conj
  (λ (length node a)
    (match-adt Natural length
      [(zero)
       a]
      [(succ length-1)
       (match-adt Natural length-1
         [(zero)
          (vnode ???
                 (make-v32/2 node a ???))]
         [(succ length-2)
          (match-adt VNode node
            [(vnode tree tail)
             (match-adt Natural (remainder (add1 length) BRANCHING-FACTOR)
               [(zero)
                (vnode (vnode-conj (quotient length BRANCHING-FACTOR)
                                   tree
                                   (v32-conj BRANCHING-FACTOR-1 tail a))
                       (make-v32 ???))]
               [(succ r)
                (vnode tree (v32-conj r tail a))])])])])))

;; vnode-nth : Natural (VNodeof A d) Natural -> A
(define vnode-nth
  (λ (length tree i)
    (match-adt Natural length
      [(zero) ???]
      [(succ length-1)
       (match-adt Natural length-1
         [(zero) tree]
         [(succ length-2)
          (match-adt VNode tree
            [(vnode sub tail)
             (let ([lq (quotient length BRANCHING-FACTOR)]
                   [iq (quotient i BRANCHING-FACTOR)]
                   [ir (remainder i BRANCHING-FACTOR)])
               (match-adt Boolean (natural=? iq lq)
                 [(true)
                  (v32-nth tail ir)]
                 [(false)
                  (v32-nth (vnode-nth lq sub iq) ir)]))])])])))

;; build-vnode/acc : Natural [Natural -> A] Natural (VNodeof A depth)
;; i must be less than or equal to n
(define build-vnode/acc
  (λ (n f i tree)
    (match-adt Boolean (natural=? n i)
      [(true) tree]
      [(false)
       (build-vnode/acc n f
         (add1 i)
         (vnode-conj i tree (f i)))])))

;; vnode-append/acc : Natural (VNodeof A a-depth) Natural Natural (VNodeof A b-depth) -> (VNodeof A ab-depth)
(define vnode-append/acc
  (λ (a-len a-tree i b-len b-tree)
    (match-adt Boolean (natural=? i b-len)
      [(true) a-tree]
      [(false)
       (vnode-append/acc
        (add1 a-len)
        (vnode-conj a-len a-tree (vnode-nth b-len b-tree i))
        (add1 i)
        b-len
        b-tree)])))

;; vnode-elems=? : [A A -> Boolean] (VNodeof A d) (VNodeof A d) Natural Natural -> Boolean
;; a and b must both be vnodes for vectors of length n
;; i must be less than or equal to n
(define vnode-elems=?
  (λ (elem=? a b n i)
    (match-adt Boolean (natural=? n i)
      [(true) (true)]
      [(false)
       (match-adt Boolean (elem=? (vnode-nth n a i) (vnode-nth n b i))
         [(true) (vnode-elems=? elem=? a b n (add1 i))]
         [(false) (false)])])))

