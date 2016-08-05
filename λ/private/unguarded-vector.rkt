#lang gnal λ/adt

;; The lengthless internal representation of functional vectors.
;; These vectors have length, but the length is not stored as part of it,
;; so the programmer needs to supply the correct length to every operation.

(provide empty-unguarded-vector
         make-unguarded-vector
         build-unguarded-vector
         unguarded-vector=?
         unguarded-vector-conj
         unguarded-vector-nth
         unguarded-vector-ref
         ;unguarded-vector-set-nth
         unguarded-vector-append
         )

(require "boolean.rkt"
         "maybe.rkt"
         "base-32-natural.rkt"
         "v32.rkt"
         
         )

;; BRANCHING-FACTOR = 2^5 = 32
(define BRANCHING-FACTOR n32)
(define BRANCHING-FACTOR-1/digit d31)

(define ??? (λ (f) (f)))
(define v32???
  (make-v32 ???))

;; An (Unguared-Vectorof A 0) is a
;;   (U ??? A)
;; An (Unguared-Vectorof A 1) is a
;;   (vnode (Unguared-Vectorof (V32of A) 0)
;;          (V32of? A))
;; An (Unguared-Vectorof A 2) is a
;;   (vnode (Unguared-Vectorof (V32of A) 1)
;;          (V32of? A))
;; An (Unguared-Vectorof A (add1 d)) is a
;;   (vnode (Unguared-Vectorof (V32of A) d)
;;          (V32of? A))
;; [] ; n = 0, q = 0, r = 0, d = 0
;;   = ???
;; [a0] ; n = 1, q = 0, r = 1, d = 0
;;   = a0
;; [a0 a1] ; n = 2, q = 0, r = 2, d = 1
;;   = (vnode
;;       ???
;;       (v32 a0 a1 ??? ...))
;; [a0 ... a30] ; n = 31, q = 0, r = 31, d = 1
;;   = (vode
;;       ???
;;       (v32 a0 ... a30 ???))
;; [a0 ... a31] ; n = 32, q = 1, r = 0, d = 1
;;   = (vnode
;;       (v32 a0 ... a31)
;;       (v32 ??? ...))
;; [a0 ... a32] ; n = 33, q = 1, r = 1, d = 1
;;   = (vnode
;;       (v32 a0 ... a31)
;;       (v32 a32 ??? ...))
;; [a0 ... a33] ; n = 34, q = 1, r = 2, d = 1
;;   = (vnode
;;       (v32 a0 ... a32)
;;       (v32 a32 a33 ??? ...))
;; [a0 ... a62] ; n = 63, q = 1, r = 31, d = 1
;;   = (vnode
;;       (v32 a0 ... a31)
;;       (v32 a32 ... a62 ???))
;; [a0 ... a63] ; n = 64, q = 2, r = 0, d = 2
;;   = (vnode
;;       (vnode
;;         ???
;;         (v32 (v32 a0 ... a31) (v32 a32 ... a63) ??? ...))
;;       (v32 ??? ...))
;; [a0 ... a64] ; n = 65, q = 2, r = 1, d = 2
;;   = (vnode
;;       (vnode
;;         ???
;;         (v32 (v32 a0 ... a31) (v32 a32 ... a63) ??? ...))
;;       (v32 a64 ??? ...))
;; [a0 ... a65] ; n = 66, q = 2, r = 2, d = 2
;;   = (vnode
;;       (vnode
;;         ???
;;         (v32 (v32 a0 ... a31) (v32 a32 ... a63) ??? ...))
;;       (v32 a64 a65 ??? ...))
;; [a0 ... a94] ; n = 95, q = 2, r = 31, d = 2
;;   = (vnode
;;       (vnode
;;         ???
;;         (v32 (v32 a0 ... a31) (v32 a32 ... a63) ??? ...))
;;       (v32 a64 ... a94 ???))
;; [a0 ... a95] ; n = 96, q = 3, r = 0, d = 2
;;   = (vnode
;;       (vnode
;;         ???
;;         (v32 (v32 a0 ... a31) (v32 a32 ... a63) (v32 a64 ... a95) ??? ...))
;;       (v32 ??? ...))
;; [a0 ... a96] ; n = 97, q = 3, r = 1, d = 2
;;   = (vnode
;;       (vnode
;;         ???
;;         (v32 (v32 a0 ... a31) (v32 a32 ... a63) (v32 a64 ... a95) ??? ...))
;;       (v32 a96 ??? ...))
;; [a0 ... a97] ; n = 98, q = 3, r = 2, d = 2
;;   = (vnode
;;       (vnode
;;         ???
;;         (v32 (v32 a0 ... a31) (v32 a32 ... a63) (v32 a64 ... a95) ??? ...))
;;       (v32 a96 a97 ??? ...))
;; [a0 ... a991] ; n = 992, q = 31, r = 0, d = 2
;;   = (vnode
;;       (vnode
;;         ???
;;         (v32 (v32 a0 ... a31)
;;              (v32 a32 ... a63)
;;              ...
;;              (v32 a960 ... a991)
;;              ???)))
;;       (v32 ??? ...))
;; [a0 ... a992] ; n = 993, q = 31, r = 1, d = 2
;;   = (vnode
;;       (vnode
;;         ???
;;         (v32 (v32 a0 ... a31)
;;              (v32 a32 ... a63)
;;              ...
;;              (v32 a960 ... a991)
;;              ???)))
;;       (v32 a992 ??? ...))
;; [a0 ... a1022] ; n = 1023, q = 31, r = 31, qq = 0, qr = 31, d = 2
;;   = (vnode
;;       (vnode
;;         ???
;;         (v32 (v32 a0 ... a31)
;;              (v32 a32 ... a63)
;;              ...
;;              (v32 a960 ... a991)
;;              ???)))
;;       (v32 a992 ... a1022 ???))
;; [a0 ... a1023] ; n = 1024, q = 32, r = 0, qq = 1, qr = 0, d = 2
;;   = (vnode
;;       (vnode
;;         (v32 (v32 a0 ... a31)
;;              (v32 a32 ... a63)
;;              ...
;;              (v32 a992 ... a1023)))
;;         (v32 ??? ...))
;;       (v32 ??? ...))
;; [a0 ... a1024] ; n = 1025, q = 32, r = 1, qq = 1, qr = 0, d = 2
;;   = (vnode
;;       (vnode
;;         (v32 (v32 a0 ... a31)
;;              (v32 a32 ... a63)
;;              ...
;;              (v32 a992 ... a1023)))
;;         (v32 ??? ...))
;;       (v32 a1024 ??? ...))
;; [a0 ... a1055] ; n = 1056, q = 33, r = 0, qq = 1, qr = 1, d = 2
;;   = (vnode
;;       (vnode
;;         (v32 (v32 a0 ... a31)
;;              (v32 a32 ... a63)
;;              ...
;;              (v32 a992 ... a1023)))
;;         (v32 (v32 a1024 ... n1055) ??? ...))
;;       (v32 ??? ...))
;; [a0 ... a1056] ; n = 1057, q = 33, r = 1, qq = 1, qr = 1, d = 2
;;   = (vnode
;;       (vnode
;;         (v32 (v32 a0 ... a31)
;;              (v32 a32 ... a63)
;;              ...
;;              (v32 a992 ... a1023)))
;;         (v32 (v32 a1024 ... n1055) ??? ...))
;;       (v32 n1056 ??? ...))

(define-adt VNode
  (vnode tree tail))

(define empty-unguarded-vector ???)

;; unguarded-vector-nth : Natural (Unguarded-Vectorof A d) Natural -> A
(define unguarded-vector-nth
  (λ (n tree i)
    (match-adt Natural n
      [(zero) ???]
      [(nat-cons n0 n-rest)
       (match-adt Boolean (and (d01? n0) (zero? n-rest))
         [(true) tree]
         [(false)
          (match-adt VNode tree
            [(vnode sub tail)
             (let ([lq (quotient32 n)]
                   [iq (quotient32 i)]
                   [ir (remainder32->digit i)])
               (match-adt Boolean (natural=? iq lq)
                 [(true)
                  (v32-nth/digit tail ir)]
                 [(false)
                  (v32-nth/digit (unguarded-vector-nth lq sub iq) ir)]))])])])))

;; unguarded-vector-ref : Natural (Unguarded-Vectorof A d) Natural -> (Maybe Byte)
(define unguarded-vector-ref
  (λ (n v i)
    (match-adt Maybe (?∆ i n)
      [(none) (none)]
      [(some i∆n)
       (match-adt Boolean (zero? i∆n)
         [(true) (none)]
         [(false) (some (unguarded-vector-nth n v i))])])))

;; unguarded-vector-conj : Natural (Unguarded-Vectorof A old-d) A -> (Unguarded-Vectorof new-d)
(define unguarded-vector-conj
  (λ (n node a)
    (match-adt Natural n
      [(zero)
       a]
      [(nat-cons n0 n-rest)
       (match-adt Boolean (and (d01? n0) (zero? n-rest))
         [(true)
          (vnode ???
                 (make-v32/2 node a ???))]
         [(false)
          (match-adt VNode node
            [(vnode tree tail)
             (match-adt Maybe (?digit-sub1 (remainder32->digit (add1 n)))
               [(none)
                (vnode (unguarded-vector-conj
                         (quotient32 n)
                         tree
                         (v32-conj/digit BRANCHING-FACTOR-1/digit tail a))
                       v32???)]
               [(some r)
                (vnode tree (v32-conj/digit r tail a))])])])])))

;; make-unguarded-vector : Natural A -> (Unguarded-Vectorof A d)
(define make-unguarded-vector
  (λ (n v)
    (build-unguarded-vector n (λ (i) v))))

;; build-unguarded-vector : Natural [Natural -> A] -> (Unguarded-Vectorof A d)
(define build-unguarded-vector
  (λ (n f)
    (build-unguarded-vector/acc n f n0 ???)))

;; unguarded-vector-append :
;;   Natural
;;   (Unguarded-Vectorof A a-depth)
;;   Natural
;;   (Unguarded-Vectorof A b-depth)
;;   -> (Unguarded-Vectorof A ab-depth)
(define unguarded-vector-append
  (λ (a-len a b-len b)
    (unguarded-vector-append/acc a-len a n0 b-len b)))

;; unguarded-vector=? :
;;   [A A -> Boolean]
;;   -> [Natural
;;       (Unguarded-Vectorof A a-depth)
;;       Natuarl
;;       (Unguarded-Vectorof A b-depth)
;;       -> Boolean]
(define unguarded-vector=?
  (λ (elem=?)
    (λ (n a b-len b)
      (match-adt Boolean (natural=? n b-len)
        [(true)
         (unguarded-vector-elems=? elem=? a b n n0)]
        [(false)
         (false)]))))

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
  (λ (a)
    (match-adt Natural a
      [(zero) n0]
      [(nat-cons a0 ar)
       (match-adt Boolean (and (d01? a0) (zero? ar))
         [(true) n0]
         [(false)
          (add1 (vlength->depth (quotient32 a)))])])))

;; build-unguarded-vector/acc : Natural [Natural -> A] Natural (Unguarded-Vectorof A depth)
;; i must be less than or equal to n
(define build-unguarded-vector/acc
  (λ (n f i tree)
    (match-adt Boolean (natural=? n i)
      [(true) tree]
      [(false)
       (build-unguarded-vector/acc n f
         (add1 i)
         (unguarded-vector-conj i tree (f i)))])))

;; unguarded-vector-append/acc :
;;   Natural
;;   (Unguarded-Vectorof A a-depth)
;;   Natural
;;   Natural
;;   (Unguarded-Vectorof A b-depth)
;;   -> (Unguarded-Vectorof A ab-depth)
(define unguarded-vector-append/acc
  (λ (a-len a-tree i b-len b-tree)
    (match-adt Boolean (natural=? i b-len)
      [(true) a-tree]
      [(false)
       (unguarded-vector-append/acc
        (add1 a-len)
        (unguarded-vector-conj a-len a-tree (unguarded-vector-nth b-len b-tree i))
        (add1 i)
        b-len
        b-tree)])))

;; unguarded-vector-elems=? :
;;   [A A -> Boolean]
;;   (Unguarded-Vectorof A d)
;;   (Unguarded-Vectorof A d)
;;   Natural
;;   Natural
;;   -> Boolean
;; a and b must both be unguarded-vectors of length n
;; i must be less than or equal to n
(define unguarded-vector-elems=?
  (λ (elem=? a b n i)
    (match-adt Boolean (natural=? n i)
      [(true) (true)]
      [(false)
       (match-adt Boolean (elem=? (unguarded-vector-nth n a i) (unguarded-vector-nth n b i))
         [(true) (unguarded-vector-elems=? elem=? a b n (add1 i))]
         [(false) (false)])])))

