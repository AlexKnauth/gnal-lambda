#lang gnal λ/adt

;; A lengthless internal representation of functional vectors.
;; These vectors have length, but the length is not stored as part of it,
;; so the programmer needs to supply the correct length to every operation.

(provide empty-unguarded-vector
         make-unguarded-vector
         build-unguarded-vector
         unguarded-vector=?
         unguarded-vector-conj
         unguarded-vector-nth
         unguarded-vector-ref
         unguarded-vector-set-nth
         unguarded-vector-append
         )

(require "boolean.rkt"
         "maybe.rkt"
         "byte.rkt"
         "base-2-natural.rkt"
         "pair.rkt"
         )

;; BRANCHING-FACTOR = 2^1 = 2
(define BRANCHING-FACTOR n2)
(define BRANCHING-FACTOR-1/digit (1-bit))

(define ??? (λ (f) (f)))
(define pair???
  (pair ??? ???))

;; An (Unguared-Vectorof A 0) is a
;;   (U ??? A)
;; An (Unguared-Vectorof A 1) is a
;;   (vnode (Unguared-Vectorof (Pairof A A) 0)
;;          (Pairof? A A))
;; An (Unguared-Vectorof A 2) is a
;;   (vnode (Unguared-Vectorof (Pairof A A) 1)
;;          (Pairof? A A))
;; An (Unguared-Vectorof A (add1 d)) is a
;;   (vnode (Unguared-Vectorof (Pairof A A) d)
;;          (Pairof? A A))
;; [] ; n = 0, q = 0, r = 0, d = 0
;;   = ???
;; [a0] ; n = 1, q = 0, r = 1, d = 0
;;   = a0
;; [a0 a1] ; n = 2, q = 1, r = 0, d = 1
;;   = (vnode
;;       (pair a0 a1)
;;       (pair ??? ???))
;; [a0 a1 a2] ; n = 3, q = 1, r = 1, d = 1
;;   = (vode
;;       (pair a0 a1)
;;       (pair a2 ???))
;; [a0 ... a3] ; n = 4, q = 2, r = 0, d = 2
;;   = (vnode
;;       (vnode
;;         (pair (pair a0 a1)
;;               (pair a2 a3))
;;         (pair ??? ???))
;;       (pair ??? ???))
;; [a0 ... a4] ; n = 5, q = 2, r = 1, d = 2
;;   = (vnode
;;       (vnode
;;         (pair (pair a0 a1)
;;               (pair a2 a3))
;;         (pair ??? ???))
;;       (pair a4 ???))
;; [a0 ... a5] ; n = 6, q = 3, r = 0, d = 2
;;   = (vnode
;;       (vnode
;;         (pair (pair a0 a1)
;;               (pair a2 a3))
;;         (pair (pair a4 a5)
;;               ???))
;;       (pair ??? ???))
;; [a0 ... a6] ; n = 7, q = 3, r = 1, d = 2
;;   = (vnode
;;       (vnode
;;         (pair (pair a0 a1)
;;               (pair a2 a3))
;;         (pair (pair a4 a5)
;;               ???))
;;       (pair a6 ???))
;; [a0 ... a7] ; n = 8, q = 4, r = 0, d = 2
;;   = (vnode
;;       (vnode
;;         (pair (pair a0 a1)
;;               (pair a2 a3))
;;         (pair (pair a4 a5)
;;               (pair a6 a7))
;;       (pair ??? ???))
;; [a0 ... a8] ; n = 9, q = 4, r = 1, d = 2
;;   = (vnode
;;       (vnode
;;         (pair (pair a0 a1)
;;               (pair a2 a3))
;;         (pair (pair a4 a5)
;;               (pair a6 a7))
;;       (pair a8 ???))
;; [a0 ... a9] ; n = 10, q = 5, r = 0, d = 3
;;   = (vnode
;;       (vnode
;;         (vnode
;;           (pair (pair (pair a0 a1)
;;                       (pair a2 a3))
;;                 (pair (pair a4 a5)
;;                       (pair a6 a7)))
;;           (pair ??? ???))
;;         (pair (pair a8 a9) ???))
;;       (pair ??? ???))

(define-adt VNode
  (vnode tree tail))

(define empty-unguarded-vector ???)

;; unguarded-vector-nth : Natural (Unguarded-Vectorof A d) Natural -> A
(define unguarded-vector-nth
  (λ (n tree i)
    (match-adt Natural n
      [(zero) ???]
      [(nat-cons n0 n-rest)
       (match-adt Boolean (and (1-bit? n0) (zero? n-rest))
         [(true) tree]
         [(false)
          (match-adt VNode tree
            [(vnode sub tail)
             (let ([lq (quotient2 n)]
                   [iq (quotient2 i)]
                   [ir (remainder2->bit i)])
               (match-adt Boolean (natural=? iq lq)
                 [(true)
                  (pair-ref/bit tail ir)]
                 [(false)
                  (pair-ref/bit (unguarded-vector-nth lq sub iq) ir)]))])])])))

;; unguarded-vector-set-nth : Natural (Unguarded-Vectorof A d) Natural A -> (Unguarded-Vectorof A d)
(define unguarded-vector-set-nth
  (λ (n tree i a)
    (match-adt Natural n
      [(zero) tree]
      [(nat-cons n0 n-rest)
       (match-adt Boolean (and (1-bit? n0) (zero? n-rest))
         [(true) (match-adt Boolean (zero? i)
                   [(true) a]
                   [(false) tree])]
         [(false)
          (match-adt VNode tree
            [(vnode sub tail)
             (let ([lq (quotient2 n)]
                   [iq (quotient2 i)]
                   [ir (remainder2->bit i)])
               (match-adt Boolean (natural=? iq lq)
                 [(true)
                  (vnode sub
                         (pair-set/bit tail ir))]
                 [(false)
                  (vnode (unguarded-vector-update-nth lq sub iq
                           (λ (p) (pair-set/bit p ir a)))
                         tail)]))])])])))

;; unguarded-vector-update-nth : Natural (Unguarded-Vectorof A d) Natural [A -> A] -> (Unguarded-Vectorof A d)
(define unguarded-vector-update-nth
  (λ (n tree i f)
    (unguarded-vector-set-nth n tree i (f (unguarded-vector-nth n tree i)))))

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
       (match-adt Boolean (and (1-bit? n0) (zero? n-rest))
         [(true)
          (vnode (pair node a)
                 pair???)]
         [(false)
          (match-adt VNode node
            [(vnode tree tail)
             (match-adt Bit (remainder2->bit (add1 n))
               [(0-bit)
                (vnode (unguarded-vector-conj
                         (quotient2 n)
                         tree
                         (pair (fst tail) a))
                       pair???)]
               [(1-bit)
                (vnode tree (pair a (snd tail)))])])])])))

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
;; (vlength->depth 3) = 1
;; (vlength->depth 4) = 2
;; (vlength->depth 9) = 2
;; (vlength->depth 10) = 3
(define vlength->depth
  (λ (a)
    (match-adt Natural a
      [(zero) n0]
      [(nat-cons a0 ar)
       (match-adt Boolean (and (1-bit? a0) (zero? ar))
         [(true) n0]
         [(false)
          (add1 (vlength->depth (quotient2 a)))])])))

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

