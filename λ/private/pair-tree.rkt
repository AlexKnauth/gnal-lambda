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
         vector-ref
         vector-set-nth
         vector-append
         )

(require "boolean.rkt"
         "maybe.rkt"
         "base-2-natural.rkt"
         "pair.rkt"
         "unguarded-pair-tree.rkt"
         )

;; A (Vectorof A) is a (vector-internal Natural (Unguarded-Vectorof A depth))

(define-adt Vector
  (vector-internal length tree))

;; vector-length : (Vectorof A) -> Natural
(define vector-length
  (λ (v)
    (match-adt Vector v
      [(vector-internal length tree)
       length])))

(define empty-vector
  (vector-internal n0 empty-unguarded-vector))

;; vector-nth : (Vectorof A) Natural -> A
(define vector-nth
  (λ (v i)
    (match-adt Vector v
      [(vector-internal length tree)
       (unguarded-vector-nth length tree i)])))

;; vector-set-nth : (Vectorof A) Natural A -> (Vectorof A)
(define vector-set-nth
  (λ (v i a)
    (match-adt Vector v
      [(vector-internal length tree)
       (vector-internal length (unguarded-vector-set-nth length tree i a))])))

;; vector-ref : (Vectorof A) Natural -> (Maybe Byte)
(define vector-ref
  (λ (v i)
    (match-adt Vector v
      [(vector-internal n tree)
       (unguarded-vector-ref n tree i)])))

;; vector-conj : (Vectorof A) A -> (Vectorof A)
(define vector-conj
  (λ (v a)
    (match-adt Vector v
      [(vector-internal length tree)
       (vector-internal (add1 length)
         (unguarded-vector-conj length tree a))])))

;; make-vector : Natural A -> (Vectorof A)
(define make-vector
  (λ (n v)
    (vector-internal n
      (make-unguarded-vector n v))))

;; build-vector : Natural [Natural -> A] -> (Vectorof A)
(define build-vector
  (λ (n f)
    (vector-internal n
      (build-unguarded-vector n f))))

;; vector-append : (Vectorof A) (Vectorof A) -> (Vectorof A)
(define vector-append
  (λ (a b)
    (match-adt Vector a
      [(vector-internal a-len a-tree)
       (match-adt Vector b
         [(vector-internal b-len b-tree)
          (vector-internal (+ a-len b-len)
            (unguarded-vector-append a-len a-tree b-len b-tree))])])))

;; vector=? : [A A -> Boolean] -> [(Vectorof A) (Vector A) -> Boolean]
(define vector=?
  (λ (elem=?)
    (let ([unguarded-vector=? (unguarded-vector=? elem=?)])
      (λ (a b)
        (match-adt Vector a
          [(vector-internal a-len a)
           (match-adt Vector b
             [(vector-internal b-len b)
              (unguarded-vector=? a-len a b-len b)])])))))

