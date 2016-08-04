#lang gnal λ/adt

;; these byte strings should be thought of as opaque
(provide empty-byte-string byte-string1 byte-string-append
         byte-string-length byte-string-ref
         byte-string=?
         ;; private
         byte-string->rkt)

(require "boolean.rkt"
         "byte.rkt"
         "natural.rkt"
         "maybe.rkt"
         "vector.rkt"
         )

;; A Byte-String is a (Vectorof Byte)

;; empty-byte-string : Byte-String
(define empty-byte-string empty-vector)

;; byte-string1 : Byte -> Byte-String
(define byte-string1
  (λ (b)
    (make-vector n1 b)))

;; byte-string-append : Byte-String Byte-String -> Byte-String
(define byte-string-append vector-append)

;; byte-string-length : Byte-String -> Natural
(define byte-string-length vector-length)

;; byte-string-ref : Byte-String Natural -> (Maybe Byte)
(define byte-string-ref vector-ref)

;; byte-string=? : Byte-String Byte-String -> Boolean
(define byte-string=? (vector=? byte=?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; private

(define byte-string->rkt
  (λ (byte->rkt natural->rkt rkt->natural build-bytes index-error)
    (λ (bstr)
      (build-bytes
       (natural->rkt (byte-string-length bstr))
       (λ (i)
         (match-adt Maybe (byte-string-ref bstr (rkt->natural i))
           [(none) (index-error i)]
           [(some byte) (byte->rkt byte)]))))))

