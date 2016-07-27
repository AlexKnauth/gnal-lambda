#lang gnal λ/adt

;; these byte strings should be thought of as opaque
(provide empty-byte-string byte-string1 byte-string-append
         byte-string-length byte-string-ref
         ;; private
         byte-string->rkt)

(require "byte.rkt"
         "natural.rkt"
         "maybe.rkt"
         )

;; TODO:
;;   implement byte-strings in terms of persistent vectors

;; DON'T PROVIDE THIS `Byte-String` TYPE!
;; OR EVEN ANYTHING FROM IT!
(define-adt Byte-String
  (make-empty-byte-string)
  (cons-byte-string first rest))

;; empty-byte-string : Byte-String
(define empty-byte-string (make-empty-byte-string))

;; byte-string1 : Byte -> Byte-String
(define byte-string1
  (λ (b)
    (cons-byte-string b empty-byte-string)))

;; byte-string-append : Byte-String Byte-String -> Byte-String
(define byte-string-append
  (λ (bstr1 bstr2)
    (match-adt Byte-String bstr1
      [(make-empty-byte-string) bstr2]
      [(cons-byte-string first rest)
       (cons-byte-string first (byte-string-append rest bstr2))])))

;; byte-string-length : Byte-String -> Natural
(define byte-string-length
  (λ (bstr)
    (match-adt Byte-String bstr
      [(make-empty-byte-string) n0]
      [(cons-byte-string first rest)
       (add1 (byte-string-length rest))])))

;; byte-string-ref : Byte-String Natural -> (Maybe Byte)
(define byte-string-ref
  (λ (bstr i)
    (match-adt Byte-String bstr
      [(make-empty-byte-string) (none)]
      [(cons-byte-string first rest)
       (match-adt Natural i
         [(zero) (some first)]
         [(succ i-1)
          (byte-string-ref rest i-1)])])))

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

