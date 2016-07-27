#lang gnal λ/adt

(provide io-pure
         io-bind
         display-byte-string
         io-begin
         ;; private
         ;; run-io should only be used by the λ+ language private
         ;; implementation
         run-io)

(require "byte.rkt"
         "byte-string.rkt")

;; TODO:
;;   implement byte-strings in terms of persistent vectors
;;   This file needs empty-byte-string and byte-string-append

(define-adt Unit
  (void))

;; TODO: support user input
(define-adt IO
  (with-byte-string-out val bstr))

(define io-pure
  (λ (v)
    (with-byte-string-out v empty-byte-string)))

(define io-bind
  (λ (io-v next)
    (match-adt IO io-v
      [(with-byte-string-out v1 bstr1)
       (match-adt IO (next v1)
         [(with-byte-string-out v2 bstr2)
          (with-byte-string-out v2 (byte-string-append bstr1 bstr2))])])))

(define display-byte-string
  (λ (bstr)
    (with-byte-string-out (void) bstr)))

(define io-begin
  (λ (io1 io2)
    (io-bind
     io1
     (λ (ignored)
       io2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; private

(define run-io
  (λ (write-bytes byte-string->rkt)
    (λ (io-v)
      (match-adt IO io-v
        [(with-byte-string-out v bstr)
         (write-bytes (byte-string->rkt bstr))]))))

