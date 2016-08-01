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


;; An (IO A) is one of:
;;  - (io-pure A)
;;  - (with-byte-string-out Byte-String (IO A))
;; TODO: support user input
(define-adt IO
  (io-pure v)
  (with-byte-string-out bstr io-v))

(define io-bind
  (λ (io-v next)
    (match-adt IO io-v
      [(io-pure v) (next v)]
      [(with-byte-string-out bstr1 v1)
       (match-adt IO (io-bind v1 next)
         [(io-pure v2)
          (with-byte-string-out bstr1 (io-pure v2))]
         [(with-byte-string-out bstr2 v2)
          (with-byte-string-out (byte-string-append bstr1 bstr2) v2)])])))

(define display-byte-string
  (λ (bstr)
    (with-byte-string-out bstr (io-pure (void)))))

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
        [(io-pure v)
         (write-bytes (byte-string->rkt empty-byte-string))]
        [(with-byte-string-out bstr v)
         (write-bytes (byte-string->rkt bstr))]))))

