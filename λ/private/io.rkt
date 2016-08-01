#lang gnal λ/adt

(provide io-pure
         io-bind
         display-byte-string
         read-byte-string-line
         io-begin
         with-input-from-byte-string-line
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
;;  - (wait-for-byte-string-in (-> Byte-String (IO A)))
(define-adt IO
  (io-pure v)
  (with-byte-string-out bstr io-v)
  (wait-for-byte-string-in f))

;; io-bind : (IO A) [A -> (IO B)] -> (IO B)
(define io-bind
  (λ (io-v next)
    (match-adt IO io-v
      [(io-pure v) (next v)]
      [(with-byte-string-out bstr1 v1)
       (match-adt IO (io-bind v1 next)
         [(io-pure v2)
          (with-byte-string-out bstr1 (io-pure v2))]
         [(with-byte-string-out bstr2 v2)
          (with-byte-string-out (byte-string-append bstr1 bstr2) v2)]
         [(wait-for-byte-string-in f)
          (with-byte-string-out bstr1 (wait-for-byte-string-in f))])]
      [(wait-for-byte-string-in f)
       (wait-for-byte-string-in
        (λ (bstr) (io-bind (f bstr) next)))])))

;; display-byte-string : Byte-String -> (IO Unit)
(define display-byte-string
  (λ (bstr)
    (with-byte-string-out bstr (io-pure (void)))))

;; read-byte-string-line : -> (IO Byte-String)
(define read-byte-string-line
  (λ ()
    (wait-for-byte-string-in (λ (bstr) (io-pure bstr)))))

;; with-input-from-byte-string-line : Byte-String (IO A) -> (IO A)
(define with-input-from-byte-string-line
  (λ (bstr1 v1)
    (match-adt IO v1
      [(io-pure v) v1]
      [(with-byte-string-out bstr2 v2)
       (with-byte-string-out bstr2 (with-input-from-byte-string-line bstr1 v2))]
      [(wait-for-byte-string-in f)
       (f bstr1)])))

;; io-begin : (IO A) (IO B) -> (IO B)
(define io-begin
  (λ (io1 io2)
    (io-bind
     io1
     (λ (ignored)
       io2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; private

(define run-io
  (λ (void begin write-bytes byte-string->rkt read-bytes-line rkt->byte-string)
    (λ (io-v)
      (match-adt IO io-v
        [(io-pure v)
         (void)]
        [(with-byte-string-out bstr next-io)
         (begin
           (write-bytes (byte-string->rkt bstr))
           ((run-io void begin write-bytes byte-string->rkt read-bytes-line rkt->byte-string)
            next-io))]
        [(wait-for-byte-string-in f)
         ((run-io void begin write-bytes byte-string->rkt read-bytes-line rkt->byte-string)
          (f (rkt->byte-string (read-bytes-line))))]))))

