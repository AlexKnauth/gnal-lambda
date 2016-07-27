#lang racket/base

(provide run-io/current-print)

(require "conversions.rkt"
         (file "../../../λ/private/io.rkt"))

(struct displayed-byte-string (bstr)
  #:property prop:custom-write
  (λ (this out mode)
    (write-bytes (displayed-byte-string-bstr this) out)))

(define (run-io/current-print old-printer)
  (define (write-bytes bstr)
    (old-printer (displayed-byte-string bstr)))
  (run-io write-bytes byte-string->rkt))

