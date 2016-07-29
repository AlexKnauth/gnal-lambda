#lang racket/base

(provide show-adt)

(require "adt.rkt"
         "byte-string.rkt"
         "adt/private/byte-string-literal.rkt"
         (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     ))

(define space-byte
  (byte (0-bit) (0-bit) (0-bit) (0-bit) (0-bit) (1-bit) (0-bit) (0-bit)))

(define open-paren-byte
  (byte (0-bit) (0-bit) (0-bit) (1-bit) (0-bit) (1-bit) (0-bit) (0-bit)))
(define close-paren-byte
  (byte (1-bit) (0-bit) (0-bit) (1-bit) (0-bit) (1-bit) (0-bit) (0-bit)))

(define space (byte-string1 space-byte))
(define open-paren (byte-string1 open-paren-byte))
(define close-paren (byte-string1 close-paren-byte))

(define add-space
  (λ (bstr)
    (byte-string-append space bstr)))

(define-syntax show-adt
  (lambda (stx)
    (syntax-parse stx
      [(show-adt adt-type:id
         (variant:id show-element:expr ...)
         ...)
       #:with [variant-bstr:id ...] (generate-temporaries #'[variant ...])
       #:with [[field:id ...] ...] (stx-map generate-temporaries #'[[show-element ...] ...])
       #'(let ([variant-bstr (byte-string-literal/id variant)] ...)
           (λ (v)
             (match-adt adt-type v
               [(variant field ...)
                (byte-string-append*
                 open-paren
                 variant-bstr
                 (add-space (show-element field))
                 ...
                 close-paren)]
               ...)))])))

