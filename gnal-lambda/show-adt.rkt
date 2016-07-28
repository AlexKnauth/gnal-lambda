#lang racket/base

(provide show-adt)

(require "adt.rkt"
         "byte-string.rkt"
         (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     "byte-string.rkt"
                     "adt/private/conversions.rkt"
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

(define-syntax byte-string-append*
  (lambda (stx)
    (syntax-parse stx
      [(bytes-string-append* a:expr) #'a]
      [(bytes-string-append* a:expr b:expr ...+)
       #'(byte-string-append a (byte-string-append* b ...))])))

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

(define-syntax byte-string-literal/id
  (lambda (stx)
    (syntax-parse stx
      [(byte-string-literal/id id:id)
       (define bs (bytes->list (string->bytes/utf-8 (symbol->string (syntax-e #'id)))))
       (define lcbs (map rkt->byte bs))
       #`(byte-string-append*
          #,@(for/list ([lcbyte (in-list lcbs)])
               #`(byte-string1
                  (byte
                   #,@
                   (lcbyte (λ (b0 b1 b2 b3 b4 b5 b6 b7)
                             (list (b0 (λ () #'(0-bit)) (λ () #'(1-bit)))
                                   (b1 (λ () #'(0-bit)) (λ () #'(1-bit)))
                                   (b2 (λ () #'(0-bit)) (λ () #'(1-bit)))
                                   (b3 (λ () #'(0-bit)) (λ () #'(1-bit)))
                                   (b4 (λ () #'(0-bit)) (λ () #'(1-bit)))
                                   (b5 (λ () #'(0-bit)) (λ () #'(1-bit)))
                                   (b6 (λ () #'(0-bit)) (λ () #'(1-bit)))
                                   (b7 (λ () #'(0-bit)) (λ () #'(1-bit))))))))))])))

