#lang racket/base

(provide byte-string-literal/id
         byte-string-append*
         )

(require "../../byte-string.rkt"
         (for-syntax racket/base
                     syntax/parse
                     "../../byte-string.rkt"
                     "conversions.rkt"
                     ))

(define-syntax byte-string-append*
  (lambda (stx)
    (syntax-parse stx
      [(bytes-string-append* a:expr) #'a]
      [(bytes-string-append* a:expr b:expr ...+)
       #'(byte-string-append a (byte-string-append* b ...))])))

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

