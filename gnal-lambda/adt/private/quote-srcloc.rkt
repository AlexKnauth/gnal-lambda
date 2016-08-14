#lang racket/base

(provide quote-srcloc-source)

(require "byte-string-literal.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))

(begin-for-syntax
  (define (source->symbol src)
    (cond [(symbol? src) src]
          [(string? src) (string->symbol src)]
          [(path? src) (string->symbol (path->string src))]
          [else (error 'quote-srcloc-source "bad syntax-source: ~v" src)])))

(define-syntax quote-srcloc-source
  (lambda (stx)
    (syntax-parse stx
      [(quote-srcloc-source stx)
       #:with src-sym (source->symbol (syntax-source #'stx))
       #'(byte-string-literal/id src-sym)])))

