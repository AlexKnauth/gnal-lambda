#lang racket/base

(provide let let*)

(require "lc.rkt"
         syntax/parse/define
         (for-syntax racket/base))

(define-simple-macro (let ([x:id v:expr] ...) body:expr)
  ((λ (x ...) body) v ...))

(define-syntax let*
  (syntax-parser
    [(let* () body:expr) #'body]
    [(let* ([x:id v:expr] rst ...) body:expr)
     #'(let ([x v]) (let* (rst ...) body))]))

