#lang racket/base

(provide define)

(require syntax/parse/define
         (only-in racket/base [define -define]))

(define-simple-macro (define x:id v:expr)
  (-define x v))

