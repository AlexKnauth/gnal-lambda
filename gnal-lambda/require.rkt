#lang racket/base

(provide require)

(require syntax/parse/define
         (only-in racket/base [require -require]))

(define-simple-macro (require s:str ...)
  (-require s ...))

