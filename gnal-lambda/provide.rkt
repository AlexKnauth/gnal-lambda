#lang racket/base

(provide provide)

(require syntax/parse/define
         (only-in racket/base [provide -provide]))

(define-simple-macro (provide x:id ...)
  (-provide x ...))

