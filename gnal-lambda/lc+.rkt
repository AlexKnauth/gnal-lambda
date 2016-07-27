#lang racket/base

(provide #%module-begin (all-from-out "lc/adt.rkt"))

(require (except-in "lc/adt.rkt" #%module-begin)
         syntax/wrap-modbeg
         (for-syntax racket/base))

(define old-printer (current-print))

(define printer
  (λ (v)
    (old-printer v)))

(define-syntax #%module-begin
  (make-wrapping-module-begin #'printer))

