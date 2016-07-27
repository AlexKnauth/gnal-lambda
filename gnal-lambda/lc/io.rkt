#lang racket/base

(provide #%module-begin (all-from-out "adt.rkt" "../io.rkt"))

(require (except-in "adt.rkt" #%module-begin)
         "../io.rkt"
         "../adt/private/run-io.rkt"
         syntax/wrap-modbeg
         (for-syntax racket/base))

(define run-io (run-io/current-print (current-print)))

(define-syntax #%module-begin
  (make-wrapping-module-begin #'run-io))

