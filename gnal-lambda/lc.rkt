#lang racket/base

(provide λ #%app #%module-begin #%top-interaction #%top #%datum)

(require syntax/parse/define
         (only-in racket/base [#%plain-lambda -λ] [#%plain-app -app])
         (for-syntax racket/base syntax/parse))

(define-simple-macro (λ (x:id ...) body:expr)
  (-λ (x ...) body))

(define-simple-macro (#%app f:expr arg:expr ...)
  (-app f arg ...))

(define-syntax #%datum
  (lambda (stx)
    (syntax-parse stx
      [(#%datum . b:boolean)
       (raise-syntax-error '#f '"literal booleans are not supported" stx #'b)]
      [(#%datum . n:number)
       (raise-syntax-error '#f '"literal numbers are not supported" stx #'n)]
      [(#%datum . c:char)
       (raise-syntax-error '#f '"literal characters are not supported" stx #'c)]
      [(#%datum . str:str)
       (raise-syntax-error '#f '"literal strings are not supported" stx #'str)]
      [(#%datum . bstr)
       #:when (bytes? (syntax-e #'bstr))
       (raise-syntax-error '#f '"literal byte strings are not supported" stx #'bstr)]
      [(#%datum . v)
       (raise-syntax-error '#f '"literals are not supported" stx #'v)])))

