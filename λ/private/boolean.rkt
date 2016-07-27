#lang gnal λ/adt

(provide Boolean true false and or not boolean=?
         ;; private
         boolean->rkt)

;; A Boolean is one of:
;;  - (true)
;;  - (false)
;; true : -> Boolean
;; false : -> Boolean
(define-adt Boolean
  (true)
  (false))

;; and : Boolean Boolean -> Boolean
(define and
  (λ (b1 b2)
    (match-adt Boolean b1
      [(true) b2]
      [(false) (false)])))

;; or : Boolean Boolean -> Boolean
(define or
  (λ (b1 b2)
    (match-adt Boolean b1
      [(true) b1]
      [(false) b2])))

;; not : Boolean -> Boolean
(define not
  (λ (b)
    (match-adt Boolean b
      [(true) (false)]
      [(false) (true)])))

;; boolean=? : Boolean Boolean -> Boolean
(define boolean=?
  (λ (b1 b2)
    (match-adt Boolean b1
      [(true) b2]
      [(false) (not b2)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; private

(define boolean->rkt
  (λ (t f)
    (λ (b)
      (match-adt Boolean b
        [(true) t]
        [(false) f]))))

