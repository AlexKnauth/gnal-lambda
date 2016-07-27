#lang gnal λ/adt

(provide Bit 0-bit 1-bit 0-bit? 1-bit? bit=?
         Byte byte byte=?
         ;; private
         bit->rkt byte->rkt)

(require "boolean.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bit

;; A Bit is one of:
;;  - (0-bit)
;;  - (1-bit)
;; 0-bit : -> Boolean
;; 1-bit : -> Boolean
(define-adt Bit
  (0-bit)
  (1-bit))

;; 0-bit? : Bit -> Boolean
(define 0-bit?
  (λ (b)
    (match-adt Bit b
      [(0-bit) (true)]
      [(1-bit) (false)])))

;; 1-bit? : Bit -> Boolean
(define 1-bit?
  (λ (b)
    (match-adt Bit b
      [(0-bit) (false)]
      [(1-bit) (true)])))

;; bit=? : Bit Bit -> Boolean
(define bit=?
  (λ (b1 b2)
    (match-adt Bit b1
      [(0-bit)
       (0-bit? b2)]
      [(1-bit)
       (1-bit? b2)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Byte

;; A Byte is: 
;;  - (byte Bit Bit Bit Bit Bit Bit Bit Bit)
;; byte : Bit Bit Bit Bit Bit Bit Bit Bit -> Boolean
(define-adt Byte
  (byte b0 b1 b2 b3 b4 b5 b6 b7))

;; byte=? : Byte Byte -> Boolean
(define byte=?
  (λ (a b)
    (match-adt Byte a
      [(byte a0 a1 a2 a3 a4 a5 a6 a7)
       (match-adt Byte b
         [(byte b0 b1 b2 b3 b4 b5 b6 b7)
          (and (bit=? a0 b0)
           (and (bit=? a1 b1)
            (and (bit=? a2 b2)
             (and (bit=? a3 b3)
              (and (bit=? a4 b4)
               (and (bit=? a5 b5)
                (and (bit=? a6 b6)
                     (bit=? a7 b7))))))))])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; private

(define bit->rkt
  (λ (zero one)
    (λ (b)
      (match-adt Bit b
        [(0-bit) zero]
        [(1-bit) one]))))

(define byte->rkt
  (λ (zero one two + *)
    (let ([bit->rkt (bit->rkt zero one)])
      (λ (b)
        (match-adt Byte b
          [(byte b0 b1 b2 b3 b4 b5 b6 b7)
           (+ (* (bit->rkt b0))
              (* (bit->rkt b1) two)
              (* (bit->rkt b2) two two)
              (* (bit->rkt b3) two two two)
              (* (bit->rkt b4) two two two two)
              (* (bit->rkt b5) two two two two two)
              (* (bit->rkt b6) two two two two two two)
              (* (bit->rkt b7) two two two two two two two))])))))

