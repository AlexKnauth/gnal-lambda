#lang gnal λ/adt

(provide Bit Byte Bit-τ Byte-τ show-Bit show-Byte 0-bit 1-bit byte)

(require "../gnal-lambda/show-adt.rkt"
         "../gnal-lambda/trait/trait.rkt"
         "trait/show.rkt"
         "trait/equal.rkt"
         "private/byte.rkt")

;; show-Bit : Bit -> Byte-String
(define show-Bit
  (show-adt Bit (0-bit) (1-bit)))

;; show-Byte : Byte -> Byte-String
(define show-Byte
  (show-adt Byte
    (byte show-Bit show-Bit show-Bit show-Bit show-Bit show-Bit show-Bit show-Bit)))

(define Bit-τ
  (interp
    (trait-impl Show
      [show show-Bit])
    (trait-impl Equal
      [equal? bit=?])))

(define Byte-τ
  (interp
    (trait-impl Show
      [show show-Byte])
    (trait-impl Equal
      [equal? byte=?])))

