#lang gnal λ/adt

(provide Bit Byte show-Bit show-Byte 0-bit 1-bit byte)

(require "../gnal-lambda/show-adt.rkt"
         "private/byte.rkt")

;; show-Bit : Bit -> Byte-String
(define show-Bit
  (show-adt Bit (0-bit) (1-bit)))

;; show-Byte : Byte -> Byte-String
(define show-Byte
  (show-adt Byte
    (byte show-Bit show-Bit show-Bit show-Bit show-Bit show-Bit show-Bit show-Bit)))

