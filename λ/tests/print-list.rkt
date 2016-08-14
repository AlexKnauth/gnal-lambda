#lang gnal λ/io

(require "../../gnal-lambda/show-adt.rkt"
         "../../gnal-lambda/trait/trait.rkt"
         "../../gnal-lambda/adt/private/quote-srcloc.rkt"
         "../trait/show.rkt"
         "../trait/functor.rkt"
         "../byte.rkt"
         "../list.rkt"
         )

(define print
  (λ (τ v)
    (display-byte-string ((show τ) v))))

(print (Listof Bit-τ) (empty))
(print (Listof Bit-τ) (cons (0-bit) (empty)))
(print (Listof Bit-τ) (cons (1-bit) (empty)))
(print (Listof Bit-τ) (cons (0-bit) (cons (0-bit) (empty))))
(print (Listof Bit-τ) (cons (1-bit) (cons (0-bit) (empty))))
(print (Listof Bit-τ) (cons (0-bit) (cons (1-bit) (empty))))
(print (Listof Bit-τ) (cons (1-bit) (cons (1-bit) (empty))))

(define flip-bit
  (λ (x) (match-adt Bit x [(0-bit) (1-bit)] [(1-bit) (0-bit)])))

(print (Listof Bit-τ) ((map (Listof Bit-τ)) flip-bit (cons (0-bit) (cons (1-bit) (empty)))))

(define ha
  (byte-string-append
   (byte-string1 (byte (0-bit) (0-bit) (0-bit) (1-bit) (0-bit) (1-bit) (1-bit) (0-bit)))
   (byte-string1 (byte (1-bit) (0-bit) (0-bit) (0-bit) (0-bit) (1-bit) (1-bit) (0-bit)))))

(with-input-from-byte-string-line
 ha
 (io-bind
  (read-byte-string-line)
  (λ (bstr)
    (display-byte-string (byte-string-append bstr bstr)))))

(display-byte-string (quote-srcloc-source here))
