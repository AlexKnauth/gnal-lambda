#lang gnal λ/io

(define space-byte
  (byte (0-bit) (0-bit) (0-bit) (0-bit) (0-bit) (1-bit) (0-bit) (0-bit)))

(define open-paren-byte
  (byte (0-bit) (0-bit) (0-bit) (1-bit) (0-bit) (1-bit) (0-bit) (0-bit)))
(define close-paren-byte
  (byte (1-bit) (0-bit) (0-bit) (1-bit) (0-bit) (1-bit) (0-bit) (0-bit)))

(define a-byte
  (byte (1-bit) (0-bit) (0-bit) (0-bit) (0-bit) (1-bit) (1-bit) (0-bit)))
(define b-byte
  (byte (0-bit) (1-bit) (0-bit) (0-bit) (0-bit) (1-bit) (1-bit) (0-bit)))
(define c-byte
  (byte (1-bit) (1-bit) (0-bit) (0-bit) (0-bit) (1-bit) (1-bit) (0-bit)))
;d
(define e-byte
  (byte (1-bit) (0-bit) (1-bit) (0-bit) (0-bit) (1-bit) (1-bit) (0-bit)))
;fghijkl
(define m-byte
  (byte (1-bit) (0-bit) (1-bit) (1-bit) (0-bit) (1-bit) (1-bit) (0-bit)))
(define n-byte
  (byte (0-bit) (1-bit) (1-bit) (1-bit) (0-bit) (1-bit) (1-bit) (0-bit)))
(define o-byte
  (byte (1-bit) (1-bit) (1-bit) (1-bit) (0-bit) (1-bit) (1-bit) (0-bit)))
(define p-byte
  (byte (0-bit) (0-bit) (0-bit) (0-bit) (1-bit) (1-bit) (1-bit) (0-bit)))
;qr
(define s-byte
  (byte (1-bit) (1-bit) (0-bit) (0-bit) (1-bit) (1-bit) (1-bit) (0-bit)))
(define t-byte
  (byte (0-bit) (0-bit) (1-bit) (0-bit) (1-bit) (1-bit) (1-bit) (0-bit)))
;uvwx
(define y-byte
  (byte (1-bit) (0-bit) (0-bit) (1-bit) (1-bit) (1-bit) (1-bit) (0-bit)))
;z

;; bstr:empty : Byte-String
(define bstr:empty
  (byte-string-append
   (byte-string1 e-byte)
   (byte-string-append
    (byte-string1 m-byte)
    (byte-string-append
     (byte-string1 p-byte)
     (byte-string-append
      (byte-string1 t-byte)
      (byte-string1 y-byte))))))

;; bstr:cons : Byte-String
(define bstr:cons
  (byte-string-append
   (byte-string1 c-byte)
   (byte-string-append
    (byte-string1 o-byte)
    (byte-string-append
     (byte-string1 n-byte)
     (byte-string1 s-byte)))))

;; make-empty-bstr : -> Byte-String
(define make-empty-bstr
  (λ ()
    (byte-string-append
     (byte-string1 open-paren-byte)
     (byte-string-append
      bstr:empty
      (byte-string1 close-paren-byte)))))

;; make-cons-bstr : Byte-String Byte-String -> Byte-String
(define make-cons-bstr
  (λ (a b)
    (byte-string-append
     (byte-string1 open-paren-byte)
     (byte-string-append
      bstr:cons
      (byte-string-append
       (byte-string1 space-byte)
       (byte-string-append
        a
        (byte-string-append
         (byte-string1 space-byte)
         (byte-string-append
          b
          (byte-string1 close-paren-byte)))))))))

;; A (Listof A) is one of:
;;  - (empty)
;;  - (cons A (Listof A))
(define-adt List
  (empty)
  (cons first rest))

(define show-list
  (λ (show-element)
    (λ (lst)
      (match-adt List lst
        [(empty) (make-empty-bstr)]
        [(cons first rest)
         (make-cons-bstr
          (show-element first)
          ((show-list show-element) rest))]))))

(define show-bit
  (λ (b)
    (b
     (λ () (byte-string1 a-byte))
     (λ () (byte-string1 b-byte)))))

(define print-list
  (λ (show-element)
    (λ (lst)
      (display-byte-string ((show-list show-element) lst)))))

(define print-listof-bit
  (print-list show-bit))

(print-listof-bit (empty))
(print-listof-bit (cons (0-bit) (empty)))
(print-listof-bit (cons (1-bit) (empty)))
(print-listof-bit (cons (0-bit) (cons (0-bit) (empty))))
(print-listof-bit (cons (1-bit) (cons (0-bit) (empty))))
(print-listof-bit (cons (0-bit) (cons (1-bit) (empty))))
(print-listof-bit (cons (1-bit) (cons (1-bit) (empty))))

