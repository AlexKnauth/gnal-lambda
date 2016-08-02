#lang racket/base

(provide rkt->boolean boolean->rkt
         rkt->natural natural->rkt
         rkt->bit bit->rkt
         rkt->byte byte->rkt
         rkt->byte-string byte-string->rkt
         )

(require racket/match
         (prefix-in
          -
          (combine-in
           (file "../../../λ/private/boolean.rkt")
           (file "../../../λ/private/byte.rkt")
           (file "../../../λ/private/natural.rkt")
           (file "../../../λ/private/byte-string.rkt")
           (file "../../../λ/private/v32.rkt")
           )))

(module+ test
  (require (only-in rackunit test-case check-equal?)
           (submod ".." testing-forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Boolean

(define (rkt->boolean b)
  (if b (-true) (-false)))

(define boolean->rkt
  (-boolean->rkt #true #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bit and Byte

(define (rkt->bit b)
  (match b
    [0 (-0-bit)]
    [1 (-1-bit)]))

(define (rkt->byte n)
  (unless (byte? n)
    (error 'rkt->byte "expected a racket byte, given ~v" n))
  (define n/1 n)
  (define-values [n/2 b0] (quotient/remainder n/1 2))
  (define-values [n/4 b1] (quotient/remainder n/2 2))
  (define-values [n/8 b2] (quotient/remainder n/4 2))
  (define-values [n/16 b3] (quotient/remainder n/8 2))
  (define-values [n/32 b4] (quotient/remainder n/16 2))
  (define-values [n/64 b5] (quotient/remainder n/32 2))
  (define-values [n/128 b6] (quotient/remainder n/64 2))
  (define-values [n/256 b7] (quotient/remainder n/128 2))
  (unless (zero? n/256)
    (error 'rkt->byte "this should never happen, n = ~v, n/256 = ~v" n n/256))
  (-byte
   (rkt->bit b0)
   (rkt->bit b1)
   (rkt->bit b2)
   (rkt->bit b3)
   (rkt->bit b4)
   (rkt->bit b5)
   (rkt->bit b6)
   (rkt->bit b7)))

(define bit->rkt
  (-bit->rkt 0 1))

(define byte->rkt
  (-byte->rkt 0 1 2 + *))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Natural

(define (rkt->natural n)
  (unless (exact-nonnegative-integer? n)
    (error 'rkt->natural "expected a racket natural number, given ~v" n))
  (let loop ([acc -n0] [n n])
    (cond [(zero? n) acc]
          [else (loop (-add1 acc) (sub1 n))])))

(define natural->rkt
  (-natural->rkt 0 add1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Byte-String

(define (rkt->byte-string bstr)
  (define n (bytes-length bstr))
  (for/fold ([acc -empty-byte-string])
            ([i (in-range n)])
    (define b (bytes-ref bstr (- n i 1)))
    (-byte-string-append (-byte-string1 (rkt->byte b)) acc)))

(define (build-bytes n proc)
  (define bs (make-bytes n))
  (for ([i (in-range n)])
    (bytes-set! bs i (proc i)))
  bs)

(define byte-string->rkt
  (-byte-string->rkt
   byte->rkt
   natural->rkt
   rkt->natural
   build-bytes
   (λ (i) (error 'byte-string->rkt "bad index: ~v" i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tests

(module* testing-forms #f
  (provide check-boolean=?
           check-true
           check-false
           check-bit=?
           check-byte=?
           check-natural=?
           )
  (require (only-in rackunit define-check define-binary-check))
  (define-binary-check (check-boolean=? actual expected)
    (boolean->rkt (-boolean=? actual expected)))
  (define-check (check-true v)
    (check-boolean=? v (-true)))
  (define-check (check-false v)
    (check-boolean=? v (-false)))
  (define-binary-check (check-bit=? actual expected)
    (boolean->rkt (-bit=? actual expected)))
  (define-binary-check (check-byte=? actual expected)
    (boolean->rkt (-byte=? actual expected)))
  (define-binary-check (check-natural=? actual expected)
    (boolean->rkt (-natural=? actual expected)))
  )

(module+ test
  (test-case "booleans"
    (check-equal? (boolean->rkt (rkt->boolean #true)) #true)
    (check-equal? (boolean->rkt (rkt->boolean #false)) #false)
    (check-boolean=? (-true) (-true))
    (check-boolean=? (-false) (-false))
    (check-boolean=? (rkt->boolean #true) (-true))
    (check-boolean=? (rkt->boolean #false) (-false))
    (check-boolean=? (-not (-true)) (-false))
    (check-boolean=? (-not (-false)) (-true))
    (check-boolean=? (-and (-false) (-false)) (-false))
    (check-boolean=? (-and (-false) (-true)) (-false))
    (check-boolean=? (-and (-true) (-false)) (-false))
    (check-boolean=? (-and (-true) (-true)) (-true))
    (check-boolean=? (-or (-false) (-false)) (-false))
    (check-boolean=? (-or (-false) (-true)) (-true))
    (check-boolean=? (-or (-true) (-false)) (-true))
    (check-boolean=? (-or (-true) (-true)) (-true))
    (check-boolean=? (-boolean=? (-false) (-false)) (-true))
    (check-boolean=? (-boolean=? (-false) (-true)) (-false))
    (check-boolean=? (-boolean=? (-true) (-false)) (-false))
    (check-boolean=? (-boolean=? (-true) (-true)) (-true))
    )
  (test-case "bits"
    (check-equal? (bit->rkt (rkt->bit 0)) 0)
    (check-equal? (bit->rkt (rkt->bit 1)) 1)
    (check-bit=? (-0-bit) (-0-bit))
    (check-bit=? (-1-bit) (-1-bit))
    (check-bit=? (rkt->bit 0) (-0-bit))
    (check-bit=? (rkt->bit 1) (-1-bit))
    (check-boolean=? (-0-bit? (-0-bit)) (-true))
    (check-boolean=? (-0-bit? (-1-bit)) (-false))
    (check-boolean=? (-1-bit? (-0-bit)) (-false))
    (check-boolean=? (-1-bit? (-1-bit)) (-true))
    )
  (test-case "bytes"
    (for ([n (in-range 256)])
      (check-equal? (byte->rkt (rkt->byte n)) n))
    (check-byte=? (-byte (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit))
                  (-byte (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit)))
    (check-byte=? (rkt->byte 0)
                  (-byte (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit)))
    (check-true (-byte=?
                 (-byte (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit))
                 (-byte (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit))))
    (check-false (-byte=?
                  (-byte (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit))
                  (-byte (-0-bit) (-0-bit) (-0-bit) (-1-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit))))
    )
  (test-case "natural numbers"
    (for ([n (in-range 100)])
      (check-equal? (natural->rkt (rkt->natural n)) n))
    (for* ([a (in-range 10)]
           [b (in-range 6)])
      (define -a (rkt->natural a))
      (define -b (rkt->natural b))
      (check-equal? (natural->rkt (-+ -a -b)) (+ a b))
      (check-equal? (natural->rkt (-* -a -b)) (* a b))
      (check-equal? (natural->rkt (-^ -a -b)) (expt a b))
      (when (not (zero? b))
        (check-equal? (natural->rkt (-quotient -a -b)) (quotient a b))
        (check-equal? (natural->rkt (-remainder -a -b)) (remainder a b)))
      ))
  (test-case "byte-strings"
    (check-equal? (byte-string->rkt (rkt->byte-string #"")) #"")
    (check-equal? (byte-string->rkt (rkt->byte-string #"a")) #"a")
    (check-equal? (byte-string->rkt (rkt->byte-string #"b")) #"b")
    (check-equal? (byte-string->rkt (rkt->byte-string #"c")) #"c")
    (check-equal? (byte-string->rkt (rkt->byte-string #"abc")) #"abc")
    (check-equal? (byte-string->rkt (rkt->byte-string #"abcdefg")) #"abcdefg")
    (check-equal? (byte-string->rkt (rkt->byte-string #"FROGGY!")) #"FROGGY!")
    (for* ([a (in-list (list #"dog" #"cat" #"mouse" #"FROGGY!"))]
           [b (in-list (list #"dog" #"cat" #"mouse" #"FROGGY!"))])
      (define -a (rkt->byte-string a))
      (define -b (rkt->byte-string b))
      (check-equal? (byte-string->rkt (-byte-string-append -a -b))
                    (bytes-append a b)))
    )
  (test-case "v32"
    (check-boolean=? ((-v32=? -bit=?)
                      (-v32 (-0-bit) (-1-bit) (-0-bit) (-1-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit)
                            (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-1-bit) (-0-bit) (-1-bit)
                            (-1-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit)
                            (-0-bit) (-0-bit) (-1-bit) (-0-bit) (-0-bit) (-0-bit) (-1-bit) (-0-bit))
                      (-v32 (-0-bit) (-1-bit) (-0-bit) (-1-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit)
                            (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-1-bit) (-0-bit) (-1-bit)
                            (-1-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit)
                            (-0-bit) (-0-bit) (-1-bit) (-0-bit) (-0-bit) (-0-bit) (-1-bit) (-0-bit)))
                     (-true))
    (check-boolean=? ((-v32=? -bit=?)
                      (-v32 (-0-bit) (-1-bit) (-0-bit) (-1-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit)
                            (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-1-bit) (-0-bit) (-1-bit)
                            (-1-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit)
                            (-0-bit) (-0-bit) (-1-bit) (-0-bit) (-0-bit) (-0-bit) (-1-bit) (-0-bit))
                      (-v32 (-0-bit) (-1-bit) (-0-bit) (-1-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit)
                            (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-1-bit) (-0-bit) (-1-bit)
                            (-1-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit) (-0-bit)
                            (-0-bit) (-0-bit) (-1-bit) (-0-bit) (-0-bit) (-0-bit) (-1-bit) (-1-bit)))
                     (-false))
    )
  )
