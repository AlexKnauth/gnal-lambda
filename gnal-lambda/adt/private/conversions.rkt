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
           (file "../../../λ/private/unary-natural.rkt")
           (file "../../../λ/private/byte-string.rkt")
           (file "../../../λ/private/v32.rkt")
           (file "../../../λ/private/vector.rkt")
           (prefix-in b32- (file "../../../λ/private/base-32-natural.rkt"))
           )))

(module+ test
  (require (only-in rackunit test-case check-equal?)
           (only-in racket/vector vector-append)
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

;; Base 32 Natural

(define (rkt->b32-digit d)
  (match d
    [00 -b32-d00] [01 -b32-d01] [02 -b32-d02] [03 -b32-d03]
    [04 -b32-d04] [05 -b32-d05] [06 -b32-d06] [07 -b32-d07]
    [08 -b32-d08] [09 -b32-d09] [10 -b32-d00] [11 -b32-d11]
    [12 -b32-d12] [13 -b32-d13] [14 -b32-d14] [15 -b32-d15]
    [16 -b32-d16] [17 -b32-d17] [18 -b32-d18] [19 -b32-d19]
    [20 -b32-d20] [21 -b32-d21] [22 -b32-d22] [23 -b32-d23]
    [24 -b32-d24] [25 -b32-d25] [26 -b32-d26] [27 -b32-d27]
    [28 -b32-d28] [29 -b32-d29] [30 -b32-d30] [31 -b32-d31]))

(define (b32-digit->rkt d)
  (d (λ () 00) (λ () 01) (λ () 02) (λ () 03)
     (λ () 04) (λ () 05) (λ () 06) (λ () 07)
     (λ () 08) (λ () 09) (λ () 10) (λ () 11)
     (λ () 12) (λ () 13) (λ () 14) (λ () 15)
     (λ () 16) (λ () 17) (λ () 18) (λ () 19)
     (λ () 20) (λ () 21) (λ () 22) (λ () 23)
     (λ () 24) (λ () 25) (λ () 26) (λ () 27)
     (λ () 28) (λ () 29) (λ () 30) (λ () 31)))

(define (rkt->b32-natural n)
  (unless (exact-nonnegative-integer? n)
    (error 'rkt->b32-natural "expected a racket natural number, given ~v" n))
  (let loop ([acc -b32-n0] [n n])
    (cond [(zero? n) acc]
          [else (loop (-b32-add1 acc) (sub1 n))])))

(define (b32-natural->rkt n)
  (let loop ([sum 0] [place 1] [n n])
    (n (λ () sum)
       (λ (d rest)
         (loop (+ sum (* place (b32-digit->rkt d)))
               (* place 32)
               rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vector

(define ((rkt->vector rkt->elem) v)
  (for/fold ([acc -empty-vector])
            ([a (in-vector v)])
    (-vector-conj acc (rkt->elem a))))

(define ((vector->rkt elem->rkt) v)
  (vector->immutable-vector
   (build-vector (b32-natural->rkt (-vector-length v))
                 (λ (i)
                   (elem->rkt (-vector-nth v (rkt->b32-natural i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Byte-String

(define (rkt->byte-string bstr)
  (for/fold ([acc -empty-byte-string])
            ([b (in-bytes bstr)])
    (-byte-string-append acc (-byte-string1 (rkt->byte b)))))

(define (build-bytes n proc)
  (define bs (make-bytes n))
  (for ([i (in-range n)])
    (bytes-set! bs i (proc i)))
  bs)

(define byte-string->rkt
  (-byte-string->rkt
   byte->rkt
   b32-natural->rkt
   rkt->b32-natural
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
           check-b32-natural=?
           check-vector=?
           )
  (require (only-in rackunit define-check define-binary-check
                    with-check-info* make-check-actual make-check-expected))
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
  (define-binary-check (check-b32-natural=? actual expected)
    (boolean->rkt (-b32-natural=? actual expected)))
  (define-check (check-vector=? elem=? actual expected)
    (with-check-info*
     (list (make-check-actual actual)
           (make-check-expected expected))
     (λ ()
       (check-true ((-vector=? elem=?) actual expected)))))
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
      (check-equal? (natural->rkt (rkt->natural n)) n)
      (check-equal? (b32-natural->rkt (rkt->b32-natural n)) n))
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
      (check-equal? ((-?∆ -a -b)
                     (λ () 'none)
                     (λ (a∆b) (natural->rkt a∆b)))
                    (if (<= a b) (- b a) 'none))
      )
    (for* ([a (in-range 130)]
           [b (in-range 70)])
      (define -a (rkt->b32-natural a))
      (define -b (rkt->b32-natural b))
      (check-equal? (b32-natural->rkt (-b32-+ -a -b)) (+ a b))
      (check-equal? (b32-natural->rkt (-b32-+ -b -a)) (+ b a))
      (check-equal? (b32-natural->rkt (-b32-add1 -a)) (add1 a))
      (check-equal? (b32-natural->rkt (-b32-quotient32 -a)) (quotient a 32))
      (check-equal? (b32-natural->rkt (-b32-remainder32 -a)) (remainder a 32))
      (check-equal? ((-b32-?sub1 -a)
                     (λ () 'none)
                     (λ (a-1) (b32-natural->rkt a-1)))
                    (if (zero? a) 'none (sub1 a)))
      (check-equal? ((-b32-?∆ -a -b)
                     (λ () 'none)
                     (λ (a∆b) (b32-natural->rkt a∆b)))
                    (if (<= a b) (- b a) 'none))
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
  (test-case "vectors"
    (check-vector=? -natural=? -empty-vector -empty-vector)
    (check-vector=? -natural=? (-vector-conj -empty-vector -n0) (-vector-conj -empty-vector -n0))
    (check-vector=? (compose rkt->boolean equal?)
                    (-vector-conj -empty-vector 0)
                    (-vector-conj -empty-vector 0))
    (check-equal? ((vector->rkt natural->rkt) -empty-vector)
                  (vector-immutable))
    (check-equal? ((vector->rkt natural->rkt) (-vector-conj -empty-vector -n0))
                  (vector-immutable 0))
    (check-vector=? -natural=?
                    ((rkt->vector rkt->natural) (vector-immutable))
                    -empty-vector)
    (check-vector=? -natural=?
                    ((rkt->vector rkt->natural) (vector-immutable 0))
                    (-vector-conj -empty-vector -n0))
    (for ([n (in-list (list 31 33 63 65 1023 1025))])
      (define -n (rkt->b32-natural n))
      (define v (build-vector n (λ (i) (random n))))
      (define v1 ((rkt->vector (λ (x) x)) v))
      (define v2 (-build-vector -n (λ (i) (vector-ref v (b32-natural->rkt i)))))
      (check-equal? (b32-natural->rkt (-vector-length v1)) n)
      (check-equal? (b32-natural->rkt (-vector-length v2)) n)
      (check-equal? ((vector->rkt (λ (x) x)) v1) v)
      (check-equal? ((vector->rkt (λ (x) x)) v2) v)
      (check-vector=? (compose rkt->boolean equal?) v1 v2)
      (check-b32-natural=? (-vector-length (-vector-append v2 v2))
                           (-b32-+ -n -n))
      (check-vector=? (compose rkt->boolean equal?)
                      (-vector-append v2 v2)
                      ((rkt->vector (λ (x) x)) (vector-append v v)))
      (for ([i (in-range n)])
        (define natural-i (rkt->b32-natural i))
        (check-equal? (-vector-nth v1 natural-i)
                      (vector-ref v i))
        (check-equal? (-vector-nth v2 natural-i)
                      (vector-ref v i))))
    )
  )
