#lang gnal λ/adt

(provide Natural natural=?
         zero nat-cons zero?
         + * add1 *2
         ?∆ ?sub1
         quotient2
         remainder2
         remainder2->bit
         quotient32
         remainder32
         n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15
         n16 n17 n18 n19 n20 n21 n22 n23 n24 n25 n26 n27 n28 n29 n30 n31
         n32
         )

(require "boolean.rkt"
         "maybe.rkt"
         "byte.rkt")

;; A Digit is a Bit
(define d0 (0-bit))
(define d1 (1-bit))
(define d0? 0-bit?)
(define d1? 1-bit?)
(define digit=? bit=?)

;; A Natural is one of:
;;  - (zero)
;;  - (nat-cons Bit Natural)
(define-adt Natural
  (zero)
  (nat-cons 1s-digit rest))

(define zero?
  (λ (a)
    (match-adt Natural a
      [(zero) (true)]
      [(nat-cons a0 ar) (false)])))

(define zero?*
  (λ (a)
    (match-adt Natural a
      [(zero) (true)]
      [(nat-cons a0 ar)
       (match-adt Boolean (d0? a0)
         [(true) (zero?* ar)]
         [(false) (false)])])))

(define nat-cons*
  (λ (1s-digit rest)
    (match-adt Boolean (and (d0? 1s-digit) (zero?* rest))
      [(true) n0]
      [(false) (nat-cons 1s-digit rest)])))

(define natural=?
  (λ (a b)
    (match-adt Natural a
      [(zero) (zero? b)]
      [(nat-cons a0 a-rest)
       (match-adt Natural b
         [(zero) (false)]
         [(nat-cons b0 b-rest)
          (match-adt Boolean (digit=? a0 b0)
            [(true) (natural=? a-rest b-rest)]
            [(false) (false)])])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add1 : Natural -> Natural
(define add1
  (λ (a)
    (+/digit a d1)))

;; *2 : Natural -> Natural
(define *2
  (λ (a)
    (nat-cons* d0 a)))

;; *4 : Natural -> Natural
(define *4
  (λ (a)
    (*2 (*2 a))))

;; + : Natural Natural -> Natural
(define +
  (λ (a b)
    (+/carry a b d0)))

;; * : Natural Natural -> Natural
(define *
  (λ (a b)
    (match-adt Natural a
      [(zero) n0]
      [(nat-cons ar aq)
       (match-adt Natural b
         [(zero) n0]
         [(nat-cons br bq)
          (+
           (+
            (+
             (*/digits ar br)
             (*2 (*/digit bq ar)))
            (*2 (*/digit aq br)))
           (*4 (* aq bq)))])])))

;; quotient2 : Natural -> Natural
(define quotient2
  (λ (a)
    (match-adt Natural a
      [(zero) n0]
      [(nat-cons a0 a-rest) a-rest])))

;; remainder2 : Natural -> Natural
(define remainder2
  (λ (a)
    (match-adt Natural a
      [(zero) n0]
      [(nat-cons a0 a-rest)
       (nat-cons* a0 n0)])))

;; remainder2->bit : Natural -> Bit
(define remainder2->bit
  (λ (a)
    (match-adt Natural a
      [(zero) d0]
      [(nat-cons a0 ar) a0])))

;; quotient4 : Natural -> Natural
(define quotient4
  (λ (a) (natural-drop a n2)))

;; remainder4 : Natural -> Natural
(define remainder4
  (λ (a) (natural-take a n2)))

;; quotient8 : Natural -> Natural
(define quotient8
  (λ (a) (natural-drop a n3)))

;; remainder8 : Natural -> Natural
(define remainder8
  (λ (a) (natural-take a n3)))

;; quotient16 : Natural -> Natural
(define quotient16
  (λ (a) (natural-drop a n4)))

;; remainder16 : Natural -> Natural
(define remainder16
  (λ (a) (natural-take a n4)))

;; quotient32 : Natural -> Natural
(define quotient32
  (λ (a) (natural-drop a n5)))

;; remainder32 : Natural -> Natural
(define remainder32
  (λ (a) (natural-take a n5)))

;; +/carry : Natural Natural Digit -> Natural
(define +/carry
  (λ (a b carry)
    (match-adt Natural a
      [(zero) (+/digit b carry)]
      [(nat-cons a0 ar)
       (match-adt Natural b
         [(zero) (+/digit a carry)]
         [(nat-cons b0 br)
          ((full-adder a0 b0 carry)
           (λ (ab0 ab-carry)
             (nat-cons* ab0
               (+/carry ar br ab-carry))))])])))

;; +/digit : Natural Digit -> Natural
(define +/digit
  (λ (a d)
    (match-adt Natural a
      [(zero) (nat-cons* d n0)]
      [(nat-cons a0 ar)
       ((half-adder a0 d)
        (λ (ad0 ad-carry)
          (match-adt Bit ad-carry
            [(0-bit) (nat-cons* ad0 ar)]
            [(1-bit) (nat-cons ad0 (+/digit ar d1))])))])))

;; */digits : Digit Digit -> Natural
(define */digits
  (λ (a b)
    (match-adt Bit a
      [(0-bit) n0]
      [(1-bit)
       (match-adt Bit b
         [(0-bit) n0]
         [(1-bit) n1])])))

;; */digit : Natural Digit -> Natural
(define */digit
  (λ (a b)
    (match-adt Bit b
      [(0-bit) n0]
      [(1-bit) a])))

;; ?∆ : Natural Natural -> (Maybe Natural)
;; (?∆ a b) contains b - a when a < b, none otherwise
(define ?∆
  (λ (a b)
    (match-adt Maybe (?sub1 a)
      [(none)
       (some b)]
      [(some a-1)
       (opt-bind
        (?sub1 b)
        (λ (b-1)
          (?∆ a-1 b-1)))])))

;; ?sub1 : Natural -> (Maybe Natural)
(define ?sub1
  (λ (a)
    (match-adt Natural a
      [(zero) (none)]
      [(nat-cons a0 ar)
       (match-adt Maybe (?digit-sub1 a0)
         [(none)
          (match-adt Maybe (?sub1 ar)
            [(none) (none)]
            [(some ar-1) (some (nat-cons* d1 ar-1))])]
         [(some a0-1)
          (some (nat-cons* a0-1 ar))])])))

;; ?digit-sub1 : Digit -> (Maybe Digit)
(define ?digit-sub1
  (λ (d)
    (match-adt Bit d
      [(0-bit) (none)]
      [(1-bit) (some d0)])))

;; full-adder : Digit Digit Digit -> (Pair Digit Digit)
(define full-adder
  (λ (a b c)
    (λ (pair)
      ((half-adder a b)
       (λ (ab0 ab1)
         ((half-adder ab0 c)
          (λ (abc0 ab0c1)
            (pair abc0
              (match-adt Bit ab1
                [(0-bit) (match-adt Bit ab0c1
                           [(0-bit) d0]
                           [(1-bit) d1])]
                [(1-bit) (match-adt Bit ab0c1
                           [(0-bit) d1]
                           [(1-bit) #;TODO d1])])))))))))

;; half-adder : Digit Digit -> (Pair Digit Bit)
(define half-adder
  (λ (a b)
    (λ (pair)
      (match-adt Bit a
        [(0-bit) (pair b d0)]
        [(1-bit)
         (match-adt Bit b
           [(0-bit) (pair d1 d0)]
           [(1-bit) (pair d0 d1)])]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; natural-drop : Natural Natural -> Natural
(define natural-drop
  (λ (a n)
    (match-adt Maybe (?sub1 n)
      [(none) a]
      [(some n-1)
       (natural-drop (quotient2 a) n-1)])))

;; natural-take : Natural Natural -> Natural
(define natural-take
  (λ (a n)
    (match-adt Maybe (?sub1 n)
      [(none) n0]
      [(some n-1)
       (nat-cons* (remainder2->bit a) (natural-take (quotient2 a) n-1))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define n0 (zero))
(define n1 (nat-cons d1 n0))
(define n2 (nat-cons d0 n1))
(define n3 (nat-cons d1 n1))
(define n4 (nat-cons d0 n2))
(define n5 (nat-cons d1 n2))
(define n6 (nat-cons d0 n3))
(define n7 (nat-cons d1 n3))
(define n8 (nat-cons d0 n4))
(define n9 (nat-cons d1 n4))
(define n10 (nat-cons d0 n5))
(define n11 (nat-cons d1 n5))
(define n12 (nat-cons d0 n6))
(define n13 (nat-cons d1 n6))
(define n14 (nat-cons d0 n7))
(define n15 (nat-cons d1 n7))

(define n16 (nat-cons d0 n8))
(define n17 (nat-cons d1 n8))
(define n18 (nat-cons d0 n9))
(define n19 (nat-cons d1 n9))
(define n20 (nat-cons d0 n10))
(define n21 (nat-cons d1 n10))
(define n22 (nat-cons d0 n11))
(define n23 (nat-cons d1 n11))
(define n24 (nat-cons d0 n12))
(define n25 (nat-cons d1 n12))
(define n26 (nat-cons d0 n13))
(define n27 (nat-cons d1 n13))
(define n28 (nat-cons d0 n14))
(define n29 (nat-cons d1 n14))
(define n30 (nat-cons d0 n15))
(define n31 (nat-cons d1 n15))

(define n32 (nat-cons d0 n16))

