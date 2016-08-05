#lang gnal λ/adt

(provide Digit digit=?
         ?digit-sub1
         d00of32 d01of32 d02of32 d03of32 d04of32 d05of32 d06of32 d07of32
         d08of32 d09of32 d10of32 d11of32 d12of32 d13of32 d14of32 d15of32
         d16of32 d17of32 d18of32 d19of32 d20of32 d21of32 d22of32 d23of32
         d24of32 d25of32 d26of32 d27of32 d28of32 d29of32 d30of32 d31of32
         d00 d01 d02 d03 d04 d05 d06 d07
         d08 d09 d10 d11 d12 d13 d14 d15
         d16 d17 d18 d19 d20 d21 d22 d23
         d24 d25 d26 d27 d28 d29 d30 d31
         d01?
         Natural natural=?
         zero nat-cons zero?
         + add1
         ?∆ ?sub1
         quotient32
         remainder32
         remainder32->digit
         n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15
         n16 n17 n18 n19 n20 n21 n22 n23 n24 n25 n26 n27 n28 n29 n30 n31
         n32 n33 n34 n35 n36 n37 n38 n39 n40 n41 n42 n43 n44 n45 n46 n47
         n48 n49 n50 n51 n52 n53 n54 n55 n56 n57 n58 n59 n60 n61 n62 n63
         n64
         )

(require "boolean.rkt"
         "maybe.rkt"
         "byte.rkt")

;; a base-32 digit
(define-adt Digit
  (d00of32) (d01of32) (d02of32) (d03of32) (d04of32) (d05of32) (d06of32) (d07of32)
  (d08of32) (d09of32) (d10of32) (d11of32) (d12of32) (d13of32) (d14of32) (d15of32)
  (d16of32) (d17of32) (d18of32) (d19of32) (d20of32) (d21of32) (d22of32) (d23of32)
  (d24of32) (d25of32) (d26of32) (d27of32) (d28of32) (d29of32) (d30of32) (d31of32))

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
       (match-adt Boolean (d00? a0)
         [(true) (zero?* ar)]
         [(false) (false)])])))

(define nat-cons*
  (λ (1s-digit rest)
    (match-adt Boolean (and (d00? 1s-digit) (zero?* rest))
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

(define d00 (d00of32))
(define d01 (d01of32))
(define d02 (d02of32))
(define d03 (d03of32))
(define d04 (d04of32))
(define d05 (d05of32))
(define d06 (d06of32))
(define d07 (d07of32))
(define d08 (d08of32))
(define d09 (d09of32))
(define d10 (d10of32))
(define d11 (d11of32))
(define d12 (d12of32))
(define d13 (d13of32))
(define d14 (d14of32))
(define d15 (d15of32))
(define d16 (d16of32))
(define d17 (d17of32))
(define d18 (d18of32))
(define d19 (d19of32))
(define d20 (d20of32))
(define d21 (d21of32))
(define d22 (d22of32))
(define d23 (d23of32))
(define d24 (d24of32))
(define d25 (d25of32))
(define d26 (d26of32))
(define d27 (d27of32))
(define d28 (d28of32))
(define d29 (d29of32))
(define d30 (d30of32))
(define d31 (d31of32))

(define d00?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (true)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d01?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (true)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d02?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (true)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d03?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (true)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d04?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (true)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d05?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (true)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d06?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (true)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d07?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (true)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d08?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (true)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d09?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (true)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d10?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (true)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d11?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (true)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d12?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (true)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d13?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (true)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d14?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (true)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d15?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (true)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d16?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (true)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d17?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (true)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d18?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (true)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d19?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (true)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d20?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (true)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d21?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (true)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d22?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (true)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d23?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (true)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d24?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (true)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d25?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (true)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d26?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (true)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d27?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (true)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d28?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (true)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (false)])))

(define d29?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (true)] [(d30of32) (false)] [(d31of32) (false)])))

(define d30?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (true)] [(d31of32) (false)])))

(define d31?
  (λ (d)
    (match-adt Digit d
      [(d00of32) (false)] [(d01of32) (false)] [(d02of32) (false)] [(d03of32) (false)]
      [(d04of32) (false)] [(d05of32) (false)] [(d06of32) (false)] [(d07of32) (false)]
      [(d08of32) (false)] [(d09of32) (false)] [(d10of32) (false)] [(d11of32) (false)]
      [(d12of32) (false)] [(d13of32) (false)] [(d14of32) (false)] [(d15of32) (false)]
      [(d16of32) (false)] [(d17of32) (false)] [(d18of32) (false)] [(d19of32) (false)]
      [(d20of32) (false)] [(d21of32) (false)] [(d22of32) (false)] [(d23of32) (false)]
      [(d24of32) (false)] [(d25of32) (false)] [(d26of32) (false)] [(d27of32) (false)]
      [(d28of32) (false)] [(d29of32) (false)] [(d30of32) (false)] [(d31of32) (true)])))

(define digit=?
  (λ (a b)
    (match-adt Digit a
      [(d00of32) (d00? b)] [(d01of32) (d01? b)] [(d02of32) (d02? b)] [(d03of32) (d03? b)]
      [(d04of32) (d04? b)] [(d05of32) (d05? b)] [(d06of32) (d06? b)] [(d07of32) (d07? b)]
      [(d08of32) (d08? b)] [(d09of32) (d09? b)] [(d10of32) (d10? b)] [(d11of32) (d11? b)]
      [(d12of32) (d12? b)] [(d13of32) (d13? b)] [(d14of32) (d14? b)] [(d15of32) (d15? b)]
      [(d16of32) (d16? b)] [(d17of32) (d17? b)] [(d18of32) (d18? b)] [(d19of32) (d19? b)]
      [(d20of32) (d20? b)] [(d21of32) (d21? b)] [(d22of32) (d22? b)] [(d23of32) (d23? b)]
      [(d24of32) (d24? b)] [(d25of32) (d25? b)] [(d26of32) (d26? b)] [(d27of32) (d27? b)]
      [(d28of32) (d28? b)] [(d29of32) (d29? b)] [(d30of32) (d30? b)] [(d31of32) (d31? b)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add1 : Natural -> Natural
(define add1
  (λ (a)
    (+ a n1)))

;; + : Natural Natural -> Natural
(define +
  (λ (a b)
    (+/carry a b d00)))

;; quotient32 : Natural -> Natural
(define quotient32
  (λ (a)
    (match-adt Natural a
      [(zero) n0]
      [(nat-cons a0 a-rest) a-rest])))

;; remainder32 : Natural -> Natural
(define remainder32
  (λ (a)
    (match-adt Natural a
      [(zero) n0]
      [(nat-cons a0 a-rest)
       (nat-cons* a0 n0)])))

;; remainder32->digit : Natural -> Digit
(define remainder32->digit
  (λ (a)
    (match-adt Natural a
      [(zero) d00]
      [(nat-cons a0 ar) a0])))

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
            [(1-bit) (nat-cons ad0 (+/digit ar d01))])))])))

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
            [(some ar-1) (some (nat-cons* d31 ar-1))])]
         [(some a0-1)
          (some (nat-cons* a0-1 ar))])])))

;; ?digit-sub1 : Digit -> (Maybe Digit)
(define ?digit-sub1
  (λ (d)
    (match-adt Digit d
      [(d00of32) (none)]
      [(d01of32) (some d00)]
      [(d02of32) (some d01)]
      [(d03of32) (some d02)]
      [(d04of32) (some d03)]
      [(d05of32) (some d04)]
      [(d06of32) (some d05)]
      [(d07of32) (some d06)]
      [(d08of32) (some d07)]
      [(d09of32) (some d08)]
      [(d10of32) (some d09)]
      [(d11of32) (some d10)]
      [(d12of32) (some d11)]
      [(d13of32) (some d12)]
      [(d14of32) (some d13)]
      [(d15of32) (some d14)]
      [(d16of32) (some d15)]
      [(d17of32) (some d16)]
      [(d18of32) (some d17)]
      [(d19of32) (some d18)]
      [(d20of32) (some d19)]
      [(d21of32) (some d20)]
      [(d22of32) (some d21)]
      [(d23of32) (some d22)]
      [(d24of32) (some d23)]
      [(d25of32) (some d24)]
      [(d26of32) (some d25)]
      [(d27of32) (some d26)]
      [(d28of32) (some d27)]
      [(d29of32) (some d28)]
      [(d30of32) (some d29)]
      [(d31of32) (some d30)])))

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
                           [(0-bit) d00]
                           [(1-bit) d01])]
                [(1-bit) (match-adt Bit ab0c1
                           [(0-bit) d01]
                           [(1-bit) d02])])))))))))

;; half-adder : Digit Digit -> (Pair Digit Bit)
(define half-adder
  (λ (a b)
    (λ (pair)
      (match-adt Digit a
        [(d00of32) (pair b (0-bit))]
        [(d01of32)
         (match-adt Digit b
           [(d00of32) (pair d01 (0-bit))]
           [(d01of32) (pair d02 (0-bit))]
           [(d02of32) (pair d03 (0-bit))]
           [(d03of32) (pair d04 (0-bit))]
           [(d04of32) (pair d05 (0-bit))]
           [(d05of32) (pair d06 (0-bit))]
           [(d06of32) (pair d07 (0-bit))]
           [(d07of32) (pair d08 (0-bit))]
           [(d08of32) (pair d09 (0-bit))]
           [(d09of32) (pair d10 (0-bit))]
           [(d10of32) (pair d11 (0-bit))]
           [(d11of32) (pair d12 (0-bit))]
           [(d12of32) (pair d13 (0-bit))]
           [(d13of32) (pair d14 (0-bit))]
           [(d14of32) (pair d15 (0-bit))]
           [(d15of32) (pair d16 (0-bit))]
           [(d16of32) (pair d17 (0-bit))]
           [(d17of32) (pair d18 (0-bit))]
           [(d18of32) (pair d19 (0-bit))]
           [(d19of32) (pair d20 (0-bit))]
           [(d20of32) (pair d21 (0-bit))]
           [(d21of32) (pair d22 (0-bit))]
           [(d22of32) (pair d23 (0-bit))]
           [(d23of32) (pair d24 (0-bit))]
           [(d24of32) (pair d25 (0-bit))]
           [(d25of32) (pair d26 (0-bit))]
           [(d26of32) (pair d27 (0-bit))]
           [(d27of32) (pair d28 (0-bit))]
           [(d28of32) (pair d29 (0-bit))]
           [(d29of32) (pair d30 (0-bit))]
           [(d30of32) (pair d31 (0-bit))]
           [(d31of32) (pair d00 (1-bit))])]
        [(d02of32)
         (match-adt Digit b
           [(d00of32) (pair d02 (0-bit))]
           [(d01of32) (pair d03 (0-bit))]
           [(d02of32) (pair d04 (0-bit))]
           [(d03of32) (pair d05 (0-bit))]
           [(d04of32) (pair d06 (0-bit))]
           [(d05of32) (pair d07 (0-bit))]
           [(d06of32) (pair d08 (0-bit))]
           [(d07of32) (pair d09 (0-bit))]
           [(d08of32) (pair d10 (0-bit))]
           [(d09of32) (pair d11 (0-bit))]
           [(d10of32) (pair d12 (0-bit))]
           [(d11of32) (pair d13 (0-bit))]
           [(d12of32) (pair d14 (0-bit))]
           [(d13of32) (pair d15 (0-bit))]
           [(d14of32) (pair d16 (0-bit))]
           [(d15of32) (pair d17 (0-bit))]
           [(d16of32) (pair d18 (0-bit))]
           [(d17of32) (pair d19 (0-bit))]
           [(d18of32) (pair d20 (0-bit))]
           [(d19of32) (pair d21 (0-bit))]
           [(d20of32) (pair d22 (0-bit))]
           [(d21of32) (pair d23 (0-bit))]
           [(d22of32) (pair d24 (0-bit))]
           [(d23of32) (pair d25 (0-bit))]
           [(d24of32) (pair d26 (0-bit))]
           [(d25of32) (pair d27 (0-bit))]
           [(d26of32) (pair d28 (0-bit))]
           [(d27of32) (pair d29 (0-bit))]
           [(d28of32) (pair d30 (0-bit))]
           [(d29of32) (pair d31 (0-bit))]
           [(d30of32) (pair d00 (1-bit))]
           [(d31of32) (pair d01 (1-bit))])]
        [(d03of32)
         (match-adt Digit b
           [(d00of32) (pair d03 (0-bit))]
           [(d01of32) (pair d04 (0-bit))]
           [(d02of32) (pair d05 (0-bit))]
           [(d03of32) (pair d06 (0-bit))]
           [(d04of32) (pair d07 (0-bit))]
           [(d05of32) (pair d08 (0-bit))]
           [(d06of32) (pair d09 (0-bit))]
           [(d07of32) (pair d10 (0-bit))]
           [(d08of32) (pair d11 (0-bit))]
           [(d09of32) (pair d12 (0-bit))]
           [(d10of32) (pair d13 (0-bit))]
           [(d11of32) (pair d14 (0-bit))]
           [(d12of32) (pair d15 (0-bit))]
           [(d13of32) (pair d16 (0-bit))]
           [(d14of32) (pair d17 (0-bit))]
           [(d15of32) (pair d18 (0-bit))]
           [(d16of32) (pair d19 (0-bit))]
           [(d17of32) (pair d20 (0-bit))]
           [(d18of32) (pair d21 (0-bit))]
           [(d19of32) (pair d22 (0-bit))]
           [(d20of32) (pair d23 (0-bit))]
           [(d21of32) (pair d24 (0-bit))]
           [(d22of32) (pair d25 (0-bit))]
           [(d23of32) (pair d26 (0-bit))]
           [(d24of32) (pair d27 (0-bit))]
           [(d25of32) (pair d28 (0-bit))]
           [(d26of32) (pair d29 (0-bit))]
           [(d27of32) (pair d30 (0-bit))]
           [(d28of32) (pair d31 (0-bit))]
           [(d29of32) (pair d00 (1-bit))]
           [(d30of32) (pair d01 (1-bit))]
           [(d31of32) (pair d02 (1-bit))])]
        [(d04of32)
         (match-adt Digit b
           [(d00of32) (pair d04 (0-bit))]
           [(d01of32) (pair d05 (0-bit))]
           [(d02of32) (pair d06 (0-bit))]
           [(d03of32) (pair d07 (0-bit))]
           [(d04of32) (pair d08 (0-bit))]
           [(d05of32) (pair d09 (0-bit))]
           [(d06of32) (pair d10 (0-bit))]
           [(d07of32) (pair d11 (0-bit))]
           [(d08of32) (pair d12 (0-bit))]
           [(d09of32) (pair d13 (0-bit))]
           [(d10of32) (pair d14 (0-bit))]
           [(d11of32) (pair d15 (0-bit))]
           [(d12of32) (pair d16 (0-bit))]
           [(d13of32) (pair d17 (0-bit))]
           [(d14of32) (pair d18 (0-bit))]
           [(d15of32) (pair d19 (0-bit))]
           [(d16of32) (pair d20 (0-bit))]
           [(d17of32) (pair d21 (0-bit))]
           [(d18of32) (pair d22 (0-bit))]
           [(d19of32) (pair d23 (0-bit))]
           [(d20of32) (pair d24 (0-bit))]
           [(d21of32) (pair d25 (0-bit))]
           [(d22of32) (pair d26 (0-bit))]
           [(d23of32) (pair d27 (0-bit))]
           [(d24of32) (pair d28 (0-bit))]
           [(d25of32) (pair d29 (0-bit))]
           [(d26of32) (pair d30 (0-bit))]
           [(d27of32) (pair d31 (0-bit))]
           [(d28of32) (pair d00 (1-bit))]
           [(d29of32) (pair d01 (1-bit))]
           [(d30of32) (pair d02 (1-bit))]
           [(d31of32) (pair d03 (1-bit))])]
        [(d05of32)
         (match-adt Digit b
           [(d00of32) (pair d05 (0-bit))]
           [(d01of32) (pair d06 (0-bit))]
           [(d02of32) (pair d07 (0-bit))]
           [(d03of32) (pair d08 (0-bit))]
           [(d04of32) (pair d09 (0-bit))]
           [(d05of32) (pair d10 (0-bit))]
           [(d06of32) (pair d11 (0-bit))]
           [(d07of32) (pair d12 (0-bit))]
           [(d08of32) (pair d13 (0-bit))]
           [(d09of32) (pair d14 (0-bit))]
           [(d10of32) (pair d15 (0-bit))]
           [(d11of32) (pair d16 (0-bit))]
           [(d12of32) (pair d17 (0-bit))]
           [(d13of32) (pair d18 (0-bit))]
           [(d14of32) (pair d19 (0-bit))]
           [(d15of32) (pair d20 (0-bit))]
           [(d16of32) (pair d21 (0-bit))]
           [(d17of32) (pair d22 (0-bit))]
           [(d18of32) (pair d23 (0-bit))]
           [(d19of32) (pair d24 (0-bit))]
           [(d20of32) (pair d25 (0-bit))]
           [(d21of32) (pair d26 (0-bit))]
           [(d22of32) (pair d27 (0-bit))]
           [(d23of32) (pair d28 (0-bit))]
           [(d24of32) (pair d29 (0-bit))]
           [(d25of32) (pair d30 (0-bit))]
           [(d26of32) (pair d31 (0-bit))]
           [(d27of32) (pair d00 (1-bit))]
           [(d28of32) (pair d01 (1-bit))]
           [(d29of32) (pair d02 (1-bit))]
           [(d30of32) (pair d03 (1-bit))]
           [(d31of32) (pair d04 (1-bit))])]
        [(d06of32)
         (match-adt Digit b
           [(d00of32) (pair d06 (0-bit))]
           [(d01of32) (pair d07 (0-bit))]
           [(d02of32) (pair d08 (0-bit))]
           [(d03of32) (pair d09 (0-bit))]
           [(d04of32) (pair d10 (0-bit))]
           [(d05of32) (pair d11 (0-bit))]
           [(d06of32) (pair d12 (0-bit))]
           [(d07of32) (pair d13 (0-bit))]
           [(d08of32) (pair d14 (0-bit))]
           [(d09of32) (pair d15 (0-bit))]
           [(d10of32) (pair d16 (0-bit))]
           [(d11of32) (pair d17 (0-bit))]
           [(d12of32) (pair d18 (0-bit))]
           [(d13of32) (pair d19 (0-bit))]
           [(d14of32) (pair d20 (0-bit))]
           [(d15of32) (pair d21 (0-bit))]
           [(d16of32) (pair d22 (0-bit))]
           [(d17of32) (pair d23 (0-bit))]
           [(d18of32) (pair d24 (0-bit))]
           [(d19of32) (pair d25 (0-bit))]
           [(d20of32) (pair d26 (0-bit))]
           [(d21of32) (pair d27 (0-bit))]
           [(d22of32) (pair d28 (0-bit))]
           [(d23of32) (pair d29 (0-bit))]
           [(d24of32) (pair d30 (0-bit))]
           [(d25of32) (pair d31 (0-bit))]
           [(d26of32) (pair d00 (1-bit))]
           [(d27of32) (pair d01 (1-bit))]
           [(d28of32) (pair d02 (1-bit))]
           [(d29of32) (pair d03 (1-bit))]
           [(d30of32) (pair d04 (1-bit))]
           [(d31of32) (pair d05 (1-bit))])]
        [(d07of32)
         (match-adt Digit b
           [(d00of32) (pair d07 (0-bit))]
           [(d01of32) (pair d08 (0-bit))]
           [(d02of32) (pair d09 (0-bit))]
           [(d03of32) (pair d10 (0-bit))]
           [(d04of32) (pair d11 (0-bit))]
           [(d05of32) (pair d12 (0-bit))]
           [(d06of32) (pair d13 (0-bit))]
           [(d07of32) (pair d14 (0-bit))]
           [(d08of32) (pair d15 (0-bit))]
           [(d09of32) (pair d16 (0-bit))]
           [(d10of32) (pair d17 (0-bit))]
           [(d11of32) (pair d18 (0-bit))]
           [(d12of32) (pair d19 (0-bit))]
           [(d13of32) (pair d20 (0-bit))]
           [(d14of32) (pair d21 (0-bit))]
           [(d15of32) (pair d22 (0-bit))]
           [(d16of32) (pair d23 (0-bit))]
           [(d17of32) (pair d24 (0-bit))]
           [(d18of32) (pair d25 (0-bit))]
           [(d19of32) (pair d26 (0-bit))]
           [(d20of32) (pair d27 (0-bit))]
           [(d21of32) (pair d28 (0-bit))]
           [(d22of32) (pair d29 (0-bit))]
           [(d23of32) (pair d30 (0-bit))]
           [(d24of32) (pair d31 (0-bit))]
           [(d25of32) (pair d00 (1-bit))]
           [(d26of32) (pair d01 (1-bit))]
           [(d27of32) (pair d02 (1-bit))]
           [(d28of32) (pair d03 (1-bit))]
           [(d29of32) (pair d04 (1-bit))]
           [(d30of32) (pair d05 (1-bit))]
           [(d31of32) (pair d06 (1-bit))])]
        [(d08of32)
         (match-adt Digit b
           [(d00of32) (pair d08 (0-bit))]
           [(d01of32) (pair d09 (0-bit))]
           [(d02of32) (pair d10 (0-bit))]
           [(d03of32) (pair d11 (0-bit))]
           [(d04of32) (pair d12 (0-bit))]
           [(d05of32) (pair d13 (0-bit))]
           [(d06of32) (pair d14 (0-bit))]
           [(d07of32) (pair d15 (0-bit))]
           [(d08of32) (pair d16 (0-bit))]
           [(d09of32) (pair d17 (0-bit))]
           [(d10of32) (pair d18 (0-bit))]
           [(d11of32) (pair d19 (0-bit))]
           [(d12of32) (pair d20 (0-bit))]
           [(d13of32) (pair d21 (0-bit))]
           [(d14of32) (pair d22 (0-bit))]
           [(d15of32) (pair d23 (0-bit))]
           [(d16of32) (pair d24 (0-bit))]
           [(d17of32) (pair d25 (0-bit))]
           [(d18of32) (pair d26 (0-bit))]
           [(d19of32) (pair d27 (0-bit))]
           [(d20of32) (pair d28 (0-bit))]
           [(d21of32) (pair d29 (0-bit))]
           [(d22of32) (pair d30 (0-bit))]
           [(d23of32) (pair d31 (0-bit))]
           [(d24of32) (pair d00 (1-bit))]
           [(d25of32) (pair d01 (1-bit))]
           [(d26of32) (pair d02 (1-bit))]
           [(d27of32) (pair d03 (1-bit))]
           [(d28of32) (pair d04 (1-bit))]
           [(d29of32) (pair d05 (1-bit))]
           [(d30of32) (pair d06 (1-bit))]
           [(d31of32) (pair d07 (1-bit))])]
        [(d09of32)
         (match-adt Digit b
           [(d00of32) (pair d09 (0-bit))]
           [(d01of32) (pair d10 (0-bit))]
           [(d02of32) (pair d11 (0-bit))]
           [(d03of32) (pair d12 (0-bit))]
           [(d04of32) (pair d13 (0-bit))]
           [(d05of32) (pair d14 (0-bit))]
           [(d06of32) (pair d15 (0-bit))]
           [(d07of32) (pair d16 (0-bit))]
           [(d08of32) (pair d17 (0-bit))]
           [(d09of32) (pair d18 (0-bit))]
           [(d10of32) (pair d19 (0-bit))]
           [(d11of32) (pair d20 (0-bit))]
           [(d12of32) (pair d21 (0-bit))]
           [(d13of32) (pair d22 (0-bit))]
           [(d14of32) (pair d23 (0-bit))]
           [(d15of32) (pair d24 (0-bit))]
           [(d16of32) (pair d25 (0-bit))]
           [(d17of32) (pair d26 (0-bit))]
           [(d18of32) (pair d27 (0-bit))]
           [(d19of32) (pair d28 (0-bit))]
           [(d20of32) (pair d29 (0-bit))]
           [(d21of32) (pair d30 (0-bit))]
           [(d22of32) (pair d31 (0-bit))]
           [(d23of32) (pair d00 (1-bit))]
           [(d24of32) (pair d01 (1-bit))]
           [(d25of32) (pair d02 (1-bit))]
           [(d26of32) (pair d03 (1-bit))]
           [(d27of32) (pair d04 (1-bit))]
           [(d28of32) (pair d05 (1-bit))]
           [(d29of32) (pair d06 (1-bit))]
           [(d30of32) (pair d07 (1-bit))]
           [(d31of32) (pair d08 (1-bit))])]
        [(d10of32)
         (match-adt Digit b
           [(d00of32) (pair d10 (0-bit))]
           [(d01of32) (pair d11 (0-bit))]
           [(d02of32) (pair d12 (0-bit))]
           [(d03of32) (pair d13 (0-bit))]
           [(d04of32) (pair d14 (0-bit))]
           [(d05of32) (pair d15 (0-bit))]
           [(d06of32) (pair d16 (0-bit))]
           [(d07of32) (pair d17 (0-bit))]
           [(d08of32) (pair d18 (0-bit))]
           [(d09of32) (pair d19 (0-bit))]
           [(d10of32) (pair d20 (0-bit))]
           [(d11of32) (pair d21 (0-bit))]
           [(d12of32) (pair d22 (0-bit))]
           [(d13of32) (pair d23 (0-bit))]
           [(d14of32) (pair d24 (0-bit))]
           [(d15of32) (pair d25 (0-bit))]
           [(d16of32) (pair d26 (0-bit))]
           [(d17of32) (pair d27 (0-bit))]
           [(d18of32) (pair d28 (0-bit))]
           [(d19of32) (pair d29 (0-bit))]
           [(d20of32) (pair d30 (0-bit))]
           [(d21of32) (pair d31 (0-bit))]
           [(d22of32) (pair d00 (1-bit))]
           [(d23of32) (pair d01 (1-bit))]
           [(d24of32) (pair d02 (1-bit))]
           [(d25of32) (pair d03 (1-bit))]
           [(d26of32) (pair d04 (1-bit))]
           [(d27of32) (pair d05 (1-bit))]
           [(d28of32) (pair d06 (1-bit))]
           [(d29of32) (pair d07 (1-bit))]
           [(d30of32) (pair d08 (1-bit))]
           [(d31of32) (pair d09 (1-bit))])]
        [(d11of32)
         (match-adt Digit b
           [(d00of32) (pair d11 (0-bit))]
           [(d01of32) (pair d12 (0-bit))]
           [(d02of32) (pair d13 (0-bit))]
           [(d03of32) (pair d14 (0-bit))]
           [(d04of32) (pair d15 (0-bit))]
           [(d05of32) (pair d16 (0-bit))]
           [(d06of32) (pair d17 (0-bit))]
           [(d07of32) (pair d18 (0-bit))]
           [(d08of32) (pair d19 (0-bit))]
           [(d09of32) (pair d20 (0-bit))]
           [(d10of32) (pair d21 (0-bit))]
           [(d11of32) (pair d22 (0-bit))]
           [(d12of32) (pair d23 (0-bit))]
           [(d13of32) (pair d24 (0-bit))]
           [(d14of32) (pair d25 (0-bit))]
           [(d15of32) (pair d26 (0-bit))]
           [(d16of32) (pair d27 (0-bit))]
           [(d17of32) (pair d28 (0-bit))]
           [(d18of32) (pair d29 (0-bit))]
           [(d19of32) (pair d30 (0-bit))]
           [(d20of32) (pair d31 (0-bit))]
           [(d21of32) (pair d00 (1-bit))]
           [(d22of32) (pair d01 (1-bit))]
           [(d23of32) (pair d02 (1-bit))]
           [(d24of32) (pair d03 (1-bit))]
           [(d25of32) (pair d04 (1-bit))]
           [(d26of32) (pair d05 (1-bit))]
           [(d27of32) (pair d06 (1-bit))]
           [(d28of32) (pair d07 (1-bit))]
           [(d29of32) (pair d08 (1-bit))]
           [(d30of32) (pair d09 (1-bit))]
           [(d31of32) (pair d10 (1-bit))])]
        [(d12of32)
         (match-adt Digit b
           [(d00of32) (pair d12 (0-bit))]
           [(d01of32) (pair d13 (0-bit))]
           [(d02of32) (pair d14 (0-bit))]
           [(d03of32) (pair d15 (0-bit))]
           [(d04of32) (pair d16 (0-bit))]
           [(d05of32) (pair d17 (0-bit))]
           [(d06of32) (pair d18 (0-bit))]
           [(d07of32) (pair d19 (0-bit))]
           [(d08of32) (pair d20 (0-bit))]
           [(d09of32) (pair d21 (0-bit))]
           [(d10of32) (pair d22 (0-bit))]
           [(d11of32) (pair d23 (0-bit))]
           [(d12of32) (pair d24 (0-bit))]
           [(d13of32) (pair d25 (0-bit))]
           [(d14of32) (pair d26 (0-bit))]
           [(d15of32) (pair d27 (0-bit))]
           [(d16of32) (pair d28 (0-bit))]
           [(d17of32) (pair d29 (0-bit))]
           [(d18of32) (pair d30 (0-bit))]
           [(d19of32) (pair d31 (0-bit))]
           [(d20of32) (pair d00 (1-bit))]
           [(d21of32) (pair d01 (1-bit))]
           [(d22of32) (pair d02 (1-bit))]
           [(d23of32) (pair d03 (1-bit))]
           [(d24of32) (pair d04 (1-bit))]
           [(d25of32) (pair d05 (1-bit))]
           [(d26of32) (pair d06 (1-bit))]
           [(d27of32) (pair d07 (1-bit))]
           [(d28of32) (pair d08 (1-bit))]
           [(d29of32) (pair d09 (1-bit))]
           [(d30of32) (pair d10 (1-bit))]
           [(d31of32) (pair d11 (1-bit))])]
        [(d13of32)
         (match-adt Digit b
           [(d00of32) (pair d13 (0-bit))]
           [(d01of32) (pair d14 (0-bit))]
           [(d02of32) (pair d15 (0-bit))]
           [(d03of32) (pair d16 (0-bit))]
           [(d04of32) (pair d17 (0-bit))]
           [(d05of32) (pair d18 (0-bit))]
           [(d06of32) (pair d19 (0-bit))]
           [(d07of32) (pair d20 (0-bit))]
           [(d08of32) (pair d21 (0-bit))]
           [(d09of32) (pair d22 (0-bit))]
           [(d10of32) (pair d23 (0-bit))]
           [(d11of32) (pair d24 (0-bit))]
           [(d12of32) (pair d25 (0-bit))]
           [(d13of32) (pair d26 (0-bit))]
           [(d14of32) (pair d27 (0-bit))]
           [(d15of32) (pair d28 (0-bit))]
           [(d16of32) (pair d29 (0-bit))]
           [(d17of32) (pair d30 (0-bit))]
           [(d18of32) (pair d31 (0-bit))]
           [(d19of32) (pair d00 (1-bit))]
           [(d20of32) (pair d01 (1-bit))]
           [(d21of32) (pair d02 (1-bit))]
           [(d22of32) (pair d03 (1-bit))]
           [(d23of32) (pair d04 (1-bit))]
           [(d24of32) (pair d05 (1-bit))]
           [(d25of32) (pair d06 (1-bit))]
           [(d26of32) (pair d07 (1-bit))]
           [(d27of32) (pair d08 (1-bit))]
           [(d28of32) (pair d09 (1-bit))]
           [(d29of32) (pair d10 (1-bit))]
           [(d30of32) (pair d11 (1-bit))]
           [(d31of32) (pair d12 (1-bit))])]
        [(d14of32)
         (match-adt Digit b
           [(d00of32) (pair d14 (0-bit))]
           [(d01of32) (pair d15 (0-bit))]
           [(d02of32) (pair d16 (0-bit))]
           [(d03of32) (pair d17 (0-bit))]
           [(d04of32) (pair d18 (0-bit))]
           [(d05of32) (pair d19 (0-bit))]
           [(d06of32) (pair d20 (0-bit))]
           [(d07of32) (pair d21 (0-bit))]
           [(d08of32) (pair d22 (0-bit))]
           [(d09of32) (pair d23 (0-bit))]
           [(d10of32) (pair d24 (0-bit))]
           [(d11of32) (pair d25 (0-bit))]
           [(d12of32) (pair d26 (0-bit))]
           [(d13of32) (pair d27 (0-bit))]
           [(d14of32) (pair d28 (0-bit))]
           [(d15of32) (pair d29 (0-bit))]
           [(d16of32) (pair d30 (0-bit))]
           [(d17of32) (pair d31 (0-bit))]
           [(d18of32) (pair d00 (1-bit))]
           [(d19of32) (pair d01 (1-bit))]
           [(d20of32) (pair d02 (1-bit))]
           [(d21of32) (pair d03 (1-bit))]
           [(d22of32) (pair d04 (1-bit))]
           [(d23of32) (pair d05 (1-bit))]
           [(d24of32) (pair d06 (1-bit))]
           [(d25of32) (pair d07 (1-bit))]
           [(d26of32) (pair d08 (1-bit))]
           [(d27of32) (pair d09 (1-bit))]
           [(d28of32) (pair d10 (1-bit))]
           [(d29of32) (pair d11 (1-bit))]
           [(d30of32) (pair d12 (1-bit))]
           [(d31of32) (pair d13 (1-bit))])]
        [(d15of32)
         (match-adt Digit b
           [(d00of32) (pair d15 (0-bit))]
           [(d01of32) (pair d16 (0-bit))]
           [(d02of32) (pair d17 (0-bit))]
           [(d03of32) (pair d18 (0-bit))]
           [(d04of32) (pair d19 (0-bit))]
           [(d05of32) (pair d20 (0-bit))]
           [(d06of32) (pair d21 (0-bit))]
           [(d07of32) (pair d22 (0-bit))]
           [(d08of32) (pair d23 (0-bit))]
           [(d09of32) (pair d24 (0-bit))]
           [(d10of32) (pair d25 (0-bit))]
           [(d11of32) (pair d26 (0-bit))]
           [(d12of32) (pair d27 (0-bit))]
           [(d13of32) (pair d28 (0-bit))]
           [(d14of32) (pair d29 (0-bit))]
           [(d15of32) (pair d30 (0-bit))]
           [(d16of32) (pair d31 (0-bit))]
           [(d17of32) (pair d00 (1-bit))]
           [(d18of32) (pair d01 (1-bit))]
           [(d19of32) (pair d02 (1-bit))]
           [(d20of32) (pair d03 (1-bit))]
           [(d21of32) (pair d04 (1-bit))]
           [(d22of32) (pair d05 (1-bit))]
           [(d23of32) (pair d06 (1-bit))]
           [(d24of32) (pair d07 (1-bit))]
           [(d25of32) (pair d08 (1-bit))]
           [(d26of32) (pair d09 (1-bit))]
           [(d27of32) (pair d10 (1-bit))]
           [(d28of32) (pair d11 (1-bit))]
           [(d29of32) (pair d12 (1-bit))]
           [(d30of32) (pair d13 (1-bit))]
           [(d31of32) (pair d14 (1-bit))])]
        [(d16of32)
         (match-adt Digit b
           [(d00of32) (pair d16 (0-bit))]
           [(d01of32) (pair d17 (0-bit))]
           [(d02of32) (pair d18 (0-bit))]
           [(d03of32) (pair d19 (0-bit))]
           [(d04of32) (pair d20 (0-bit))]
           [(d05of32) (pair d21 (0-bit))]
           [(d06of32) (pair d22 (0-bit))]
           [(d07of32) (pair d23 (0-bit))]
           [(d08of32) (pair d24 (0-bit))]
           [(d09of32) (pair d25 (0-bit))]
           [(d10of32) (pair d26 (0-bit))]
           [(d11of32) (pair d27 (0-bit))]
           [(d12of32) (pair d28 (0-bit))]
           [(d13of32) (pair d29 (0-bit))]
           [(d14of32) (pair d30 (0-bit))]
           [(d15of32) (pair d31 (0-bit))]
           [(d16of32) (pair d00 (1-bit))]
           [(d17of32) (pair d01 (1-bit))]
           [(d18of32) (pair d02 (1-bit))]
           [(d19of32) (pair d03 (1-bit))]
           [(d20of32) (pair d04 (1-bit))]
           [(d21of32) (pair d05 (1-bit))]
           [(d22of32) (pair d06 (1-bit))]
           [(d23of32) (pair d07 (1-bit))]
           [(d24of32) (pair d08 (1-bit))]
           [(d25of32) (pair d09 (1-bit))]
           [(d26of32) (pair d10 (1-bit))]
           [(d27of32) (pair d11 (1-bit))]
           [(d28of32) (pair d12 (1-bit))]
           [(d29of32) (pair d13 (1-bit))]
           [(d30of32) (pair d14 (1-bit))]
           [(d31of32) (pair d15 (1-bit))])]
        [(d17of32)
         (match-adt Digit b
           [(d00of32) (pair d17 (0-bit))]
           [(d01of32) (pair d18 (0-bit))]
           [(d02of32) (pair d19 (0-bit))]
           [(d03of32) (pair d20 (0-bit))]
           [(d04of32) (pair d21 (0-bit))]
           [(d05of32) (pair d22 (0-bit))]
           [(d06of32) (pair d23 (0-bit))]
           [(d07of32) (pair d24 (0-bit))]
           [(d08of32) (pair d25 (0-bit))]
           [(d09of32) (pair d26 (0-bit))]
           [(d10of32) (pair d27 (0-bit))]
           [(d11of32) (pair d28 (0-bit))]
           [(d12of32) (pair d29 (0-bit))]
           [(d13of32) (pair d30 (0-bit))]
           [(d14of32) (pair d31 (0-bit))]
           [(d15of32) (pair d00 (1-bit))]
           [(d16of32) (pair d01 (1-bit))]
           [(d17of32) (pair d02 (1-bit))]
           [(d18of32) (pair d03 (1-bit))]
           [(d19of32) (pair d04 (1-bit))]
           [(d20of32) (pair d05 (1-bit))]
           [(d21of32) (pair d06 (1-bit))]
           [(d22of32) (pair d07 (1-bit))]
           [(d23of32) (pair d08 (1-bit))]
           [(d24of32) (pair d09 (1-bit))]
           [(d25of32) (pair d10 (1-bit))]
           [(d26of32) (pair d11 (1-bit))]
           [(d27of32) (pair d12 (1-bit))]
           [(d28of32) (pair d13 (1-bit))]
           [(d29of32) (pair d14 (1-bit))]
           [(d30of32) (pair d15 (1-bit))]
           [(d31of32) (pair d16 (1-bit))])]
        [(d18of32)
         (match-adt Digit b
           [(d00of32) (pair d18 (0-bit))]
           [(d01of32) (pair d19 (0-bit))]
           [(d02of32) (pair d20 (0-bit))]
           [(d03of32) (pair d21 (0-bit))]
           [(d04of32) (pair d22 (0-bit))]
           [(d05of32) (pair d23 (0-bit))]
           [(d06of32) (pair d24 (0-bit))]
           [(d07of32) (pair d25 (0-bit))]
           [(d08of32) (pair d26 (0-bit))]
           [(d09of32) (pair d27 (0-bit))]
           [(d10of32) (pair d28 (0-bit))]
           [(d11of32) (pair d29 (0-bit))]
           [(d12of32) (pair d30 (0-bit))]
           [(d13of32) (pair d31 (0-bit))]
           [(d14of32) (pair d00 (1-bit))]
           [(d15of32) (pair d01 (1-bit))]
           [(d16of32) (pair d02 (1-bit))]
           [(d17of32) (pair d03 (1-bit))]
           [(d18of32) (pair d04 (1-bit))]
           [(d19of32) (pair d05 (1-bit))]
           [(d20of32) (pair d06 (1-bit))]
           [(d21of32) (pair d07 (1-bit))]
           [(d22of32) (pair d08 (1-bit))]
           [(d23of32) (pair d09 (1-bit))]
           [(d24of32) (pair d10 (1-bit))]
           [(d25of32) (pair d11 (1-bit))]
           [(d26of32) (pair d12 (1-bit))]
           [(d27of32) (pair d13 (1-bit))]
           [(d28of32) (pair d14 (1-bit))]
           [(d29of32) (pair d15 (1-bit))]
           [(d30of32) (pair d16 (1-bit))]
           [(d31of32) (pair d17 (1-bit))])]
        [(d19of32)
         (match-adt Digit b
           [(d00of32) (pair d19 (0-bit))]
           [(d01of32) (pair d20 (0-bit))]
           [(d02of32) (pair d21 (0-bit))]
           [(d03of32) (pair d22 (0-bit))]
           [(d04of32) (pair d23 (0-bit))]
           [(d05of32) (pair d24 (0-bit))]
           [(d06of32) (pair d25 (0-bit))]
           [(d07of32) (pair d26 (0-bit))]
           [(d08of32) (pair d27 (0-bit))]
           [(d09of32) (pair d28 (0-bit))]
           [(d10of32) (pair d29 (0-bit))]
           [(d11of32) (pair d30 (0-bit))]
           [(d12of32) (pair d31 (0-bit))]
           [(d13of32) (pair d00 (1-bit))]
           [(d14of32) (pair d01 (1-bit))]
           [(d15of32) (pair d02 (1-bit))]
           [(d16of32) (pair d03 (1-bit))]
           [(d17of32) (pair d04 (1-bit))]
           [(d18of32) (pair d05 (1-bit))]
           [(d19of32) (pair d06 (1-bit))]
           [(d20of32) (pair d07 (1-bit))]
           [(d21of32) (pair d08 (1-bit))]
           [(d22of32) (pair d09 (1-bit))]
           [(d23of32) (pair d10 (1-bit))]
           [(d24of32) (pair d11 (1-bit))]
           [(d25of32) (pair d12 (1-bit))]
           [(d26of32) (pair d13 (1-bit))]
           [(d27of32) (pair d14 (1-bit))]
           [(d28of32) (pair d15 (1-bit))]
           [(d29of32) (pair d16 (1-bit))]
           [(d30of32) (pair d17 (1-bit))]
           [(d31of32) (pair d18 (1-bit))])]
        [(d20of32)
         (match-adt Digit b
           [(d00of32) (pair d20 (0-bit))]
           [(d01of32) (pair d21 (0-bit))]
           [(d02of32) (pair d22 (0-bit))]
           [(d03of32) (pair d23 (0-bit))]
           [(d04of32) (pair d24 (0-bit))]
           [(d05of32) (pair d25 (0-bit))]
           [(d06of32) (pair d26 (0-bit))]
           [(d07of32) (pair d27 (0-bit))]
           [(d08of32) (pair d28 (0-bit))]
           [(d09of32) (pair d29 (0-bit))]
           [(d10of32) (pair d30 (0-bit))]
           [(d11of32) (pair d31 (0-bit))]
           [(d12of32) (pair d00 (1-bit))]
           [(d13of32) (pair d01 (1-bit))]
           [(d14of32) (pair d02 (1-bit))]
           [(d15of32) (pair d03 (1-bit))]
           [(d16of32) (pair d04 (1-bit))]
           [(d17of32) (pair d05 (1-bit))]
           [(d18of32) (pair d06 (1-bit))]
           [(d19of32) (pair d07 (1-bit))]
           [(d20of32) (pair d08 (1-bit))]
           [(d21of32) (pair d09 (1-bit))]
           [(d22of32) (pair d10 (1-bit))]
           [(d23of32) (pair d11 (1-bit))]
           [(d24of32) (pair d12 (1-bit))]
           [(d25of32) (pair d13 (1-bit))]
           [(d26of32) (pair d14 (1-bit))]
           [(d27of32) (pair d15 (1-bit))]
           [(d28of32) (pair d16 (1-bit))]
           [(d29of32) (pair d17 (1-bit))]
           [(d30of32) (pair d18 (1-bit))]
           [(d31of32) (pair d19 (1-bit))])]
        [(d21of32)
         (match-adt Digit b
           [(d00of32) (pair d21 (0-bit))]
           [(d01of32) (pair d22 (0-bit))]
           [(d02of32) (pair d23 (0-bit))]
           [(d03of32) (pair d24 (0-bit))]
           [(d04of32) (pair d25 (0-bit))]
           [(d05of32) (pair d26 (0-bit))]
           [(d06of32) (pair d27 (0-bit))]
           [(d07of32) (pair d28 (0-bit))]
           [(d08of32) (pair d29 (0-bit))]
           [(d09of32) (pair d30 (0-bit))]
           [(d10of32) (pair d31 (0-bit))]
           [(d11of32) (pair d00 (1-bit))]
           [(d12of32) (pair d01 (1-bit))]
           [(d13of32) (pair d02 (1-bit))]
           [(d14of32) (pair d03 (1-bit))]
           [(d15of32) (pair d04 (1-bit))]
           [(d16of32) (pair d05 (1-bit))]
           [(d17of32) (pair d06 (1-bit))]
           [(d18of32) (pair d07 (1-bit))]
           [(d19of32) (pair d08 (1-bit))]
           [(d20of32) (pair d09 (1-bit))]
           [(d21of32) (pair d10 (1-bit))]
           [(d22of32) (pair d11 (1-bit))]
           [(d23of32) (pair d12 (1-bit))]
           [(d24of32) (pair d13 (1-bit))]
           [(d25of32) (pair d14 (1-bit))]
           [(d26of32) (pair d15 (1-bit))]
           [(d27of32) (pair d16 (1-bit))]
           [(d28of32) (pair d17 (1-bit))]
           [(d29of32) (pair d18 (1-bit))]
           [(d30of32) (pair d19 (1-bit))]
           [(d31of32) (pair d20 (1-bit))])]
        [(d22of32)
         (match-adt Digit b
           [(d00of32) (pair d22 (0-bit))]
           [(d01of32) (pair d23 (0-bit))]
           [(d02of32) (pair d24 (0-bit))]
           [(d03of32) (pair d25 (0-bit))]
           [(d04of32) (pair d26 (0-bit))]
           [(d05of32) (pair d27 (0-bit))]
           [(d06of32) (pair d28 (0-bit))]
           [(d07of32) (pair d29 (0-bit))]
           [(d08of32) (pair d30 (0-bit))]
           [(d09of32) (pair d31 (0-bit))]
           [(d10of32) (pair d00 (1-bit))]
           [(d11of32) (pair d01 (1-bit))]
           [(d12of32) (pair d02 (1-bit))]
           [(d13of32) (pair d03 (1-bit))]
           [(d14of32) (pair d04 (1-bit))]
           [(d15of32) (pair d05 (1-bit))]
           [(d16of32) (pair d06 (1-bit))]
           [(d17of32) (pair d07 (1-bit))]
           [(d18of32) (pair d08 (1-bit))]
           [(d19of32) (pair d09 (1-bit))]
           [(d20of32) (pair d10 (1-bit))]
           [(d21of32) (pair d11 (1-bit))]
           [(d22of32) (pair d12 (1-bit))]
           [(d23of32) (pair d13 (1-bit))]
           [(d24of32) (pair d14 (1-bit))]
           [(d25of32) (pair d15 (1-bit))]
           [(d26of32) (pair d16 (1-bit))]
           [(d27of32) (pair d17 (1-bit))]
           [(d28of32) (pair d18 (1-bit))]
           [(d29of32) (pair d19 (1-bit))]
           [(d30of32) (pair d20 (1-bit))]
           [(d31of32) (pair d21 (1-bit))])]
        [(d23of32)
         (match-adt Digit b
           [(d00of32) (pair d23 (0-bit))]
           [(d01of32) (pair d24 (0-bit))]
           [(d02of32) (pair d25 (0-bit))]
           [(d03of32) (pair d26 (0-bit))]
           [(d04of32) (pair d27 (0-bit))]
           [(d05of32) (pair d28 (0-bit))]
           [(d06of32) (pair d29 (0-bit))]
           [(d07of32) (pair d30 (0-bit))]
           [(d08of32) (pair d31 (0-bit))]
           [(d09of32) (pair d00 (1-bit))]
           [(d10of32) (pair d01 (1-bit))]
           [(d11of32) (pair d02 (1-bit))]
           [(d12of32) (pair d03 (1-bit))]
           [(d13of32) (pair d04 (1-bit))]
           [(d14of32) (pair d05 (1-bit))]
           [(d15of32) (pair d06 (1-bit))]
           [(d16of32) (pair d07 (1-bit))]
           [(d17of32) (pair d08 (1-bit))]
           [(d18of32) (pair d09 (1-bit))]
           [(d19of32) (pair d10 (1-bit))]
           [(d20of32) (pair d11 (1-bit))]
           [(d21of32) (pair d12 (1-bit))]
           [(d22of32) (pair d13 (1-bit))]
           [(d23of32) (pair d14 (1-bit))]
           [(d24of32) (pair d15 (1-bit))]
           [(d25of32) (pair d16 (1-bit))]
           [(d26of32) (pair d17 (1-bit))]
           [(d27of32) (pair d18 (1-bit))]
           [(d28of32) (pair d19 (1-bit))]
           [(d29of32) (pair d20 (1-bit))]
           [(d30of32) (pair d21 (1-bit))]
           [(d31of32) (pair d22 (1-bit))])]
        [(d24of32)
         (match-adt Digit b
           [(d00of32) (pair d24 (0-bit))]
           [(d01of32) (pair d25 (0-bit))]
           [(d02of32) (pair d26 (0-bit))]
           [(d03of32) (pair d27 (0-bit))]
           [(d04of32) (pair d28 (0-bit))]
           [(d05of32) (pair d29 (0-bit))]
           [(d06of32) (pair d30 (0-bit))]
           [(d07of32) (pair d31 (0-bit))]
           [(d08of32) (pair d00 (1-bit))]
           [(d09of32) (pair d01 (1-bit))]
           [(d10of32) (pair d02 (1-bit))]
           [(d11of32) (pair d03 (1-bit))]
           [(d12of32) (pair d04 (1-bit))]
           [(d13of32) (pair d05 (1-bit))]
           [(d14of32) (pair d06 (1-bit))]
           [(d15of32) (pair d07 (1-bit))]
           [(d16of32) (pair d08 (1-bit))]
           [(d17of32) (pair d09 (1-bit))]
           [(d18of32) (pair d10 (1-bit))]
           [(d19of32) (pair d11 (1-bit))]
           [(d20of32) (pair d12 (1-bit))]
           [(d21of32) (pair d13 (1-bit))]
           [(d22of32) (pair d14 (1-bit))]
           [(d23of32) (pair d15 (1-bit))]
           [(d24of32) (pair d16 (1-bit))]
           [(d25of32) (pair d17 (1-bit))]
           [(d26of32) (pair d18 (1-bit))]
           [(d27of32) (pair d19 (1-bit))]
           [(d28of32) (pair d20 (1-bit))]
           [(d29of32) (pair d21 (1-bit))]
           [(d30of32) (pair d22 (1-bit))]
           [(d31of32) (pair d23 (1-bit))])]
        [(d25of32)
         (match-adt Digit b
           [(d00of32) (pair d25 (0-bit))]
           [(d01of32) (pair d26 (0-bit))]
           [(d02of32) (pair d27 (0-bit))]
           [(d03of32) (pair d28 (0-bit))]
           [(d04of32) (pair d29 (0-bit))]
           [(d05of32) (pair d30 (0-bit))]
           [(d06of32) (pair d31 (0-bit))]
           [(d07of32) (pair d00 (1-bit))]
           [(d08of32) (pair d01 (1-bit))]
           [(d09of32) (pair d02 (1-bit))]
           [(d10of32) (pair d03 (1-bit))]
           [(d11of32) (pair d04 (1-bit))]
           [(d12of32) (pair d05 (1-bit))]
           [(d13of32) (pair d06 (1-bit))]
           [(d14of32) (pair d07 (1-bit))]
           [(d15of32) (pair d08 (1-bit))]
           [(d16of32) (pair d09 (1-bit))]
           [(d17of32) (pair d10 (1-bit))]
           [(d18of32) (pair d11 (1-bit))]
           [(d19of32) (pair d12 (1-bit))]
           [(d20of32) (pair d13 (1-bit))]
           [(d21of32) (pair d14 (1-bit))]
           [(d22of32) (pair d15 (1-bit))]
           [(d23of32) (pair d16 (1-bit))]
           [(d24of32) (pair d17 (1-bit))]
           [(d25of32) (pair d18 (1-bit))]
           [(d26of32) (pair d19 (1-bit))]
           [(d27of32) (pair d20 (1-bit))]
           [(d28of32) (pair d21 (1-bit))]
           [(d29of32) (pair d22 (1-bit))]
           [(d30of32) (pair d23 (1-bit))]
           [(d31of32) (pair d24 (1-bit))])]
        [(d26of32)
         (match-adt Digit b
           [(d00of32) (pair d26 (0-bit))]
           [(d01of32) (pair d27 (0-bit))]
           [(d02of32) (pair d28 (0-bit))]
           [(d03of32) (pair d29 (0-bit))]
           [(d04of32) (pair d30 (0-bit))]
           [(d05of32) (pair d31 (0-bit))]
           [(d06of32) (pair d00 (1-bit))]
           [(d07of32) (pair d01 (1-bit))]
           [(d08of32) (pair d02 (1-bit))]
           [(d09of32) (pair d03 (1-bit))]
           [(d10of32) (pair d04 (1-bit))]
           [(d11of32) (pair d05 (1-bit))]
           [(d12of32) (pair d06 (1-bit))]
           [(d13of32) (pair d07 (1-bit))]
           [(d14of32) (pair d08 (1-bit))]
           [(d15of32) (pair d09 (1-bit))]
           [(d16of32) (pair d10 (1-bit))]
           [(d17of32) (pair d11 (1-bit))]
           [(d18of32) (pair d12 (1-bit))]
           [(d19of32) (pair d13 (1-bit))]
           [(d20of32) (pair d14 (1-bit))]
           [(d21of32) (pair d15 (1-bit))]
           [(d22of32) (pair d16 (1-bit))]
           [(d23of32) (pair d17 (1-bit))]
           [(d24of32) (pair d18 (1-bit))]
           [(d25of32) (pair d19 (1-bit))]
           [(d26of32) (pair d20 (1-bit))]
           [(d27of32) (pair d21 (1-bit))]
           [(d28of32) (pair d22 (1-bit))]
           [(d29of32) (pair d23 (1-bit))]
           [(d30of32) (pair d24 (1-bit))]
           [(d31of32) (pair d25 (1-bit))])]
        [(d27of32)
         (match-adt Digit b
           [(d00of32) (pair d27 (0-bit))]
           [(d01of32) (pair d28 (0-bit))]
           [(d02of32) (pair d29 (0-bit))]
           [(d03of32) (pair d30 (0-bit))]
           [(d04of32) (pair d31 (0-bit))]
           [(d05of32) (pair d00 (1-bit))]
           [(d06of32) (pair d01 (1-bit))]
           [(d07of32) (pair d02 (1-bit))]
           [(d08of32) (pair d03 (1-bit))]
           [(d09of32) (pair d04 (1-bit))]
           [(d10of32) (pair d05 (1-bit))]
           [(d11of32) (pair d06 (1-bit))]
           [(d12of32) (pair d07 (1-bit))]
           [(d13of32) (pair d08 (1-bit))]
           [(d14of32) (pair d09 (1-bit))]
           [(d15of32) (pair d10 (1-bit))]
           [(d16of32) (pair d11 (1-bit))]
           [(d17of32) (pair d12 (1-bit))]
           [(d18of32) (pair d13 (1-bit))]
           [(d19of32) (pair d14 (1-bit))]
           [(d20of32) (pair d15 (1-bit))]
           [(d21of32) (pair d16 (1-bit))]
           [(d22of32) (pair d17 (1-bit))]
           [(d23of32) (pair d18 (1-bit))]
           [(d24of32) (pair d19 (1-bit))]
           [(d25of32) (pair d20 (1-bit))]
           [(d26of32) (pair d21 (1-bit))]
           [(d27of32) (pair d22 (1-bit))]
           [(d28of32) (pair d23 (1-bit))]
           [(d29of32) (pair d24 (1-bit))]
           [(d30of32) (pair d25 (1-bit))]
           [(d31of32) (pair d26 (1-bit))])]
        [(d28of32)
         (match-adt Digit b
           [(d00of32) (pair d28 (0-bit))]
           [(d01of32) (pair d29 (0-bit))]
           [(d02of32) (pair d30 (0-bit))]
           [(d03of32) (pair d31 (0-bit))]
           [(d04of32) (pair d00 (1-bit))]
           [(d05of32) (pair d01 (1-bit))]
           [(d06of32) (pair d02 (1-bit))]
           [(d07of32) (pair d03 (1-bit))]
           [(d08of32) (pair d04 (1-bit))]
           [(d09of32) (pair d05 (1-bit))]
           [(d10of32) (pair d06 (1-bit))]
           [(d11of32) (pair d07 (1-bit))]
           [(d12of32) (pair d08 (1-bit))]
           [(d13of32) (pair d09 (1-bit))]
           [(d14of32) (pair d10 (1-bit))]
           [(d15of32) (pair d11 (1-bit))]
           [(d16of32) (pair d12 (1-bit))]
           [(d17of32) (pair d13 (1-bit))]
           [(d18of32) (pair d14 (1-bit))]
           [(d19of32) (pair d15 (1-bit))]
           [(d20of32) (pair d16 (1-bit))]
           [(d21of32) (pair d17 (1-bit))]
           [(d22of32) (pair d18 (1-bit))]
           [(d23of32) (pair d19 (1-bit))]
           [(d24of32) (pair d20 (1-bit))]
           [(d25of32) (pair d21 (1-bit))]
           [(d26of32) (pair d22 (1-bit))]
           [(d27of32) (pair d23 (1-bit))]
           [(d28of32) (pair d24 (1-bit))]
           [(d29of32) (pair d25 (1-bit))]
           [(d30of32) (pair d26 (1-bit))]
           [(d31of32) (pair d27 (1-bit))])]
        [(d29of32)
         (match-adt Digit b
           [(d00of32) (pair d29 (0-bit))]
           [(d01of32) (pair d30 (0-bit))]
           [(d02of32) (pair d31 (0-bit))]
           [(d03of32) (pair d00 (1-bit))]
           [(d04of32) (pair d01 (1-bit))]
           [(d05of32) (pair d02 (1-bit))]
           [(d06of32) (pair d03 (1-bit))]
           [(d07of32) (pair d04 (1-bit))]
           [(d08of32) (pair d05 (1-bit))]
           [(d09of32) (pair d06 (1-bit))]
           [(d10of32) (pair d07 (1-bit))]
           [(d11of32) (pair d08 (1-bit))]
           [(d12of32) (pair d09 (1-bit))]
           [(d13of32) (pair d10 (1-bit))]
           [(d14of32) (pair d11 (1-bit))]
           [(d15of32) (pair d12 (1-bit))]
           [(d16of32) (pair d13 (1-bit))]
           [(d17of32) (pair d14 (1-bit))]
           [(d18of32) (pair d15 (1-bit))]
           [(d19of32) (pair d16 (1-bit))]
           [(d20of32) (pair d17 (1-bit))]
           [(d21of32) (pair d18 (1-bit))]
           [(d22of32) (pair d19 (1-bit))]
           [(d23of32) (pair d20 (1-bit))]
           [(d24of32) (pair d21 (1-bit))]
           [(d25of32) (pair d22 (1-bit))]
           [(d26of32) (pair d23 (1-bit))]
           [(d27of32) (pair d24 (1-bit))]
           [(d28of32) (pair d25 (1-bit))]
           [(d29of32) (pair d26 (1-bit))]
           [(d30of32) (pair d27 (1-bit))]
           [(d31of32) (pair d28 (1-bit))])]
        [(d30of32)
         (match-adt Digit b
           [(d00of32) (pair d30 (0-bit))]
           [(d01of32) (pair d31 (0-bit))]
           [(d02of32) (pair d00 (1-bit))]
           [(d03of32) (pair d01 (1-bit))]
           [(d04of32) (pair d02 (1-bit))]
           [(d05of32) (pair d03 (1-bit))]
           [(d06of32) (pair d04 (1-bit))]
           [(d07of32) (pair d05 (1-bit))]
           [(d08of32) (pair d06 (1-bit))]
           [(d09of32) (pair d07 (1-bit))]
           [(d10of32) (pair d08 (1-bit))]
           [(d11of32) (pair d09 (1-bit))]
           [(d12of32) (pair d10 (1-bit))]
           [(d13of32) (pair d11 (1-bit))]
           [(d14of32) (pair d12 (1-bit))]
           [(d15of32) (pair d13 (1-bit))]
           [(d16of32) (pair d14 (1-bit))]
           [(d17of32) (pair d15 (1-bit))]
           [(d18of32) (pair d16 (1-bit))]
           [(d19of32) (pair d17 (1-bit))]
           [(d20of32) (pair d18 (1-bit))]
           [(d21of32) (pair d19 (1-bit))]
           [(d22of32) (pair d20 (1-bit))]
           [(d23of32) (pair d21 (1-bit))]
           [(d24of32) (pair d22 (1-bit))]
           [(d25of32) (pair d23 (1-bit))]
           [(d26of32) (pair d24 (1-bit))]
           [(d27of32) (pair d25 (1-bit))]
           [(d28of32) (pair d26 (1-bit))]
           [(d29of32) (pair d27 (1-bit))]
           [(d30of32) (pair d28 (1-bit))]
           [(d31of32) (pair d29 (1-bit))])]
        [(d31of32)
         (match-adt Digit b
           [(d00of32) (pair d31 (0-bit))]
           [(d01of32) (pair d00 (1-bit))]
           [(d02of32) (pair d01 (1-bit))]
           [(d03of32) (pair d02 (1-bit))]
           [(d04of32) (pair d03 (1-bit))]
           [(d05of32) (pair d04 (1-bit))]
           [(d06of32) (pair d05 (1-bit))]
           [(d07of32) (pair d06 (1-bit))]
           [(d08of32) (pair d07 (1-bit))]
           [(d09of32) (pair d08 (1-bit))]
           [(d10of32) (pair d09 (1-bit))]
           [(d11of32) (pair d10 (1-bit))]
           [(d12of32) (pair d11 (1-bit))]
           [(d13of32) (pair d12 (1-bit))]
           [(d14of32) (pair d13 (1-bit))]
           [(d15of32) (pair d14 (1-bit))]
           [(d16of32) (pair d15 (1-bit))]
           [(d17of32) (pair d16 (1-bit))]
           [(d18of32) (pair d17 (1-bit))]
           [(d19of32) (pair d18 (1-bit))]
           [(d20of32) (pair d19 (1-bit))]
           [(d21of32) (pair d20 (1-bit))]
           [(d22of32) (pair d21 (1-bit))]
           [(d23of32) (pair d22 (1-bit))]
           [(d24of32) (pair d23 (1-bit))]
           [(d25of32) (pair d24 (1-bit))]
           [(d26of32) (pair d25 (1-bit))]
           [(d27of32) (pair d26 (1-bit))]
           [(d28of32) (pair d27 (1-bit))]
           [(d29of32) (pair d28 (1-bit))]
           [(d30of32) (pair d29 (1-bit))]
           [(d31of32) (pair d30 (1-bit))])]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define n0 (zero))

(define n1 (nat-cons d01 n0))
(define n2 (nat-cons d02 n0))
(define n3 (nat-cons d03 n0))
(define n4 (nat-cons d04 n0))
(define n5 (nat-cons d05 n0))
(define n6 (nat-cons d06 n0))
(define n7 (nat-cons d07 n0))
(define n8 (nat-cons d08 n0))
(define n9 (nat-cons d09 n0))
(define n10 (nat-cons d10 n0))
(define n11 (nat-cons d11 n0))
(define n12 (nat-cons d12 n0))
(define n13 (nat-cons d13 n0))
(define n14 (nat-cons d14 n0))
(define n15 (nat-cons d15 n0))
(define n16 (nat-cons d16 n0))
(define n17 (nat-cons d17 n0))
(define n18 (nat-cons d18 n0))
(define n19 (nat-cons d19 n0))
(define n20 (nat-cons d20 n0))
(define n21 (nat-cons d21 n0))
(define n22 (nat-cons d22 n0))
(define n23 (nat-cons d23 n0))
(define n24 (nat-cons d24 n0))
(define n25 (nat-cons d25 n0))
(define n26 (nat-cons d26 n0))
(define n27 (nat-cons d27 n0))
(define n28 (nat-cons d28 n0))
(define n29 (nat-cons d29 n0))
(define n30 (nat-cons d30 n0))
(define n31 (nat-cons d31 n0))

(define n32 (nat-cons d00 n1))
(define n33 (nat-cons d01 n1))
(define n34 (nat-cons d02 n1))
(define n35 (nat-cons d03 n1))
(define n36 (nat-cons d04 n1))
(define n37 (nat-cons d05 n1))
(define n38 (nat-cons d06 n1))
(define n39 (nat-cons d07 n1))
(define n40 (nat-cons d08 n1))
(define n41 (nat-cons d09 n1))
(define n42 (nat-cons d10 n1))
(define n43 (nat-cons d11 n1))
(define n44 (nat-cons d12 n1))
(define n45 (nat-cons d13 n1))
(define n46 (nat-cons d14 n1))
(define n47 (nat-cons d15 n1))
(define n48 (nat-cons d16 n1))
(define n49 (nat-cons d17 n1))
(define n50 (nat-cons d18 n1))
(define n51 (nat-cons d19 n1))
(define n52 (nat-cons d20 n1))
(define n53 (nat-cons d21 n1))
(define n54 (nat-cons d22 n1))
(define n55 (nat-cons d23 n1))
(define n56 (nat-cons d24 n1))
(define n57 (nat-cons d25 n1))
(define n58 (nat-cons d26 n1))
(define n59 (nat-cons d27 n1))
(define n60 (nat-cons d28 n1))
(define n61 (nat-cons d29 n1))
(define n62 (nat-cons d30 n1))
(define n63 (nat-cons d31 n1))

(define n64 (nat-cons d00 n2))

