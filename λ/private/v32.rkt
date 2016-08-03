#lang gnal λ/adt

(provide V32 v32 make-v32 make-v32/1 make-v32/2
         v32-nth v32-set-nth v32-update-nth v32=?)

(require "boolean.rkt" "natural.rkt")

;; A (V32of A) is a
;; (v32 A A A A A A A A A A A A A A A A
;;      A A A A A A A A A A A A A A A A)
(define-adt V32
  (v32 v00 v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15
       v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31))

;; make-v32 : A -> (V32of A)
(define make-v32
  (λ (a)
    (v32 a a a a a a a a a a a a a a a a
         a a a a a a a a a a a a a a a a)))

;; make-v32/1 : A A -> (V32of A)
(define make-v32/1
  (λ (a b)
    (v32 a b b b b b b b b b b b b b b b
         b b b b b b b b b b b b b b b b)))

;; make-v32/2 : A A A -> (V32of A)
(define make-v32/2
  (λ (a b c)
    (v32 a b c c c c c c c c c c c c c c
         c c c c c c c c c c c c c c c c)))

;; v32-nth : (V32of A) Natural -> A
(define v32-nth
  (λ (v i)
    (match-adt V32 v
      [(v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
            s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)
       (match-adt Natural i [(zero) s00]
      [(succ i)
       (match-adt Natural i [(zero) s01]
      [(succ i)
       (match-adt Natural i [(zero) s02]
      [(succ i)
       (match-adt Natural i [(zero) s03]
      [(succ i)
       (match-adt Natural i [(zero) s04]
      [(succ i)
       (match-adt Natural i [(zero) s05]
      [(succ i)
       (match-adt Natural i [(zero) s06]
      [(succ i)
       (match-adt Natural i [(zero) s07]
      [(succ i)
       (match-adt Natural i [(zero) s08]
      [(succ i)
       (match-adt Natural i [(zero) s09]
      [(succ i)
       (match-adt Natural i [(zero) s10]
      [(succ i)
       (match-adt Natural i [(zero) s11]
      [(succ i)
       (match-adt Natural i [(zero) s12]
      [(succ i)
       (match-adt Natural i [(zero) s13]
      [(succ i)
       (match-adt Natural i [(zero) s14]
      [(succ i)
       (match-adt Natural i [(zero) s15]
      [(succ i)
       (match-adt Natural i [(zero) s16]
      [(succ i)
       (match-adt Natural i [(zero) s17]
      [(succ i)
       (match-adt Natural i [(zero) s18]
      [(succ i)
       (match-adt Natural i [(zero) s19]
      [(succ i)
       (match-adt Natural i [(zero) s20]
      [(succ i)
       (match-adt Natural i [(zero) s21]
      [(succ i)
       (match-adt Natural i [(zero) s22]
      [(succ i)
       (match-adt Natural i [(zero) s23]
      [(succ i)
       (match-adt Natural i [(zero) s24]
      [(succ i)
       (match-adt Natural i [(zero) s25]
      [(succ i)
       (match-adt Natural i [(zero) s26]
      [(succ i)
       (match-adt Natural i [(zero) s27]
      [(succ i)
       (match-adt Natural i [(zero) s28]
      [(succ i)
       (match-adt Natural i [(zero) s29]
      [(succ i)
       (match-adt Natural i [(zero) s30]
      [(succ i)
       (match-adt Natural i [(zero) s31]
      [(succ i) s00])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])))

;; v32-set-nth : (V32of A) Natural A -> (V32of A)
(define v32-set-nth
  (λ (v i a)
    (match-adt V32 v
      [(v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
            s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)
       (match-adt Natural i
         [(zero) (v32 a s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 a s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 a s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 a s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 a s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 a s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 a s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 a s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 a s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 a s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 a s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 a s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 a s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 a s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 a s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 a
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      a s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 a s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 a s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 a s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 a s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 a s22 s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 a s23 s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 a s24 s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 a s25 s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 a s26 s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 a s27 s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 a s28 s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 a s29 s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 a s30 s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 a s31)]
      [(succ i)
       (match-adt Natural i
         [(zero) (v32 s00 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15
                      s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 a)]
      [(succ i) v])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])))

;; v32-update-nth : (V32of A) Natural [A -> B] -> (V32of B)
(define v32-update-nth
  (λ (v i f)
    (v32-set-nth v i (f (v32-nth v i)))))

;; v32=? : [A A -> Boolean] -> [(V32of A) (V32of A) -> Boolean]
(define v32=?
  (λ (elem=?)
    (λ (a b)
      (match-adt V32 a
        [(v32 a00 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15
              a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31)
         (match-adt V32 b
           [(v32 b00 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15
                 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
            (match-adt Boolean (elem=? a00 b00) [(true)
            (match-adt Boolean (elem=? a01 b01) [(true)
            (match-adt Boolean (elem=? a02 b02) [(true)
            (match-adt Boolean (elem=? a03 b03) [(true)
            (match-adt Boolean (elem=? a04 b04) [(true)
            (match-adt Boolean (elem=? a05 b05) [(true)
            (match-adt Boolean (elem=? a06 b06) [(true)
            (match-adt Boolean (elem=? a07 b07) [(true)
            (match-adt Boolean (elem=? a08 b08) [(true)
            (match-adt Boolean (elem=? a09 b09) [(true)
            (match-adt Boolean (elem=? a10 b10) [(true)
            (match-adt Boolean (elem=? a11 b11) [(true)
            (match-adt Boolean (elem=? a12 b12) [(true)
            (match-adt Boolean (elem=? a13 b13) [(true)
            (match-adt Boolean (elem=? a14 b14) [(true)
            (match-adt Boolean (elem=? a15 b15) [(true)
            (match-adt Boolean (elem=? a16 b16) [(true)
            (match-adt Boolean (elem=? a17 b17) [(true)
            (match-adt Boolean (elem=? a18 b18) [(true)
            (match-adt Boolean (elem=? a19 b19) [(true)
            (match-adt Boolean (elem=? a20 b20) [(true)
            (match-adt Boolean (elem=? a21 b21) [(true)
            (match-adt Boolean (elem=? a22 b22) [(true)
            (match-adt Boolean (elem=? a23 b23) [(true)
            (match-adt Boolean (elem=? a24 b24) [(true)
            (match-adt Boolean (elem=? a25 b25) [(true)
            (match-adt Boolean (elem=? a26 b26) [(true)
            (match-adt Boolean (elem=? a27 b27) [(true)
            (match-adt Boolean (elem=? a28 b28) [(true)
            (match-adt Boolean (elem=? a29 b29) [(true)
            (match-adt Boolean (elem=? a30 b30) [(true) (elem=? a31 b31)]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])]
              [(false) (false)])])]))))

