#lang gnal λ/adt

(provide V32 v32 v32-nth v32-set-nth v32-update-nth)

(require "natural.rkt")

;; A (V32of A) is a
;; (v32 A A A A A A A A A A A A A A A A
;;      A A A A A A A A A A A A A A A A)
(define-adt V32
  (v32 v00 v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15
       v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31))

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

