#lang gnal λ/adt

(provide Srcloc srcloc
         srcloc-source
         srcloc-line
         srcloc-column
         srcloc-position
         srcloc-span
         ;; private
         srcloc->rkt)

;; source   : Byte-String
;; line     : Base-2-Natural
;; column   : Base-2-Natural
;; position : Base-2-Natural
;; span     : Base-2-Natural
(define-adt Srcloc
  (srcloc source line column position span))

(define srcloc-source
  (λ (s)
    (match-adt Srcloc s
      [(srcloc src ln col pos span) src])))

(define srcloc-line
  (λ (s)
    (match-adt Srcloc s
      [(srcloc src ln col pos span) ln])))

(define srcloc-column
  (λ (s)
    (match-adt Srcloc s
      [(srcloc src ln col pos span) col])))

(define srcloc-position
  (λ (s)
    (match-adt Srcloc s
      [(srcloc src ln col pos span) pos])))

(define srcloc-span
  (λ (s)
    (match-adt Srcloc s
      [(srcloc src ln col pos span) span])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define srcloc->rkt
  (λ (make-srcloc byte-string->rkt b2-natural->rkt)
    (λ (s)
      (match-adt Srcloc s
        [(srcloc src ln col pos span)
         (make-srcloc (byte-string->rkt src)
                      (b2-natural->rkt ln)
                      (b2-natural->rkt col)
                      (b2-natural->rkt pos)
                      (b2-natural->rkt span))]))))
