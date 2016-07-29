#lang gnal λ/adt

(provide id=?
         ;; private
         unique-id/internal-state)

;; A (Unique-Id-Msg Internal) is one of:
;;  - (.id=? (Unique-Id Internal))
;;  - (.get-internal-state)
(define-adt Unique-Id-Msg
  (.id=? other-id)
  (.get-internal-state))

;; A unique-id is a function that takes a Unique-Id-Msg argument,
;; either (.id=? other-id) or (.get-internal-state).
;; If it's a (.get-internal-state) msg, return the internal state.
;; Otherwise, call other-id with (.get-internal-state) to get its
;; internal state, and compare its internal state with yours.

;; (define-type (Unique-ID Internal)
;;   (case-> [(.id=? (Unique-Id Internal)) -> Boolean]
;;           [(.get-internal-state) -> Internal]))

;; id=? : (Unique-IDof Internal) (Unique-IDof Internal) -> Boolean
(define id=?
  (λ (a b)
    (a (.id=? b))))

;; unique-id/internal-state :
;; [Internal Internal -> Boolean] -> [Internal -> (Unique-IDof Internal)]
(define unique-id/internal-state
  (λ (internal-state=?)
    (λ (this-internal)
      (λ (msg)
        (match-adt Unique-Id-Msg msg
          [(.id=? other-id)
           (internal-state=? (other-id (.get-internal-state)) this-internal)]
          [(.get-internal-state)
           this-internal])))))

