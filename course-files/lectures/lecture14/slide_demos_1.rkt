#lang htdp/asl ; Notice, we're in ASL now!
; this is a superset of ISL+

(check-expect (sum-list/func '()) 0)
(check-expect (sum-list/func '(1 2 3 4)) 10)
(check-expect (sum-list/func '(0 0 0 0)) 0)
; sum-list/func : (listof number) -> number
; uses recursion to calculate sum of a list of numbers
(define (sum-list/func a-list)
  (if (empty? a-list)
      0
      (+ (first a-list)
         (sum-list/func
          (rest a-list)))))

(check-expect (sum-list '()) 0)
(check-expect (sum-list '(1 2 3 4)) 10)
(check-expect (sum-list '(0 0 0 0)) 0)
; sum-list : (listof number) -> number
; uses imperatives to calculate sum of a list of numbers
(define (sum-list a-list)
  (local [(define sum 0)
          (define rem a-list)
          (define (loop)
            (if (empty? rem)
                sum
                (begin (set! sum
                             (+ sum 
                                (first rem)))
                       (set! rem
                             (rest rem))
                       (loop))))]
    (loop)))

;; Broken sum-list below to demonstrate imperative dangers
;; (define (sum-list a-list)
;;   (local [(define sum 0)
;;           (define remaining a-list)
;;           (define (loop)
;;             (if (empty? remaining)
;;                 sum
;;                 (begin (set! remaining
;;                              (rest remaining))
;;                        (set! sum
;;                              (+ sum (first remaining)))
;;                        (loop))))]
;;     (loop)))
;; 
;; (sum-list (list 1 2 3 4))

;; Higher order imperatives
;; (define (sum-list a-list)
;;   (local [(define sum 0)]
;;     (begin (for-each (λ (element)
;;                        (set! sum (+ sum element)))
;;                      a-list)
;;            sum)))
