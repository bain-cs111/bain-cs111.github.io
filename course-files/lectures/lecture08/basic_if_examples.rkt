#lang htdp/isl+

; Booleans!
#true
#t
true
(= 5 5)

(define x 6)
(> x 7)
(< x 3)

; Basic If Example
;(define u 5)
;(if (= u 5)
;    "u equals 5"
;    "u doesn't equal 5")

; Proof that if is a special form
;(if (= 1 1) 6 (/ 6 0))

;;; Designing Absolute Value

;; TYPE SIGNATURE: ???
;; PURPOSE:???
; (define absolute "FILL ME IN")
;; Make sure to design some check-expects BEFORE
;; writing the function!!


;;; Compound Predicates
(define f 2)
(define g 3)

;; OR Demo - "Are any of these things true?"
;(or (= f 2)
;    (= g 3)
;    (= g 5))

;; AND Demo - "Are *all* of these things true?"
;(and (= f 2)
;     (= g 5)
;     (= g 3))

;; NOT Demo - "What's the opposite of this predicate?"
;(not (= f 2))
;(not (= g 4))


; tricky local example
;(define a #true)
;(local [(define a #false)]
;  (not a))



;;;;; EXTRA STUFF
; Using COND
; Note, we won't get to this in class today. We'll talk about it next week

;(if C1 R1 R2)
;
;(if (> x 5) "big" "small")
;
;
;(cond [C1 R1]
;      [C2 R2]
;      [Cn Rn])
;
;(cond [(> x 5) "big"]
;      [(= x 5) "medium"]
;      [else    "small"])
;; You don't HAVE to have an else...it's just that it's possible that
;; if you don't have one, you won't cover all possible cases (values of x)
