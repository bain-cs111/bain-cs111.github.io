#lang htdp/isl+

; GENERAL REMINDERS for Recursion on Lists!
; Simplest Case: empty list
; Simpler Case: a slightly shorter list (rest list)
; TRUST THE RECURSION

; REGULAR RECURSION
;my-length: (listof X) -> number
;returns the number of items in the list
(define my-length
  (lambda (lon)
    (if (empty? lon)
        0
        (+ 1
           (my-length (rest lon))))))

(check-expect (my-length (list "A" "B" "C")) 3)
(check-expect (my-length (list)) 0)

; ITERATIVE RECURSION - What's new?

;my-length-helper: (listof X) number -> number
;returns the number of items in the list
(define my-length-helper
  (lambda (lst length-so-far)
    (if (empty? lst)
        length-so-far
        (my-length-helper (rest lst)
                                 (+ length-so-far 1)))))

;my-length/iter (listof X) -> number
;returns the number of items in the list
(define my-length/iter
  (lambda (lox)
    (my-length-helper lox 0)))

(check-expect (my-length/iter (list "A" "B" "C")) 3)
(check-expect (my-length/iter (list)) 0)

;my-length-alt/iter (listof X) -> number
;returns the number of items in the list
(define mylength3
  (lambda (lox)
    (local [(define help
              (lambda (lst length-so-far)
                (if (empty? lst)
                    length-so-far
                    (help (rest lst)
                          (+ length-so-far 1)))))]
      (help lox 0))))

(check-expect (my-length-alt/iter (list "A" "B" "C")) 3)
(check-expect (my-length-alt/iter (list)) 0)

; from lecture 11 examples
;contains-my-num?: Number (listof Number) -> Boolean
;determines whether number is in listof numbers
(define contains-my-num?
  (lambda (n lon)
    (if (empty? lon)
        false
        (or (= n (first lon))
            (contains-my-num? n (rest lon))))))

(check-expect (contains-my-num? 5 (list 1 2 3)) false)
(check-expect (contains-my-num? 5 (list 1 5 3)) true)

;contains-my-num?/iter: Number (listof Number) -> Boolean
;determines whether number is in listof numbers
(define (contains-my-num?/iter n lon)
  (local [(define help
            (lambda (lst accum)
              (if (empty? lst)
                  accum
                  (help (rest lst)
                        (or (= n (first lst))
                            accum)))))]
    (help lon false)))

(check-expect (contains-my-num?/iter 5 (list 1 2 3 4 5)) true)
(check-expect (contains-my-num?/iter 7 (list 1 2 3 4 5)) false)

; GENERAL REMINDERS for Recursion on **Numbers**:
; Simplest Case: 0 
; Simpler Case: slightly smaller number (- n 1)
; Trust the Recursion

;fact: number -> number
;returns the value of the input number factorial
(define fact
  (lambda (n)
    (if (= n 0)
        1
        (* n
           (fact (- n 1))))))

(check-expect (fact 5) 120)
(check-expect (fact 1) 1)
(check-expect (fact 20) 2432902008176640000)

; Substitution model for regular recursion of fact
;(fact 3)
;(* 3 (fact 2))
;(* 3 (* 2 (fact 1)))
;(* 3 (* 2 (* 1 (fact 0))))
;(* 3 (* 2 (* 1 1)))

;fact/iter number -> number
;returns the value of the input number factorial
(define fact/iter
  (lambda (n)
    (local [(define help
              (lambda (m accum)
                (if (= m 0)
                    accum
                    (help (- m 1)
                          (* m accum)))))]
      (help n 1))))

(check-expect (fact/iter 5) 120)
(check-expect (fact/iter 1) 1)
(check-expect (fact/iter 20) 2432902008176640000)

; Substitution Model for fact/iter
; (fact/iter 3)
; (help 3 1)
; (help 2 3)
; (help 1 6)
; (help 0 6)
; 6


; Tree recursion - multiple recursive calls - Example

; fibonacci sequence
; 0 1 1 2 3 5 8 13 21 34 ....

;; Remember...cond allows us to test MULTIPLE things aka
;; have more than two conditionals!
;; In fact here's an if and a cond that behave identically:

;(if test
;    do-this-if-test-true
;    do-this-if-test-false)
;
;(cond [test do-this-if-test-true]
;      [else do-this-if-test-false])
;
;; the only real difference is the square brackets and the else. We have to use
;; that else because unlike if, cond requires a test for each of its actions.
;; That allows us to do complicated changes of conditionals:
;
;(cond [test do-this-if-test-true]
;      [test-2 otherwise-do-this-if-test-2-true]
;      [test-3 otherwise-do-this-if-test-3-true]
;      [else otherwise-just-do-this])

; fib: number -> number
; returns the corresponding item in the fibonacci sequence
(define fib
  (lambda (n)
    (cond [(= n 0)    0]
          [(= n 1)    1]
          [else       (+ (fib (- n 1)) (fib (- n 2)))])))

(check-expect (fib 6) 8)
(check-expect (fib 1) 1)
(check-expect (fib 8) 21)

























