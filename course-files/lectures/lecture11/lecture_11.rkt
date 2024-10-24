#lang htdp/isl+
; Recursion on Lists
; Code from Slides and Extra Practice
; Search for "challenge" if you want more practice.

; sum-list: (listof number) -> number
; returns the sum of the numbers in the list
(define sum-list
  (lambda (lon)
    (if (empty? lon)
        0
        (+ (first lon) (sum-list (rest lon))))))

(check-expect (sum-list (list 1 2 3)) 6)
(check-expect (sum-list (list))       0)

;;sum-list-alt: (listof number) -> number
;;returns the sum of the numbers in the list

;; Instead of using if statements you could use what's
;; called a cond (or conditional) statement. It allows you
;; specificy more than one condition and basically says:
;; "if this is true...do this, otherwise if this is true, do this...
;; all the way down to the last "else" which says "otherwise just do this"
(define sum-list-alt
  (lambda (lon)
    (cond [(empty? lon)   0]
          [else           (+ (first lon)
                             (sum-list-alt (rest lon)))])))

(check-expect (sum-list-alt (list 1 2 3)) 6)
(check-expect (sum-list-alt empty) 0)

;;my-length: (listof number) -> number
;;returns the number of items in the list
(define my-length
  (lambda (lon)
    (if (empty? lon)
        0
        (+ 1
           (my-length (rest lon))))))

(check-expect (my-length (list 1 2 3)) 3)
(check-expect (my-length empty) 0)

; myfoldr: (X X -> X) X (listof X) -> X
; behaves like the foldr function
(define my-foldr
  (lambda (func start lst)
    (if (empty? lst)
        start
        (func (first lst)
              (my-foldr func start (rest lst))))))

(check-expect (my-foldr + 0 (list 1 2 3)) 6)


;;;; THESE AREN'T IN THE SLIDES AND ARE HERE FOR EXTRA PRACTICE!

;; Challenge #1
; Write contatins-my-num? using ormap
; contains-my-num?: Number (listof Number) -> Boolean
; determines whether number is in the listof numbers


; Challenge #2
; Now write it using recursion!

;;contains-my-num-alt?: Number (listof Number) -> Boolean
;;determines whether number is in the listof numbers


;; SUBSTITUTION MODEL
; This is what your function will look like under the substitution model
;(contains-my-num? 1 (list 1 2))
;(or (= 1 1)
;    (contains-my-num? 1 (list 2)))
;(or true
;    (contains-my-num? 1 (list 2)))
;(or true
;    (or (= 1 2)
;        (contains-my-num? 1 (list))))
;(or true
;    (or false
;        false))
;(or true
;    false)
;true


;; Challenge #3 Now write your own ormap!

; my-ormap (X -> Boolean) (listof X) -> Boolean
; behaves like ormap



;; I'm going to start using `cond` for these ones down below. Remember,
;; cond and if work very similarly. In fact here's an if and a cond
;; that behave identically:

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

;; ;; Challenge #4 Write this function without recursion by using andmap

; all-my-num?: Number (listof Number) -> Boolean
; determines whether number is in the listof numbers


;; Challenge #5 Now write is using recursion

;;all-my-num-alt?: Number (listof Number) -> Boolean
;;determines whether the list of numbers are all my number

;; Challenge #6 Now write your own andmap!

; my-andmap: (X -> Boolean) (listof X) -> Boolean
; behaves like andmap
