;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tutorial_6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Activity 1.1 - Make a global variable `balance` with some money.
; Note: the check-expects assume that you've defined one with a starting
; value of **500**

;;;;;;; Activity 1.2
; deposit!: number -> number
; Deposits money into our bank account
; Effect: balance increases by deposit amount
(define deposit!
  "fill me in")

(check-expect (deposit! 250) 750)
(check-expect (deposit! 100) 850)

;;;;;;; Activity 1.3
; withdraw!: number -> number
; Withdraws money from our account
; Effect: balance decreases by deposit amount unless
;         balance is less than withdrawal amount
(define withdraw!
  "fill me in")

(check-expect (withdraw! 350) 500)
(check-error (withdraw! 750) "Not enough money!")


;;;;;;;; PART 2


;;; Activity 2.1

; list-max: (listof number) -> number
; Returns the largest number in a list
; Assume the passed in list is never empty
(define list-max
  "fill me in")

(check-expect (list-max (list -1000 -213 -1  -123 0)) 0)
(check-expect (list-max (list -1000 -213 -1  -123 -2)) -1)
(check-expect (list-max (list -1000 23 -2 231 2312 312 1)) 2312)

;;; Activity 2.2

; list-max/iter: (listof number) -> number
; Returns the largest number in a list
; Assume the passed in list is never empty
(define list-max/iter
  "fill me in")

(check-expect (list-max/iter (list -1000 -213 -1  -123 0)) 0)
(check-expect (list-max/iter (list -1000 -213 -1  -123 -2)) -1)
(check-expect (list-max/iter (list -1000 23 -2 231 2312 312 1)) 2312)


; Activity 3: GUI problem

(require 2htdp/image)
(require 2htdp/universe)

(define (key-pressed key)
  "fill me in") 

;;; Don't modify the code below.
(define the-text "")
(define quit? false)
(define (edit-text)
  ;; My apologies to the authors of big-bang for taking their nice functional
  ;; simulator framework and using it in a completely imperative manner.
  (begin (set! quit? false)
         (big-bang null
           (stop-when (λ (ignore) quit?))
           (on-key (λ (ignore key)
                     (begin (key-pressed key)
                            "")))
           (on-draw (λ (state)
                      (overlay (text the-text 24 "green")
                               (rectangle 300 50 "solid" "black")))))
         the-text))

; Uncomment to have the text editor open
; when you run your code. Can also call (edit-text)
; in the REPL to open the text editor on demand.
; (edit-text)