#lang htdp/asl
;;;   ^^^^^^^^ LOOOOOOOK! We're so cool now.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;  PART 2  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;; Activity 2.1 - Make a global variable `balance` with some money.
; Note: the check-expects assume that you've defined one with a starting
; value of **500**



;;;;;;; Activity 2.2
; deposit! : number -> number
; Deposits money into our bank account
; Effect: balance increases by deposit amount
(define deposit!
  "fill me in")

(check-expect (deposit! 250) 750)
(check-expect (deposit! 100) 850)

;;;;;;; Activity 2.3
; withdraw! : number -> number
; Withdraws money from our account
; Effect: balance decreases by deposit amount unless
;         balance is less than withdrawal amount
(define withdraw!
  "fill me in")

(check-expect (withdraw! 350) 500)
(check-expect (withdraw! 750) 500) ; Note there was a bug here!

;;;;;;; Activity 2.4
; withdraw-with-fee! : number -> number
; Withdraws money from our bank account!
; Effect: balance decreases by withdraw amount unless
;         balance is less than withdraw number also prints out
;         starting and ending balance. It also charges a fee of $5
;         if a withdrawal results in an overdraft.
(define withdraw-with-fee!
  "fill me in")

(check-expect (withdraw-with-fee! 495) 5) ; note there was a bug here!
(check-expect (withdraw-with-fee! 10) 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;  PART 3  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Activity 3.1

; list-max : (listof number) -> number
; Returns the largest number in a list
; Assume the passed in list is never empty
(define list-max
  "fill me in")

(check-expect (list-max (list -1000 -213 -1  -123 0)) 0)
(check-expect (list-max (list -1000 -213 -1  -123 -2)) -1)
(check-expect (list-max (list -1000 23 -2 231 2312 312 1)) 2312)

;;; Activity 3.2

; list-max/iter : (listof number) -> number
; Returns the largest number in a list
; Assume the passed in list is never empty
(define list-max/iter
  "fill me in")

(check-expect (list-max/iter (list -1000 -213 -1  -123 0)) 0)
(check-expect (list-max/iter (list -1000 -213 -1  -123 -2)) -1)
(check-expect (list-max/iter (list -1000 23 -2 231 2312 312 1)) 2312)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;  PART 4  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 2htdp/image)
(require 2htdp/universe)

; A character is a...
;   - string
;       Where that string is either a single letter from your keyboard
;       or a special string that represents special keys like:
;         - "\b" (the backspace key)
;         - "\r" (the return / enter key)

; key-pressed: character -> (void)
; Add the new character that was typed to the screen.
; Side-Effect: updates the-text variable 
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
           ;(close-on-stop #true) ; uncomment this line if you want the editor to close on return
           (on-key (λ (ignore key)
                     (begin (key-pressed key)
                            "")))
           (on-draw (λ (state)
                      (overlay (text the-text 24 "green")
                               (rectangle 300 50 "solid" "black")))))
         the-text))

; WHEN YOU ARE READY...call (edit-text) in the REPL (Interactions Window)
; to open the text editor on demand.