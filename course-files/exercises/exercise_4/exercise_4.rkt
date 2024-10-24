#lang htdp/isl+
(require 2htdp/image)
(require "./no_list_procs.rkt")

; not needed to implement your functions
; but very helpful for testing
; (require "./iterated-images.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Part 0. Fixing Recursion Errors          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activity 1: Computing Sum of Squares
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sum-of-squares-up-to : number -> number
;;   Given a non-negative integer n, compute 1^2 + 2^2 + ... + n^2

; (check-expect (sum-of-squares-up-to 3) 14) ;; uncomment this after fixing the error

(define (sum-of-squares-up-to n)
  (if (= n 0)
      0
      (+ (* n n)
         (sum-of-squares-up-to n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activity 2: Multiply Everything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-expect (multiply-everything (list 3 4 5)) 60)

;; multiply-everything : (listof number) -> number
;;   Given a list of numbers, compute their product.
(define (multiply-everything lst-nums)
  (if (empty? lst-nums)
      0
      (* (first lst-nums) (multiply-everything (rest lst-nums)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activity 3: Finding Zeroes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fill in your test case for any-zeros? here
(define test-input-for-any-zeros?
  "fill me in")

;; any-zeros?: (listof number) -> boolean
;; Given a list of numbers, checks whether there is a zero in it.
(check-expect (any-zeros? empty)
              #false)
(check-expect (any-zeros? test-input-for-any-zeros?)
              "fill me in")

(define (any-zeros? lst-nums)
  (if (empty? lst-nums)
      #false
      (and (= (first lst-nums) 0)
           (any-zeros? (rest lst-nums)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activity 4: Prof. Bain's Stamp Collection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A stamp-collection-entry is a struct:
;   (make-stamp-collection-entry String Number Number)
(define-struct stamp-collection-entry (name value count))
;; - `name` is the name of the stamp. It must store a string.
;; - `value' is the monetary value of the stamp. It must store a positive number.
;; - `count' is how many of that stamp you have in your collection. It must store a non-negative integer.

(define prof-bains-collection
  (list (make-stamp-collection-entry "Inverted Jenny" 20000 1)
        (make-stamp-collection-entry "GWash Z-Grill Stamp" 1000 0)
        (make-stamp-collection-entry "Tyrian Plum" 500 3)
        (make-stamp-collection-entry "Forever Stamps" 1 10)))

; This should pass...but doesn't!
(check-expect (total-value prof-bains-collection)
              21510)

;; total-value : (listof stamp-collection-entry) -> number
;; calculates the total value of a stamp collection
(define (total-value lst-stamps)
  (if (empty? lst-stamps)
      0
      (+ (* (stamp-collection-entry-value (first lst-stamps))
            (second (first lst-stamps)))
         (total-value (rest lst-stamps)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Part 1. Ordinary Recursion               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FILL IN FUNCTIONS HERE
; MUST add signatures and purpose statements

; Activity 1.
(check-expect ... "recursion,is,so,awesome,")


; Activity 2.


; Activity 3.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Part 2. Iterative Recursion              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Activity 1.

(check-expect ...
              "the,quick,brown,fox,")

;; concat-words/iter: (listof string) -> string
;; Takes a list of strings and concatenates them using iterative recursion
(define (concat-words/iter lst-strs)
  (concat-words-helper lst-strs concat-words-initial-value))

(define concat-words-initial-value
  "fill me in")

(define (concat-words-helper lst-strs acc)
  ...)


; Activity 2.
; FILL IN FUNCTIONS HERE


; Activity 3.
; FILL IN FUNCTIONS HERE


; BE SURE TO WRITE YOUR OWN TESTS...
; The only one's will give you today are these that check your function's names!
(check-satisfied test-input-for-any-zeros? list?)
(check-satisfied concat-words procedure?)
(check-satisfied total-value procedure?)
(check-satisfied my-iterated-overlay procedure?)
(check-satisfied my-iterated-any procedure?)
(check-satisfied concat-words-initial-value string?)
(check-satisfied concat-words-helper procedure?)
(check-satisfied concat-words/iter procedure?)
(check-satisfied my-iterated-overlay/iter procedure?)
(check-satisfied my-iterated-any/iter procedure?)

