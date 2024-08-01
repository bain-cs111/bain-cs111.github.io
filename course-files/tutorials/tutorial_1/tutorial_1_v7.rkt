;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tutorial_1_v7.rk) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The two lines below import the 2htdp
; and iterated-images libraries, and are necessary
; for your code to work!
(require 2htdp/image)
(require "./iterated-images.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; READ THIS PLEASE!!!!!!!!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; PLEASE READ THE ASSIGNMENT BEFORE STARTING THIS TUTORIAL.
; FOR EACH FUNCTION, REPLACE THE ENTIRE
; "FILL ME IN", INCLUDING THE QUOTES, WITH YOUR
; CODE. FOR EXAMPLE, (+ 2 "FILL ME IN")
; MAY BECOME (+ 2 3), NOT (+ 2 "3") OR (+ 2 3 "FILL ME IN"), ETC

; UNCOMMENT EACH FUNCTION AND ITS CHECK-EXPECT WHEN YOU'RE DONE WITH
; THE FUNCTION. IF YOUR CODE COMES UP AS HIGHLIGHTED IN
; BLACK (Spooky Code), THIS IS NOT AN ERROR. THIS IS RACKET SAYING THAT
; THE HIGHLIGHTED CODE HAS NOT BEEN RAN/TESTED.

; BELOW ARE FUNCTION SKELETONS WITH
; TYPE SIGNATURES. PLEASE WRITE THE
; TYPE SIGNATURES FOR THE
; LAST TWO FUNCTION

; A TYPE SIGNATURE IS LIKE A CONTRACT OR ID
; FOR A FUNCTION. For something like:

; (define add2
;     (λ (n)
;        (+ 2 n)))

; Its type signature is number -> number
; It takes in a number and returns a number.

; If we see something like:
; (define my-circle
;     (λ (n)
;        (circle (+ 2 n)
;                "outline"
;                "red")))

; Its type signature is number -> image
; It takes in a number and returns an image.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BASIC ITERATION

;;; TYPE SIGNATURE: image number -> image
;;; Returns count copies of a given image
;;; all beside another using an iterator
(define my-row
    (λ (image count)
      (iterated-beside (λ (image-number)
			   "FILL ME IN")
		       count)))


;(check-expect (my-row (circle 50
;                              "solid"
;                              "blue")
;                      5)
;              .)

;(check-expect (my-row (star 50 "solid" "gold") 3)
;
;              .)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Abstraction and Iterating on Iterators

;; PLEASE FILL IN THE TYPE SIGNATURE:
;;; Returns an n x n grid of the
;;; given image, using the my-row
;;; procedure
(define my-grid "Fill me in!")

;(check-expect (my-grid (circle 30
;                               "outline"
;                               "green")
;                       4)
;              
;             .)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using the Iterator's Built-in Counter
;
;;; TYPE SIGNATURE: number -> image
;;; Returns n overlay'd circles outlined in purple,
;;; with radii increasing at 25 * i, where i is the
;;; number that is changing in iterated-overlay
(define my-bullseye "Fill me in!")


;(check-expect (my-bullseye 5)
;              .)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Combining Both Ideas
;

;;; PLEASE FILL IN THE TYPE SIGNATURE: 
;;; Returns n rows of images,
;;; in which the bottom row has n images
;;; and the top row has 1 image,
;;; creating a pyramid, and it has
;;; to use my-row

(define my-pyramid "Fill me in!")

;(check-expect (my-pyramid (circle 10
;                               "outline"
;                               "red")
;                       5)
;              .)


;;;; Messing with colors

(define my-bullseye/color "Fill me in!")

;(check-expect (my-bullseye/color 5 (color 255 0 0) (color 0 255 0))
;              .)