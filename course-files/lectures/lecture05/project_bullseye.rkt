;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname project_bullseye) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(underlay (circle 100 "solid" "blue")
          (circle 80  "solid" "green")
          (circle 60  "solid" "blue")
          (circle 40  "solid" "green"))

;; simple-bullseye: number -> image
;; creates a simple bullseye with the specified size
(define simple-bullseye
  (λ (size)
    (underlay (circle (- size 0)  "solid" "blue")
              (circle (- size 20) "solid" "green")
              (circle (- size 40) "solid" "blue")
              (circle (- size 60) "solid" "green"))))

; Now lets test our simple-bullseye function by CALLING it several times with inputs.
(simple-bullseye 100)
(simple-bullseye 80)
(simple-bullseye 200)
; (simple-bullseye 10) ; uh oh, this one gives us some trouble!


; We could write a slightly different version that does fractional size reductions!

;; fractional-bullseye number -> image
;; creates a bullseye with the specified size with a 25% reduction in radius for each ring
(define fractional-bullseye
  (λ (size)
    (underlay (circle (* size 1)    "solid" "blue")
              (circle (* size 0.75) "solid" "green")
              (circle (* size 0.5)  "solid" "blue")
              (circle (* size 0.25) "solid" "green"))))
; And test it...
(fractional-bullseye 100)
(fractional-bullseye 80)
(fractional-bullseye 200)
(fractional-bullseye 10)

; We could further customize by adding another input – like percentage

;; bullseye number number -> image
;; creates a bullseye with the specified size with a percentage reduction in radius for each ring
(define bullseye
  (λ (size percent)
    (underlay (circle (* size (- 1 (* 0 percent))) "solid" "blue")
              (circle (* size (- 1 (* 1 percent))) "solid" "green")
              (circle (* size (- 1 (* 2 percent))) "solid" "blue")
              (circle (* size (- 1 (* 3 percent))) "solid" "green"))))

; And test it...
(bullseye 100 0.25)
(bullseye 80  0.2)
(bullseye 200 0.5)
;(bullseye 10  1) ; hmmm, what's wrong with this one?
