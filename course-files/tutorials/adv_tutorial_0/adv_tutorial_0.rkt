;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname adv_tutorial_0) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "shading.rkt")

;;;
;;; Examples
;;;

;; gradient: point -> brightness
;; An image, represented as a function of position, whose brightness increases from left to right.
(define gradient
  (λ (p)
    (point-x p)))

;(render gradient)

;; Note: if you think about it, gradient is actually the same as just point-x
;; so you can also just say (render point-x).

;; sin-grating: point -> brightness
;; An image of diagonal bands of dark and light.
(define sin-grating
  (λ (p)
    (* 255
       (abs (sin (* 0.1
                    (+ (point-x p)
                       (point-y p))))))))
;(render sin-grating)
