#lang htdp/isl+

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
