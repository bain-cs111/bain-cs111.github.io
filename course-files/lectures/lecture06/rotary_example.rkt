;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rotary_example) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#lang htdp/isl+
(require 2htdp/image)
(require "iterated-images.rkt")

;; rotary: number -> image
;; creates a rotary image with rectangles that change color
(define rotary (λ (num-spokes)
                 (iterated-overlay
                  (λ (picture-num)
                    (rotate (* (quotient 360 (* 2 num-spokes)) picture-num)
                            (rectangle 50 200
                                       "solid"
                                       (color (- 255 (* picture-num (quotient 255 num-spokes)))
                                              (* picture-num (quotient 255 num-spokes))
                                              0))))
                  num-spokes)))


;;; rotary: number -> image
;;; creates a rotary image with rectangles that change color
;(define rotary-2 (λ (num-spokes)
;                   (iterated-overlay
;                    (λ (picture-num)
;                      (local [(define q (quotient 250 num-spokes))]
;                        (rotate (* (quotient 360 (* 2 num-spokes)) picture-num)
;                                (rectangle 50 200
;                                           "solid"
;                                           (color (- 255 
;                                                     (* picture-num 
;                                                        q))
;                                                  (* picture-num 
;                                                     q)
;                                                  0)))))
;                    num-spokes)))
