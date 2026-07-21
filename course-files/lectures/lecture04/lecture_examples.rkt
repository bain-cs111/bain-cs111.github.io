#lang htdp/isl+
(require "iterated-images.rkt")
(require 2htdp/image)

;(overlay (square 50  "solid" (color 0 250 0))
;     (square 100 "solid" (color 0 200 0))
;     (square 150 "solid" (color 0 150 0))
;     (square 200 "solid" (color 0 100 0))
;     (square 250 "solid" (color 0 50  0)))

;;; box: number -> image
;;; makes a size x size square
;(define box
;  (λ (size)
;    (square size
;            "solid"
;            (color 0
;                   (- 260 size)
;                   0))))
;; draw the shape
;(overlay (box 10) (box 20) (box 30) (box 40)
;         (box 50) (box 60) (box 70) (box 80) (box 90)
;         (box 100) (box 110) (box 120) (box 130) (box 140)
;         (box 150) (box 160) (box 170) (box 180) (box 190)
;         (box 200) (box 210) (box 220) (box 230) (box 240)
;         (box 250))

;(iterated-overlay ;; number -> image
;                    ;; makes a square rotated by n degrees
;                    (λ (n)
;                      (rotate n
;                              (square 100 "outline" "blue")))
;                              10)

;(iterated-overlay ;; number -> image
;                    ;; makes a square rotated by n degrees
;                    (λ (n)
;                     (rotate n
;                              (square 100 "outline" "blue")))
;                              10)

;(iterated-overlay (λ (n)
;                      (rotate (* 36 n)
;                              (square 100 "outline" "blue")))
;                    10)
;

;(iterated-overlay
; ;; number -> image
; ;; make a square rotated and colored 
; ;; based on n
; (λ (n)
;   (square (* (+ n 1)                                 
;              50)
;           "solid"                                       
;           (color 0
;                  (- 250                                        
;                     (* n 50))                                             
;                  0)))
; 5)


;; boxes: number -> image
;;; Makes an image with a specified number of boxes
;(define boxes
;(λ (count)
;    (iterated-overlay
;      ;; number -> image
;      ;; make a box sized and colored based on n.
;      (λ (n)
;        (square (* (+ n 1)
;                   (quotient 250 count))
;                "solid"
;                (color 0
;                       (- 250
;                          (* n
;                             (quotient 250
;                                       count)))
;                       0)))
;      count)))

; (iterated-beside boxes 10)