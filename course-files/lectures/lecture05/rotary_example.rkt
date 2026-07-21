#lang htdp/isl+
(require 2htdp/image)
(require "iterated-images.rkt")

;; rotary: number -> image
;; creates a rotary image with rectangles that change color from green to blue with 5 spokes
(define rotary
  (λ (num-spokes)
    (iterated-overlay (λ (current-count)
                        (rotate (* (/ 360 (* num-spokes 2)) current-count)
                                (rectangle 50 200
                                           "solid"
                                           (interpolate-colors (color 255 0 0)
                                                               (color 0 255 0)
                                                               (/ current-count (- num-spokes 1))))))
                      num-spokes)))
