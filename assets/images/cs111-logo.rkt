;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname cs111-logo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(define (colored-carpet colors)
    (cond
      [(empty? (rest colors))
       (square 1 "solid" (first colors))]
      [else
       (local [(define c (colored-carpet (rest colors)))
               (define i (square (image-width c) "solid" (car colors)))]
         (above (beside c c c)
                (beside c i c)
                (beside c c c)))]))


(colored-carpet
   (list (color 164 149 195) ; pinkish
         
 
         (color 48 16 78) ; less blue
         (color 228 224 238) ; purple
        
         (color 118 93 160) ; pink
          (color 78 42 132)
         (color 78 42 132) ; blue
         )) ; yellow