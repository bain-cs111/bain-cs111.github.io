#lang htdp/isl+
;; These included code is mostly tests to make sure all of your images are named correctly for grading.
;; These don't guarantee all of your tasks are correct...but it does give you an indication that you're on the
;; right track. When you're ready to double-check an exercise, uncomment the appropriate lines by deleting
;; the semi-colon (;)

;; Activity 1

; your function call to make a red square goes here

;(check-expect (image? a-red-square) #t) ; uncomment this when ready to test!

;; Activity 2

; your function call to make a blue circle goes here

;(check-expect (image? a-blue-circle) #t)

;; Activity 3
;(check-expect (image? a-northwestern-stop-sign) #t)

;; Activity 4
;(check-expect (image? outlined-square) #t)
;(check-expect (image? outlined-circle) #t)
;(check-expect (image? outlined-stop-sign) #t)

;; Activity 5
;(check-expect (image? row-of-hexagons) #t)
;(check-expect (image? column-of-hexagons) #t)
;(check-expect (image? nested-hexagons) #t)

;; Activity 6
;(overlay (circle 30 "solid" "green" square 100 "solid" "orange")))
;(check-expect (image? debugging-exercise) #t)

;; Activity 7
;(check-expect (image? barbie-bowtie) #t)

;; Activity 8
;(check-expect (image? flag-of-chicago) #t)

;; Activity 9
;(check-expect (image? my-custom-design) #t)

;; Activity 10
; Note, "procedure" is another word for FUNCTION
;(check-expect (procedure? glasses) #t)
;(check-expect (image? (glasses (circle 50 "outline" "black") (rectangle 20 2 "outline" "black")) #t))