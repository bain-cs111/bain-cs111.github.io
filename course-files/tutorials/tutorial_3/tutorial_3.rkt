#lang htdp/isl+

;; Part 1



;; Part 2




;; Part 3

; Activity 8

;(check-expect (cat? my-cat) #true)
;(check-expect (string? (cat-name my-cat)) #true)
;(check-expect (string? (cat-breed my-cat)) #true)
;(check-expect (string? (cat-color my-cat)) #true)
;(check-expect (string? (cat-color my-cat)) #true)

; Put your two check-expects for meow-volume here

; Activity 9

; Uncomment these when ready...
;(check-expect (string? got-the-name) #true) 
;(check-expect (string? got-the-breed) #true) 

; Activity 10
(define lst-of-lsts
  (list (list 1 2 3) 
        (list 4 5 6) 
        (list 1 1 1 1)))
; (lists-product lst-of-lsts) should be equal to (1 * 2 * 3) * (4 * 5 * 6) * (1 * 1 * 1 * 1) or 720


; Activity 11
(define word-list (list "lollipops" "orange" "leaf" "uranium" "winnebago" "igloo" "neutron"))
; (first-letter-word-maker word-list) should be equal to "loluwin"