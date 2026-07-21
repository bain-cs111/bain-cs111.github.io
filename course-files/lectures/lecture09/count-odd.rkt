#lang htdp/isl+
(check-expect (count-odd (list 1 2 3)) 2)
(check-expect (count-odd (list 3 3 3)) 3)
(check-expect (count-odd (list 2 4 6)) 0)
(check-expect (count-odd (list))       0)

(define (count-odd a-list)
  (if (empty? a-list)
      0 ; there are no odd #s in an empty list
      ; here I'm lazy and write a single recursive
      ;    step since it's the same in both cases
      (local [(define do-the-rest!
                (count-odd (rest a-list)))]
        ; if the first number is odd
        (if (odd? (first a-list))
            ; then the answer is 1 + however many
            ;   odd numbers in the rest
            (+ 1 do-the-rest!)
            ; it's not odd, then the answer is 0 +
            ;   however many odd numbers in rest
            (+ 0 do-the-rest!)))))

;; notice how local saves us some effort!