#lang htdp/isl+

; filter - used to search through lists
; e.g. get all strings longer than 3 from a list of strings
(filter (λ (a-string) (> (string-length a-string)
                         3))
        (list "abc" "abcd" "abcde"))
; list as input...list as output!

(define sample-numbers (list 1 2 3 4))
; map - used to transform lists
; e.g. double all the numbers in a list
(define doubler
  (λ (a-list)
     (map (λ (n) (* n
                    2))
          a-list)))
; Check to see if the doubler function works by saying
; "here's a list...make sure I've doubled each element!"
(check-expect (doubler sample-numbers)
              (list 2 4 6 8))
; list as input...list as output!

; foldl/r - used to aggregate lists
; e.g. sum all the numbers in a list
(foldl (λ (x y)
         (+ x y))
       0
       (list 1 2 3 4))

; (list 1 2 3 4) -> 10
; list as input...output is an aggregate value!
; Notice the lambda as input to fold has 2 inputs!

; andmap / ormap - ask a question about ALL items of a list
; e.g. "are all the strings in this list longer than 3?"
(andmap (λ (x)
          (> (string-length x) 3))
        (list "abc" "abcd" "abcde"))
; (list "abc" "abcd" "abcde") -> true or false (Yes or No)
; Notice the lambda MUST return a boolean!