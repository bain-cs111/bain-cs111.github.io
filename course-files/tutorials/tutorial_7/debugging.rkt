#lang htdp/asl

; sum-list: (listof number) -> number
; returns the sum of the numbers in the list
(define (sum-list lon)
  (local [(define lst lon)
          (define sum 0)
          (define (help)
            (cond [(empty? lst)    sum]
                  [else            (begin
                                     (set! sum (+ sum (first lst)))
                                     (set! lst (rest lst))  
                                     ;; We could just print each variable one at
                                     ;;   at a time using the print function which only takes in
                                     ;;   one input. Just be careful you know exactly what you're
                                     ;;   printing. Try uncommenting the below (remove # and the ;)
                                     #;(print sum)
                                     #;(print lst)
                                     ;; Alternatively we could use the printf function that takes in
                                     ;;   a "print template" and some variables.
                                     ;;   This will print down in the interactions window
                                     ;;   the string we specify with the "~a"s replaced
                                     ;;   with the variables we provide as the later inputs
                                     ;;   and "~n" represents a new line.
                                     #;(printf "sum: ~a; lst: ~a~n" sum lst)
                                     (help))]))]
    (help)))

(sum-list '(1 2 3))