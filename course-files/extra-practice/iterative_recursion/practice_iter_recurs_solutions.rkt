;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname practice_iter_recurs_solutions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|

Practice Problems for: Iterative Recursion

Procedure coding questions
will have a description.

Based on the description, we ask that you
complete any incomplete check-expects.

Please note that sussman form is used here.
Sussman form is just a shorthand notation;
these two procedures are the same:

(define (f x)    <--->      (define f
   (+ x 2))                     (λ (x)
                                  (+ 2 x)))

List shorthand notation:
(list 1 2 3)  <---> '(1 2 3)

;;;;;;;;;;;;;;;;;;;
Iterative Recursive Procedures!

Remember that most recursive
procedures can be written with two steps:

1. If base case*, return result of easy case
2. If not, then recurse with updated input

(if base case
    base case answer
    recurse w/ updated input)

The big difference between iterative and regular
recursion, is that with iterative recursion we RECURSE FIRST,
and use an accumulator to build up our result as we go.

As an example...

(define (sum-lst-iter lst part-sum)  ---> USE AN ACCUMULATOR
   (if (empty? lst)
       part-sum
       (sum-lst-iter (rest lst)     ---> RECURSE FIRST
                     (+ (first lst)
                        part-sum))))

(define (sum-lst lst)   ---> NO ACCUMULATOR
   (if (empty? lst)
       0
       (+ (first lst)
          (sum-lst (rest lst)))))  ---> RECURSE SECOND

*Some procedures may require more than one base case
|#

(define test-str
  "Delete this string and complete the check-expect!")
(define proc-str
  "Delete this string and fill in with code!")

#|
QUESTION 1
Create a procedure!

fours is a procedure that takes
in one input: lon, a list of numbers

it returns true if there are at least
three numbers in lon that are divisible by
four evenly (meaning that (remainder n 4) == 0)

it returns false otherwise

write this iteratively

hint: use a helper procedure with local or a
seperate definition outside of fours, and use >= to
implement "at least"
|#

(define (fours-help lst accum)
  (if (empty? lst)
      accum
      (if (= (remainder (first lst)  ; checking for evenly divisible by four
                         4)
             0)
          (fours-help (rest lst)
                      (+ 1 accum))
          (fours-help (rest lst)
                      accum))))

(define (fours lst)
  (if (>= (fours-help lst 0)
          3)
      true
      false))

(check-expect (fours '(4 8 12))
              true)
(check-expect (fours '(4 8 13))
              false)



#|
QUESTION 2
Create a procedure!

countX is a procedure that
takes in one input: str, a string

countX returns the number of "x" in str

write this iteratively, using a helper procedure

substring, string-length, and string=? are useful here,
and the base case is an empty string
|#

(define (countX str)
  (local [(define (countX-help str part-sum)
            (if (= (string-length str)
                   0)
                part-sum
                (if (string=? (substring str 0 1)
                              "x")
                    (countX-help (substring str 1)
                                 (+ 1 part-sum))
                    (countX-help (substring str 1)
                                 part-sum))))
                ]
         (countX-help str 0)))

(check-expect (countX "ahsdkdjklqjwe  ")
              0)
(check-expect (countX "")
              0)
(check-expect (countX "x ajsjal x auwdlj x")
              3)

#|
QUESTION 3
Create a procedure!

list220 (inspired from https://codingbat.com/prob/p173469):

Given a list of numbers, compute the number of times that
a value in the list is immediately followed by that
value times 10. 

(list220 '(1 2 20)) -> one
(list220 '(3 30)) -> one
(list220 '(3)) -> zero

(list220 '(1 10 2 20 3 30)) -> 3,
explanation: 1 then 10, 2 then 20, 3 then 30

assume that list220 takes in one input:
lst, a list of numbers

the base case is if the length of lst is 1 or
the empty list, and write this iteratively
|#

(define (list220 lst)
  (local [(define (list220-help lst part-sum)
            (if (<= (length lst)  ; accounts for both base cases
                    1)
                part-sum
                (if (= (/ (second lst)  ; checks if following number is 10 times current numebr
                          (first lst)) 
                       10)
                    (list220-help (rest lst)
                                  (+ 1 part-sum))
                    (list220-help (rest lst)
                                  part-sum))))]
         (list220-help lst 0)))

(check-expect (list220 '(1 10 2 20 3 30))
              3)
(check-expect (list220 '(1 10 2 20 3 30 4))
              3)
(check-expect (list220 '(1 10))
              1)
(check-expect (list220 '())
              0)
(check-expect (list220 '(1))
              0)
(check-expect (list220 '(1 10 2 20 3 30 5 6 13 5 78 283 91 910))
              4)

#|
QUESTION 4
Create a procedure!

good-bad-cops (inspired from https://codingbat.com/prob/p107330):

good-bad-cops takes in one input: lst, a list
of numbers

every even number counts for one good cop, and
every odd number counts for one bad cop

good-bad-cops returns G minus B, in which G is the
number of good cops, and B is the number of bad cops

write this iteratively
|#

(define (count-good-and-bad lst part-sum-good part-sum-bad)
  (if (empty? lst)
      (list part-sum-good part-sum-bad)  ; return two-element list: '(G B)
      (if (even? (first lst))
          (count-good-and-bad (rest lst)
                              (+ 1 part-sum-good) ; increment G
                               part-sum-bad)
          (count-good-and-bad (rest lst) ; has to be an odd number
                              part-sum-good
                              (+ 1 part-sum-bad))))) ; increment B

(define good-bad-cops
  (λ (lst)
    (- (first (count-good-and-bad lst 0 0)) ; extracts the count of good cops, G
       (second (count-good-and-bad lst 0 0))))) ; extracts the count of bad cops, B

(check-expect (good-bad-cops '())
              0)
(check-expect (good-bad-cops '(2 4 6 8 1 3 5))
              1)
(check-expect (good-bad-cops '(9 11 23 54))
              -2)




