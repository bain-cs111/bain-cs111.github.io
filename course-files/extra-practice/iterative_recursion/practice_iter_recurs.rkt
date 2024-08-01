;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname practice_iter_recurs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


(define (fours lst)
 proc-str)

(check-expect (fours '(4 8 12))
              true)
(check-expect (fours '(4 8 13))
              test-str)



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
  proc-str)

(check-expect (countX "ahsdkdjklqjwe  ")
              0)
(check-expect (countX "")
              0)
(check-expect (countX "x ajsjal x auwdlj x")
              test-str)

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
  proc-str)

(check-expect (list220 '(1 10 2 20 3 30))
              3)
(check-expect (list220 '(1 10 2 20 3 30 4))
              3)
(check-expect (list220 '(1 10))
              1)
(check-expect (list220 '())
              test-str)
(check-expect (list220 '(1))
              test-str)
(check-expect (list220 '(1 10 2 20 3 30 5 6 13 5 78 283 91 910))
              test-str)

#|
QUESTION 4
Create a procedure!

good-bad-cops (inspired from https://codingbat.com/prob/p107330):

good-bad-cops takes in one input: lst, a list
of numbers

every even number counts for one good cop, and
every odd number counts for one bad cop

good-bad-cops returns G - B, in which G is the
number of good cops, and B is the number of bad cops

write this iteratively
|#


(define (good-bad-cops lst)
    proc-str)

(check-expect (good-bad-cops '())
              0)
(check-expect (good-bad-cops '(2 4 6 8 1 3 5))
              1)
(check-expect (good-bad-cops '(9 11 23 54))
              test-str)




