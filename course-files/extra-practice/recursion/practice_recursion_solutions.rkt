;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname practice_recursion_solutions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|

Practice Problems for: Recursion

Writing out problems may help with
wrapping your head around recursion

example:
(define (sum-lst lst)
   (if (empty? lst)
       0
       (+ (first lst)
          (sum-lst (rest lst)))))

(sum-lst '(1 2 3 4))

(+ 1
   (sum-lst '(2 3 4)))

(+ 1
   (+ 2
      (sum-lst '(3 4))))

(+ 1
   (+ 2
      (+ 3
         (sum-lst '(4)))))

(+ 1
   (+ 2
      (+ 3
         (+ 4
            (sum-lst '())))))

(+ 1
   (+ 2
      (+ 3
         (+ 4
            0))))

which is 10

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
Recursive Procedures!

Remember that most recursive
procedures can be written with two steps:

1. If base case*, return result of easy case
2. If not, then recurse with updated input

(if base case
    base case answer
    recurse w/ updated input)

Recursion is also really cool! It comes up a lot
in CS 348 and CS 214, as well many other classes.
It's even in design. The design process can be recursive:
research, brainstorm, create mock-ups, test and review,
and repeat. Even some parts of life are recursive.
"Practice makes perfect" may be thought of as a recursive
mindset.

*Some procedures may require more than one base case
|#


#|
QUESTION 1
Create a procedure!

only-ones is a procedure that takes in
one input:

lon, a list of numbers

only-ones should recursively add all of the
ones in lon, and return the sum, and it skips
all numbers that aren't one. Assume the empty
list is your base case.
|#

(define (only-ones lon)
  (if (empty? lon)
      0
      (if (= 1 (first lon))
          (+ 1
             (only-ones (rest lon)))   
          (only-ones (rest lon)))))

(check-expect (only-ones '(1 2 1 2 0 1 4 5))
              3)

(check-expect (only-ones '(2 173 92 74 2 4 7 9))
              0)


#|
QUESTION 2
Create a procedure!

reverse-str is a procedure
that takes in one input:

str, a string

reverse-str should return str in its reversed form
(reverse-str "racket") = "tekcar"

use substring here and string-append
local is also helpful in making things neat
please write this recursively

substring: string number (optional) number -> string

substring returns the parts of its input string
from the index of the first number, to the
index of the second number, exclusive

So (substring "computer" 1 4) = "omp"
The second number is optional. If it
is not given, then substring will go to the
last index, so (substring "canvas" 3) = "vas"
|#

(define (reverse-str str)
 (local [(define last-char
                (- (string-length str)
                   1))]
        (if (= 0
               (string-length str))
            ""
            (string-append (substring str
                                      last-char)
                           (reverse-str (substring str
                                                   0
                                                   last-char)))
      )))

(check-expect (reverse-str "iheart111")
              "111traehi")
(check-expect (reverse-str "Northwestern")
              "nretsewhtroN")
(check-expect (reverse-str "")
              "")

#|
QUESTION 3
Create a procedure!

recur-exponent is a procedure that
takes in two inputs:

base: a number
power: a non-negative number

it returns base^power,
so (recur-exponent 2 3) = 2^3 = 8
|#

(define (recur-exponent base power)
  (if (= power 0)
      1
      (* base
         (recur-exponent base
                         (- power 1)))))
(check-expect (recur-exponent 2 3)
              8)
(check-expect (recur-exponent 2 5)
              32)
(check-expect (recur-exponent 10000 0)
              1)

#|
QUESTION 4
Create a procedure!

find-z takes in one input:

str, a string

find-z returns true if a "z"
occurs in str, false otherwise

assume that str has
at least one character

local, string=?, and substring are helpful here
|#

(define (find-z str)
 (local [(define first-char (substring str
                                       0
                                       1))]
         (if (= (string-length str)
                1)
             (string=? "z"
                        first-char)    
             (or (string=? "z"
                           first-char)
                 (find-z (substring str 1))))))
(check-expect (find-z "hhifhslfhdjsfhz")
              #t)
(check-expect (find-z "ifhflfhufbwfuey")
              #f)

#|
Sources:
The inspiration for problems 2 and 4 came from
https://www.w3resource.com/c-programming-exercises/recursion/index.php
|#