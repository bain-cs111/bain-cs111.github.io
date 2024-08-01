;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname practice_linked_lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|

Practice Problems for: Linked Lists

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
List Procedures using cons

Remember that cons works like this:

(cons 4 (list 3 2 1)) = (list 4 3 2 1)
|#

(define proc-str
  "Delete this string and write your code!")

(define test-str
  "Delete this string and finish the check-expect!")

#|
QUESTION 1
Create a procedure!

num-detector takes in two inputs:

lon: a list of numbers
num: a number

num-detector returns the number of times
that num appears in lon

write this by first extracting a list of
booleans from lon, in which each element
is true if that corresponding element
in lon is equal to num, and false otherwise

as an example, if lon = '(1 2 3 4 5 4)
and num = 4

then first extract this list:
'(false false false true false true)

then use some procedure or expression
to compute the number of true's (which
would be 2 in the example above)

use cons and recursion to build your list of booleans
and assume that the base case is an empty list
|#

(define (num-detector lon num)
  proc-str)

(check-expect (num-detector '(1 2 1 4) 1)
              test-str)
(check-expect (num-detector '(1 2 3239 272 491 482 29 38 212 29 0) 3239)
              1)
(check-expect (num-detector '(1 23 4 5 6 7 8 19) 20)
              test-str)


#|
QUESTION 2
Create a procedure!

only-odds takes in one input:

lon, a list of numbers

only-odds returns a new list
of only the odd numbers in lon

use cons for this and write it
recursively, the base case
is the empty list
|#

(define (only-odds lon)
  proc-str)

(check-expect (only-odds '(1 2 3 4 5 6))
              test-str)
(check-expect (only-odds '())
              '())

#|
QUESTION 3
Create a procedure!

list-of-facts takes in one input:

lst, a list of numbers

list-of-facts computes the factorial
(n * (n-1) * (n-2)...) of
each elemeent in lst and cons it as part
of the result

(list-of-facts '(3 2 1)) = '(6 2 1)

the factorial of 0 and 1 is 1

write this recursively

|#

(define (list-of-facts lst)
  proc-str)
(check-expect (list-of-facts '(4 3 2 1))
              test-str)
(check-expect (list-of-facts '())
              '())

#|
QUESTION 4
Create a procedure!

first-letters takes in one input:

los: a list of strings

it returns a list, in which the first
letter of each element is
cons'd onto the result if
that element is at least three
characters long

write this recursively and use cons
as well as substring,
the empty list is the base case

(first-letters '("abc"  "de" "egh")) = '("a" "e")
|#

(define (first-letters los)
  proc-str)

(check-expect (first-letters '("abc"  "de" "egh"))
              '("a" "e"))
(check-expect (first-letters '("a" "b" "d" "eghk" "khge"))
              test-str)
(check-expect (first-letters '("a" "b" "c"))
              test-str)


