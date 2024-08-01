;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname practice_linked_lists_solutions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define test-str
  "Delete this string and complete the check-expect!")
(define proc-str
  "Delete this string and fill in with code!")

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

(define (true-counter lob) ; to count the number of true's
  (if (empty? lob)
      0
      (if (first lob)  ; if a true is encountered
          (+ 1
             (true-counter (rest lob)))
          (true-counter (rest lob)))))

(check-expect (true-counter '(#f #f #f))
              0)
(check-expect (true-counter '(#f #t #f #t #f))
              2)

(define (num-counter lon num) ; to convert the lon to a list of booleans
  (if (empty? lon) ; return empty list as the base case
      lon
      (cons (= num (first lon)) ; cons this result, which is true or false
            (num-counter (rest lon)
                          num))))

(check-expect (num-counter '(1 2 1 4) 1)
              '(#t #f #t #f))
(check-expect (num-counter '() 0)
              '())
(check-expect (num-counter '(1 2 1 4) 5)
              '(#f #f #f #f))

(define (num-detector lon num)
  (true-counter (num-counter lon num)))

(check-expect (num-detector '(1 2 1 4) 1)
              2)
(check-expect (num-detector '(1 2 3239 272 491 482 29 38 212 29 0) 3239)
              1)
(check-expect (num-detector '(1 23 4 5 6 7 8 19) 20)
              0)


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
  (if (empty? lon)
      lon
      (if (odd? (first lon)) ; test to see if the 1st element is odd
          (cons (first lon)
                (only-odds (rest lon)))
          (only-odds (rest lon)))))

(check-expect (only-odds '(1 2 3 4 5 6))
              '(1 3 5))
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

(define (fact num) ; helper function to compute a factorial
  (if (<= num 1)
      1
      (* num
         (fact (- num 1)))))
(check-expect (fact 4)
              24)
(check-expect (fact 0)
              1)
(check-expect (fact 5)
              120)

(define (list-of-facts lst)
  (if (empty? lst)
      lst
      (cons (fact (first lst)) ; cons the factorial of the first element
            (list-of-facts (rest lst)))))
(check-expect (list-of-facts '(4 3 2 1))
              '(24 6 2 1))
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
  (if (empty? los)
      los
      (if (>= (string-length (first los))
              3)
          (cons (substring (first los)
                           0
                           1)
                (first-letters (rest los)))
          (first-letters (rest los)))))

(check-expect (first-letters '("abc"  "de" "egh"))
              '("a" "e"))
(check-expect (first-letters '("a" "b" "d" "eghk" "khge"))
              '("e" "k"))
(check-expect (first-letters '("a" "b" "c"))
              '())


