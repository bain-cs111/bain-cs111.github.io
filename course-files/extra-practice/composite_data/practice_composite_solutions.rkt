;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname practice_composite_solutions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|

Practice Problems for: Composite Data

Procedure coding questions
will have a description.

There are at most two check-expects for any given problem.
- One check-expect will already be completed
- If there is a second one, then based on
  the description, we ask that you
  complete the second check-expect

Please note that sussman form is used here.
Sussman form is just a shorthand notation;
these two procedures are the same:

(define (f x)    <--->      (define f
   (+ x 2))                     (λ (x)
                                  (+ 2 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Rules of Computation

; Questions 1A-1G are about the Rules of Computation.
; Knowing these rules are helpful for doing well on
; exams.
;;;;;;;;;;;;;;;;;;;;;;;;

QUESTION 1A
What do you do if
it's a word (variable name)?

Answer: Look up its value and return it

QUESTION 1B
What do you do if
it's a constant?

Answer: It's its own value, return it

QUESTION 1C
What do you do if it
looks like (a b c...)?

Answer: Check for a special form.
If it's not, then it's a procedure call, run a with
b c... as inputs, and return the output

QUESTION 1D
What do you do if it's (define n v)?

Answer: Run v, and assign its value
to n in the dictionary

QUESTION 1E
What do you do if it starts with local?
(local [(define n1 v1)
          (define n2 v2)
           ....
          (define n v)]
       result-exp)

Answer: run all v1, v2... and
assign them to their respective names
(n1, n2...) in the dictionary. Then
run result-exp and return the dictionary
to how it was before, and return the output
from result-exp

QUESTION 1F
What do you do if it starts with λ?
 (λ (n1 n2...)
   result-exp)

Answer: make a procedure with n1 n2,,, as inputs,
and return the output from result-exp (which
may or may not use n1 n2...!)

;;;;;;;;;;;;;;;;;;;
Structs

Structs are an important
concept in programming that'll come
up in other CS classes.
;;;;;;;;;;;;;;;;;;;


Let's say that we define an animal
struct, and two animals as shown below:
|#

(define-struct animal
  [food lifetime type])

(define my-dog
  (make-animal "salmon"
               10
               "Dalmatian"))
(define your-dog
  (make-animal "chicken"
               8
               "Jack Wrestler"))

#|
QUESTION 2A
What does (animal-food my-dog)
return?

Answer: "salmon"

QUESTION 2B
What does (string-append "It's a "
                         (animal-type your-dog))
return?

Answer: "It's a Jack Wrestler"
|#

#|
QUESTION 3
Create a procedure!

lost-dalmatian is a procedure
that takes in one input:

It returns true if anim has:

lifetime: 10, food: "turkey", and
type: "dalmatian"

It returns false otherwise


string=? and = may be used
|#

(define (lost-dalmatian anim)
    (if (and (= 10
                (animal-lifetime anim))
             (string=? "turkey"
                       (animal-food anim))
             (string=? "dalmatian"
                       (animal-type anim)))
        true
        false))

(check-expect (lost-dalmatian (make-animal "chicken"
                                           8
                                           "beagle"))
              false)
(check-expect (lost-dalmatian (make-animal "turkey"
                                           9
                                           "dalmatian"))
              false)

#|
QUESTION 4
Create a procedure!

distance-formula takes in
two inputs: p1 and p2

p1 and p2 are two instances
of the vect struct
(defined below as "vect")

distance-formula calculates the
distance between p1 and p2

for example, if p1 is (0, 0), and
p2 is (2, 1), then distance-formula
returns the value of ((2-0)^2 + (1-0)^2)^0.5,

which is (4 + 1)^0.5, or
the square root of 5

define a helper function/procedure
called sq, to calculate the square
of a number for you

you may use sqrt in distance-formula

|#

(define-struct vect
  [x y])

;;;;;;;;;;;;;;;;
; Helper procedure: sq
(define (sq num)
  (* num num))

(check-expect (sq 2)
              4)
;;;;;;;;;;;;;;;;


(define (distance-formula p1 p2)
  (sqrt (+  (sq (- (vect-x p1)
                   (vect-x p2)))
            (sq (- (vect-y p1)
                   (vect-y p2))))))
(define p1
  (make-vect 4 3))
(define p2
  (make-vect 0 0))
(define p3
  (make-vect 6 8))


(check-expect (distance-formula p1 p2)
              5)
(check-expect (distance-formula p2 p3)
              10)
