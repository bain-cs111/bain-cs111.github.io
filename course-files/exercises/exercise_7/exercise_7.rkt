;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname exercise_7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "define_super_struct.rkt")

#|
printf basics
-------------

printf (which stands for print formatted)
can be used to print (or produce) a string in the REPL (the bottom window),
with arguments that are formatted as strings.

For example:

> (printf "number ~a" 1)
number 1

Here, the ~a tells printf, whatever comes after the string, substitute that for ~a, which
is the number 1 in this case

You can use multiple ~a's, as such:

> (printf "numbers ~a, ~a, and ~a!" 1 2 3)
numbers 1, 2, and 3!

~n tells printf to create a new line (i.e. a line break):

> (printf "~n numbers")

 numbers
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1
; Define the question struct and its methods here. 

; a question is a ....
; (make-question string symbol number)


; A sample 
(define q1 (make-question "What is the home country of the Black Panther?" 'wakanda 3))

; Tests to see if the methods are both defined
(check-expect (procedure? display) true)
(check-expect (procedure? check-answer) true)

; Test to see if it's a question
(check-expect (question? q1) true)

; Test to check the question-text is correct
(check-expect (question-text q1) "What is the home country of the Black Panther?")

; Test to check that the check-answer method works
(check-expect (check-answer q1 'wakanda)
              true)

; Test 6 - Check an answer that would be false, like 'atlantis
; Test 7 - Check the point value is 3.


;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2
; Define the multiple-choice-question struct and its methods here. 

; a multiple choice question is a ...
;   (make-multiple-choice-question string symbol number number (listof string))

(define q2
  (make-multiple-choice-question
   "In the post-credits scene of The Avengers movie, what food
do the six Avengers go to eat?"
   '1 3 3
   (list "Shawarma" "Kabobs" "Manakish")))

; Test 1 - check to see q2 is a question
; Test 2 - check to see if q2 is a multiple-choice-question
; Test 3 - check to see the number-of-choices is 3
; Test 4 - Check to make sure the answer is '1
; Test 5 - Check its point value is 3
; Test 6 - Check how many choices there are



;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 3
; Define the numeric-question struct and its methods here. 

; a numeric question is a ...
;  - (make-numeric-question string symbol number number)

(define q3 (make-numeric-question "What version or mark armor does Tony Stark
 use during the Battle of Earth featured in Avengers: Endgame?" 85 3 2))

; Test 1 - Test to see if q3 is a question
; Test 2 - Test to see if it's a numeric-question
; Test 3 - Test to see it's NOT a multiple-choice-question
; Tests 4 to 7 - Test the range of accepted results (84, 85, 86 all should work...but 88 should not)


; an example "quiz" (a list of questions)
(define myquiz (list q1 q2 q3)) 

; runquiz: (listof question) -> void
; Takes a list of questions. In order, displays the question,
; gets a response from the user and checks the answer.
; Effect: A quiz has been displayed and run.
(define (runquiz somequiz) 
  (begin
    (printf "Welcome to the Marvel Cinematic Universe Super-Fan quiz! ~n")
    (local [(define user-response "")
            (define points-correct 0)
            (define total-points-possible 0)]
      (begin (for-each (λ (q)
                         (begin (display q)
                                (set! user-response (read))
                                (if (check-answer q user-response)
                                    (begin (printf "J.A.R.V.I.S: Wow, nice work! ~n~n")
                                           (set! points-correct (+ points-correct
                                                                   (question-point-value q))))
                                    (printf "J.A.R.V.I.S: Sorry, that's incorrect. ~n~n"))
                                (set! total-points-possible (+ total-points-possible
                                                               (question-point-value q)))))
                       somequiz)
             (printf "Your overall score is....")
             (print points-correct)
             (printf " out of ")
             (print total-points-possible)
             (newline)))))
; run the quiz on our list of questions
; Once you've completed parts 1 and 2, uncomment the following line try to out the quiz! 
;(runquiz myquiz)