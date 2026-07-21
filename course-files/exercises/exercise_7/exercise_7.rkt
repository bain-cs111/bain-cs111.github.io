#lang htdp/asl

(require "quiz_lib.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; printf basics
;; -------------
;;
;; printf (which stands for print formatted)
;; can be used to print (or produce) a string in the REPL (the bottom window),
;; with arguments that are formatted as strings.
;;
;; For example:
;;
#;   (printf "number ~a" 1)
;; number 1
;;
;; Here, the ~a tells printf, whatever comes after the string, substitute that for ~a, which
;; is the number 1 in this case
;;
;; You can use multiple ~a's, as such:
;;
#;   (printf "numbers ~a, ~a, and ~a!" 1 2 3)
;; numbers 1, 2, and 3!
;;
;; \n tells printf to create a new line (i.e. a line break):
;;
#;   (printf "\nnumbers")
;;
;;  numbers

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1
; Define the question struct and its methods here. 

; a question is a ....
;  - (make-question string string number)

; A sample 
(define q1
  (make-question
   "What is the first name of the Wicked Witch of the West?"
   "elphaba"
   15))

; Test to see if it's a question
(check-expect (question? q1) true)

; Test to check the question-text is correct
(check-expect (question-text q1) "What is the first name of the Wicked Witch of the West?")

; Test to check that the question text is printed correctly.
;;  Right-click on with-output-to-string to view its documentation.
;;  It runs the provided procedure and redirects the printed output to a string.
(check-expect
 (with-output-to-string
   (lambda () (print-question q1)))
 "Question (15%): What is the first name of the Wicked Witch of the West?\n")

; Test to check that the check-answer method works
(check-expect (check-answer q1 "elphaba")
              true)

; Test 5 - Check an answer that would be false, like "glinda"
; Test 6 - Check the point value is 15.
; Test 7 (and 8) - Check that get-point assigns the correct score for the given answer(s)


; an example "quiz" (a list of questions)
(define myquiz1 (list (make-question
                       "Where does the Wizard live?"
                       "Emerald City"
                       5)
                      q1))

;; grade-quiz : (listof question) (listof string-or-number) -> number
;;   Given a list of questions, a list of answers, print the incorrect questions
;;   and return the total score.
;;   - The list of questions can be just the question type or its subtype
;;   - The list of answers can be assumed to have correct types (i.e. either strings or numbers)
;;   - The list of questions and the list of answers have the same length


; Test to check that grade-quiz prints the questions correctly.
;;  Right-click on with-output-to-string to view its documentation.
;;  It runs the provided procedure and redirects the printed output to a string.
(check-expect
 (with-output-to-string
   (lambda () (grade-quiz myquiz1 (list "I don't know" "I don't know"))))
 (string-append "Missed Questions\n"
                "=====\n"
                "Question (5%): Where does the Wizard live?\n"
                "=====\n"
                "Question (15%): What is the first name of the Wicked Witch of the West?\n"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2
; Define the multichoice-question struct and its methods here. 

; a multiple choice question is a ...
;   (make-multichoice-question string number number number (listof string))

(define q2
  (make-multichoice-question
   "Which of the following is a song from Wicked?"
   1  ;; answer
   10 ;; point-value
   4  ;; count
   (list "Defying Gravity" "Wait For It" "Tomorrow" "None of the above"))) ;; choices

; Test 1 - check to see q2 is a question
; Test 2 - check to see if q2 is a multichoice-question
; Test 3 - check to see the count attribute is 4
; Test 4 - check that the question text and the choices are printed correctly.
;;  Right-click on with-output-to-string to view its documentation.
;;  It runs the provided procedure and redirects the printed output to a string.
(check-expect
 (with-output-to-string
   (lambda () (print-question q2)))
 (string-append "Question (10%): Which of the following is a song from Wicked?\n"
                "Enter a number between 1 and 4.\n"
                "1. Defying Gravity\n"
                "2. Wait For It\n"
                "3. Tomorrow\n"
                "4. None of the above\n"))
; Test 5 - Check to make sure the answer is 1
; Test 6 - Check its point value is 10
; Test 7 - Check that there are 4 items in the choices attribute



;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 3
; Define the numeric-question struct and its methods here. 

; a numeric question is a ...
;  - (make-numeric-question string number number number)

(define q3
  (make-numeric-question
   "How many millions of tickets have been sold for Wicked the musical?"
   55
   10
   2))

; Test 1 - Test to see if q3 is a question
; Test 2 - Test to see if it's a numeric-question
; Test 3 - Test to see it's NOT a multichoice-question
; Tests 4 to 7 - Test the range of accepted results
; Test 8 (and/or beyond) - Test if get-point correctly assigns credit when given correct answers


;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 4
; Define the partialcredit-question struct and its methods here.

; an partialcredit-question question is a ...
;  - (make-partialcredit-question string number number number (listof string) (listof number))


(define q4
  (make-partialcredit-question
   "Who played Glinda in the Broadway premiere of Wicked?"
   3
   15
   4
   (list "Cynthia Erivo" "Ariana Grande" "Kristin Chenoweth" "Idina Menzel")
   (list 2 4))) ;; other (partial-credit) answers

; Test 1 - Test to see if q4 is a multichoice-questionquestion
; Test 2 - Test to see if it's a partialcredit-question
; Test 3 - Test to see it's NOT a numeric-question
; Tests 4 to 6 - Test if get-point assigns the points 0, 15/2 and 15 to
;                the answers 1, 2, and 3


;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 5

; an example "quiz" (a list of questions)
(define myquiz (list q1 q2 q3 q4))


; runquiz : (listof question) -> void
; Takes a list of questions. In order, displays the question,
; gets a response from the user and checks the answer.
; Effect: A quiz has been displayed and run.
(define (runquiz somequiz) 
  (local [(define user-response "")
          (define points-correct 0)
          (define total-points-possible 0)]
    (begin (printf "Welcome to the Wicked Super-Fan quiz!\n\n")
           (for-each
            (lambda (q)
              (begin (print-question q)
                     (printf "> ")
                     (set! user-response (read-line))
                     (when (number? (question-answer/defaults-to-N/A q))
                       (set! user-response (string->number user-response)))
                     (cond
                       [(not (false? user-response))
                        (begin
                          (set! points-correct (+ points-correct (get-point q user-response)))
                          (if (check-answer q user-response)
                              (printf "Narrator: Wow, nice work!\n\n")
                              (printf "Narrator: Sorry, that's incorrect.\n\n")))]
                       [else  (printf "Narrator: TypeError: unsupported operand type(s).\n\n")])
                     (set! total-points-possible (+ total-points-possible
                                                    (question-point-value/defaults-to-0 q)))))
            somequiz)
           (printf "Your overall score is.... ~a out of ~a\n"
                   (number->string-digits points-correct 2)
                   (number->string-digits total-points-possible 2)))))
; run the quiz on our list of questions
; Once you've completed parts 1 and 2, uncomment the following line try to out the quiz! 
;(runquiz myquiz)
