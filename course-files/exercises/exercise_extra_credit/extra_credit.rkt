;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname extra_credit) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;; COMP_SCI 111 Extra Credit Review Exercise

;;;
;;; INSTRUCTIONS
;;; Fill in the code for the functions below, based on the specifications
;;; given in their signature and purpose statements.  In order to be clearer
;;; about our intentions, we've included a test case or two for each one.
;;; But you should still write test cases of your own.
;;;

;;;
;;; Part 1: LISTS
;;;

;; QUESTION 1: contains-a
;; contains-a: any (listof any) -> bool
;; Returns true if the first input is in the listof inputs and
;; returns false otherwise. It MUST be recursive.

(check-expect (contains-a 1 '(1 2 3 4))
              true)

(check-expect (contains-a "a" '("b" "c"))
              false)


;;
;; QUESTION 2: filter-map
;;

;; filter-map: (In -> Out) (In -> boolean) (listof In) -> Out
;; Calls the first argument, a function of every element of the list
;; for which the second argument returns true, and returns a list of the
;;  result.  So (filter-map function predicate list) behaves like
;; (map function (filter predicate list)).  But you must write this as
;; a recursive function. That is, you may not use map or filter. Hint: Start
;; by simply trying to write a function that filters the list, then edit
;; it to add the mapping. 

(define (filter-map f p l)
  "Fill me in!")

;; Simple test case: computing the squares of the odd numbers in a list.
(define (square-of x)
  (* x x))

(check-expect (filter-map square-of 
                          odd?
                          '(1 2 3 4 5))
              (list (square-of 1)
                    (square-of 3)
                    (square-of 5)))

; Write more test cases here.



;;
;; QUESTION 3: lookup
;; Here you'll write a function to look up the values of names
;; in a dictionary.
;;

;;
;; A dictionary is a data structure that stores relationships
;; between names and values.  For our purposes, we will represent a
;; dictionary as a list of (name value) lists, each stating that
;; the specified name has the specified value.
;;
;; Here's a sample dictionary.  It says the value of the variable
;; named "a" is 1, the value of the variable named "b" is 2, etc.
;; Note that this includes variables named "+", "*", etc., whose
;; values are the functions with the same names.
;;
(define sample-dictionary
  (list (list "a" 1)
        (list "b" 2)
        (list "+" +)
        (list "-" -)
        (list "*" *)
        (list "/" /)))

;; lookup: string dictionary -> any
;; Returns the value associated with the name in the dictionary, or
;; false if the name is not present in the dictionary.
(define (lookup name dict)
  "Fill me in!")

(check-expect (lookup "a" sample-dictionary)
              1)
(check-expect (lookup "b" sample-dictionary)
              2)

;;;
;;; PART 2: TREE RECURSION
;;;

;;
;; QUESTION 4: execute
;; In this question, you'll write a trivial interpreter for Racket in Racket.
;; If this sounds really cool, talk to someone in the teaching staff.
;; CS 321 teaches you some of the foundations of building
;; programming languages, and... the class is entirely in Racket!
;; Before we do this, we need to explain to you how to represent Racket code
;; as a Racket data structure!
;;

;;
;; A list-expression is Racket code written as a list, with variable
;; names as strings.  For example, this is how you'd write (* (+ 1 a) b)
;; as a list-expression:
;;
(define a-list-expression
  '("*" ("+" 1 "a")
        "b"))

;; Formally, a list-expression is either:
;;   a number, in which case, it just means the number
;;   a string, in which cases it means a variable
;;   a list of list-expressions, in which case it means a function call

;; So if you ran the code in a-list-expression, using the values of the
;; variables given in sample-dictionary, above, you'd get 4 because if you
;; ran (* (+ 1 a) b) when a=1, and b=2, you'd get 4.

;; Okay, time to reimplement Racket!  Write the following function.

;; execute: list-expression dictionary -> any
;; Returns the value of the Racket code represented by the list expression.
;; Hint: this is defined recursively
;;  If the expression is a number, return the number
;;  If it's a string, look it up in the dictionary
;;  If it's a list, it's a function call, so:
;;     - Use map to find the values of the subexpressions (elements of the list)
;;     - And then use apply to call
;;          - The function subexpression
;;          - With the values of the other subexpression as inputs
;;       Then just return the result of the call!

(define (execute exp dict)
  "Fill me in!")

;; Here are some test cases:
(check-expect (execute 1 sample-dictionary)
              1)
(check-expect (execute "b" sample-dictionary)
              2)
(check-expect (execute '("+" 1 2)
                       sample-dictionary)
              3)
(check-expect (execute a-list-expression sample-dictionary)
              4)

;;;
;;; PART 3: IMPERATIVE PROGRAMMING
;;;

;;
;; QUESTION 5: while
;; In this question, you'll write a function in Racket that acts
;; like the "while loops" in languages like Java.  A while loop
;; Takes two pieces of code to run: a condition and a body.  It
;; starts by running the condition to see if it's true.  If so,
;; it runs the body, then tries the condition again, running the
;; body again if it's true.  This continues until the condition
;; returns false.  Then the while loop returns.
;;

;; while: (-> boolean) (-> void) -> void
;; Repeatedly runs body until condition returns false.
(define (while condition body)
  "Fill me in!")

; Here are some test cases.

; If the condition starts false, never run the imperative
(check-expect (begin (while (λ () false)
                            (λ () (error "This code should never run!")))                              
                     "It didn't break")
              "It didn't break")

; Add up the numbers from 1 to 10
(check-expect (local [(define sum 0)
                      (define counter 1)]
                ;; While count <= 10, add counter to sum and add 1 to counter
                (begin (while (λ () (<= counter 10))
                              ; Add to sum and counter 
                              (λ ()
                                (begin (set! sum (+ sum counter))
                                       (set! counter (+ counter 1)))))
                       
                       sum))
              (+ 1 2 3 4 5 6 7 8 9 10))

;;; Now use your while function.

;;
;; QUESTION 6: sum-list redux
;; One more time, with feeling: write sum-list using while.
;;

;; sum-list: (listof number) -> number
;; Sums a list of numbers.
;; You MUST answer this by using your while function.
(define (sum-list l)
  "Fill me in!")

(check-expect (sum-list '(1 2 3))
              6)



