;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lecture_15) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; need to store employee names and social security numbers

;an employee is...
; - (make-employee number string)
(define-struct employee (ssn name))
;constructor - make-employee
;predicate - employee?
;selectors/accessors - employee-ssn, employee-name

(define ava-e (make-employee 1 "ava"))
(define george-e (make-employee 2 "george"))
(define jessica-e (make-employee 3 "jessica"))
(define steve-e (make-employee 4 "steve"))

; VERSION 1
; an employee database is simply a list of employees
; a database is
; - empty
; - (cons employee database)

;add: employee database -> database
;to add the employee to the database


; lookup-v1: number database -> employee or false
; find the employee in the database with the provided ssn
;  false if not found






; VERSION 2
; an employee database is a sorted-list of people

; Definition: an INVARIANT is a constraint that always holds for a data type

; A sorted-list is either
;  - empty
;  - (cons employee[e] sorted-list[l])
; INVARIANT: each ssn number in the list l is larger than ssn of employee e

; add: employee database -> database
; adds the employee to the database, maintaining ascending order
; (define (add-v2 per db)
;   ...)

; (check-expect (add-v2 jessica-e (list ava-e george-e steve-e))
;               (list ava-e george-e jessica-e steve-e))
; (check-expect (add-v2 ava-e empty)
;               (list ava-e))

; Challenge, how do we insert a employee into an already sorted list?



; lookup-v2: number database -> employee or false
; find the employee in the database with the provided ssn
;   false if not found (the database is sorted)

(check-expect (lookup-v2 2 (list ava-e george-e jessica-e steve-e))
              george-e)
(check-expect (lookup-v2 2 empty)
              false)
(check-expect (lookup-v2 5 (list ava-e george-e jessica-e steve-e))
              false)
(check-expect (lookup-v2 1 (list george-e jessica-e steve-e))
              false)

;VERSION 3
; an employee database is a sorted tree of people

; A database is either
; - empty
; - (make-db-node employee[e] database database)
(define-struct db-node [employee left right])

; constructor - make-db-node
; predicate - db-node? empty?
; selectors - db-node-employee, db-node-left, db-node-right

; INVARIANT: every employee in 'left' has a smaller SSN than employee 'e'
;   and every employee in 'right' has a larger SSN than employee 'e'

; Exploiting the invariant - using the invariant to make our code more
;   efficient

; lookup-v3 : number database -> employee or false
; returns employee if ssn in database, false otherwise
(define (lookup-v3 n db)
  ;what do you do if the db is empty?
  ;what do you do if the employee at the root (db-node-employee db)
  ;    has the ssn that we're looking for
  ;what do you do if the number we are looking for is less
  ;    than the employee at the root
  ;what do you do if the number we are looking for is greater
  ;    than the employee at the root
)

; binary search tree example 1
;      j
;     / \
;    a   s
;     \ 
;      g
(define g-node (make-db-node george-e empty empty))
(define a-node (make-db-node ava-e empty g-node))
(define s-node (make-db-node steve-e empty empty))
(define j-node (make-db-node jessica-e a-node s-node))

; Test our new lookup-v3!

; binary search tree example 2
;     g
;    / \
;   a   s
;      /
;     j
(define j-node-2 (make-db-node jessica-e empty empty))
(define s-node-2 (make-db-node steve-e j-node-2 empty))
(define a-node-2 (make-db-node ava-e empty empty))
(define g-node-2 (make-db-node george-e a-node-2 s-node-2))

; CHALLENGE: Write some tests that use this second binary tree.

;a
; \
;  g
;   \
;    j
;     \
;      s

; CHALLENGE: Is the above a binary search tree? If it is, what
; Racket code would you have to write to represent it?

;; NOTE: The following is NOT a valid BST
; GARBAGE IN (invalid binary search tree) GARBAGE OUT
; If we give our perfect v3 function this tree...we won't get
; the corect answer as it DEPENDS on the structure of a BST
(define j-node-3 (make-db-node jessica-e empty empty))
(define s-node-3 (make-db-node steve-e empty j-node-3))
; s
;  \
;   j
(check-expect (lookup-v3 3 s-node-3) false) 