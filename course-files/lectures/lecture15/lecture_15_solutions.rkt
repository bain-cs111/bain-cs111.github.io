;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lecture_15_solutions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define (add e db)
  (cons e db))

(check-expect (add ava-e empty) (list ava-e))
(check-expect (add ava-e (list jessica-e)) (list ava-e jessica-e))

; lookup-v1: number database -> employee or false
; find the employee in the database with the provided ssn
;  false if not found
(define (lookup-v1 n db)
  (cond [(empty? db)                        false]
        [(= n (employee-ssn (first db)))    (first db)]
        [else                               (lookup-v1 n (rest db))]))

(check-expect (lookup-v1 2 (list jessica-e ava-e george-e steve-e))
              george-e)
(check-expect (lookup-v1 2 empty)
              false)
(check-expect (lookup-v1 5 (list jessica-e ava-e george-e steve-e))
              false)

; imagine doing a lookup in a database that has 100 items
; BEST CASE: the employee we are looking for is first in the list
;            1 call to lookup
; WORST CASE: we do a lookup, the ssn is NOT in the list (false)
;             101 calls to lookup (n + 1)
; AVERAGE CASE: we do a lookup and the employee is in the middle of
;               the list 50 calls to lookup (n / 2 calls to lookup)

; VERSION 2
; an employee database is a sorted-list of people

; Definition: an INVARIANT is a constraint that always holds for a data type

; A sorted-list is either
;  - empty
;  - (cons employee[e] sorted-list[l])
; INVARIANT: each ssn number in the list l is larger than ssn of employee e

; add: employee database -> database
; adds the person to the database, maintaining ascending order
; (define (addv2 per db)
;   ...)

; (check-expect (addv2 jessica-e (list ava-e george-e steve-e))
;               (list ava-e george-e jessica-e steve-e))
; (check-expect (addv2 ava-e empty)
;               (list ava-e))

; Challenge, how do we insert a person into an already sorted list?

; lookupv2: number database -> person or false
; find the person in the database with the provided ssn
;   false if not found (the database is sorted)
(define (lookup-v2 n db)
  (cond [(empty? db)                       false]
        [(< n (employee-ssn (first db)))   false]
        [(= n (employee-ssn (first db)))   (first db)]
        [else                              (lookup-v2 n (rest db))])) ; n > first

(check-expect (lookup-v2 2 (list ava-e george-e jessica-e steve-e))
              george-e)
(check-expect (lookup-v2 2 empty)
              false)
(check-expect (lookup-v2 5 (list ava-e george-e jessica-e steve-e))
              false)
(check-expect (lookup-v2 1 (list george-e jessica-e steve-e))
              false)

; imagine doing a lookup in a database that has 100 items
; BEST CASE: the employee we are looking for is first in the list
;            or the number we are looking for is smaller than the first
;            1 call to lookup
; WORST CASE: we do a lookup, the ssn is NOT in the list (false)
;             101 calls to lookup (n + 1)
; AVERAGE CASE: we do a lookup and the person is in the middle of
;              the list 50 calls to lookup (n / 2 calls to lookup)

;VERSION 3
; an employee database is a sorted tree of people

; A database is either
; - empty
; - (make-db-node employee[e] database database)
(define-struct db-node [employee left right])

; constructor - make-db-node
; predicate - db-node? empty?
; selectors - db-node-employee, db-node-left, db-node-right

; INVARIANT: every person in 'left' has a smaller SSN than employee 'e'
;   and every person in 'right' has a larger SSN than person 'e'

; Exploiting the invariant - using the invariant to make our code more
;   efficient

; lookup-v3 : number database -> person or false
; returns person if ssn in database, false otherwise
;(define (lookup-v3 n db)
  ;what do you do if the db is empty?
  ;what do you do if the person at the root (db-node-person db)
  ;    has the ssn that we're looking for
  ;what do you do if the number we are looking for is less
  ;    than the person at the root
  ;what do you do if the number we are looking for is greater
  ;    than the person at the root
;  )
(define (lookup-v3 n db)
  (cond [(empty? db)         false]
        [(= n (employee-ssn (db-node-employee db))) (db-node-employee db)]
        [(< n (employee-ssn (db-node-employee db))) (lookup-v3 n (db-node-left db))]
        [(> n (employee-ssn (db-node-employee db))) (lookup-v3 n (db-node-right db))]))

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
(check-expect (lookup-v3 1 j-node) ava-e)
(check-expect (lookup-v3 3 j-node) jessica-e)
(check-expect (lookup-v3 4 j-node) steve-e)
(check-expect (lookup-v3 6 j-node) false)

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
;GARBAGE IN (invalid binary search tree) GARBAGE OUT
; If we give our perfect v3 function this tree...we won't get
; the corect answer as it DEPENDS on the structure of a BST
(define j-node-3 (make-db-node jessica-e empty empty))
(define s-node-3 (make-db-node steve-e empty j-node-3))
; s
;  \
;   j
(check-expect (lookup-v3 3 s-node-3) false) 

; imagine doing a lookup in a database that has 100 items
; BEST CASE: the person we are looking for is at the top of the tree
;            1 call to lookup
; WORST CASE:there are 100 people in the tree and we need to get to the
;            bottom of the tree. how deep is the tree?
;            how many times do we need to divide n by 2, to get to 0
;            log2(n)

;        7
;    4        12
;  2    5  11   14
;7 / 2 = 3
;3 / 2 = 1
;1 / 2 = 0

;log2(7) = about 3
;log2(1 million) = about 20