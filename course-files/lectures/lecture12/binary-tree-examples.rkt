#lang htdp/isl+
; Pre-Recorded Lecture Binary Tree
(define-struct binary-tree (data left right))
; A binary tree is...
; - an empty list or
; - (make-binary-tree any binary-tree binary-tree)

; So all of these are valid Pre-Recorded Lecture Binary Trees

;     "hello"
(define lec-ex1 (make-binary-tree "hello!" empty empty)) ; a leaf with no children

;     #true
;    /     \
;   4   "hello!"
(define lec-ex2
  (make-binary-tree #true
                    (make-binary-tree 4 empty empty)
                    (make-binary-tree "hello!" empty empty)))


; count-tree/lecture : binary-tree -> number
(check-expect (count-tree/lecture lec-ex1) 1)
(check-expect (count-tree/lecture lec-ex2) 3)

(define (count-tree/lecture a-tree)
  (if (empty? a-tree)
      0
      (+ 1 (count-tree/lecture (binary-tree-left a-tree))
           (count-tree/lecture (binary-tree-right a-tree)))))


; Tutorial Binary Tree
(define-struct branch (data left right))
; A binary tree is...
; - a number or
; - (make-branch number binary-tree binary-tree)

; So all of these are valid Tutorial Binary Trees

;   0
(define tut-ex1 0) ; the simplest possible

;     0
;    / \
;   17  5
(define tut-ex2
  (make-branch 0 17 5)) ; a root with two children leaf nodes

;     0
;    / \
;   17 20
;  / \ / \
; 1  2 3  4
(define tut-ex3
  (make-branch 0 (make-branch 17 1 2)
                 (make-branch 20 3 4)))

; count-tree/tut : branch -> number
(check-expect (count-tree/tut tut-ex1) 1)
(check-expect (count-tree/tut tut-ex2) 3)
(check-expect (count-tree/tut tut-ex3) 7)

(define (count-tree/tut a-tree)
  (if (number? a-tree)
      1
      (+ 1 (count-tree/tut (branch-left a-tree))
           (count-tree/tut (branch-right a-tree)))))



; Exercise 5 Binary Tree
(define-struct human (name parentA parentB))
; An ancestry-tree is either
; - an empty list or
; - (make-human string ancestry-tree ancestry-tree)
