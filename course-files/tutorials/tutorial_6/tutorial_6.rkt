#lang htdp/isl+

;; drop : (listof T) number -> (listof T)
;; removes the first k elements of the list

(check-expect (drop '() 0)
              '())

(check-expect (drop '("a" "b" "c") 0)
              '("a" "b" "c"))

(check-expect (drop '("a" "b" "c") 1)
              '("b" "c"))

(check-expect (drop '("a" "b" "c") 2)
              '("c"))

(check-expect (drop '("a" "b" "c") 3)
              '())

;; take : (listof T) number -> (listof T)
;; Return the first k elements of the list

(check-expect (take '() 0)
              '())
(check-expect (take '("a" "b" "c") 0)
              '())
(check-expect (take '("a" "b" "c") 1)
              '("a"))
(check-expect (take '("a" "b" "c") 2)
              '("a" "b"))
(check-expect (take '("a" "b" "c") 3)
              '("a" "b" "c"))

;;;
;;; TREE STUFF
;;;

(require "tutorial_tree_tools.rkt")

;; A binary-tree is one of:
;;   - A number, e.g. 2025
;;   - A (make-branch number binary-tree binary-tree)
;;
;; Note: binary trees get used for lots of things and not always for storing numbers.
;; So we really *ought* to call this something like binary-number-tree, but that's
;; too cumbersome to type.  So we'll just leave it here as binary-tree.
(define-struct branch (number left right))

;; Use (draw-binary-tree SOME-TREE) to visualize a binary-tree
;; Example:
(draw-binary-tree (make-branch 2 1 3))

;; tree-a, tree-b and tree-c : binary-tree
;; Write down in ISL+ the binary-tree corresponding to the visualization on the assignment

(define tree-a
  "fill me in")

(define tree-b
  "fill me in")

(define tree-c
  "fill me in")


;; count-tree : binary-tree -> number
;; Returns the number of numbers in the tree

(check-expect (count-tree 0)
              1)
(check-expect (count-tree (make-branch 1 0 0))
              3)
(check-expect (count-tree (make-branch 1
                                       (make-branch 2 0 0)
                                       (make-branch 3 0 4)))
              7)

;; sum-tree : binary-tree -> number
;; Returns the sum of all the numbers in the tree

(check-expect (sum-tree 12)
              12)
(check-expect (sum-tree (make-branch 1 2 3))
              6)
(check-expect (sum-tree (make-branch 1
                                       (make-branch 2 0 0)
                                       (make-branch 3 0 4)))
              10)
