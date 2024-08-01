;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tutorial_5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; drop: (listof T) number -> (listof T)
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

;; take: (listof T) number -> (listof T)
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

;; A binary-tree is a number of (make-branch number binary-tree binary-tree
;; Note: binary trees get used for lots of things and not always for storing numbers.
;; So we really *ought* to call this something like binary-number-tree, but that's
;; too cumbersome to type.  So we'll just leave it here as binary-tree.
(define-struct branch (number left right))

;; count-tree: binary-tree -> number
;; Returns the number of numbers in the tree

(check-expect (count-tree 0)
              1)
(check-expect (count-tree (make-branch 1 0 0))
              3)
(check-expect (count-tree (make-branch 1
                                       (make-branch 2 0 0)
                                       (make-branch 3 0 4)))
              7)

;; sum-tree: binary-tree -> number
;; Returns the sum of all the numbers in the tree

(check-expect (sum-tree 12)
              12)
(check-expect (sum-tree (make-branch 1 2 3))
              6)
(check-expect (sum-tree (make-branch 1
                                       (make-branch 2 0 0)
                                       (make-branch 3 0 4)))
              10)
