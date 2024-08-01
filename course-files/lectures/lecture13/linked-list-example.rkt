;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname linked-list-example) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; my-map: (X -> Y) (listof X) -> (listof Y)
; applies a function to each item in a list
(define (my-map func lst)
  (if (empty? lst)
      '()
      (cons (func (first lst))
            (my-map func (rest lst)))))

(check-expect (my-map abs (list)) (list))
(check-expect (my-map odd? (list 1 2 3 4))
              (list true false true false))
(check-expect (my-map abs (list -1 2 -3 4))
              (list 1 2 3 4))

; count-down: Number -> (listof Number)
; produce a list of numbers counting down from the input to 1



; my-append: (listof any) (listof any) -> (listof any)
; appends the lists together
; does recursion ON list1, plucking off one element at a time


; Challenge Problems

; in?: string (listof string) -> boolean
; takes string and a list of strings and returns true if the item
; is in the list

; uniquify: (listof any) -> (listof any)
; takes a list and returns a uniquified list (deduplicating)

; again, harder than our usual recursion problems. use a cond with 3 tests:
; 1. base case
; 2. is the element we're looking at in? the rest of the list? if so,
;    we don't need it. throw it out and recurse
; 3. otherwise, put this one on our output list and recurse

; insert: Number (listof Number) -> (listof Number)
; inserts number into the proper spot in a sorted list
; maintaining ascending order.

; Note this will look slightly different than our normal recursions
; as it will require either a nested if conditional or a cond with 3
; different test - option pairs.

; Hint: 1. base case
;       2. Is the insert number smaller than the one we're looking at 
;       3. Otherwise, cons the rest of the list

;insertion sort
;unsorted list          sorted list
;4 2 3 5 1
;2 3 5 1                4
;3 5 1                  2 4
;5 1                    2 3 4
;1                      2 3 4 5
;                       1 2 3 4 5

;sorter: (listof Number) -> (listof Number)
;to sort a list of numbers in ascending order

































