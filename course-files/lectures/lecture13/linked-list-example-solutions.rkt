;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |10 - Linked Lists|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define (count-down n)
  (cond [(= n 0) '()]
        [else (cons n
                    (count-down (- n 1)))]))

(check-expect (count-down 5) (list 5 4 3 2 1))
(check-expect (count-down 0) '())

; my-append: (listof any) (listof any) -> (listof any)
; appends the lists together
; does recursion ON list1, plucking off one element at a time
(define (my-append list1 list2)
  (if (empty? list1)
      list2
      (cons (first list1)
            (my-append (rest list1) list2))))

(check-expect (my-append (list 1 2 3) (list 4 5 6))
              (list 1 2 3 4 5 6))
(check-expect (my-append (list 1 2 3) empty)
              (list 1 2 3))
(check-expect (my-append empty (list 4 5 6))
              (list 4 5 6))

; Challenge Problems

; insert: Number (listof Number) -> (listof Number)
; inserts number into the proper spot in a sorted list
; maintaining ascending order.
(define (insert n lon)
  (cond [(empty? lon)         (list n)]
        [(<= n (first lon))   (cons n lon)]
        [else                 (cons (first lon)
                                    (insert n (rest lon)))]))

(check-expect (insert 9 (list 1 3 8 10))
              (list 1 3 8 9 10))
(check-expect (insert 2 (list 4 5 6))
              (list 2 4 5 6))
(check-expect (insert 4 empty)
              (list 4))

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
(define (sorter lon)
  (cond [(empty? lon)    empty]
        [else            (insert (first lon)
                                 (sorter (rest lon)))]))

(check-expect (sorter empty) empty)
(check-expect (sorter (list 1 2)) (list 1 2))
(check-expect (sorter (list 3 2 1 5 4 6))
              (list 1 2 3 4 5 6))



; in?: string (listof string) -> boolean
; takes string and a list of strings and returns true if the item
; is in the list
(define (in? s l)
  (if (empty? l)
      false
      (or (string=? s (first l))
          (in? s (rest l)))))

(check-expect (in? "cat" (list "cat" "dog" "mouse"))
              true)
(check-expect (in? "cat" (list "dog" "mouse"))
              false)
(check-expect (in? "cat" empty)
              false)

;uniquify: (listof any) -> (listof any)
;takes a list and returns a uniquified list (deduplicating)
(define (uniquify l)
  (cond [(empty? l)                     empty]
        [(in? (first l) (rest l))   (uniquify (rest l))]
        [else                           (cons (first l)
                                              (uniquify (rest l)))]))

(check-expect (uniquify (list "cat" "dog" "mouse" "mouse" "cat"))
              (list "dog" "mouse" "cat"))
(check-expect (uniquify empty) empty)


































