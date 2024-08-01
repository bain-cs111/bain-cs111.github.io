;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname lecture22_solutions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "define_super_struct.rkt")

(define x 3)
(define y "connor")
(define z true)

; printf stands for print formatted data
(printf "print some things ~a, ~a, and ~a ~n" x y z)
; can do the above using a bunch of print and newlines
;   but printf does it in a more consolidated way

; a roster is a
;  - (make-roster string string (listof string))
(define-struct roster (course-name instructor students)
  ; make-roster
  ; roster?
  ; roster-course-name, roster-instructor, roster-students
  ; set-roster-course-name!, set-roster-instructor!, set-roster-students!
  #:methods
  ; display: roster -> void
  ; to display the values in the attributes of the roster
  (define (display r)
    (begin (printf "~a~n" (roster-course-name r))
           (printf "instructor: ~a~n" (roster-instructor r))
           (local [(define student-num 1)]
             (for-each (λ (s)       
                         (begin (printf "~a. student: ~a~n" student-num s)
                                (set! student-num (+ 1 student-num))))
                       (roster-students r))))))

(define cs111 (make-roster "Fundamentals of Computer Programming"
                           "Connor Bain"
                           (list "yanning"
                                 "ava"
                                 "paula"
                                 "david"
                                 "liza"
                                 "jose")))
(display cs111)

; why do this? why not just have display outside of roster?
; Because now we can write display functions for any type

; a gastank is a
; (make-gastank number number)
(define-struct gastank (capacity gas-level)
  #:methods
  (define (display g)
    (printf "Gastank has capacity ~a and current level ~a.~n"
            (gastank-capacity g)
            (gastank-gas-level g))))

(define my-gastank (make-gastank 20 2))
(display my-gastank)

;so we have a unified way to call display on different object types,
;   but have variations in behavior
;   it does different things for different types


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; an animal is:
;  - (make-animal string number number)
; it has one method, age-one-year! that increases its age by 1
(define-struct animal (name weight age)
  #:methods
  (define (age-one-year! m)
    (set-animal-age! m (+ 1 (animal-age m)))))

;a cat is a subtype of animal that is:
; - (make-cat string number number string)
; it has one method, feed! that increases its weight by 2
(define-struct (cat animal) (sleeping-spot)
  #:methods
  (define (feed! m)
    (set-animal-weight! m (+ 2 (animal-weight m))))
  )

; a dog is a subtype of animal that is:
;   - (make-dog string number number string)
; it has two methods, feed! that increases its weight by 3
; and dr-my-name! which adds "dr. " to the front of the name
(define-struct (dog animal) (best-friend)
  #:methods
  (define (feed! m)
    (set-animal-weight! m (+ 3 (animal-weight m)))
  )
  (define (dr-my-name! m)
    (set-animal-name! m (string-append "Dr. " (animal-name m))))
  )
  
; a mouse is:
;  - (make-mouse string number number string) 
(define-struct (mouse animal) (hiding-spot)
  #:methods
  (define (feed! m)
    (set-animal-weight! m
                        (+ 1
                           (animal-weight m))))
  )

; examples
(define figaro (make-cat "figaro" 12 15 "horseshoe"))
(define molly  (make-dog "molly"  12  2 "emily"))
(define ralph (make-mouse "ralph" 1 1 "under the fridge"))
(define myzoo (list (make-cat "Obi" 10 13 "under the bed")
                    (make-dog "lassie" 20 19 "Timmy")))


;one can have methods that only work on some of the subtypes
;molly
;(dr-my-name! molly)
;molly

;or that behave the same for all subtypes
;ralph
;(age-one-year! ralph)
;ralph

;or methods that are named the same but vary in behavior for the
;   different subtypes
;what happens when we call feed!?
;figaro
;(feed! figaro)
;figaro

;molly
;(feed! molly)
;molly

; feed-and-print: animal number -> void
; feeds an animal n times and prints their updated weight after each feeding
; Effect: the animal has been fed n times, and displayed
(define (feed-and-print a n)
  (when (> n 0)
    (begin (feed! a)
           (printf "~a's weight is ~a after another feeding.~n"
                   (animal-name a)
                   (animal-weight a))
           (feed-and-print a (- n 1)))))

(feed-and-print ralph 4)
(feed-and-print figaro 2)
(feed-and-print molly 6)

; feed-zoo!: (listof animal) -> void
; takes a list of animals and feeds them all a meal
; that is appropriate for their species
; Effect: every animal in the zoo is larger
(define (feed-zoo! loa)
  (for-each (λ (a)
              (feed! a))
            loa))

(feed-zoo! myzoo)