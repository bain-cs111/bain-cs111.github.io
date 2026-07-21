#lang htdp/asl
(require "define_super_struct.rkt")

; a human is a...
;  - (make-human string number)
(define-struct human (name age))
;; Practice listing all the automatically defined functions!

; a student is a...
;  - (make-student string number string number)
(define-struct (student human) (netid year))
;; Practice listing all the automatically defined functions!

; a roster is a...
;  - (make-roster string human (listof student)
(define-struct roster (name instructor students))
;; Practice listing all the automatically defined functions!

;; Create a sample roster of students

; Write a function called show-roster that prints out the name and netid of each student
;   enrolled in some given class
;   roster -> (void)

; Write a function called, is-enrolled? that takes in a roster and a netid (string)
;  and returns whether or not that student is enrolled in that roster.