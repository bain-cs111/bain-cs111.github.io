#lang htdp/isl+

;             typename    field/attribute names
(define-struct student (netid first-name last-name))

;; This automatically defines...

; Constructor
; make-student: string string string -> student
; Creates a student given a netid, first name, and last name

; Predicate
; student?: any -> Boolean
; Checks to see if a given object is a student

; Accessors
; student-netid: student -> string
; Grabs just the netid from a given student

; student-first-name: student -> string
; Grabs just the first-name from a given student

; student-last-name: student -> string
; Grabs just the last-name from a given student

(define mascot (make-student "abc1234" "Willie" "Wildcat"))

(student-netid mascot) ; should give us "abc1234"

(student? mascot) ; gives us true
(student? 11) ; gives us false

