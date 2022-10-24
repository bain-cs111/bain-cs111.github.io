;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Part 1

; An ancestry-tree is either
; - empty
; - (make-human string ancestry-tree ancestry-tree)
(define-struct human (name parent-1 parent-2))

; Here are some examples
; Note: For Game of Thrones fans, Jamie and Cersei makes
; these ancestries more complicated so for the purposes of this
; assignment they're Robert's kids, I apologize to purists :)
(define tytos (make-human "tytos" empty empty))

(define tywin (make-human "tywin" tytos empty))
(define joanna (make-human "joanna" empty empty))

(define kevan (make-human "kevan" tytos empty))
(define dorna (make-human "dorna" empty empty))

(define jamie (make-human "jamie" tywin joanna))
(define cersei (make-human "cersei" tywin joanna))
(define tyrion (make-human "tyrion" tywin joanna))

(define robert (make-human "robert" empty empty))

(define tommen (make-human "tommen" robert cersei))
(define myrcella (make-human "myrcella" robert cersei))
(define joffrey (make-human "joffrey" robert cersei))

(define lancel (make-human "lancel" kevan dorna))
(define willem (make-human "willem" kevan dorna))
(define martyn (make-human "martyn" kevan dorna))

; Question 1: ancestors-names
; ancestors-names: ancestry-tree -> (listof string)
; returns a list of all of the names of one’s ancestors including one's own name
(define (ancestors-names pers)
    "fill me in")

(check-expect (ancestors-names empty)
              empty)
(check-expect (ancestors-names tytos)
              (list "tytos"))
(check-expect (ancestors-names tyrion)
              (list "tyrion" "tywin" "tytos" "joanna"))
(check-expect (ancestors-names tywin)
              (list "tywin" "tytos"))

;Question 2: ancestors-names-exclusive

; ancestors-names-exclusive: ancestry-tree -> (listof string)
; returns a list of the names of one's ancestors excluding one's own name
(define (ancestors-names-exclusive pers)
    "fill me in")

(check-expect (ancestors-names-exclusive empty)
              empty)
(check-expect (ancestors-names-exclusive joanna)
              empty)
(check-expect (ancestors-names-exclusive myrcella)
              (list "robert" "cersei" "tywin" "tytos" "joanna"))

;Question 3: related?

; related?: ancestry-tree ancestry-tree -> boolean
; returns true if the family trees have a common ancestor
(define (related? a1 a2)
    "fill me in")

(check-expect (related? kevan kevan)
              true)
(check-expect (related? kevan dorna)
              false)
(check-expect (related? myrcella cersei)
              true)
(check-expect (related? robert willem)
              false)
(check-expect (related? tytos tommen)
              true)

; Part 2

; a person is
; (make-person number string)
(define-struct person (ssn name))
  
; A binary-search-tree is either
; - empty
; - (make-node person binary-search-tree binary-search-tree)
(define-struct node (person smaller larger))

; INVARIANT:
; every person in `smaller` has a smaller SSN than `person`, and
; every person in `larger` has a larger SSN than `person`

(define sara (make-person 1 "sara"))
(define ian (make-person 2 "ian"))
(define russ (make-person 3 "russ"))
(define simone (make-person 4 "simone"))
(define nell (make-person 5 "nell"))
(define jennie (make-person 6 "jennie"))
(define connor (make-person 7 "connor"))

(define sara-node (make-node sara empty empty))
(define russ-node (make-node russ empty empty))
(define ian-node (make-node ian sara-node russ-node))
(define simone-node (make-node simone ian-node empty))
(define jennie-node (make-node jennie empty empty))
(define connor-node (make-node connor jennie-node empty))
(define nell-node (make-node nell simone-node connor-node))


;                    nell
;                  /      \
;            simone        connor
;           /             /
;         ian          jennie
;       /     \
;    sara     russ

; Question 4: list-contents

; list-contents: binary-search-tree -> (listof number)
; takes a search-treee and returns a list of the SSNs of all people
; in the tree, in ascending order.
; You MUST exploit the invariant in your solution
; You may NOT use any kind of sorting function.
(define (list-contents tree)
  "fill me in")

(check-expect (list-contents nell-node)
              (list 1 2 3 4 5 6 7))
(check-expect (list-contents connor-node)
              (list 6 7))
(check-expect (list-contents sara-node)
              (list 1))

; Question 5: lookup

; lookup: number binary-search-tree -> string
; returns the name of the person with matching ssn
; if any person in the tree has the given ssn
; otherwise the string "not found" (see check-expects
; below for examples)
;
; You MUST exploit the invariant in your solution
; You may NOT use any kind of sorting function.
(define (lookup social-num pn)
  "fill me in")

(check-expect (lookup 1 nell-node)
              "sara")
(check-expect (lookup 1 connor-node)
              "not found")
(check-expect (lookup 6 nell-node)
              "jennie")
(check-expect (lookup 99999 nell-node)
              "not found")