#lang htdp/isl+
(require "tree_lib.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Part 1 - Ancestry Trees ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; An ancestry-tree is either
; - '() i.e. empty list which can also be represented by the keyword empty
; - (make-human string ancestry-tree ancestry-tree)
(define-struct human (name parentA parentB))

; Here are some examples
; Note: For Game of Thrones fans, Jamie and Cersei makes
; these ancestries more complicated so for the purposes of this
; assignment they're Robert's kids, I apologize to purists
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


#;  (draw-ancestry-tree joffrey)
;;  ^~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Run this function in the Interactions Window to visualize the ancestry tree
;; rooted at joffrey. Try other person's name too.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Activity 1. all-ancestors-names
(check-expect (all-ancestors-names empty)
              empty)
(check-expect (all-ancestors-names tytos)
              (list "tytos"))
(check-expect (all-ancestors-names tyrion)
              (list "tyrion" "tywin" "tytos" "joanna"))
(check-expect (all-ancestors-names tywin)
              (list "tywin" "tytos"))
; all-ancestors-names : ancestry-tree -> (listof string)
; returns a list of all of the names of one’s ancestors including one's own name





; Activity 2. my-ancestors-names
(check-expect (my-ancestors-names empty)
              empty)
(check-expect (my-ancestors-names joanna)
              empty)
(check-expect (my-ancestors-names myrcella)
              (list "robert" "cersei" "tywin" "tytos" "joanna"))
; my-ancestors-names : ancestry-tree -> (listof string)
; returns a list of the names of one's ancestors excluding one's own name




; Activity 3. are-they-related?
(check-expect (are-they-related? kevan kevan)
              true)
(check-expect (are-they-related? kevan dorna)
              false)
(check-expect (are-they-related? myrcella cersei)
              true)
(check-expect (are-they-related? robert willem)
              false)
(check-expect (are-they-related? tytos tommen)
              true)
; are-they-related? : ancestry-tree ancestry-tree -> boolean
; returns true if the family trees have a common ancestor







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Part 2 - Binary Search Trees ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; a person is...
; - (make-person number string)
(define-struct person (ssn name))
  
; A binary-search-tree is either...
; - false
; - (make-node person binary-search-tree binary-search-tree)
(define-struct node (data smaller larger))

; INVARIANT:
; every person in `smaller` has a smaller SSN than the person in `data`, and
; every person in `larger` has a larger SSN than the person in `data`

(define bhagavatula (make-person 1 "bhagavatula"))
(define geisler (make-person 2 "geisler"))
(define agarwal (make-person 3 "agarwal"))
(define st-amour (make-person 4 "st-amour"))
(define zhang (make-person 5 "zhang"))
(define hummel (make-person 6 "hummel"))
(define bain (make-person 7 "bain"))

#;  (draw-person zhang)
#;  (draw-person bain)
#;  (draw-person hummel)
#;  (draw-person st-amour)
;;  ^~~~~~~~~~~~~~~~~~~~
;; Run these commented lines of code to visualize `person` objects in the Interactions Window

(define bhagavatula-node (make-node bhagavatula #false #false))
(define agarwal-node (make-node agarwal #false #false))
(define geisler-node (make-node geisler bhagavatula-node agarwal-node))
(define st-amour-node (make-node st-amour geisler-node #false))
(define hummel-node (make-node hummel #false #false))
(define bain-node (make-node bain hummel-node #false))
(define zhang-node (make-node zhang st-amour-node bain-node))

#;  (draw-bst zhang-node)
;;  ^~~~~~~~~~~~~~~~~~~~
;; Run this commented code to visualize an entire binary search tree in the interactions window


;; IMPORTANT: what are the differences between
#;  (draw-person zhang)
;; and
#;  (draw-bst zhang-node)
;; and why they are different?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Activity 4. Creating BSTs
(check-expect (andmap check-bst
                      the-bsts)
              true)
(check-expect ... 5) ; there are 5 possibilities so what should you check about your list?

(define the-bsts (list
                  "fill me in" ; <- delete me & fill in the 5 binary search trees
                  ))

; You can use (map draw-bst the-bsts) to visualize all of your trees at the same time






; Activity 5. list-ssns
(check-expect (list-ssns zhang-node)
              (list 1 2 3 4 5 6 7))
(check-expect (list-ssns bain-node)
              (list 6 7))
(check-expect (list-ssns bhagavatula-node)
              (list 1))

; list-ssns : binary-search-tree -> (listof number)
; takes a search-treee and returns a list of the SSNs of all people
; in the tree, in ascending order.
; You MUST exploit the invariant in your solution
; You may NOT use any kind of sorting function.




; Activity 6. lookup
(check-expect (lookup 1 zhang-node)
              "bhagavatula")
(check-expect (lookup 1 bain-node)
              "not found")
(check-expect (lookup 6 zhang-node)
              "hummel")
(check-expect (lookup 99999 zhang-node)
              "not found")

; lookup : number binary-search-tree -> string
; returns the name of the person with matching ssn
; if any person in the tree has the given ssn
; otherwise the string "not found" (see check-expects
; below for examples)
;
; You MUST exploit the invariant in your solution
; You may NOT use any kind of sorting function.

(check-satisfied all-ancestors-names procedure?)
(check-satisfied my-ancestors-names procedure?)
(check-satisfied are-they-related? procedure?)
(check-satisfied the-bsts list?)
(check-satisfied list-ssns procedure?)
(check-satisfied lookup procedure?)

; Don't delete this line.