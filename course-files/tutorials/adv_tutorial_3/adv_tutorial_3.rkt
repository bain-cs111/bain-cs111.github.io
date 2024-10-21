#lang htdp/isl+

;; A dictionary contains relationships between variable names and their values
;; We represent a dictionary as a list of "variable bindings" where each variable binding is
;; itself a list of the name of the variable and the value of the variable. So the binding
;; (list 'a 1) says the variable a is 1.
(define dictionary
  (list
   ;; a has the value 1
   (list 'a 1)
   ;; b has the value 2
   (list 'b 2)
   ;; c has the value 3
   (list 'c 3)
   ;; name has the value "Connor"
   (list 'name "Connor")
   ;; + has the same value that Racket has for +
   (list '+ +)
   ;; * has the same value that Racket has for *
   (list '* *)
   ;; Etc.
   (list '- -)
   (list '/ -)
   (list '= =)
   (list '> >)
   (list '< <)))

;; lookup: variable-name, dictionary -> any
;; Returns the value associated with the variable name in the dictionary
(define (lookup variable-name dict)
  (local [(define probe
            ;; assq returns the sublist of dict that starts with variable-name
            ;; Or false if there isn't any sublist that starts with variable-name.
            (assq variable-name dict))]
    (if (list? probe)
        ;; Found it; return the value, which is the second element of the list
        (second probe)
        ;; Didn't find it; print an error message.
        (error "Unknown variable" variable-name))))

; Note, all of the below are commented out...and they don't actually check to see if your procedures work. Write
; your own check-expects to test your new interpreter functions!
; check-satisfied is just like check-expect but instead checking to see if two values
; are the same, it checks to see if the second input (a predicate) returns true on the
; first input, some expression.
;(check-satisfied evaluate-primitive-expression procedure?)
;(check-satisfied evaluate-complex-expression procedure?)
;(check-satisfied evaluate-procedure-call procedure?)
;(check-satisfied evaluate procedure?)
