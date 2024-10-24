#lang htdp/isl+

;; A dictionary contains relationships between variable names and their values
;; We represent a dictionary as a list of "variable bindings" where each variable binding is
;; itself a list of the name of the variable and the value of the variable.  So the binding
;; (list 'a 1) says the variable a is 1.
(define dictionary
  (list
   ;; a has the value 1
   (list 'a 1)
   ;; b has the value 2
   (list 'b 2)
   ;; c has the value 3
   (list 'c 3)
   ;; name has the value "Connor
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
            ;; Assq returns the sublist of dict that starts with variable-name
            ;; Or false if there isn't any sublist that starts with variable-name.
            (assq variable-name dict))]
    (if (list? probe)
        ;; Found it; return the value, which is the second element of the list
        (second probe)
        ;; Didn't find it; print an error message.
        (error "Unknown variable: " variable-name))))

;; NEW
;; extend-dictionary: (listof symbol), (listof any), dictionary -> dictionary
;; Makes a dictionary that starts with the specified variables and values and
;; then continues with the values in the old dicionary
(define (extend-dictionary variables values old-dictionary)
  (if (empty? variables)
      old-dictionary
      (cons (list (first variables)
                  (first values))
            (extend-dictionary (rest variables)
                               (rest values)
                               old-dictionary))))

;(check-expect (extend-dictionary '(a b)
;                                 '(1 2)
;                                 '((c 3) (d 4)))
;              '((a 1) (b 2) (c 3) (d 4)))

;; NEW
;; An interpreted-procedure object represents a procedure that was made within the interpreter
;; by a lambda expression.  It holds the argument list (inputs), the expression to compute the
;; result, and the dictionary within which it was defined.
;;
;; These objects are also called "closures" in programming language theory.
;; If you're curious, see https://en.wikipedia.org/wiki/Closure_(computer_programming)
(define-struct interpreted-procedure
  (inputs result dictionary))

;; ARGUMENT ADDED
;; evaluate: any dictionary -> any
;; Runs the expression and returns its value
(define (evaluate exp d)
  (if (list? exp)
      (evaluate-complex exp d)
      (evaluate-primitive exp d)))

;; ARGUMENT ADDED
;; evaluate-primitive: non-list dictionary -> any
;; Runs a primitive expression (a constant or a variable name)
(define (evaluate-primitive exp d)
  (if (symbol? exp)
      (lookup exp d)
      exp))

;; ARGUMENT ADDED
;; evaluate-complex: list dictionary -> any
;; Runs a complex expression and returns its value
(define (evaluate-complex exp d)
  "FILL-ME-IN")

;;;
;;; SPECIAL FORMS
;;;

;; NEW
;; evaluate-if list dictionary -> any
;; Runs an if expression represented as a list
;(define (evaluate-if if-exp d)
;  FILL ME IN)

;; NEW
;; evaluate-lambda: list dictionary -> interpreted-procedure
;; Runs a lambda expression to create an interpreted procedure
(define (evaluate-lambda lambda-exp d)
  "FILL-ME-IN")

;;;
;;; PROCEDURE CALLS
;;;

;; ARGUMENT ADDED
;; evaluate-procedure-call: list dictionary -> any
;; Runs a procedure call and returns the result
(define (evaluate-procedure-call exp d)
  "FILL-ME-IN")

(define (apply-interpreted-procedure proc args)
  "FILL-ME-IN")


;(check-expect (evaluate '1 dictionary)
;              1)
;(check-expect (evaluate 'a dictionary)
;              1)
;(check-expect (evaluate '(+ 1 1)
;                        dictionary)
;              2)
;(check-expect (evaluate '(+ a (* b 2))
;                        dictionary)
;              5)
;(check-expect (evaluate '(= a 1)
;                        dictionary)
;              true)

(check-expect (evaluate '(if (= 1 1) 1 0)
                        dictionary)
              1)
(check-expect (evaluate '(if (= 1 0) 1 0)
                        dictionary)
              0)

(check-expect (evaluate '(lambda (x) 1)
                        '())       ; A dictionary with no variables in it
              (make-interpreted-procedure '(x)
                                          1
                                          '()))

(check-expect (evaluate '((lambda (x) 1) 2)
                        dictionary)
              1)

(check-expect (evaluate '((lambda (x) (+ x 1)) 1)
                        dictionary)
              2)

(check-expect (evaluate '((lambda (x)
                            (lambda (y) x))
                          1)
                        '())
              (make-interpreted-procedure '(y)         ; input is y
                                          'x           ; returns the value of x (ignoring y)
                                          '((x 1))))   ; dictionary says x is 1




