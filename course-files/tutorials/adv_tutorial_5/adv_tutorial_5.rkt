;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname adv_tutorial_5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
   ;; name has the value "Ian
   (list 'name "Ian")
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

;; expand-cond: list -> list
;; Transforms a cond expression into the equivalent set of nested ifs.
(define (expand-cond cond-exp)
  (local [(define (expand-clauses clauses)
            (local [(define next-clause (first clauses))
                    (define test (first next-clause))
                    (define value (second next-clause))]
              (if (eq? test 'else)
                  value
                  (list 'if
                        test
                        value
                        (expand-clauses (rest clauses))))))]
    (expand-clauses (rest cond-exp))))

(check-expect (expand-cond '(cond (a b)
                                  (c d)
                                  (else e)))
              '(if a
                   b
                   (if c
                       d
                       e)))

;; expand-and: list -> list
;; Transforms an and expression into an equivalent set of nested ifs.
(define (expand-and and-exp)
  "fill me in")

;(check-expect (expand-and '(and a b c))
;              '(if a
;                   (if b
;                       c
;                       #f)
;                   #f))

;; expand-or: list -> list
;; Transforms an or expression into an equivalent set of nested ifs.
(define (expand-or or-exp)
  "fill me in")

;(check-expect (expand-or '(or a b c))
;              '(if a
;                   #t
;                   (if b #t c)))

;; expand-local: list -> list
;; Transforms a local expression into an equivalent procedure call.
(define (expand-local local-exp)
  "Fill me in")

;(check-expect (expand-local '(local ((define a (+ x 1))
;                                     (define b (- y 1)))
;                               (* a b)))
;              '((lambda (a b)
;                  (* a b))
;                (+ x 1)
;                (- y 1)))

;; Dictionary of just the macros
;; As with the regular dictionary, it's a list of two-element sublists.
;; Each sublist is of the form: (name expander)
;; Where name is the name of the macro (e.g. cond)
;; and expander is a procedure that will rewrite the use of the macro into
;; equivalent code that doesn't use the macro.
(define macro-dictionary
  (list (list 'cond expand-cond)
        (list 'or expand-or)
        (list 'and expand-and)
        (list 'local expand-local)))

;; macro-expander: any -> procedure or #f
;; If argument is the name of a macro, return its expander.  Otherwise, #f.
(define (macro-expander operator)
  (if (symbol? operator)
      (local [[define probe (assq operator macro-dictionary)]]
        (if (list? probe)
            (second probe)
            #f))
      #f))

(check-satisfied (macro-expander 'cond)
                 procedure?)
(check-expect (macro-expander 'foo)
              #f)
(check-expect (macro-expander '(I am a list not a symbol))
              #f)


                   
;(check-expect (extend-dictionary '(a b)
;                                 '(1 2)
;                                 '((c 3) (d 4)))
;              '((a 1) (b 2) (c 3) (d 4)))

;; Struct to represent a procedure made by lambda
;; inputs: names of arguments (list of symbols)
;; result: expression to run to compute the output
;; dictionary: dictionary to use when calling the procedure
(define-struct interpreted-procedure
  (inputs result dictionary))

;; evaluate: any dictionary -> any
;; Runs the expression and returns its value
(define (evaluate exp d)
  (if (list? exp)
      (evaluate-complex exp d)
      (evaluate-primitive exp d)))

;; evaluate-primitive: non-list dictionary -> any
;; Runs a primitive expression (a constant or a variable name)
(define (evaluate-primitive exp d)
  (if (symbol? exp)
      (lookup exp d)
      exp))

;; evaluate-complex: list dictionary -> any
;; Runs a complex expression and returns its value
(define (evaluate-complex exp d)
  (local [(define operator (first exp))]
    (if (eq? operator 'if)
        (evaluate-if exp d)
        (if (eq? operator 'lambda)
            (evaluate-lambda exp d)
            (evaluate-call exp d)))))

;;;
;;; SPECIAL FORMS
;;;

;; evaluate-if list dictionary -> any
;; Runs an if expression represented as a list
(define (evaluate-if if-exp d)
  (if (evaluate (second if-exp) d)
      (evaluate (third if-exp) d)
      (evaluate (fourth if-exp) d)))

;; evaluate-lambda: list dictionary -> interpreted-procedure
;; Runs a lambda expression to create an interpreted procedure
(define (evaluate-lambda lambda-exp d)
  (make-interpreted-procedure (second lambda-exp)
                              (third lambda-exp)
                              d))

;;;
;;; CALLS
;;;

;; evaluate-call: list dictionary -> any
;; Evaluate a call, either a procedure call or a macro call.
(define (evaluate-call exp d)
  "Fill me in!")

;; evaluate-procedure-call: list dictionary -> any
;; Runs a procedure call and returns the result
(define (evaluate-procedure-call exp d)
  (local [(define proc (evaluate (first exp) d))
          (define args (map (λ (arg) (evaluate arg d))
                            (rest exp)))]
    (if (interpreted-procedure? proc)
        (apply-interpreted-procedure proc args)
        (apply proc args))))

(define (apply-interpreted-procedure proc args)
  (evaluate (interpreted-procedure-result proc)
            (extend-dictionary (interpreted-procedure-inputs proc)
                               args
                               (interpreted-procedure-dictionary proc))))

(check-expect (evaluate '1 dictionary)
              1)
(check-expect (evaluate 'a dictionary)
              1)
(check-expect (evaluate '(+ 1 1)
                        dictionary)
              2)
(check-expect (evaluate '(+ a (* b 2))
                        dictionary)
              5)
(check-expect (evaluate '(= a 1)
                        dictionary)
              true)

(check-expect (evaluate '(if (= 1 1) 1 0)
                        dictionary)
              1)
(check-expect (evaluate '(if (= 1 0) 1 0)
                        dictionary)
              0)

(check-expect (evaluate '(lambda (x) 1)
                        '())
              (make-interpreted-procedure '(x) 1 '()))

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


(check-expect (evaluate '(cond ((= 1 1) 1)
                               ((= 1 2) 2)
                               (else 0))
                        dictionary)
              1)
(check-expect (evaluate '(cond ((= 2 1) 1)
                               ((= 2 2) 2)
                               (else 0))
                        dictionary)
              2)
(check-expect (evaluate '(cond ((= 0 1) 1)
                               ((= 0 2) 2)
                               (else 0))
                        dictionary)
              0)

(check-expect (evaluate '(local ((define x (* b c)))
                           (* x x))
                        dictionary)
              36)
