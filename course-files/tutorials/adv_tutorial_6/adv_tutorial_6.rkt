;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Advanced tutorial 7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; repl -> void
;; Runs your interpreter interactively!
;; REPL stands for Read-Evaluate-Print Loop.
(define (repl)
  (local [(define input (read))]
    (unless (eq? input 'quit)
      (begin (print (evaluate input dictionary))
             (newline)
             (repl)))))

;; A binding holds a variable and its value, along with a link to the next binding, if any.
;; Note that we're in advanced student language now, so define-struct also defines a procedure
;; called set-variable-name! that you can use to change the value in a binding.
(define-struct binding [variable-name value rest])

;; So now a dictionary is empty or (make-binding varaible value dictionary)
(define dictionary
  ;; + has the same value that Racket has for +
  (make-binding '+ +
                ;; * has the same value that Racket has for *
                (make-binding '* *
                              ;; Etc.
                              (make-binding '- -
                                            (make-binding '/ -                                                          
                                                          (make-binding 'print print
                                                                        (make-binding 'printf printf
                                                                                      (make-binding '= =
                                                                                                    (make-binding '> >
                                                                                                                  (make-binding '< <
                                                                                                                                (make-binding 'empty empty
                                                                                                                                              empty)))))))))))

;; lookup: variable-name, dictionary -> any
;; Returns the value associated with the variable name in the dictionary
(define (lookup variable-name dict)
  "Fill me in")

(check-expect (lookup 'empty dictionary)
              empty)
(check-error (lookup 'this-variable-shouldnt-exist
                     dictionary)
             "Unknown variable: 'this-variable-shouldnt-exist")

;; update-binding!: variable-name, dictionary -> void
;; Changes the value associated with the variable name in the dictionary
(define (update-binding! variable-name value dict)
  "Fill me in")

;; Change the value of empty
(check-expect (begin
                (update-binding! 'empty 0 dictionary)
                (lookup 'empty dictionary))
              0)
;; Change it back!
(check-expect (begin
                (update-binding! 'empty '() dictionary)
                (lookup 'empty dictionary))
              '())
(check-error (update-binding! 'this-variable-shouldnt-exist
                              0
                              dictionary)
             "Unknown variable: 'this-variable-shouldnt-exist")

;; extend-dictionary: (listof symbol), (listof any), dictionary -> dictionary
;; Makes a dictionary that starts with the specified variables and values and
;; then continues with the values in the old dicionary
(define (extend-dictionary variables values old-dictionary)
  "Fill me in")

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
  (local [(define (expand-conjuncts conjuncts)
            (if (empty? (rest conjuncts))
                (first conjuncts)
                (list 'if
                      (first conjuncts)
                      (expand-conjuncts (rest conjuncts))
                      #f)))]
    (expand-conjuncts (rest and-exp))))

(check-expect (expand-and '(and a b c))
              '(if a
                   (if b
                       c
                       #f)
                   #f))

;; expand-or: list -> list
;; Transforms an or expression into an equivalent set of nested ifs.
(define (expand-or or-exp)
  (local [(define (expand-disjuncts disjuncts)
            (if (empty? (rest disjuncts))
                (first disjuncts)
                (list 'if
                      (first disjuncts)
                      #t
                      (expand-disjuncts (rest disjuncts)))))]
    (expand-disjuncts (rest or-exp))))

(check-expect (expand-or '(or a b c))
              '(if a
                   #t
                   (if b #t c)))

;; expand-local: list -> list
;; Transforms a local expression into an equivalent procedure call.
(define (expand-local local-exp)
  (local [(define definitions (second local-exp))
          (define names (map second definitions))
          (define values (map third definitions))
          (define result-exp (third local-exp))]
    (cons (list 'lambda names result-exp)
          values)))

(check-expect (expand-local '(local ((define a (+ x 1))
                                     (define b (- y 1)))
                               (* a b)))
              '((lambda (a b)
                  (* a b))
                (+ x 1)
                (- y 1)))

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
    (cond [(eq? operator 'if)
           (evaluate-if exp d)]
          [(eq? operator 'lambda)
           (evaluate-lambda exp d)]
          [else
           (evaluate-call exp d)])))

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
  (local [(define expander (macro-expander (first exp)))]
    (if (procedure? expander)
        (evaluate (expander exp) d)
        (evaluate-procedure-call exp d))))

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
(check-satisfied (evaluate '(define a 1)
                           dictionary)
                 void?)              
(check-expect (evaluate 'a dictionary)
              1)
(check-satisfied (evaluate '(define b 2)
                           dictionary)
                 void?)
(check-expect (evaluate 'b dictionary)
              2)
(check-satisfied (evaluate '(define c 3)
                           dictionary)
                 void?)
(check-expect (evaluate 'c dictionary)
              3)

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
                                          (make-binding 'x 1 empty)))   ; dictionary says x is 1

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

(check-expect (begin
                (evaluate '(set! a 0)
                          dictionary)
                (lookup 'a dictionary))
              0)

(check-expect (evaluate '(begin (set! a -1)
                                a)
                        dictionary)
              -1)

(define (for-each proc list)
  (unless (empty? list)
    (begin (proc (first list))
           (for-each proc (rest list)))))

(check-expect (begin
                (for-each (λ (exp)
                            (evaluate exp dictionary))
                          '((define x 0)
                            (define sum 0)
                            (while (< x 10)
                                   (set! sum (+ sum x))
                                   (set! x (+ x 1)))))
                (lookup 'sum dictionary))
              (+ 0 1 2 3 4 5 6 7 8 9))

(check-expect (begin
                (update-binding! 'sum 0 dictionary)
                (evaluate '(for x 0
                           (< x 10)
                           (+ x 1)
                           (set! sum (+ sum x)))
                        dictionary)
                (lookup 'sum dictionary))
              (+ 0 1 2 3 4 5 6 7 8 9))
                
