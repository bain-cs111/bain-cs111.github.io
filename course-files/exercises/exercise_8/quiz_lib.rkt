#lang racket/base

(require mzlib/pconvert
         mzlib/pconvert-prop
         (for-syntax racket/base)
         (only-in "define_super_struct.rkt"
                  define/check-method
                  define-struct/methods
                  define/implicit-parameter
                  define-struct-name-specification
                  define-struct-method-if-not-exists
                  check-define-struct:warn-not-allowed-method?
                  check-define-struct:warn-missing-method?
                  check-define-struct:warn-unspecified-struct?))

#| Expected struct declarations for exercise 7 |#
(provide
 read-line
 (rename-out
  [define-struct/methods define-struct]
  [define/check-method define])

 delay-unbound-constructor-error?

;; Exercise 7 - Autograded Quizzes
 make-question
 question-answer/defaults-to-N/A
 question-point-value/defaults-to-0
 struct-spec:question

 make-multichoice-question
 struct-spec:multichoice-question

 make-numeric-question
 struct-spec:numeric-question

 make-partialcredit-question
 struct-spec:partialcredit-question

 print-question
 check-answer
 get-point
 

 ;; Exercise 8 - HashMaps / BuzzFeed Quizzes
 make-quiz
 struct-spec:quiz
 update-scoring-hash
 reset-scoring-hash
 get-scoring-outcome

 struct-spec:fun-question
 show-text
 choice-ref
 outcome-ref)


;; IMPORTANT: Disable showing value sharing
(show-sharing #f)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delaying the unbound identifier error for struct constructors
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (raise-unbound-fun-error self)
  (raise
   (exn:fail
    (apply string-append
           (format "~a: " (unbound-struct-ctor-name self))
           "this struct is not defined\n  inputs to the struct construtor procedure:"
           ;; ^ not using the "...:" convention to prevent DrRacket from folding error messages
           (for/list ([arg (in-list (unbound-struct-ctor-args self))])
             (format "\n    ~s" (print-convert arg))))
    (unbound-struct-ctor-cm self))))

(define delay-unbound-constructor-error? (make-parameter #t))

(struct unbound-struct-ctor (name args cm)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (raise-unbound-fun-error self))]
  #:property prop:print-converter
  (lambda (self recur-convert)
    (raise-unbound-fun-error self))
  #:transparent)

(define (guarded-unbound-struct-ctor name args cm)
  (define usc (unbound-struct-ctor name args cm))
  (unless (delay-unbound-constructor-error?)
    (raise-unbound-fun-error usc))
  usc)

(define-for-syntax (delayed-unbound-identifier-transformer stx)
  (syntax-case stx ()
    [form
     (identifier? #'form)
     (raise-syntax-error (syntax-e #'form)
                         "this struct is not defined"
                         #'form)]
    [(form arg ...)
     (syntax/loc stx
       (guarded-unbound-struct-ctor 'form (list arg ...) (current-continuation-marks)))]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quiz library
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct-method-if-not-exists print-question)
(define-struct-method-if-not-exists check-answer)
(define-struct-method-if-not-exists get-point)

(define-struct-method-if-not-exists show-text)
(define-struct-method-if-not-exists update-scoring-hash)
(define-struct-method-if-not-exists reset-scoring-hash)
(define-struct-method-if-not-exists get-scoring-outcome)
(define-struct-method-if-not-exists choice-ref)
(define-struct-method-if-not-exists outcome-ref)

(check-define-struct:warn-not-allowed-method? #t)
(check-define-struct:warn-missing-method? #f)
(check-define-struct:warn-unspecified-struct? #t)

(define-syntax make-question delayed-unbound-identifier-transformer)
(define-struct-name-specification question
  #:fields (text answer point-value)
  #:override-or-new-methods ()
  #:allowed-methods (print-question check-answer get-point))

(define/implicit-parameter (question-answer/defaults-to-N/A q)
  #:freevars ([question-answer (lambda (q) "N/A")])
  (question-answer q))

(define/implicit-parameter (question-point-value/defaults-to-0 q)
  #:freevars ([question-point-value (lambda (q) 0)])
  (question-point-value q))

(define-syntax make-multichoice-question delayed-unbound-identifier-transformer)
(define-struct-name-specification multichoice-question
  #:super question
  #:fields (count choices)
  #:override-or-new-methods (print-question)
  #:allowed-methods (print-question))

(define-syntax make-numeric-question delayed-unbound-identifier-transformer)
(define-struct-name-specification numeric-question
  #:super question
  #:fields (epsilon)
  #:override-or-new-methods (check-answer)
  #:allowed-methods (check-answer))

(define-syntax make-partialcredit-question delayed-unbound-identifier-transformer)
(define-struct-name-specification partialcredit-question
  #:super multichoice-question
  #:fields (others)
  #:override-or-new-methods (get-point)
  #:allowed-methods (get-point))


(define-syntax make-quiz delayed-unbound-identifier-transformer)
(define-struct-name-specification quiz
  #:fields (title questions possible-outcomes scoring-of-outcomes)
  #:override-or-new-methods ()
  #:allowed-methods (update-scoring-hash reset-scoring-hash get-scoring-outcome))

(define-struct-name-specification fun-question
  #:fields (text choices outcomes)
  #:override-or-new-methods ()
  #:allowed-methods (show-text
                     choice-ref
                     outcome-ref))