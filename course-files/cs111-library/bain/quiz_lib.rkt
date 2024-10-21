#lang racket/base

(require mzlib/pconvert
         mzlib/pconvert-prop
         (for-syntax racket/base)
         "define_super_struct.rkt")

#| Expected struct declarations for exercise 7 |#
(provide
 read-line
 define-struct

 make-question
 question-answer/defaults-to-N/A
 question-point-value/defaults-to-0
 struct-spec:question

 make-multichoice-question
 struct-spec:multichoice-question

 make-numeric-question
 struct-spec:numeric-question

 print-question
 check-answer)


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
           ;; ^ not using the "...:" convention to avoid DrRacket folding the error message
           (for/list ([arg (in-list (unbound-struct-ctor-args self))])
             (format "\n    ~s" (print-convert arg))))
    (unbound-struct-ctor-cm self))))

(struct unbound-struct-ctor (name args cm)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (raise-unbound-fun-error self))]
  #:property prop:print-converter
  (lambda (self recur-convert)
    (raise-unbound-fun-error self))
  #:transparent)

(define-for-syntax (delayed-unbound-identifier-transformer stx)
  (syntax-case stx ()
    [form
     (identifier? #'form)
     (raise-syntax-error (syntax-e #'form)
                         "this struct is not defined"
                         #'form)]
    [(form arg ...)
     (syntax/loc stx
       (unbound-struct-ctor 'form (list arg ...) (current-continuation-marks)))]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quiz library
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct-method-if-not-exists print-question)
(define-struct-method-if-not-exists check-answer)

(check-define-struct:warn-not-allowed-method? #t)
(check-define-struct:warn-unspecified-struct? #t)

(define-syntax make-question delayed-unbound-identifier-transformer)
(define-struct-name-specification question
  #:fields (text answer point-value)
  #:override-or-new-methods ()
  #:allowed-methods (print-question check-answer))

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
  #:allowed-methods (print-question check-answer))

(define-syntax make-numeric-question delayed-unbound-identifier-transformer)
(define-struct-name-specification numeric-question
  #:super question
  #:fields (epsilon)
  #:override-or-new-methods (check-answer)
  #:allowed-methods (print-question check-answer))
