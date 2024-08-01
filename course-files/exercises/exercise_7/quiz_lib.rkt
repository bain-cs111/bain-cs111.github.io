#lang racket/base

(require "define_super_struct.rkt")

#| Expected struct declarations for exercise 7 |#
(provide
 define-struct
 struct-spec:question
 struct-spec:multiple-choice-question
 struct-spec:numeric-question
 display
 check-answer)

(define-struct-method-if-not-exists display)
(define-struct-method-if-not-exists check-answer)

(check-define-struct:warn-not-allowed-method? #t)
(check-define-struct:warn-unspecified-struct? #t)

(define-struct-name-specification question
  #:fields (text answer point-value)
  #:override-or-new-methods ()
  #:allowed-methods (display check-answer))

(define-struct-name-specification multiple-choice-question
  #:super question
  #:fields (number-of-choices choices)
  #:override-or-new-methods (display)
  #:allowed-methods (display check-answer))

(define-struct-name-specification numeric-question
  #:super question
  #:fields (error-range)
  #:override-or-new-methods (check-answer)
  #:allowed-methods (display check-answer))
