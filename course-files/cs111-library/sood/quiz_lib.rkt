#lang racket/base

(require mzlib/pconvert
         mzlib/pconvert-prop
         (for-syntax racket/base)
         "define_super_struct.rkt")

#| Expected struct declarations for exercise 7 and 8 |#
(provide
 read-line
 define-struct

 make-question
 question-answer/defaults-to-N/A
 question-points/defaults-to-0
 struct-spec:question

 make-multichoice-question
 struct-spec:multichoice-question

 make-numeric-question
 struct-spec:numeric-question

 show-text
 check-response

 make-quiz
 struct-spec:quiz
 update-scoring-hash
 reset-scoring-hash
 get-scoring-outcome

 struct-spec:fun-question
 choice-ref
 corresponding-outcome-ref)


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


(define-struct-method-if-not-exists show-text)
(define-struct-method-if-not-exists check-response)
(define-struct-method-if-not-exists update-scoring-hash)
(define-struct-method-if-not-exists reset-scoring-hash)
(define-struct-method-if-not-exists get-scoring-outcome)
(define-struct-method-if-not-exists choice-ref)
(define-struct-method-if-not-exists corresponding-outcome-ref)

(check-define-struct:warn-not-allowed-method? #t)
(check-define-struct:warn-unspecified-struct? #t)

(define-syntax make-question delayed-unbound-identifier-transformer)
(define-struct-name-specification question
  #:fields (text answer points)
  #:override-or-new-methods ()
  #:allowed-methods (show-text check-response))

(define/implicit-parameter (question-answer/defaults-to-N/A q)
  #:freevars ([question-answer (lambda (q) "N/A")])
  (question-answer q))

(define/implicit-parameter (question-points/defaults-to-0 q)
  #:freevars ([question-points (lambda (q) 0)])
  (question-points q))

(define-syntax make-multichoice-question delayed-unbound-identifier-transformer)
(define-struct-name-specification multichoice-question
  #:super question
  #:fields (option-count options)
  #:override-or-new-methods (show-text)
  #:allowed-methods (show-text check-response))

(define-syntax make-numeric-question delayed-unbound-identifier-transformer)
(define-struct-name-specification numeric-question
  #:super question
  #:fields (delta)
  #:override-or-new-methods (check-response)
  #:allowed-methods (show-text check-response))

(define-syntax make-quiz delayed-unbound-identifier-transformer)
(define-struct-name-specification quiz
  #:fields (title questions possible-outcomes scoring-of-outcomes)
  #:override-or-new-methods ()
  #:allowed-methods (update-scoring-hash reset-scoring-hash get-scoring-outcome))

(define-struct-name-specification fun-question
  #:fields (text choices corresponding-outcomes)
  #:override-or-new-methods ()
  #:allowed-methods (show-text
                     choice-ref
                     corresponding-outcome-ref))
