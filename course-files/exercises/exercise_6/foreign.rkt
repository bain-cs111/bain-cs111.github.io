#lang racket/base

(provide provide struct-out provide-for-test)

(require (for-syntax racket/base))

(define-syntax (provide-for-test stx)
  (syntax-case stx ()
    [(_ . args)
     (let ()
       (unless (and (list? (syntax-e #'args))
                    (andmap identifier? (syntax-e #'args)))
         (raise-syntax-error 'provide-and-test
                             "expected a list of identifiers"
                             #'args))
       (syntax/loc stx (module+ internal (provide . args))))]
    [_
     (raise-syntax-error 'provide-and-test
                         "expected a list of identifiers: (provide-for-test id ...)"
                         stx)]))
