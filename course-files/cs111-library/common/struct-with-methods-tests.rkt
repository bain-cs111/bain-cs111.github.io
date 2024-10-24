#lang at-exp racket/base

(require rackunit
         racket/sandbox
         racket/string
         racket/port
         racket/runtime-path
         (only-in openssl ssl-default-verify-sources)
         syntax/modread
         (for-syntax racket/base syntax/parse/pre))

(define-syntax (check-define-struct-message stx)
  (syntax-parse stx
    [(_ exn-message-pattern:expr program:expr)
     (syntax/loc stx
       (check-exn (regexp (regexp-quote exn-message-pattern))
                  (λ () program)))]))

(define-syntax (check-define-struct-output stx)
  (syntax-parse stx
    [(_ output-message-pattern:expr program:expr)
     (syntax/loc stx
       (check-regexp-match
        (regexp (regexp-quote output-message-pattern))
        (with-output-to-string
          (λ () program))))]))

(module+ test
  (check-define-struct-message
   @string-append{
   define-struct: parent struct type not defined
   }
   @run-in-htdp-asl{
   (define-struct (X UNDEF) (n))
   })

  (check-exn
   #rx"oddp: this function is not defined"
   (λ ()
     (define M (run-in-htdp-asl))
     (M '(define (evenp n) (if (zero? n) true (oddp (sub1 n)))))))

  (check-equal?
   (let ()
     (define M (run-in-htdp-asl))
     (M '(define-struct S (x)
           #:methods
           (define (to_string s)
             (format "#<struct:S ~a>" (S-x s)))))
     (M '(to_string (S 123))))
   "#<struct:S 123>")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Error messages about struct fields
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (check-define-struct-message
   @string-append{
   define-struct: expected field names (in parenthesis) after the structure name, but nothing's there.
    If the structure has no (extra) fields, put an empty pair of parenthese there.
   }
   @run-in-htdp-asl{
   (define-struct T)
   })

  (check-define-struct-message
   @string-append{
   define-struct: expected field names (in parenthesis) after the structure name, but nothing's there.
    If the structure has no (extra) fields, put an empty pair of parenthese there.
   }
   @run-in-htdp-asl{
   (define-struct S (x y)
     #:methods
     (define (get-type s) 'S))

   (define-struct (T S))
   })

  (check-define-struct-message
   @string-append{
   define-struct: expected field names (in parenthesis) after the structure name, but found something else: #:methds
   }
   @run-in-htdp-asl{
   (define-struct S (x y)
     #:methods
     (define (get-type s) 'S))

   (define-struct (T S)
     #:methds
     (define (get-type t) 'T))
   })

  (check-define-struct-message
   @string-append{
   define-struct: expected field names (in parenthesis) after the structure name, but found something else: #:methods
    If the structure has no (extra) fields, put an empty pair of parenthese there.
   }
   @run-in-htdp-asl{
   (define-struct S (x y)
     #:methods
     (define (get-type s) 'S))

   (define-struct (T S)
     #:methods
     (define (get-type t) 'T))
   })


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Error messages about methods & their arguments
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (check-define-struct-message
   @string-append{
   define-struct: expected procedure definitions such as (define (method-id arg-id ...) body-expr) or (define method-id (lambda (arg-id ...) body-expr)) after the #:methods keyword, but found something else: (circle 15 "solid" "brown")
   }
   @run-in-htdp-asl{
   (define-struct asteroids (velocity)
     #:methods
     (circle 15 "solid" "brown"))
   })

  (check-define-struct-message
   @string-append{
   define-struct: expected procedure definitions such as (define (method-id arg-id ...) body-expr) or (define method-id (lambda (arg-id ...) body-expr)) after the #:methods keyword, but found something else: (define render (circle 15 "solid" "brown"))
   }
   @run-in-htdp-asl{
   (define-struct asteroids (velocity)
     #:methods
     (define render
       (circle 15 "solid" "brown")))
   })

  (check-define-struct-message
   @string-append{
   define-struct: the method to_string should have at least one parameter, but there is none
   }
   @run-in-htdp-asl{
   (define-struct U ()
     #:methods
     (define (to_string) "U"))
   })

  (check-define-struct-message
   @string-append{
   define-struct: the method to_string should have at least one parameter, but there is none
   }
   @run-in-htdp-asl{
   (define-struct U ()
     #:methods
     (define to_string (lambda () "U")))
   })

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Error messages about the #:methods syntax part
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (check-define-struct-message
   @string-append{
   define-struct: expected either nothing or method declarations, but found 1 extra part.
    Did you forget to put #:methods before the first method?
   }
   @run-in-htdp-asl{
   (define-struct U (x)
     (define (to_string u) "(U)"))
   })

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Duplicate fields and duplicate methods
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (check-define-struct-message
   @string-append{
   define-struct: found a field name that is used more than once: y
   }
   @run-in-htdp-asl{
   (define-struct S (x))
   (define-struct (T S) (y y)
     #:methods
     (define (get-type t) 'T))
   })

  (check-define-struct-message
   @string-append{
    define-struct: found a method that is defined more than once: radius
   }
   @run-in-htdp-asl{
   (require 2htdp/image)
   (define-struct V (x)
     #:methods
     (define (radius p)
       (circle 15 "solid" "blue"))
     (define (to_string v) (format "(V ~a)" (to_string (make-V 123))))
     (define (radius p)
       15))
   })




  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Error messages about not matching structure specifications
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; fields
  (check-define-struct-message
   @string-append{
   define-struct: the structure X has missing fields: i
   }
   @run-in-htdp-asl{
   (define-struct-name-specification X
     #:fields (i j)
     #:override-or-new-methods ()
     #:allowed-methods ())

   (define-struct X (j))
   })

  (check-define-struct-message
   @string-append{
   define-struct: the structure X has missing fields: j
   }
   @run-in-htdp-asl{
   (define-struct-name-specification X
     #:fields (i j)
     #:override-or-new-methods ()
     #:allowed-methods ())

   (define-struct X (k i))
   })

  (check-define-struct-message
   @string-append{
   define-struct: the structure X has extra fields: k
   }
   @run-in-htdp-asl{
   (define-struct-name-specification X
     #:fields (i j)
     #:override-or-new-methods ()
     #:allowed-methods ())

   (define-struct X (i j k))
   })

  (check-define-struct-message
   @string-append{
   define-struct: expected the structure X with fields: i, j and k
     given the fields with a different order: j, k and i
   }
   @run-in-htdp-asl{
   (define-struct-name-specification X
     #:fields (i j k)
     #:override-or-new-methods ()
     #:allowed-methods ())

   (define-struct X (j k i))
   })

  ;; super structs
  (check-define-struct-message
   @string-append{
   define-struct: expected a structure definition without any super struct
     given super struct: S
   }
   @run-in-htdp-asl{
   (define-struct-name-specification X
     #:fields (n)
     #:override-or-new-methods ()
     #:allowed-methods ())

   (define-struct S ())
   (define-struct (X S) (n))
   })

  (check-define-struct-message
   @string-append{
   define-struct: expected a structure definition with the super struct S
     given: no super struct
   }
   @run-in-htdp-asl{
   (define-struct-name-specification X
     #:super S
     #:fields (n)
     #:override-or-new-methods ()
     #:allowed-methods ())

   (define-struct S ())
   (define-struct X (n))
   })

  (check-define-struct-message
   @string-append{
   define-struct: expected a structure definition with the super struct S
     given: Z
   }
   @run-in-htdp-asl{
   (define-struct-name-specification X
     #:super S
     #:fields (n)
     #:override-or-new-methods ()
     #:allowed-methods ())

   (define-struct S ())
   (define-struct Z ())
   (define-struct (X Z) (n))
   })

  ;; unspecified structs (could be a typo) or unspecified methods
  (check-define-struct-output
   @string-append{
   define-struct warning: expected a structure definition specified in the assignment
     given: (define-struct X (a b c) ...)
   }
   @run-in-htdp-asl[#:error-output (current-output-port)]{
   (check-define-struct:warn-unspecified-struct? #t)
   (define-struct X (a b c))
   })

  (check-define-struct-output
   @string-append{
   define-struct warning: expected a method specified in the assignment
     given: get-name
   }
   @run-in-htdp-asl[#:error-output (current-output-port)]{
   (check-define-struct:warn-not-allowed-method? #t)
   (define-struct-name-specification X
     #:fields (i j)
     #:override-or-new-methods ()
     #:allowed-methods (to_string))

   (define-struct X (i j)
     #:methods
     (define (get-name x) 'X))
   })
  )

;; Prepends the program with #lang htdp/asl and (require "define_super_struct.rkt")
(define (run-in-htdp-asl #:error-output [error-output #f] . strs)
  (parameterize ([current-directory here]
                 [current-load-relative-directory here]
                 [sandbox-make-code-inspector current-code-inspector]
                 [sandbox-path-permissions
                  (list* (list 'read (find-system-path 'pref-file))
                         (list 'read here)
                         (list 'exists (current-directory))
                         (append
                          (filter
                           values
                           (for/list ([path (in-list (ssl-default-verify-sources))])
                             (cond
                               [(path-string? path)
                                (list 'exists path)]
                               [(and (pair? path) (equal? 'directory (car path)))
                                (list 'exists (cadr path))]
                               [else #f])))
                          (sandbox-path-permissions)))]
                 [sandbox-error-output (or error-output (sandbox-error-output))]
                 [sandbox-memory-limit 256]
                 [sandbox-eval-limits '(10 256)])
    (make-module-evaluator
     (with-module-reading-parameterization
       (λ () (read-syntax "define_super_struct_tests.rkt"
                          (open-input-string
                           @string-append*{
 #lang htdp/asl
 (require "struct-with-methods.rkt")
 @strs
 })))))))
;; Run tests in ASL with require paths relative to the current file
(define-runtime-path here ".")
