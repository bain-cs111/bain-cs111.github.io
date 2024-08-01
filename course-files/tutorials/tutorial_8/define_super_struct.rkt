#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/set
                     racket/string
                     racket/match
                     racket/syntax
                     syntax/parse/pre)
         racket/undefined
         (only-in lang/htdp-advanced
                  [define asl:define]
                  [lambda asl:lambda]
                  [λ asl:λ]
                  [define-struct asl:define-struct]
                  [set! asl:set!]))

(provide
 ;; Augmented define-struct that supports methods
 (rename-out
  [define-struct/methods define-struct])

 ;; Structure method library
 (for-syntax struct:struct-method-info
             struct-method-info
             struct-method-info?
             struct-method-info-name
             struct-method-info-dispatch-id
             struct-method-info-prop:-id
             struct-method-info-has?-id
             struct-method-info--ref-id)
 (struct-out dynamic-dispatcher)
 define-struct-method-if-not-exists

 ;; Simple structure name specification library
 define-struct-name-specification
 check-define-struct:warn-not-allowed-method?
 check-define-struct:warn-unspecified-struct?
 (for-syntax struct-name->struct-spec-name
             check-struct-name-specification-except-methods

             struct:struct-spec-info
             struct-spec-info
             struct-spec-info?
             struct-spec-info-name
             struct-spec-info-super
             struct-spec-info-fields
             struct-spec-info-override-or-new-methods
             struct-spec-info-allowed-methods)

 ;; Basic uni-directional functor & implicit parameter library
 define/implicit-parameter
 define-global-free-variables
 define/linking
 mutable-variable-pack
 struct-spec-pack/defaults ;; note: struct-spec-pack/defaults does not link methods
 )

;; If you need to rename the struct fields, find the form `define-global-free-variables`
;; and select "Rename XXX" from the right-click context menu.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Structure Method Library
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-struct-method-if-not-exists stx)
  (unless (memq (syntax-local-context) '(top-level module))
    (raise-syntax-error
     'define-or-register-struct-method-properties
     "this form is only available in top-level context or module context"
     stx))
  (syntax-parse stx
    [(_ method-name:id (~optional (~seq (~datum #:default) default-expr)))
     #:with (method-property-name
             method-dispatcher
             prop-method
             has-method?
             method-ref)
     (list (format-symbol "~a-method" (syntax-e #'method-name))
           (format-id #'here "~a-method-dispatcher" #'method-name)
           (format-id #'here "prop:~a-method" #'method-name)
           (format-id #'here "has-~a-method?" #'method-name)
           (format-id #'here "~a-method-ref" #'method-name))
     (define method-info/#f
       (syntax-local-value #'method-name (λ () #f)))
     (cond
       [(struct-method-info? method-info/#f)
        #`(begin)]
       [else
        #`(begin
            (define-values (prop-method has-method? method-ref)
              (make-struct-type-property 'method-property-name))
            (define method-dispatcher
              (dynamic-dispatcher 'method-name
                                  has-method?
                                  method-ref
                                  (~? default-expr '#f)))
            (define-syntax method-name
              (struct-method-info 'method-name
                                  (quote-syntax method-dispatcher)
                                  (quote-syntax prop-method)
                                  (quote-syntax has-method?)
                                  (quote-syntax method-ref))))])]))

(struct dynamic-dispatcher (name has? -ref default)
  #:property prop:object-name (struct-field-index name)
  #:property prop:procedure
  (λ (dispatch-info self-arg . other-args)
    (define method-impl
      (check-and-extract-method dispatch-info self-arg))
    (apply method-impl self-arg other-args)))

(define (check-and-extract-method dispatch-info self-arg)
  (define has-method? (dynamic-dispatcher-has? dispatch-info))
  (define method-ref (dynamic-dispatcher--ref dispatch-info))
  (cond
    [(has-method? self-arg) (unbox (method-ref self-arg))]
    [(dynamic-dispatcher-default dispatch-info) => values]
    [else
     (define method-name (dynamic-dispatcher-name dispatch-info))
     (raise-argument-error
      method-name
      (format "a structure that defines the method '~a'"
              method-name)
      self-arg)]))

(begin-for-syntax
  (struct struct-method-info (name dispatch-id prop:-id has?-id -ref-id)
    #:property prop:procedure
    (λ (method-info stx)
      (with-syntax ([name (struct-method-info-name method-info)]
                    [dispatcher (struct-method-info-dispatch-id method-info)])
        (syntax-parse stx
          [method:id
           (with-disappeared-uses (record-disappeared-uses #'method)
             #'dispatcher)]
          [(method:id)
           (raise-syntax-error (syntax-e #'name)
                               (string-append
                                "structure methods expect at least one argument "
                                "for dynamic dispatching, but given none")
                               stx)]
          [(method:id self-arg other-arg:expr ...)
           (with-disappeared-uses (record-disappeared-uses #'method)
             (quasisyntax/loc stx
               (let* ([self self-arg]
                      [method-impl (check-and-extract-method dispatcher self)])
                 #,(quasisyntax/loc stx
                     (method-impl self other-arg ...)))))])))
    #:transparent)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simple Struct Declaration Library
;;;
;;;   Specify field names, super struct information and method names.
;;;
;;;   For simplicity, the implementation simply compares the *symbols* of
;;;   the names instead of the bindings because the struct spec and the
;;;   struct definition may be separated in different modules.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define check-define-struct:warn-not-allowed-method? (make-parameter #f))
(define check-define-struct:warn-unspecified-struct? (make-parameter #f))

(define-for-syntax (struct-name->struct-spec-name stx #:context [ctxt stx])
  (format-id ctxt "struct-spec:~a" stx))

(begin-for-syntax
  (struct struct-spec-info (name super fields override-or-new-methods allowed-methods)
    #:transparent)
  )

(define-syntax (define-struct-name-specification stx)
  (syntax-parse stx
    [(form struct-name:id
           (~optional
            (~seq (~datum #:super) super-name:id))
           (~datum #:fields) (field-name:id ...)
           (~datum #:override-or-new-methods) (override-method-name:id ...)
           (~datum #:allowed-methods) (allowed-method-name:id ...))
     #:with struct-spec (struct-name->struct-spec-name #'struct-name #:context #'form)
     (quasisyntax/loc stx
       (define-syntax struct-spec
         (struct-spec-info 'struct-name
                           '(~? super-name #f)
                           '(field-name ...)
                           '(override-method-name ...)
                           '(allowed-method-name ...))))]))

(define (warn-not-allowed-method method-name in-allowed-list? continuation-marks)
  (when (and (check-define-struct:warn-not-allowed-method?)
             (not in-allowed-list?))
    (define edisplay (error-display-handler))
    (define msg
      (format (string-append
               "define-struct warning: expected a method specified in the assignment\n"
               "  given: ~a")
              method-name))
    (edisplay
     msg
     (exn:fail:contract msg continuation-marks))))

(define (warn-unspecified-struct elided-struct-definition continuation-marks)
  (when (check-define-struct:warn-unspecified-struct?)
    (define edisplay (error-display-handler))
    (define msg
      (format (string-append
               "define-struct warning: expected a structure definition "
               "specified in the assignment\n"
               "  given: ~a")
              elided-struct-definition))
    (edisplay
     msg
     (exn:fail:contract msg continuation-marks))))

(define-for-syntax (check-struct-name-specification-except-methods spec struct-stx)
  (define/syntax-parse (_ (~and name-or-super-form
                                (~or struct-name:id
                                     (struct-name:id super-name:id)))
                          fields . rest)
    struct-stx)
  (unless (eq? (syntax-e #'struct-name) (struct-spec-info-name spec))
    (raise-argument-error
     'check-struct-name-specification-except-methods
     "a struct definition that has the same name as the specification"
     1
     spec
     struct-stx))
  (cond
    [(struct-spec-info-super spec)
     (define spec-super-name (struct-spec-info-super spec))
     (when (or (not (attribute super-name))
               (not (eq? (syntax-e #'super-name) spec-super-name)))
       (raise-syntax-error 'define-struct
                           (format (string-append
                                    "expected a structure definition with "
                                    "the parent type `~a`\n  given: ~a")
                                   spec-super-name
                                   (syntax-e #'(~? super-name "no parent type")))
                           struct-stx
                           #'name-or-super-form))]
    [else
     (when (attribute super-name)
       (raise-syntax-error 'define-struct
                           (format (string-append
                                    "expected a structure definition without "
                                    "any parent type\n  given parent type: ~a")
                                   (syntax-e #'super-name))
                           struct-stx
                           #'name-or-super-form))])
  (define expected-field-syms
    (struct-spec-info-fields spec))
  (define field-syms
    (map syntax-e (syntax->list #'fields)))
  (when (not (equal? expected-field-syms field-syms))
    (define expected-field-syms-set (list->set expected-field-syms))
    (define field-syms-set (list->set field-syms))
    (cond
      [(not
        (set-empty?
         (set-subtract expected-field-syms-set
                       field-syms-set)))
       (raise-syntax-error
        'define-struct
        (format "the structure ~a has missing fields: ~a"
                (syntax-e #'struct-name)
                (string-join
                 (set-map (set-subtract expected-field-syms-set field-syms-set)
                          symbol->string)
                 ", "
                 #:before-last " and "))
        struct-stx
        #'fields)]
      [(not
        (set-empty?
         (set-subtract field-syms-set
                       expected-field-syms-set)))
       (raise-syntax-error
        'define-struct
        (format "the structure ~a has extra fields: ~a"
                (syntax-e #'struct-name)
                (string-join
                 (set-map (set-subtract field-syms-set expected-field-syms-set)
                          symbol->string)
                 ", "
                 #:before-last " and "))
        struct-stx
        #'fields)]
      [else
       ;; In this case, both expected-field-syms and field-syms are non-empty
       (raise-syntax-error
        'define-struct
        (format (string-append
                 "expected the structure ~a with fields: ~a\n"
                 "  given the fields with a different order: ~a")
                (syntax-e #'struct-name)
                (string-join
                 (map symbol->string expected-field-syms)
                 ", "
                 #:before-last " and ")
                (string-join
                 (map symbol->string field-syms)
                 ", "
                 #:before-last " and "))
        struct-stx
        #'fields)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic implicit parameters
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
    (define/implicit-parameter (function-id arg-id ...)
      #:freevars (freevar-spec ...)
      body-expr ...+)

    freevar-spec ::= freevar-id
                  |  [freevar-id default-expr]

    The function function-id is converted in to a curried function
    that first takes freevar-id ... as its parameter.

    The implicit parameters, freevar-id ..., will be instantiated
    and supplied unhygienically at each use site of function-id.
    If free-var-id is not defined at use site, default-expr is used instead.
|#
(define-for-syntax global-free-variables '())
(define-syntax (define-global-free-variables stx)
  (syntax-parse stx
    [(_ free-var:id ...+)
     ;; FIXME: NO ERROR CHECK!
     (set! global-free-variables
           (append (map syntax-local-introduce (syntax->list #'(free-var ...)))
                   global-free-variables))
     #'(begin)]))
(define-syntax (define/implicit-parameter stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...)
        (~datum #:freevars)
        [(~or* free-var:id [free-var:id default-expr:expr]) ...]
        body-expr ...+)
     #:with name/parameterized (format-id #'here "~a/parameterized" #'name)
     #:with name/defaults (format-id #'here "~a/defaults" #'name)
     #:with (global-free-var ...) (map syntax-local-introduce global-free-variables)
     #:with (global-free-var-no-default ...)
     (for/list ([global-free-var (in-list global-free-variables)])
       #'#f)
     #:with (has-default? ...)
     (for/list ([def-val (in-list (syntax-e #'((~? default-expr #f) ...)))])
       #`#,(and (syntax-e def-val) #t))
     #:with (free-var/undef ...) (generate-temporaries #'(free-var ...))
     #`(begin
         (define name/defaults (list (~? default-expr undefined) ...))
         (define (name/parameterized global-free-var ... free-var/undef ...)
           (let-values ([(free-var ...) (filter-undefs name/defaults free-var/undef ...)])
             (let ([name
                    #,(quasisyntax/loc stx
                        (λ (arg ...)
                          body-expr ...))])
               name)))
         #,(quasisyntax/loc stx
             (define-syntax name
               (function/implicits
                #'name/parameterized
                (list 'global-free-var ... 'free-var ...)
                '(global-free-var-no-default ... has-default? ...)))))]))
(define (filter-undefs default-values . vals)
  (apply values
         (for/list ([def-val (in-list default-values)]
                    [val (in-list vals)])
           (if (eq? val undefined)
               def-val
               val))))
(define-syntax (#%top stx)
  (define id (cdr (syntax-e stx)))
  (raise-syntax-error (syntax-e id)
                      (string-append
                       "unbound identifier\n"
                       " Are you using a function with free variables "
                       "(i.e. defined by define/implicit-parameter) "
                       "but the free variables are not defined yet?\n"
                       " To define another function that uses functions with free variables,"
                       " try replacing define with define/implicit-parameter.")
                      id))
(begin-for-syntax
  (struct function/implicits (procedure-id freevars-syms has-default?s)
    #:property prop:procedure
    (λ (implicit-info stx)
      (define proc-stx (function/implicits-procedure-id implicit-info))
      (define freevars-syms (function/implicits-freevars-syms implicit-info))
      (define has-default?s (function/implicits-has-default?s implicit-info))
      (define freevars-ids
        (for/list ([freevar-sym (in-list freevars-syms)]
                   [has-default? (in-list has-default?s)])
          (define id (datum->syntax stx freevar-sym stx))
          (if (or (not has-default?) (identifier-binding id))
              id
              #'undefined)))
      (with-syntax ([proc proc-stx]
                    [(freevar ...) freevars-ids])
        (syntax-parse stx
          [name:id
           (with-disappeared-uses (record-disappeared-uses #'name)
             (quasisyntax/loc stx
               (proc freevar ...)))]
          [(name:id arg:expr ...)
           (with-disappeared-uses (record-disappeared-uses #'name)
             (quasisyntax/loc stx
               (let ([instantiated-proc (proc freevar ...)])
                 #,(quasisyntax/loc stx
                     (instantiated-proc arg ...)))))])))
    #:transparent)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic units/functors and implicit parameters
;;;
;;; like define/implicit-parameter except that the free variables are
;;; specified through either the mutable-variable-pack form or the
;;; struct-spec-pack/defaults form.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define/linking stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...)
        (~datum #:links) (pack:expr ...)
        body-expr ...+)
     #:with proc-name (format-id #'here "~a-impl" #'name)
     #:with (pack-arg ...) (generate-temporaries #'(pack ...))
     #`(begin
         (define-syntax (name usesite-stx)
           (syntax-parse usesite-stx
             [_:id
              (quasisyntax/loc usesite-stx
                (proc-name
                 (add-use-site #,#'#,usesite-stx pack)
                 ...))]
             [(form:id . args)
              #:with app (datum->syntax usesite-stx '#%app)
              (quasisyntax/loc usesite-stx
                (app form . args))]))
         #,(quasisyntax/loc stx
             (define (proc-name pack-arg ...)
               (unpack-links pack-arg pack)
               ...
               (let ([name
                      (λ (arg ...)
                        body-expr ...)])
                 name))))]))

(begin-for-syntax
  (struct link-pack (use-site-transformer unpack-site-transformer)
    #:property prop:procedure (struct-field-index use-site-transformer)
    #:transparent)
  )

(define-syntax mutable-variable-pack
  (link-pack
   (λ (stx)
     (syntax-parse stx
       [(_ use-site mutable-var:id)
        #:with use-site-mutable-var
        (syntax-property
         (datum->syntax #'use-site (syntax-e #'mutable-var))
         'original-for-check-syntax #t #t)
        #:with (mutable-var-ref set-mutable-var!)
        (list (format-id #'here "~a-ref" #'mutable-var)
              (format-id #'here "set-~a!" #'mutable-var))
        (syntax/loc stx
          (list (let ([mutable-var-ref (λ () use-site-mutable-var)])
                  mutable-var-ref)
                (let ([set-mutable-var! (λ (new-value)
                                          (set! use-site-mutable-var new-value))])
                  set-mutable-var!)))]))
   (λ (stx)
     (syntax-parse stx
       [(_ from-arg:id mutable-var:id)
        #'(begin
            (define -ref-proc (car from-arg))
            (define set!-proc (cadr from-arg))
            (define-syntax mutable-var
              (make-set!-transformer
               (λ (var-stx)
                 (syntax-parse var-stx
                   #:literals (set! asl:set!)
                   [var:id
                    (syntax/loc var-stx (-ref-proc))]
                   [((~and form (~or set! asl:set!)) var:id new-value:expr)
                    (with-disappeared-uses (record-disappeared-uses #'form)
                      (syntax/loc var-stx
                        (set!-proc new-value)))])))))]))))

(define-for-syntax (struct-name->binding-names struct-name)
  (define struct-spec
    (syntax-local-value (struct-name->struct-spec-name struct-name)))
  (define struct-sym
    (struct-spec-info-name struct-spec))
  (define field-syms
    (struct-spec-info-fields struct-spec))
  (append
   (list struct-sym
         (format-symbol "make-~a" struct-sym)
         (format-symbol "~a?" struct-sym))
   (for/list ([field-sym (in-list field-syms)])
     (format-symbol "~a-~a" struct-sym field-sym))
   (for/list ([field-sym (in-list field-syms)])
     (format-symbol "set-~a-~a!" struct-sym field-sym))))

(define-syntax struct-spec-pack/defaults
  (link-pack
   (λ (stx)
     (syntax-parse stx
       [(_ use-site struct-name:id)
        (define struct-binding-names
          (struct-name->binding-names #'struct-name))
        (quasisyntax/loc stx
          (list
           #,@(for/list ([binding-name (in-list struct-binding-names)])
                (define use-site-struct-operation
                  (syntax-property
                   (datum->syntax #'use-site binding-name)
                   'original-for-check-syntax #t #t))
                (cond
                  [(identifier-binding use-site-struct-operation)
                   use-site-struct-operation]
                  [else #''#f]))))]))
   (λ (stx)
     (syntax-parse stx
       [(_ from-arg:id struct-name:id)
        (define struct-binding-names
          (struct-name->binding-names #'struct-name))
        (quasisyntax/loc stx
          (define-values
            #,(for/list ([binding-name (in-list struct-binding-names)])
                (datum->syntax #'struct-name binding-name))
            (apply values from-arg)))]))))

(define-syntax (add-use-site stx)
  (syntax-parse stx
    [(_ add-use-site pack:id)
     (datum->syntax #'add-use-site
                    (syntax-e #'pack))]
    [(_ add-use-site (pack:id arg ...))
     (syntax/loc stx
       (pack add-use-site arg ...))]))

(define-syntax (unpack-links stx)
  (syntax-parse stx
    [(_ from-arg:id pack:id)
     (quasisyntax/loc stx
       (define pack from-arg))]
    [(_ from-arg:id (pack:id arg ...))
     #:when (link-pack? (syntax-local-value #'pack (λ () #f)))
     (define link-pack
       (syntax-local-value #'pack (λ () #f)))
     (define unpack-result
       ((link-pack-unpack-site-transformer link-pack)
        (syntax/loc stx
          (pack from-arg arg ...))))
     unpack-result]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation and helper functions for define-struct (with methods)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
   check-and-extract-define-struct-methods : syntax? -> (or/c #f (listof syntax?))

     Checks if the supplied syntax conforms the grammar of define-struct (with methods).
     If there are syntax errors, either raise an exception or evaluate to #f.
     If the syntax is correct, extracts and return the method definitions.

     Modified from the implementation of Advanced Student Language
     htdp-lib, lang/private/teach, do-define-struct
|#
(define-for-syntax (check-and-extract-define-struct-methods stx)
  (unless (or (memq (syntax-local-context) '(top-level module))
              (identifier? stx))
    (raise-syntax-error
     'define-struct
     "found a definition that is not at the top level"
     stx
     #f))

  (syntax-case stx ()
    ;; First, check for a struct name:
    [(_ name-or-super . __)
     ;; Should check that name is not an identifier but well
     (not
      (syntax-parse (syntax name-or-super)
        [name:id #t]
        [(name:id super-name:id) #t]
        [_else #f]))
     (raise-syntax-error
      'define-struct
      (format
       (string-append
        "expected the structure name with one optional super-struct "
        "name after define-struct, but found something else: ~s")
       (syntax->datum (syntax name-or-super)))
      stx
      (syntax name-or-super))]
    ;; Main case (`rest' is for nice error messages):
    [(_ name-or-super (field_ ...) . rest)
     (let* ([fields (syntax->list (syntax (field_ ...)))]
            [ht     (make-hash)]
            [rest (syntax->list (syntax rest))]
            [methods (match rest
                       ['() '()]
                       [(cons (and (? syntax?) (app syntax-e '#:methods))
                              methods)
                        methods]
                       [_else #f])])

       ;; are all fields names?
       (for-each
        (lambda (field)
          (unless (identifier? field)
            (raise-syntax-error
             'define-struct
             (format "expected a field name, but found something else: ~s"
                     (syntax->datum field))
             stx
             field))
          (let ([sym (syntax-e field)])
            (when (hash-ref ht sym (lambda () #f))
              (raise-syntax-error
               'define-struct
               (format
                "found a field name that is used more than once: ~a"
                sym)
               stx
               field))
            (hash-set! ht sym #t)))
        fields)

       ;; are there methods following the field names?
       (unless methods
         (raise-syntax-error
          'define-struct
          (format
           (string-append
            "expected either nothing or method declarations, but found ~a extra part~a.\n"
            " Did you forget to put #:methods before the first method?")
           (length rest)
           (if (> (length rest) 1) "s" ""))
          stx
          (car rest)))

       ;; New code: checking method definitions
       (define method-ht (make-hash))
       (define (add-a-method! method-name-sym a-method-stx)
         (when (hash-has-key? method-ht method-name-sym)
           (raise-syntax-error
            'define-struct
            (format "found a method that is defined more than once: ~a" method-name-sym)
            stx
            a-method-stx
            (list (hash-ref method-ht method-name-sym))))
         (hash-set! method-ht method-name-sym a-method-stx))
       (define extracted-methods
         (filter-map
          (lambda (a-method-stx)
            (syntax-parse a-method-stx
              #:literals (asl:define asl:lambda asl:λ)
              ;; (define (method-id self-arg-id other-arg-id ...) body-expr), or
              ;; (define method-id (lambda (self-arg-id other-arg-id ...) body-expr))
              [(asl:define . (~or ((~and (method-name:id) method-hdr) . body-exprs)
                                  (method-name:id
                                   (asl:lambda (~and () method-hdr) . body-exprs))
                                  (method-name:id
                                   (asl:λ (~and () method-hdr) . body-exprs))))
               (raise-syntax-error
                'define-struct
                (format
                 (string-append
                  "the method ~a should have at least one parameter, "
                  "but there is none")
                 (syntax-e (syntax method-name)))
                stx
                #'method-hdr)]
              [(asl:define (method-name:id self-arg:id arg:id ...) body-expr ...)
               (add-a-method! (syntax-e #'method-name) a-method-stx)
               a-method-stx]
              [(asl:define method-name:id
                           ((~or asl:lambda asl:λ) (self-arg:id arg:id ...)
                                                   body-expr ...))
               (add-a-method! (syntax-e #'method-name) a-method-stx)
               a-method-stx]
              [_else
               (raise-syntax-error
                'define-struct
                (format
                 (string-append
                  "expected procedure definitions such as "
                  "(define (method-id arg-id ...) body-expr) or "
                  "(define method-id (lambda (arg-id ...) body-expr)) "
                  "after the #:methods keyword, but found something else: ~s")
                 (syntax->datum a-method-stx))
                stx
                a-method-stx)]))
          methods))

       extracted-methods)]
    [(_ name-or-super fields_ . rest)
     (raise-syntax-error
      'define-struct
      (format
       (string-append
        "expected field names (in parenthesis) after the structure name, "
        "but found something else: ~s"
        (if (equal? (syntax-e (syntax fields_)) '#:methods)
            "\n If the structure has no (extra) fields, put an empty pair of parenthese there."
            ""))
       (syntax->datum (syntax fields_)))
      stx
      (syntax fields_))]
    [(_ name-or-super)
     (raise-syntax-error
      'define-struct
      (string-append
       "expected field names (in parenthesis) after the structure name, "
       "but nothing's there."
       "\n If the structure has no (extra) fields, put an empty pair of parenthese there.")
      stx)]
    [_else
     #f]))

(define-for-syntax (parse-struct-method-name a-method-stx)
  (syntax-case a-method-stx ()
    [(_define method-name . exprs)
     (identifier? #'method-name)
     #'method-name]
    [(_define (method-name . args) . exprs)
     (identifier? #'method-name)
     #'method-name]))

(define-for-syntax (parse-and-transform-struct-method spec-info/#f a-method-stx)
  (syntax-parse a-method-stx
    #:literals (asl:define asl:lambda asl:λ)
    [(asl:define . (~or ((method-name:id arg:id ...+) body-expr ...)
                        (method-name:id
                         ((~or asl:λ asl:lambda) (arg:id ...+) body-expr ...))))
     #:with local-method-name (format-id #'here "method-impl-~a" #'method-name)
     #:with uninitialized-local-method (format-id #'here "uninitialized:~a" #'method-name)
     (define warn-method-stx
       (cond
         [(not spec-info/#f) '()]
         [else
          (list
           #`(warn-not-allowed-method
              'method-name
              '#,(and (member (syntax-e #'method-name)
                              (struct-spec-info-allowed-methods spec-info/#f))
                      #t)
              #,(syntax-property
                 (quasisyntax/loc a-method-stx
                   (current-continuation-marks))
                 'errortrace:annotate #t #t)))]))
     (values #'local-method-name
             #'(define local-method-name (box 'uninitialized-local-method))
             (syntax-property
              #`(set-box! local-method-name
                          (let ([_ (if #t #f method-name)]
                                [method-name
                                 #,(syntax-property
                                    (syntax/loc a-method-stx
                                      (asl:lambda (arg ...) body-expr ...))
                                    'errortrace:annotate #t #t)])
                            #,@warn-method-stx
                            method-name))
              'disappeared-binding
              (list (syntax-local-introduce #'method-name))))]))

#|
   Structure definitions with methods. The methods are dispatched
   using the struct type of the first parameter.

         (define-struct name-or-name-with-super (field-id ...)
           maybe-methods)

         name-or-name-with-super ::= name-id
                                  |  (name-id super-id)

         maybe-methods ::=
                        |  #:methods method-form ...

         method-form   ::= (define method-id (lambda (self-id arg-id ...) body-expr)
                        |  (define (method-id self-id arg-id ...) body-expr)
|#
(define-syntax (define-struct/methods stx)
  (syntax-case stx ()
    [(_ name-or-super . fields-and-rest)
     ;; After this point, check-and-extract-define-struct-methods guarantees
     ;; that `name-or-super` is `name:id` or `(name:id super-name:id)` and that
     ;; `fields` is `(field:id ...)`.
     (cond
       [(check-and-extract-define-struct-methods stx)
        =>
        (λ (methods-stxs)
          (define method-names-stxs
            (for/list ([a-method-stx (in-list methods-stxs)])
              (parse-struct-method-name a-method-stx)))
          (with-syntax ([(method-name ...) method-names-stxs])
            #`(begin
                (define-struct-method-if-not-exists method-name)
                ...
                ;; Use trampoline to bring the definitions of methods
                ;; (method-name ...), properties, names, etc. in scope
                #,(quasisyntax/loc stx
                    (define-struct/methods-impl name-or-super . fields-and-rest)))))]
       [else
        (syntax/loc stx
          (asl:define-struct name-or-super . fields-and-rest))])]
    [(_ form ...)
     (syntax/loc stx
       (asl:define-struct form ...))]
    [_else
     (raise-syntax-error
      'define-struct
      "bad syntax: try placing define-struct after an open parenthesis"
      stx)]))

(define-syntax (define-struct/methods-impl stx)
  (define/syntax-parse (_ name-or-super fields . rest)
    stx)
  (define/syntax-parse (~or struct-name:id (struct-name:id _))
    #'name-or-super)
  (define methods-stxs
    (check-and-extract-define-struct-methods stx))
  (define spec-info-any
    (syntax-local-value (struct-name->struct-spec-name #'struct-name)
                        (λ () #f)))
  (define spec-info/#f
    (and (struct-spec-info? spec-info-any) spec-info-any))
  (when spec-info/#f
    (check-struct-name-specification-except-methods spec-info/#f stx))
  (define warn-unspecified-struct-stx
    (cond
      [spec-info/#f '()]
      [else
       (list
        #`(warn-unspecified-struct
           '(define-struct name-or-super fields (... ...))
           #,(syntax-property
              (quasisyntax/loc stx
                (current-continuation-marks))
              'errortrace:annotate #t #t)))]))

  (define method-names-stxs
    (for/list ([a-method-stx (in-list methods-stxs)])
      (parse-struct-method-name a-method-stx)))
  (define-values (local-method-names-stxs local-method-decls-stxs method-implementations-stxs)
    (for/lists (local-method-names local-method-decls method-impls)
               ([a-method-stx (in-list methods-stxs)])
      (parse-and-transform-struct-method spec-info/#f a-method-stx)))
  (with-syntax ([(local-method-decl ...) local-method-decls-stxs]
                [(method-implementation ...) method-implementations-stxs])
    #`(begin
        local-method-decl
        ...
        #,@warn-unspecified-struct-stx
        #,(quasisyntax/loc stx
            (define-struct name-or-super fields
              #:transparent
              #:mutable
              .
              #,(apply
                 append
                 (for/list ([a-method-name-stx (in-list method-names-stxs)]
                            [a-local-method-name-stx (in-list local-method-names-stxs)])
                   (list
                    #'#:property
                    (struct-method-info-prop:-id
                     (syntax-local-value a-method-name-stx))
                    a-local-method-name-stx)))))
        method-implementation
        ...)))

(module+ test
  (require rackunit)
  (provide (all-defined-out))

  (define-struct/methods my-posn (x y)
    #:methods
    (asl:define (add a b)
                (make-my-posn (+ (my-posn-x a)
                                 (my-posn-x b))
                              (+ (my-posn-y a)
                                 (my-posn-y b)))))

  (check-equal? (add (make-my-posn 1 1) (make-my-posn 2 2))
                (make-my-posn 3 3))

  (check-exn
   exn:fail?
   (λ () (add (make-my-posn 1 1) 6 7)))

  (check-exn
   exn:fail?
   (λ () (add (make-my-posn 8 9))))

  (define-struct/methods (posn3d my-posn) (z)
    #:methods
    (asl:define (add a b)
                (make-posn3d (+ (my-posn-x a)
                                (my-posn-x b))
                             (+ (my-posn-y a)
                                (my-posn-y b))
                             (+ (posn3d-z a)
                                (posn3d-z b)))))

  (check-equal? (add (make-posn3d 1 1 1) (make-posn3d 2 2 2))
                (make-posn3d 3 3 3))

  (check-equal? (add (make-my-posn 1 1) (make-posn3d 2 2 5))
                (make-my-posn 3 3))

  (check-exn
   exn:fail?
   (λ () (add (make-posn3d 2 2 5) (make-my-posn 1 1))))

  (define-struct/methods (posn2 my-posn) ())

  (check-equal? (add (make-posn2 1 1) (make-posn2 2 2))
                (make-my-posn 3 3))

  (check-exn
   exn:fail?
   (λ () (add "a" "b")))
  )
