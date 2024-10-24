#lang racket/base

(require racket/match
         2htdp/image
         (only-in pict pict->bitmap inset panorama [scale pict:scale])
         pict/tree-layout
         racket/undefined
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre))

(provide draw-binary-tree
         check-binary-tree)


;; If you need to rename the struct fields, find the form `define-global-free-variables`
;; and select "Rename XXX" from the right-click context menu.


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
;;; Tree image library, for tutorials
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Unfortunately, #lang htdp/isl+ cannot print pict?s so we have to
;; convert it to bitmaps at the price of losing image quality
(define (pict->2htdp p)
  (pict->bitmap (inset (panorama p) 5)))

(define (connect-layout layout)
  (and layout
       (tree-edge layout
                  #:edge-width 2)))

(define (tree->layout T
                      #:node? node?
                      #:node->pict node->pict
                      #:leaf->pict [leaf->pict (λ (T) #f)]
                      #:node-children node-children)
  (define (do-convert-to-layout T)
    (cond [(node? T)
           (apply tree-layout
                  #:pict (node->pict T)
                  (for/list ([child (in-list (node-children T))])
                    (connect-layout (do-convert-to-layout child))))]
          [else (leaf->pict T)]))
  (do-convert-to-layout T))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing ancestry tree and binary search tree
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-global-free-variables
  branch? branch-number branch-left branch-right)

(define/implicit-parameter (node->pict p)
  #:freevars ()
  (define-values (label data)
    (if (branch? p)
        (values "branch" (branch-number p))
        (values "leaf" p)))
  (define node-text
    (above
     (text (format "~a" data) 18 "dark magenta")
     (text label 12 "magenta")))
  (define radius
    (quotient (+ 20 (max (image-width node-text) (image-height node-text)))
              2))
  (overlay node-text
           (circle radius "outline" "indigo")
           (circle radius "solid" "white")))

(define/implicit-parameter (draw-binary-tree T)
  #:freevars ()
  (let ([who 'draw-binary-tree])
    (check-binary-tree T))
  (pict->2htdp
   (naive-layered
    (tree->layout T
                  #:node? branch?
                  #:node->pict (λ (T) (node->pict T))
                  #:leaf->pict (λ (T) (tree-layout #:pict (node->pict T)))
                  #:node-children (λ (T)
                                    (list (branch-left T)
                                          (branch-right T)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Validating binary trees
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A binary-tree is one of...
;; *  number
;; *  (make-branch number binary-tree binary-tree)
#;
(define-struct branch [number left right])

(define/implicit-parameter (check-binary-tree T)
  #:freevars ([who 'check-binary-tree])
  (define (do-check-binary-tree v)
    (match v
      [(? number?) #true]
      [(? branch? nod)
       (unless (number? (branch-number nod))
         (raise-argument-error who
                               (format (string-append ":\n  "
                                                      "a number in the number field\n"
                                                      "  at: ~e")
                                       nod)
                               T))
       (and (do-check-binary-tree (branch-left nod))
            (do-check-binary-tree (branch-right nod)))]
      [v
       (raise-argument-error who
                             (format (string-append ":\n  "
                                                    "a binary tree: either a number or "
                                                    "a branch object\n"
                                                    "  at: ~e")
                                     v)
                             T)]))
  (do-check-binary-tree T))
