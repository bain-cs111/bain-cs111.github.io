#lang racket/base

(require racket/match
         2htdp/image
         (only-in pict pict->bitmap inset panorama [scale pict:scale])
         pict/tree-layout
         racket/undefined
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre))

(provide draw-ancestry-tree
         check-ancestry-tree

         draw-faculty
         check-faculty

         draw-muddDB-tree
         check-muddDB-tree)


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
;;; Tree image library
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
                      #:node-children node-children)
  (define (do-convert-to-layout T)
    (cond [(node? T)
           (apply tree-layout
                  #:pict (frame (node->pict T))
                  (for/list ([child (in-list (node-children T))])
                    (connect-layout (do-convert-to-layout child))))]
          [else #f]))
  (do-convert-to-layout T))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing ancestry tree and binary search tree
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-global-free-variables
  person? person-name person-parentA person-parentB
  faculty? faculty-office faculty-name
  muddDB? muddDB-data muddDB-smaller muddDB-larger)

(define/implicit-parameter (human->pict p)
  #:freevars ()
  (define node-text
    (text (person-name p) 16 "black"))
  (define bg
    (empty-scene (+ 8 (image-width node-text)) (+ 8 (image-height node-text))))
  (overlay node-text bg))

(define/implicit-parameter (draw-ancestry-tree T)
  #:freevars ()
  (let ([who 'draw-ancestry-tree])
    (check-ancestry-tree T))
  (pict->2htdp
   (naive-layered (tree->layout T
                                #:node? person?
                                #:node->pict human->pict
                                #:node-children (λ (T)
                                                  (list (person-parentA T)
                                                        (person-parentB T))))
                  #:transform (λ (x y) (values x (- y))))))

(define/implicit-parameter (node-data->pict p)
  #:freevars ()
  (define node-text
    (above
     (text (format "~a" (faculty-office p)) 18 "purple")
     (text (faculty-name p) 12 "black")))
  (define bg
    (empty-scene (+ 10 (image-width node-text)) (+ 10 (image-height node-text))))
  (frame (overlay node-text bg)))

(define/implicit-parameter (draw-muddDB-tree T)
  #:freevars ()
  (let ([who 'draw-muddDB-tree])
    (check-muddDB-tree T))
  (pict->2htdp
   (binary-tidier (tree->layout T
                                #:node? muddDB?
                                #:node->pict (λ (T) (node-data->pict (muddDB-data T)))
                                #:node-children (λ (T)
                                                  (list (muddDB-smaller T)
                                                        (muddDB-larger T)))))))

(define/implicit-parameter (draw-faculty pn)
  #:freevars ()
  (let ([who 'draw-faculty])
    (check-faculty pn))
  (pict->2htdp
   (node-data->pict pn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Validating ancestry tree and binary search tree
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; An ancestry-tree is either...
;; *  -1
;; *  (make-person string ancestry-tree ancestry-tree)
#;
(define-struct person [name parentA parentB])

(define/implicit-parameter (check-ancestry-tree T)
  #:freevars ([who 'check-ancestry-tree])
  (define (do-check-ancestry-tree v)
    (match v
      [-1 #true]
      [(? person? p)
       (unless (string? (person-name p))
         (raise-argument-error who
                               (format ":\n  a string in the name field\n  at: ~e" p)
                               T))
       (and (do-check-ancestry-tree (person-parentA p))
            (do-check-ancestry-tree (person-parentB p)))]
      [v
       (raise-argument-error who
                             (format (string-append ":\n  "
                                                    "an ancestry tree: either -1 or "
                                                    "a person object\n"
                                                    "  at: ~e")
                                     v)
                             T)]))
  (do-check-ancestry-tree T))


;; A faculty is...
;; *  (make-faculty number string)
#;
(define-struct faculty [office name])

(define/implicit-parameter (check-faculty v)
  #:freevars ([who 'check-faculty])
  (unless (faculty? v)
    (raise-argument-error who "a faculty object" v))
  (unless (number? (faculty-office v))
    (raise-argument-error who "a number in the office field" v))
  (unless (string? (faculty-name v))
    (raise-argument-error who "a string in the name field" v))
  #true)


;; A binary-search-tree is one of...
;; *  #false
;; *  (make-muddDB faculty binary-search-tree binary-search-tree)
#;
(define-struct muddDB [data smaller larger])

(define/implicit-parameter (check-muddDB-tree T)
  #:freevars ([who 'check-muddDB-tree])
  (define (do-check-muddDB-tree v)
    (match v
      [#false #true]
      [(? muddDB? nod)
       (define emp (muddDB-data nod))
       (unless (faculty? emp)
         (raise-argument-error who
                               (format (string-append ":\n  "
                                                      "a faculty object in the data field\n"
                                                      "  at: ~e")
                                       nod)
                               T))
       (unless (number? (faculty-office emp))
         (raise-argument-error who
                               (format (string-append ":\n  "
                                                      "a number in the office field\n"
                                                      "  at: ~e")
                                       emp)
                               T))
       (unless (string? (faculty-name emp))
         (raise-argument-error who
                               (format (string-append ":\n  "
                                                      "a string in the name field\n"
                                                      "  at: ~e")
                                       emp)
                               T))
       (and (do-check-muddDB-tree (muddDB-smaller nod))
            (do-check-muddDB-tree (muddDB-larger nod)))]
      [v
       (raise-argument-error who
                             (format (string-append ":\n  "
                                                    "a binary seach tree: either #false or "
                                                    "a muddDB object\n"
                                                    "  at: ~e")
                                     v)
                             T)]))
  (do-check-muddDB-tree T))

#|
;; These are auxiliary code for producing figures in Exercise 4.pdf.
;; They are not part of the solution.
;;
;; In addition, since the tree images are pict?s rather than 2htdp images,
;; these code have to be used in tree_lib.rkt
(provide write-images)
(define/implicit-parameter (write-images)
  #:freevars (kevan martyn samir peter samir-tree)
  (define (add-background img)
    (define w (image-width img))
    (define h (image-height img))
    (place-image img
                 (+ 5 (quotient w 2))
                 (+ 5 (quotient h 2))
                 (rectangle (+ w 10) (+ h 10) "solid" "white")))
  (set! pict->2htdp
        (let ([old-pict->2htdp pict->2htdp])
          (lambda (p)
            (old-pict->2htdp (pict:scale p 2)))))
  (save-image (draw-ancestry-tree kevan) "images/kevan.png")
  (save-image (draw-ancestry-tree martyn) "images/martyn.png")
  (save-image (draw-faculty samir) "images/samir.png")
  (save-image (draw-faculty peter) "images/peter.png")
  (save-image (draw-muddDB-tree samir-tree) "images/muddDB.png")
  (void))
; |#
