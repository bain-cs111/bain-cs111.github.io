#lang racket/base

(require (only-in lang/htdp-intermediate-lambda
                  local member
                  posn? make-posn posn-x posn-y
                  true false empty sqr)
         racket/undefined
         (for-syntax racket/base racket/syntax syntax/parse/pre)
         2htdp/image
         2htdp/universe)

(provide play-game
         board-length)

;; If you need to struct fields, find the form `define-global-free-variables`
;; and select "Rename XXX" from the right-click context menu.
;;
;; To rename functions, find `play-game` and select "Rename XXX" for
;; the free variables declared in the #:freevars list.

(define board-length 50)
(define cell-length (* 2 5))

(define alive-color 'green)
(define dead-color 'red)
(define food-color 'yellow)
(define score-color 'gray)
(define obstacle-color 'purple)
(define board
  (rectangle (* cell-length board-length)
             (* cell-length board-length)
             'solid
             'black))

(define tick-freq 1/20)

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
                (list 'global-free-var ... 'free-var ...)))))]))
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
  (struct function/implicits (procedure-id freevars-syms)
    #:property prop:procedure
    (λ (implicit-info stx)
      (define proc-stx (function/implicits-procedure-id implicit-info))
      (define freevars-syms (function/implicits-freevars-syms implicit-info))
      (define freevars-ids
        (for/list ([freevar-sym (in-list freevars-syms)])
          (define id (datum->syntax stx freevar-sym stx))
          (if (identifier-binding id)
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

(define-global-free-variables
  game-snake game-food game-obstacles game-ticks
  snake-heading snake-segments)

;; random-free-posn : game -> posn
(define/implicit-parameter (random-free-posn g)
  #:freevars ()
  (nth-free-posn (random (num-free-posns g)) (make-posn 1 1) g))

;; num-free-posns : game -> posn
(define/implicit-parameter (num-free-posns g)
  #:freevars ()
  (- (sqr board-length)
     (length (snake-segments (game-snake g)))
     (length (game-food g))))

;; nth-free-posn : nat posn game -> posn
(define/implicit-parameter (nth-free-posn n p g)
  #:freevars ()
  (cond [(zero? n) (next-free-posn p g)]
        [else
         (nth-free-posn (- n 1) (next-posn (next-free-posn p g)) g)]))

;; next-free-posn : posn game -> posn
(define/implicit-parameter (next-free-posn p g)
  #:freevars ()
  (cond [(occupied? p g)
         (next-free-posn
          (make-posn
           (cond [(= (posn-x p) board-length) 1]
                 [else (+ (posn-x p) 1)])
           (cond [(= (posn-x p) board-length) (+ (posn-y p) 1)]
                 [else (posn-y p)]))
          g)]
        [else p]))

;; next-posn : posn -> posn
(define (next-posn p)
  (make-posn
   (cond [(= (posn-x p) board-length) 1]
         [else (+ (posn-x p) 1)])
   (cond [(= (posn-x p) board-length) (+ (posn-y p) 1)]
         [else (posn-y p)])))

;; occupied? : posn game -> boolean
(define/implicit-parameter (occupied? p g)
  #:freevars ()
  (or (member p (game-food g))
      (member p (snake-segments (game-snake g)))))

;; item : color -> image
(define (item c)
  (circle (/ cell-length 2) 'solid c))

(define alive-segment (item alive-color))
(define dead-segment (item dead-color))
(define food-morsel (item food-color))
(define obstacle-image (item obstacle-color))

;; place-items : image list-of-posn image -> image
(define (render-items item locations background)
  (foldr (lambda (location image)
           (place-item item location image))
         background
         locations))

;; place-item : image posn image -> image
(define (place-item item location background)
  (overlay/xy item
              (- (* cell-length (- (posn-x location) 1)))
              (- (* cell-length (- board-length (posn-y location))))
              background))

;; play-game : game -> game
;;
;; free variables:
;;   game-advance : (game -> game)
;;   add-food-to-game : (game posn -> game)
;;   change-snake-direction : (game direction -> game)
;;   game-score : (game -> nat)
;;   game-over? : (game -> boolean)
(define/implicit-parameter (play-game initial-game)
  #:freevars ([add-food-to-game
               (λ (game pos)
                 (eprintf "add-food-to-game: this function is not defined.\n")
                 game)]
              [change-snake-direction
               (λ (game dir)
                 (eprintf "change-snake-direction: this function is not defined.\n")
                 game)]
              [game-advance
               (let ([already-warned? #f])
                 (λ (game)
                   (unless already-warned?
                     (set! already-warned? #t)
                     (eprintf "game-advance: this function is not defined.\n"))
                   game))]
              [game-score
               (let ([already-warned? #f])
                 (λ (game)
                   (unless already-warned?
                     (set! already-warned? #t)
                     (eprintf "game-score: this function is not defined.\n"))
                   -9999999))]
              [game-over?
               (let ([already-warned? #f])
                 (λ (game)
                   (unless already-warned?
                     (set! already-warned? #t)
                     (eprintf "game-over?: this function is not defined.\n"))
                   #f))])
  (local [;; render-game : game -> image
          (define (render-game g)
            (render-items
             (cond [(game-over? g) dead-segment]
                   [else alive-segment])
             (snake-segments (game-snake g))
             (render-items obstacle-image
                           (game-obstacles g)
                           (render-items food-morsel
                                         (game-food g)
                                         (render-score g board)))))

          ;; render-score : game -> image
          (define (render-score game background)
            (overlay/align 'right 'top
                           (text (string-append "Score: "
                                                (number->string (game-score game)))
                                 12
                                 score-color)
                           background))

          ;; game-tick : game -> game
          (define (game-tick g)
            (local [(define morsels (length (game-food g)))
                    (define f (game-advance g))]
              (cond [(= (length (game-food f)) morsels) f]
                    [else (add-food-to-game f (random-free-posn f))])))

          ;; game-key : game key-event -> game
          (define (game-key g e)
            (cond [(member e '("up" "down" "left" "right"))
                   (change-snake-direction g e)]
                  [else g]))]
    (big-bang initial-game
              (on-tick game-tick tick-freq)
              (on-key game-key)
              (stop-when game-over? render-game)
              (to-draw render-game))))

(module+ internal
  (provide (all-from-out (submod ".."))
           empty sqr member true false
           posn? make-posn posn-x posn-y))
