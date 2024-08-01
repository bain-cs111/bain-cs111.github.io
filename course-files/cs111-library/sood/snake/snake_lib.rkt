#lang racket/base

(require (only-in lang/htdp-intermediate-lambda
                  local member member? andmap ormap
                  posn? make-posn posn-x posn-y
                  true false empty sqr)
         mzlib/pconvert
         racket/undefined
         (for-syntax racket/base racket/syntax syntax/parse/pre)
         2htdp/image
         2htdp/universe)

(provide play-game
         draw-game
         board-length
         game-score
         game-launcher
         check-snake
         check-game
         (rename-out
          [disallow-reverse reverse]
          [disallow-remove remove]
          [disallow-remove-all remove-all]
          [disallow-filter filter]
          [disallow-apply apply]
          [disallow-foldl foldl]
          [disallow-foldr foldr]
          [disallow-ormap ormap]
          [disallow-andmap andmap]
          [disallow-member member]
          [disallow-member? member?]
          ;; for game launcher
          [module+ racket:module+]
          [require racket:require]))

;; If you need to rename the struct fields, find the form `define-global-free-variables`
;; and select "Rename XXX" from the right-click context menu.
;;
;; To rename functions, find `play-game` and select "Rename XXX" for
;; the free variables declared in the #:freevars list.

(define board-length 25)
(define cell-length 15)
(define tick-freq 1/8)

(define eye-color "black")
(define alive-color "yellow green")
(define dead-color "pink")
(define food-color "goldenrod")
(define score-color "red")
(define obstacle-color "purple")
(define grid-color "gray")
(define default-background
  (local [(define img-size (* cell-length board-length))
          (define hline (line img-size 0 grid-color))
          (define vline (line 0 img-size grid-color))
          (define bg-scene (empty-scene img-size img-size))]
    (for/fold ([bg bg-scene])
              ([i (in-range 1 board-length)])
      (place-image hline
                   (/ img-size 2) (* cell-length i)
                   (place-image vline
                                (* cell-length i) (/ img-size 2)
                                bg)))))

(define (make-head-image eye color)
  (overlay/align/offset
   "center" "top"
   eye
   (/ cell-length 5) -1
   (overlay/align/offset
    "center" "top"
    eye
    (- (/ cell-length 5)) -1
    (square (- cell-length 1) "solid" color))))

(define (make-body-image color)
  (square (- cell-length 4) "solid" color))

(define eye (rectangle 2 4 "solid" eye-color))
(define dead-eye (overlay (line 3 3 eye-color) (line 3 -3 eye-color)))
(define alive-head (make-head-image eye alive-color))
(define alive-body (make-body-image alive-color))
(define dead-head (make-head-image dead-eye dead-color))
(define dead-body (make-body-image dead-color))
(define food-morsel (star (* cell-length 1/2) "solid" food-color))
(define obstacle-image
  (overlay/align/offset
   "left" "top"
   (circle 2 "solid" "light gray")
   -3 -3
   (overlay/align/offset
    "center" "bottom"
    (ellipse (* cell-length 9/10) (* cell-length 4/5) "solid" "gray")
    0 1
    (ellipse (* cell-length 4/5) (* cell-length 1/5) "solid" "dim gray"))))


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
;;; Snake game library
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-global-free-variables
  game? make-game game-snake game-obstacles game-foods game-ticks
  snake? snake-direction snake-body)

(define (listof-posn? lst)
  (and (list? lst) (andmap posn? lst)))

(define/implicit-parameter (check-snake v)
  #:freevars ([who 'check-snake])
  (unless (snake? v)
    (raise-argument-error who "a snake object" v))
  (unless (member (snake-direction v) '("up" "down" "left" "right"))
    (raise-argument-error who
                          "a direction in the direction field of the snake object"
                          v))
  (unless (and (listof-posn? (snake-body v))
               (not (null? (snake-body v))))
    (raise-argument-error who
                          "a non-empty list of posn objects in the body field of the snake object"
                          v))
  #true)

(define/implicit-parameter (check-game v)
  #:freevars ([who 'check-game])
  (unless (game? v)
    (raise-argument-error who "a game object" v))
  (let ([who (string->symbol (format "~a:\n  in the snake field of ~s\n  "
                                     who
                                     (print-convert v)))])
    (check-snake (game-snake v)))
  (unless (listof-posn? (game-obstacles v))
    (raise-argument-error who
                          "a list of posn objects in the obstacles field of the game object"
                          v))
  (unless (listof-posn? (game-foods v))
    (raise-argument-error who
                          "a list of posn objects in the foods field of the game object"
                          v))
  (unless (number? (game-ticks v))
    (raise-argument-error who
                          "a number in the ticks field of the game object"
                          v))
  #true)

(define (make-unary-default-function who make-default-result)
  (define already-warned? #f)
  (λ (game)
    (unless already-warned?
      (set! already-warned? #t)
      (eprintf "~a: this function is not defined.\n" who))
    (make-default-result game)))

(define (make-binary-default-function who make-default-result)
  (define already-warned? #f)
  (λ (game other-arg)
    (unless already-warned?
      (set! already-warned? #t)
      (eprintf "~a: this function is not defined.\n" who))
    (make-default-result game other-arg)))

(define default-add-food
  (make-binary-default-function 'copy-game/new-food (λ (_ game) game)))

(define (default-change-direction _ game)
  (eprintf "~a: this function is not defined.\n" 'copy-game/new-snake-direction)
  game)

(define default-advance-game
  (make-unary-default-function 'next-game-state (λ (game) game)))

(define default-game-over?
  (make-unary-default-function 'game-over? (λ (_) #f)))

;; random-free-posn : game -> posn
(define/implicit-parameter (random-free-posn g)
  #:freevars ()
  (nth-free-posn (random (num-free-posns g)) (make-posn 1 1) g))

;; num-free-posns : game -> posn
(define/implicit-parameter (num-free-posns g)
  #:freevars ()
  (- (sqr board-length)
     (+ 1 (length (snake-body (game-snake g))))
     (length (game-foods g))
     (length (game-obstacles g))))

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
         (next-free-posn (next-posn p) g)]
        [else p]))

;; next-posn : posn -> posn
(define (next-posn p)
  (make-posn
   (modulo (+ (posn-x p) 1) board-length)
   (modulo (+ (posn-y p) (quotient (+ (posn-x p) 1) board-length))
           board-length)))

;; occupied? : posn game -> boolean
(define/implicit-parameter (occupied? p g)
  #:freevars ()
  (or (member p (game-foods g))
      (member p (game-obstacles g))
      (member p (snake-body (game-snake g)))))

;; place-items : image list-of-posn image -> image
(define (place-items item locations background)
  (foldr (lambda (location image)
           (if (or (< (posn-x location) 0) (<= board-length (posn-x location))
                   (< (posn-y location) 0) (<= board-length (posn-y location)))
               image
               (place-item item location image)))
         background
         locations))

;; place-item : image posn image -> image
(define (place-item item location background)
  (place-image item
               (+ (* cell-length (posn-x location)) (/ cell-length 2))
               (+ (* cell-length (posn-y location)) (/ cell-length 2))
               background))

;; draw-score : game -> image
(define/implicit-parameter (draw-score game background)
  #:freevars (game-score)
  (overlay/align/offset
   "right" "top"
   (text (string-append "Score: "
                        (number->string (game-score game)))
         16
         score-color)
   4 -4
   background))

;; game-score : game -> nat
(define/implicit-parameter (game-score g)
  #:freevars ()
  (max (- (* 50 (length (snake-body (game-snake g))))
          (quotient (* 2 (game-ticks g)) 3))
       0))

;; draw-game : game -> image
(define/implicit-parameter (draw-game g)
  #:freevars (game-score)
  (let ([who 'draw-game])
    (check-game g))
  (define hd (car (snake-body (game-snake g))))
  (define over?
    (or (member? hd (append (game-obstacles g) (cdr (snake-body (game-snake g)))))
        (not (and (<= 0 (posn-x hd) (- board-length 1)) (<= 0 (posn-y hd) (- board-length 1))))))
  (define dir (snake-direction (game-snake g)))
  (define orig-head-img (if over? dead-head alive-head))
  (define head-img
    (cond [(equal? dir "right") (rotate -90 orig-head-img)]
          [(equal? dir "left") (rotate 90 orig-head-img)]
          [(equal? dir "down") (rotate 180 orig-head-img)]
          [else orig-head-img])) ;; "up"
  (place-items food-morsel
               (game-foods g)
               (place-items obstacle-image (game-obstacles g)
                            (place-items head-img
                                         (list (car (snake-body (game-snake g))))
                                         (place-items (if over? dead-body alive-body)
                                                      (cdr (snake-body (game-snake g)))
                                                      default-background)))))


;; play-game : game -> game
;;
;; free variables:
;;   game-advance : (game -> game)
;;   add-food-to-game : (game posn -> game)
;;   change-snake-direction : (game direction -> game)
;;   game-score : (game -> nat)
;;   game-over? : (game -> boolean)
(define/implicit-parameter (play-game initial-game)
  #:freevars ([copy-game/new-food default-add-food]
              [copy-game/new-snake-direction default-change-direction]
              [next-game-state default-advance-game]
              [game-over? default-game-over?])
  (let ([who 'play-game])
    (check-game initial-game))
  (local [;; copy-game/increment-tick : game -> game
          (define (copy-game/increment-tick g)
            (make-game (game-snake g)
                       (game-obstacles g)
                       (game-foods g)
                       (+ 1 (game-ticks g))))

          ;; game-tick : game -> game
          (define (game-tick g)
            (define morsels (length (game-foods g)))
            (define h (next-game-state g))
            (let ([who '|the output of next-game-state|])
              (check-game h))
            (define f (copy-game/increment-tick h))
            (cond [(= (length (game-foods f)) morsels) f]
                  [else
                   (define e (copy-game/new-food (random-free-posn f) f))
                   (let ([who '|the output of copy-game/new-food|])
                     (check-game e))
                   e]))

          ;; game-key : game key-event -> game
          (define (game-key g e)
            (cond [(member e '("up" "down" "left" "right"))
                   (define f (copy-game/new-snake-direction e g))
                   (let ([who '|the output of copy-game/new-snake-direction|])
                     (check-game f))
                   f]
                  [else g]))]
    (big-bang initial-game
      (on-tick game-tick tick-freq)
      (on-key game-key)
      (stop-when game-over? (λ (g)
                              (draw-score g
                                          (draw-game g))))
      (to-draw (λ (g)
                 (draw-score g
                             (draw-game g)))))))

(module+ internal
  (provide (all-from-out (submod ".."))
           empty sqr member true false
           posn? make-posn posn-x posn-y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Snake game interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (game-launcher stx)
  (syntax-case stx ()
    [(_ initial-board)
     (datum->syntax
      stx
      `(racket:module+
        main
        (racket:require racket/base racket/class racket/pretty mzlib/pconvert racket/gui/base)
        (pretty-print-columns 60)
        (port-count-lines! (current-output-port))

        (printf "\nClick \"Launch Game\" or enter (play board-medium) in the Interaction Window.\n\n")
        (define wnd
          (new frame%
               [label "Snake Game"]
               [width 200]
               [height 100]))

        (define pn
          (new pane%
               [parent wnd]
               [alignment '(center center)]))

        (define btn
          (new button%
               [parent pn]
               [label "Launch New Game"]
               [callback (lambda (self ev)
                           (printf "\nLaunching the snake game...\n")
                           (thread
                            (lambda ()
                              (define res
                                (play ,#'initial-board))
                              (printf "\nGame over. Final game state:\n\n    ")
                              (pretty-write (print-convert res))
                              (newline))))]))

        (send wnd show #t))
      stx)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Disallowing certain list procedures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (make-always-disallowed-id orig-id)
  (λ (stx)
    (raise-syntax-error (syntax-e orig-id)
                        "this procedure cannot be used in this assignment"
                        stx)))

(define-syntax (define-disallowed-id stx)
  (syntax-case stx ()
    [(form [new-id orig-id] ...)
     #'(begin
         (define-syntax new-id (make-always-disallowed-id #'orig-id))
         ...)]))

(define-disallowed-id
  [disallow-reverse reverse]
  [disallow-remove remove]
  [disallow-remove-all remove-all]
  [disallow-filter filter]
  [disallow-apply apply]
  [disallow-foldl foldl]
  [disallow-foldr foldr]
  [disallow-ormap ormap]
  [disallow-andmap andmap]
  [disallow-member member]
  [disallow-member? member?])
