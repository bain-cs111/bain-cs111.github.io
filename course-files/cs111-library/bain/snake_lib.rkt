#lang racket/base

(require (only-in lang/htdp-intermediate-lambda
                  local member member? andmap ormap
                  posn? make-posn posn-x posn-y
                  true false empty sqr)
         mzlib/pconvert
         racket/undefined
         (for-syntax racket/base racket/syntax syntax/parse/pre
                     stepper/private/syntax-property)
         racket/async-channel racket/port
         racket/class (only-in racket/draw the-color-database)
         2htdp/image
         2htdp/universe)

(provide play-game
         draw-game
         board-length
         game-score
         game-launcher
         check-snake
         check-game
         ;; Comment this to ENABLE reverse
         (rename-out
          [disallow-reverse reverse])
         ;; Uncomment these exports to DISABLE remove, filter, ormap, andmap, etc.
         #;
         (rename-out
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
         (rename-out
          ;; for game launcher
          [submod racket:submod]
          [module+ racket:module+]
          [require racket:require]))

;; If you need to rename the struct fields, find the form `define-global-free-variables`
;; and select "Rename XXX" from the right-click context menu.
;;
;; To rename functions, find `play-game` and select "Rename XXX" for
;; the free variables declared in the #:freevars list.

(define board-length 25)
(define cell-length 16)
(define tick-freq 1/8)

(define eye-color "black")
(define alive-color "yellow green")
(define dead-color "pink")
(define food-color "goldenrod")
(define score-color "red")
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
(define dead-color-alpha 160)
(define dead-head
  (make-head-image
   dead-eye
   (cond
     [(color? dead-color)
      (make-color (color-red dead-color)
                  (color-green dead-color)
                  (color-blue dead-color)
                  dead-color-alpha)]
     [else
      (define c (send the-color-database find-color dead-color))
      (make-color (send c red) (send c green) (send c blue) dead-color-alpha)])))
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
     (for/list ([def-val (in-list (syntax-e #'((~? (#t . default-expr) (#f . #f)) ...)))])
       #`#,(syntax-e (car (syntax-e def-val))))
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
  snake? make-snake snake-direction snake-body)

(define (listof-posn? lst)
  (and (list? lst) (andmap posn? lst)))

(define/implicit-parameter (check-snake v)
  #:freevars ([who 'check-snake])
  (unless (snake? v)
    (raise-argument-error who "a snake object" v))
  (unless (member (snake-direction v) '("up" "down" "left" "right"))
    (raise-argument-error who
                          "a direction in the direction attribute of the snake object"
                          v))
  (unless (and (listof-posn? (snake-body v))
               (not (null? (snake-body v))))
    (raise-argument-error who
                          "a non-empty list of posn objects in the body attribute of the snake object"
                          v))
  #true)

(define/implicit-parameter (check-game v)
  #:freevars ([who 'check-game])
  (unless (game? v)
    (raise-argument-error who "a game object" v))
  (let ([who (string->symbol (format "~a:\n  in the snake attribute of ~s\n  "
                                     who
                                     (print-convert v)))])
    (check-snake (game-snake v)))
  (unless (listof-posn? (game-obstacles v))
    (raise-argument-error who
                          "a list of posn objects in the obstacles attribute of the game object"
                          v))
  (unless (listof-posn? (game-foods v))
    (raise-argument-error who
                          "a list of posn objects in the foods attribute of the game object"
                          v))
  (unless (number? (game-ticks v))
    (raise-argument-error who
                          "a number in the ticks attribute of the game object"
                          v))
  #true)

(provide guard-game-or-builtin-basic-game)
(define/implicit-parameter (guard-game-or-builtin-basic-game g)
  #:freevars ()
  (cond
    [(game? g) g]
    [else
     (eprintf "Error starting game; an alternative default game is selected.\n  expected: a game object\n  given: ~s\n"
              (print-convert g))
     (make-game (make-snake "right"
                            (list (make-posn 4 0)
                                  (make-posn 3 0)
                                  (make-posn 2 0)
                                  (make-posn 1 0)))
                (list (make-posn 6 1)
                      (make-posn 7 1)
                      (make-posn 7 2)
                      (make-posn 8 1)
                      (make-posn 8 3)
                      (make-posn 13 10)
                      (make-posn 13 11)
                      (make-posn 14 10)
                      (make-posn 15 10)
                      (make-posn 15 11)
                      (make-posn 15 12)
                      (make-posn 14 12)
                      (make-posn 13 12))
                (list (make-posn 0 1)
                      (make-posn 1 3)
                      (make-posn 4 4)
                      (make-posn 14 11))
                0)]))

(define (make-unary-default-function who make-default-result #:warn-once? [warn-once? #t])
  (define already-warned? #f)
  (λ (game)
    (unless (and already-warned? warn-once?)
      (set! already-warned? #t)
      (eprintf "~a: this function is not defined.\n" who))
    (make-default-result game)))

(define (make-binary-default-function who make-default-result #:warn-once? [warn-once? #t])
  (define already-warned? #f)
  (λ (game other-arg)
    (unless (and already-warned? warn-once?)
      (set! already-warned? #t)
      (eprintf "~a: this function is not defined.\n" who))
    (make-default-result game other-arg)))

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

;; draw-terminated : image -> image
(define (draw-terminated err-msg board)
  (define err-img-by-line
    (for/list ([line (in-list (regexp-split #rx"\n" err-msg))]
               #:when (not (zero? (string-length line))))
      (text/font (if (> (string-length line) 50)
                     (string-append (substring line 0 47) "...")
                     line)
                 10
                 "red"
                 #f
                 "modern"
                 "normal"
                 "normal"
                 #f)))
  (define err-img
    (cond [(null? err-img-by-line) empty-image]
          [(null? (cdr err-img-by-line)) (car err-img-by-line)]
          ;; needs at least two lines
          [else (apply above/align "left" err-img-by-line)]))
  (define txt-img
    (above (text "PROGRAM CRASHED" 22 "red")
           (rectangle 0 5 "solid" "white")
           (text "(Check the Interaction Window)" 16 "red")
           (rectangle 0 5 "solid" "white")
           (line (* board-length cell-length 5/6) 0 "red")
           (rectangle 0 5 "solid" "white")
           err-img))
  (define txt-bg
    (rectangle (+ 10 (image-width txt-img))
               (+ 10 (image-height txt-img))
               "solid"
               (make-color 255 255 255 160)))
  (overlay
   txt-img
   txt-bg
   board))

;; draw-score : game image -> image
(define/implicit-parameter (draw-score game board)
  #:freevars ()
  (overlay/align/offset
   "right" "top"
   (text (string-append "Score: "
                        (number->string (game-score game)))
         16
         score-color)
   4 -4
   board))

;; game-score : game -> nat
(define/implicit-parameter (game-score g)
  #:freevars ()
  (max (- (* 50 (length (snake-body (game-snake g))))
          (quotient (* 2 (game-ticks g)) 3))
       0))

;; draw-game : game -> image
(define/implicit-parameter (draw-game g)
  #:freevars ()
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
               (place-items head-img
                            (list (car (snake-body (game-snake g))))
                            (place-items (if over? dead-body alive-body)
                                         (cdr (snake-body (game-snake g)))
                                         (place-items obstacle-image (game-obstacles g)
                                                      default-background)))))

;; put-new-food : posn game -> game
;;   Given a posn and a game, returns a new game where food has been added at that posn.
(define/implicit-parameter (put-new-food p g)
  #:freevars ()
  (make-game (game-snake g)
             (game-obstacles g)
             (cons p (game-foods g))
             (game-ticks g)))

(struct ended-with (exn world) #:transparent)
(define ach (make-async-channel))
(define agch (make-async-channel))

(define (format-output-name name) (string->symbol (format "the output of ~a" name)))
(define copy-game/remove-food-name 'remove-food)
(define copy-game/add-food-name 'put-new-food)
(define copy-game/add-snake-head-name 'add-new-head)
(define copy-game/drop-snake-tail-name 'drop-tail)
(define copy-game/set-snake-direction-name 'set-direction)
(define next-game-state-name 'step-game)
(define game-over?-name 'is-snake-dead?)

;; play-game : game -> game
(define/implicit-parameter (play-game initial-game)
  #:freevars ([who 'play-game]
              [debug? #f]
              [remove-food (make-binary-default-function
                            copy-game/remove-food-name
                            (λ (p game) game))]
              [add-new-head (make-binary-default-function
                             copy-game/add-snake-head-name
                             (λ (p game) game))]
              [drop-tail (make-unary-default-function
                          copy-game/drop-snake-tail-name
                          (λ (game) game))]
              [set-direction (make-binary-default-function
                              copy-game/set-snake-direction-name
                              (λ (dir game) game)
                              #:warn-once? #f)]
              [step-game (make-unary-default-function
                          next-game-state-name
                          (λ (game) game))]
              [is-snake-dead? (make-unary-default-function
                               game-over?-name
                               (λ (game) #f))])
  (let ([who who])
    (check-game initial-game))

  ;; copy-game/increment-tick : game -> game
  (define (copy-game/increment-tick g)
    (make-game (game-snake g)
               (game-obstacles g)
               (game-foods g)
               (+ 1 (game-ticks g))))

  ;; game-should-add-new-food? : game -> boolean
  (define (game-should-add-new-food? g old-g)
    (define t (* (floor (abs (game-ticks g))) tick-freq))
    (or (not (= (length (game-foods g))
                (length (game-foods old-g))))
        (and (>= t 4)
             (<= (length (game-foods g)) 8)
             (integer? (sqrt t)))))

  ;; game-tick : game -> game
  (define (game-tick g)
    (with-handlers ([exn:fail? (λ (e) (ended-with e g))])
      (define h (step-game g))
      (let ([who (format-output-name next-game-state-name)])
        (check-game h))
      (define new-g (copy-game/increment-tick h))
      (cond [(not (game-should-add-new-food? new-g g)) new-g]
            [else
             (define final-g (put-new-food (random-free-posn new-g) new-g))
             (let ([who (format-output-name copy-game/add-food-name)])
               (check-game final-g))
             final-g])))

  ;; game-key : game key-event -> game
  (define (game-key g e)
    (cond [(member e '("up" "down" "left" "right"))
           (with-handlers ([exn:fail? (λ (e) (ended-with e g))])
             (define f (set-direction e g))
             (let ([who (format-output-name copy-game/set-snake-direction-name)])
               (check-game f))
             f)]
          [else g]))

  (define (world->image w)
    (cond [(ended-with? w)
           (define g (ended-with-world w))
           (draw-terminated (exn-message (ended-with-exn w))
                            (draw-score g (draw-game g)))]
          [else
           (draw-score w (draw-game w))]))

  (define res
    (if debug?
        (big-bang initial-game
          [name "Northwestern CS111 Snake (Debug)"]
          [on-tick (λ (g)
                     (define ev (async-channel-try-get ach))
                     (if (not ev)
                         g
                         (with-handlers ([exn:fail? (λ (e) (ended-with e g))])
                           (define args
                             (for/list ([arg (in-list (cdr ev))])
                               (with-input-from-string arg read)))
                           (define res
                             (cond
                               [(string=? (car ev) (symbol->string copy-game/add-food-name))
                                (put-new-food (make-posn (car args) (cadr args)) g)]
                               [(string=? (car ev) (symbol->string copy-game/remove-food-name))
                                (remove-food (make-posn (car args) (cadr args)) g)]
                               [(string=? (car ev) (symbol->string copy-game/add-snake-head-name))
                                (add-new-head (make-posn (car args) (cadr args)) g)]
                               [(string=? (car ev) (symbol->string copy-game/drop-snake-tail-name))
                                (drop-tail g)]
                               [(string=? (car ev) (symbol->string copy-game/set-snake-direction-name))
                                (set-direction (car args) g)]
                               [(string=? (car ev) (symbol->string next-game-state-name))
                                (step-game g)]
                               [else g]))
                           (async-channel-put agch res)
                           (let ([who (format-output-name (car ev))])
                             (check-game res))
                           res)))
                   1/15]
          [stop-when ended-with? world->image]
          [to-draw world->image])
        (big-bang initial-game
          [name "Northwestern CS111 Snake"]
          [on-tick game-tick tick-freq]
          [on-key game-key]
          [stop-when (λ (w)
                       (or (ended-with? w)
                           (is-snake-dead? w)))
                     world->image]
          [to-draw world->image])))

  (if (ended-with? res)
      (raise (ended-with-exn res))
      res))

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

(module+ gui
  (provide launch-gui)

  (require racket/string racket/class racket/pretty mzlib/pconvert
           racket/gui/base
           framework)

  (define-global-free-variables
    game? make-game game-snake game-obstacles game-foods game-ticks
    snake? make-snake snake-direction snake-body)

  (define/implicit-parameter (launch-gui var-init-name brd-init)
    #:freevars (play debug)
    (pretty-print-columns 60)
    (port-count-lines! (current-output-port))

    (define (get-board)
      (define brd-str
        (send t-world get-text 0 'eof #t))
      (guard-game-or-builtin-basic-game
       (with-handlers ([exn:fail? (λ (e)
                                    ((error-display-handler) (exn-message e) e)
                                    (eprintf "\n")
                                    #f)])
         (eval
          (with-input-from-string brd-str
            read)))))

    (printf "\nClick \"Play New Game\" or enter (play ~a) in the Interaction Window.\n\n"
            var-init-name)
    (define wnd
      (new frame%
           [label "Console"]
           [width 650]
           [height 400]))

    (define v-pn
      (new vertical-pane%
           [parent wnd]
           [alignment '(center center)]))

    (define t-init-board-name
      (new text-field%
           [label "Initial board"]
           [parent v-pn]
           [init-value var-init-name]
           [vert-margin 10]
           [horiz-margin 15]
           [callback (λ (tf ev)
                       (with-handlers ([exn:fail? void])
                         (define var-name (string-trim (send tf get-value)))
                         (when (non-empty-string? var-name)
                           (set-t-world-game!
                            (namespace-variable-value (string->symbol var-name)))
                           (send t-world lock #f))))]))

    (define h-pn
      (new horizontal-pane%
           [parent v-pn]
           [alignment '(center center)]
           [stretchable-height #f]))

    (new button%
         [parent h-pn]
         [label "Play New Game"]
         [callback (lambda (self ev)
                     (printf "\nLaunching the snake game...\n")
                     (thread
                      (lambda ()
                        (define brd (get-board))
                        (set-t-world-game! brd)
                        (send self enable #f)
                        (define res
                          (with-handlers ([exn:fail? values])
                            (play brd)))
                        (send self enable #t)
                        (send t-world lock #f)
                        (cond
                          [(exn:fail? res)
                           (eprintf "\nAn error occurred. Game terminated.\n\n")
                           ((error-display-handler) (exn-message res) res)]
                          [else
                           (printf "\nGame over. Final game state:\n\n    ")
                           (pretty-write (print-convert res))
                           (newline)]))))])

    (new button%
         [parent h-pn]
         [label "Debug (Stepper)"]
         [callback (lambda (self ev)
                     (send self enable #f)
                     (send self set-label "(Debugging)")
                     (thread
                      (lambda ()
                        (define brd (get-board))
                        (set-t-world-game! brd)
                        (for ([btn (in-list ctl-btns)])
                          (send btn enable #t))
                        (define res
                          (with-handlers ([exn:fail? values])
                            (debug brd)))
                        (send self enable #t)
                        (send self set-label "Debug (Stepper)")
                        (send t-world lock #f)
                        (for ([btn (in-list ctl-btns)])
                          (send btn enable #f))
                        (when (exn:fail? res)
                          (raise res)))))])

    (define h-pn2 (new horizontal-pane% [parent v-pn]))

    (define v-pn2
      (new vertical-pane%
           [parent h-pn2]
           [alignment '(right top)]
           [spacing 20]
           [stretchable-width #f]))

    (define t-world
      (new (text:line-numbers-mixin
            (class* text:basic% (editor:standard-style-list<%>) ;; editor:standard-style-list-mixin
              (super-new)
              (with-handlers ([exn:fail? void])
                (editor:set-standard-style-list-delta
                 (editor:get-default-color-style-name)
                 (make-object style-delta% 'change-size 14)))
              (define/override (on-default-event mev)
                (cond [(eq? (send mev get-event-type) 'right-down)
                       (send (send t-world get-admin) popup-menu pm
                             (send mev get-x)
                             (send mev get-y))]
                      [else
                       (super on-default-event mev)]))))))
    (define mb (new menu-bar% [parent wnd]))
    (append-editor-operation-menu-items (new menu% [label "Edit"] [parent mb]))
    (define pm (new popup-menu%))
    (append-editor-operation-menu-items pm)
    (send t-world set-max-undo-history 100)
    (void
     (thread
      (lambda ()
        (let loop ()
          (define g (async-channel-get agch))
          (set-t-world-game! g)
          (loop)))))
    (define style-list (editor:get-standard-style-list))
    (define std-font
      (send (or (send style-list
                      find-named-style
                      (editor:get-default-color-style-name))
                (send style-list find-named-style "Standard")
                (send style-list basic-style))
            get-font))
    (define (set-t-world-game! g)
      (send t-world lock #f)
      (send t-world erase)
      (send t-world change-style
            (let ([delta (make-object style-delta% 'change-size 14)])
              (send delta set-delta-face (send std-font get-face) (send std-font get-family))
              delta))
      (send t-world insert
            (parameterize ([pretty-print-current-style-table
                            (pretty-print-extend-style-table
                             (pretty-print-current-style-table)
                             '(make-snake)
                             '(and))])
              (pretty-format (print-convert g)
                             55
                             #:mode 'write)))
      (send t-world lock #t))

    (set-t-world-game! brd-init)
    (send t-world lock #f)

    (new editor-canvas%
         [parent h-pn2]
         [editor t-world])

    (define ctl-btns
      (append
       (for/list ([label (in-list (list copy-game/add-food-name
                                        copy-game/remove-food-name
                                        copy-game/add-snake-head-name))])
         (define v-pn3 (new vertical-pane% [parent v-pn2] [alignment '(right top)] [stretchable-height #f]))
         (define h-pn3 (new horizontal-pane% [parent v-pn3] [stretchable-height #f]))
         (define t-x (new text-field% [label "x"] [parent h-pn3] [init-value "0"] [vert-margin 0]))
         (define t-y (new text-field% [label "y"] [parent h-pn3] [init-value "0"] [vert-margin 0]))
         (new button%
              [parent v-pn3]
              [vert-margin 0]
              [label (symbol->string label)]
              [callback (λ (btn ev)
                          (when (and (non-empty-string? (string-trim (send t-x get-value)))
                                     (non-empty-string? (string-trim (send t-y get-value))))
                            (async-channel-put ach (list (send btn get-label)
                                                         (send t-x get-value)
                                                         (send t-y get-value)))))]
              [enabled #f]))
       (list
        (new button%
             [parent v-pn2]
             [vert-margin 0]
             [label (symbol->string copy-game/drop-snake-tail-name)]
             [callback (λ (btn ev)
                         (async-channel-put ach (list (send btn get-label))))]
             [enabled #f])

        (let ()
          (define v-pn3 (new vertical-pane% [parent v-pn2] [alignment '(right top)] [stretchable-height #f]))
          (define ch
            (new choice%
                 [parent v-pn3]
                 [horiz-margin 10]
                 [vert-margin 0]
                 [label ""]
                 [choices '("\"up\"" "\"right\"" "\"down\"" "\"left\"")]))
          (new button%
               [parent v-pn3]
               [vert-margin 0]
               [label (symbol->string copy-game/set-snake-direction-name)]
               [callback (λ (btn ev)
                           (async-channel-put ach (list (send btn get-label)
                                                        (send ch get-string-selection))))]
               [enabled #f]))

        (new button%
             [parent v-pn2]
             [vert-margin 0]
             [label (symbol->string next-game-state-name)]
             [callback (λ (btn ev)
                         (async-channel-put ach (list (send btn get-label))))]
             [enabled #f]))))

    (send wnd show #t))
  )

(define-syntax (game-launcher stx)
  (syntax-case stx ()
    [(_ initial-board)
     (datum->syntax
      stx
      `(racket:module+
        main
        (racket:require (racket:submod "snake_lib.rkt" gui))
        ,(stepper-syntax-property
          (datum->syntax
           stx
           (if (identifier? #'initial-board)
               `(launch-gui ,(symbol->string (syntax-e #'initial-board))
                            ,#'initial-board)
               `(launch-gui "" ,#'initial-board)))
          'stepper-skip-completely
          #t))
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
