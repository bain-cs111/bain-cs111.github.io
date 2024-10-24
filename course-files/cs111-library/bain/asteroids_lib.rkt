#lang racket/base
(require 2htdp/image
         2htdp/universe
         (only-in lang/htdp-advanced
                  local
                  true false pi
                  make-posn posn-x posn-y
                  empty empty? cons? first rest)
         "./define_super_struct.rkt")

(provide
 ;; Re-provide the define-struct form that supports subtypes and methods
 define-struct
 ;; Main game components
 link-and-start-asteroids-game
 ;; methods and structs that the students need to implement
 render radius update! destroy!
 struct-spec:game-object
 struct-spec:player
 struct-spec:asteroid
 struct-spec:missile
 struct-spec:heat-seeker
 struct-spec:ufo

 ;; Game object utilities
 closest-asteroid-to
 heading-of
 forward-direction
 distance-between-game-objects

 ;; Vector arithmetic utilities
 posn-+
 posn--
 posn-*
 unit-vector
 posn-magnitude
 distance-squared

 ;; Random value generators
 random-color
 random-element
 random-float
 random-velocity
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Specification of structure names and methods of the Asteroids game
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-define-struct:warn-not-allowed-method? #t)
(check-define-struct:warn-unspecified-struct? #t)

(define-struct-method-if-not-exists render
  #:default
  (λ (self)
    (define name-str
      (cond
        [(object-name self) => (λ (name) (format "~a" name))]
        [else "(unknown)"]))
    (define color-str
      (hash-ref DEFAULT-FALLBACK-COLOR name-str (λ () "Gray")))
    (rotate 90 (text name-str 14 color-str))))
(define-struct-method-if-not-exists radius
  #:default (λ (self) DEFAULT-FALLBACK-RADIUS))
(define-struct-method-if-not-exists update!
  #:default void)
(define-struct-method-if-not-exists destroy!
  #:default void)

(define-struct-name-specification game-object
  #:fields (position velocity orientation rotational-velocity)
  #:override-or-new-methods (update! destroy!)
  #:allowed-methods (render radius update! destroy!))

(define-struct-name-specification player
  #:super game-object
  #:fields ()
  #:override-or-new-methods (update! render radius)
  #:allowed-methods (render radius update! destroy!))

(define-struct-name-specification asteroid
  #:super game-object
  #:fields (radius color)
  #:override-or-new-methods (render radius)
  #:allowed-methods (render radius update! destroy!))

(define-struct-name-specification missile
  #:super game-object
  #:fields (lifetime)
  #:override-or-new-methods (update! render radius)
  #:allowed-methods (render radius update! destroy!))

(define-struct-name-specification heat-seeker
  #:super missile
  #:fields ()
  #:override-or-new-methods (render update!)
  #:allowed-methods (render radius update! destroy!))

(define-struct-name-specification ufo
  #:super game-object
  #:fields ()
  #:override-or-new-methods (render radius update! destroy!)
  #:allowed-methods (render radius update! destroy!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vector arithmetic library
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; posn-+: posn posn -> posn
;; Adds two posns.
(define (posn-+ a b)
  (make-posn (+ (posn-x a)
                (posn-x b))
             (+ (posn-y a)
                (posn-y b))))

;; posn--: posn posn -> posn
;; Subtracts two posns
(define (posn-- a b)
  (make-posn (- (posn-x a)
                (posn-x b))
             (- (posn-y a)
                (posn-y b))))

(define (unit-vector posn)
  (posn-* (/ 1 (posn-magnitude posn))
          posn))

(define (posn-magnitude posn)
  (sqrt (+ (squared (posn-x posn))
           (squared (posn-y posn)))))

;; posn-*: number posn -> posn
;; Multiplies posn by a scalar.
(define (posn-* k p)
  (make-posn (* k (posn-x p))
             (* k (posn-y p))))

(define (distance-squared p1 p2)
  (+ (squared (- (posn-x p1)
                 (posn-x p2)))
     (squared (- (posn-y p1)
                 (posn-y p2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Randomization library
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define random-color
  (local [(define colors
            (list (color 255 0 0)
                  (color 0 255 0)
                  (color 0 0 255)
                  (color 128 128 0)
                  (color 128 0 129)
                  (color 0 128 128)))]
    (λ () (random-element colors))))

(define (random-element list)
  (list-ref list
            (random (length list))))

(define (random-float min max)
  (+ min
     (* (random)
        (- max min))))

(define (random-velocity)
  (make-posn (random-float -10 10)
             (random-float -10 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Other utility functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (wrap number limit)
  (cond [(< number 0)
         (+ number limit)]
        [(> number limit)
         (- number limit)]
        [else
         number]))

(define (squared x)
  (* x x))

(define (arg-min f list)
  (local [(define (loop best best-score remaining)
            (if (empty? remaining)
                best
                (local [(define score (f (first remaining)))]
                  (if (< score best-score)
                      (loop (first remaining)
                            score
                            (rest remaining))
                      (loop best best-score
                            (rest remaining))))))]
    (loop (first list)
          (f (first list))
          (rest list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Game object utilities using poorman's unit/functor and implicit parameters
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; closest-asteroid-to: game-object -> asteroid or #f
;; Returns the nearest asteroid to the specified game objecct
(define/implicit-parameter (closest-asteroid-to game-object)
  #:freevars (asteroid? all-game-objects game-object-position game-object?)
  (unless (game-object? game-object)
    (raise-argument-error 'closest-asteroid-to
                          "game-object?"
                          game-object))
  (define all-asteroids
    (filter asteroid? all-game-objects))
  (cond
    [(empty? all-asteroids)
     #f]
    [else
     (arg-min (lambda (x)
                (distance-squared (game-object-position game-object)
                                  (game-object-position x)))
              all-asteroids)]))

;; heading-of: game-object game-object -> posn
;; Heading (unit vector) of the second object with respect to the first.
(define/implicit-parameter (heading-of object viewer)
  #:freevars (game-object-position game-object?)
  (for ([pos (in-naturals)]
        [game-object (in-list (list object viewer))])
    (unless (game-object? game-object)
      (raise-argument-error 'heading-of
                            "game-object?"
                            pos
                            object
                            viewer)))
  (unit-vector (posn-- (game-object-position object)
                       (game-object-position viewer))))

;; forward-direction: game-object -> posn
;; Returns a unit vector in the forward direction fo the game object.
(define/implicit-parameter (forward-direction object)
  #:freevars (game-object-orientation game-object?)
  (unless (game-object? object)
    (raise-argument-error 'forward-direction
                          "game-object?"
                          object))
  (local [(define o (game-object-orientation object))]
    (make-posn (cos o)
               (sin o))))

;; distance-between-game-objects: game-object game-object -> number
;; Returns the distance between two game objects
(define/implicit-parameter (distance-between-game-objects go1 go2)
  #:freevars (game-object-position game-object?)
  (for ([pos (in-naturals)]
        [game-object (in-list (list go1 go2))])
    (unless (game-object? game-object)
      (raise-argument-error 'distance-between-game-objects
                            "game-object?"
                            pos
                            go1
                            go2)))
  (sqrt (distance-squared (game-object-position go1)
                          (game-object-position go2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Asteroids game setup and parameters
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Tunable constants
;;;

(define WINDOW-WIDTH 800)
(define WINDOW-HEIGHT 600)
(define FRAME-RATE 30)
(define INTER-FRAME-INTERVAL (/ 1.0 FRAME-RATE))
(define ASTEROID-COUNT 10)
(define DEFAULT-FALLBACK-RADIUS 5)
(define DEFAULT-FALLBACK-COLOR
  #hash(("player" . "Lime Green")
        ("asteroid" . "Dodger Blue")
        ("missile" . "White")
        ("heat-seeker" . "Pink")
        ("ufo" . "Violet")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Asteroids game library and main implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/linking (link-and-start-asteroids-game)
  #:links ((mutable-variable-pack all-game-objects)
           (mutable-variable-pack the-player)
           (mutable-variable-pack firing-engines?)
           (struct-spec-pack/defaults game-object)
           (struct-spec-pack/defaults player)
           (struct-spec-pack/defaults asteroid)
           (struct-spec-pack/defaults missile)
           (struct-spec-pack/defaults heat-seeker)
           (struct-spec-pack/defaults ufo))

  ;; Creates the objects to be used in the game.
  (define (create-game-objects!)
    (begin (set! the-player
                 (make-player (make-posn (/ WINDOW-WIDTH 2)
                                         (/ WINDOW-HEIGHT 2))
                              (make-posn 0 0)
                              0
                              0))
           (set! all-game-objects
                 (cons the-player
                       (build-list ASTEROID-COUNT
                                   (λ (ignore) (new-asteroid)))))
           (set! firing-engines? false)
           (unless make-ufo
             (printf "Asteroids: warning: the ufo structure is not defined.\n"))
           (when (procedure? make-ufo)
             (set! all-game-objects
                   (cons (make-ufo (make-posn 100 100)
                                   (make-posn 0 0)
                                   0
                                   10)
                         all-game-objects)))
           ))

  ;;;
  ;;; Event dispatch
  ;;;

  ;; on-key-press: keyname -> void
  ;; Take appropriate actions for key presses.
  (define (on-key-press key)
    (cond [(equal? key "up")
           (set! firing-engines? true)]
          [(equal? key "left")
           (set-game-object-rotational-velocity! the-player
                                                 -2)]
          [(equal? key "right")
           (set-game-object-rotational-velocity! the-player
                                                 2)]
          [(equal? key " ")
           (fire-missile! make-missile)]
          [(equal? key "s")
           (cond
             [(procedure? make-heat-seeker)
              (fire-missile! make-heat-seeker)]
             [else
              (printf "Asteroids: warning: the 's' key is pressed but ")
              (printf "the heat-seeker structure is not defined.\n")])]
          [else
           (void)]))

  ;; on-key-release: keyname -> void
  ;; Take appropriate actions for key releases.
  (define (on-key-release key)
    (cond [(equal? key "up")
           (set! firing-engines? false)]
          [(or (equal? key "left")
               (equal? key "right"))
           (set-game-object-rotational-velocity! the-player 0)]
          [else
           (void)]))

  ;;;
  ;;; Object creation
  ;;;

  ;; new-asteroid: -> asteroid
  ;; Makes a new asteroid, but does not add it to all-game-objects
  (define (new-asteroid)
    (make-asteroid (make-asteroid-position)
                   (random-velocity)
                   0
                   0
                   (random-float 10 30)
                   (random-color)))

  ;; make-asteroid-position: -> posn
  ;; Chooses a random position that isn't too close to the player's initial position.
  (define (make-asteroid-position)
    (local [(define candidate (make-posn (random WINDOW-WIDTH)
                                         (random WINDOW-HEIGHT)))]
      (if (> (distance-squared candidate (game-object-position the-player))
             (squared (* 5
                         (radius the-player))))
          candidate
          (make-asteroid-position))))

  ;; file-missile!: (posn posn number number number -> missile) -> missile
  ;; Fires a missle in front of the player.
  ;; Argument is the constructor procedure for the missile;
  ;; use make-missile or make-heat-seeker.
  (define (fire-missile! missile-maker)
    (local [(define forward (forward-direction the-player))]
      (set! all-game-objects
            (cons (missile-maker (posn-+ (game-object-position the-player)
                                         (posn-* (+ (radius the-player) 5)
                                                 forward))
                                 (posn-+ (game-object-velocity the-player)
                                         (posn-* 100 forward))
                                 (game-object-orientation the-player)
                                 0
                                 100)
                  all-game-objects))))

  ;;;
  ;;; Driver loop
  ;;;

  (define last-player
    (make-posn (/ WINDOW-WIDTH 2) (/ WINDOW-HEIGHT 2)))

  ;; launch-asteroids-game -> void
  ;; Runs the asteroids game
  (define (launch-asteroids-game)
    (begin (create-game-objects!)
           (big-bang all-game-objects
             (on-key (λ (ignore key)
                       (on-key-press key)
                       all-game-objects))
             (on-release (λ (ignore key)
                           (on-key-release key)
                           all-game-objects))
             (on-tick (lambda (game-objects)
                        (for-each update! game-objects)
                        (for-each
                         (lambda (object)
                           (when (player? object)
                             (set! last-player object)))
                         game-objects)
                        (update-physics!)
                        all-game-objects)
                      INTER-FRAME-INTERVAL)
             (stop-when (lambda (game-objects)
                          (not (ormap player? game-objects)))
                        (lambda (game-objects)
                          (place-image
                           (overlay
                            (radial-star 9 3 10 "solid" "Light Pink")
                            (radial-star 9 5 30 "solid" "Hot Pink")
                            (radial-star 9 7 40 "solid" "Firebrick"))
                           (posn-x (game-object-position last-player))
                           (posn-y (game-object-position last-player))
                           (draw-all-objects (cons last-player game-objects)))))
             (to-draw draw-all-objects
                      800
                      600))))

  ;;;
  ;;; Rendering (drawing on the screen)
  ;;;

  (define radians->rotation-coefficient
    (/ -360.0
       (* 2 pi)))

  (define (radians->rotation radians)
    (+ (* radians radians->rotation-coefficient)
       -90))

  ;; draw-all-objects : (listof game-object?) -> scene (or image?)
  ;; Draws all game-objects onto a scene
  (define (draw-all-objects game-objects)
    (foldl
     (lambda (object scene)
       (place-image (rotate (radians->rotation (game-object-orientation object))
                            (render object))
                    (posn-x (game-object-position object))
                    (posn-y (game-object-position object))
                    scene))
     (rectangle WINDOW-WIDTH WINDOW-HEIGHT "solid" "black")
     game-objects))

  ;;;
  ;;; State update
  ;;;

  ;; update-physics!: -> void
  ;; Updates the orientation and position of every game object based on
  ;; velocity and angular velocity, and then does collision detection.
  ;; EFFECT: updates positions and orientations
  ;; EFFECT: objects destroyed because of collisions
  ;; EFFECT: updates velocities of asteroids if they bounce off one another
  (define (update-physics!)
    (for-each
     (λ (object)
       (set-game-object-orientation! object
                                     (+ (game-object-orientation object)
                                        (* INTER-FRAME-INTERVAL
                                           (game-object-rotational-velocity object))))
       (set-game-object-position!
        object
        (local [(define new-position
                  (posn-+ (posn-* INTER-FRAME-INTERVAL
                                  (game-object-velocity object))
                          (game-object-position object)))]
          (make-posn (wrap (posn-x new-position) WINDOW-WIDTH)
                     (wrap (posn-y new-position) WINDOW-HEIGHT)))))
     all-game-objects)
    (handle-collisions! all-game-objects))

  ;;;
  ;;; Collision handling
  ;;;

  ;; handle-collisions!: (listof game-object) -> void
  ;; Checks the objects in the list for collisions.
  ;; EFFECT: calls handle-collision! when a collision is found.
  (define (handle-collisions! objects)
    (unless (empty? objects)
      (local [(define head (first objects))
              (define tail (rest objects))]
        (begin (for-each (λ (object)
                           (when (collided? head object)
                             (handle-collision! head object)))
                         tail)
               (handle-collisions! tail)))))

  (define (collided? a b)
    (< (distance-squared (game-object-position a)
                         (game-object-position b))
       (squared (+ (radius a)
                   (radius b)))))

  ;; handle-collision!: game-object game-object -> void
  ;; Do whatever is necessary to handle a pair of colliding objects.
  ;; EFFECT: objects may bounce
  ;; EFFECT: objects may be destroyed.
  (define (handle-collision! a b)
    (cond
      [(and (asteroid? a) (asteroid? b))
       (bounce! a b)]
      [(or (and (player? a) (missile? b))
           (and (missile? a) (player? b))
           (and (missile? a) (missile? b)))
       (void)]
      [else
       (destroy! a)
       (destroy! b)]))

  ;; bounce!: game-object game-object -> void
  ;; update velocities of game-objects to simulate elastic collision.
  (define (bounce! a b)
    (local [(define mass-a (mass a))
            (define mass-b (mass b))
            (define vel-a (game-object-velocity a))
            (define vel-b (game-object-velocity b))
            (define one-over-mass (/ 1 (+ mass-a mass-b)))]
      (begin (set-game-object-velocity! a
                                        (posn-* one-over-mass
                                                (posn-+ (posn-* (- mass-a mass-b)
                                                                vel-a)
                                                        (posn-* (* 2 mass-b)
                                                                vel-b))))
             (set-game-object-velocity! b
                                        (posn-* one-over-mass
                                                (posn-+ (posn-* (- mass-b mass-a)
                                                                vel-b)
                                                        (posn-* (* 2 mass-a)
                                                                vel-a)))))))

  (define (mass asteroid)
    (squared (radius asteroid)))

  (void (launch-asteroids-game)))
