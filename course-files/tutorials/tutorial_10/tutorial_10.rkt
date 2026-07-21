#lang htdp/asl

(require 2htdp/image)
(require 2htdp/universe)
(require "./asteroids_lib.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Game state and controlling flags
;;;
;;; DO NOT MODIFY ANY CODE until you see "START MODIFYING CODE"
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Control state

; the-player : player?.
; The unique instance of the player struct.
;   Automatically initialized by the Asteroids lib.
(define the-player "the-player: the Asteroids game is not running")

; firing-engines? : #true or #false.
; Whether the "up" key is pressed right now.
;   Automatically updated by the Asteroids lib.
(define firing-engines? false)

;;; Tracking game objects

; all-game-objects : (listof game-object)
; The list of all game objects, including the player, the asteroids, the missiles, etc.
(define all-game-objects '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type definitions
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the base type of all objects on screen.
;; However, this is an "abstract" type. We will never say (make-game-object ...),
;;   we'll make different *subtypes* of game-object.
(define-struct game-object
  (position velocity orientation rotational-velocity)
  #:methods
  ;; update!: game-object -> void
  ;; Update object for the next frame.
  ;; This is a default method; it will be used by any subtypes that don't
  ;; define their own update! method.
  (define (update! me)
    ;; Do nothing on purpose.
    (void))
  
  ;; destroy!: game-object -> void
  ;; Destroys the game object
  ;; This is a default method; it will be used by any subtypes that don't
  ;; define their own destroy! method.
  (define (destroy! me)
    (set! all-game-objects
          (remove me all-game-objects)))
  
  ;; render: game-object -> image
  ;; Draws the game-object.
  ;; There is no default method for render, since there is no default
  ;; appearance for objects. You must fill in a render method for your
  ;; subclass.
  
  ;; radius: game-object -> number
  ;; Size of the game object for purposes of detecting collisions.
  ;; There is no default method for radius, since there's no default
  ;; size for objects. You must fill in a radius method for your
  ;; subclass.
  )

(check-satisfied update! procedure?)
(check-satisfied destroy! procedure?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; START WRITING YOUR CODE FROM HERE.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here are a few functions provided by the Asteroids lib. You may find
;; some of them helpful.
;;
;; For explanations about what each function does, check out
;; the tutorial page and the exercise page.
;;
;; ;; Game object utilities
;; forward-direction
;; closest-asteroid-to
;; heading-of
;;
;; ;; Vector arithmetic utilities
;; posn-+
;; posn--
;; posn-*

;; This is the type for the player's ship.
;; There will always be exactly one of these, and it will be stored
;; in the global variable the-player.
(define-struct (player game-object)
  () ; no special properties
  #:methods
  ;; FILL IN THE FOLLOWING METHODS
  
  ;; update!: player -> void
  ;; Accelerate if the engines are firing.
  
  ;; render: player -> image
  ;; Draw the player's ship
  
  ;; radius: player -> number
  ;; Size of the object (for collision detection)
  )

(check-satisfied
 (make-player (make-posn 400 300)
              (make-posn 0 0)
              0
              0)
 game-object?)
(check-satisfied render procedure?)
(check-satisfied radius procedure?)

; These check-expects are VERY basic. Make sure to write your own.


;; This is the type for the asteroids.
;; Asteroids come in different sizes, so they have a radius
;; field in addition to their color field.
(define-struct (asteroid game-object)
  (radius color)
  #:methods
  ;; FILL THESE IN
  
  ;; render: asteroid -> image
  ;; Draw the asteroid
  
  ;; radius: asteroid -> number
  ;; Size of the asteroid
  )

(check-satisfied
 (make-asteroid (make-posn (random 800) (random 600))
                (random-velocity)
                0
                0
                (random-float 10 30)
                (random-color))
 game-object?)

;; REMINDER: Set aside some time to write some check-expects

;;;

; how-many-of : (game-object -> boolean) -> number
; calculates how many of some game-object are currently in the game

;;;


;; This is the type for normal missiles.
(define-struct (missile game-object)
  (lifetime)
  #:methods
  ;; FILL THESE IN
  
  ;; update!: missile -> void
  ;; Decrement missile lifetime and destroy if necessary.
  
  ;; render: missile -> image
  ;; Draw the missile
  
  ;; radius: missile -> number
  ;; Size of the missile
  )

(check-satisfied
 (make-missile (make-posn 420 350)
               (make-posn 5 3)
               0
               0
               100)
 game-object?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HOMING MISSILE OBJECT DEFINITION HERE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Some basic check-expects to enable when ready
#;(check-satisfied make-homing-missile procedure?)
#;(check-satisfied
   (make-homing-missile (make-posn 420 350)
                        (make-posn 5 3)
                        0
                        0
                        100)
   missile?)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UFO OBJECT DEFINITION HERE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Some basic check-expects to enable when ready
#;(check-satisfied make-ufo procedure?)
#;(check-satisfied
 (make-ufo (make-posn 400 300)
           (make-posn 0 0)
           0
           0)
 game-object?)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Don't change the below
;;; Main asteroids game
(define (asteroids)
  (link-and-start-asteroids-game))

;; TO RUN, call the asteroids function in the INTERACTIONS WINDOW
;; (asteroids)