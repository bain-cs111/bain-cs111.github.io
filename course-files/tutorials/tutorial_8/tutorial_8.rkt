;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname tutorial_8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require "./asteroids_lib.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Game state and controlling flags
;;;
;;; Don't modify the code in this section
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Control state

; the-player : player?.
; The unique instance of the player struct. Automatically initialized by the Asteroids lib.
(define the-player "the-player: the Asteroids game is not running")

; firing-engines? : #true or #false.
; Whether the "up" key is pressed right now. Automatically updated by the Asteroids lib.
(define firing-engines? false)


;;; Tracking game objects

; all-game-objects : (listof game-object?)
; The list of all game objects, including the player, the asteroids, the missiles, etc.
(define all-game-objects '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type definitions
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the base type of all objects on screen.
;; However, this is an "abstract" type. We will never say (make-game-object ...), we'll
;; make different *subtypes* of game-object.
(define-struct game-object
  (position velocity orientation rotational-velocity)
  #:methods
  ;; update!: game-object -> void
  ;; Update object for the next frame.
  ;; This is a default method; it will be used by any subtypes that don't
  ;; define their own update! method.
  (define (update! me)
    ;; Do nothing
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
;;; Start writing your code from here
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




;;
;; HEAT SEEKER MISSILE HERE
;;

(check-satisfied make-heat-seeker procedure?)
(check-satisfied
 (make-heat-seeker (make-posn 420 350)
                   (make-posn 5 3)
                   0
                   0
                   100)
 missile?)

;;
;; UFO HERE
;;

(check-satisfied make-ufo procedure?)
(check-satisfied
 (make-ufo (make-posn 400 300)
           (make-posn 0 0)
           0
           0)
 game-object?)




;;; Main asteroids game
(define (asteroids)
  (link-and-start-asteroids-game))

(asteroids)
