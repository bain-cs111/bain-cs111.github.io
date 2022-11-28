;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname tutorial_8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require "./define_super_struct.rkt")

; Tunable constants
(define window-width 800)  ; default 800
(define window-height 600) ; default 600 
(define frame-rate 30)     ; default 30
(define inter-frame-interval (/ 1.0 frame-rate)) ; default (/ 1.0 frame-rate)
(define asteroid-count 10) ; default 10

;;; Tracking game objects
(define all-game-objects '())
(define the-player '())
(define firing-engines? false) ; True when the player is firing the engines

;;; Object Definitions
;;; Type definitions
(define-struct game-object
  (position
   velocity
   orientation
   rotational-velocity
   radius)
  #:methods
  (define (render an-object)
    (void))
  (define (update! an-object)
    (void))
  (define (destroy! object)
    (set! all-game-objects
          (remove object all-game-objects))))

(define-struct (player game-object) ()
  #:methods
  (define (render a-player) 
    (isosceles-triangle 30 40 "solid" "aquamarine"))

  ; Player -> Void
  ; Called on each tick to update the velocity of the player
  ; Effect: Change the player's velocity
  (define (update! p)
    (when firing-engines?
      (set-game-object-velocity! p
                                 (posn-+ (game-object-velocity p)
                                         (posn-* 6 
                                                 (forward-direction p)))))))

(define-struct (missile game-object) (lifetime)
  #:methods
  (define (render a-missile)
    (circle 1 "solid" "white"))
  ; Missile -> Void
  ; Called on each tick to update the lifetime of the missile
  ; and destroy it if the lifetime is zero.
  ; Effect: Change the missile lifetime
  ; Effect: May destroy the missile
  (define (update! m)
    (if (= (missile-lifetime m) 0)
        (destroy! m)
        (set-missile-lifetime! m (- (missile-lifetime m) 1)))))

(define-struct (asteroid game-object) (color)
  #:methods
  (define (render an-asteroid)
    (circle (game-object-radius an-asteroid) "solid" (asteroid-color an-asteroid))))

;;; part I: steering

; Player -> Void
; Called when the left arrow key is pressed
; Effect: Changes the players direction
(define (on-left-press p)
  (set-game-object-rotational-velocity! p -2))

; Player -> Void
; Called when the left arrow key is released
; Effect: Changes the players direction
(define (on-left-release p)
  (set-game-object-rotational-velocity! p 0))

; Player -> Void
; Called when the right arrow key is pressed
; Effect: Changes the players direction
(define (on-right-press p)
  (set-game-object-rotational-velocity! p 2))

; Player -> Void
; Called when the right arrow key is released
; Effect: Changes the players direction
(define (on-right-release p)
  (set-game-object-rotational-velocity! p 0))

;;; part II: moving
;;; also part III: making moving harder

; Player -> Void
; Called when the up arrow key is pressed
; Effect: Changes firing-engines?
(define (on-up-press p)
  (set! firing-engines? true))

; Player -> Void
; Called when the up arrow key is released
; Effect: Changes firing-engines?
(define (on-up-release p)
  (set! firing-engines? false))

;;; part IV: blowing stuff up

; -> Void
; Called when the space bar is pressed.
; There's no action to take when the space bar is released.
; Effect: may create a missile
(define (on-space-press)
  (fire-missile!))

;;; DON'T MODIFY ANYTHING BELOW THIS POINT
;;; AND DON'T WORRY THAT THERE'S STUFF HERE
;;; THAT YOU DON'T UNDERSTAND YET. THIS IS
;;; THE CODE FOR PHYSICS AND ANIMATION.

;;; Vector arithmetic
;;; you might find reading these definitions
;;; helpful but don't worry if they don't make
;;; sense to you

(define (posn-+ a b)
  (make-posn (+ (posn-x a)
                (posn-x b))
             (+ (posn-y a)
                (posn-y b))))

(define (posn-* k p)
  (make-posn (* k (posn-x p))
             (* k (posn-y p))))

(define (forward-direction object)
  (local [(define o (game-object-orientation object))]
    (make-posn (cos o)
               (sin o))))

;;; Object creation
(define (new-asteroid)
  (make-asteroid (make-asteroid-position)
                 (random-velocity)
                 0
                 0
                 (random-float 10 30)
                 (random-color)))

(define (make-asteroid-position)
  (local [(define candidate (make-posn (random window-width)
                                       (random window-height)))]
    (if (> (distance-squared candidate (game-object-position the-player))
           (squared (* 5
                       (game-object-radius the-player))))
        candidate
        (make-asteroid-position))))

(define (fire-missile!)
  (local [(define forward (forward-direction the-player))]
    (set! all-game-objects
          (cons (make-missile (posn-+ (game-object-position the-player)
                                      (posn-* (+ (game-object-radius the-player) 5)
                                              forward))
                              (posn-+ (game-object-velocity the-player)
                                      (posn-* 100 forward))
                              0
                              0
                              1
                              100)
                all-game-objects))))


(define (setup-game!)
  (begin
    (set! the-player
          (make-player (make-posn (/ window-width 2)
                                  (/ window-height 2))
                       (make-posn 0 0)
                       0
                       0
                       20))
    (set! all-game-objects
          (cons the-player
                (build-list asteroid-count
                            (λ (ignore) (new-asteroid)))))))


;;; Driver loop
(define (asteroids)
  (begin (setup-game!)
         (big-bang all-game-objects
           (on-key (λ (ignore key)
                     (begin (on-key-press key)
                            all-game-objects)))
           (on-release (λ (ignore key)
                         (begin (on-key-release key)
                                all-game-objects)))
           (on-tick (lambda (game-objects)
                      (begin (for-each update! game-objects)
                             (update-physics!)
                             all-game-objects))
                    inter-frame-interval)
           (to-draw (lambda (game-objects)
                      (foldl (lambda (object scene)
                               (place-image (rotate (radians->rotation (game-object-orientation object))
                                                    (render object))
                                            (posn-x (game-object-position object))
                                            (posn-y (game-object-position object))
                                            scene))
                             (rectangle window-width window-height "solid" "black")
                             game-objects))
                    window-width
                    window-height))))

;;; Event dispatch
(define (on-key-press key)
  (cond [(equal? key "up")
         (on-up-press the-player)]
        [(equal? key "left")
         (on-left-press the-player)]
        [(equal? key "right")
         (on-right-press the-player)]
        [(equal? key " ")
         (on-space-press)]
        [else (void)]))

(define (on-key-release key)
  (cond [(equal? key "up")
         (on-up-release the-player)]
        [(equal? key "left")
         (on-left-release the-player)]
        [(equal? key "right")
         (on-right-release the-player)]
        [else (void)]))


(define radians->rotation-coefficient
  (/ -360.0
     (* 2 pi)))

(define (radians->rotation radians)
  (+ (* radians radians->rotation-coefficient)
     -90))

;;; State update
(define (update-physics!)
  (begin (for-each (λ (object)
                     (begin (set-game-object-orientation! object
                                                          (+ (game-object-orientation object)
                                                             (* inter-frame-interval
                                                                (game-object-rotational-velocity object))))
                            (set-game-object-position! object
                                                       (local [(define new-position
                                                                 (posn-+ (posn-* inter-frame-interval
                                                                                 (game-object-velocity object))
                                                                         (game-object-position object)))]
                                                         (make-posn (wrap (posn-x new-position) window-width)
                                                                    (wrap (posn-y new-position) window-height))))))
                   all-game-objects)
         (handle-collisions all-game-objects)))

;;; Collision handling
(define (handle-collisions objects)
  (unless (empty? objects)
    (local [(define head (first objects))
            (define tail (rest objects))]
      (begin (for-each (λ (object)
                         (when (collided? head object)
                           (handle-collision head object)))
                       tail)
             (handle-collisions tail)))))

(define (collided? a b)
  (< (distance-squared (game-object-position a)
                       (game-object-position b))
     (squared (+ (game-object-radius a)
                 (game-object-radius b)))))

(define (handle-collision a b)
  (if (and (asteroid? a)
           (asteroid? b))
      (bounce a b)
      (begin (destroy! a)
             (destroy! b))))

(define (bounce a b)
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
  (squared (game-object-radius asteroid)))

;;; Randomization Functions
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

;;; Other arithmetic utilities
(define (wrap number limit)
  (cond [(< number 0)
         (+ number limit)]
        [(> number limit)
         (- number limit)]
        [else
         number]))

(define (squared x)
  (* x x))

;;; Vector arithmetic
(define (distance-squared p1 p2)
  (+ (squared (- (posn-x p1)
                 (posn-x p2)))
     (squared (- (posn-y p1)
                 (posn-y p2)))))