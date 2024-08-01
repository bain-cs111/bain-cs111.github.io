;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise_6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./snake_lib.rkt")

; a game is...
; - (make-game snake (listof posn) (listof posn) number)
(define-struct game [snake food obstacles ticks])

; a direction is one of...
; - "up"
; - "down"
; - "left"
; - "right"
; That is, a direction must be one of these four strings (all lower cases).

; a snake is...
; - (make-snake direction (listof posn))
(define-struct snake [heading segments])

; segments is either
; - (cons posn empty)
; - (cons posn segments)
; That is, segments is a non-empty list of posns. 
; x-coordinates increase from 1 to board-length (inclusive) toward the right
; y-coordinates increase from 1 to board-length (inclusive) toward the top
; the default value for board-length is 50.

; food is either
; - empty
; - (cons posn food)
; That is, food is a list of posns.

; obstacles is either
; - empty
; - (cons posn obstacles)
; Obstacles is also a list of posns.

; add-food-to-game : game posn -> game
; Given a game and posn, returns a new game (so you want to call make-game here)
; where food has been added at that posn. 

(check-expect
 (add-food-to-game (make-game (make-snake "up" (list (make-posn 1 2)))
                              (list (make-posn 3 4))
                              (list (make-posn 10 10)
                                    (make-posn 20 20))
                              5)
                   (make-posn 6 7))
 (make-game (make-snake "up" (list (make-posn 1 2)))
            (list (make-posn 6 7) (make-posn 3 4))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            5))

; change-snake-direction: game direction -> game
; Given a game and direction, returns a new game where the snake
;   is now headed in the provided direction. 

(check-expect
 (change-snake-direction
  (make-game (make-snake "down" (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5)
  "left")
 (make-game (make-snake "left" (list (make-posn 1 2)))
            (list (make-posn 3 4))
            empty
            5))

; game-score : game -> number
; Given a game, returns a score (as a number)


; no tests are provided for game-score because it is open-ended
; feel free to implement it however you would like to

; game-over? : game -> boolean
; Given a game, returns true if that snake has died and false otherwise.
; We strongly recommend writing helper functions for this question!

(check-expect 
 (game-over? (make-game (make-snake "up" (list (make-posn 1 1))) empty empty 5))
 false)
(check-expect 
 (game-over? (make-game (make-snake "up" (list (make-posn -1 1))) empty empty 5))
 true)

; game-advance: game -> game
; Takes a game as input and advances the game one tick. The snake
;  moves forward one segment and eats or not. 

(check-expect
 (game-advance
  (make-game (make-snake "down" (list (make-posn 2 2)
                                      (make-posn 2 3)
                                      (make-posn 3 3)))
             empty
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake "down" (list (make-posn 2 1)
                                     (make-posn 2 2)
                                     (make-posn 2 3)))
            empty
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))
(check-expect
 (game-advance
  (make-game (make-snake "down" (list (make-posn 2 2)
                                      (make-posn 2 3)
                                      (make-posn 3 3)))
             (list (make-posn 2 1) (make-posn 8 9))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake "down" (list (make-posn 2 1)
                                     (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
            (list (make-posn 8 9))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))

; a starting game to experiment with
(define game-start
  (make-game (make-snake "up" (list (make-posn 12 12)))
             (list (make-posn 2 2) 
                   (make-posn 5 20)
                   (make-posn 15 15)
                   (make-posn 24 24))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             0))

;; play : game -> game
(define (play initial-game)
  (play-game initial-game))

;to start a game
(play game-start)