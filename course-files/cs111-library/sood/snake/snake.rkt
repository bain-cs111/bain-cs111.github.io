#lang htdp/isl+

(require "snake_lib.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;           Auxiliary List Procedures               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; drop-tail : (listof posn) -> (listof posn)



;; delete-posn : (listof posn) posn -> (listof posn)
(define (delete-posn lst p)
  ...)


;; contains-posn? : (listof posn) posn -> boolean





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Snake Game
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a game is...
;; - (make-game snake (listof posn) (listof posn) number)
(define-struct game [snake obstacles foods ticks])
;; foods and obstacles are both lists of posns.

;; a snake is...
;; - (make-snake direction (listof posn))
(define-struct snake [direction body])
;; body will always be a non-empty list of psns

;; a direction is one of...
;; - "up"
;; - "down"
;; - "left"
;; - "right"
;; That is, a direction must be one of these four strings (all lower cases).

;; body is either
;; - (cons posn empty)
;; - (cons posn body)
;; That is, body is a non-empty list of posns. 
;;
;; x-coordinates increase from 0 to 24 (inclusive) toward the right
;; y-coordinates increase from 0 to 24 (inclusive) toward the bottom

;; A starting game to experiment with
(define board-small
  (make-game (make-snake "down" (list (make-posn 0 1) (make-posn 0 0)))
             (list (make-posn 1 3))
             (list (make-posn 24 24))
             0))

;; A larger game example
(define board-medium
  (make-game (make-snake "right" (list (make-posn 4 0) (make-posn 3 0)))
             (list (make-posn 1 0)
                   (make-posn 8 23)
                   (make-posn 14 5)
                   (make-posn 15 5)
                   (make-posn 16 5)
                   (make-posn 16 6))
             (list (make-posn 1 3) 
                   (make-posn 5 20)
                   (make-posn 6 11)
                   (make-posn 15 15))
             0))

;; copy-game/new-food : posn game -> game
;;   Given a posn and a game, returns a new game where food has been added at that posn.
(define (copy-game/new-food p g)
  (make-game (game-snake g)
             (game-obstacles g)
             (cons p (game-foods g))
             (game-ticks g)))

(check-expect
 (copy-game/new-food (make-posn 6 7)
                     (make-game (make-snake "up" (list (make-posn 1 2)))
                                (list (make-posn 10 10)
                                      (make-posn 20 20))
                                (list (make-posn 3 4))
                                5))
 (make-game (make-snake "up" (list (make-posn 1 2)))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            (list (make-posn 6 7) (make-posn 3 4))
            5))

;; copy-game/remove-food : posn game -> game
;;   Given a posn and a game, returns a new game where food at that posn has been removed.
(define (copy-game/remove-food p g)
  (make-game (game-snake g)
             (game-obstacles g)
             (delete-posn (game-foods g) p)
             (game-ticks g)))

(check-expect
 (copy-game/remove-food (make-posn 6 7)
                        (make-game (make-snake "up" (list (make-posn 1 2)))
                                   (list (make-posn 10 10)
                                         (make-posn 20 20))
                                   (list (make-posn 6 7) (make-posn 3 4))
                                   5))
 (make-game (make-snake "up" (list (make-posn 1 2)))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            (list (make-posn 3 4))
            5))

;; copy-game/new-snake-direction : direction game -> game


(check-expect
 (copy-game/new-snake-direction
  "left"
  (make-game (make-snake "down" (list (make-posn 1 2)))
             empty
             (list (make-posn 3 4))
             5))
 (make-game (make-snake "left" (list (make-posn 1 2)))
            empty
            (list (make-posn 3 4))
            5))

;; copy-game/add-snake-head : posn game -> game



;; copy-game/drop-snake-tail : game -> game


;; game-over? : game -> boolean


(check-expect 
 (game-over? (make-game (make-snake "up" (list (make-posn 25 1))) empty empty 5))
 #true)


;; adjacent-posn : posn direction -> posn

;; next-game-state : game -> game


(check-expect
 (next-game-state
  (make-game (make-snake "up" (list (make-posn 2 2) (make-posn 2 3) (make-posn 3 3)))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             empty
             5))
 (make-game (make-snake "up" (list (make-posn 2 1) (make-posn 2 2) (make-posn 2 3)))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            empty
            5))


;; play : board -> board
(define (play initial-board)
  (play-game initial-board))

;to start a game
;(play board-medium)

;; DON'T CHANGE EXISTING check-expectS!!!
;; These are just tests to make sure all of your images are named correctly for grading
(check-satisfied drop-tail procedure?)
(check-satisfied delete-posn procedure?)
(check-satisfied contains-posn? procedure?)
(check-satisfied board-medium-visualized image?)
(check-satisfied copy-game/new-food procedure?)
(check-satisfied copy-game/remove-food procedure?)
(check-satisfied copy-game/new-snake-direction procedure?)
(check-satisfied copy-game/add-snake-head procedure?)
(check-satisfied copy-game/drop-snake-tail procedure?)
(check-satisfied game-over? procedure?)
(check-satisfied adjacent-posn procedure?)
(check-satisfied next-game-state procedure?)

(game-launcher)
