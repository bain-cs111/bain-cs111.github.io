#lang htdp/isl+

(require "foreign.rkt")
(require 2htdp/image)
(require 2htdp/universe)

(provide (struct-out game)
         (struct-out snake)
         play-game
         board-length)

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

; a game is
; (make-game snake food obstacles nat) 
(define-struct game [snake food obstacles ticks]) 

; a direction is either
; - 'up
; - 'down
; - 'left
; - 'right

; a snake is
; (make-snake direction body)
(define-struct snake [heading segments])

; a body is either
; - (cons posn empty)
; - (cons posn body)
; x-coordinates increase from 1 to board-length (inclusive) toward the right
; y-coordinates increase from 1 to board-length (inclusive) toward the top
; the default value for board-length is 50.


; a food is either
; - empty
; - (cons posn food)

; obstacles is either
; - empty
; - (cons posn obstacles)

;; random-free-posn : game -> posn
(define (random-free-posn g)
  (nth-free-posn (random (num-free-posns g)) (make-posn 1 1) g))

;; num-free-posns : game -> posn
(define (num-free-posns g)
  (- (sqr board-length)
     (length (snake-segments (game-snake g)))
     (length (game-food g))))

(provide-for-test num-free-posns)

;; nth-free-posn : nat posn game -> posn
(define (nth-free-posn n p g)
  (cond [(zero? n) (next-free-posn p g)]
        [else
         (nth-free-posn (- n 1) (next-posn (next-free-posn p g)) g)]))

(provide-for-test nth-free-posn)

;; next-free-posn : posn game -> posn
(define (next-free-posn p g)
  (cond [(occupied? p g)
         (next-free-posn 
          (make-posn
           (cond [(= (posn-x p) board-length) 1]
                 [else (+ (posn-x p) 1)])
           (cond [(= (posn-x p) board-length) (+ (posn-y p) 1)]
                 [else (posn-y p)]))
          g)]
        [else p]))

(provide-for-test next-free-posn)

;; next-posn : posn -> posn
(define (next-posn p)
  (make-posn
   (cond [(= (posn-x p) board-length) 1]
         [else (+ (posn-x p) 1)])
   (cond [(= (posn-x p) board-length) (+ (posn-y p) 1)]
         [else (posn-y p)])))

(provide-for-test next-posn)

;; occupied? : posn game -> boolean
(define (occupied? p g)
  (or (member p (game-food g))
      (member p (snake-segments (game-snake g)))))

(provide-for-test occupied?)

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

;; play-game : game
;              (game -> game) 
;;             (game direction -> game)
;;             (game posn -> game)
;;             (game -> nat) 
;;             (game -> boolean) 
;;             -> game
(define (play-game initial-game advance-game add-food change-direction game-score game-over?)
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
                    (define f (advance-game g))]
              (cond [(= (length (game-food f)) morsels) f]
                    [else (add-food f (random-free-posn f))])))
          
          ;; game-key : game key-event -> game
          (define (game-key g e)
            (cond [(member e '("up" "down" "left" "right"))
                   (change-direction g (string->symbol e))]
                  [else g]))]
    (big-bang initial-game
              (on-tick game-tick tick-freq)
              (on-key game-key)
              (stop-when game-over? render-game)
              (to-draw render-game))))

(provide-for-test empty sqr true false
                  make-posn posn-x posn-y)
