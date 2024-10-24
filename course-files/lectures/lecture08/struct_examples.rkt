#lang htdp/isl+
; posn examples

; posn datatype is built in to Racket, but if it weren't, you would define it like this:
; (define-struct posn (x y))

; constructor
(define a (make-posn 1 2))
(define b (make-posn 4624 1))

; accessors
(posn-x a)
(posn-y b)

; predicate?
(posn? a)

; flip: posn -> posn
 ;to swap the x and y coordinates of a posn
(define (flip p)
  (make-posn (posn-y p) (posn-x p)))

(check-expect (flip (make-posn 1 2)) (make-posn 2 1))

;max-part: posn -> Number
;to return the maximum coordinate of the posn
(define (max-part p)
  (if (> (posn-x p) (posn-y p))
      (posn-x p)
      (posn-y p)))

(check-expect (max-part (make-posn 1 2)) 2)
(check-expect (max-part (make-posn 3 3)) 3)

;;CHALLENGE
;;write max-part using max
;;write max-part using lambda form


;;; Now let's make some animals!

; a snake is:
; - (make-snake number symbol)
(define-struct snake (weight food))

; constructor
(define joe (make-snake 47 "rabbits"))

; accessors
(snake-weight joe)
(snake-food joe)

;predicate
(snake? joe)

; skinny-snake?: Snake -> Boolean
; Returns true if snake is less than 10 pounds, false otherwise
(define (skinny-snake? s)
  (< (snake-weight s) 10))

; (if TEST
;     true
;     false)
; replace the above with TEST

(check-expect (skinny-snake? joe) false)
(check-expect (skinny-snake? (make-snake 8 "mice")) true)

; feed-snake: Snake -> Snake
; feeds the snake a 5lb meal
(define (feed-snake s)
  (make-snake (+ 5 (snake-weight s))
              (snake-food s)))

(check-expect (feed-snake (make-snake 4 "rat"))
              (make-snake 9 "rat"))
(check-expect (feed-snake joe)
              (make-snake 52 "rabbits"))

; a dillo is
; - (make-dillo number boolean)
(define-struct dillo (weight dead?))

; constructor - make-dillo
; accessors - dillo-weight, dillo-dead?
; predicate - dillo?

; run-over: Dillo -> Dillo
; kills a dillo
(define (run-over d)
  (make-dillo (dillo-weight d) true))

(check-expect (run-over (make-dillo 12 true))
              (make-dillo 12 true))
(check-expect (run-over (make-dillo 11 false))
              (make-dillo 11 true))

; feed-dillo: Dillo -> Dillo
; feeds a dillo a 2lb meal if it isn't dead
(define (feed-dillo d)
  (if (dillo-dead? d)
      d
      (make-dillo (+ 2 (dillo-weight d))
                  (dillo-dead? d))))

(check-expect (feed-dillo (make-dillo 12 true))
              (make-dillo 12 true))
(check-expect (feed-dillo (make-dillo 12 false))
              (make-dillo 14 false))

;an ant is:
;-(make-ant number posn)
(define-struct ant (weight loc))

(define ant1 (make-ant 0.01 (make-posn 0 0)))
(define ant2 (make-ant 0.005 (make-posn 2 3)))

;constructor - make-ant
;selectors - ant-weight, ant-loc
;predicate - ant?

;ant-at-home?: ant-> boolean
; returns whether the ant is at the origin
(define (ant-at-home? a)
  (and (= 0 (posn-x (ant-loc a)))
       (= 0 (posn-y (ant-loc a)))))

(check-expect (ant-at-home? ant1) true)
(check-expect (ant-at-home? ant2) false)

;; Challenge
;; use local to save us from calling ant-loc twice

; feed-ant: ant -> ant
; feeds ant a 0.01 lb meal
(define (feed-ant a)
  (make-ant (+ 0.01 (ant-weight a))
            (ant-loc a)))

(check-expect (feed-ant ant1) (make-ant 0.02 (make-posn 0 0)))
(check-expect (feed-ant ant2) (make-ant 0.015 (make-posn 2 3)))

;an animal is either:
; - snake, or
; - dillo, or
; - ant



; feed-animal: animal -> animal
; feeds the animal appropriately-sized meal
(define (feed-animal a)
  (cond [(snake? a)   (feed-snake a)]
        [(dillo? a)   (feed-dillo a)]
        [(ant? a)     (feed-ant a)]))

(check-expect (feed-animal ant1) (make-ant 0.02 (make-posn 0 0)))
(check-expect (feed-animal (make-dillo 12 true))
              (make-dillo 12 true))
(check-expect (feed-animal joe)
              (make-snake 52 "rabbits"))




  








              
























