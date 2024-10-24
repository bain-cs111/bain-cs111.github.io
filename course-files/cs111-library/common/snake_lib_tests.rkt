#lang racket/base

(require rackunit
         (submod "snake_lib.rkt" internal))

(define-struct game [snake obstacles foods ticks])
(define-struct snake [direction body])

;; Usage:
;; - Run: raco test snake_lib_test.rkt
;; - Or: Run in DrRacket. Make sure the test submodule is
;;       checked in Language -> Choose Language ->
;;       The Racket Language -> Submodules to Run
(module+ test
  (check-equal? 
   (num-free-posns
    (make-game (make-snake "up" (list (make-posn 5 1))) 
               empty
               (list (make-posn 9 1) (make-posn 9 2))
               5))
   (- (sqr board-length) 1 3))

  (check-equal? 
   (nth-free-posn
    0
    (make-posn 1 1)
    (make-game (make-snake "up" (list (make-posn 9 9))) empty empty 5))
   (make-posn 1 1))
  (check-equal? 
   (nth-free-posn
    1
    (make-posn 1 1)
    (make-game (make-snake "up" (list (make-posn 9 9))) empty empty 5))
   (make-posn 2 1))
  (check-equal? 
   (nth-free-posn
    board-length
    (make-posn 1 1)
    (make-game (make-snake "up" (list (make-posn 9 9))) empty empty 5))
   (make-posn 1 2))
  (check-equal? 
   (nth-free-posn
    board-length
    (make-posn 1 1)
    (make-game (make-snake "up" (list (make-posn 5 1))) empty (list (make-posn 9 1)) 5))
   (make-posn 3 2))

  (check-equal? 
   (next-free-posn
    (make-posn 24 2)
    (make-game (make-snake "up" (list (make-posn 0 3)))
               empty
               (list (make-posn 24 2))
               5))
   (make-posn 1 3))

  (check-equal? (next-posn (make-posn 1 1)) (make-posn 2 1))
  (check-equal? (next-posn (make-posn 24 1)) (make-posn 0 2))

  (check-equal? 
   (occupied? (make-posn 1 2)
              (make-game (make-snake "up" (list (make-posn 1 2)))
                         empty
                         (list (make-posn 3 4))
                         5))
   true)
  (check-equal?
   (occupied? (make-posn 1 2)
              (make-game (make-snake "up" (list (make-posn 3 4)))
                         empty
                         (list (make-posn 1 2))
                         5))
   true)
  (check-equal? 
   (occupied? (make-posn 0 2)
              (make-game (make-snake "up" (list (make-posn 1 2)))
                         empty
                         (list (make-posn 3 4))
                         5))
   false)
  )
