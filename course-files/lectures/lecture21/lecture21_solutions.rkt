;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname lecture21_solutions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "define_super_struct.rkt")

; recall structures....

; An album is:
; - (make-album symbol symbol symbol)
(define-struct album (title artist genre))
;make-album
;album?
;album-title, album-artist, album-genre

(make-album "Blood, Sweat and Tears" "Johnny Cash" "Country")
(define my-album (make-album "Blood, Sweat and Tears" "Johnny Cash" "Country"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a snake is:
; - (make-snake number symbol)
(define-struct snake (weight food))
; make-snake
; snake?
; snake-weight, snake-food
; set-snake-weight!, set-snake-food!

; feed-snake: Snake -> Snake
; feeds the snake a 5lb meal
(define (feed-snake s)
  (make-snake (+ 5 (snake-weight s)) (snake-food s)))

(check-expect (feed-snake (make-snake 4 'rat)) (make-snake 9 'rat))
(check-expect (feed-snake (make-snake 9 'mouse)) (make-snake 14 'mouse))

; assignment - changing a variables value
(define a 5)
(set! a 6)

; mutation - changing the value of a field of an object
(define bob (make-snake 11 'birds))
(set! bob (make-snake 12 'crickets)) ; not mutation, this is assignment
(set-snake-food! bob 'grass)

; feed-snake!: snake -> void
; feeds the snake a 5lb meal
; Effect: the input snake weighs 5lb more
(define (feed-snake! s)
  (set-snake-weight! s (+ 5 (snake-weight s))))

(define a-snake (make-snake 4 'rat))
(check-expect (begin (feed-snake! a-snake)
                     (snake-weight a-snake)) 9) ; checking side effect, mutation
(check-expect (feed-snake! a-snake) (void)) ; checking output

; an armadillo is
; (make-armadillo number boolean)
(define-struct armadillo (weight dead?))
; make-armadillo
; armadillo?
; armadillo-weight, armadillo-dead?
; set-armadillo-weight!, set-armadillo-dead?!


; feed-armadillo: armadillo -> armadillo 
; feeds a armadillo a 2lb meal if it isn't dead
(define (feed-armadillo d)
  (cond [(armadillo-dead? d) (make-armadillo (armadillo-weight d)
                                             true)]  ;or just return d
        [else            (make-armadillo (+ 2 (armadillo-weight d))
                                         false)]))

(check-expect (feed-armadillo (make-armadillo 12 false)) 
              (make-armadillo 14 false))
(check-expect (feed-armadillo (make-armadillo 11 true)) 
              (make-armadillo 11 true))


; feed-armadillo!: armadillo -> void 
; feeds a armadillo a 2lb meal if it isn't dead
; Effect: an alive armadillo is 2lb larger
(define (feed-armadillo! d)
  (when (not (armadillo-dead? d)) ; (unless (armadillo-dead? d)
    (set-armadillo-weight! d (+ 2 (armadillo-weight d)))))

(define f (make-armadillo 12 false))
(define g (make-armadillo 13 true))
(check-expect (begin (feed-armadillo! f)
                     (armadillo-weight f))
              14)
(check-expect (begin (feed-armadillo! g)
                     (armadillo-weight g))
              13)

; an ant is
; - (make-ant number posn)
(define-struct ant (weight loc))
; make-ant
; ant?
; ant-weight, ant-loc
; set-ant-weight!, set-ant-loc!

(define ant1 (make-ant 0.01 (make-posn 0 0)))
(define ant2 (make-ant 0.005 (make-posn 2 3)))

; feed-ant: Ant -> Ant
; feeds ant a 0.01lb meal
(define (feed-ant a)
  (make-ant (+ (ant-weight a) 0.01) (ant-loc a)))

(check-expect (feed-ant (make-ant 0.01 (make-posn 0 0)))
              (make-ant 0.02 (make-posn 0 0)))

; feed-ant!: Ant -> void
; feeds ant a 0.01lb meal
; Effect: ant is 0.01 larger
(define (feed-ant! a)
  (set-ant-weight! a (+ 0.01 (ant-weight a))))

(define alden (make-ant 0.01 (make-posn 0 0)))
(check-expect (begin (feed-ant! alden)
                     (ant-weight alden))
              0.02)



;; This is an example of Type Dispatch

; weight: animal -> number
; returns the weight of the animal
(define (weight a)
  (cond [(snake? a)        (snake-weight a)]
        [(armadillo? a)    (armadillo-weight a)]
        [(ant? a)          (ant-weight a)])) 

; zoo-weight: (listof animals) -> Number
; takes a list of animals and returns the sum of their weights
(define (zoo-weight loa)
  (cond [(empty? loa)        0]
        [else                (+ (weight (first loa))
                                (zoo-weight (rest loa)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Motivation for subtyping and inheritance
;    to write zoo-weight, we had to have this cond
;    that used the predicates to ask what kind of
;    animal it was before extracting its weight.
;    this is bad...

; an animal is:
; - (make-animal string number number)
(define-struct animal (name weight age))
; make-animal
; animal?
; animal-name, animal-weight, animal-age
; set-animal-name!, set-animal-weight!, set-animal-age!

; a cat is:
; - (make-cat string number number string) 
(define-struct (cat animal) (sleeping-spot))
; make-cat
; cat?
; cat-sleeping-spot
; set-cat-sleeping-spot!
; cat-weight, cat-age, cat-name XXXX not valid
; use the selectors and mutators from parent type

; a dog is:
; - (make-dog string number number string) 
(define-struct (dog animal) (best-friend))
; make-dog
; dog?
; dog-best-friend
; set-dog-best-friend!

; a mouse is:
; - (make-mouse string number number string) 
(define-struct (mouse animal) (hiding-spot))
; make-mouse
; mouse?
; mouse-hiding-spot
; set-mouse-hiding-spot!

(define pixel (make-cat "pixel" 12 15 "outside"))
(define gizmo (make-dog "gizmo" 12 2 "sanjay"))
(define ralph (make-mouse "ralph" 0.5 1 "under the fridge"))

; zoo-weight-2: list-of-animals -> Number
; takes a list of animals and returns a sum of their weights
(define (zoo-weight-2 zoo)
  (cond [(empty? zoo)    0]
        [else            (+ (animal-weight (first zoo))
                            (zoo-weight-2 (rest zoo)))]))

(check-expect (zoo-weight-2 (list pixel ralph gizmo))
              24.5)

; make-animal-older!: animal -> void
; takes an animal and ages that animal by 1 year
; Effect: the animal is 1 year older
(define (make-animal-older! anim)
  (set-animal-age! anim (+ 1 (animal-age anim))))

(define pixel-age-before-test (animal-age pixel))
(check-expect (begin (make-animal-older! pixel)
                     (animal-age pixel))
              (+ 1 pixel-age-before-test))

; feed-animal!: animal -> void
; feeds an animal a 2 lb meal
; Effect: the animal is 2lbs heavier
(define (feed-animal! anim)
  (set-animal-weight! anim (+ 2 (animal-weight anim))))

(define pixel-weight-before-test (animal-weight pixel))
(check-expect (begin (feed-animal! pixel)
                     (animal-weight pixel))
              (+ 2 pixel-weight-before-test))

; grow-zoo!: list-of-animals -> void
; takes a list of animals and adds one to the weight of each animal
; Effect: Each animal in the zoo is 2 pound heavier
(define (grow-zoo! loa)
  (for-each feed-animal! ; (lambda (a) (set-animal-weight! a (+ 1 (animal-weight a)))))
            loa))

(define myzoo (list gizmo ralph))
(define zoo-weight-before (zoo-weight-2 myzoo))
(check-expect (begin (grow-zoo! myzoo)
                     (zoo-weight-2 myzoo))
              (+ zoo-weight-before 4))