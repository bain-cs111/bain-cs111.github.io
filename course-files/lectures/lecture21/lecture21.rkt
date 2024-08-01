;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname lecture21) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "define_super_struct.rkt")

; recall structures....

; An album is:
; - (make-album string string string)
(define-struct album (title artist genre))

;; Constructor
; make-album: string string string -> album

;; Accessors
; album-title:  album -> string
; album-artist: album -> string
; album-genre:  album -> string

;; Predicate
; album?: any -> boolean

;; Anything new?

(define my-album (make-album "Montero"
                             "Lil Nas X"
                             "Hip-Hop"))




; assignment - changing a variables value

; mutation - changing the value of a field of an object




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a snake is:
; - (make-snake number symbol)
;(define-struct snake (weight food))

; Practice listing the automatically created functions...

; feed-snake: Snake -> Snake
; feeds the snake a 5lb meal
;(define (feed-snake s)
;  (make-snake (+ 5 (snake-weight s))
;              (snake-food s)))

;(check-expect (feed-snake (make-snake 4 'rat))
;              (make-snake 9 'rat))
;(check-expect (feed-snake (make-snake 9 'mouse))
;              (make-snake 14 'mouse))

; feed-snake!
;;; FILL THIS IN

;(define a-snake (make-snake 4 'rat))
;(check-expect (begin (feed-snake! a-snake)
;                     (snake-weight a-snake)) 9) ; checking side effect, mutation
;(check-expect (feed-snake! a-snake) (void)) ; checking output


; an armadillo is
; (make-armadillo number boolean)
;(define-struct armadillo (weight dead?))

; feed-armadillo: armadillo -> armadillo
; feeds a armadillo a 2lb meal if it isn't dead
;(define (feed-armadillo d)
;  (cond [(armadillo-dead? d) (make-armadillo (armadillo-weight d)
;                                             true)]  ; or just return d
;        [else                (make-armadillo (+ 2 (armadillo-weight d))
;                                             false)]))

;(check-expect (feed-armadillo (make-armadillo 12 false)) 
;              (make-armadillo 14 false))
;(check-expect (feed-armadillo (make-armadillo 11 true)) 
;              (make-armadillo 11 true))

; feed-armadillo!
;;; FILL THIS IN

;(define armadillo-1 (make-armadillo 12 false))
;(define armadillo-2 (make-armadillo 13 true))
;(check-expect (begin (feed-armadillo! armadillo-1)
;                     (armadillo-weight armadillo-1))
;              14)
;(check-expect (begin (feed-armadillo! armadillo-2)
;                     (armadillo-weight armadillo-2))
;              13)










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; an ant is
; - (make-ant number posn)
;(define-struct ant (weight loc))
; make-ant
; ant?
; ant-weight, ant-loc
; set-ant-weight!, set-ant-loc!

;(define antonio-banderas (make-ant 0.01 (make-posn 0 0)))
;(define ulysses-s-grant  (make-ant 0.005 (make-posn 2 3)))
;
;; feed-ant: Ant -> Ant
;; feeds ant a 0.01lb meal
;(define (feed-ant a)
;  (make-ant (+ (ant-weight a) 0.01) (ant-loc a)))
;
;(check-expect (feed-ant (make-ant 0.01 (make-posn 0 0)))
;              (make-ant 0.02 (make-posn 0 0)))
;
;; feed-ant!: Ant -> void
;; feeds ant a 0.01lb meal
;; Effect: ant is 0.01 larger
;(define (feed-ant! a)
;  (set-ant-weight! a (+ 0.01 (ant-weight a))))
;
;(define marc-antony (make-ant 0.01 (make-posn 0 0)))
;(check-expect (begin (feed-ant! marc-antony)
;                     (ant-weight marc-antony))
;              0.02)


;;;;;;;;;;;;;;;;;;;;;; TYPE DISPATCH
;
;(define my-first-zoo (list a-snake
;                           armadillo-1
;                           armadillo-2
;                           marc-antony
;                           antonio-banderas
;                           ulysses-s-grant))

; weight: animal -> number
; returns the weight of the animal

; zoo-weight: (listof animals) -> Number
; takes a list of animals and returns the sum of their weights

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Motivation for subtyping and inheritance
;    to write zoo-weight, we had to have this cond
;    that used the predicates to ask what kind of
;    animal it was before extracting its weight.
;    this is bad...

;; an animal is:
;; - (make-animal string number number)
;(define-struct animal (name weight age))
;
;; a cat is:
;; - (make-cat string number number string) 
;(define-struct (cat animal) (sleeping-spot))
;
;; a dog is:
;; - (make-dog string number number string) 
;(define-struct (dog animal) (best-friend))
;
;; a mouse is:
;; - (make-mouse string number number string) 
;(define-struct (mouse animal) (hiding-spot))
;
;
;
;(define figaro (make-cat "figaro"
;                         12
;                         15
;                         "horseshoe"))
;
;(define molly  (make-dog "molly"
;                         12
;                         2
;                         "emily"))
;
;(define ralph (make-mouse "ralph"
;                          0.5
;                          1
;                          "under the fridge"))
;
;(define full-house (list figaro molly ralph)

; zoo-weight-2: list-of-animals -> Number
; takes a list of animals and returns a sum of their weights

;; FILL THIS IN

; make-animal-older!: animal -> void
; takes an animal and ages that animal by 1 year
; Effect: the animal is 1 year older

;; FILL THIS IN

;(define molly-age-before-test (animal-age molly))
;(check-expect (begin (make-animal-older! molly)
;                     (animal-age molly))
;              (+ 1 molly-age-before-test))

; feed-animal!: animal -> void
; feeds an animal a 2 lb meal
; Effect: the animal is 2lbs heavier

;; FILL THIS IN

;(define molly-weight-before-test (animal-weight molly))
;(check-expect (begin (feed-animal! molly)
;                     (animal-weight molly))
;              (+ 2 molly-weight-before-test))

; grow-zoo!: list-of-animals -> void
; takes a list of animals and adds two to the weight of each animal
; Effect: Each animal in the zoo is 2 pound heavier

;; FILL THIS IN

;(define myzoo (list figaro ralph))

;(define zoo-weight-before (zoo-weight-2 myzoo))
;(check-expect (begin (grow-zoo! myzoo)
;                     (zoo-weight-2 myzoo))
;              (+ zoo-weight-before 4))