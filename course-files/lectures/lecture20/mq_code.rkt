;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname mq_code) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (im-tired-of hearing about)
  (local [(define result "")]
    (begin (set! result (string-append hearing result))
           (set! result (string-append result " and "))
           (set! result (string-append result about))
           result)))


(define z 5)

(define (foo z)
  (- z 1))
(foo 5)
(foo 4)
(foo z)

(define (bar z)
  (* z z))
(bar 6)
(bar z)
(local [(define z 3)]
  (bar z))

(define (baz y)
  (+ z y))
(baz 2)
(baz z)


(define x "hello")
(local [(define x 1)]
  (set! x (+ x 1)))
x

(define y "hello")
(local [(define y 1)]
  (begin (set! y (+ y 1))
         y))
y
