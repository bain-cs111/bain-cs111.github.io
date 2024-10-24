#lang racket

(require racket/draw "noise.rkt")

(provide flip-horizontal flip-vertical replicate map-pattern
         render
         point point-x point-y point-+ point-- point-* point-/
         magnitude
         turbulence clouds noise-vector
         horizontal-bars
         vertical-bars)

;;;;
;;;; Procedural shading in 2D
;;;; A pattern is a function from points to brightnesses:
;;;;    pattern : point% -> byte
;;;; Patterns are essentially infinite resolution images.
;;;; Use the show procedure to visualize one as a bitmap
;;;;

;;;
;;; Pattern combinators
;;;

(define (flip-horizontal pattern)
  (λ (p)
    (pattern (point (- (point-x p))
                    (point-y p)))))

(define (flip-vertical pattern)
  (λ (p)
    (pattern (point (point-x p)
                    (- (point-y p))))))

(define replicate
  ;; Returns an infinitely replicated version of the width×height subimage of pattern.
  (λ (width height pattern)
    (λ (p)
      (pattern (point (modulo (point-x p)
                              width)
                      (modulo (point-y p)
                              height))))))



(define map-pattern
  ;; Make a new pattern by applying function to respective points of patterns.
  (case-lambda
    [(function pattern)
     (λ (p)
       (function (pattern p)))]
    [(function . patterns)
     (λ (p)
       (apply function
              (map (λ (pattern)
                     (pattern p))
                   patterns)))]))

;;;
;;; Visualization
;;;

(define (render f [width 300] [height 300])
  (define bitmap
    (make-object bitmap% width height))
  (define pixel-data
    (make-bytes (* width height 4)
                255))
  (define (clamp-pixel-value n)
  (min 255
       (max 0
            ;; Surely there must be an easier way than this to coerce a float to a fixnum?
            (inexact->exact (floor n)))))
  (let loop ((x 0)
             (y 0)
             (index 0))
    (define pixel (clamp-pixel-value (f (point x y))))
    ;; Alpha preset to 255 - set red
    (bytes-set! pixel-data
                (+ index 1)
                pixel)
    ;; Set green
    (bytes-set! pixel-data
                (+ index 2)
                pixel)
    ;; Set blue
    (bytes-set! pixel-data
                (+ index 3)
                pixel)
    (if (= x (- width 1))
        (if (= y (- height 1))
            (begin (send bitmap
                         set-argb-pixels
                         0 0
                         width height
                         pixel-data)
                   bitmap)
            (loop 0
                  (+ y 1)
                  (+ index 4)))
        (loop (+ x 1)
              y
              (+ index 4)))))

;;;
;;; Point operations
;;;

(define printing-point%
  (class* point% (writable<%>)
    (define/public (custom-write port)
      (fprintf port "(point ~a ~a)" (point-x this) (point-y this)))
    (define/public (custom-display port)
      (custom-write port))
    (super-new)))

(define (point x y)
  (make-object printing-point% x y))

(define (point-x p)
  (send p get-x))

(define (point-y p)
  (send p get-y))

;; I take it there's no way to extend the arithmetic operators other than to define a
;; new version and export it from your module?
(define (point-+ p1 p2)
  (point (+ (point-x p1)
            (point-x p2))
         (+ (point-y p1)
            (point-y p2))))

(define (point-- p1 p2)
  (point (- (point-x p1)
            (point-x p2))
         (- (point-y p1)
            (point-y p2))))

(define (point-* number p)
  (point (* number (point-x p))
         (* number (point-y p))))

(define (point-/ p number)
  (point-* (/ 1.0 number)
           p))

(define (magnitude p)
  (sqrt (+ (square (point-x p))
           (square (point-y p)))))

(define (square x)
  (* x x))

(define (turbulence p)
  (turbul (exact->inexact (point-x p))
          (exact->inexact (point-y p))))

(define clouds
  (λ (point) (* 350
                (turbulence (point-* 0.01
                                     point)))))

(define horizontal-bars
  (λ (p)
    (* 255
       (sin (* 0.3
               (point-y p))))))

(define vertical-bars
  (λ (p)
    (* 255
       (sin (* 0.3
               (point-x p))))))

(define noise-vector
  (λ (p strength frequency)
    (point (* strength
              (turbulence (point-* frequency p)))
           (* strength
              (turbulence (point-* frequency
                                   (point (point-y p)
                                          (point-x p))))))))


    