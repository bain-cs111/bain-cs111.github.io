#lang racket/base
(require 2htdp/image
         racket/class
         (prefix-in rd: racket/draw))

;;;
;;; Definitions for image iterators used in EECS-111
;;; Ian Horswill, 10/6/2015
;;;
;;; Please note that these definitions are a little fancier than those
;;; given in class:
;;;   - The version in class doesn't handle a count of 0 or 1
;;;   - This version gives better error messages
;;;

(provide iterated-overlay iterated-underlay iterated-beside iterated-above
         row column grid
         safe-color
         interpolate-colors)

(define (extract-color c)
  (cond
    [(and (string? c) (image-color? c))
     (define cl (send rd:the-color-database find-color c))
     (cond
       [cl
        (make-color (send cl red)
                    (send cl green)
                    (send cl blue))]
       [else c])]
    [else c]))

(define (check-real who v [expected "a real number"])
  (unless (real? v)
    (raise-argument-error who expected v)))

(define iterate
  (λ (name combiner generator count)
    (cond [(not (procedure? generator))
           (raise-argument-error name "procedure" generator)]
          [(not (procedure-arity-includes? generator 1))
           (raise-argument-error name "procedure of one argument" generator)]
          [(not (integer? count))
           (raise-argument-error name "integer" count)]
          [(< count 0)
           (raise-argument-error name "non-negative-integer" count)]
          [(= count 0)
           empty-image]
          [(= count 1)
           (generator 0)]
          [else
           (define l (build-list count generator))
           (for-each
            (lambda (i)
              (unless (image? i)
                (error name "Expected generator to return an image, but instead it returned ~a"
                       i)))
            l)
           (apply combiner
                  l)])))

(define iterated-overlay
  (λ (generator count)
    (iterate 'iterated-overlay overlay generator count)))

(define iterated-underlay
  (λ (generator count)
    (iterate 'iterated-underlay underlay generator count)))

(define iterated-above
  (λ (generator count)
    (iterate 'iterated-above above generator count)))

(define iterated-beside
  (λ (generator count)
    (iterate 'iterated-beside beside generator count)))

(define row
  (λ (item count)
    (iterated-beside (λ (ignore) item)
                     count)))

(define column
  (λ (item count)
    (iterated-above (λ (ignore) item)
                     count)))

(define grid
  (λ (item columns rows)
    (column (row item columns)
            rows)))

(define safe-color
  (case-lambda
    [(r g b)
     (check-real 'safe-color r)
     (check-real 'safe-color g)
     (check-real 'safe-color b)
     (color (safe r) (safe g) (safe b))]
    [(r g b a)
     (check-real 'safe-color r)
     (check-real 'safe-color g)
     (check-real 'safe-color b)
     (check-real 'safe-color a)
     (color (safe r) (safe g) (safe b) (safe a))]))

(define safe
  (λ (value)
    (min 255 (max 0 (inexact->exact (round value))))))

(define interpolate
  (λ (a b weight)
    (+ a
       (* weight
          (- b a)))))

(define interpolate-colors
  (λ (original-c1 original-c2 weight)
    (define c1 (extract-color original-c1))
    (define c2 (extract-color original-c2))
    (unless (color? c1)
      (raise-argument-error 'interpolate-colors
                            "a color created by (color R G B) or (color R G B A)"
                            0
                            c1
                            c2))
    (unless (color? c2)
      (raise-argument-error 'interpolate-colors
                            "a color created by (color R G B) or (color R G B A)"
                            1
                            c1
                            c2))
    (check-real 'interpolate-colors weight "a real number between 0 and 1")
    (safe-color (interpolate (color-red c1)
                             (color-red c2)
                             weight)
                (interpolate (color-green c1)
                             (color-green c2)
                             weight)
                (interpolate (color-blue c1)
                             (color-blue c2)
                             weight)
                (interpolate (color-alpha c1)
                             (color-alpha c2)
                             weight))))

(module+ test
  (require rackunit)

  (check-equal?
   (iterated-overlay
    (λ (k) (circle (* (add1 k) 50) "outline" "blue"))
    4)
   (overlay
    (circle 50 "outline" "blue")
    (circle 100 "outline" "blue")
    (circle 150 "outline" "blue")
    (circle 200 "outline" "blue")))

  (check-equal?
   (iterated-underlay
    (λ (k) (circle (* (add1 k) 50) "outline" "blue"))
    4)
   (underlay
    (circle 50 "outline" "blue")
    (circle 100 "outline" "blue")
    (circle 150 "outline" "blue")
    (circle 200 "outline" "blue")))

  (check-equal?
   (iterated-above
    (λ (k) (circle (* (add1 k) 50) "outline" "blue"))
    4)
   (above
    (circle 50 "outline" "blue")
    (circle 100 "outline" "blue")
    (circle 150 "outline" "blue")
    (circle 200 "outline" "blue")))

  (check-equal?
   (iterated-beside
    (λ (k) (circle (* (add1 k) 50) "outline" "blue"))
    4)
   (beside
    (circle 50 "outline" "blue")
    (circle 100 "outline" "blue")
    (circle 150 "outline" "blue")
    (circle 200 "outline" "blue")))

  (check-equal?
   (interpolate-colors "black" "white" 0)
   (color 0 0 0 255))

  (check-equal?
   (interpolate-colors "black" "white" 1)
   (color 255 255 255 255))

  (check-equal?
   (interpolate-colors (color 10 20 30 40)
                       (color 50 60 70 80)
                       2/5)
   (color 26 36 46 56)))
