#lang htdp/asl
(require "define_super_struct.rkt")

;; Sub-types & Inheritance
(define-struct building (name year-completed))

(define-struct (tower building) (stories height)
  #:methods
  ; add-floor : tower -> (void)
  ; Effect: adds a story to the given tower
  (define (add-floor! t)
    (begin (set-tower-stories! t (+ 1 (tower-stories t)))
           (set-tower-height! t (+ 10 (tower-height t))))))

;; Overriding and Type Dispatch
(define-struct car (mileage)
  #:methods
  ; drive! : car -> number
  ; Returns the current mileage of a car after a drive
  ; Effect: adds 5 miles to the given car's mileage
  (define (drive! c)
    (begin (set-car-mileage! (+ 5 (car-mileage c)))
           (car-mileage c))))

(define-struct (semi-truck car) ()
  #:methods
  ; drive! : semi-truck -> number
  ; Returns the current mileage of a semi-truck after a drive
  ; Effect: adds 100 miles to the given semi-truck's mileage
  (define (drive! t)
    (begin (set-car-mileage! t (+ 100 (car-mileage t)))
           (car-mileage t))))

(define my-truck (make-semi-truck 1000))
(drive! my-truck)

;; HashMaps
(define blank-map (make-hash)) ; construct an empty hashmap

; Construct a HashMap where keys are english words, and values
;   are spanish translations
(define eng2sp (make-hash (list (list "january" "enero")
                                (list "february" "febrero")
                                (list "march" "marzo")
                                (list "april" "abril"))))

; Accessing a specific value via a key
(hash-ref  eng2sp "february" "")

; Before mutation
(hash-ref  eng2sp "november" "unknown")
; Mutation
(hash-set! eng2sp "november" "noviembre")
; After mutation
(hash-ref  eng2sp "november" "unknown")