;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname more_practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "define_super_struct.rkt")

(define-struct cat (name weight breed))

; Constructor: make-cat
; Predicate: cat?
; Accessors: cat-name, cat-weight, cat-breed
; Mutators: set-cat-name!, set-cat-weight!, set-cat-breed!
