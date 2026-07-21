#lang racket/base

; `remove-duplicates` is built into racket, but not
; available in the teaching languages, so this file
; is just a way of making it available inside `exercise_2.rkt`
(require racket/list)
(provide remove-duplicates)
