#lang htdp/isl+
(require "./remove_duplicates.rkt")

; This defines the basic track datatype.
(define-struct track (title artist genre))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Below are two exemplar track libraries and a few example tests
;;; for SOME functions.
;;;
;;; ALL functions in this assignment should work for ANY given
;;; library, and the test cases below illustrate how libraries
;;; are passed to track functions.
;;;
;;; These test cases are far from complete. Remember to write new
;;; track library and tests to see if your solution works as expected.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define example-testing-library-1
  (list (make-track "My Love Goes On" "James Morrison" "Pop")
        (make-track "Bootleg"         "Kenshi Yonezu"  "J-Pop")))

(define example-testing-library-2
  (list (make-track "Nocturne in F Major, No. 1"           "Arthur Rubinstein" "Classical")
        (make-track "Maple Leaf Rag"                       "Joshua Rifkin"     "Rag")
        (make-track "Violin Sonata No. 32 in B flat major" "Lev Oborin"        "Classical")))

(define prof-bains-music-library
  (list (make-track "You Belong with Me" "Taylor Swift" "Country pop")
        (make-track "Cruel Summer"       "Taylor Swift" "Synth-pop")
        (make-track "Fearless"           "Taylor Swift" "Country pop")
        (make-track "Clean"              "Taylor Swift" "Synth-folk")
        (make-track "Easily"             "Bruno Major"  "R&B")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 0: defining your own libraries
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Enter two list of tracks (library) below
;;; They need not be the actual tracks you own.
;;; But you should include enough variety to adequately
;;; test your code.

(define hip-hop-lib
  "fill me in")

(define pop-lib
  "fill me in")



;;; Add the functions you write (e.g. all-genres, multi-genre-artists)
;;; below.  Be sure to test your functions to make sure they work.
;;; We only provide very few test cases this time, so you need
;;; to write your own test cases to make sure the code works.
;;; We will use our own test cases when grading and assign you
;;; a grade based on the number of test cases that passed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 1: all-titles
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; all-titles: (listof track) -> (listof string)

(check-expect (all-titles example-testing-library-1)
              (list "My Love Goes On"
                    "Bootleg"))

(check-expect (all-titles example-testing-library-2)
              (list "Nocturne in F Major, No. 1"
                    "Maple Leaf Rag"
                    "Violin Sonata No. 32 in B flat major"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 2: all-artists
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; all-artists: (listof track) -> (listof string)

(check-expect (all-artists prof-bains-music-library)
              ...) ;; <= fill me in!




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 3: all-genres
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; all-genres: (listof track) -> (listof string)

(check-expect (all-genres (append hip-hop-lib pop-lib))
              ...) ;; <= fill me in!




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 4: artist-tracks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; artist-tracks: string (listof track) -> (listof track)



(check-expect (artist-tracks "Scott Joplin" example-testing-library-2)
              (list))

(check-expect (artist-tracks "Joshua Rifkin" example-testing-library-2)
              (list (make-track "Maple Leaf Rag" "Joshua Rifkin" "Rag")))

(check-expect (artist-tracks "Joshua Rifkin" example-testing-library-1)
              (list))

(check-expect (artist-tracks "Kenshi Yonezu" example-testing-library-1)
              (list (make-track "Bootleg" "Kenshi Yonezu" "J-Pop")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 5: artist-genres
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; artist-genres: string (listof track) -> (listof string)

; Don't forget to write some check-expects!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 6: artist-is-multi-genre?
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; artist-is-multi-genre?: string, (listof track) -> boolean

(check-expect (artist-is-multi-genre? "James Morrison" example-testing-library-1)
              #false)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 7: multi-genre-artists
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; multi-genre-artists: (listof track) -> (listof string)



; Don't forget to write some check-expects!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 8: artist-track-counts
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; artist-track-counts: (listof track) -> (listof (list string number))

(check-expect (artist-track-counts example-testing-library-1)
              (list (list "James Morrison" 1)
                    (list "Kenshi Yonezu" 1)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 9: genre-track-counts
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; genre-track-counts: (listof track) -> (listof (list string number))




; Don't forget to write some check-expects!

;; NOTE: Remember that "function" and "procedure" are the same thing! Racket's predicate
;; for seeing if something is a function is called procedure?
(check-expect (list? hip-hop-lib) #true)
(check-expect (list? pop-lib) #true)
(check-expect (procedure? all-titles) #true)
(check-expect (procedure? all-artists) #true)
(check-expect (procedure? all-genres) #true)
(check-expect (procedure? artist-tracks) #true)
(check-expect (procedure? artist-genres) #true)
(check-expect (procedure? artist-is-multi-genre?) #true)
(check-expect (procedure? multi-genre-artists) #true)
(check-expect (procedure? artist-track-counts) #true)
(check-expect (procedure? genre-track-counts) #true)
