;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise_3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./remove_duplicates.rkt")

; This defines the basic album datatype.
(define-struct album (title artist genre))
; define-struct automatically creates the following functions for you:
;
; `make-<struct-name>` (in this case `make-album`)
;   a function to create an instance of the struct
;   this function takes arguments for each of the fields listed, so for example
;   (make-struct 'Sway' 'Tove Styrke' 'Pop') will create an album struct
;    with title 'Sway', artist 'Tove Styrke' & genre 'Pop
;
; `<struct-name>-<field-name>` (for each field)
;    functions for accessing values of each field in the struct
;    for album this would mean we'd have the following functions:
;    `album-title`, `album-artist`, `album-genre`
;    the following examples creates an album and then accesses its fields
;    ```
;    (define sway (make-album 'Sway' 'Tove Styrke' 'Pop')
;    (album-title sway) ; returns 'Sway'
;    (album-artist sway) ; returns 'Tove Styrke'
;    (album-genre sway) ; returns 'Pop'
;    ```
;
; `<struct-name>?` (in this case `album?`)
;   a predicate (function which returns a boolean) that checks a value and
;   returns true if it's an instance of the struct, false otherwise
;   using the `sway` album defined in the previous example
;   ```
;   (album? sway) ; returns true
;   (album? 1) ; returns false
;   (album? 'hi') ; returns false
;   ```

;;; Enter a list of albums below
;;; They need not be the actual albums you own.
;;; But you should include enough variety to adequately
;;; test your code.
;;;
;;; Here's what we mean. One of the questions involves
;;; writing a function that finds all the albums of a
;;; given genre.  If all the albums in the library are
;;; in the rock genre, then there's only one genre and
;;; when you ask for all the rock albums and it gives
;;; back all the albums, you don't know whether that's
;;; because the code really works, or because it's
;;; not even paying attention to the genre.  So you want
;;; to make sure there are multiple artists and genres,
;;; some artists with only one album or genre, others
;;; with multiple artists or genres, etc.

(define testing-library-1
  ;; Fill in the info below
  (list (make-album "title" "artist" "genre")
        ; Now delete the example above
        ; and include your own albums below here
        
        ))

;;; Add the functions you write (e.g. all-genres, versatile-artists)
;;; below.  Be sure to test your functions to make sure they work.
;;; We only provide very few test cases this time, so you need
;;; to write your own test cases to make sure the code works.
;;; We will use our own test cases when grading and assign you
;;; a grade based on the number of test cases that passed.


;; all-titles : (listof album) -> (listof string)



;; all-artists: (listof album) -> (listof string)



;; all-genres: (listof album) -> (listof string)



;; artist-albums : string, (listof album) -> (listof album)



;; artist-genres: string, (listof album) -> (listof string)



;; artist-is-versatile?: string, (listof album) -> boolean



;; versatile-artists: (listof album) -> (listof string)



;; artist-album-counts: (listof album) -> (listof (list string number))



;; genre-album-counts: (listof album) -> (listof (list string number))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Below are two example album libraries and a few example tests
;;; for SOME functions.
;;;
;;; ALL functions in this assignment should work for ANY given
;;; library, and the test cases below illustrate how libraries
;;; are passed to album functions.
;;;
;;; These test cases are far from complete. Remember to write new
;;; album library and tests to see if your solution works as expected.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define example-testing-library-1
  (list (make-album "You're Stronger Than You Know" "James Morrison" "Pop")
        (make-album "Bootleg"                       "Kenshi Yonezu"  "J-Pop")))

(define example-testing-library-2
  (list (make-album "Arthur Rubinstein Collection" "Arthur Rubinstein" "Classic")
        (make-album "Scott Joplin Piano Rags"      "Joshua Rifkin"     "Rags")
        (make-album "The Violin Sonatas (4 CDs)"   "Lev Oborin"        "Classic")))

(define prof-bains-music-library
  (list (make-album "Midnights"   "Taylor Swift" "Synth-pop")
        (make-album "1989"        "Taylor Swift" "Synth-pop")
        (make-album "Red"         "Taylor Swift" "Country")
        (make-album "Speak Now"   "Taylor Swift" "Country pop")))

(check-expect (all-titles example-testing-library-1)
              (list "You're Stronger Than You Know"
                    "Bootleg"))

(check-expect (all-titles example-testing-library-2)
              (list "Arthur Rubinstein Collection"
                    "Scott Joplin Piano Rags"
                    "The Violin Sonatas (4 CDs)"))

(check-expect (all-genres example-testing-library-2)
              (list "Classic" "Rags"))

(check-expect (artist-albums "Scott Joplin" example-testing-library-2)
              (list))

(check-expect (artist-albums "Joshua Rifkin" example-testing-library-2)
              (list (make-album "Scott Joplin Piano Rags" "Joshua Rifkin" "Rags")))

(check-expect (artist-albums "Joshua Rifkin" example-testing-library-1)
              (list))

(check-expect (artist-albums "Kenshi Yonezu" example-testing-library-1)
              (list (make-album "Bootleg" "Kenshi Yonezu" "J-Pop")))

(check-expect (artist-is-versatile? "James Morrison" example-testing-library-1)
              #false)

(check-expect (artist-album-counts example-testing-library-1)
              (list (list "James Morrison" 1)
                    (list "Kenshi Yonezu" 1)))


;; NOTE: Remember that "function" and "procedure" are the same thing! Racket's predicate
;; for seeing if something is a function is called procedure?
(check-expect (procedure? all-titles) #true)
(check-expect (procedure? all-artists) #true)
(check-expect (procedure? all-genres) #true)
(check-expect (procedure? artist-albums) #true)
(check-expect (procedure? artist-genres) #true)
(check-expect (procedure? artist-is-versatile?) #true)
(check-expect (procedure? versatile-artists) #true)
(check-expect (procedure? artist-album-counts) #true)
(check-expect (procedure? genre-album-counts) #true)
