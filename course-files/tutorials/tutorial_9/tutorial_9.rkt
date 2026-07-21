#lang htdp/asl
(require "./file_operations.rkt")

; sum : (listof number) -> number
; Returns the sum of a list of numbers (will be helpful for Activity 1)
(define (sum lst)
  (foldl + 0 lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Files and Folders Activities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Activity 1.1 / 1.2
; backup! : Path Path -> Void
; Recursively copies all the files and subdirectories in the from-directory (1st arg)
; directory to the to-directory (2nd arg). 
; EFFECT 1: to-directory and all its contents now exist
; EFFECT 2: may overwrite existing files at the to-directory
(define (backup! from to) 
  (begin
    ; create the destination directory if it doesn't already exist
    (unless (directory-exists? to)
      (make-directory! to))

    ; for each file (leaf node) in the origin directory,
    ; copy it over to the destination directory
    (for-each (λ (file)
                (begin
                  (printf "Copying file ~a to ~a~n" file to)
                  (copy-file! file
                              (build-path to (path-filename file))
                              #true)))
              (directory-files from))

    ; for each folder (recursive child node) in the origin directory,
    ; recursively backup! its contents
    (for-each (λ (subdir)
                (backup! subdir
                         ; add the subdirectory's name to the
                         ; end of the original destination path
                         (build-path to (path-filename subdir))))
              (directory-subdirectories from))))

; Activity 1.3 / 1.4
(define (backup-new! from to) 
  ...)


;;;;;; Part 2 ;;;;;

;; Activity 2.1
; count-files : path -> number
(define (count-files dir-path)
  ...)

(check-satisfied count-files procedure?)


;; Activity 2.2
; concat : (listof (listof path)) -> (listof path)
(define (concat lst-of-lists)
  ...)

(check-satisfied concat procedure?)
(check-expect
 (concat
  (list (list (build-path "test"))
        (list (build-path "test" "test_2"))))
 (list (build-path "test")
       (build-path "test" "test_2")))


;; Activity 2.3
; all-directories : path -> (listof path)
(define (all-directories dir-path)
  ...)

(check-satisfied all-directories procedure?)
(check-expect
 (all-directories (string->path "test"))
 ;; There are at least two directories in test, including test itself.
 ;;   Open the folder "test" in Finder (mac) or Explorer (Windows)
 ;;   to see which directories to include as tests!
 (list (build-path "test")
       (build-path "test" "test_2")))

;; Activity 2.4
;; search-file-name: string path -> (listof path)
(define (search-file-name name dir-path)
  ...)

(check-satisfied search-file-name procedure?)
(check-expect
 (search-file-name "test" (string->path "test"))
 (list (build-path "test" "test.txt")))
(check-expect
 (search-file-name "bar" (string->path "test"))
 (list (build-path "test" "test_2" "bar.txt")))