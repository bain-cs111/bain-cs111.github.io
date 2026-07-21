#lang htdp/asl

(require "./file_operations.rkt")

(define (all-files directory)
  (append (directory-files directory)
          (apply append
                 (map all-files 
                      (directory-subdirectories                
                       directory)))))

; "." is a short hand for "the current folder's path"
(define path-to-current-folder
  (build-path "."))

; list all files in the current folder
(directory-files path-to-current-folder)
; note: you might see some weird results here...because there are hidden files!

; list all subdirectories in the current folder
(directory-subdirectories path-to-current-folder)

; list all files and subdirectories in this folder AND its subfolders
(all-files path-to-current-folder)

(all-files (build-path "." "test"))