#lang htdp/isl+

(define-struct album (title artist genre))

(define database
  (list (make-album "The white album"
                    "The Beatles"
                    "Rock")
        (make-album "Sgt. Pepper’s Lonely Hearts Club Band"
                    "The Beatles"
                    "Rock")
        (make-album "Pod"
                    "The Breeders"
                    "Rock")
        (make-album "Dummy"
                    "Portishead"
                    "Triphop")))

(define Beatles? (lambda (album)
                   (string=? (album-artist album)
                             "The Beatles")))

(check-expect (filter Beatles? database) 
              (list (make-album "The white album"
                                "The Beatles"
                                "Rock")
                    (make-album "Sgt. Pepper’s Lonely Hearts Club Band"
                                "The Beatles"
                                "Rock")))

(check-expect (map album-genre database)
              (list "Rock" "Rock" "Rock"
                    "Triphop"))




