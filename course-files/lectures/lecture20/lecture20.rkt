;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 2_23) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;Finding/Correcting errors
;For #1-#3 Find and correct the errors in the functions to pass
; the provided tests. If you want to simulate an exam, don't rely
; on rerunning the code multiple times.

;#1
;sum-length: (listof String) -> Number
;Takes a list of strings and returns the sum of their lengths.
(define (sum-length lst)
  (foldl + 0 (for-each string-length lst)))

(check-expect (sum-length (list "abc" "def" "ghi")) 9)

;#2
;rev: (listof Any) -> (listof Any)
;Reverses a list. 
(define (rev lst)
  (local [(define result lst)]
    (begin (for-each (lambda (e)
                       (set! result (cons e result)))
                     lst)
           result)))

(check-expect (rev (list 10 20 30)) (list 30 20 10))

;#3
;sumlist: (listof Number) -> Number
;sums a list of numbers
(define (sumlist lst)
  (local [(define sum 0)
          (define remaining lst)
          (define (sum-help)
            (cond [(empty? remaining) sum]
                  [else (begin (set! remaining (rest remaining))
                               (set! sum (+ sum (first remaining)))
                               (sum-help))]))]
    (sum-help)))

(check-expect (sumlist (list 1 2 3)) 6)

;Writing code
;Fill in the function definitions below to pass the provided tests. 

;#4
;count-occurences-imperative: String (listof String) -> Number
;Counts and returns the number of times that st occurs in lst.
;Implement this using imperatives (e.g. set!), but WITHOUT using for-each.
(define (count-occurences-imperative st lst)
  "Write code here")
    
(check-expect (count-occurences-imperative "abc" (list "abc" "def" "ghi" "jkl" "abc" "def")) 2)
(check-expect (count-occurences-imperative "xyz" (list "abc" "def" "ghi" "jkl" "abc" "def")) 0)
(check-expect (count-occurences-imperative "xyz" (list)) 0)

;#5
;count-occurences-for-each: String (listof String) -> Number
;Counts and returns the number of times that st occurs in lst.
;Implement this using imperatives (e.g. set!), and using for-each.
(define (count-occurences-for-each st lst)
  "Write code here")
           
(check-expect (count-occurences-for-each "abc" (list "abc" "def" "ghi" "jkl" "abc" "def")) 2)
(check-expect (count-occurences-for-each "xyz" (list "abc" "def" "ghi" "jkl" "abc" "def")) 0)
(check-expect (count-occurences-for-each "xyz" (list)) 0)
 
;#6
;count-occurrences-predicate: (Number -> Boolean) (listof Number) -> Number
;Counts and returns the number of numbers and in the lst that the
; provided predicate returns true for.
;You must implement this *without* using filter or map.
(define (count-occurrences-predicate p lst)
  "Write code here")
           
(check-expect (count-occurrences-predicate odd? (list 1 2 3 4 5)) 3)
(check-expect (count-occurrences-predicate even? (list 1 3 5)) 0)
(check-expect (count-occurrences-predicate odd? (list)) 0)
(check-expect (count-occurrences-predicate (lambda (n) (= n 8)) (list 8 2 3 8 4)) 2)


