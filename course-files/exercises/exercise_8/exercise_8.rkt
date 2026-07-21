#lang htdp/asl
(require "quiz_lib.rkt")
; Note the missing methods are marked *** TODO ***

; If a user answers with a particular choice, you can think of that as a vote
;   for a particular outcome (the result of the quiz as a whole).

; a fun-question is a ...
;   - (make-fun-question string (listOf string) (listOf string))
(define-struct fun-question (text choices outcomes)
  #:methods
  (define (show-text q)
    (local [(define (print-choices label choices)
              (unless (empty? choices)
                (begin
                  (printf "  ~a. ~a~n" label (first choices))
                  (print-choices (+ label 1) (rest choices)))))]
      (begin
        (printf "Q: ~a~n" (fun-question-text q))
        (print-choices 1 (fun-question-choices q)))))
  
  ; choice-ref : fun-question number -> String
  ; Returns the choice associated with a particular number.
  (define (choice-ref q choice)
    (list-ref (fun-question-choices q)
              (- choice 1))) ; lists start at 0 so we correct for that here

  ; outcome-ref : fun-question Number -> String
  ; Returns the outcome associated with a particular number.
  (define (outcome-ref q choice)
    (list-ref (fun-question-outcomes q)
              (- choice 1)))) ; lists start at 0 so we correct for that here


; Stranger Things Kids Personality Quiz
; Credit (https://www.buzzfeed.com/marycolussi/which-stranger-things-character-are-you)
(check-expect (choice-ref q1 5) "Snickers")
(check-expect (outcome-ref q1 5) "Dustin")
(define q1
  (make-fun-question
   "Choose a snack:"
   (list   "Ice Cream"   "Reese's Pieces"  "Red Vines"     "Eggo Waffles"  "Snickers"  "Mac & Cheese")
   (list   "Max"            "Will"         "Lucas"          "Eleven"        "Dustin"     "Mike")))

(check-expect (choice-ref q2 5) "Fight back, lack of superpowers be damned.")
(check-expect (outcome-ref q2 5) "Mike")
(define q2
  (make-fun-question
   "AH!!! It's a demogorgon. How do you react?"
   (list "Honestly, getting dragged to the Upside Down if this happens."
         "Be sarcastic until it slinks away in shame."
         "Grab a bat and protect your friends."
         "Wait, demogorgon's exist?"
         "Fight back, lack of superpowers be damned."
         "Single-handedly destroy it.")
   (list "Will" "Max" "Dustin" "Lucas" "Mike" "Eleven")))

(check-expect (choice-ref q3 5) "Demodogs")
(check-expect (outcome-ref q3 5) "Dustin")
(define q3
  (make-fun-question
   "Who's the scariest villain?"
   (list "Papa"
         "Vecna"
         "Demogorgons"
         "Billy"
         "Demodogs"
         "The Mind Flayer")
   (list "Eleven" "Max" "Will" "Lucas" "Dustin" "Mike")))


; a quiz is a ...
; (make-quiz string (listOf fun-question) (listOf string) (hashOf string number))
(define-struct quiz (title questions possible-outcomes scoring-of-outcomes)
  #:methods

  ; *** TODO ***
  ; reset-scoring-hash : quiz -> void
  ; For each outcome in possible-outcomes, sets the value associated it
  ;   in the hash table in scoring-of-outcomes to 0.
  ; Effect: scoring-of-outcomes property has mutated, all values set to 0.


  ; *** TODO ***
  ; update-scoring-hash : quiz string -> void
  ; Increments the scoring-of-outcomes hash, adding one to the value
  ;   associated with the given outcome.
  ; Effect: scoring-of-outcomes property has mutated, incrementing value
  ;  associated with the given outcome by 1.

  
  ; *** TODO ***
  ; get-scoring-outcome: quiz -> String
  ; Iterates over the scoring-of-outcomes hash to find and return the
  ;   outcome with the highest associated value.

  
  )

; An example "quiz" (a list of questions)
(define my-quiz
  (make-quiz "Which Stranger Things kid are you?"
             (list q1 q2 q3)
             (list "Eleven" "Lucas" "Max" "Will" "Mike" "Dustin")
             (make-hash)))


; A sample check-expect for reset-scoring-hash
(check-expect
 (local [(define testquiz
           (make-quiz "Title"
                      (list q1)
                      (list "Eleven" "Lucas" "Max" "Will")
                      (make-hash)))]
   (begin
     (reset-scoring-hash testquiz)
     testquiz))
 (make-quiz "Title"
            (list q1)
            (list "Eleven" "Lucas" "Max" "Will")
            (make-hash
             (list (list "Eleven" 0) (list "Lucas" 0)
                   (list "Max" 0) (list "Will" 0)))))


; A sample check-expect for update-scoring-hash
(check-expect
 (local [(define testquiz
           (make-quiz "Title"
                      (list q1)
                      (list "Eleven" "Lucas" "Max" "Will")
                      (make-hash)))]
   (begin
     (reset-scoring-hash testquiz)
     (update-scoring-hash testquiz "Lucas")
     (update-scoring-hash testquiz "Eleven")
     (update-scoring-hash testquiz "Eleven")
     (update-scoring-hash testquiz "Max")
     (quiz-scoring-of-outcomes testquiz)))
 (make-hash
  (list (list "Will" 0) (list "Lucas" 1)
        (list "Eleven" 2) (list "Max" 1))))

; A check-expect for get-scoring-outcome
(check-expect
 (local [(define testquiz
           (make-quiz "Title"
                      (list q1)
                      (list "Lucas" "Max" "Dustin" "Eleven")
                      (make-hash)))]
   (begin
     (reset-scoring-hash testquiz)
     (update-scoring-hash testquiz "Lucas")
     (update-scoring-hash testquiz "Eleven")
     (update-scoring-hash testquiz "Eleven")
     (update-scoring-hash testquiz "Max")
     (update-scoring-hash testquiz "Dustin")
     (update-scoring-hash testquiz "Eleven")
     ; **HINT**: insert (print testquiz) here to see the votes
     (get-scoring-outcome testquiz)))
 "Eleven")


; runquiz : (listof question) -> void
; Takes a list of questions. In order, displays the question,
;   gets a response from the user and checks the answer.
; Effect: A quiz has been displayed and run.
(define (runquiz somequiz)
  (local [(define user-response "")
          (define user-responses (list))]
    (begin
      (printf "Welcome to my quiz!~n>>> ~a <<<~n" (quiz-title somequiz))
      (reset-scoring-hash somequiz)
      (for-each (lambda (q)
                  (begin (newline)
                         (show-text q)
                         (printf "> ")
                         (set! user-response (read))
                         (set! user-responses (append user-responses
                                                      (list (choice-ref q user-response))))
                         (update-scoring-hash somequiz
                                              (outcome-ref q
                                                           user-response))))
                (quiz-questions somequiz))
      (printf "~nYou answered...~n")
      (for-each (lambda (s) (printf "    - ~a~n" s)) user-responses)
      (printf "Your result is...")
      (get-scoring-outcome somequiz))))


;; run the quiz on our list of questions
;(runquiz my-quiz)