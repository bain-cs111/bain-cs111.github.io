---
layout: two-column
title:  Quiz 4 (Section 1)
type: quiz
draft: 0
num: 4
canvas_id: friday-quiz-4-section-1-dec-9
due_date: 2022-12-09
---
Quiz 4 will cover all the content from the beginning of the class up to the end of the class. You can think of it as a mash-up of Quizzes 1, 2, and 3.

The best way to study for this quiz is to use the three practice exams for Quizzes 1, 2, and 3 as well as the three real exams for Q1, Q2, and Q3. The quiz will essentially draw 1/3 of its content from Q1, 1/3 from Q2, and 1/3 from Q3.

It will ALL be in the Advanced Student Language.

* * *

## Logistics
* In-person here in the Auditorium (ANU Students please keep an eye out for an email from me this evening with accommodations details)
* Taken on the Lockdown Browser on your personal computer; details on <a href="https://canvas.northwestern.edu/courses/178849/modules/items/2427335" target="_blank">how to set it up are on our Canvas page.</a>
* During your assigned class time; you MUST attend the one you're registered for on CAESAR.

* * *

## Style and Content
Three Types of Problems:
1. What's the type of this expression?

**Examples**: What's the type of the following expression?
```racket
(lambda (n)
  (* n 2))
```
The answer would be: `number -> number` because this is a **function** that takes as input number and produces as output number.

If we were instead to give you an expression:
```racket
(define pineapple 5)
(+ 5 pineapple)
```
The answer would be `number` as this would produce a number (note you aren't responsible for the value).

If we were to give you something that causes an exception, like:
```racket
(+ 1 "this is dumb")
```
Then the answer would be `exception`.

2. Here's a program; here's what we want to get out of it; here's what we get; fix it.

Examples on the Practice Quiz.

3. Write a valid test for the given function definition
Covers everything up to and including Lecture 9 (Manipulating Composite Data)

**Example**:
```racket
; a snake is
; (make-snake Number String)
(define-struct snake (weight food))

; skinny-snake?: Snake -> Boolean
; returns true if snake is less than 10 lbs, false otherwise

(define (skinny-snake? s)
     (< (snake-weight s) 10))
```

In the blank below, please provide one valid test (`check-expect`) of the procedure.

A valid answer would be:
```racket
(check-expect (skinny-snake? (make-snake 11 "mice")) false)
```
* * *

## Reminders
* You do NOT need to memorize all of the functions <a target="_blank" href="https://bain-cs111.github.io/course-files/quizzes/q3_glossary_compact.pdf">(you get a glossary)</a>
* You DO need to know the Rules of Execution and Special Forms.
