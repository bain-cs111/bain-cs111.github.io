---
layout: lecture-two-column
title: Quiz 2
type: quiz
draft: 0
canvas_id: wednesday-quiz-2-nov-2
num: 1
due_date: 2022-11-02
---

Quiz 2 will cover all the content from the beginning of the class up to and including Lecture 15 on Binary Search Trees. It will mostly be focused on recursion.

* * *

## Logistics
* In-person here in the Auditorium (ANU Students please keep an eye out for an email from me this evening with accommodations details)
* Taken on the Lockdown Browser on your personal computer; details on <a href="https://canvas.northwestern.edu/courses/178849/modules/items/2427335" target="_blank">how to set it up are on our Canvas page.</a>
* Practice Exam available on Canvas
  * there's also tips on how to use a Practice Exam effectively
* We'll do additional review on Monday in-class (it will serve as Mini-Quiz 5)
* During your assigned class time; you MUST attend the one you're registered for on CAESAR.

* * *

## Style and Content
Three Types of Problems:
1. What's the type of this expression?

**Examples**: What's the type of the following expression?
```racket
(lambda (n)
  * n 2)
```
The answer would be: `number -> number` because this is a **function** that takes as input numbers and produces as output numbers.

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
* You do NOT need to memorize all of the functions <a target="_blank" href="https://bain-cs111.github.io/course-files/quizzes/q2_glossary_compact.pdf">(you get a glossary)</a>
* You DO need to know the Rules of Execution and Special Forms.
