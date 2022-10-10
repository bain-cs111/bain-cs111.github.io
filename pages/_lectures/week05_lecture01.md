---
layout: module
title: Quiz 1
type: quiz
draft: 0
canvas_id: monday-quiz-1-oct-17
num: 1
due_date: 2022-10-17
---

Quiz 1 will cover all the content from the beginning of the class up to and including Lecture 9 on Manipulating Composite Data.

* * *

## Logistics
* In-person here in the Auditorium (ANU Students please keep an eye out for an email from me this evening with accommodations details)
* Taken on the Lockdown Browser on your personal computer; details on [how to set it up are on our Canvas page]().
* Practice Exam available on Canvas at 6pm this evening
  * there's also tips on how to use a Practice Exam effectively
  * solutions will be available for the practice on Wednesday evening
  * there will be two versions: one that requires the Lockdown Browser (so you can test that you've got it setup correctly) and one that does not so you can take it and refer to other resources
* We'll do additional review on Friday in-class (it will serve as Mini-Quiz 3)
* During your assigned class time; you MUST attend the one you're registered for on CAESAR.
* Unrelated, but reminder that you can find your section # by looking at CAESAR.
  * COMP_SCI 111-0-1 vs COMP_SCI 111-0-2

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

Examples on the Practice Quiz (which will be released Monday evening).

3. Write a valid test for the given function definition
Covers everything up to and including Lecture 9 (Manipulating Composite Data)

**Example**:
```racket
; a snake is
; (make-snake Number String)
(define-struct snake [weight food])

; skinny-snake?: Snake -> Boolean
; returns true if snake is less than 10 lbs, false otherwise

(define  (skinny-snake?  s)
     (<  (snake-weight  s)  10))
```


In the blank below, please provide one valid test (`check-expect`) of the procedure.

A valid answer would be:
```racket
(check-expect (skinny-snake? (make-snake 11 "mice")) false)
```

* * *

## Reminders

* You do NOT need to memorize all of the functions [(you get a glossary)](course-files/quizzes/q1_glossary.pdf)
* You DO need to know the [Rules of Execution and Special Forms](https://docs.google.com/presentation/d/1i5YPqSRd2-E7Pm-c8A1322LBsmeyR6qyfjv0UcvhvPM/edit#slide=id.g1632f97878b_0_0).
