---
layout: module
title: Lambda Abstraction
type: lecture
draft: 0
num: 6
due_date: 2022-10-05
exercise_url: "lecture06.zip"
slides:
  - url: https://docs.google.com/presentation/d/1pA84JhQwF_-BGFODYLqXtHGrnrksK7BoCe3R-77tPwk/edit?usp=sharing
    title: "Lambda Abstraction"

videos:
  - url: https://northwestern.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=ed97942a-a93d-421b-a606-af23011e3347
    title: "Lecture 6.0: Intro + Substitution Model"
    duration: "07:24"
    live: 1
  - url: https://northwestern.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=00d49850-8c03-4350-959e-af23011e3378
    title: "Lecture 6.1: Making a Color Gradient"
    duration: "10:56"
    live: 1
  - url: https://northwestern.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=85211baf-868d-4e4a-9676-af23011e9568
    title: "Lecture 6.2: Lambda Abstraction"
    duration: "04:23"
    live: 1
  - url: https://northwestern.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=0ec94d5a-95db-45d0-b737-af23011e33df
    title: "Lecture 6.3: Making a Rotary Pattern"
    duration: "11:32"
    live: 1
  - url: https://northwestern.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=c98ff6af-dfd1-4fe0-b68b-af23011e5839
    title: "Lecture 6.4: Local Variables"
    duration: "05:43"
    live: 1
  - url: https://northwestern.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=54b1e131-505a-4756-a3c3-af23011e3303
    title: "Lecture 6.5: Grids Revisited"
    duration: "04:04"
    live: 1
---

Today, in the pre-recorded lecture, we'll talk about the idea of _lambda abstraction_. How do we break a problem down into small parts...gradually building a program that is both powerful and flexible to solve problems.

> **Reminder**: Watch the pre-recorded lecture and come to class ready to complete Tutorial 1!

## Local defines

Nearly every language has a way of defining local variables to hold intermediate values.  It’s another bread-and-butter programming thing.  Unfortunately, the `local` expression is one of the uglier features of the Racket student languages. Most languages, including real Racket, have much less verbose ways of defining local variables.

The designers of the student language designed the `local` special form the way they did because they found it caused less confusion for new programmers than the alternatives. But in subsequent courses, you’ll find that you can add a new variable with a lot less typing, including in Racket.
