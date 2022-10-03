---
layout: module
title: Lambda Abstraction
type: lecture
draft: 1
num: 6
due_date: 2022-10-05
slides:
  - url: https://docs.google.com/presentation/d/1pA84JhQwF_-BGFODYLqXtHGrnrksK7BoCe3R-77tPwk/edit?usp=sharing
    title: "Lambda Abstraction"

videos:
  - url: https://northwestern.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=398c2221-4c26-4f55-a4c0-ae670037b1ba
    title: "Live Lecture (Not Yet Posted)"
    duration: "50:00"
    live: 1
---

Today, in the pre-recorded lecture, we'll talk about the idea of _lambda abstraction_. How do we break a problem down into small parts...gradually building a program that is both powerful and flexible to solve problems.

> **Reminder**: Watch the pre-recorded lecture and come to class ready to complete Tutorial 1!

## Local defines

Nearly every language has a way of defining local variables to hold intermediate values.  It’s another bread-and-butter programming thing.  Unfortunately, the `local` expression is one of the uglier features of the Racket student languages. Most languages, including real Racket, have much less verbose ways of defining local variables.

The designers of the student language designed the `local` special form the way they did because they found it caused less confusion for new programmers than the alternatives. But in subsequent courses, you’ll find that you can add a new variable with a lot less typing, including in Racket.
