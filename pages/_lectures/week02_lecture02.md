---
layout: module
title: Compound Procedures
type: lecture
num: 3
draft: 0
description:
canvas_id: wednesday-lecture-3-pre-recorded-compound-procedures-sep-28
due_date: 2022-09-28
slides:
  - url: https://docs.google.com/presentation/d/1TD4F4amyU2_9WRU1r9cT74_glm79gSMNgd1qmVmV518/edit?usp=sharing
    title: Compound Functions (Pre-Recorded)
videos:
  - url: https://northwestern.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=5fbaa84a-8909-4135-9246-ae670037b16d
    title: "Compound Functions (Pre-Recorded)"
    duration: "25:51"
    live: 1

exercise_url: "lecture03.zip"
---

Today is our first Tutorial day! In the pre-recorded lecture, we explore the idea of _abstraction_: of making our code flexible rather than fixed.

The idea of **compound functions** is essentially a function within another function. Put more simply, it's the idea that we can `define` our OWN functions. We do this through something called a `lambda` expression. Just as we can use `(define netid "abc1234")` to store the value of `"abc1234"` inside the name `netid`, we can store the value of a function inside a name like so:

```racket
(define add-1
  (lambda (x) (+ x 1)))
```

This means we can then call this function with inputs later on:

```
(add-1 1)
```

and Racket will look up the value of `add-1` in its dictionary and then execute it with the input `1`...eventually giving us `2`!

Pretty cool.

**In-class** we'll complete Tutorial 0 which is mostly focused on the content from Monday's lecture on 2D Graphics, but the last activity of which requires you to define your own `glasses` function.

[Link to Tutorial 0](https://canvas.northwestern.edu/courses/178849/assignments/1124990)
