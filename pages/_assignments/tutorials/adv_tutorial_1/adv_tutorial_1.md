---
layout: assignment-two-column
title: Procedural shading
type: tutorial
abbreviation: Advanced Tutorial 1
draft: 1
points: 100
num: 1
canvas_id: 1136335
description:
due_date: 2022-10-05
---


> Note: If you're completing the advanced tutorial remotely and plan to submit your RKT file, **please make sure your file name has in it the string `adv`**, otherwise we will assume you are submitting the classic version of the tutorial.


<a class="nu-button" href="https://bain-cs111.github.io/course-files/tutorials/adv_tutorial_1_template.zip" target="_blank">
    Advance Tutorial 1 Starter Files <i class="fas fa-download"></i>
</a>


In this tutorial, we’ll experiment with using iterators (functions that take another function as an argument and run it repeatedly), and then write some of our own iterators. Start by opening `adv_tutorial_1.rkt`. It will load up the iterated-images library that you need to for the tutorial and the assignment.

## Getting started
Start by writing the following functions. See the Racket file for example inputs and outputs:

```racket
; green-box: number -> image
; Makes a green square of the specified size.
; It should be drawn as an outline and the outline should be green.
```

```racket
;	nicer-green-box: number -> image
; Same, but makes a filled green box with a black outline.
```

## Some simple iteration

Now write some functions that iterate. See the Racket file for example inputs and outputs:

```racket
; green-row: number number -> image
; Makes a row of nicer green boxes.
; The first argument is size and the second is count.
```


```racket
; row-of: image number -> image
; Now change green-row to take the image to duplicate in
; the row as its first argument.
```

```racket
; concentric: image number -> image
; Make set of concentric copies of image, scaled by
; increasing scale factors.  So the first copy is
; scaled by scale 1, the second by scale 2, etc.,
; up to scale number+1.
```

```racket
; rotating-concentric: image number number -> image
; Make set of concentric copies of image, scaled by
; increasing scale factors and each rotated by the
; specified number of degrees. Image is first argument,
; angle second, number of copies is the third.
```

## Making your own iterators

Remember that an iterator is just a function that take some code to run and runs it repeatedly. There's nothing special about it.  There's no reason you can't write your own. So let's do it.

One of the things that's annoying about the iterators we gave you is that they always count from zero. That is, if you say `(iterated-overlay p 3)`, it will always run `p` with arguments `0`, `1`, and `2`. If you want it to call `p` with argument `10`, `15`, and `20`, you have to write some math to compute those from `0`, `1`, and `2`, and you end up writing that kind of math repeatedly.

Let's write versions of `iterated-overlay` that do that for you.  We will make have two different versions: one of which stops just before getting to the end value and one of which goes all the way up to the `end-value`.

```Racket
; iterate-in-range-exclusive: (number -> image) number number number -> image
; Calls picture generating function (first argument) a specified number of times
; with numbers ranging between two values.
```

Arguments should be: `generator`, `start-value`, `end-value`, and `count`. It should call `generator` `count` times with values starting at `start-value` and stopping just short of `end-value`.

For example, `(iterate-in-range p 10 20 5)` should call `p` `5` times, with arguments `10`, `12`, `14`, `16`, and `18`, but not `20`.

```Racket
; iterate-in-range-inclusive: (number -> image) number number number -> image
; Same, but goes all the way up to the end value.
```
For example, `(iterate-in-range p 10 20 3)` should call `p` `3` times, with arguments `10`, `15`, and `20`.

## Getting Credit for Your Work

If you're in class make sure to check-in with your PM. You don't need to submit a `rkt` file. Your attendance will be posted on Canvas by around 5pm today.

> **Note**: If you're completing the advanced tutorial remotely and plan to submit your RKT file, **please make sure your file name has in it the string `adv`**, otherwise we will assume you are submitting the classic version of the tutorial.

If you're submitting remotely, you MUST submit your completed tutorial to Canvas and it will be graded for completion. Make sure your that all of the built-in `check-expect`s pass and your functions are named correctly.
