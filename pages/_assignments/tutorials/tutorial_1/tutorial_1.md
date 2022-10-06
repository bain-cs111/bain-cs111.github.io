---
layout: assignment-two-column
title: Iterating on your Expectations
type: tutorial
abbreviation: Tutorial 1
draft: 0
points: 100
num: 2
description:
canvas_id: 1136335
due_date: 2022-10-05
---

In this tutorial, you will be experimenting with creating images using the `iterated-images` library. To set up this tutorial, download the `tutorial_1.zip` and make sure to extract everything (if you're on a Windows computer, make sure to follow the same instructions as you found on Exercise 1).


<a class="nu-button" href="/course-files/tutorials/tutorial_1_template.zip" target="_blank"> Tutorial 1 Starter Files <i class="fas fa-download"></i></a>


Ensure that `iterated-images.rkt` is in the same `tutorial_1` folder as your `tutorial_1.rkt` file. Throughout, you will practice defining functions that depend on the functions from this library in order to create more complicated images than in Tutorial 0.

Remember a great resource for general Racket questions is the [official Racket documentation](https://docs.racket-lang.org/). This documentation includes definitions and explanations for features in Racket and how to use them. The `iterated-images.rkt` library also has its own documentation we'll take a look at later.

* * *

## Systematically Testing Your Programs

Over the past couple of weeks, odds are you've been manually checking your programs using the REPL (i.e. run your function and manually compare the output to what you expected it to be by eye). However, this very quickly becomes a) annoying and b) quite difficult to do with images (can you tell the difference between a circle with radius 50 and a circle with radius 51?). So instead of doing things manually, we can augment our program with `check-expect`s to test our program..well...programmatically!

`check-expect` is a function that tests to see if the output of its first argument equals the output of the second argument. These are usually typed in the Definitions Window so that whenever you run your program, it also runs the tests you've written.

> Note: in this class, we'll generally refer to these as `check-expect`s, but elsewhere in the wide world of computer science these might be referred to as "tests" or "unit tests."

Since `check-expect` is a just a function with two inputs, we use the usual notation to call the function:

```racket
(check-expect expression
              expected-expression)
```

For example, this check-expect would pass:

```racket
(check-expect (+ 1 1)
                   2)
```

Once the `check-expect` runs, the REPL pane would return "The test passed!" Otherwise, the `check-expect` will return a report of which tests failed, displaying both the actual and expected outputs. For instance, try the following and see what you get:

```racket
(check-expect (+1 1)
                  11)
```

> **Note**: `check-expect` is a function that returns a _boolean_ object. Booleans can have one of two values: `#true` or `#false`.

Throughout this assignment, we'll ask you to both run some premade `check-expect`s as well as design your own to check your functions as you write them.

* * *

## Basic Iteration

Having already written some functions to produce images in the last tutorial, we are now going to write some **iterations** that will make our code more efficient at making complicated images. Let's start with a simple function that will display multiple copies of an image next to each other, called `my-row`.

You are going to try and create a row of circles of a fixed size using `iterated-beside`, which is a function that takes as input a function and a count, and then displays an image defined by the inputted function `count` times (e.g. if you passed in a function call to `circle` and a `5` it would place 5 identical circles next to each other).

```racket
(define my-row
  (λ (image count)
    (iterated-beside (λ (image-number)
                        ...)
                     count)))
```

When you think you've got it, make sure to uncomment the `check-expect` for the `my-row` function (i.e. delete the `#|` and `|#` that currently surround the `check-expect` function call) and run your program to see if it matches the expected output.

Note that just because it works for those blue circles doesn't mean it works for all other cases. Make sure to try your `my-row` function with some different inputs that aren't blue circles to make sure it work the way you expect it to.

* * *

## Abstraction and Iterating on Iterators

Now that you've got a working `my-row` function, let's use it to make an even more complicated image: a complete grid of whatever image someone inputs to our function. For example, say I wanted to make a grid (5 wide and 5 high) of outlined red circles:

<img src="/assets/tutorial_1/my-grid.svg" alt="My Grid Example" width="33%"/>

Use your `my-row` function as well as the `iterated-above` function to generate a square-grid of any inputted `image`. Call the function `my-grid`.

> Note: `iterated-above` is literally a version of the [built-in `above` function](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._above%29%29) that can be iterated. If you think of the `overlay` function, it does the following in English: "overlay shape 1 on top of shape 2 on on top of shape 3 on top of shape 4, etc..." The same is true of `above`: "place shape 1 above shape 2, which is above shape 3, which is above shape 4, etc..." Since you're creating identical rows this isn't so important at the moment, but it will be important later.

Similar to your `my-row` function since the size of the shapes and rows aren’t changing, the `my-grid` function shouldn’t use the `count` variable to change anything about the shape or the row length. This function will take in an image and a number (`n`) and output a grid of images of size `n * n`.

> Note: this might seem easy on its face...but it's very easy to get bogged down in parentheses. If you get stuck, try using your code from `my-row` as a sort of "iterator template." In `my-row` you were just drawing a simple shape using whatever `image` was inputted. You're doing the same thing here, except instead of just drawing the inputted `image`, you're drawing a `my-row` of `image`s!

By using the `my-row` function inside `my-grid`, you're practicing the key concept of **abstraction**. Once you've made a function to create a row of images, you no longer need to remember all those details–you just call the function! Once you've created `my-grid`, you don't need to worry about ever writing it again–you just call the function! In a way, you're creating larger and larger Lego bricks you can later combine into even more complex programs.

* * *

## Using the Iterator's Built-in Counter

Remember that all iterators’ have a built-in counter variable that start at 0 and end at n-1. That is, they will run `count` times, but at their first iteration, their counter will be 0, then 1, then 2, and so on. They will then stop once the `n - 1` iteration has finished its business.

For your next image, you are going to try and recreate a bullseye made out of red-outlined circles with radii increasing by 25 each time. This function, called `my-bullseye`, will take one input, the number of circles to draw, and will involve `iterated-overlay`. The output should look something like this.

<img src="/assets/tutorial_1/my-bullseye.svg" alt="Bullseye Example" width="45%"/>

Notice that there is a tiny little speck in the middle of the bullseye example. What could _that_ be?
 
> **Answer**: Remember the iteration counter starts at 0 (just like our exercises and tutorials started out at 0). In this bullseye and all the circles in the bullseye have a radius of their circle-number multiplied by a constant - in this case, 25 - then this must mean that the little speck in the middle of the circle is another circle, but with a radius of 0.

* * *

## Combining Both Ideas

For the final image, you are going to try to create a pyramid with a function called `my-pyramid` to create some awesome pyramids that would put Giza to shame.

<img src="/assets/tutorial_1/my-pyramid.svg" alt="My Grid Example" width="33%"/>

You are going to want to use a structure something similar to your `my-grid` function for this, but this time, change the length of the rows on each iteration (just like you changed the size of each circle in `my-bullseye`).

> **Note**: Remember that `iterated-above` places "shape 1 on above shape 2, which is above shape 3, which is above shape 4, etc..." What this means is that if you think about building your pyramid from bottom-to-top...you might actually get a flipped pyramid. If this happens to you, try to think about building it from top-to-bottom.

This function should take as inputs an image and a number (`n`) and then output an image of a pyramid with height `n` and base `n` made out of the input image. This will allow you to build a pyramid with any image you want and any size with literally just a few lines of a program. _Abstraction is SO cool._

* * *

## Getting Credit for Your Work
If you're in class make sure to check-in with your PM. You don't need to submit a `rkt` file. Your attendance will be posted on Canvas by around 5pm today.

If you're submitting remotely, you MUST submit your completed tutorial to Canvas and it will be graded for completion. Make sure your functions are named `my-row`, `my-grid`, `my-bullseye`, and `my-pyramid` and that all of the built-in `check-expect`s pass.

Before turning your assignment in, **run the file one last time** to make sure that it runs properly and doesn’t generate any exceptions, and all the tests pass.

Then, make sure to read the [Autograder Guide](https://canvas.northwestern.edu/courses/178849/pages/whats-an-autograder) one last time. Not only is it a useful check of your work, but it will also tell you which file you should submit.

> Note: For groups that have a significant amount of programming experience, your PM may suggest trying out the [Advanced Tutorial 1](https://bain-cs111.github.io/assignments/adv-tutorial-1)
