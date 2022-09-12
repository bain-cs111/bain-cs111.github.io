---
layout: assignment-two-column
title: Getting Acquainted with Racket
abbreviation: Exercise 1
type: homework
due_date: 2022-10-03
ordering: 2
draft: 0
points: 100
---

In this assignment you’ll play with simple functions for making images that we’ll use later on in the course. If you have not already completed Tutorial 0, do so before starting this assignment as it will assume that you have correctly configured Dr Racket and that you have a solid understanding of how to create both simple and compound images.

> **Note**: We've tried to make this assignment accessible to those who have trouble seeing colors, but please let us know if we can provide additional help. On this assignment, it is not necessary to exactly reproduce sizes and colors unless they are specifically stated in the problem.

In order to start this assignment, please download the starter files:

<a class="nu-button" href="/course-files/homework/ex01_template.zip" target="_blank">
    Exercise 1 Starter Files <i class="fas fa-download"></i>
</a>

This download is a ZIP file which _must be extracted_ before the files can be edited. On a Mac, you can simply double click on the ZIP folder and it will extract the files for you. On a Windows computer, you need to can use [these instructions](https://support.microsoft.com/en-us/windows/zip-and-unzip-files-8d28fa72-f2f9-712f-67df-f80cf89fd4e5).

> **Note**: We highly recommend moving the extracted folder to your CS 111 work folder and renaming it to make sure you submit the file you mean to later when you go to submit.

Now, open up the extracted file. For this exercise, it's called `exercise_1.rkt.`

* * *

## Activity 1 - Red Square
First, let's make a 100x100 red square like the below:

![red-square](/assets/exercise_1/a-red-square.svg)

Now you should give this square a name. In the Definitions Window type:

<pre>
(define <b>a-red-square</b> <i>your-code-for-the-square</i>)
</pre>

and hit run.

You can now refer to the square by typing `a-red-square` into the interaction window or any code you write.

> **VERY Important Note**: The names you give your images (i.e. the names after the word “define”) **must be exactly** as described otherwise the autograder will reject your submission!

* * *

## Activity 2 - Blue Circle

Now let's make a blue circle of radius 50.

![Blue Circle](/assets/exercise_1/a-blue-circle.svg)

Define it as the name: `a-blue-circle`.
* * *

## Activity 3 - Outline Mode
Repeat Activities 1 and 2, but use outline mode:

<!-- Note I had to manually edit the viewbox of both of these to 0 0 101 101 to make sure they didn't clip - CB 08-2022 -->

`outlined-square` | `outlined-circle`
- | -
![Outlined Square](/assets/exercise_1/outlined-square.svg) | ![Outlined Circle](/assets/exercise_1/outlined-circle.svg)

Define them as the names `outlined-square` and `outlined-circle` respectively.
* * *

## Activity 4 - Compound Images
Now let’s make compound images from simpler images. Use `overlay`, `above`, and `beside` to make the following compound images, defined as follows:

<span style="font-weight:normal">`row-of-squares`</span> | <img alt="row-of-squares" src="/assets/exercise_1/row-of-squares.svg" width="15%"/> |
--------- | :-:
`column-of-squares` | <img alt="column-of-squares" src="/assets/exercise_1/column-of-squares.svg" width="5%"/>
`nested-squares` | ![nested-squares](/assets/exercise_1/nested-squares.svg)

Make sure to draw out the dataflow diagrams for each of these. You don’t need to turn them in–just draw them on some scratch paper or a whiteboard to make sure you understand how the data moves through the chain of calls.

* * *
## Activity 5 - Rotate

Read the documentation for the `rotate` function and try making an image that looks like this:

<img alt="rotated-squares" src="/assets/exercise_1/rotated-squares.svg" width="20%"/>

Define it as `rotated-squares`. And draw out the dataflow diagram for this one. Once again, you don’t need to turn the diagram in.

* * *
## Activity 6 - Flag of Chicago

Now, make the flag of Chicago:

<!-- viewBox="-1 -1 201 122" stroke-width:2px -->
![flag-of-chicago](/assets/exercise_1/flag-of-chicago.svg)

You might need some more functions than the ones we have discussed so far. You may find `radial-star` and `overlay/xy` to be helpful. Remember all the functions in Racket are explained in the Racket documentation and your image doesn’t need to be exact! Define it as `flag-of-chicago`. And again, make sure that you can sketch out the dataflow diagram for it.

* * *

## Turning it in
Once you’ve finished your assignment, **PLEASE READ THE AUTOGRADER GUIDE ONE MORE TIME** before submitting it to Canvas. Congratulations! You’re done with the assignment for this week!
