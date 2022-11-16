---
layout: assignment-two-column
title: Inheritance
type: tutorial
abbreviation: Tutorial 7
draft: 0
points: 100
num: 10
description:
due_date: 2022-11-16
canvas_id: 1147674
---

Our goals for today are pretty simple:
1. Understand why subtyping is useful
2. Practice implementing subtypes in Racket
3. Understand why methods are useful
4. Practice implementing methods in Racket

In this tutorial, you'll deal with these two concepts separately but in this week's exercise, you'll have to deal with them at the same time while designing interactive quizzes.


<br>

<a class="nu-button" href="/course-files/tutorials/tutorial_7_template.zip" target="_blank">
    Tutorial 7 Starter Files <i class="fas fa-download"></i>
</a>

<br>

* * *

## Part 1 - Fun at the zoo

Today we'll be defining a whole zoo's worth of animals. However, as we saw on Monday, we want tot use _subtyping_ to make our object design more efficient.

### Activity 1
First define a _abstract_ struct called `animal` that has the following fields:

* `name`
* `age`
* `weight`

> **Note**: if you're still struggling with structs, I HIGHLY recommend listing out all of the automagically defined functions you get as comments in your program like I did on Monday and in the demo video.

### Activity 2
Now define a subtype of `animal` called `cat` which has an additional field:

* `sleeping-spot`

### Activity 3
Now define a subtype of `animal` called `dog` that has an additional field:

* `best-friend`

### Activity 4
Now define a subtype of `animal` called `mouse` that has an additional field:

* `hiding-spot`

### Activity 5
Now write a function called `feed-animal!` that takes as input a single `animal` and **mutates** its weight by adding 2 to it.

Make sure to write at least a couple of tests to make sure its working the way you think!

> **Hint**: Remember, that now that we have imperatives...writing tests is a little confusing. You need to take into account what the weight WAS in order to find out what the weight will be AFTER calling the function.

### Activity 6 (Challenge)
If you're in attendance and it's already the 25 minute mark, move on to Part 2. Come back to this question if you have time.

Write a function called `feed-animals-with-favortism!` that takes in a list of animals (`loa` for short) and uses `feed-animal!` _for each_ animal in the list with one big caveat...

Bob is gizmo's best friend. He is also the zoo keeper! Bob wants to feed all of the animals in the zoo, but he will feed any animal twice who lists him as their `best-friend`.

We've included some tests for you on this one.


* * *

## Part 2 - A roster of students

In the Racket file you downloaded scroll down until you see "PART 2". Here you'll find two different struct definitions.

### `student`

A `student` has several fields:

| Name | Type | Description |
| ---  | --- | ---- |
| name | `string` | the student's name |
| grad-year | `number` | what year the student will graduate in |
| major     | `string` | the student's major |

This struct _does not_ have any methods.

### `roster`

This struct represents the roster of a course. Here are its fields:

| Name | Type | Description |
| ---  | --- | ---- |
| course-name | `string` | the course's name |
| instructor | `string` | the name of the instructor |
| students     | `(listof student)` | the list of `student`s in the class |

It also has a method:

| Name | Inputs | Description |
| ---- | ------ | ----------- |
| `display`      | takes as input ONLY a `roster` | Displays the roster in the interactions window, class name, instructor name, and then each of the students in the class.                                            |


### Activity 7

Your first job is to implement a new method for `roster` with the following specification:

| Name | Inputs | Description |
| ---- | ------ | ----------- |
| `search-by-major` | takes a `roster` and a `string` as input | searches through the `roster`, returning the list of students with the provided major |


### Activity 8

Next (and last) implement one final method:

| Name | Inputs | Description |
| ---- | ------ | ----------- |
| `names-by-grad-year` | takes a `roster` and a `number` as input | searches through the `roster`, returning the list of students' names with the provided grad-year |

* * *

## Getting Credit for Your Work
If you're in class make sure to check-in with your PM with your name and NetID. You don't need to submit a `rkt` file. Your attendance will be posted on Canvas by around 5pm today.

If you're submitting remotely, you MUST submit your completed tutorial to Canvas and it will be graded for completion.

Before turning your assignment in, **run the file one last time** to make sure that it runs properly and doesn’t generate any exceptions, and all the tests pass.

Then, make sure to read the [Autograder Guide](https://canvas.northwestern.edu/courses/178849/pages/whats-an-autograder) one last time. Not only is it a useful check of your work, but it will also tell you which file you should submit.

> Note: Unfortunately, there's no advanced tutorial this week. If you have an idea for one (related to subtyping or inheritance...let me know! I tried a couple of variations of adding this sort of thing to our interepreter...but they were way too hard to complete in one 50 minute period...)
>
> Speaking of which...
> If you were working on the adv tutorial last week and didn't finish...why not work on that now? <a target="blank" href="/assignments/adv-tutorial-6">Advanced Tutorial 6</a>
>
> It'd probably be more fun.
>
> But who am I to say what's fun.
>
> Especially since I'm having a conversation with myself. In a blockquote...that maybe no one will read.
>
> Oh well. Who knows. Come find me if you're bored, we can find something for you to do!
