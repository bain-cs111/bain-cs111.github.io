---
layout: assignment-two-column
title: Composing and Manipulating Composite Data
type: tutorial
abbreviation: Tutorial 2
draft: 0
points: 100
num: 3
canvas_id: 1138589
description:
due_date: 2022-10-12
---

For this tutorial, you’ll experiment with `list`s and `structs`. **Make sure to write `check-expects` for each expression you write to test its correctness.**

There a number of goals here:
1. Get some practice using composite data (both lists and structs)
2. Get a lot of practice manipulating lists using those library of list functions we talked about in Lecture 10
3. **Practice thinking about and writing our OWN tests** for our programs.

<a class="nu-button" href="/course-files/tutorials/tutorial_2_template.zip" target="_blank"> Tutorial 2 Starter Files <i class="fas fa-download"></i></a>

Alright, now that you've read the goals and downloaded the template files, let's get started!

* * *

## Part 1: Iterating Over Lists of Lists

Let’s start by experimenting with `list`s and the built-in iterators `map`, `foldl`, and `filter`.

### Activity 1

Define a list of numbers and call it `num-list`. The numbers in the list are 2, 3, 4, 5, and 6.

BEFORE YOU DO SO, go ahead and write a `check-expect` (that's right, we're going to write a test BEFORE we write a program). In this case, we don't care WHAT is in this list...just that it's a list, so we'll use the `list?` predicate to make sure the thing we make actually is a list. We'll give you this one, but from now on, you're going to have to write your own:

```racket
(check-expect (list? num-list)
              true)
```

### Activity 2

Now use `map` to write a function, call it `list-times-5`, that takes as input a list and multiplies each number in `num-list` by 5, and returns the result. **Write a `check-expect` for your whatever values you put in `num-list`. For example, if you had used `(list 0 1 2 3)`, then you'd be expecting to get back `(list 0 5 10 15)`.

* First step: write the code to work with one element of the list. So start by writing a function that takes a number and multiplies it by 5. You don’t have to worry about giving it a name, you can just write it as a lambda
* Second step: run the code over all the elements of the list. Now map that function over the list
* Third step: turn it into a function.

### Activity 3

Now use `filter` to write a function, call it `activity-3`, that finds all the numbers that are **either** equal to 2 or greater than 5. Don't forget to write some tests to try your function on! Make sure to test it on your `num-list`

* First step: write the code to test an element of the list. Start by writing a function that take a number and returns `true` if the number is 2 or greater than 5. Again, you don’t need to give it a name. You can just write a `lambda`.
* Second step: run the code over the list. Now write a `filter` expression to use this function to find the desired elements of the list.
* Third step: turn it into a function.

### Activity 4

Next use `foldl` to write a function, call it `my-sum`, to compute the sum of the numbers in a list. Check it against the sum of your `num-list`.

* First step: write the code to combine two elements of the list. In this case, that’s super easy: + will do it.
* Second step: run it over all the elements of the list. Write a `foldl` expression to run + on all the elements of the list.
Third step: turn it into a function.
> Hint: the sum of the empty list (a list with no elements) is just zero.

### Activity 5

And now we are going to use `andmap` to determine whether **all** the numbers in a list are less than three. Write a function that does this, call it `all-less-than-three?`, and make sure to test it on your `num-list` list.

* First step: write the code to test one element. Write a function that tests if a number is less than three
* Second step: run it over all the elements of the list. Use `andmap` and the function you just wrote to test if ALL a list's elements less than three.
* Third step: turn it into a function

### Activity 6

Next try and see if you can find another function to determine whether **any** of the numbers in a list are less than three. Make a new function called `any-less-than-three?` and write a test using at least your `num-list`.

### Activity 7

We’ve included a list of lists of numbers in a variable called `lst-of-lsts`. Write a function to compute the product of all the numbers in all its sublists and call it `lists-product`.

* First step: write the code to compute the product of one sublist. Start by writing a function to compute the product of all the numbers in a list of numbers. That is, to multiply all the numbers in the list together. Notice that this is a lot like computing a sum!
* Second step: run it on each the sublists of the list to build a new list where each element is a product of a sublist. Which iterator (`map`, `filter`, or `foldl`) seems appropriate for this purpose?
* Third step: turn it into a function.

### Activity 8

We’ve put a list of strings in your Racket file called `word-list`.  Write a function, call it `first-letter-word-maker`, to append together the first character of each string in the list.

* Start by making a list of the first characters.
  * First step: write a function that takes a string and returns just the first character. Look up the `substring` function in the documentation to see how to do this.
  * Second step: run it on all the elements of the list. You’re an old hand at this now. You should know how to do it!
* Now append all those first characters together.
    * First step: Write a function that will append two strings together (you actually don’t need to do this. There’s already one build in called `string-append`).
    * Second step: now run that function on all the elements of the list to squeeze them into one string. Again, does this seem like a job for `map`, `fold`, or `filter`? (Note: if you get the `string` but backwards...consider which direction you're going in!)

* * *

## Part 2: Building `struct`ures!
This one is a lot simpler. Remember that you define a new kind of data type by saying:

```racket
(define-struct type-name (field-names ...))
```

Where `type-name` is the name to give to this new kind of data and `field-names` are the names to give to the different bits of information you want to keep track of in this new data type. So, for example, if we wanted to keep track of information about `employee`s of a company, specifically, their first name, last name, and social security number (SSN), we might write something like this:

```Racket
(define-struct employee (first-name last-name ssn))
```

When we run that, the system makes a bunch function automatically:
* `(make-employee first-name last-name ssn)` makes a new employee object with the specified info stored inside it.
* `(employee? object)` is a **predicate** that tests if object is specifically an `employee` object or some other kind of data.
* `(employee-first-name e)` takes an employee object `e` and returns the `first-name` stored in it.
* `(employee-last-name e)` takes an employee object `e` and returns the `last-name` stored in it.
* `(employee-ssn e)` takes an employee object `e` and returns the `ssn` stored in it.

> Note: those last three are sometimes referred to as **getters** or **accessors** because they help us extract different property values from some given object.

### Activity 9

Suppose you want to represent information about cats, because cats are purrfect. Specifically, you want to represent their name, [their breed](https://www.google.com/search?q=breeds+of+cat), their hair color, and the volume of their meow. Write a `define-struct` expression for a new data type, `cat`, that contains the fields `name`, `breed`, `hair-color`, and `meow-volume`.

### Activity 10

Now pick a name, breed, color, and meow volume for a cat (the first three should  be `string`s and the last one should be a number between 0 and 10) and make a `cat` object to represent it. Store it in the variable `my-cat`.

### Activity 11

Now write expressions to get the `name` and `breed` of the cat object in `my-cat`. Store them in `got-the-name` and `got-the-breed` respectively.

* * *

## Getting Credit for Your Work
If you're in class make sure to check-in with your PM with your name and NetID. You don't need to submit a `rkt` file. Your attendance will be posted on Canvas by around 5pm today.

If you're submitting remotely, you MUST submit your completed tutorial to Canvas and it will be graded for completion.

Before turning your assignment in, **run the file one last time** to make sure that it runs properly and doesn’t generate any exceptions, and all the tests pass.

Then, make sure to read the [Autograder Guide](https://canvas.northwestern.edu/courses/178849/pages/whats-an-autograder) one last time. Not only is it a useful check of your work, but it will also tell you which file you should submit.

> Note: For groups that may have a significant amount of programming experience, your PM may suggest trying out the <a target="blank" href="https://bain-cs111.github.io/assignments/adv-tutorial-2">Advanced Tutorial 2</a>
