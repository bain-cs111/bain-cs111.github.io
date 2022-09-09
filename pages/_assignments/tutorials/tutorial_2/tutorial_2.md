---
layout: assignment-two-column
title: Composing Composite Data
type: tutorial
abbreviation: Tutorial 2
draft: 0
points: 100
num: 3
description:
due_date: 2022-10-12
---

# Tutorial 2: Composing Composite Data

For this tutorial, you’ll experiment with `list`s and `structs`. **Make sure to write `check-expects` for each expression you write to test its correctness.**

***
## Part 1: Iterating Over Lists of Lists

Let’s start by experimenting with `list`s and the built-in iterators `map`, `foldl`, and `filter`.

**Activity 1**. Define a list of numbers and call it `num-list`. The numbers in the list are 2, 3, 4, 5, and 6.

**Activity 2**. Now use `map` to write an expression that multiplies each number in `num-list` by 5.
* First step: write the code to work with one element of the list. So start by writing a function that takes a number and multiplies it by 5. You don’t have to worry about giving it a name, you can just write it as a lambda
* Second step: run the code over all the elements of the list. Now map that function over the list

**Activity 3**. Now use `filter` to write an expression that finds all the numbers that are **either** equal to 2 or greater than 5.
* First step: write the code to test an element of the list. Start by writing a function that take a number and returns `true` if the number is 2 or greater than 5. Again, you don’t need to give it a name. You can just write a `lambda`.
* Second step: run the code over the list. Now write a `filter` expression to use this function to find the desired elements of the list.

**Activity 4**. Next use `foldl` to write an expression to compute the sum of the numbers in `num-list`.
* First step: write the code to combine two elements of the list. In this case, that’s super easy: + will do it.
* Second step: run it over all the elements of the list. Write a `foldl` expression to run + on all the elements of the list.
> Hint: the sum of the empty list (a list with no elements) is just zero.

**Activity 5**. And now we are going to use `andmap` to determine whether **all** the numbers in `num-list` are less than three.
* First step: write the code to test one element. Write a function that tests if a number is less than three
* Second step: run it over all the elements of the list. Use `andmap` and the function you just wrote to test if ALL a list's elements less than three.

**Activity 6**. Next try and see if you can find another function to determine whether **any** of the numbers in a list are less than three.

**Activity 7**. We’ve included a list of lists of numbers in a variable called `list-of-lists`. Write an expression to compute the product of all the numbers in all its sublists.
* First step: write the code to compute the product of one sublist. Start by writing a function to compute the product of all the numbers in a list of numbers. That is, to multiply all the numbers in the list together. Notice that this is a lot like computing a sum!
* Second step: run it on each the sublists of the list to build a new list where each element is a product of a sublist. Which iterator (`map`, `filter`, or `foldl`) seems appropriate for this purpose?

**Activity 8**. We’ve put a list of strings in your Racket file called `word-list`.  Write an expression to append together the first character of each string in the list.
* Start by making a list of the first characters.
  * First step: write a function that takes a string and returns just the first character. Look up the `substring` function in the documentation to see how to do this.
  * Second step: run it on all the elements of the list. You’re an old hand at this now. You should know how to do it!
* Now append all those first characters together.
    * First step: Write a function that will append two strings together (you actually don’t need to do this. There’s already one build in called `string-append`).
    * Second step: now run that function on all the elements of the list to squeeze them into one string. Again, does this seem like a job for `map`, `fold`, or `filter`? (Note: if you get the `string` but backwards...consider which direction you're going in!)
***
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

**Activity 1**. Suppose you want to represent information about cats, because cats are purrfect. Specifically, you want to represent their name, [their breed](https://www.google.com/search?q=breeds+of+cat), their hair color, and the volume of their meow. Write a `define-struct` expression for a new data type, `cat`, that contains the fields `name`, `breed`, `hair-color`, and `meow-volume`.

**Activity 2**.	Now pick a name, breed, color, and meow volume for a cat (the first three should  be `string`s and the last one should be a number between 0 and 10) and make a `cat` object to represent it. Store it in the variable `my-cat`.

**Activity 3**.	Now write expressions to get the `name` and `breed` of the cat object in `my-cat`.

***
## Getting Credit for Your Work
If you're in class, make sure to submit the Google Form with the secret word and your group number; you don't need to submit a `rkt` file. If you're submitting remotely, you MUST submit your completed tutorial to Canvas and it will be graded for completion.
