---
layout: assignment-two-column
title: More Recursion Practice
type: tutorial
abbreviation: Tutorial 4
draft: 0
points: 100
num: 5
canvas_id: 1140257
description:
due_date: 2022-10-21
---
<script type="text/javascript" async
  src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML"></script>

Today we'll continue to practice recursion with a specific focus on a specific type of recursion we call **iterative recursion**.

* * *

## Getting Started
Like last time, there are no template files for this Tutorial so you'll need to start from a blank DrRacket file. You can do this by either just opening DrRacket and it'll open a new `Untitled` file or if you already have DrRacket open, you can go to the File menu and hit `New` or `New Tab`.

After you've got a blank document open, might as well save it with a good sensible name somewhere on your computer where you store important docs.

* * *
## Problem 1 - `power`

Write a function, `power`, that raises a number to a power: `(power a b)` should return $$a^b$$. So `(power 4 2)` is $$4^2 = 16$$, `(power 4 3)` is $$4^3 = 64$$, ..., etc. Remember that in math class, for any `a`, `(power a 0)` is 1 (anything to the zeroth power is 1) and `(power a 1)` is `a`. We won't worry about fractional or negative powers.

Time for our subgoals!

What is the type of power (that is, what is its type signature, the types of its input and output)?

Step 1. What is the purpose statement for it?

Step 2. What should the base case be (aka the “easy case”) for the recursion? Note that here we have two inputs to the function. Which one would we be changing as we recurse? Or would we change both of them?

Step 3. Use the usual template for the base case:
```racket
(if test-for-base-case
    answer-for-base-case
    TODO)
```
Where `test-for-base-case` is something that looks at the input to the function and determines if it’s the base case input and `answer-for-base-case` is the answer for the base case. We’ll fill in `TODO` in a minute.

Step 4. What’s the recursive case? Remember the recursive case is what runs when the input is too hard to be handled by the base case. We call ourselves on a version of the input that's one step closer to being the base case. Then we take the answer to that and turn it into the answer for the original problem.  So you can to decide two things:
  * How do you make the input to power be closer to your base case?
  * If you had the answer for the easier input, how would you use it to solve for the original input?

Step 5. Now fill in the part in your code that says TODO:
  * Write your recursive call, i.e. `(power easier-inputs)` where `easier-inputs` are whatever the easier version of the inputs are.
  * Change it to `(fixit (power easier-input))` where `fixit` is something that changes the answer to `(power easier-input)` into the answer for the original input.

* * *

## Problem 2 - `power/iter`
Now write `power` as an iterative recursion. Start by writing a helper function, `power-helper`, that takes an extra accumulator input:

```racket
(define (power-helper number exponent accumulator)
   ...stuff...)
```

And returns the answer (`number` raised to `exponent`). To do that, first answer these questions for yourself:
* Remember that the `accumulator` stores the partial answer. What is the answer you’re ultimately trying to compute (in English)?
* What would a partial answer be like for this problem?
* What should the initial value of the accumulator be?
* We’ll use the same base case as before (when input is zero). What should it return in that case?
* Then how do we write the recursive case?
* Only once you've answered all these questions should you actually write the code for the function!

And finally, write the definition of power itself:

```racket
(define power/iter
  (lambda (number exponent)
   (power-helper ...fill this in...)))
```

* * *

## Problem 3 - `count/iter`

Now write the count function from last time:
```racket
count: (X -> Boolean) (listof X) -> number
```

As an iterative recursion. Again, start by writing the helper function that returns the answer (the number of elements of `list` that satisfy the predicate).

```racket
(define count-helper
  (lambda (predicate list accumulator)
  ...stuff...))
```

To do that:

* Remember that the `accumulator` stores the partial answer. What is the answer you’re ultimately trying to compute (in English)?
* What would a partial answer be like for this problem?
* What should the initial value of the accumulator be?
* We’ll use the same base case as before (when the list is empty).  What should it return in that case?
* And how do we write the recursive case then?
* Now write the code for the helper!

And finally, write the definition of `count` itself.
* * *

## Problem 4 - `any?`
Write a function, `(any? predicate list)` that returns `true` if `predicate` returns `true` for any element of `list`, otherwise it returns `false`.  

> **Note**: Yes, this is the same as `ormap`, we’re just naming it `any?` because `ormap` is already in use and also because `ormap` does not really make sense to someone that isn't already thinking boolean-ly.

* What should the base case be (in English)?
* What should the recursive case be (in English)?

Note that you may write this as an iterative or non-iterative recursion. However, be aware that the iterative version doesn’t need an extra accumulator variable. You can write it with just two arguments, without needing to do a `fixup` step to the answer from the recursive call.

> Note: Think about why this might be the case. Once you find one element that satisfies your `predicate` do you really care if the rest do?

We recommend just writing it and then looking at the result to see whether you ended up with an iterative or non-iterative version.

* * *

## Problem 5 - `every?`
Write a function, `(every? predicate list)` that returns true if `predicate` returns `true` for every element of `list`, otherwise it returns `false`.

> Note: Yep, you guessed it...it's `andmap`! Think carefully about how this is different from Problem 4. Once you find one element that _does not_ satisfy your predicate, do you really care if the rest do or do not?

* What should the base case be (in English)?
* What should the recursive case be (in English)?

You can write this as an iterative or non-iterative recursion.  

* * *

## Getting Credit for Your Work
If you're in class make sure to check-in with your PM with your name and NetID. You don't need to submit a `rkt` file. Your attendance will be posted on Canvas by around 5pm today.

If you're submitting remotely, you MUST submit your completed tutorial to Canvas and it will be graded for completion.

Before turning your assignment in, **run the file one last time** to make sure that it runs properly and doesn’t generate any exceptions, and all the tests pass.

Then, make sure to read the [Autograder Guide](https://canvas.northwestern.edu/courses/178849/pages/whats-an-autograder) one last time. Not only is it a useful check of your work, but it will also tell you which file you should submit.

> Note: Sadly, there's no advanced tutorial today. We'll resume working on our interpreter next week!
