---
layout: assignment-two-column
title: Recursion
type: tutorial
abbreviation: Tutorial 3
draft: 0
points: 100
num: 4
description:
due_date: 2022-10-19
---

Today's tutorial is all about practicing recursion. Remember, **recursion** itself is a very simple concept: a function is **recursive** if it calls itself in its definition.

[Relevant "XKCD" Comic Strip](https://thomaspark.co/2017/01/relevant-xkcd/) (totally worth a click):

![XKCD Comic About Recursion](/assets/tutorial_3/xkcd.png)

***
## Problem 1 - `factorial`

Write the **factorial** function $n!$ as a **recursive** function.  The factorial of a number is the product of all the numbers from one to that number:

$$ n! = \prod_{i=1}^{n}i = 1 * 2 * 3 * \ldots * n $$

So $1! = 1$, $2! = 1 * 2 = 2$, $3! = 1 * 2 * 3 = 6$, $4! = 24$, $5! = 120$, and so on. Here are some subgoals to accomplish on your way to writing the function:

_Subgoal 1_. What is the type of `factorial` (that is, what is its type signature, the types of its input and output)?

_Subgoal 2_. What is the purpose statement for it?

_Subgoal 3_. What should the **base case** be (aka the “easy case”) for the recursion. The base case is an input for the function where you know the answer without having to recurse. What’s a really easy input to compute the factorial of?

_Subgoal 4_. Now time to implement that base case. Write inside your function:
```racket
(if test-for-base-case
    answer-for-base-case
    TODO)
```
Where `test-for-base-case` is something that looks at the input to the function and determines if it’s the base-case input, and answer-for-base-case is the answer for the base case. We’ll fill in `TODO` in a minute.

_Subgoal 5_. What’s the **recursive case**? The recursive case is what runs when the input is anything that's NOT the base case. Basically, our goal is to call ourselves (i.e. `factorial`) with a new input that's "one step closer" to the base case. Once we get that answer back (it could take a lot of recursions), we'll take the answer to that and turn it into the answer for the original problem.

You'll need to decide two things:
* How do you make the input to `factorial` be "one step closer" to your base case?
* If you had the answer for the "one step closer" input, how would you use it to solve for the original input?

_Subgoal 6_. Now fill in the part in your code that says TODO:
* Write your recursive call, i.e. `(factorial easier-input)` where `easier-input` is whatever the easier version of the input is.
* Change it to `(fixit (factorial easier-input))` where `fixit` is something that transforms the answer to `(factorial one-step-closer)` into the answer for the original input.

> **Note**: our notation here is a little misleading because it makes it look like `fixit` should just be the name of a function. But for this problem, you’ll want to pass an additional argument to the function besides the result from the recursive call. That means for this problem, the form will really be:
> ```racket
> (some-function some-argument
>                 (factorial easier-input))
> ```

> **Hint**: Remember, that a factorial can be expressed like this:
> $$5! = (5 - 0) * 4! = (5 - 0) * (5 - 1) * 3! = \ldots $$

Nice work! You've officially written a recursive function. Remember that all you need to do is: 1. find a base case; 2. think of a way of getting the original input "one step closer" to the base case at a time; and 3. think of a way to combine the results to the "one step closer" problems.
***

## Problem 2 - `count-odd`
Write a function

```racket
count-odd: (listof number) -> number
```

that returns the number of odd elements in a list of numbers. Remember that you can determine if a number is odd by calling the predicate `odd?`.

_Subgoal 1_. We’ve already given you a type for `count-odd`. So write a type signature comment.

_Subgoal 2_. Now write a purpose statement in your own words. Don’t start writing code before you start thinking about what you’re trying to accomplish!

_Subgoal 3_. Okay, now what’s the base case? Again, this is a case that’s so easy we don’t have to do much. What would be a `list` where you’d just know the answer?

_Subgoal 4_. Write your skeleton for a simple recursion:
```racket
(if base-case
    base-case-answer
    TODO)
```
and fill in the first two parts.

_Subgoal 5_. Now what’s your recursive case? Again, this has two parts:
* What’s an easier input that’s "one step closer" to your base case?
* How do you transform the answer for the easier input into the answer for the original input? Transforming the answer for this one is a little more complicated because it requires its own `if` expression.

> **Note**: It might be useful to remember two convenient `list` functions, `first` which returns the first element of a given list and `rest` which returns all of the elements _except_ the first one. Check the out in the documentation.

_Subgoal 6_.
Finally, fill in the `TODO` part with your full recursive case. Normally, the recursive case would look something like:
```racket
(fixit (count-odd easier-input))
```

However, since the `fixit` part here needs to involve an `if` expression that will use the answer from the recursive a few different times, it’s probably easier to put the result of the recursive call into a local variable using the `local` expression.

Your code will look something like:
```racket
(if base-case
          base-case-answer
          (local [(define recursive-answer (count-odd easier-input))]
              (if something-is-true
                  do-something
                  do-something-else)))
```

And then `recursive-answer` will get used inside of two of the `do-something`s.

Wow, you're good at this! Two down, two to go!
***

## Problem 3 - `count`
Now abstract your answer from the previous question to make a function
```racket
count: (X -> Boolean) (listof X) -> number
```
that takes a `list`, but also a **predicate**, and returns the number of elements in the list that the predicate returns `true`. For example, `(count odd? my-list)` should do the same thing as `(count-odd my-list)`.

Good news! This does not require coming up with a new base case or recursive case. It requires only abstracting your code (remember, abstraction is both powerful and cool). That is, you should:
* Copy your code for **Problem 2**
* Change the name of the function
* Add an extra input to the function so that the user can input a predicate
* Change the code so that instead of always using `odd?` it instead uses the predicate the user specified. (Make sure to change the `recursive-answer`!)

Wow. Abstraction **and** recursion. This is straight :fire:. Better call the :fire_engine:.
***

## Problem 4 - Seeing the `tree`s from the Forest

Write a recursive function, `tree`, that makes a recursive image something like this:

![Image of a cool tree](/assets/tutorial_3/tree.svg)

How do you make something like this? Here’s the basic idea:
* At its base, it's just a solid green rectangle
* Then, we have just two solid green rectangles rotated at say a 45 degree angle
* REPEAT FOR ALL GREEN RECTANGLES (until you feel like stopping)

Another way of describing it is that tree is a stick for the trunk and then two simpler trees sticking out of it at angles. Those simpler trees are each sticks with two simpler trees sticking out of them at angles. And so on, and so on, until eventually it just draws a stick.

We can write that as a recursion. The function `tree` takes a number of levels of branching (levels of recursion) and gives you back a `image` of that tree with that number of levels. So zero levels of branching is just a stick. One level of branching is a Y shape (a stick with two sticks pointing out of it), two levels of branching is a Y where the branches on top each each Y’s themselves, and so on. That means:

* A tree with zero levels of branching is a stick
* A tree with $n$ levels of branching is a stick with two subtrees that have $n-1$ levels of branching

Here’s what your function should (roughly) produce across inputs from 0 levels of branching up to 9 levels:

![Trees at Different Levels from 0 to 9](/assets/tutorial_3/tree-sequence.svg)

Your trees don’t have to look exactly like ours–that would require a lot of trial and error that you wouldn’t learn anything from. Just experiment with making recursive pictures like this and have fun. You can make some absolutely dope images with this simple pattern.

> **Note**: remember that you’ll need to add `(require 2htdp/image)` to your file to get access to the graphics functions.

Time for some subgoals:
* As always, what is the type of the function? That is, what type of input does it expect and what type of output does it generate?  Remember that the input is the number of levels of branching (i.e. number of levels of recursion).
* Write a type signature comment.
* What is it trying to do in your own words?
* Write a purpose statement
* What’s your base case? That is, for what number of levels of branching do you not need to recurse at all? And what do you return then?
* What’s your recursive case?
* What’s the "one step closer" number of levels of branching that brings you closer to the base case?
* When you recurse, you’re going to make a subtree and you’re going to paste it into the final output twice. So you might as well do just one recursive call and put it in a local variable using local. So write something that looks like:
```racket
  (local [(define subtree (tree simpler-input))]
    TODO)
```
* Finally, you need to fill in `TODO` with something that makes the subtree itself by assembling the two copies of the subtree with a trunk. Rather than make you fiddle for a long time, here's the basic structure, which is:
```racket
(above (beside (rotate angle (scale factor subtree))
               (rotate (* -1 angle) (scale factor subtree))
               (rectangle ... some pretty args ...))
```

Pick whatever `angle` you want (we used 45 degrees). For `factor`, choose some number less than 1, so that the subtrees are smaller than the original stick (by a factor of...well...`factor`). And for `rectangle`, use whatever arguments you want, but you want to be sure the rectangle is narrow and tall.

Feel free to experiment.  You can add little circles at the ends, for example to make something that looks vaguely like leaves.  There’s no specific right answer we’re looking for on this one.

***
## Getting Credit for Your Work
If you're in class, make sure to submit the Google Form with the secret word and your group number; you don't need to submit a `rkt` file. If you're submitting remotely, you MUST submit your completed tutorial to Canvas and it will be graded for completion.
