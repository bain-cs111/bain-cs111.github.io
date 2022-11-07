---
layout: assignment-two-column
title: Interpreting Imperatives!
type: tutorial
abbreviation: Advanced Tutorial 6
draft: 0
points: 100
num: 7
description:
due_date: 2022-11-09
canvas_id: 1145543
---

> Note: If you're completing the advanced tutorial remotely and plan to submit your RKT file, **please make sure your file name has in it the string `adv`**, otherwise we will assume you are submitting the classic version of the tutorial.

> Note 2: It's unlikely you can complete this whole assignment in 50 minutes and it will more likely take 2 tutorial sessions to finish it up. We'll continue working on this same tutorial assignment next Tutorial time as well.

In this tutorial, we’ll use imperative programming to implement `define`, `set!` and `begin`. We’ve also included an interactive version of the interpreter, called a “repl” or “Read-Evaluate-Print Loop”.

<br>

<a class="nu-button" href="/course-files/tutorials/adv_tutorial_6_template.zip" target="_blank">
    Advanced Tutorial 6 Starter Files <i class="fas fa-download"></i>
</a>

<br>

* * *

## Reimplementing Dictionaries

In the past, we implemented the dictionary for you as a list of sublists. But the student languages don’t allow you to change elements of a list once the list is made, so we need to reimplement the dictionary.

We’re basically going to make something like a list, which you’ll recall is a chain of pairs. In this case, rather than using pairs, we’re going to use a three-element struct containing the name of a variable, the name of its value, and a link to the list of the dictionary. Associations between variables and their values are often called **bindings**, so we’ll call this `struct` a `binding`, since it records the value of a single variable:
```racket
(define-struct binding (variable-name value rest))
```
Here, `variable-name` is the name of a variable, `value` is its value, and `rest` is either another `binding`, or `empty`. So once again, we have a recursively defined data structure:
```racket
; a dictionary is either:
; - empty or
; - (make-binding variable value dictionary)
```

For example, a dictionary that says `x=1`, and `y=2` would look like this:

```racket
(make-binding 'x 1
              (make-binding 'y 2
                            empty))
```

This is just like making a list `'(1 2 3)` by saying `(cons 1 (cons 2 (cons 3 empty)))`, except we’re chaining bindings together rather than pairs.

We’ve rewritten the dictionary to use bindings, but we haven’t rewritten any of the procedures to work with dictionaries. So write the following procedures:

### `(lookup variable-name dictionary)`
As you’ve used in previous weeks. For this, you’ll need a procedure you haven’t used before: `(error message value)`

This generates an exception that prints `message`, along with `value`. When `lookup` is called with a variable that isn’t in the dictionary, you should do:
```racket
(error "Unknown variable: " variable-name)
```

### `(extend-dictionary variable-names values dictionary)`

As in previous weeks.

### `(update-binding! variable-name new-value dictionary)`
This one’s new! It finds the existing `binding` for `variable-name` in the `dictionary` and changes its associated value to `new-value`.  If there is no `binding` for `variable-name`, it should generate an error. You’ll need a new procedure for this: `(set-binding-value! binding new-value)`.

This is created automatically by `define-struct`. It changes the value of the “value” field of an existing `binding` object to the specified value. If you’re a Java or Python programmer, think of it as doing:
```java
binding.value = new-value;
```

* * *

## Using the repl
Once you have the dictionary reimplemented, you should have all the functionality of the old interpreter working. Try running `(repl)`.  This will start up an interactive version of your interpreter that you can type expressions into and see the results. To stop the repl, just type `quit`.

* * *

## Using the debugger
Now try running the repl, but instead of hitting the **run** button, hit the **debug** button. This will compile the program but not start it yet. It should pop up the **pause**, **go**, **step**, **in**, and **out** buttons (with all but _pause_ greyed out), think for a while, and then grey out pause (because it’s now paused) and un-grey-out go and step.

It’s now waiting for you to either hit **step** or **go**. You’re going to hit **go**, which will let it run on its own. But before you do, find the `evaluate` procedure and move the mouse over the open paren for its `if` expression. You should see a little red dot appear in the middle of the paren. Right click and choose _“Pause at this point”_. Now press **go**. It should finish loading the file and give you a prompt in the interaction window. Now type `(repl)` in the interaction window and enter an expression to run. This time, you should see it pause and show you the stack when it gets to the `if` in evaluate.

Hit go again, and it will run to the next call of `evaluate`. Keep pressing go until it finishes running. Now you can type another expression to run or type `quit` or hit **stop**.

Before you move on, you may want to go back to the open paren of the `if` in evaluate, right click on it and choose _“Remove pause at this point.”_

* * *

## Implementing `define`

Now modify the code for evaluate to implement `define` expressions. Remember that `define` adds a new binding to the dictionary. So it should `set!` the global variable `dictionary` to a new, slightly larger, `dictionary` that includes the `variable` and `value`, as well as all the old ones.

* * *

## Implementing `set!`
Now implement `set!`. Remember that `set!` only changes existing bindings. So you should use `update-binding!` instead of creating a whole new `binding`. If you make a new binding, then a loop that kept updating the variable would make the dictionary larger and larger.

* * *

## Implementing `begin`

Finally, implement `begin`.  Remember that `begin` should run all the expressions inside it, in order, and then return the value of the last one. So your base case is a one-element list, **not** the empty list.

* * *
## Congratulations
You now have a reasonably complete imperative Racket interpreter!

* * *

## Making it less Rackety

“I @#@$#% hate Racket!” I hear you say. “I want to use a @#$@#$ while loop!” No problem! You are now the one in charge of your own language!

* Add a while special form to your interpreter: `(while condition body …)`

Should check condition and when it’s true, run all the expressions in body …, then loop like a while loop should.  For example:
```racket
(define x 0)
(while (< x 10)
  (print x)
  (set! x (+ x 1)))
```
should print out: `0123456789`.

* Now add a `for` loop: `(for variable initial-value condition increment body …)`

Note that this one needs to add a new `binding` for `variable` to the dictionary before looping.

For example:
```racket
(for x 0
     (< x 10)
     (+ x 1)
     (print x))
```

should print the same thing as the while loop example.

* * *

## Getting Credit for Your Work
If you're in class make sure to check-in with your PM with your name and NetID. You don't need to submit a `rkt` file. Your attendance will be posted on Canvas by around 5pm today.

If you're submitting remotely, you MUST submit your completed tutorial to Canvas and it will be graded for completion. **Since this one will probably take longer than 50 minutes, you're welcome to just submit your in-progress work.

Before turning your assignment in, **run the file one last time** to make sure that it runs properly and doesn’t generate any exceptions, and all the tests pass.

Then, make sure to read the [Autograder Guide](https://canvas.northwestern.edu/courses/178849/pages/whats-an-autograder) one last time. Not only is it a useful check of your work, but it will also tell you which file you should submit.
