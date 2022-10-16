---
layout: assignment-two-column
title: Building Racket Interpreter in Racket, pt 2
type: tutorial
abbreviation: Advanced Tutorial 3
draft: 1
points: 100
num: 1
canvas_id: 1140153
description:
due_date: 2022-10-19
---

> Note: If you're completing the advanced tutorial remotely and plan to submit your RKT file, **please make sure your file name has in it the string `adv`**, otherwise we will assume you are submitting the classic version of the tutorial.

In this tutorial, we’ll add support for `if` and `lambda` to the interpreter.

<a class="nu-button" href="https://bain-cs111.github.io/course-files/tutorials/adv_tutorial_3_template.zip" target="_blank">
    Advance Tutorial 3 Starter Files <i class="fas fa-download"></i>
</a>

* * *

## Updates from Last Week's Template

We’ve also had to change the code for the old interpreter a little from last week.

### Sussman Form

The following is part of the language we will use to WRITE our interpreter but does not need to be processed by the interpreter itself.

So you know how we have been using `lambda` to denote the creation of functions? Even when we make named functions? You don't actually have to do that. Instead there's a shortcut (in programming language design, this is sometimes called _syntactic sugar_).

Say we want to make the function `my-sum` that takes as input a list of numbers and returns the sum. Before we'd do it by saying:

```racket
(define my-sum
  (lambda (a-list)
    (apply + a-list)))
```

Well there's actually a short hand for this which is called "Sussman form" (named after one of the authors of our auxiliary textbooks.)

```racket
(define (my-sum a-list)
  (apply + a-list))
```

In other words, the `lambda` symbol itself can be skipped. Instead, we just say `(define (func-name input-1 input-2 ...) output-expression)` as shorthand. You'll see this in the template file. Sussman form is equivalent to what we have been using (actually when Racket processes your code it transpiles it to include that lambda symbol) and is literally just an optional shorthand you can use.

### More dictionaries

Since we’re going to be adding `lambda` expressions to the interpreter, and lambdas have arguments (variables to hold the inputs to the procedure), we need some way to let the dictionary change over time. To do that, we’ve modified the old interpreter so that most of the procedures take a dictionary as an additional input. That way we can use different dictionaries in different contexts.

We also added a new procedure, `extend-dictionary`, that takes a list of variables, a list of values for those variables, and an existing dictionary, and returns a new dictionary that has all the new variables and values, followed by all the old ones.

### A new kind of procedure

In last week’s interpreter, when you ran `(+ 1 1)`, the interpreter evaluated `+` and got back Racket’s `+` procedure. So all it had to do to finish the procedure call was to use apply to run Racket’s `+` procedure.

However, now we have to allow for the possibility that when the interpreter runs something like `(f 1)`, that `f` will be the result of a lambda expression like `(lambda (x) (+ x 1))`. In that case, we run the procedure call `(f 1)` by calling evaluate on the source code `(+ x 1)` from the lambda, only now in a dictionary in which `x` is set to `1`.

All of this means we need a new kind of data object to hold that source code in it so when the interpreter tries to call it, it can both recognize that it’s not a built-in Racket procedure, and also know what source code it needs to run. These data objects are traditionally called **_closures_** (you'll learn more about these in COMP_SCI 321), but in our code, we’ve called them `interpreted-procedures` since that’s more explicit. There’s a `define-struct` in the code for interpreted procedures. They have three fields:
* `inputs`: a list of the names of the variables for the procedure’s arguments
* `result`: the expression to run to compute the output of the procedure
* `dictionary`: the dictionary that was in use when the lambda was executed to make this interpreted-procedure. Remembering the dictionary will allow a lambda to use the local variables of the code around it.  

So for example, if we run `(lambda (x) (+ x 1))` while the dictionary is `((x 1) (y 2))`, we’ll get back an `interpreted-procedure` whose:
* `inputs` are `'(x)`
* `result` is `'(+ x 1)`
* `dictionary` is `'((x 1) (y 2))`.

Take a moment and read through the code. We’ve marked what’s been added, and what’s been changed.

* * *

## Adding `if` expressions
In our old version, `evaluate-complex` always called `evaluate-procedure-call`. Write a new version of `evaluate-complex` that calls `evaluate-if`, when the expression starts with the symbol `if`, and otherwise calls `evaluate-procedure-call`. You can check if a value is the symbol `if` by saying:

```racket
(eq? value 'if)
```

Now all you have to do is write `evaluate-if`. We have a skeleton for it in the code. Fill it in.

* * *

## Adding `lambda` expressions
Great. Now modify `evaluate-complex` so that if the expression starts with `lambda`, it calls `evaluate-lambda`. Then write `evaluate-lambda`. Again, we have a skeleton for you to start from in the code. Remember that `evaluate-lambda` is going to return an `interpreted-procedure` object, so it needs to call `make-interpreted-procedure` with values for the `inputs`, the `result`, and the `dictionary`.

* * *

## Reimplementing procedure calls

As before, procedure calls are implemented by first recursively evaluating all the subexpressions, then calling the value of the procedure expression on the values of the argument expressions. However, this time we have two kinds of procedures to worry about: built-in, primitive Racket procedures, like `+`; and `interpreted-procedures` created by `lambda`.

Write `evaluate-procedure-call` so that it recursively finds the values of the subexpressions, as before. But now, instead of always using `apply` to run the procedure on the arguments, it instead calls `apply-interpreted-procedure`, **if** the procedure is an `interpreted-procedure object`. Otherwise, it can use `apply` as before.

Finally, write `apply-interpreted-procedure`. It should make a new dictionary using the argument names from the `interpreted-procedure` object and the argument values being passed to it, as well as the dictionary from the `interpreted-procedure` object. Then it can just call `evaluate` on the result expression from the `interpreted-procedure` object, and the new dictionary.

* * *

## This is almost a complete programming language!

The one thing we haven’t implemented is `define`, which will have to wait until we get to imperative programming. That’s why we have test cases that look like:
```racket
(check-expect (evaluate '((lambda (x) (+ x 1))
                          1)
                        dictionary)
              2)
```

It’s the only way we can test running an interpreted procedure for the moment, but we’ll get to `define` soon.

Not having `define` also means we also don’t have _recursion_ itself (in our constructed language), which is a serious bummer. There is technically a way of implementing recursion using only what we have here. It’s called the [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus), and there’s a famous [startup accelerator](https://en.wikipedia.org/wiki/Y_Combinator) named after it. But both are way outside the scope of this course.

* * *

## Getting Credit for Your Work

If you're in class make sure to check-in with your PM. You don't need to submit a `rkt` file. Your attendance will be posted on Canvas by around 5pm today.

> **Note**: If you're completing the advanced tutorial remotely and plan to submit your RKT file, **please make sure your file name has in it the string `adv`**, otherwise we will assume you are submitting the classic version of the tutorial.

If you're submitting remotely, you MUST submit your completed tutorial to Canvas and it will be graded for completion. Make sure your that all of the built-in `check-expect`s pass and your procedures are named correctly.
