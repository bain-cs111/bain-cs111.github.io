---
layout: assignment-two-column
title: Building Racket Interpreter in Racket, pt 3
type: tutorial
abbreviation: Advanced Tutorial 5
draft: 0
points: 100
num: 6
canvas_id: 1141822
due_date: 2022-10-26
description:
---

> Note: If you're completing the advanced tutorial remotely and plan to submit your RKT file, **please make sure your file name has in it the string `adv`**, otherwise we will assume you are submitting the classic version of the tutorial.


In this tutorial, we’ll look at how many special forms, such as cond and local, are actually implemented by rewriting them into other kinds of expressions the interpreter already understands.

<a class="nu-button" href="https://bain-cs111.github.io/course-files/tutorials/adv_tutorial_5_template.zip" target="_blank">
    Advance Tutorial 5 Starter Files <i class="fas fa-download"></i>
</a>

* * *

## Macros

A macro is like a procedure in that it tells how to perform some operation by describing it in terms of other operations.  But unlike a procedure, a macro works by literally translating the source code into new source code, and then gives the new source code to the interpreter or compiler to be executed.

* * *

## `cond` expressions

For example, we’ve talked about `cond` in class. A `cond` expression has a series of “clauses”, each of which has a test expression and a result expression. It runs the test expressions of the clauses, in turn until it finds one that evaluates to true, and then evaluates its associated result expression and returns that.

So if, in the following expression, `x` happens to be a list, the following code will return the string `"it’s a list!"`:

```racket
(cond ((number? x) "it’s a number!")
      ((string? x) "it’s a string!")
      ((list? x) "it’s a list!")
      (else "I don’t know!"))
```

In other words, it’s equivalent to a bunch of nested ifs:
```racket
(if (number? x)
    "it’s a number!"
    (if (string? x)
        "it’s a string!"
        (if (list? x)
            "it’s a list!"
            "I don’t know!")))
```

But the `cond` version is considered by most programmers to be easier to type and to read.

`cond` is a macro. When you run a piece of code with `cond` in it, the system literally translates the code into the equivalent set of nested ifs and runs (or compiles) that. Remember that the code is really just a data structure, and in the case of Racket, it’s a `list`.

How do we write a procedure to transform the source code for the cond expression, represented as nested lists, into the source code for the if version, also represented as nested lists is straight-forward?  Well, we can define cond schematically like this:

* `(cond (test result) other-clauses …)` is equivalent to `(if test result (cond other-clauses …))`
* `(cond (else exp))` is equivalent to just `exp`

The first of these gives us a recursion: a clause introduces an if whose alternative is transformed version of the other clauses. And the second point gives us the base case: when we get to an else, we don’t need any more ifs. Here’s some code that does that. It takes the source code for a `cond`, pulls out the clauses, and calls `expand-clauses` on them. `expand-clauses` recurses down the clauses, generating ifs until it hits an else clause:

```racket
;; expand-cond: list -> list
;; Transforms a cond expression into the equivalent set of nested ifs.
(define (expand-cond cond-exp)
  (local [(define (expand-clauses clauses)
            (local [(define next-clause (first clauses))
                    (define test (first next-clause))
                    (define value (second next-clause))]
              (if (eq? test 'else)
                  value
                  (list 'if
                        test
                        value
                        (expand-clauses (rest clauses))))))]
    (expand-clauses (rest cond-exp))))
```

* * *

## The macro dictionary

We’ve added a second dictionary, uncreatively called macro-dictionary, that maps names of macros to procedures that transform them.  For example, it maps the name cond to the expand-cond procedure.  Then, we’ve given you a procedure, macro-expander, that given a name returns the transformer procedure associated with it, if its argument is the name of a macro, or otherwise just returns false. So:

```racket
(macro-expander 'cond)
```

returns `expand-cond`, but `(macro-expander '+)` returns `#f`.

* * *

## Modifying the interpreter to support macros

We’ve made the interpreter slightly more complicated since last time.  Before, when `evaluate-complex` found something that wasn’t if or `lambda`, it called `evaluate-procedure-call` since the only other kinds of expressions were procedure calls.  Now, it’s possible to have a macro call. So we’ve added a procedure called `evaluate-call` that has to look at the call, figure out if it’s a macro call, and if so expand it and run the expanded code, otherwise call `evaluate-procedure-call` as before.

Fill in the code for `evaluate-call`. If you did it right, then all the old check-expects for evaluate should still work, but the new ones that evaluate code containing cond should also work!

* * *

## Writing `and` and `or`

We said in class that and and or are procedure, but they’re actually not. Like `cond`, they’re macros that expand into ifs.  The expression `(and a b)` expands into `(if a b #f)`. And the expression `(and a b c)` expands into `(if a (if b c #f) #f)`.  Here’s the general pattern:

*	`(and a other-stuff …)` expands into `(if a (cond other-stuff …) #f)`
*	`(and a)` expands to just `a`

Again, the first of these gives us a recursion: for each argument to and, we make an if with the argument, then the result of the recursion on the remaining arguments, and otherwise false. The last point gives us the base case: when we only have one argument, just use that.

Fill in the code for `expand-and` and test it out.

Why is and defined this way? Because if it were a procedure, it would always run all its arguments, because the interpreter always runs of the arguments of a procedure.  But defining it in terms of if, we make a version that stops running the arguments once we find an argument that’s false, at which point it returns false. If it gets through all of them without getting false, it returns true.

Now do `expand-or`. Or works by running arguments until it finds one that’s true and returns true. The pattern is:

* `(or a other-stuff …)` expands into `(if a #t (cond other-stuff …))`
* `(or a)` expands to just `a`

So once again, this gives us a recursion.

* * *

## Writing `local`

`local` is also a macro. We’re going to implement a simplified version of `local` that just expands into a procedure call. If we say:

```racket
(local ((define a 1)
        (define b 2))
  (+ a b))
```

We can get the same effect by running:

```racket
((lambda (a b) (+ a b)) 1 2)
 ```

The lambda expression makes a procedure that puts its arguments in variables `a` and `b`, then returns `(+ a b)`. That’s what we want here, only we want `a` to be `1` and `b` to be `2`.  So we just call the procedure with `1` and `2` as arguments, and the procedure obediently puts those values in variables `a` and `b`, then returns `(+ a b)`.

Fill in the code for `expand-local`. Note that this one doesn’t need to be a recursion, although feel free to write one if you really want to. But this one is easier to do using some calls to cons, list, and map.

* * *

## Getting Credit for Your Work

If you're in class make sure to check-in with your PM. You don't need to submit a `rkt` file. Your attendance will be posted on Canvas by around 5pm today.

> **Note**: If you're completing the advanced tutorial remotely and plan to submit your RKT file, **please make sure your file name has in it the string `adv`**, otherwise we will assume you are submitting the classic version of the tutorial.

If you're submitting remotely, you MUST submit your completed tutorial to Canvas and it will be graded for completion. Make sure your that all of the built-in `check-expect`s pass and your procedures are named correctly.
