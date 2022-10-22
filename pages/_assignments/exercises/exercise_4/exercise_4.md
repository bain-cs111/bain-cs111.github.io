---
layout: assignment-two-column
title: Practicing Recursion
abbreviation: Exercise 4
type: homework
due_date: 2022-10-24
ordering: 5
draft: 0
canvas_id: 1140195
points: 100
---
<script type="text/javascript" async
  src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML"></script>

In this assignment, you will practice writing recursive functions using ordinary recursion (i.e. without accumulators), and iterative recursion (i.e. with accumulators).

For each problem, complete the following funcess:

1. Write the type signature for the function. Example:
```racket
; fact : number -> number
```
2. Write the problem statement for the function. Example:
```racket
; compute the factorial of the given number
```
3. Determine the base case for your recursion. This is the simplest case, in which the answer is trivial. Example:
```racket
; (fact 0) => 1
```
4. Write a test case (using `check-expect`) for the base case. Example:
```racket
(check-expect (fact 0) 1)
```
5. Write multiple test cases for the recursive case. Remember to test for all **edge cases** or situations where your function might break. Example:
```racket
; using Racket's * function to avoid having to do math
(check-expect (fact 1) 1)
(check-expect (fact 5)
                 (* 5 4 3 2 1))
```
6. Finally, write the code itself. Remember that although the specifics will vary, you generally want to have a pattern like this:

<pre>
(if <i>BaseCase?</i>	   ; (= n 0)
    <i>BaseCaseResult</i> ; 1
    <i>RecursiveStep</i>) ; (* n (fact (- n 1)))
</pre>

If you have multiple cases, remember that the _RecursiveStep_ can be any Racket expression, including another `if` expression.  Note that the _RecursiveStep_ often takes the form:

<pre>
(<i>Combiner FirstPartOfData</i>
         (<i>RecursiveCall RestOfData</i>))
</pre>

> **VERY VERY IMPORTANT**: Unless otherwise stated, you may not use `map`, `filter`, `foldl`, `foldr`, `apply`, or `build-list`. You will not receive credit for solutions using these functions.

<a class="nu-button" href="/course-files/exercises/exercise_4_template.zip" target="_blank">
    Exercise 4 Starter Files <i class="fas fa-download"></i>
</a>

## Part 1: Ordinary Recursion

For this section, use ordinary recursion (without an accumulator) for all your functions. This is the simplest form of recursion, and the first version we introduced in the recordings (Lecture 11).

### Question 1: `product-of`
Write a recursive function, `product-of`, to compute the product of a list of numbers.
```racket
; product-of : (listof number) -> number
```

For example, `(product-of (list 1 2 3 4))` should return 24.

* Be sure to follow the steps listed in the introduction. You must include your own test cases, signatures, and purpose statements.
* Note that the product of the empty list is defined to be 1, not 0.[^1]

[^1]: If you’re into math, that’s because it’s usually most useful to define the sum or product of an empty list as being the "identity element" for the operation you’re performing (adding or multiplying). The identity element is the number you can add/multiply another number by without changing the other number. So 0 is the identity element of + because $x = x + 0$. But 1 is the identity element of multiplication because x = x * 1. Wow I just really love the properties of rational numbers.

### Question 2: `my-iterated-overlay`

Time for everyone's favorite sequel: "Iterated Overlay Strikes Back."

Write a recursive function, `my-iterated-overlay`, that behaves exactly the same way as `iterated-overlay`. If you’ve forgotten how `iterated-overlay` works, look it up in the documentation.
```racket
; my-iterated-overlay : (number -> image) number -> image
```
Here are some pointers and hints:
* You will need to return a blank image if the number of iterations is zero. You can do this by returning `empty-image` (no parentheses needed – it’s a constant, not a function).
* Remember that calling `(iterated-overlay func 5)` calls func with the sequence 0,1,2,3,4.  More generally, calling `(iterated-overlay func count)` will generate the sequence $$0,1,...,\text{count} − 1$$.
* Remember that `(overlay a b)` puts a on top of b, and `(overlay b a)` puts b on top of a. You need to make sure the iterations stack appropriately. For instance, if `gen` is an image generator function, the result of calling `(my-iterated-overlay gen 5)` should place `(gen 0)` on top, then `(gen 1)`, and so on, with `(gen 4)` at the bottom.

Calling your function like this:

```racket
(my-iterated-overlay (lambda (n)
                       (square (* n 50)
                               "solid"
                               ; make each iteration progressively lighter
                               (color (* n 50)
                                      0 0)))
                     5)   ; number of iterations
```

should return a result like this:

<img alt="my-iterated-overlay example output" src="/assets/exercise_4/my-iterated-overlay.svg"/>

> Note: DON'T COPY AND PASTE THIS IMAGE AS A TEST. Instead, generate the image using `iterated-overlay` and compare it directly to your `my-iterated-overlay`.

> **Remember**: You can't use `foldl`, `foldr`, `apply`, etc.

### Question 3: `iterated-any`

Now you will abstract your code from `iterated-overlay` just like we did in Exercise 2. Write a recursive function, `iterated-any`, that takes an arbitrary **combiner** (a function with the signature: `image  image -> image`), a generator, and finally a number of iterations, and combines the results using the specified combiner.

```racket
; iterated-any:
; (image image -> image)  (number -> image) number -> image
; ~~~~~~~~~~~~~~~~~~~~~   ~~~~~~~~~~~~~~~~  ~~~~~~
; <Combiner>	          <Generator>	    <Count>
```

You can test your function against your implementation of `my-iterated-overlay` from the previous question.

Recall that `iterated-overlay`, `iterated-beside`, `iterated-above`, etc. all perform the same general task: they call a generator some number of times and then combine the resulting images into a single return value. The name of each function specifies the combiner used:

```racket
; square-gen : number -> image
(define (square-gen n)
  (square (* n 10)
          "solid"
          (color (* n 50)
                 0 0)))
(check-expect (iterated-beside square-gen 3)
              ; Equivalent to using `beside` to combine
              ; all of the iteration results
              (beside (square-gen 0)
                      (square-gen 1)
                      (square-gen 2)))
```

Namely, passing in overlay as our combiner:

<pre>(iterated-any overlay <i>Generator Count</i>)</pre>

should be equivalent to just calling my-iterated-overlay:

<pre>(my-iterated-overlay <i>Generator Count</i>)</pre>

which, in turn, should be equivalent to calling the original:

<pre>(iterated-overlay <i>Generator Count</i>)</pre>

The same holds for `iterated-beside`, `iterated-above`, etc.

***
## Part 2: Iterative Recursion

To wrap up the assignment, you’ll tweak a few of your functions from Part 1 to use **iterative recursion**. Recall that iterative recursion differs from ordinary recursion by the use of an accumulator argument which represents the partial result at that current point in time.

### Question 4: `product-of/iter`

Rewrite `product-of` as an iterative recursive function, `product-of/iter`. The functions should behave identically and have the same signature:

```racket
 ; product-of/iter : (listof number) -> number
```

However, `product-of/iter` must be implemented iteratively, which requires adding an extra argument to keep track the running product of all the list elements funcessed so far, sometimes called an "accumulator." This means you need to define a separate function, called a **helper function**, that uses the accumulator, then have `product-of/iter` call that function. You can do this using an entirely separate helper function, or a local expression. See the example of `length` (the main function) and `length-loop` (the helper) from the lecture on iterative recursion for an example.

### Question 5: `my-iterated-overlay/iter`

Rewrite `my-iterated-overlay` as an iterative recursive function, `my-iterated-overlay/iter`. Is that the greatest name for a function? No...but it's just the way it worked out. Once again, this function should behave identically to my-iterated-overlay and have the same signature, but must use a helper function with an accumulator.

```racket
; my-iterated-overlay/iter : (number -> image), number -> image
```

### Question 6: `iterated-any/iter`

Same deal here. Rewrite `iterated-any` to use an accumulator.

```racket
; iterated-any:
; (image image -> image) (number -> image) number -> image
; ~~~~~~~~~~~~~~~~~~~~~~  ~~~~~~~~~~~~~~~~  ~~~~~
; <Combiner>              <Generator>	    <Count>
```
* * *

## Turning It In

Make sure you’ve followed the process outlined in the introduction for every function, and that you’ve thoroughly tested your functions for all possible edge cases.

Double check that you are not using `map`, `filter`, `foldl`, `foldr`, `build-list`, `apply`, or the original `iterated-images.rkt` file in any of your functions.

Before turning your assignment in, **run the file one last time** to make sure that it runs properly and doesn’t generate any exceptions, and all the tests pass. Make sure you've also spent some time writing your OWN `check-expect` calls to test your code.

Then, make sure to read the [Autograder Guide](https://canvas.northwestern.edu/courses/178849/pages/whats-an-autograder) one last time. Not only is it a useful check of your work, but it will also tell you which file you should submit.

Assuming they do, submit on Canvas. 

* * *

## Requesting an extension
If you need to request an extension on this assignment use the <a href="https://forms.gle/fWx9jgQTNp56bAgR6">Extension Request form</a>. Please see this Syllabus for requirements. Your extension is automatically accepted if you meet the conditions. You will see your due date on Canvas update 24 hours prior to the original deadline.
* * *
