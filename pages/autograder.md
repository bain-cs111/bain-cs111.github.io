---
layout: two-column
title: Autograder and Submission FAQ
permalink: /resources/autograder/
---

# Auto-grading and Submission FAQ

## How does auto-grading work?
The exercises will describe a set of definitions (typically functions) you need to write and what each function should do. We will prepare a set of ​`check-expect​`s to see if those functions run in the same way as required by the exercise. If the functions you write pass the tests we have prepared, you will receive full credit.

For example, if the homework specifies:

```racket
;; Write a procedure add that takes two numbers and returns their sum. ;; add : number number -> number
(define add
  (lambda (number1 number2) "fill me in"))
```

We might run your homework and test it with these `check-expect`s:
```
(check-expect (add 0 0) 0)
(check-expect (add 514 217) 731)
```

The set of ​`check-expect`​s that I'm using will not be disclosed beforehand. This is because you need to be considering whether or not your program works the way you designed it to work!

## Make sure your submission runs without syntax or runtime errors!

The auto-grader needs to ​run​ the file in order to grade it.
* If the file has ​any​ syntax error (missing or extra parens, wrong λ syntax), the auto-grader won't be able to open it.
* If there's ​any​ error showing up in Interaction Window when you click `Run` the auto-grader won't be able to run your file.

In either situation, the auto-grader would not be able to assign partial credits.

## Follow question descriptions literally
Your code should follow the Exercise literally. More specifically,
* Function names (including '-' and '/') must be spelled exactly the same as in descriptions. If the Exercise description asks for an image definition named `flag-of-chicago​`, you cannot change it to `​flag_of_chicago​` or `flag-of-Chicago​`.
* The order of arguments should follow the literal order in Exercise descriptions.
  * _Example._ Implement​ `​iterated-any​`, that takes an arbitrary `combiner` function, a `generator` function, and a number of iterations, and produces...
  * You should write a function with those three arguments `(lambda (combiner generator iteration-number)...)`, where `​combiner​` is the combiner procedure, `​generator​` is the generator procedure and `​iteration-number​` is the number of iterations.
* Data type and the shape of values should match problem descriptions. If the homework description asks you to write a function that returns a list of album​s where an `album` is defined as `(define-struct album (title artist genre))`, you should not return a list of just album titles.
  * Example. If the homework description asks you to write a function that returns a list of paths, you should not return a list of strings.

## If I pass all ​check-expect​s in the starter code, will I receive full credit? Is my code correct?
No. We use our own set of `​check-expect`​s to grade submissions, not the provided ones in the starter code. However, a correct solution will have to pass all given `​check-expect`​s. Therefore any test failure indicates that there are some bugs in your solution.

## Do I need to upload supplementary files iterated-images.rkt​, ​remove_duplicates.rkt​ etc?

**Upload only your work**. We will have the supplementary files ready when grading.

## Testing Your Code
How do I test my submission?
1. Download your submission from Canvas.
2. Put the supplementary file like `​iterated-images.rkt​` or `remove_duplicates.rkt​` in the same directory. Directly use the ones from the provided ZIP file.
3. Open your submission. Click `Run` in DrRacket.
4. Ensure that there's no error and the code passes all tests.

## Do I need to pass all the included tests?
Yes. Of course, you can submit code that does not pass all tests. You will receive credit for all correct problems. However, the provided tests in the starter code at least check whether there are misspellings in function definitions. Therefore failing a test may indicate that there is a typo or that the code is not fully correct.

## Writing Your Own Tests
You should always write new `check-expect`s although we do not grade them. Directly write `check-expect​`s against the problem descriptions and make sure to follow all given instructions. As an example, if the homework specifies _"Write a function longer-than that takes a string, a number and returns if the length of the string is larger than the given number"_
```racket
;; longer-than : number string -> boolean
(define longer-than
(lambda (x1 x2) "Fill me in"))
```
You should first write ​check-expect​s like the following examples before attempting to finish `longer-than`​:
```racket
(check-expect (longer-than "hello" 3) #false)
(check-expect (longer-than "abc" 5) #true)
```

Here are two non-examples which do not follow the given instruction.
```
(check-expect (longer-than 99 "cs111") #false) ;; the argument order is wrong
(check-expect (longer-than "hello" 5) #true)   ;; the spec is ‘>’ rather than ‘>=’
```

## Can I change the name of the supplementary file or the ​require line importing the supplementary file in the starter code?
Don't change it.

## Can I add new ​require​ lines?
We have included necessary ​`(require ...)`​ lines in the starter code, so there should be no need to add or remove any ​`(require ...)`​ lines. In general, you can (only) include `2htdp` teachpacks (all of form ​`(require 2htdp/...)​`) and the supplementary file from the same week. Don't add any additional library outside this scope, even if it is the supplementary file from previous exercises.

## What part of starter code can I change?
You can replace ​"fill me in"​ with your work and write any new definitions. Other than that, you ​should not​ modify, delete or comment out existing definitions. You ​must not​ change provided struct definitions.

## How to save my definitions?
When you hit `control-s` (Windows) or `command-s` (Mac), the changes to definitions will be saved. You can also choose File -> Save Definitions.

Caution: ​never ​use items in File -> Save Other unless you are very certain about what's happening. The files produced by "Save Interactions" or "Save Definitions as Text..." either don't contain your code or are broken.
