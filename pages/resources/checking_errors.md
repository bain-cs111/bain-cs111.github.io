---
layout: two-column
title: Checking for Syntax or Runtime Errors
permalink: /resources/checking-errors/
---

Before submitting your work, always run it in DrRacket and make sure that it does not contain any errors. If your submission has syntax errors (e.g. missing or misplaced parentheses) or runtime errors, we won't be able to run the code and assign any partial credit.

To check if your code contains syntax or runtime errors, click the Run button in DrRacket and see if there are any error messages in red. Here are some examples.

# Syntax Error 1
The code is missing a close paren.

![Syntax Error Example 1](/assets/images/drr-stx-error1.jpg)

# Syntax Error 2
The body of the `lambda` function should contain exactly one expression. This is caused by a misplaced close paren. The expression `(+ x) y` should be `(+ x y)`.

![Syntax Error Example 2](/assets/images/drr-stx-error2.jpg)

# Runtime Error
When running the code, (add 10 20) at the top-level tries to compute (+ 10 "y"), causing the error. If without the (add 10 20) expression, we will be able to run and grade the code although it is still in correct.

![Runtime Error Example](/assets/images/drr-runtime-error.jpg)
