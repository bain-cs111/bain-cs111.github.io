---
layout: module
title: Iteration
type: lecture
draft: 1
num: 5
description:
due_date: 2022-10-03
slides:
  - url: https://docs.google.com/presentation/d/11OhnO2DrnBaFwO-r1aFlIXGItKn1m_4NOX9BMaicpPw/edit?usp=sharing
    title: "Iteration and Iterators"

exercise_url: "lecture05.zip"
---

**Iteration** is just an intimidating word for "repetition."  An **iterator** is just a piece of code that lets you run another piece of code repeatedly. You usually don’t want to run exactly the same piece of code each time, you want to change it slightly for each iteration (each repetition). For example, if you’re repeating making a box, you might want to change its size and/or color each time. So that’s why the piece of code being repeated wants to take an input: if you’re making boxes of different sizes, then it might take the size of the box as input. And that’s why it makes sense to have the piece of code be a function to run.

An iterator is, in particular, a piece of code that takes a function to run as an argument and runs it repeatedly for some defined set of input values – sizes of boxes, or whatever.  

In the case of functions like `iterated-overlay`, we just have it pass in a "picture number" as input and then if you want to have it make boxes of different sizes, then your function can based the size on the picture number. If you want different colors, you can base the color on the picture number.  If you want different colors and different sizes, then do both.

Of course, there are times when you really do want to make the same box repeatedly, you just want a lot of identical boxes. That’s easy; you just write a function that ignores its input.  That’s perfectly fine.

## Nested iteration
The `boxes` function in the middle of the lecture is an example of **nested iteration**. That just means it has an iterator whose code being iterated has another iterator in it. Again, that’s just fine and is a perfectly common thing to do in programming.

A lot of programming looks like nested iteration. For example, when a spreadsheet program is drawing the screen, it’s probably using one iterator to iterate over rows of the spreadsheet, and another inside it to iterate over the columns. Inside of that is something that draws a single spreadsheet cell.

## For Java and python programmers
If you grew up with Java, Python, MATLAB or a similar language, then doing looping by calling a function that calls another function probably seems weird. But that’s arguably what you’ve been doing all along. If you write a `for` loop in Java to add up a list of integers:

```java
for (int i : listOfInts) {
  sum += i;
}
```

The `{ }` expression is an argument to `for`.  It’s basically a `lambda` expression that doesn’t take an argument (or you can think of i as its argument). So you’ve been using functions as arguments for as long as you’ve been programming in other languages.

> Note:  As we’ll see, you can do the same thing in Racket. You say `(for-each (λ (i) (set! sum (+ i sum))) listOfInts)`.  But Racket also lets you do it without needing λ` or `{ }`: `(apply + listOfInts)`.

The difference is that languages like Java have draconian restrictions on what you can do with `{ }` expressions. You can pass them as arguments to `for`, `while`, and `if` (and a few other things like do, and `try/catch` if you language has those). That’s it. You can’t pass `{ }` as an argument to any code you write and you can store it into a data structure. That means you’re stuck with the iterators that your language gives you.  You don’t have a mechanism for writing your own.

That’s one of the reasons that most languages like Java have added `λ`, although they all have different ways of spelling it.

Being able to treat functions as data and pass them as arguments let you do all kinds of useful things. For example, Java and JavaScript, despite their names, have radically different, and fundamentally incompatible ways of doing object-oriented programming. Each is good for different things. But because it’s wired into the language, you’re stuck with the object system of your language. If your language lets you pass functions around as data, then you can make your own object system. So if you don’t like the one that’s built in, you can make a new one.  You can even mix-and-match.

## Why the rules of computation matter
The good news is that if you understand the rules of computation slide (it will be two slides by the end of class), you’ll get an A in the class.  The bad news is that there’s some subtlety to it and it will probably take a while for you to fully internalize what the rules mean.

The Rules of Computation slide explains how your code gets executed, which means it explains how the computer understands what you’re trying to tell it to do. To the extent that much of programming is debugging, and bugs generally come from a difference between what you think you’re telling the system to do and what you actually told it to do, you can’t hope to debug, or thereby program, unless you understand this slide.

There’s a tendency for people to take a “phrasebook” approach to programming when they learn. Rather than learning what the grammar and meaning of the language is, they try to memorize phrases (fragments of code) for doing specific things. The problem with that approach is that if you don’t understand how the phrases work (what they really mean), then you’re not in a position to be able to even recognize when you’re getting them wrong. So that approach tends to be self-defeating for programming.

Now that said, when you’re writing code, you often do start from stock phrases you remember, and that’s great. It’s just that when you run into a bug, you need to be able to slow down and think about how the computer is going to execute the code in detail. And for that, you need to understand this slide.
