---
layout: assignment-two-column
title: Shaping Up
type: tutorial
abbreviation: Tutorial 0
draft: 1
points: 100
num: 1
canvas_id: 1124990
description:
due_date: 2022-09-28
---

> Note: BECAUSE THIS FIRST TUTORIAL IS ATTENDANCE REQUIRED YOU WILL NOT ACTUALLY SUBMIT ANY FILE TO CANVAS. The course staff will ask you to verify your name and NetID before you leave class and you will receive credit based on that. 

In this assignment you’ll play with simple functions for making images that we’ll use later on in the course. Make sure to work with the other people in your group. If you find yourself needing help, ask your assigned PM or any of the other course staff members.

## Make Sure DrRacket is Configured

To begin with, we’ll need to tell DrRacket that we’re using one of the simplified “student languages” rather than the full industrial-strength version. Start DrRacket, go to the Language menu, and select “Choose Language...”. Then click on “Intermediate student language with lambda” and click OK:

![DrRacket Language Section](/assets/exercise_0/drracket_language.png)

You only need to do this once; DrRacket will remember the setting when you run it again in the future.

DrRacket's main interface consists of two main panes, which default to being in _Vertical Layout_ (on one top of the other). The top portion is the *Definitions Window* where you write your programs and execute them using the Run button at the top of the screen. The bottom portion is the *Interaction Window* where the results of running your program are displayed. You can also use the Interaction Window to run individual expressions in this window by typing them here and pressing the Enter key. Think of it like a playground where you can test small parts (usually one line or expression) of your program instead of the whole thing.

You can also set DrRacket to be in _Horizontal Layout_ where the Program Window and Interaction Windows are placed in a horizontal orientation. To switch orientations, click on the `View` menu and select `View Horizontal Layout`(you can switch back the same way). I'll usually use Horizontal Mode in class, but sometimes I waffle between the two. Do whatever works for you!

![Vertical Layout](/assets/tutorial_0/01a_vertical_layout.png) ![Horizontal Layout](/assets/tutorial_0/01b_horizontal_layout.png)

Go to the Program Window and add the following command to the beginning:

```racket
(require 2htdp/image)
```

This command tells Racket that you want to use a pre-existing software package, specifically the graphics library. Now hit the **Run** button. That will ask Racket to load the graphics library so you can use it both in the Program and Interaction windows. **Important**: Racket will only load this library if you click **Run**. If you don't hit **Run**, then Racket doesn't "checkout the book from the library," so to speak.

## Getting Started
In programming, you do most of your work by **calling** (or colloquially "running") **functions**. Just like in a math class, functions in computer science are basically machines that take in some input(s) and produce some output. In slightly different terms, functions are small programs written by someone else (and later by you) that take some **arguments** (inputs) and produce some output.

In order to use a function, we use a **function call**. In other words, we have to give the function its expected inputs and it will in turn give us its output. Racket uses a uniform notation for function calls:

```racket
(function inputs ...)
```

The name of the function you want to run comes first, followed by its arguments, separated by spaces, and the whole function call is wrapped up in a set of parentheses. Different functions require different numbers of and kinds of arguments. For example, the `rectangle` function requires two numbers (the width and height) followed by two `string`s (the drawing mode – either `"solid"` or `"outline"`, and the name of the color to use). So we can make a 50x50 blue rectangle by saying:

```racket
(rectangle 50 50 "solid" "blue")
```

Try running that program now in the Interaction Window (sometimes called the **REPL** which we'll talk about more later).

We call the `rectangle` function, passing it the inputs `50`, `50`, ``"solid"``, and ``"blue"``. Its output is another data object: the image of a blue square. Here's what that whole process looks like in a **data flow diagram**.

![Rectangle Function](/assets/tutorial_0/rectangle_function.png)

Try changing the arguments to different values and rerunning the expression. Change the width and height, the color, etc.

All the functions in Racket are documented in official Racket Documentation. You can access the documentation from Help > Racket Documentation or by right-clicking on any word in your program and selecting _Search in Help Desk for ..._.

* Look up the documentation on the `rectangle` function.
* See how it explains the each of the inputs.
* The funny notation, like `(and/c real? (not/c negative?))`, is short-hand for "a possibly fractional number that isn’t negative."
* You can click on `mode?` to see which other drawing modes are allowed.
* Now look up the documentation for the `ellipse` function. What's similar? What's different?

## Making Simple Pictures

> **Note:** In this class, it's generally a bad idea to copy and paste code from any source, including this document. For one, if you're copy and pasting, you don't get the sort of kinesthetic experience of programming. Secondly, many times when you copy and paste from documents and web pages, you accidentally copy and paste so-called "special" characters that _look_ like a character you know, but are slightly different for example: `-` and `–` look VERY similar...but are totally different characters. If you just copy and paste, you might end up with a mix of – and - which will cause lots of confusion down the road. So for all of the function calls we show below...please type them yourself! And thirdly, copy-and-pasting from sources not provided by the class may be a violation of our academic honesty policy.

### Activity 1: Simple Shapes

Let's make a series of simple images. For each element in the table below, use the relevant Racket functions to create the image you see (the lecture slides might be a useful resource here; as would the official Racket documentation):


| Description | Desired Image |
| ----------- | ------------- |
| A 100x100 solid green square|  ![A green square](/assets/tutorial_0/square.svg) |
| A blue circle with a diameter of 100 | ![A blue circle](/assets/tutorial_0/circle.svg) |

### Activity 2: Compound Shapes
Now let’s make compound images from simpler images. For example, what if you wanted to make a shape like the below:

<img alt="a-nested-circle" src="/assets/tutorial_0/a-nested-circle.svg" width="25%"/>

We can do this by first making two shapes and then joining them using a function called `overlay`. The <a href="https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._overlay%29%29" target="_blank">overlay function</a> takes images as arguments and outputs a new image that contains its inputted images stacked on one another (make sure to look it up in the reference materials!). Notice that this means we’re _composing_ or _chaining_ the calls to our shape functions with the call to `overlay`.

The outputs of the two shape functions are passed as inputs to `overlay`. The data flow we want looks like the below: two shape function calls that output each shape, followed by an overlay call that outputs the final image. This one is if you wanted to overlay two concentric (i.e. they share the same center point) circles.

<img alt="overlay function data flow" src="/assets/tutorial_0/overlay-example-final.svg" width="95%"/>

How do we chain calls in our code? By moving the function call into the place where we want it to be used as an input. The three calls we want, written as text, would look like this:

* `(overlay circle1 circle2)`, where `circle1` and `circle2` are the two circles we want to join...
  * `(circle 50 "solid" "red")`
  * `(circle 100 "solid" "blue")`

That means we just paste the second two function calls into the first one, replacing our place holders `circle1` and `circle2`:

`(overlay (circle 50 "solid" "red") (circle 100 "solid" "blue"))`

The problem is this is hard to read because you have to keep track of the parentheses to know what things are inputs to what. To make this more easily to see, we break the function call up into two lines and indent the second line so that the two inputs to `overlay` line up:

```racket
(overlay (circle 50 "solid" "red")
         (circle 100 "solid" "blue"))
```

Once you understand this indentation convention, you can see visually that this is two `circle` calls feeding their outputs to `overlay`, without having to figure out the parentheses. Some programmers break it up further:

```racket
(overlay (circle 50
                 "solid"
                 "red")
         (circle 100
                 "solid"
                 "blue"))
```

But this isn’t strictly necessary. The rules you want to follow are:
* Put the first argument on the same line as the function
* Put subsequent arguments on different lines if those arguments are calls themselves

Realize though that this is to make your code more _human-readable_, and will have no effect on how it gets run by Racket. That doesn't make it any less important (in fact, I'd argue it makes it MORE important if you ever want to share your code).

Alright, now that you've seen an example of using `overlay`, see if you can adapt it to make a compound image of our blue circle from earlier on top of our green square from earlier like the below:

<img alt="overlay activity" src="/assets/tutorial_0/overlay.svg" width="25%"/>

Of course `overlay` is just one function that makes compound images. Make sure to check out the other functions that do similar things like `above`[(documentation link)](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._above%29%29) and `beside`[(documentation link)](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._beside%29%29).

### Activity 3: Concentric Diamonds

Reproduce the shape below (don't worry about exactly matching sizes or colors):

<img alt="a-nested-diamond" src="/assets/tutorial_0/diamonds.svg" width="25%"/>

Before jumping in, try to break the problem down into smaller sub problems:
  1. Find a function in the [`2htdp/image` library]() that produces diamond shapes.
  2. Create a really small diamond.
  3. Figure out how to make 3 slightly larger diamonds of different colors.
  4. Finally, take all four of the diamonds and feed them as inputs to a function that allows you to combine images.

This approach is often called **sub-goaling** and is a particularly important programming practice. Don't try and approach a complex problem all at once. Instead, identify smaller parts you already know how to do and work towards combining them into the ultimate goal.

## Activity 4: Writing a Simple Procedure
Now write a simple procedure, `glasses`, that makes a picture of a pair of glasses. It takes two inputs, the first is the lens design (an `image`) and the second is the design for the bridge that joins the two lenses (also an `image`). It should output an `image` with two copies of the lens, joined by the bridge. So if you say:

```Racket
(glasses (circle 50 "outline" "black")
         (rectangle 20 2 "outline" "black"))
```

You should get something like this:

<!-- viewBox="0 0 221 101" -->

<img alt="glasses example" src="/assets/tutorial_0/glasses.svg" width="50%"/>

Remember, a lambda expression is one that allows you to specify something is a function _that needs to be evaluated with inputs_ rather than just a regular old piece of data. So for example, we can make a new function by saying:
<pre>
(lambda (lens bridge)
          <i>output-code</i>)
</pre>

Where output-code is whatever code you want to use to make the image of the glasses. The word `lambda` tells the system we’re making a new function, and the `(lens bridge)` part tells the system that the output code can refer to the inputs passed to the function by the names lens and bridge, respectively. When the function is called, the system will substitute the first argument passed to the function, whatever it may be, for the name `lens` wherever it appears in the _output-code_ and it will substitute the second argument in for the name `bridge`.

That’s how you _make_ a new function, but making the function doesn’t give it a name. You do that by wrapping it in a `define` expression.  So, your program should look something like this:

<pre>
(define glasses
   (lambda (lens bridge)
      <i>output-code</i>))
</pre>

Your job here is to decide what _output-code_ should be. It’s going to want to make two copies of the lens and place them around a copy of the bridge. Remember that you can insert whatever the user provided for the `lens` and `bridge` into your code just by saying `lens` and `bridge` respectively.

Feel free to change the names lens and bridge to something else if you prefer.

> **Hint**: You won't be able to use `overlay` here since you need to place shapes beside each other. See if there's another function in the `2htdp/image` library that might be useful for combining images in a different way!

### Designer Eyeware
Now have fun trying to make different interesting glasses designs, and share them with the other students in your group! The wackier the better!

## Getting Credit for Your Work
Since attendance for this tutorial is required, your PM will ask to confirm your name and NetID. You'll automatically receive full credit as long as your PM indicates you participated fully. In future weeks, if you choose not to attend and instead submit your work remotely, it will be graded for completion and, as such, you may not receive full credit.
