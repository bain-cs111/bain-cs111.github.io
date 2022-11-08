---
layout: assignment-two-column
title: Building "Snake" the Video Game
abbreviation: Exercise 6
type: homework
due_date: 2022-11-14
ordering: 7
draft: 0
points: 100
canvas_id: 1143472
---

This exercise will be all about creating Snake the video game. If you've never played before, I recommend checking out [this browser-based version](https://www.mikusa.com/snake/snake.html) before starting the assignment.

> **Note:** There are a few differences between this version and ours:
> * Our snake will start off with a single segment rather than a bunch.
> * We'll have multiple food morsels on the board at the start.
> * We'll have obstacles on the board that also pose a danger to the snake.

The purpose of this assignment is:

1. have a summative assignment of nearly all the topics we've covered in the first 7 weeks of class
2. get you used to working on a large "existing code base" where your job is to only implement a few parts of a larger project.

This is an authentic example of what you'll be doing if you continue on in CS past our course. It's incredibly rare to work on something just totally by yourself. And even if you're working by yourself, you usually rely on outside libraries and other people's programs as the basis of your larger programs.

Your finished Snake program will look like this:

<img src="/assets/exercise_6/finished_snake.png" alt="Screenshot of finished game" style="float: none; width:75%;"/>

## What is Snake?

In this game, you control a snake that wanders around a grid (square) board, eating yellow food morsels and avoiding purple obstacles. Each time the snake eats a morsel of food, it gets 1 unit longer. If the snake ever runs into itself, runs into the edge of the board or runs into a purple obstacle, the game is over. The snake never stops moving; the player can control only the direction that the snake moves by using the arrow keys.

### Some Logistical stuff

This assignment DOES NOT use imperative programming. Starting on Monday, November 7th, we'll be talking about imperative programming in class soon but this assignment DOES NOT use imperative programming. This means that in class we will be using the `Advanced Student Language` for imperative programming but as this assignment is purely functional (no imperative programming) we want to use continue using `Intermediate Student with Lambda`. You can always check what language you're using in DrRacket by looking at the bottom left hand corner of the main window.

<img src="/assets/exercise_6/drracket_language.png" alt="Screenshot of DrRacket language chooser" style="float: none; width:50%;" />

### Computer Coordinate Systems
Computers tend to use an odd coordinate system where the top left corner has coordinates `(0, 0)` and the y-coordinate increases in value as you move down. That is, the bottom left corner is at coordinates `(0, SIZE_OF_COORDINATE_SYSTEM)`. For this assignment the provided `snake-lib.rkt` file corrects for this behavior, moving the `(0, 0)` coordinate to the bottom left like coordinate systems in math. This means when the snake is moving up you’ll want to increase the y- coordinate and decrease it when the snake is moving down.

Many moons ago we talked about how Racket has some built-in structs that you can take advantage of without having to create them yourself with `define-struct`. One of them is called `posn` which is short for "position."

A `posn` is made up of two properties:
* an `x` value (number)
* a `y` value (number)

Just like our custom `struct`s, this means we have the following functions to create, check, and access data in structs:

* `make-posn: number number -> posn`
* `posn?: any? -> boolean`
* `posn-x: posn -> number`
* `posn-y: posn -> number`

## How to Get Started
To begin, download the template files below and make sure that all the files `snake-lib.rkt`, `foreign.rkt`, and `snake.rkt` are in a folder where you will do your work.

<a class="nu-button" href="/course-files/exercises/exercise_6_template.zip" target="_blank">
    Exercise 6 Starter Files <i class="fas fa-download"></i>
</a>

**DO NOT MAKE ANY CHANGES TO THE FIRST 2 FILES AT ANY POINT.**

You will do your work in `snake.rkt`. Make sure your code passes all provided `check-expects` and write additional `check-expects` to ensure you’ve covered all possible situations. Additionally, you can only use the libraries already required in the starter code, or `"./iterated-images.rkt"` and `"./remove_duplicates.rkt"` (however neither of these are actually required to complete the assignment).

 Also, `snake.rkt` **MUST START WITH** `(require "snake_lib.rkt")`; don't add any code before this line.

## Data Definitions

Just like how we've represented different types of data like employees, humans, cars, etc using custom `struct`s, so too can we represent the game of snake using a collection of `struct`s.

For this assignment, we will use several different data definitions (already included in `snake-lib.rkt`; you do not need to make these, you'll just need to use them). For our game, we'll need definitions for:

* `game`: this keeps track of all the game data including the board
* `snake`: this represents the snake character
* `food`: this will be used to represent the food particles on the board
* `obstacles`: this will be used to represent the obstacles on the board

These definitions are provided in your code _as a reference_ but the `define-struct` lines must remain commented as they are defined in already in `snake-lib.rkt` **for you**. They are also listed below.

This `require` line also provides two other definitions:

* `board-length`, the length of one side of the board (measured in terms of snake body segments – by default, this is set to 50)
* x-coordinates increase from 1 to `board-length` (inclusive) toward the right.
* y-coordinates increase from 1 to `board-length` (inclusive) toward the top.
* `play-game`, a function described near the end of this assignment.

### The `game`
```racket
; a game is...
; - (make-game snake (listof posn) (listof posn) number)
;   aka: (define-struct game (snake food obstacles ticks))
```

* the `snake` is described in more detail below (where is the snake on the screen)
* the `food` is described in more detail below (where is the food located on the screen)
* the `obstacles` are described in more detail below (where are the obstacles on the screen)
* the `ticks` just keeps track of how many "turns" have been played (aka how many times has the snake moved)

> **Interlude on Symbols**
> `'down` and its counterparts might look a little weird. It's not a string...and it's not a list. What the heck is it? The single quote (i.e. `'`) when _not_ paired with a parenthesis (e.g. `'(1 2)`) means: "the following is a **symbol**". `symbol`s are another type of data in Racket that you can think of as "strings without any spaces." Racket knows the symbol ends as soon as it sees a space.
>
> That is `'up` is a **symbol** and `"up"` is a **string**. If you see something that starts with a single quote, it's a symbol. To test to see if something is a symbol, you can use the built-in function `symbol?`. To test if two symbols are equal, you can use: `symbol=?`

### The `snake`

```racket
; a snake is...
; - (make-snake direction (listof posn))
;   aka: (define-struct snake (heading segments))
```

So a `snake` is made up:
* a `heading` (`'up`, `'left`, `'down`, or `'right`)
* and a list of `segments` (more below)

#### Snake `segments`

```racket
; segments is either
; - (cons posn empty)
; - (cons posn segments)
```

That is, `segments` is a non-empty list of `posn`s.

### `food`
```racket
; food is either
; - empty
; - (cons posn food)
```

This is basically just a list of `posn`s. If a `(x,y)` point is in this list, that means we'll see it on the board as a yellow food particle at that coordinate point.

### `obstacle-squares` (a variable)
```racket
; obstacle-squares is either
; - empty
; - (cons posn obstacle-squares)
```
This is basically just a list of `posns`. If a `(x,y)` point is in this list, that means we'll see it on the board as a purple obstacle at that coordinate point.

## Your Job

Your job is to write the following functions (and any necessary helper functions), which will serve as arguments to the `play-game` function. We’ve written about 1 test for each function. You’ll need to write more to properly test your code. I recommend that you write these functions in the order that they appear here (and in the starter code file). You will not be able to play the game until all functions are completed, but you can properly test your functions with check-expects.

Your work is limited to _defining_ these functions. This is an example of _abstraction_ in software engineering. For instance, you don't have to worry about teaching racket how to listen to the user's keyboard entries or how to launch a popup window for the game. Those things are taken care of in `snake-lib.rkt`. You can just take for granted that, for instance, your `change-direction function` (described below) gets the correct direction from the keyboard.

### Activity 1. `add-food`
```racket
add-food: game posn -> game
```
This function adds a morsel of food at the specified board position.

### Activity 2. `change-direction`
```racket
change-direction: game direction -> game
```
This function changes the direction in which the snake is traveling.

### Activity 3. `game-score`
```racket
game-score: game -> number
```
This function computes the player’s score, based on the snake’s `length` and the time (`ticks`) taken to reach that length. One possibility is to multiply the snake’s length by 100 and subtract the elapsed ticks, but you’re welcome to experiment with other definitions. There's no one correct score to use, but it needs to be numeric and it needs to stay positive.

### Activity 4. `game-over?`
```racket
game-over?: game -> boolean
```

The game ends when the snake runs into:
1. Itself - the head is the same as one of the other segments. A snake of length 1 or 2 (of 1 or 2 segments) can reverse direction without running into itself. Once a snake is 3 or more segments, reversing direction will cause it to run into itself and the game should be over. You cannot have an empty snake.
2. A wall - if the x and y coordinates of all of the snakes segments fall between 1 and 50 (board-length) inclusive, this means that the snake has NOT hit a wall. If the x or y coordinate of any of the segments becomes 0 or 51, then the snake has hit a wall. Please be very careful when checking the bounds here.
3. Or an `obstacle` (purple dot) - see if any of the segments have collided with any of the obstacles.

Note that a "dead snake" (aka, game over) is exemplified by the snake turning red on the game board. (The `snake-lib` also takes care of this for you)

> **Hint**: you might find it useful to write helper functions. One for each condition of game over.

_When your snake runs off the top or left side of the board, you’ll see a weird shift of the game board and you’ll be able to see the segment of the snake that is off the side of the board. When it runs off the bottom or right, this will not happen. This is expected behavior and you don't need to worry about it._

### Activity 5. `advance-game`
```racket
advance-game: game -> game
```
This is the most challenging part of the assignment.

This function moves the game forward one step. _One step increments the game’s `tick` component and moves the snake, possibly causing it to eat and grow._

_Moving_ the snake means that the snake both gains and loses a segment (unless it eats). The new segment’s coordinates are determined by the segment that was previously at the front of the snake and the direction the snake is heading. If the snake does not eat, it loses the oldest segment, namely the one that was previously at the end of the snake. To see if the snake has eaten, you should check to see if adding a new segment in the direction of the header collides with a piece of food.

> **Hint**: To remove an element from the end of a list, think about how you can use the rest and reverse functions (you can find reverse in the [Racket Documentation](https://docs.racket-lang.org/htdp-langs/intermediate-lam.html#%28def._htdp-intermediate-lambda._%28%28lib._lang%2Fhtdp-intermediate-lambda..rkt%29._reverse%29%29)).

This function **does not** replace eaten food; `play-game` (in `snake-lib.rkt`) handles that task. You **do need to handle removing the eaten food**, however.

Below are two examples of the behavior of this function for the snake before and after a single step. The first scenario shows the snake moving upwards without eating. The second is the same scenario except the snake eats a piece of food as it moves upward.

**Without food - Snake moving up**
<img src="/assets/exercise_6/snake_first_screenshot.png" alt="Snake screenshot of not eating food" style="float: none; width:50%;"/>

**With food - Snake moving up**
<img src="/assets/exercise_6/snake_second_screenshot.png" alt="Snake screenshot of eating food" style="float: none; width:50%;"/>

> **Hint**: Remember, in functional programming we can't update the value of a variable. If we want to "update" a game, we have to create a whole new game and update its properties using the values from the old game. So for instance if we wanted to make a new game using some old game `g`...and all we wanted was to update the `g`'s tick counter, we would have to use:
>```racket
> (make-game (game-snake g)
>            (game-food g)
>            (game-obstacles g)
>            (+ 1 (game-ticks g))
>```

### Finally Testing Your Work

After you have written and thoroughly tested these functions (you can fully test these with `check-expect`s before playing the game at all), you’ll notice that we have the below line commented out at the end of the file. Uncomment it to start playing.
```racket
(play game-start)
```

> **Note**: As you work on this assignment, you may consider what would happen if a piece of food happens to appear in the same position as an obstacle. Please ignore this issue. That is, don’t worry about testing for this situation. A new piece of food will only appear at a currently “open” location (i.e. one that does not contain a piece of food, an obstacle, or part of the snake). This check is already implemented in `snake_lib.rkt`.

* * *

## Turning It In

Before turning your assignment in, **run the file one last time** to make sure that it runs properly and doesn’t generate any exceptions, and all the tests pass. Make sure you've also spent some time writing your OWN `check-expect` calls to test your code.

Then, make sure to read the [Autograder Guide](https://canvas.northwestern.edu/courses/178849/pages/whats-an-autograder) one last time. Not only is it a useful check of your work, but it will also tell you which file you should submit.

Assuming they do, submit **only your `snake.rkt` file** on Canvas. 

* * *

## Requesting an extension
If you need to request an extension on this assignment use the <a href="https://forms.gle/fWx9jgQTNp56bAgR6">Extension Request form</a>. Please see this Syllabus for requirements. Your extension is automatically accepted if you meet the conditions. You will see your due date on Canvas update 24 hours prior to the original deadline.
