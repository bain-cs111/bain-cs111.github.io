---
layout: assignment-two-column
title: Recursion on Lists and Trees
type: tutorial
abbreviation: Tutorial 5
draft: 0
points: 100
num: 6
description:
canvas_id: 1141822
due_date: 2022-10-26
---
In this tutorial, we’ll practice working with recursion on linked structures such as lists and trees.

<a class="nu-button" href="/course-files/tutorials/tutorial_5_template.zip" target="_blank">
    Tutorial 5 Starter Files <i class="fas fa-download"></i>
</a>

## Problem 1 - `drop`
Write a function `(drop a-list k)` that returns a copy of list with the first `k` elements removed. In other words, if we ran

```racket
(drop '("a" "b" "c") 2)
```
we'd get back the one element list `'("c").`

Note that you won’t need to use `cons` for this, just `rest` as we're removing elements from a list not building a new one.

Here's some help to get you started:
* First just state the base case and recursive case in English. It’s not a bad idea to write it down, but that isn’t required. In any case, don’t go straight to writing code.
* Now that you’re clear on what the base and recursive cases should be, write the code. You’ll want to use an `if` to distinguish between the base case and the recursive cases.

## Problem 2 - `take`
Now write a function `(take list k)` that returns a list of just the first `k` elements of the inputted list. In other words,

```racket
(take '("a" "b" "c") 2)
```
should return the two element list `'("a" "b")`. Note that you’ll need to use both `rest` (to remove elements) and `cons` (to build a new list) for this. Use the same method as above.

* * *

## Inductive data structures
We talked in the linked list lecture about how lists can be built from simpler structures called pairs. Each pair has a single element and a link to the next pair, which has a single element and a link to the pair after that, ..., and so on until eventually the last pair lists the next pair as the `empty` list, usually notated as `'()`. This allows us to use a very succinct definition for a list:

>**Definition**: A `list` is either:
> * `'()`  or
> * `(cons e a-list)` where `e` is an element of `a-list` and `a-list` is some `list`.

This is called an **inductive definition**, or sometimes a **recursive definition**. Based on this definition, we know that lists include:

* `'()`, by the definition
* `(cons 1 '())`, since `(cons element list)` is a `list` and `'()` is a `list`
* `(cons 2 (cons 1 '()))`, by the same reasoning
* `(cons 3 (cons 2 (cons 1 '())))`, again by the same reasoning

And so on. As we discussed, when we type `(list 3 2 1)` or `'(3 2 1)`, we’re really just getting `(cons 3 (cons 2 (cons 1 '())))`. The quote notation is just a convenient shorthand.

* * *

## Trees
Now let’s talk about a different inductively defined data structure called a **tree**. Trees are used to represent some kind of hierarchy and are best introduced by an example. When we list a taxonomy of something:

<img alt="Taxonomy Tree" src="/assets/tutorial_5/taxonomy.svg" style="scale:66%; float:none;"/>

We structure it as a set of items, like "living thing," "plant," "animal", etc., together with lines indicating what is a "subclass" of what. This kind of branching structure is called a **tree**. The different items are called **nodes** of the tree and their connections or **branches** group them into a hierarchy. The top of the hierarchy ("living thing") is called the **root**, and the things beneath it and connected to it ("plant" and "animal") are called its **children**. They, in turn, can have children (and so on and so on). In this case, "plant" has the children "herbaceous" and "woody". At the bottom of the hierarchy are items that don’t have any children, which are called **leaves**. The leaves here are "herbaceous", "word", "fish", "lizard", "bird", and "mammal".

Tree are absolutely ubiquitous in computing and are a powerful example of an inductive or **recursive** data structure. The folders on your hard drive form a tree, for example. Because trees are defined recursively, many of the functions we use to work with trees are themselves recursive. This is one of many reasons why recursion is a powerful programming tool to have in your toolkit.

Of course, as with anything, there are different flavors of trees. One such important type of tree is a **binary tree**. That simply means _a tree in which nodes aren’t allowed to have more than two children_. So when the tree splits, it never splits more than two ways. Here’s a set of numbers arranged in a binary tree:

<img alt="Example Binary Tree" src="/assets/tutorial_5/bst.svg" style="scale:66%; float:none;"/>

In Racket, we can easily define a binary tree of numbers inductively, the same way we defined lists inductively. First, we need to define a `struct` to represent nodes that branch:

```racket
(define-struct branch (number left right))
```

This says that a branch object has a `number` and a `left` and `right` child. Of course, some nodes are just numbers. You make a branch object by saying `(make-branch number left right)` where `number` is the number you want to store in the node and `left` and `right` are its children.

Given that, we can inductively define a binary tree of numbers as follows:
> **Definition:** A binary-tree is either:
> * a number, or
> * `(make-branch number binary-tree binary-tree)`

Given this definition, we can say the following are all valid binary trees:
* `6`, since a tree can be just a number
*	`7`, same reason
* `5`, same reason
*	`(make-branch 4 6 7)`, since `6` and `7` are trees and `(make-branch number tree tree)` is a tree
* `(make-branch 2 (make-branch 4 6 7) 5)`, by the same reasoning, since `(make-branch 4 6 7)` is a tree and so is 5.

> **Note**: Having trouble visualizing what these programs are actually trying to encode? Checkout the [Lecture 14 slides](https://docs.google.com/presentation/d/1x4XjJ28pT0WP3STvJp7wk07T7kBLauLlTqz_sbC90ao/edit#slide=id.g172794ed368_1_634) for some _similar_ but not identical examples.

This definition also gives us a way to write recursive functions on binary trees. Since we know a tree is always either a `number` or a `branch` (that contains further trees inside it), we can have our base case be when the tree is just a single number (which we can test using the `number?` predicate) and our recursive case be when it’s a `branch` (in which case we recurse on the `left` and `right` children).

* * *

## Problem 3 - Making a Tree in Racket
The numerical binary tree above would look like this in Racket:

```racket
(make-branch 1
             (make-branch 2
                          (make-branch 4 6 7)
                          5)
             (make-branch 3 8 9))
```

This says that `1` is the root of the tree, but with two branches. Its branches are `2` and `3`, both of which also have branches. `2` branches into `4` and `5`, where `4` branches into `6` and `7`, but `5` doesn’t branch (it’s a leaf).  And `3` branches into `8` and `9` that are both leaves.

Make sure to define this tree in your `.rkt` file so you can use it as a complex test structure for the next 2 problems.

* * *

## Problem 4 - `count-tree`
Write a recursive function, `count-tree`, that counts the number of numbers in a binary tree.

```racket
count-tree : BinaryTree -> Number
```

If a number appears twice, count both occurrences. That is,

```racket
(count-tree (make-branch 1
                         (make-branch 2 0 0)
                         (make-branch 3 0 4)))
```

should return 7, in spite of 0 appearing three times.

However, `(count-tree 0)` should just return 1 (since the only number in that tree is `0`).

Here's some subgoals to help you get started:
* First, get clear in spoken language, what will the base case will be. Is it when the tree is a number, or when it’s a branch?
* Now get clear on what the recursive case will be. Again, don’t worry about code.
> Hint: this is a tree, so it’s probably tree recursion, right? So we’re probably calling ourselves twice and combining the answers!

* Now write the code. Use your friend `if` to decide if it’s a base case or recursive case.

## Problem 5 - `sum-tree`
Write a recursive function, `sum-tree`, that sums all the numbers in a binary tree.

```racket
sum-tree : BinaryTree -> number
```
For instance,

```racket
(sum-tree (make-branch 1
                       (make-branch 2 0 0)
                       (make-branch 3 0 4)))
```

should return 10. But `(sum-tree 3)` should just return 3. Again, here's some steps to get you started:
* What will the base case will be. Is it when the tree is a number or when it’s a branch?
* Now get clear on what the recursive case will be.
* Now write the code. Use `if` to decide if it’s a base case or recursive case.

* * *

## Getting Credit for Your Work
If you're in class make sure to check-in with your PM with your name and NetID. You don't need to submit a `rkt` file. Your attendance will be posted on Canvas by around 5pm today.

If you're submitting remotely, you MUST submit your completed tutorial to Canvas and it will be graded for completion.

Before turning your assignment in, **run the file one last time** to make sure that it runs properly and doesn’t generate any exceptions, and all the tests pass.

Then, make sure to read the [Autograder Guide](https://canvas.northwestern.edu/courses/178849/pages/whats-an-autograder) one last time. Not only is it a useful check of your work, but it will also tell you which file you should submit.

> Note: For groups that may have a significant amount of programming experience, your PM may suggest trying out the <a target="blank" href="/assignments/adv-tutorial-5">Advanced Tutorial 5</a>
