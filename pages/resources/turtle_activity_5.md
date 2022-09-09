---
layout: page
title: Turtle Activity 5
permalink: /resources/turtle-activity-5/
---
Now that you're experts, lets put your skills to some creative use. We have our delightful turtle buddy and some simple ways of controlling its movement. We also now know how to chain these movements (or commands) together into <strong>programs</strong>. But programs don't need to always be about completing some sort of task. What if we used our turtle buddy to create art? Computers are great at performing the same simple tasks repeatedly at amazing speed. Here, we're going to use a slightly modified version of our previous activity that focuses on drawing designs based off of your turtle buddy's movements.

There are a few main changes to this simulation:

1. We no longer have a checker-board grid. The patches are still there, they're just all the same color.

2. Your turtle buddy now has a pen with it and where it moves, the pen draws on the ground (you'll see what we mean). The color of the pen depends on the color of your turtle's shell.

3. The FORWARD, LEFT TURN, and RIGHT TURN command blocks are a little different than they were before. You'll notice each of them has now been <em>parameterized</em>. FORWARD now asks you how many steps that your turtle should step forward while LEFT TURN and RIGHT TURN ask you how many degrees to turn. Note that this is NOT the same thing as "changing the value of heading to right or left." Instead, it's turning at an angle. You'll see what we mean if you ask your turtle to LEFT TURN 90 degrees. If you were to ask it to LEFT TURN 90 four times....that's the same thing as LEFT TURNING 360...which is the same thing as not turning at all!

4. Oh, and we removed the lava...and the violet goal patch. Now you're free to draw to your heart's content!

5. TO MOVE, has been replaced by TO DRAW. Unlike TO MOVE that only gets executed by your turtle buddy once, TO DRAW will be repeatedly executed by your turtle until you click the DRAW button once more to stop it.

<You control the movement of your turtle just as before, by connecting movement blocks to each other and then, once satisfied, hitting the DRAW button (note: remember, once you've clicked the DRAW button, your turtle will keep following your program until you click DRAW again).

<iframe frameborder="0" height="650" scrolling="no" src="https://ct-stem.s3.amazonaws.com/uploads/bainco/2020/07/01/drawing_turtle_2020-07-01-11-13-32-984014.html" width="1200">Something has gone wrong. Please reload the page.</iframe>

## Activities
Write a program that results in your turtle drawing an <em>equilateral</em> triangle (equal angles and equal side
lengths). Explain how your program works.

Now modify your program so that it draws a square. Explain your modifications.</p>

Now try modifying your program to draw a regular (all sides are equal length) pentagon. Is there a pattern between your solutions for a regular triangle, regular square, and regular pentagon? Explain.

This process of looking for patterns among tasks is very common in programming. In this case, we might be able to <strong>abstract</strong> our programs into a single program that can draw <em>any</em> regular polygon with a given number of sides <em>n</em>.

Say you want to draw a regular polygon with <em>n</em> sides. While we know we'll need to have at least <em>n</em> forward commands, how might we express the turn angle of our turtle in terms of <em>n</em>?

(Hint: Think back to your geometry days. Maybe the term <em>interior angle</em> reminds you of something?)

Spend some time experimenting. See what sort of designs you can have your turtle buddy draw! If you're not feeling particularly creative, try recreating this program:

<img height="456" src="https://ct-stem.s3.amazonaws.com/uploads/bainco/2020/07/01/screen%20shot%202020-07-01%20at%2012.04.25%20pm_2020-07-01-11-48-33-232370.png" width="210">
