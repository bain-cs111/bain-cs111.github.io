---
layout: assignment-two-column
title: Setting Up and Signing Up
abbreviation: Exercise 0
type: homework
due_date: 2022-09-23
ordering: 1
draft: 0
points: 100
---

This assignment is super easy and super short. We'll basically complete the whole thing in class on Friday.

> **Note**: this is the only assignment that will be due on a Friday at midnight. In general, assignments will be due on Mondays at 1pm.

* * *

## Activity 0 - Signing Up for a Tutorial Team

A key part of this class is your Tutorial Team (up to 8 people; no exceptions) with whom you'll work each week to complete in-class exercises. Think of it as a smaller community within the larger course community.

> **Note**: There are two sections of this course this quarter. If you'd like to be in a Team with people from the other section, that's fine as long as you can plan on attending that class time on a consistent basis. **However, when we have our Quizzes you will be expected to take that quiz during the course time you are registered for.**

There are several ways of finding a Tutorial Team in our class:

1. **EVERYONE** is expected to fill out our Tutorial Team survey (linked below) regardless of whether or not you have Teammates in mind already.
2. If you already have teammates in mind, go ahead and sign up for your group in Canvas. [Instructions are located here](). We may add students to your pre-formed team depending on the survey results.
3. If you'd like to find teammates on your own, you can do so by posting in the `#imaginary-student-groups` chat room on Campuswire (if you haven't signed up for Campuswire, make sure to do so via the registration link on Canvas). If you find some people with similar interests, go ahead and sign up for the team on Canvas using the instructions above.
4. If you'd like to be assigned a Team, just fill out the Team Survey and we'll assign you a Team on Sunday evening.

> **Note**: If you have not signed up for a Team on Canvas by Sunday evening at 5pm, we will use your Survey results to assign you to a team. You will not be able to self-register for teams after 5pm on Sunday.

Link to [Teams Survey](https://forms.gle/TNqpEJutHby4t4RS6) (note, you must be logged into your NU Gmail account in order to access).

While we'd like for the Teams to be as stable as possible throughout the quarter, stuff does happen so switching will be allowed after Week 2.

* * *
## Activity 1: Getting DrRacket

Our first and most important task is to get DrRacket (and the Racket programming language) installed on your computer.

### Download the Installer

1. Go to the [Racket download page](https://download.racket-lang.org/).
2. Pick the installer that corresponds to your computer's OS.

> **Note**: You can select either 32-bit or 64-bit, but if your computer is less than 4 years old, you should go ahead and pick 64-bit. What does this mean you might ask? You'll learn more about this in future CS coursework, but the basic idea is that computers have memory (called _RAM_) where they store temporarily store information as they run applications. Programs are assigned specific "blocks" of memory using addresses, just you were assigned a specific room in a residence hall on campus. 32-Bit programs can use up to 2^32 different memory addresses (around 4GB of RAM) while 64-bit programs can use up to 2^64 different memory addresses (17,179,869,184 GB (16 exabytes) of RAM).

> **Note**: If you're on a Mac laptop that was made anytime after November 2020, you should select the "Apple Silicon" version. However, the "Intel" version will work on your computer regardless.

### Install DrRacket + Racket

Once you've downloaded the installer, you'll need to actually run it. On a Mac, you'll drag the `Racket` folder into your `Applications` folder. On a Windows PC, you'll run the `.exe` file to install it.

### Optional Install Method for Mac/Linux

If you're a Mac or Linux user, you can also install the latest version of Racket (v8.6 as of September 2022) via the command line:

On a Mac with [Homebrew](https://brew.sh) installed:
```bash
# On a Mac with Homebrew installed
brew install --cask racket
```

On a Linux system with `apt-get` configured:
```bash
apt-get install racket
```

* * *

## Activity 2: Choosing a Language

After installation, you should be able to double click on the DrRacket executable/application ![DrRacket Logo](/assets/exercise_0/racket_app.png) to start DrRacket. On a Windows PC, it should be in your Start menu (or whatever they're calling it these days). On a Mac, it'll be in your Applications folder.

That should launch a window that looks something like this:
<img alt="DrRacket Window" src="/assets/exercise_0/drracket_main.png" style="scale:50%;"/>
One of the main reasons we use Racket in this class is that it is essentially a programming language for programming languages. Rather than being a single programming language (e.g. Python), it allows you to implement your own languages. In this class, we'll actually be gradually progressing from one language to another language as we practice our programming skills.

In the bottom left hand corner, you should see a message that says "No language chosen" (you might also see something else here, that's okay). If you click on that menu you'll be brought to the Language Select screen (see below). On this screen, select the `Intermediate Student Language with lambda` and hit OK.

<img alt="DrRacket Language Select" src="/assets/exercise_0/drracket_language.png" style="scale:50%"/>

Once you've selected the language, hit the ![Run Button](/assets/exercise_0/drracket_run.png) button at the top of the Window which asks Racket to load the selected language. If everything went well, near the bottom of the window you'll see a line that says `Language: Intermediate Student with lambda; memory limit: 128 MB `.

* * *

## Activity 3: Running a Program

One of the core tenants of this class is that **there is no such thing as a little program.** All programs, regardless of their size/length, are powerful in that you have successfully translated an idea from your head to a form that the computer understands.

So let's write a **powerful** program! In the bottom half of the window (also called the _Interaction Window_) type the following and hit Enter:

```racket
(+ 2 3)
```

<img alt="Our First Program" src="/assets/exercise_0/drracket_calc.png" style="scale:50%"/>

If you see a result of `5`, congratulations! You've officially run your first program.

* * *

## Activity 4: Saving your Program

However, because you wrote it in the _Interaction Window_, DrRacket hasn't stored your program in any file on your computer. To do that, we need to use the Definitions Window_, the top half of the DrRacket app. Type in `(+ 2 3)` again, but this time up top in the _Definitions Window_. Then click the `Run` button ![Run Button](/assets/exercise_0/drracket_run.png)

<img alt="Our First Definitions" src="/assets/exercise_0/drracket_definitions.png" style="scale:50%"/>

You should see the same calculated result now at the bottom in the _Interactions Window_. In general, when writing programs in our class, we'll follow this same workflow:

1. Try out some code in the _Interaction Window_
2. Finalize the program in the _Definitions Window_
3. **Make sure it does what you think it does by hitting the `Run` button**

> **Note**: _Never_ turn in code that you have not tried to `Run`! Anytime you add something to your program, you should try running it. Programming is a process of translating what you have in your brain into a form the computer understands. You should be _constantly_ checking to see if that translation process is proceeding as you think it is.

Okay, now we've got a program but we haven't actually saved it on our computer. To do that, go to the `File` menu and select `Save Definitions as...`. This will bring up a prompt on your operating system to save your Racket program (just like you save Microsoft Word documents). We **highly** recommend setting up a single folder somewhere on your computer (i.e. your Desktop) that will store all of your work for this class.

I like commonsense file names, so go ahead and name your file `exercise_0.rkt`.

Once you hit save, you should be able to go to that folder on your computer and see your new document (it will have the `.rkt` file extension).

* * *

## Activity 5: Writing a Definition

While `(+ 2 3)` is a pretty dope program, sometimes we need to teach the computer to remember a particular piece of data to be used later in our programs. We can teach the computer to remember pieces of data using a `define` _special form_. Add to your program in the Definitions Window a new line that looks like the following:

```racket
(define netid "abc1234")
```

**Make sure to replace the thing in quotation marks with your actual NetID***. This tells Racket that from now on, when you give it the _symbol_ `netid` it will use the value `"abc1234"`.

>**Note**: Those quotation marks `"` are important! They tell Racket that this thing inside the quotation marks isn't just a symbol, it's a piece of data called a string (that's why DrRacket makes it green).

Make sure to run your program which should look like the below:

<img alt="Final Version of Our First Program" src="/assets/exercise_0/drracket_ex0.png" style="scale:50%"/>

Notice that you won't see your NetID outputted in the _Interactions Window_. That's because you didn't actually ask Racket to do anything with your NetID–you just asked it to remember it. That's okay for now. We just want to have it store the NetID so that when we run your program, we can tell your program apart from your classmate's.

Make sure to save your modified program using the `File` menu and selecting `Save Definitions` (you don't have to pick `Save Definition as...` since that allows you to create a brand new file and we just want to update the one we already have).

Congratulations, you're officially a _Racketeer_!

> **Life Advice**: Probably best not to use this term outside of course community as it has some nasty legal implications.

* * *

## Turning it in

All of the Exercises in this class will be graded via an autograder – a program, written in Racket, that will run your program and test it to see if it meets all the expectations of the assignment.

This means that you must _carefully_ read each assignment description and follow it exactly. If your assignment does not satisfy the requirements in any way (even if you think it's small and inconsequential) you will receive points off (or in the worst case, not receive any points at all). For this assignment, because the program is so short, there's only a few things you should check:

1. Does your program run without any errors?
2. Are you outputting the result of some math calculation?
3. Are you storing your actual netid inside the `define` statement?
  * Important: Racket **is** case-sensitive (i.e. capitals matter) so make sure that your define statement actually says `(define netid ...)` where `netid` has no spaces and no capitals.
  * Make sure your netid you enter is actually your netid. The autograder will compare the submitter's netid to the one in your program.

For this assignment, you will upload your `exercise_0.rkt` file to the assignment on Canvas. DO NOT UPLOAD ANY OTHER FILES. Once you've submitted your file to Canvas, you're done!
