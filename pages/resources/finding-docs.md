---
layout: default
title: Finding Documentation for Functions
permalink: /resources/docs/
---

## Finding Documentation for Built-in Functions
There are LOTS of ways to get more information about what sorts of inputs a function requires and what sort of output it creates. When using the base _Intermediate Student Language_, the easiest ways of accessing this sort of info is one of the following:
1. Manually going to the whole documentation for all of the ISL [which is hosted here](https://docs.racket-lang.org/htdp-langs/intermediate-lam.html)
2. Type a function name in DrRacket and then right click on it and select "Search in Help Desk for..." and then when your browser pops up click the function name
3. Or you can usually just google: `racket intermediate student language string-append function`. This usually just brings you to the first page...and then you need to search again.

**Now there are some caveats to these.** Not all of the functions we use in this class belong to the "base" (the core part) of the Intermediate Student Language. That's why, for instance, when doing the graphics work we've had to add the `(require 2htdp/image)` line which loads in an external library of functions.

## Finding Documentation for Built-in External Libraries

In the case of these functions, the ways of getting help are _similar_, it's just that they're technically separate from the base language:
1. Type a function name in DrRacket and then right click on it and select "Search in Help Desk for..." and then when your browser pops up click the function name
2. Google the name of the library (e.g. `2htpd/image`) and [find the link to the documentation](https://docs.racket-lang.org/teachpack/2htdpimage.html)


## Finding Documentation for Custom External Libraries

Starting in Week 3, we'll also be giving you _completely custom function libraries_ for which documentation does not exist on the web. For these (e.g. `iterated-images.rkt` for Exercise 2 and Tutorial1), you'll need to actually open up the `iterated-images.rkt` file and look at the various type signatures and purpose statements in the code (**this is why writing those type signatures and purpose statements is so important as we start to write our own functions**).


## Using the `help` function

Finally, [there is one additional way](https://docs.racket-lang.org/reference/Interactive_Help.html) to find help on functions that those of you who have used Python before might find familiar.

You can load the `help` library by executing `(require racket/help)` (I recommend doing this in your Definitions Window and then hitting run).

You can then ask for help for any function that has documentation associated with it by using the help function:

```racket
;; help: function -> opens a web browser
;; example: (help +)
```

Note, this is an example of a **higher-order function**! It takes as an input a _function_ and then "returns" the associated documentation (by opening it on your computer's default web browser).
