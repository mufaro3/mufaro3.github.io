---
layout: post
title:	"learning common lisp through sheer stupidity"
date:	2024-08-16 12:32:35
categories:
    - programming
tags:
    - common-lisp
    - in-progress
---

I have no idea how to program in Common Lisp, so I decided to learn for a number of reasons, the main of which being the potential to expand my methodology in respect to writing effective computer programs. *And let me just say, this has not been an easy process.*

I started with the basics: looking up simple youtube videos and StackOverFlow threads on what the most efficient means of learning Lisp would be. Simple enough, right? WRONG! I couldn't figure out how to get the Steel Bank Common Lisp compiler to work correctly (alongside additional tools like QuickLisp), and after slightly more than an hour of attempting to fix my installation and wrap my head around how SBCL actually works, I eventually just gave up and installed GNU CLISP, which, FYI, worked like a complete charm.

From there, I just did the standard "Hello, World!" program.

{% highlight common_lisp %}
(format t "Hello, World!")
{% endhighlight %}

And it seemed simple enough, but my peril with the language would only go downhill from this point onwards, as simply playing around with the absolute core basics without attempting to do any hacking would do me absolutely no good. Therefore, I immediately opted to try learning to write in Lisp via solving the easiest possible problem out of a year of [Advent of Code](adventofcode.com).

I started by picking the [Day 1 problem from last year](https://adventofcode.com/2023/day/1), a problem that I've solved in as little as four lines in Java, so I was certain that there was no way it could be too overly complicated in Lisp. I was terribly, terribly wrong.

I first had to begin by deciding on the structure of my program, and I opted to go for the kind of strictly-scoped layout that I tend to use for languages like Python or Javascript that allow for code to be run outside of functions in the global scope, which looks somewhat like the following:

```
function SOLVE-PROBLEM()
  include all core functionality of solving the problem here
end function

function RUN-TESTS()
  include all functionality for running tests of individual functions here
end function

read argument
if argument is TEST-MODE:
  call RUN-TESTS()
else
  call SOLVE-PROBLEM()
```

From there, I started on the main method of my Lisp program:

{% highlight common_lisp %}
; This function serves as the key solving algorithm to the
; problem.
(defun solve () (progn
  ; This is our input file
  (defparameter *input_file* "./test.txt")

  ; This will be our final counter
  (setq total 0)

  ; Loop through lines in file/process input 
  (let ((file (open *input_file*)))
  (when file
    (loop for line = (read-line file nil)
      while line
      ; Generate the line number
      do (let ((generated_number (strip_line_to_number line))) 
        (progn
          ; Set the value of the total to itself plus 
          ; the generated number
          (setq total (+ total generated_number))
          ; Print the original line and the generated number
          (format t "~A -> ~d~%" line generated_number)))))
  (close file))

  ; Print final number
  (format t "Total: ~d~%" total)))
{% endhighlight %}

{% include unfinished.md %}

View the final source code [here](https://gist.github.com/nickelulz/af897ea7624dc5dbf502885b30280d3d).
