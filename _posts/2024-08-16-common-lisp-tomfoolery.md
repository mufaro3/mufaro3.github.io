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

## Hacking Advent of Code in Lisp

### Part 1: Planning 

I started by picking the [Day 1 problem from last year](https://adventofcode.com/2023/day/1), a problem that I've solved in as little as four lines in Java, so I was certain that there was no way it could be too overly complicated in Lisp. I was terribly, terribly wrong.

I first had to begin by deciding on the structure of my program, and I opted to go for the kind of strictly-scoped layout that I tend to use for languages like Python or Javascript that allow for code to be run outside of functions in the global scope, which looks somewhat like the following:

{% highlight plaintext %}
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
{% endhighlight %}

*Note that if this were a language that didn't allow code at the global scope, such as C or Java, then I would simply make all of the root code from `read argument` downward my `main` method/function.*

I wasn't exactly certain how I would begin going about trying to solve this problem in Lisp, so I thought first about how I would try solving this problem in a language like C.

{% highlight plaintext %}
function CONVERT-LINE-TO-NUMBER(line-string)
  first-number and second-number := 0

  loop through the string
    if current-position is a number
      convert current-position to integer
      if first-number := 0
        first-number := current-position
      second-number := current-position

  return first-number * 10 + second-number
end function

function SOLVE-PROBLEM()
  counter := 0

  open input file
  loop through each line of the input file
    file-number-value := CONVERT-LINE-TO-NUMBER( line-string )
    add/append file-number-value to counter

  print counter
end function
{% endhighlight %}

### Part 2: Failure

From there, I started on the main function of my Lisp program:

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

At this point, I looked like I was steadily on track to solving the problem effecitvely, correct? Wrong. I kept running into prolems when attempting to develop the `strip_line_to_number` function by unnecessarily overcomplicating the program.

{% highlight common_lisp %}
; Converts a string into the number value that
; the string is worth
;
; Example: "pqr3stu8vwx" -> 38
(defun strip_line_to_number (line)
  (let ((first_last_number (get_first_last_number line)))
    ; Parse the resulting integer
    (parse-integer 
      ; Concatenate the first and last characters
      (concatenate 'string 
        ; First character of the line
        (nth 0 first_last_number)
        ; Second character of the line
        (nth 1 first_last_number)))))
{% endhighlight %}

The reason why this function layout sucks so badly is that unlike the intended imperative solution, this functional solution has to obtain the first and last characters separately as strings and then concatenate them before parsing them to a string rather than converting each character to an integer immediately then doing a simple math operation to make the final number, and the reason why this is so counterproductive can be seen in the next set of functions:

{% highlight common_lisp %}
; Removes all non-numerical characters from a
; string. Returns the remaining string.
;
; Example: "treb7uchet" -> "7"
;          "1abc2"      -> "12"
(defun remove_non_numeric (str) 
  (let ((numeric-chars (remove-if-not #'digit-char-p str))) 
    (coerce numeric-chars 'string)))

; Returns the first and last characters of a
; string as a list.
;
; Example: "treb7uchet" -> [ "t", "t" ]
;          "1abc2"      -> [ "1", "2" ]
(defun first_and_last_characters (str) 
  (if (or (null str) (zerop (length str)) nil 
    (values (char str 0) (char str 1- (length str))))))

; Returns a list containing the first and last
; numbers of a line in a string representation
;
; Example: "treb7uchet" -> ["7", "7"]
;          "1abc2"      -> ["1", "2"]
(defun get_first_last_number (str) 
  (first_and_last_characters (remove_non_numeric str)))
{% endhighlight %}

This function chooses to first manually remove all non-numeric characters from the string (which is already one \\(O(n)\\) loop right there), before obtaining the first and last characters of the new integer-only string as a list trhoguh indexing, and this method sucks because edge cases where there is only one number in the string have to have additional code written to manually copy the value to both our `first-number` and `second-number` variables.

### Part 3: Success

### Part 4: What I Learned About Lisp In The Process

{% include unfinished.md %}

View the final source code [here](https://gist.github.com/nickelulz/af897ea7624dc5dbf502885b30280d3d).
