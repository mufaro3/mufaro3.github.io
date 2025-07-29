---
layout: post
title:  "Looking into \"Excursions in Number Theory\""
date:   2025-07-28 11:30:18
comments: true
categories:
    - programming
tags:
    - mathematics
    - common-lisp
    - in-progress
---

In New York, I picked up a new fun textbook, *Excursions in Number Theory* by C. Stanley Ogilvy and John T. Anderson. It's a fun little pocketread about all sorts of wacky things in number theory, a field of principle mathematics that seems pointless on the surface but tends to have big implications. The book is fairly short, only 144 pages of direct content, but the information is puzzling to say the least. I found the book next to mathematical books about various games like Poker and Chess, and as this book tackles concepts like probability and combinatorics heavily (as that is where the majority of the implications of number theory seems to lie), it seemed like it fit in well.

In this post, I'm going to dive into the book chapter-by-chapter with some light thoughts, some proofs, and what I think about what was written. Additionally, in order to understand what was going on in each chapter, I sought to try and write some example code alongside each lesson in Common Lisp, so perhaps this could be something of an educational journey for both you, the reader, and me. As usual, it's best to read this post as though it were a Jupyter notebook (or something of the sort).

### Chapter 1: The Beginnings

The book opens by explaining the necessity of number theory. One thing it shows is how number theory serves as the basis by which efficient algorithms for all kinds of calculations is usually produced.

One example the authors provide is the how non-decimal number systems had no mainstream application or use for many years (a topic largely only explored under number theory) until the invention of binary computers, which operate fundamentally on the binary number system (zeroes and ones) and then at higher levels on octal and hexadecimal.

For those unaware, the basic understanding of what a number system is essentially the basis number through which numbers are represented by the multiplication of numbers less than that number by powers of that number. Put more mathematically, we can represent any number that exists using a number system based on the natural number $$n$$ by multiplying powers of $$n$$ by the whole numbers within $$[0,n-1]$$. For example, if we wanted to make the number $$N$$, we would use the form

$$N = a_1 \times n^0 + a_2 \times n^1 + a_3 \times n^2 + \dots$$

where $$a_i \in [0, n-1]$$ for all $$i$$. For an even simpler, direct example, the decimal system works where $$n=10$$. Imagine we wanted to represent a number like 203463 in decimal (which is just as we normally write it). Therefore, we could represent it as

$$203463 = 2 \times 10^5 + 0 \times 10^4 + 3 \times 10^3 + 4 \times 10^2 + 6 \times 10^1 + 3 \times 10^0.$$

Now, continuing onward, we know that the decimal system makes the most sense for computers because its the easiest to represent (as it only requires two values, 1 and 0. For example, we can represent the number 13 using the following powers, essentially just adding up specific values of powers of 2:

$$13 = 1 \times 2^3 + 1 \times 2^2 + 0 \times 2^1 + 1 \times 2^0 = 8 + 4 + 0 + 1,$$

and then, we can represent it as just being the coefficients from that expression, leading us to represent 13 in binary as 1101.

Now, specifically, one of the applications of number theory that the book references is the fast algorithm for converting decimal numbers into binary (as its very quick to convert numbers from non-decimal number systems into demical but seemingly tedious to convert from decimal to non-decimal systems due to having to find and calculate the powers of the system).

This quick algorithm boils down to taking an ordered stock of the pattern in which the division of a number by 2 repeatedly results in odd or even numbers (discarding the $$1/2$$ each time the result is odd). For example, for the number 13, we can divide it by 2 until we reach 0, discarding $$\frac{1}{2}$$ each time, to get the sequence 13, 6, 3, 1. Then, we can write 1 for each odd number and 0 for each even number and reverse the order, giving 1, 0, 1, 1, which is the digits of 13 in binary (as we showed before).

I thought that this algorithm was ingenious, so I wrote some common lisp code to test it out.

{% highlight lisp %}
(defun binary (n)
  (labels ((binary-recursive (cur seen)
	     (if (= 0 cur)
		 seen
		 (let ((next-value-int (truncate (/ cur 2))))
		   (binary-recursive next-value-int
				     (cons (if (evenp cur) 0 1) seen))))))
    (binary-recursive n nil)))
{% endhighlight %}

and a few tests of the function result in exactly the behavior we expect!

{% highlight lisp %}
(print (binary 13)) -> (1 1 0 1)
(print (binary 10)) -> (1 0 1 0)
(print (binary 27)) -> (1 1 0 1 1)
{% endhighlight %}

### Chapter 2: Number Patterns

The next chapter deals with patterns in numbers, such as efficient algorithms for adding numbers. One example they provide is the somewhat legendary story of Gauss adding all of the numbers between 1 and 100 incredibly quickly as a child (I'll save you all the story: he just knew to pair up numbers on either side of 50 including 0 such that it turned into a product of 11 50s, or $$11 \times 50 = 5050$$).

I thought this chapter was interesting and good foundational mathematics. It deals with series such as the triangular and square numbers and how formulae for adding numbers, squares, and cubes were proven (but not developed) by introducing the method of proof by induction.

### Chapter 3: Prime Numbers as Building Blocks

This chapter, obviously, is all about prime numbers: finding primes, calculatings made easy with primes, and relatively prime numbers. One thing it touches on, for example, is Euclid's algorithm for calculating greatest common denominators as a way to check if two numbers are relatively prime (meaning their only shared factor is 1) or not. The algorithm consists of taking two numbers you want to test, a and b, and then repeatedly changing whichever value is greater to the absolute value of the difference between the two numbers until the two numbers become factorable, at which case the greatest common denominator is the smaller of the two values.

To test this, I wrote the following code
{% highlight lisp %}
(defun euclid-gcd (a b)
  (if (= 0 (mod a b))
      (min a b)
      (if (> a b)
	  (euclid-gcd (- a b) b)
	  (euclid-gcd a (- b a)))))
{% endhighlight %}

Now, Common Lisp does provide Euclid's greatest common denominator algorithm as a standard library function in the form `gcd` but I chose to make my own, `euclid-gcd` regardless. What I found is that this function seemed to work (of course, I can't test it for all numbers, but we know that by proof given in the book, it works.

Furthermore, this is where things get insane. The book poses the question of "*If we picked any two numbers at random, what is the probability that they are relatively prime?*," and it provides a lengthy proof involving the Riemann Zeta function that proves that the probability is $$6/\pi^2,$$ or roughly 0.61 (or about 61%). I wanted to test this empirically, so I wrote the following function:

{% highlight lisp %}
(defun test-probability (to-see max-value)
  (labels ((test-probability-recursive (total-seen prime-pairs)
	     (if (>= total-seen to-see)
		 (/ prime-pairs total-seen)
		 (progn (if (= 1 (euclid-gcd (random max-value)
					     (random max-value)))
			    (incf prime-pairs)
			    nil)
			(test-probability-recursive (1+ total-seen)
						    prime-pairs)))))
    (test-probability-recursive 0 0)))
{% endhighlight %}

and with the following test, running the function 10 times, generating 10,000 pairs each time and selecting values within $$[0, 10000]$$:

{% highlight lisp %}
(loop for x from 0 to 10 do
      (format T "~f~%" (test-probability 10000 10000)))
{% endhighlight %}

I empirically observed the result

{% highlight text %}
0.6068
0.6089
0.6052
0.6054
0.6129
0.6033
0.5967
0.6007
0.6056
0.6029
{% endhighlight %}

So, wow! It was true! I honestly couldn't believe my eyes when I saw this. That's just the power of number theory, we could calculate a limit to a problem like that that seemed so random and neigh-unanswerable.

{% include unfinished.md %}