>﻿{{Haskell minitoc|chapter=Haskell Performance}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Haskell Performance}}

Complexity Theory is the study of how long a program will take to run,
depending on the size of its input.  There are many good introductory
books to complexity theory and the basics are explained in any good
algorithms book.  I'll keep the discussion here to a minimum.

The idea is to say how well a program scales with more data.  If you
have a program that runs quickly on very small amounts of data but
chokes on huge amounts of data, it's not very useful (unless you know
you'll only be working with small amounts of data, of course).
Consider the following Haskell function to return the sum of the
elements in a list:

<pre>
sum [] = 0
sum (x:xs) = x + sum xs
</pre>
How long does it take this function to complete?  That's a very
difficult question; it would depend on all sorts of things: your
processor speed, your amount of memory, the exact way in which the
addition is carried out, the length of the list, how many other
programs are running on your computer, and so on.  This is far too
much to deal with, so we need to invent a simpler model.  The model we
use is sort of an arbitrary "machine step."  So the question is
"how many machine steps will it take for this program to complete?"
In this case, it only depends on the length of the input list.

If the input list is of length <math>0</math>, the function will take either <math>0</math>
or <math>1</math> or <math>2</math> or some very small number of machine steps, depending
exactly on how you count them (perhaps <math>1</math> step to do the pattern
matching and <math>1</math> more to return the value <math>0</math>).  What if the list is
of length <math>1</math>?  Well, it would take however much time the list of
length <math>0</math> would take, plus a few more steps for doing the first (and
only element).

If the input list is of length <math>n</math>, it will take however many steps an
empty list would take (call this value <math>y</math>) and then, for each element
it would take a certain number of steps to do the addition and the
recursive call (call this number <math>x</math>).  Then, the total time this
function will take is <math>nx+y</math> since it needs to do those additions <math>n</math>
many times.  These <math>x</math> and <math>y</math> values are called ''constant
values'', since they are independent of <math>n</math>, and actually dependent
only on exactly how we define a machine step, so we really don't want
to consider them all that important.  Therefore, we say that the
complexity of this <code>sum</code> function is <math>\mathcal{O}(n)</math> (read "order <math>n</math>").
Basically saying something is <math>\mathcal{O}(n)</math> means that for some constant
factors <math>x</math> and <math>y</math>, the function takes <math>nx+y</math> machine steps to
complete.

Consider the following sorting algorithm for lists (commonly called
"insertion sort"):

<pre>
sort []  = []
sort [x] = [x]
sort (x:xs) = insert (sort xs)
    where insert [] = [x]
          insert (y:ys) | x <= y    = x : y : ys
                        | otherwise = y : insert ys
</pre>
The way this algorithm works is as follow: if we want to sort an empty
list or a list of just one element, we return them as they are, as
they are already sorted.  Otherwise, we have a list of the form
<code>x:xs</code>.  In this case, we sort <code>xs</code> and then want to insert
<code>x</code> in the appropriate location.  That's what the <code>insert</code>
function does.  It traverses the now-sorted tail and inserts <code>x</code>
wherever it naturally fits.

Let's analyze how long this function takes to complete.  Suppose it
takes <math>f(n)</math> stepts to sort a list of length <math>n</math>.  Then, in order to
sort a list of <math>n</math>-many elements, we first have to sort the tail of
the list first, which takes <math>f(n-1)</math> time.  Then, we have to insert
<code>x</code> into this new list.  If <code>x</code> has to go at the end, this will
take <math>\mathcal{O}(n-1)=\mathcal{O}(n)</math> steps.  Putting all of this together, we see that
we have to do <math>\mathcal{O}(n)</math> amount of work <math>\mathcal{O}(n)</math> many times, which means
that the entire complexity of this sorting algorithm is <math>\mathcal{O}(n^2)</math>.
Here, the squared is not a constant value, so we cannot throw it out.

What does this mean?  Simply that for really long lists, the <code>sum</code>
function won't take very long, but that the <code>sort</code> function will
take quite some time.  Of course there are algorithms that run much
more slowly than simply <math>\mathcal{O}(n^2)</math> and there are ones that run more
quickly than <math>\mathcal{O}(n)</math>. (Also note that a <math>\mathcal{O}(n^2)</math> algorithm may actually be much faster than a <math>\mathcal{O}(n)</math> algorithm in practice, if it takes much less time to perform a single step of the <math>\mathcal{O}(n^2)</math> algorithm.)

Consider the random access functions for lists and arrays.  In the
worst case, accessing an arbitrary element in a list of length <math>n</math>
will take <math>\mathcal{O}(n)</math> time (think about accessing the last element).
However with arrays, you can access any element immediately, which is
said to be in ''constant'' time, or <math>\mathcal{O}(1)</math>, which is basically as
fast an any algorithm can go.

There's much more in complexity theory than this, but this should be
enough to allow you to understand all the discussions in this
tutorial.  Just keep in mind that <math>\mathcal{O}(1)</math> is faster than <math>\mathcal{O}(n)</math> is
faster than <math>\mathcal{O}(n^2)</math>, etc.

== Optimising ==

{{Haskell stub|sectiononly=1}}

=== Profiling ===

{{Haskell navigation|chapter=Haskell Performance}}

{{Auto category}}
