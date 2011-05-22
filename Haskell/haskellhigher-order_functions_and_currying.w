>﻿{{Haskell minitoc|chapter=Elementary Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Elementary Haskell}}

Higher-order functions are functions that take other functions as arguments. We have already met some of them, such as <code>map</code>, so there isn't anything really frightening or unfamiliar about them.  They offer a form of abstraction that is unique to the functional programming style.  In functional programming languages like Haskell, functions are just like any other value, so it doesn't get any harder to deal with higher-order functions. 

Higher order functions have a separate chapter in this book, not because they are particularly difficult -- we've already worked with them, after all -- but because they are powerful enough to draw special attention to them.   We will see in this chapter how much we can do if we can pass around functions as values.  Generally speaking, it is a good idea to abstract over a functionality whenever we can.  Besides,  Haskell without higher order functions wouldn't be quite as much fun. 

== The Quickest Sorting Algorithm In Town ==
Don't get too excited, but <code>quickSort</code> is certainly ''one'' of the quickest. Have you heard of it? If so, you can skip the following subsection and go straight to the next one:
=== The Idea Behind <code>quickSort</code> ===
The idea is very simple. For a big list, we pick an element, and divide the whole list into three parts. 

The first part has all elements that should go before that element, the second part consists of all of the elements that are equal to the picked element, the third has the elements that ought to go after that element. And then, of course, we are supposed to concatenate these. What we get is somewhat better, right? 

The trick is to note that only the first and the third are yet to be sorted, and for the second, sorting doesn't really make sense (they are all equal!). How to go about sorting the yet-to-be-sorted sub-lists? Why... apply the same algorithm on them again! By the time the whole process is finished, you get a completely sorted list.

=== So Let's Get Down To It! ===

<source lang="haskell">
-- if the list is empty, we do nothing
-- note that this is the base case for the recursion
quickSort [] = []

-- if there's only one element, no need to sort it
-- actually, the third case takes care of this one pretty well
-- I just wanted you to take it step by step
quickSort [x] = [x]

-- this is the gist of the process
-- we pick the first element as our "pivot", the rest is to be sorted
-- don't forget to include the pivot in the middle part!
quickSort (x : xs) = (quickSort less) ++ (x : equal) ++ (quickSort more)
                     where less = filter (< x) xs
                           equal = filter (== x) xs
                           more = filter (> x) xs
</source>

And we are done! I suppose if you ''have'' met <code>quickSort</code> before, you probably thought recursion was a neat trick but difficult to implement.

== Now, How Do We Use It? ==
With <code>quickSort</code> at our disposal, sorting any list is a piece of cake. Suppose we have a list of <code>String</code>, maybe from a dictionary, and we want to sort them; we just apply <code>quickSort</code> to the list. For the rest of this chapter, we will use a pseudo-dictionary of words (but a 25,000 word dictionary should do the trick as well):

<source lang="haskell">
dictionary = ["I", "have", "a", "thing", "for", "Linux"]
</source>

We get, for <code>quickSort dictionary</code>,

<source lang="haskell">
["I", "Linux", "a", "for", "have", "thing"]
</source>

But, what if we wanted to sort them in the ''descending'' order? Easy, just reverse the list, <code>reverse (quickSort dictionary)</code> gives us what we want.

But wait! We didn't really ''sort'' in the descending order, we sorted (in the ''ascending'' order) and reversed it. They may have the same effect, but they are not the same thing!

Besides, you might object that the list you got isn't what you wanted. "a" should certainly be placed before "I". "Linux" should be placed between "have" and "thing". What's the problem here?

The problem is, the way <code>String</code>s are represented in a typical programming settings is by a list of ASCII characters. ASCII (and almost all other encodings of characters) specifies that the character code for capital letters are less than the small letters. Bummer. So "Z" is less than "a". We should do something about it. Looks like we need a case insensitive <code>quickSort</code> as well. It might come handy some day.

But, there's no way you can blend that into <code>quickSort</code> as it stands. We have work to do.

== Tweaking What We Already Have ==
What we need to do is to factor out the comparisons <code>quickSort</code> makes. We need to provide <code>quickSort</code> with a ''function'' that compares two elements, and gives an <code>Ordering</code>, and as you can imagine, an <code>Ordering</code> is any of <code>LT, EQ, GT</code>.

To sort in the descending order, we supply <code>quickSort</code> with a function that returns the opposite of the usual <code>Ordering</code>. For the case-insensitive sort, we may need to define the function ourselves. By all means, we want to make <code>quickSort</code> applicable to all such functions so that we don't end up writing it over and over again, each time with only minor changes.

== <code>quickSort</code>, Take Two ==
Our <code>quickSort</code> will take two things this time: first, the comparison function, and second, the list to sort.

A comparison function will be a function that takes two things, say, <code>x</code> and <code>y</code>, and compares them. If <code>x</code> is less than <code>y</code> (according to the criteria we want to implement by this function), then the value will be <code>LT</code>. If they are equal (well, equal with respect to the comparison, we want "Linux" and "linux" to be equal when we are dealing with the insensitive case), we will have <code>EQ</code>. The remaining case gives us <code>GT</code> (pronounced: greater than, for obvious reasons).

<source lang="haskell">
-- no matter how we compare two things
-- the first two equations should not change
-- they need to accept the comparison function though
-- but the result doesn't need it
quickSort _ [] = []
quickSort _ [x] = [x]

-- we are in a more general setting now
-- but the changes are worth it!
-- c is the comparison function
quickSort c (x : xs) = (quickSort c less) ++ (x : equal) ++ (quickSort c more)
                             where less  = filter (\y -> y `c` x == LT) xs
                                   equal = filter (\y -> y `c` x == EQ) xs
                                   more  = filter (\y -> y `c` x == GT) xs
</source>

Cool!

{{body note|1=
Almost all the basic data types in Haskell are members of the <code>Ord</code> class. This class defines an ordering, the "natural" one. The functions (or, operators, in this case) <code>(<)</code>, <code>(==)</code> or <code>(>)</code> provide shortcuts to the <code>compare</code> function each type defines. When we ''want'' to use the natural ordering as defined by the types themselves, the above code can be written using those operators, as we did last time. In fact, that makes for much clearer style; however, we wrote it the long way just to make the relationship between sorting and comparing more evident.}}

== But What Did We Gain? ==
Reuse. We can reuse <code>quickSort</code> to serve different purposes.

<source lang="haskell">
-- the usual ordering
-- uses the compare function from the Ord class
usual = compare

-- the descending ordering, note we flip the order of the arguments to compare
descending x y = compare y x

-- the case-insensitive version is left as an exercise!
insensitive = ... 
-- can you think of anything without making a very big list of all possible cases?
</source>

And we are done!

<pre>quickSort usual dictionary</pre> should, then, give <pre>["I", "Linux", "a", "for", "have", "thing"]</pre>
The comparison is just <code>compare</code> from the <code>Ord</code> class. This was our <code>quickSort</code>, before the tweaking.


<pre>quickSort descending dictionary</pre> now gives <pre>["thing", "have", "for", "a", "Linux", "I"]</pre>


And finally, <pre>quickSort insensitive dictionary</pre> gives <pre>["a", "for", "have", "I", "Linux", "thing"]</pre>

Exactly what we wanted!

{{Exercises | Write <code>insensitive</code>, such that <code>quickSort insensitive dictionary</code> gives <code>["a", "for", "have", "I", "Linux", "thing"]</code>}}

== Higher-Order Functions and Types ==
Our <code>quickSort</code> has type <code>(a -> a -> Ordering) -> [a] -> [a]</code>. 

Most of the time, the type of a higher-order function provides a good guideline about how to use it. A straightforward way of reading the type signature would be, "<code>quickSort</code> takes a function that gives an ordering of <code>a</code>s, and a list of <code>a</code>s, to give a list of <code>a</code>s". It is then natural to guess that the function sorts the list respecting the given ordering function.

Note that the parentheses surrounding <code>a -> a -> Ordering</code> is mandatory.  It says that <code>a -> a -> Ordering</code> altogether form a single argument, an argument that happens to be a function.  What happens if we omit the parentheses?  We would get a function of type <code>a -> a -> Ordering -> [a] -> [a]</code>, which accepts four arguments instead of the desired two (<code>a -> a -> Ordering</code> and <code>[a]</code>).  Furthermore none of the four arguments, neither <code>a</code> nor <code>Ordering</code> nor <code>[a]</code> are functions, so omitting the parentheses would give us something that isn't a higher order function.

Furthermore, it's worth noting that the <code>-></code> operator is right-associative, which means that <code>a -> a -> Ordering -> [a] -> [a]</code> means the same thing as <code>a -> (a -> (Ordering -> ([a] -> [a])))</code>.  We really must insist that the <code>a -> a -> Ordering</code> be clumped together by writing those parentheses... but wait... if <code>-></code> is right-associative, wouldn't that mean that the correct signature <code>(a -> a -> Ordering) -> [a] -> [a]</code> actually means... <code>(a -> a -> Ordering) -> ([a] -> [a])</code>?

Is that ''really'' what we want?

If you think about it, we're trying to build a function that takes two arguments, a function and a list, returning a list.  Instead, what this type signature is telling us is that our function takes ''one'' argument (a function) and returns another function.  That is profoundly odd... but if you're lucky, it might also strike you as being profoundly beautiful.  Functions in multiple arguments are fundamentally the same thing as functions that take one argument and give another function back. It's OK if you're not entirely convinced.  We'll go into a little bit more detail below and then show how something like this can be turned to our advantage.

{{Exercises|1=
The following exercise combines what you have learned about higher order functions, recursion and IO.  We are going to recreate what programmers from more popular languages call a "for loop".  Implement a function  <pre>for :: a -> (a->Bool) -> (a->a) -> (a-> IO ()) -> IO ()
for i p f job = -- ???</pre>An example of how this function would be used might be<pre>for 1 (<10) (+1) (print)</pre>which prints the numbers 1 to 9 on the screen.

Starting from an initial value <code>i</code>, the <code>for</code> executes <code>job i</code>.  It then modifies this value <code>f i</code> and checks to see if the modified value satisfies some condition.  If it doesn't, it stops; otherwise, the for loop continues, using the modified <code>f i</code> in place of <code>i</code>.
# The paragraph above gives an imperative description of the for loop.  What would a more functional description be?
# Implement the for loop in Haskell.
# Why does Haskell not have a for loop as part of the language, or in the standard library?

Some more challenging exercises you could try
# What would be a more Haskell-like way of performing a task like 'print the list of numbers from 1 to 10'?  Are there any problems with your solution?
# Implement a function <code>sequenceIO :: [IO a] -> IO [a]<code>.  Given a list of actions, this function runs each of the actions in order and returns all their results as a list.
# Implement a function <code>mapIO :: (a -> IO b) -> [a] -> IO [b]</code> which given a function of type <code>a -> IO b</code> and a list of type <code>[a]</code>, runs that action on each item in the list, and returns the results.

This exercise was inspired from a blog post by osfameron.  No peeking!
}}

== Currying ==
I hope you followed the reasoning of the preceding chapter closely enough. If you haven't, you should, so give it another try. 

Currying is a technique<ref>named after the outstanding logician Haskell Brooks Curry, the same guy after whom the language is named.</ref> that lets you partially apply a multi-parameter function.  When you do that, it remembers those given values, and waits for the remaining parameters. 

Our <code>quickSort</code> takes two parameters, the comparison function, and the list. We can, by currying, construct variations of <code>quickSort</code> with a given comparison function. The variation just "remembers" the specific comparison, so you can apply it to the list, and it will sort the list using the that comparison function.

<source lang="haskell">descendingSort = quickSort descending</source>

What is the type of </code>descendingSort</code>? <code>quickSort</code> was <code>(a -> a -> Ordering) -> [a] -> [a]</code>, and the comparison function <code>descending</code> was <code>a -> a -> Ordering</code>. Applying <code>quickSort</code> to <code>descending</code> (that is, applying it ''partially'', we haven't specified the list in the definition) we get a function (our <code>descendingSort</code>) for which the first parameter is already given, so you can scratch that type out from the type of <code>quickSort</code>, and we are left with a simple <code>[a] -> [a]</code>. So we can apply this one to a list right away!

<source lang="haskell">descendingSort dictionary</source> gives us <source lang="haskell">["thing", "have", "for", "a", "Linux", "I"]</source>

It's rather neat. But is it useful? You bet it is. It is particularly useful as sections, you might notice. Currying often makes Haskell programs very concise. More than that, it often makes your intentions a lot clearer. Remember 

<source lang="haskell">less = filter (< x) xs</source>

from the first version of <code>quickSort</code>? You can read it aloud like "keep those in <code>xs</code> that are less than <code>x</code>". Although <code>(< x)</code> is just a shorthand for <code>(\y -> y < x)</code>, try reading ''that'' aloud!

== Notes ==
<references/>

{{Haskell navigation|chapter=Elementary Haskell}}

{{Auto category}}
