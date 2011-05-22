>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Elementary Haskell}}

'''Recursion''' is a clever idea which plays a central role in Haskell (and computer science in general): namely, recursion is the idea of using a given function as part of its own definition.  A function defined in this way is said to be '''recursive'''.  It might sound like this always leads to infinite regress, but if done properly it doesn't have to.

Generally speaking, a recursive definition comes in two parts.  First, there are one or more ''base cases'' which say what to do in simple cases where no recursion is necessary (that is, when the answer can be given straightaway without recursively calling the function being defined).  This ensures that the recursion can eventually stop. The ''recursive case'' is more general, and defines the function in terms of a 'simpler' call to itself.  Let's look at a few examples.





== Numeric recursion ==

=== The factorial function ===
In mathematics, especially combinatorics, there is a function used fairly frequently called the '''factorial''' function <ref>In mathematics, ''n''! normally means the factorial of ''n'', but that syntax is impossible in Haskell, so we don't use it here. </ref>. It takes a single number as an argument, finds all the numbers between one and this number, and multiplies them all together. For example, the factorial of 6 is 1 × 2 × 3 × 4 × 5 × 6 = 720. This is an interesting function for us, because it is a candidate to be written in a recursive style.

The idea is to look at the factorials of adjacent numbers:

{{HaskellExample|Factorials of consecutive numbers|
<pre>
Factorial of 6 = 6 × 5 × 4 × 3 × 2 × 1
Factorial of 5 =     5 × 4 × 3 × 2 × 1
</pre>
}}

Notice how we've lined things up. You can see here that the factorial of 6 involves the factorial of 5. In fact, the factorial of 6 is just 6 × (factorial of 5). Let's look at another example:

{{HaskellExample|Factorials of consecutive numbers|
<pre>
Factorial of 4 = 4 × 3 × 2 × 1
Factorial of 3 =     3 × 2 × 1
Factorial of 2 =         2 × 1
Factorial of 1 =             1
</pre>
}}

Indeed, we can see that the factorial of any number is just that number multiplied by the factorial of the number one less than it. There's one exception to this: if we ask for the factorial of 0, we don't want to multiply 0 by the factorial of -1 In fact, we just say the factorial of 0 is 1 (we ''define'' it to be so. It just is, okay?<ref>Actually, defining the factorial of 0 to be 1 is not just arbitrary; it's because the factorial of 0 represents an [[w:empty product|empty product]].</ref>). So, 0 is the ''base case'' for the recursion: when we get to 0 we can immediately say that the answer is 1, without using recursion.  We can summarize the definition of the factorial function as follows:

* The factorial of 0 is 1.
* The factorial of any other number is that number multiplied by the factorial of the number one less than it.

We can translate this directly into Haskell:

{{HaskellExample|1=Factorial function|2=<source lang="haskell">
factorial 0 = 1
factorial n = n * factorial (n-1)
</source>}}

This defines a new function called <code>factorial</code>.  The first line says that the factorial of 0 is 1, and the second one says that the factorial of any other number <code>n</code> is equal to <code>n</code> times the factorial of <code>n-1</code>.  Note the parentheses around the <code>n-1</code>: without them this would have been parsed as <code>(factorial n) - 1</code>; function application (applying a function to a value) will happen before anything else does (we say that function application ''binds more tightly'' than anything else).

{{warning|1=
This function must be defined in a file, rather than using <code>let</code> statements in the interpreter. The latter will cause the function to be redefined with the last definition, losing the important first definition and leading to infinite recursion.
}}

This all seems a little voodoo so far. How does it work? Well, let's look at what happens when you execute <code>factorial 3</code>:

* 3 isn't 0, so we ''recur'': work out the factorial of 2
** 2 isn't 0, so we recur.
*** 1 isn't 0, so we recur.
**** 0 ''is'' 0, so we return 1.
*** We multiply the current number, 1, by the result of the recursion, 1, obtaining 1 (1 × 1).
** We multiply the current number, 2, by the result of the recursion, 1, obtaining 2 (2 × 1 × 1).
* We multiply the current number, 3, by the result of the recursion, 2, obtaining 6 (3 × 2 × 1 × 1).

We can see how the multiplication 'builds up' through the recursion.

(Note that we end up with the one appearing twice, since the base case is 0 rather than 1; but that's okay since multiplying by one has no effect.  We could have designed <code>factorial</code> to stop at 1 if we had wanted to, but it's conventional, and often useful, to have the factorial of 0 defined.)

One more thing to note about the recursive definition of <code>factorial</code>: the order of the two declarations (one for <code>factorial 0</code> and one for <code>factorial n</code>) ''is'' important.  Haskell decides which function definition to use by starting at the top and picking the first one that matches. In this case, if we had the general case (<code>factorial n</code>) before the 'base case' (<code>factorial 0</code>), then the general <code>n</code> would match ''anything'' passed into it -- including 0. So <code>factorial 0</code> would match the general <code>n</code> case, the compiler would conclude that <code>factorial 0</code> equals <code>0 * factorial (-1)</code>, and so on to negative infinity.  Definitely not what we want.  The lesson here is that one should always list multiple function definitions starting with the most specific and proceeding to the most general.

{{Exercises|1=
# Type the factorial function into a Haskell source file and load it into your favourite Haskell environment.
#* What is <code>factorial 5</code>?
#* What about <code>factorial 1000</code>? If you have a scientific calculator (that isn't your computer), try it there first. Does Haskell give you what you expected?
#* What about <code>factorial (-1)</code>?  Why does this happen?
# The ''double factorial'' of a number n is the product of ''every other'' number from 1 (or 2) up to n.  For example, the double factorial of 8 is 8 × 6 × 4 × 2 = 384, and the double factorial of 7 is 7 × 5 × 3 × 1 = 105.  Define a <code>doublefactorial</code> function in Haskell.
}}

=== A quick aside ===
:''This section is aimed at people who are used to more imperative-style languages like C and Java.''

''Loops'' are the bread and butter of imperative languages.  For example, the idiomatic way of writing a factorial function in an imperative language would be to use a ''for'' loop, like the following (in C):

{{HaskellExample|1=The factorial function in an imperative language|2=
<source lang="c">
int factorial(int n) {
  int res;
  for (res = 1; n > 1; n--)
    res *= n;
  return res;
}
</source>
}}

This isn't directly possible in Haskell, since changing the value of the variables <code>res</code> and <code>n</code> (a destructive update) would not be allowed. However, you can always translate a loop into an equivalent recursive form.  The idea is to make each loop variable in need of updating into a parameter of a recursive function. For example, here is a direct 'translation' of the above loop into Haskell:

{{HaskellExample|1=Using recursion to simulate a loop|2=<source lang="haskell">
factorial n = factorialWorker n 1 where
factorialWorker n res | n > 1     = factorialWorker (n - 1) (res * n)
                      | otherwise = res
</source>}}

The expressions after the vertical bars are called ''guards'', and we'll learn more about them in the section on [[Haskell/Control structures|control structures]].  For now, you can probably figure out how they work by comparing them to the corresponding C code above.

Obviously this is not the shortest or most elegant way to implement <code>factorial</code> in Haskell (translating directly from an imperative paradigm into Haskell like this rarely is), but it can be nice to know that this sort of translation is always possible.

Another thing to note is that you shouldn't be worried about poor performance through recursion with Haskell. In general, functional programming compilers include a lot of optimisation for recursion, including an important one called ''tail-call optimisation''; remember too that Haskell is lazy -- if a calculation isn't needed, it won't be done. We'll learn about these in later chapters.

=== Other recursive functions ===
As it turns out, there is nothing particularly special about the <code>factorial</code> function; a great many numeric functions can be defined recursively in a natural way. For example, let's think about multiplication. When you were first introduced to multiplication (remember that moment? :)), it may have been through a process of 'repeated addition'. That is, 5 × 4 is the same as summing four copies of the number 5. Of course, summing four copies of 5 is the same as summing three copies, and then adding one more -- that is, 5 × 4 = 5 × 3 + 5.  This leads us to a natural recursive definition of multiplication:

{{HaskellExample|1=Multiplication defined recursively|2=<source lang="haskell">
mult n 0 = 0                      -- anything times 0 is zero
mult n 1 = n                      -- anything times 1 is itself
mult n m = (mult n (m - 1)) + n   -- recur: multiply by one less, and add an extra copy
</source>}}

Stepping back a bit, we can see how numeric recursion fits into the general recursive pattern.  The base case for numeric recursion usually consists of one or more specific numbers (often 0 or 1) for which the answer can be immediately given.  The recursive case computes the result by recursively calling the function with a smaller argument and using the result in some manner to produce the final answer.  The 'smaller argument' used is often one less than the current argument, leading to recursion which 'walks down the number line' (like the examples of <code>factorial</code> and <code>mult</code> above), but it doesn't have to be; the smaller argument could be produced in some other way as well.

{{Exercises|1=
# Expand out the multiplication 5 × 4 similarly to the expansion we used above for <code>factorial 3</code>.
# Define a recursive function <code>power</code> such that <code>power x y</code> raises <code>x</code> to the <code>y</code> power.
# You are given a function <code>plusOne x = x + 1</code>.  Without using any other <code>(+)</code>s, define a recursive function <code>addition</code> such that <code>addition x y</code> adds <code>x</code> and <code>y</code> together.
# (Harder) Implement the function <code>log2</code>, which computes the integer log (base 2) of its argument.  That is, <code>log2</code> computes the exponent of the largest power of 2 which is less than or equal to its argument.  For example, <code>log2 16 = 4</code>, <code>log2 11 = 3</code>, and <code>log2 1 = 0</code>. (Small hint: read the last phrase of the paragraph immediately preceding these exercises.)
}}

== List-based recursion ==

A ''lot'' of functions in Haskell turn out to be recursive, especially those concerning lists.<ref>This is no coincidence; without mutable variables, recursion is the only way to implement control structures.  This might sound like a limitation until you get used to it (it isn't, really).</ref> Consider the <code>length</code> function that finds the length of a list:

{{HaskellExample|1=The recursive definition of <code>length</code>|2=<source lang="haskell">
length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs
</source>}}

Don't worry too much about the syntax; we'll learn more about it in the section on  [[../Pattern matching/]].  For now, let's rephrase this code into English to get an idea of how it works.  The first line gives the type of <code>length</code>: it takes any sort of list and produces an <code>Int</code>.  The next line says that the length of an empty list is 0.  This, of course, is the base case.  The final line is the recursive case: if a list consists of a first element <code>x</code> and another list <code>xs</code> representing the rest of the list, the length of the list is one more than the length of <code>xs</code>.

How about the concatenation function <code>(++)</code>, which joins two lists together? (Some examples of usage are also given, as we haven't come across this function so far in this chapter.)

{{HaskellExample|1=The recursive <code>(++)</code>|2=
{{HaskellGHCi|1=
Prelude> [1,2,3] ++ [4,5,6]
[1,2,3,4,5,6]
Prelude> "Hello " ++ "world" -- Strings are lists of Chars
"Hello world"
}}

<source lang="haskell">
(++) :: [a] -> [a] -> [a]
[] ++ ys     = ys
(x:xs) ++ ys = x : xs ++ ys
</source>
}}

This is a little more complicated than <code>length</code> but not too difficult once you break it down.  The type says that <code>(++)</code> takes two lists and produces another.  The base case says that concatenating the empty list with a list <code>ys</code> is the same as <code>ys</code> itself.  Finally, the recursive case breaks the first list into its head (<code>x</code>) and tail (<code>xs</code>) and says that to concatenate the two lists, concatenate the tail of the first list with the second list, and then tack the head <code>x</code> on the front.

There's a pattern here: with list-based functions, the base case usually involves an empty list, and the recursive case involves passing the tail of the list to our function again, so that the list becomes progressively smaller.

{{Exercises|1=
Give recursive definitions for the following list-based functions. In each case, think what the base case would be, then think what the general case would look like, in terms of everything smaller than it.
# <code>replicat :: Int -> a -> [a]</code>, which takes an element and a count and returns the list which is that element repeated that many times. E.g. <code>replicat 3 'a' = "aaa"</code>. (Hint: think about what replicate of anything with a count of 0 should be; a count of 0 is your 'base case'.)
# <code>(!!) :: [a] -> Int -> a</code>, which returns the element at the given 'index'. The first element is at index 0, the second at index 1, and so on. Note that with this function, you're recurring ''both'' numerically and down a list.
# (A bit harder.) <code>zip :: [a] -> [b] -> [(a, b)]</code>, which takes two lists and 'zips' them together, so that the first pair in the resulting list is the first two elements of the two lists, and so on. E.g. <code>zip [1,2,3] "abc" = [(1, 'a'), (2, 'b'), (3, 'c')]</code>. If either of the lists is shorter than the other, you can stop once either list runs out. E.g. <code>zip [1,2] "abc" = [(1, 'a'), (2, 'b')]</code>.
}}

Recursion is used to define nearly all functions to do with lists and numbers. The next time you need a list-based algorithm, start with a case for the empty list and a case for the non-empty list and see if your algorithm is recursive.

== Don't get TOO excited about recursion... ==
Although it's very important to have a solid understanding of recursion when programming in Haskell, one rarely has to write functions that are explicitly recursive.  Instead, there are all sorts of standard library functions which perform recursion for you in various ways, and one usually ends up using those instead.  For example, a much simpler way to implement the <code>factorial</code> function is as follows:

{{HaskellExample|1=Implementing factorial with a standard library function|2=<source lang="haskell">
factorial n = product [1..n]
</source>}}

Almost seems like cheating, doesn't it? :) This is the version of <code>factorial</code> that most experienced Haskell programmers would write, rather than the explicitly recursive version we started out with.  Of course, the <code>product</code> function is using some list recursion behind the scenes<ref>Actually, it's using a function called <code>foldl</code>, which actually does the recursion.</ref>, but writing <code>factorial</code> in this way means you, the programmer, don't have to worry about it.

== Summary ==
Recursion is the practise of using a function you're defining in the body of the function itself. It nearly always comes in two parts: a base case and a recursive case. Recursion is especially useful for dealing with list- and number-based functions.

{{Haskell navigation|chapter=Elementary Haskell}}

== Notes ==
<references />

{{Auto category}}
