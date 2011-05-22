>{{Haskell minitoc|chapter=Elementary Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Elementary Haskell}}

By now we have already met the basic tools for working with lists.  We can build lists up from the cons operator <code>(:)</code> and the empty list <code>[]</code> (see [[../Lists and tuples/]] if you are unsure about this); and we can take them apart by using a combination of recursion and pattern matching (as demonstrated [[Haskell/Recursion#List-based recursion|in the previous module]]).  In this and the next chapter we will delve a little deeper into the inner workings and the usage of Haskell lists.  In that process, we'll discover a bit of new notation and some characteristically Haskell-ish features like infinite lists and list comprehensions and explore some useful list manipulation functions.  But before going into this, let us step back for a moment and put together the things we have already learned about lists.

==Constructing lists==

We'll start by writing and analysing a function to double every element of a list of integers.  First, we must specify the type declaration for our function.  For our purposes here, the function takes a list of integers and evaluates to another list of integers:

<source lang="haskell">
doubleList :: [Integer] -> [Integer]
</source>

Then we must write the function definition.  As usual in such cases, we will go for a recursive definition:

<source lang="haskell">
doubleList [] = []
doubleList (n:ns) = (n * 2) : doubleList ns
</source>

First, let us briefly dissect the definition:

* The base case is for an empty list. In it, the function evaluates to an empty list.
* As for the general case, let us consider the two sides of the equality separately:
** On the left-hand side, the operator (:) is seen playing a special role on dealing with the list passed as argument, splitting it as part of a process called ''pattern matching''. This kind of breaking down always splits a list into its first element (known as the "head") and the rest of the list (known as the "tail"). In this case, the argument passed to doubleList is split into head, <code>n</code>, and tail, <code>ns</code>,<ref>it costs nothing to use the opportunity to mention the naming convention we are adopting in (n:ns) is motivated by the fact that if the list head is an element "n" then the list tail is likely to contain several other "ns" (pronounced "ens").</ref> which are then referenced on the right-hand side of the equation. 
** On the right-hand side, doubleList builds up a new list by using (:). The first element of this new list is twice the head of the argument, and the rest of the result is obtained by a recursive call - the application of doubleList to the tail. If the tail happens to be an empty list, the base case will be invoked and recursion stops.<ref>In fact, the splitting with (:) doesn't apply to empty lists - for starters, they have no elements, and therefore no head to be split.</ref>

By understanding the recursive definition we can picture what actually happens when we evaluate an expression such as

<source lang="haskell">
doubleList [1,2,3,4]
</source>

We can work it out longhand by substituting the argument into the function definition, just like schoolbook algebra:

<source lang="haskell">
doubleList 1:[2,3,4] = (1*2) : doubleList (2 : [3,4])
                     = (1*2) : (2*2) : doubleList (3 : [4])
                     = (1*2) : (2*2) : (3*2) : doubleList (4 : [])
                     = (1*2) : (2*2) : (3*2) : (4*2) : doubleList []
                     = (1*2) : (2*2) : (3*2) : (4*2) : []
                     = 2 : 4 : 6 : 8 : []
                     = [2, 4, 6, 8]
</source>

One important thing to notice is that in this longhand evaluation exercise the ''moment'' at which we choose to evaluate the multiplications does not affect the result <ref>unless one of them is an error or nontermination: we'll get to that later</ref>. If we had done them immediately after each recursive call of doubleList it would have made no difference. This reflects an important property of Haskell: it is a "pure" functional programming language. Because evaluation order can never change the result, it is mostly left to the compiler to decide when to actually evaluate things.  Haskell is a "lazy" evaluation language, so evaluation is usually deferred until the value is really needed, but the compiler is free to evaluate things sooner if this will improve efficiency.  From the programmer's point of view evaluation order rarely matters (except, for instance, in the case of infinite lists, which will be discussed soon).

A function to double a list has quite limited applicability. An obvious improvement would be generalizing it to allow multiplication by any number. Doing so would require a function that takes a multiplicand as well as a list of integers. It could look like this:

<source lang="haskell">
multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList _ [] = []
multiplyList m (n:ns) = (m*n) : multiplyList m ns
</source>

This example deploys <code>_</code>, which is used as a "don't care" argument. The multiplicand is not used for the base case, so instead of giving it a name (like <code>m</code>, <code>n</code> or <code>ns</code>) it is explicitly thrown away.

An additional observation about the type declaration. Hiding behind the rather odd syntax is a deep and clever idea.  The "->" arrow is actually an operator for types, and is right associative.  So if you add in the implied brackets the type definition is actually

<source lang="haskell">
multiplyList :: Integer -> ( [Integer] -> [Integer] )
</source>

Think about what this is saying.  It means that "multiplyList" doesn't take two arguments.  Instead it takes one (an Integer), and then evaluates to ''a new function''.  This new function itself takes one argument (a list of Integers) and evaluates to another list of Integers. This process is called "currying", and is a very important technique which will be explored further later in the book.

The new function can be used in a straightforward way:

<source lang="haskell">
evens = multiplyList 2 [1,2,3,4]
</source>

or it can do something which, in most other languages, would be an error; this is partial function application and because we're using Haskell, we can write the following neat & elegant bits of code:

<source lang="haskell">
doubleList = multiplyList 2
evens = doubleList [1,2,3,4]
</source>

It may help you to understand if you put the implied brackets in the first definition of "evens":

<source lang="haskell">
evens = (multiplyList 2) [1,2,3,4]
</source>

In other words <code>multiplyList 2</code> evaluates to a new function that is then applied to [1,2,3,4].

===Dot Dot Notation===
Haskell has a convenient shorthand for specifying a list containing a sequence of integers.  Some examples to illustrate it:

<source lang="haskell">
Code             Result
----             ------
[1..10]          [1,2,3,4,5,6,7,8,9,10]
[2,4..10]        [2,4,6,8,10]
[5,4..1]         [5,4,3,2,1]
[1,3..10]        [1,3,5,7,9]
</source>

The same notation can be used for floating point numbers and characters as well.  However, be careful with floating point numbers: rounding errors can cause unexpected things to happen. Try this:

<source lang="haskell">
[0,0.1 .. 1]
</source>

Additionally, the notation only works be used for sequences with fixed differences between consecutive elements. For instance, you can't put in 

<source lang="haskell">
[0,1,1,2,3,5,8..100]
</source>

and expect to get back the rest of the Fibonacci series, or put in the beginning of a geometric sequence like

<source lang="haskell">
[1,3,9,27..100]
</source>

===Infinite Lists===
One of the most mind-bending things about Haskell lists is that they are allowed to be ''infinite''.  For example, the following generates the infinite list of integers starting with 1:

<source lang="haskell">
[1..]
</source>

(If you try this in GHCi, remember you can stop an evaluation with Ctrl-c).

Or you could define the same list in a more primitive way by using a recursive function:

<source lang="haskell">
intsFrom n = n : intsFrom (n+1)
positiveInts = intsFrom 1
</source>

This works because Haskell uses lazy evaluation: it never actually evaluates more than it needs at any given moment.  In most cases an infinite list can be treated just like an ordinary one.  The program will only go into an infinite loop when evaluation would actually require all the values in the list.  Examples of this include sorting or printing the entire list.  However:

<source lang="haskell">
evens = doubleList [1..]
</source>

will define "evens" to be the infinite list [2,4,6,8..].  And you can pass "evens" into other functions, and it will all just work.  See the exercise 4 below for an example of how to process an infinite list and then take the first few elements of the result.

Infinite lists are quite useful in Haskell.  Often it's more convenient to define an infinite list and then take the first few items than to create a finite list.  Functions that process two lists in parallel generally stop with the shortest, so making the second one infinite avoids having to find the length of the first.  An infinite list is often a handy alternative to the traditional endless loop at the top level of an interactive program.

{{Exercises|1=
Write the following functions and test them out.  Don't forget the type declarations. 

# takeInt returns the first ''n'' items in a list.  So takeInt 4 [11,21,31,41,51,61] returns [11,21,31,41]
# dropInt drops the first ''n'' items in a list and returns the rest.  so dropInt 3 [11,21,31,41,51] returns [41,51].
# sumInt returns the sum of the items in a list.
# scanSum adds the items in a list and returns a list of the running totals.  So scanSum [2,3,4,5] returns [2,5,9,14].  Is there any difference between "scanSum (takeInt 10 [1..])" and "takeInt 10 (scanSum [1..])"?
# diffs returns a list of the differences between adjacent items.  So diffs [3,5,6,8] returns [2,1,2]. (Hint: write a second function that takes two lists and finds the difference between corresponding items).
}}

===head and tail===

As an additional note, Prelude (the standard library available by default to all Haskell programs) provides a wrapper for the "(x:xs)" pattern matching splitting of lists through the functions <tt>head</tt> and <tt>tail</tt>. Their definitions should look entirely unsurprising:

{{HaskellExample|1=<code>head</code> and <code>tail</code>|2=<source lang="haskell">
head             :: [a] -> a
head (x:_)       =  x
head []          =  error "Prelude.head: empty list"

tail             :: [a] -> [a]
tail (_:xs)      =  xs
tail []          =  error "Prelude.tail: empty list"
</source>}}

While at first it might look like there is not much need to resort to <tt>head</tt> and <tt>tail</tt> when you could just use pattern matching, the functions prove quite convenient, for instance, when doing function composition.

{{Exercises|1=
#The (!!) operator takes a list and an Int as arguments and returns the list element at the position given by the Int (starting from zero). Write a function which performs that task using head and tail.
#Write functions that when applied to lists give the ''last'' element of the list and the list with the last element dropped. That can be done with the usual pattern matching schemes or using head and tail only. We suggest you to try, and compare, both approaches. (Note that this functionality is provided by Prelude through the <tt>last</tt> and <tt>init</tt> functions.)
}}

== map ==

Because lists are such a fundamental data type in Haskell, there is a large collection of standard functions for list processing, most of them being in Prelude - and thus available by default (there are also additional list processing functions to be found in the <code>Data.List</code> module, but that needn't be an immediate concern of yours). We will close this module by discussing one particularly important function, called <code>map</code>. In the next module, [[Haskell/List processing|List processing]], will pick up from that point and describe some other key list processing tools.

Let us begin by recalling the <code>multiplyList</code> function from a couple of sections ago.

<source lang="haskell">
multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList _ [] = []
multiplyList m (n:ns) = (m*n) : multiplyList m ns
</source>

This works on a list of integers, multiplying each item by a constant.  But Haskell allows us to pass functions around just as easily as we can pass integers.  So instead of passing a multiplier <code>m</code> we could pass a function <code>f</code>, like this:

<source lang="haskell">
mapList1 :: (Integer -> Integer) -> [Integer] -> [Integer]
mapList1 _ [] = []
mapList1 f (n:ns) = (f n) : mapList1 f ns
</source>

Take a minute to compare the two functions.  The difference is in the first parameter.  Instead of being just an <code>Integer</code> it is now a function.  This function parameter has the type <code>(Integer -> Integer)</code>, meaning that it is a function from one integer to another.  The second line says that if this is applied to an empty list then the result is itself an empty list, and the third line says that for a non-empty list the result is <code>f</code> applied to the first item in the list, followed by a recursive call to <code>mapList1</code> for the rest of the list.

Remember that <code>(*)</code> has type <code>Integer -> Integer -> Integer</code>.  So if we write <code>(2*)</code> then this returns a new function that doubles its argument and has type <code>Integer -> Integer</code>.  But that is exactly the type of functions that can be passed to <code>mapList1</code>.  So now we can write <code>doubleList</code> like this:

<source lang="haskell">
doubleList = mapList1 (2*)
</source>

We could also write it like this, making all the arguments explicit:

<source lang="haskell">
doubleList ns = mapList1 (2*) ns
</source>

(The two are equivalent because if we pass just one argument to mapList1 we get back a new function.  The second version is more natural for newcomers to Haskell, but experts often favour the first, known as 'point free' style.)

Obviously this idea is not limited to just integers.  We could just as easily write

<source lang="haskell">
mapListString :: (String -> String) -> [String] -> [String]
mapListString _ [] = []
mapListString f (n:ns) = (f n) : mapListString f ns
</source>

and have a function that does this for strings.  But this is horribly wasteful: the code is exactly the same for both strings and integers.  What is needed is a way to say that <code>mapList</code> works for both Integers, Strings, and any other type we might want to put in a list.  In fact there is no reason why the input list should be the same type as the output list: we might very well want to convert a list of integers into a list of their string representations, or vice versa.  And indeed Haskell provides a way to do this.  The Standard Prelude contains the following definition of <code>map</code>:

<source lang="haskell">
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = (f x) : map f xs
</source>

Instead of constant types like <code>String</code> or <code>Integer</code>, this definition uses type variables.  These start with lower case letters (as opposed to type constants that start with upper case) and otherwise follow the same lexical rules as normal variables.  However, the convention is to start with <tt>a</tt> and go up the alphabet.  Even the most complicated functions rarely get beyond <tt>d</tt>.

So what this says is that <code>map</code> takes two parameters:
* A function from a thing of type <code>a</code> to a thing of type <code>b</code>.
* A list of things of type <code>a</code>.

Then, it returns a new list containing things of type <code>b</code>, constructed by applying the function to each element of the input list.

{{Exercises|1=
# Use <code>map</code> to build functions that, given a list l of Ints, return:
#* A list that is the element-wise negation of l.
#* A list of lists of Ints ll that, for each element of l, contains the factors of l.  It will help to know that<pre>factors p = [ f | f <- [1..p], p `mod` f == 0 ]</pre>
#* The element-wise negation of ll.
# Implement a Run Length Encoding (RLE) encoder and decoder.
#* The idea of RLE is simple; given some input:<pre>"aaaabbaaa"</pre> compress it by taking the length of each run of characters:<pre>(4,'a'), (2, 'b'), (3, 'a')</pre>
#* The <code>concat</code> and <code>group</code> functions might be helpful.  In order to use <code>group</code>, you will need to import the Data.List module.  You can access this by typing <code>:m Data.List</code> at the ghci prompt, or by adding <code>import Data.List</code> to your Haskell source code file.
#* What is the type of your <code>encode</code> and <code>decode</code> functions?
#* How would you convert the list of tuples (e.g. <code>[(4,'a'), (6,'b')]</code>) into a string (e.g. "4a6b")?
#* (bonus) Assuming numeric characters are forbidden in the original string, how would you parse that string back into a list of tuples?
}}

==Notes==

<references/>

{{Haskell navigation|chapter=Elementary Haskell}}

{{Auto category}}
