>﻿{{Haskell minitoc|chapter=Elementary Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Elementary Haskell}}

As promised, we'll pick up from where we stopped in the previous module and continue to explore list processing functions.  Conceptually, our main concern this time will be three classes of functions, namely folds, scans, and filters.

== Folds ==

A fold applies a function to a list in a way similar to <tt>map</tt>, but accumulates a single result instead of a list.

Take, for example, a function like <tt>sum</tt>, which might be implemented as follows:

{{HaskellExample|sum|<pre>
 sum :: [Integer] -> Integer
 sum []     = 0
 sum (x:xs) = x + sum xs</pre>}}

or <tt>product</tt>:

{{HaskellExample|product|<pre>
 product :: [Integer] -> Integer
 product []     = 1
 product (x:xs) = x * product xs</pre>}}

or <tt>concat</tt>, which takes a list of lists and joins (concatenates) them into one:

{{HaskellExample|concat|<pre>
 concat :: [[a]] -> [a]
 concat []     = []
 concat (x:xs) = x ++ concat xs</pre>}}

There is a certain pattern of recursion common to all of these examples. This pattern is known as a fold, possibly from the idea that a list is being "folded up" into a single value, or that a function is being "folded between" the elements of the list.

The Standard Prelude defines four <code>fold</code> functions: <code>foldr</code>, <code>foldl</code>, <code>foldr1</code> and <code>foldl1</code>.

=== foldr ===

The ''right-associative'' <code>foldr</code> folds up a list from the right, that is, it walks from the last to the first element of the list and applies the given function to each of the elements and the accumulator, the initial value of which has to be set:

 foldr            :: (a -> b -> b) -> b -> [a] -> b
 foldr f acc []     = acc
 foldr f acc (x:xs) = f x (foldr f acc xs)

The first argument is a function with two arguments, the second is a "zero" value for the accumulator, and the third is the list to be folded.

For example, in <code>sum</code>, <code>f</code> is <code>(+)</code>, and <code>acc</code> is <code>0</code>, and in <code>concat</code>, <code>f</code> is <code>(++)</code> and <code>acc</code> is <code>[]</code>. In many cases, like all of our examples so far, the function passed to a fold will have both its arguments be of the same type, but this is not necessarily the case in general.

What <tt>foldr f acc xs</tt> does is to replace each cons (:) in the list <code>xs</code> with the function <code>f</code>, and the empty list at the end with <code>acc</code>.  That is,

 a : b : c : []

becomes

 f a (f b (f c acc))

This is perhaps most elegantly seen by picturing the list data structure as a tree:
   :                         f
  / \                       / \
 a   :       foldr f acc   a   f
    / \    ------------->     / \
   b   :                     b   f
      / \                       / \
     c  []                     c   acc

It is fairly easy to see with this picture that <tt>foldr (:) []</tt> is just the identity function on lists (that is, the function which returns its argument unmodified).

=== foldl ===

The ''left-associative'' <tt>foldl</tt> processes the list in the opposite direction, starting at the left side with the first element, and proceeding to the last one step by step:

 foldl            :: (a -> b -> a) -> a -> [b] -> a
 foldl f acc []     =  acc
 foldl f acc (x:xs) =  foldl f (f acc x) xs

So brackets in the resulting expression accumulate on the left. Our list above, after being transformed by <tt>foldl f z</tt> becomes:
 
 f (f (f acc a) b) c

The corresponding trees look like:
   :                            f
  / \                          / \
 a   :       foldl f acc      f   c
    / \    ------------->    / \
   b   :                    f   b 
      / \                  / \
     c  []                acc a

{{body note|1=
''Technical Note'': The left associative fold is ''tail-recursive'', that is, it recurs immediately, calling itself. For this reason the compiler will optimise it to a simple loop, and it will then be much more efficient than <tt>foldr</tt>. However, Haskell is a lazy language, and so the calls to ''f'' will by default be left unevaluated, building up an expression in memory whose size is linear in the length of the list, exactly what we hoped to avoid in the first place. To get back this efficiency, there is a version of foldl which is ''strict'', that is, it forces the evaluation of ''f'' immediately, called <tt>foldl'</tt>.  Note the single quote character: this is pronounced "fold-ell-tick".  A tick is a valid character in Haskell identifiers.  foldl' can be found in the library <code>Data.List</code> (which can be imported by adding <tt>import Data.list</tt> to a source file).  As a rule of thumb you should use <tt>foldr</tt> on lists that might be infinite or where the fold is building up a data structure, and <tt>foldl'</tt> if the list is known to be finite and comes down to a single value. <code>foldl</code> (without the tick) should rarely be used at all, unless the list is not too long, or memory usage isn't a problem.
}}

''

=== foldr1 and foldl1 ===

As previously noted, the type declaration for <tt>foldr</tt> makes it quite possible for the list elements and result to be of different types.  For example, "read" is a function that takes a string and converts it into some type (the type system is smart enough to figure out which one).  In this case we convert it into a float.

{{HaskellExample|The list elements and results can have different types|
<pre>
 addStr :: String -> Float -> Float
 addStr str x = read str + x

 sumStr :: [String] -> Float
 sumStr = foldr addStr 0.0</pre>
}}

If you substitute the types <tt>Float</tt> and <tt>String</tt> for the type variables <code>a</code> and <code>b</code> in the type of foldr you will see that this is type correct.

There is also a variant called <tt>foldr1</tt> ("fold - arr - one") which dispenses with an explicit zero by taking the last element of the list instead:

 foldr1           :: (a -> a -> a) -> [a] -> a
 foldr1 f [x]     =  x
 foldr1 f (x:xs)  =  f x (foldr1 f xs)
 foldr1 _ []      =  error "Prelude.foldr1: empty list"

And <tt>foldl1</tt> as well:

 foldl1           :: (a -> a -> a) -> [a] -> a
 foldl1 f (x:xs)  =  foldl f x xs
 foldl1 _ []      =  error "Prelude.foldl1: empty list"

''Note: There is additionally a strict version of foldl1 called foldl1' in the Data.List library.''

Notice that in this case all the types have to be the same, and that an empty list is an error. These variants are occasionally useful, especially when there is no obvious candidate for ''z'', but you need to be sure that the list is not going to be empty. If in doubt, use foldr or foldl'.

=== folds and laziness ===

One good reason that right-associative folds are more natural to use in Haskell than left-associative ones is that right folds can operate on infinite lists, which are not so uncommon in Haskell programming.  If the input function f only needs its first parameter to produce the first part of the output, then everything works just fine. However, a left fold must call itself recursively until it reaches the end of the input list; this is inevitable, because the recursive call is not made in an argument to f. Needless to say, this never happens if the input list is infinite and the program will spin endlessly in an infinite loop.

As a toy example of how this can work, consider a function <code>echoes</code> taking a list of integers and producing a list where if the number n occurs in the input list, then n replicated n times will occur in the output list. We will make use of the prelude function <code>replicate</code>: <tt>replicate n x</tt> is a list of length n with x the value of every element.

We can write echoes as a foldr quite handily: 
 echoes = foldr (\x xs -> (replicate x x) ++ xs) []

(''Note:'' This is very compact thanks to the& <code>\x xs -></code>& syntax. Instead of defining a function somewhere else and passing it to foldr we provided the definition ''in situ''; <code>x</code> and <code>xs</code> being the arguments and the right-hand side of the definition being what is after the <code>-></code>)

or, equally handily, as a foldl:
 echoes = foldl (\xs x -> xs ++ (replicate x x)) []

but only the first definition works on an infinite list like [1..]. Try it! (If you try this in GHCi, remember you can stop an evaluation with Ctrl-c, but you have to be quick and keep an eye on the system monitor or your memory will be consumed in no time and your system will hang.)



As a final example, another thing that you might notice is that <code>map</code> itself is patterned as a fold:
 map f = foldr (\x xs -> f x : xs) []

Folding takes a little time to get used to, but it is a fundamental pattern in functional programming and eventually becomes very natural.  Any time you want to traverse a list and build up a result from its members, you likely want a fold.

{{Exercises|1=
Define the following functions recursively (like the definitions for <code>sum</code>, <code>product</code> and <code>concat</code> above), then turn them into a fold:
* <code>and :: [Bool] -> Bool</code>, which returns True if a list of Bools are all True, and False otherwise.
* <code>or :: [Bool] -> Bool</code>, which returns True if any of a list of Bools are True, and False otherwise.

Define the following functions using <code>foldl1</code> or <code>foldr1</code>:
* <code>maximum :: Ord a => [a] -> a</code>, which returns the maximum element of a list (hint: <code>max :: Ord a => a -> a -> a</code> returns the maximum of two values).
* <code>minimum :: Ord a => [a] -> a</code>, which returns the minimum element of a list (hint: <code>min :: Ord a => a -> a -> a</code> returns the minimum of two values).
}}

== Scans ==

A "scan" is much like a cross between a <tt>map</tt> and a fold.  Folding a list accumulates a single return value, whereas mapping puts each item through a function with no accumulation.  A scan does both: it accumulates a value like a fold, but instead of returning a final value it returns a list of all the intermediate values.

The Standard Prelude contains four scan functions:

 scanl   :: (a -> b -> a) -> a -> [b] -> [a] 	

This accumulates the list from the left, and the second argument becomes the first item in the resulting list.  So  <tt>scanl (+) 0 [1,2,3] = [0,1,3,6]</tt>

 scanl1  :: (a -> a -> a) -> [a] -> [a] 

This is the same as <tt>scanl</tt>, but uses the first item of the list as a zero parameter.  It is what you would typically use if the input and output items are the same type.  Notice the difference in the type signatures.  <tt>scanl1 (+) [1,2,3] = [1,3,6]</tt>.

 scanr   :: (a -> b -> b) -> b -> [a] -> [b] 	
 scanr1  :: (a -> a -> a) -> [a] -> [a] 	

These two functions are the exact counterparts of <tt>scanl</tt> and <tt>scanl1</tt>.  They accumulate the totals from the right.  So:

 scanr (+) 0 [1,2,3] = [6,5,3,0]
 scanr1 (+) [1,2,3] = [6,5,3]

{{Exercises|1=
# Write your own definition of <code>scanr</code>, first using recursion, and then using <code>foldr</code>. Do the same for <code>scanl</code> first using recursion then <code>foldl</code>.
# Define the following functions:
#*<code>factList :: Integer -> [Integer]</code>, which returns a list of factorials from 1 up to its argument.  For example, <code>factList 4 = [1,2,6,24]</code>.
''More to be added''}}

==filter==

A very common operation performed on lists is [[Wikipedia:Filter (mathematics)|filtering]], which is generating a new list composed only of elements of the first list that meet a certain condition. One simple example of that would be taking a list of integers and making from it a list which only retains its even numbers.

<source lang="haskell">
retainEven :: [Int] -> [Int]
retainEven [] = []
retainEven (n:ns) =
-- mod n 2 computes the remainder for the integer division of n by 2, so if it is zero the number is even
  if ((mod n 2) == 0)
    then n : (retainEven ns)
    else retainEven ns
</source>

That works fine, but it is a slightly verbose solution. It would be nice to have a more concise way to write the filter function. Also, it would certainly be very useful to be able to generalize the filtering operation - that is, make it capable of filtering a list using any boolean condition we'd like. To help us with both of these issues Prelude provides a <tt>filter</tt> function. <tt>filter</tt> has the following type signature:

<source lang="haskell">(a -> Bool) -> [a] -> [a]</source>

That means it evaluates to a list when given two arguments, namely an <tt>(a -> Bool)</tt> function which carries the actual test of the condition for the elements of the list and the list to be filtered. In order to write <tt>retainEven</tt> using <tt>filter</tt>, we need to state the condition as an auxiliary <tt>(a -> Bool)</tt> function, like this one:

<source lang="haskell">
isEven :: Int -> Bool 
isEven n = ((mod n 2) == 0) 
</source>

And then retainEven becomes simply:

<source lang="haskell">
retainEven ns = filter isEven ns
</source>

It can be made even more terse by writing it in point-free style:

<source lang="haskell">
retainEven = filter isEven
</source>

This is just like what we demonstrated before for <tt>map</tt> and the folds. Like <tt>filter</tt>, those take another function as argument; and using them point-free emphasizes this "functions-of-functions" aspect.

==List comprehensions==

An additional tool for list processing is the [[Wikipedia:List comprehension|list comprehension]], a powerful, concise and expressive syntactic construct. In their most elementary form, list comprehensions are [[Wikipedia:Syntactic sugar|syntactic sugar]] for filtering. So, instead of using the Prelude <tt>filter</tt>, we could write <tt>retainEven</tt> like this:

<source lang="haskell">
retainEven es = [ n | n <- es , isEven n ]
</source>

This compact syntax may look a bit intimidating at first, but it is simple to break down. One possible way to read it would be:

* (Starting from the middle) Take the list ''es'' and draw (the "<-") each of its elements as a value ''n''.
* (After the comma) For each drawn ''n'' test the boolean condition <code>isEven n</code>.
* (Before the vertical bar) If (and only if) the boolean condition is satisfied, prepend ''n'' to the new list being created (note the square brackets around the whole expression).

Thus if <code>es</code> is equal to [1,2,3,4], then we would get back the list [2,4]. 1 and 3 were not drawn because <code>(isEven n) == False </code>. 

The real power of list comprehensions, though, comes from the fact they are easily extensible. Firstly, we can use as many tests as we wish (even zero!). Multiple conditions are written as a comma-separated list of expressions (which should evaluate to a Boolean, of course). For a simple example, suppose we want to modify <tt>retainEven</tt> so that only numbers larger than 100 are retained:

<source lang="haskell">
retainLargeEvens :: [Int] -> [Int]
retainLargeEvens es = [ n | n <- es , isEven n, n > 100 ]
</source>

Writing this function with <tt>filter</tt> would require explicitly defining a new function to combine the conditions by a logical and <tt>(&&)</tt>. List comprehensions can provide a clearer syntax in such cases, particularly if there are several conditions to be tested. 

Another useful thing to know is that list comprehensions can be used for defining simple variables as well as functions. For instance:

<source lang="haskell">
someListOfEvens = [ n | n <- [1..10000] , isEven n ]
</source>

<tt>someListOfEvens</tt> will be [2,4..10000], as expected.

Furthermore, we are not limited to using <tt>n</tt> as the element to be prepended when generating a new list. Instead, we could place any expression before the vertical bar (if it is compatible with the type of the list, of course). For instance, if we we wanted to subtract one from every even number, all it would take is:

<source lang="haskell">
evensMinusOne es = [ n - 1 | n <- es , isEven n ]
</source>

In effect, that means the list comprehension syntax incorporates the functionality of <tt>map</tt> as well as of <tt>filter</tt>. Now ''that'' is conciseness! (and in a very readable way despite the terseness)

To further sweeten things, the left arrow notation in list comprehensions can be combined with pattern matching.  For example, suppose we had a list of <code>(Int, Int)</code> tuples and we would like to construct a list with the first element of every tuple whose second element is even. Using list comprehensions, we might write it as follows:

<source lang="haskell">
firstForEvenSeconds :: [(Int, Int)] -> [Int]
firstForEvenSeconds ps = [ fst p | p <- ps, isEven (snd p) ] -- here, p is for pairs.
</source>

Patterns (which can be used with tuples as well as with lists) can make what the function is doing more obvious:

<source lang="haskell">
firstForEvenSeconds ps = [ x | (x,y) <- ps, isEven y ]
</source>

As in other cases, arbitrary expressions may be used before the |. If we wanted a list with the double of those first elements:

<source lang="haskell">
doubleOfFirstForEvenSeconds :: [(Int, Int)] -> [Int]
doubleOfFirstForEvenSeconds ps = [ 2 * x | (x,y) <- ps, isEven y ]
</source>

Note that the function code is actually shorter than its descriptive name.

{{Exercises|1=
# Write a <code>returnDivisible :: Int -> [Int] -> [Int]</code> function which filters a list of integers retaining only the numbers divisible by the integer passed as first argument. For integers x and n, x is divisible by n if <code>(mod x n) == 0</code> (note that the test for evenness is a specific case of that)
#
#* Write - using list comprehension syntax, a single function definition and ''no'' <tt>if</tt>, <tt>case</tt> and similar constructs - a <code><nowiki>[[Int]] -> [[Int]]</nowiki></code> which, from a list of lists of <tt>Int</tt>, returns a list of the tails of those lists using, as filtering condition, that the head of each <tt>[Int]</tt> must be larger than 5. Also, your function must not trigger an error when it meets an empty <tt>[Int]</tt>, so you'll need to add an additional test to detect emptiness.
#* Does order matter when listing the conditions for list comprehension? (You can find it out by playing with the function you wrote for the first part of the exercise.) 
# Over this section we've seen how list comprehensions are essentially syntactic sugar for <tt>filter</tt> and <tt>map</tt>. Now work in the opposite direction and define alternative versions of the <tt>filter</tt> and <tt>map</tt> using the list comprehension syntax.
# Rewrite <tt>doubleOfFirstForEvenSeconds</tt> using <tt>filter</tt> and <tt>map</tt> instead of list comprehension.
}}


{{Haskell navigation|chapter=Elementary Haskell}}

{{Auto category}}
