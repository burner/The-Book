>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Elementary Haskell}}

Over the previous modules of this book we occasionally mentioned pattern matching, pointing out some common situations in which it was involved without providing a clear definition. Now that you have some familiarity with the fundamental language constructs, it is a proper time for a more formal discussion of pattern matching. Let us start with a possible one-line definition:

''Pattern matching is a convenient way to '''bind''' variables to different '''parts''' of a given '''value'''.''

{{body note|1=
'' '''Pattern matching on what?''' ''

Some languages like Perl and Python use the term ''pattern matching'' for matching regular expressions against strings.  The pattern matching we are referring to in this chapter is something completely different.  In fact, you're probably best off forgetting what you know about pattern matching for now.<ref> If you accidentally came here looking for regex pattern matching, you might be interested in looking at the Haskell {{Haskell lib|package=regex-compat|Text|Regex}} library wrapper.</ref>  Here, pattern matching is used in the same way as in other ML-like languages <code>:</code> to deconstruct values according to their type specification.
}}


== What is pattern matching? ==
Actually, you have seen many examples of pattern matching already - it is virtually everywhere. For instance, pick a function definition like that of <code>map</code>:

<source lang="haskell">
map _ []     = []
map f (x:xs) = f x : map f xs
</source>

Here there are four different patterns involved, two per equation. Let's explore each one in turn:

* <code>f</code> is a pattern which matches ''anything at all'', and binds the <code>f</code> variable to that something.
* <code>(x:xs)</code> is a pattern that matches a ''non-empty list'' which is formed by something (which gets bound to the <code>x</code> variable) which was cons'd (by the function <code>(:)</code>) onto something else (which gets bound to <code>xs</code>). <code>xs</code> can be an empty list.
* <code>[]</code> is a pattern that matches ''the empty list''. It doesn't bind any variables.
* <code>_</code> is the pattern which matches anything at all, but doesn't do any binding.

So pattern matching is a way of:
* ''recognizing values''. For instance, when <code>map</code> is called and the second argument matches <code>[]</code> the first definition of <code>map</code> is used instead of the second one.
* ''binding variables'' to the recognized values. In this case, the variables <code>f</code>, <code>x</code> and <code>xs</code> are assigned to the values passed as arguments to map when the second definition is used. Not all patterns do binding - for example, <code>_</code> is used as a "whatever/don't care" wildcard exactly because it doesn't bind variables, and so it is not possible to refer to any value it matches.
* ''breaking down values into parts'', as the <code>(x:xs)</code> pattern does by assigning two variables to parts (head and tail) of a single argument (the list).

At this point, the process of using a function to break down a value by effectively undoing its effects may look a little bit too magical. It is not, however, a universally deployable technique. For example, one might think of, by analogy to how <code>(:)</code> is used to break down a list, define a function which uses <code>(++)</code> to chop off the first three elements of a list:

<source lang="haskell">
dropThree ([x,y,z] ++ xs) = xs
</source>

However, that ''will not work''. The function <code>(++)</code> isn't allowed in patterns, and in fact most other functions that act on lists wouldn't be allowed either. So which functions ''are'' allowed?

The one-word answer is ''constructors'' - the functions used to build values of algebraic data types. Let us consider a generic example:

<source lang="haskell">
data Foo = Bar | Baz Int
</source>

Here <code>Bar</code> and <code>Baz</code> are constructors for the type <code>Foo</code>. And so you can use them for pattern matching <code>Foo</code> values, and bind variables to the <code>Int</code> value contained in a <code>Foo</code> constructed with <code>Baz</code>:

<source lang="haskell">
f :: Foo -> Int
f Bar     = 1
f (Baz x) = x - 1
</source>

That was exactly what was going on back when we defined <code>showAnniversary</code> and <code>showDate</code> in the Type declarations module. For instance:

<source lang="haskell">
data Date = Date Int Int Int   -- Year, Month, Day
showDate :: Date -> String
showDate (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d
</source>

The <code>(Date y m d)</code> pattern in the left-hand side of the <code>showDate</code> definition matches a <code>Date</code> (built with the <code>Date</code> constructor) and binds the variables <code>y</code>, <code>m</code> and <code>d</code> to the contents of the <code>Date</code> value.

As for lists, they are not different from <code>data</code>-defined algebraic data types as far as pattern matching is concerned. In effect, it's like lists were defined with this <code>data</code> declaration (note that the following isn't actually valid syntax: lists are in reality deeply ingrained into Haskell):

<source lang="haskell">
data [a] = [] | a : [a]
</source>

So the empty list, <code>[]</code>, and the <code>(:)</code> function, are in reality constructors of the list datatype, so you can pattern match with them. <code>[]</code> takes no arguments, and therefore no variables are bound when it is used for pattern matching. <code>(:)</code> takes two arguments, the list head and tail, which can then be bound to variables when the pattern is recognized. 

Furthermore, since <code>[x, y, z]</code> is just syntactic sugar for <code>x:y:z:[]</code>, there is a pattern matching solution for the <code>dropThree</code> problem above:

<source lang="haskell">
dropThree (_:_:_:xs) = xs
</source>

=== Introduction to records ===

For constructors with many elements, ''records'' provide useful syntactical assistance. Briefly, they are a way of naming values in a datatype, using the following syntax:

<source lang="haskell">
data Foo2 = Bar2 | Baz2 {barNumber::Int, barName::String}
</source>

Using records allows doing matching and binding only for the variables relevant to the function we're writing, making code much clearer:

<source lang="haskell">
h :: Foo2 -> Int
h Baz2 {barName=name} = length name
h Bar2 {} = 0
</source>

Also, the <code>{}</code> pattern can be used for matching a constructor regardless of the datatype elements even if you don't use records in the <code>data</code> declaration:

<source lang="haskell">
data Foo = Bar | Baz Int
g :: Foo -> Bool
g Bar {} = True
g Baz {} = False
</source>

The function <code>g</code> does not have to be changed if we modify the number or the type of elements of the constructors <code>Bar</code> or <code>Baz</code>.

The record syntax will be covered with more detail later in the book.

=== As-patterns ===

Sometimes, when matching a pattern with a value, it may be useful to bind a name also to the whole value being matched. As-patterns allow exactly this: they are of the form ''<code>var@pattern</code>'' and have the additional effect to bind the name <code>var</code> to the whole value being matched by <code>pattern</code>.

For instance, the following code snippet:

<source lang="haskell">
other_map f [] = []
other_map f list@(x:xs) = f x list : other_map f xs
</source>

creates a variant of map, called <code>other_map</code>, which passes to the parameter function <code>f</code> also the original list. Writing it without as-patterns would have been more cumbersome, because you must ''recreate'' the original value, i.e. <code>x:xs</code>:

<source lang="haskell">
other_map f [] = []
other_map f (x:xs) = f x (x:xs) : other_map f xs
</source>

The difference would be more notable with a more complex pattern.

=== Literal values ===

A simple piece-wise function definition like this one

<source lang="haskell">
f :: Int -> Int
f 0 = 1
f 1 = 5
f 2 = 2
f _ = -1
</source>

is performing pattern matching as well, matching the argument of <code>f</code> with the <code>Int</code> literals 0, 1 and 2. In general, numeric and character literals can be used in pattern matching. They can also be used together with constructors. For instance, this function

<source lang="haskell">
g :: [Int] -> Bool
g (0:[]) = False
g (0:xs) = True
g _ = False
</source>

will evaluate to False for the [0] list, to True if the list has 0 as first element and a non-empty tail and to False in all other cases. Also, lists with literal elements like [1,2,3], or even "abc" (which is equivalent to ['a','b','c']) can be used for pattern matching as well, since these forms are only syntactic sugar for the (:) constructor. 

It costs nothing to emphasize that the above considerations are only valid for literal values, so the following will '''not''' work:

<source lang="haskell">
k = 1
--again, this won't work as expected
h :: Int -> Bool
h k = True
h _ = False
</source>

{{Exercises|1=
Test the flawed <code>h</code> function above in GHCi, with arguments equal to and different from 1. Then, try to explain what goes wrong. (Hint: re-read the detailed description of the pattern matching used in the <code>map</code> definition at the start of the module.)
}}

=== n+k patterns ===

There is one exception to the rule that you can only pattern match with constructor functions. It is known as <code>n+k</code> patterns, which make it valid Haskell 98 to write something like:

<source lang="haskell">
pred :: Int -> Int
pred (n+1) = n
</source>

However, this exception is generally considered unsightly and has now been removed in the Haskell 2010 standard.

== Where you can use it == 

The short answer is that ''wherever you can bind variables, you can pattern match''. Let's have a look at that more precisely.

=== Equations ===

The first place is in the left-hand side of function definition equations, which were the subject of our examples so far.

<source lang="haskell">
map _ []     = []
map f (x:xs) = f x : map f xs
</source>

In the <code>map</code> definition we're binding, and doing pattern-matching, on the left hand side of both of these equations.

=== Let expressions ===
Let expressions are a way of doing local variable bindings. As such, you can also use pattern matching in them. A simple example:

<source lang="haskell">
y =
  let 
    (x:xs) = map (2*) [1,2,3]
  in x + 5
</source>

Here, <code>x</code> will be bound to the first element of <code>map (2*) [1,2,3]</code>. <code>y</code>, therefore, will evaluate to <math>2 + 5 = 7</math>.

=== Case expressions ===
Another typical usage of pattern binding is on the left hand side of case branches:

<source lang="haskell">
describeList :: [a] -> String
describeList list = 
  case list of
   []     -> "The list was empty"
   (x:xs) -> "The list wasn't empty: the first element was " ++ show x ++ ", and " ++
             "there were " ++ show (length xs) ++ " more elements in the list."
</source>

=== List comprehensions ===
After the <code>|</code> in list comprehensions you can pattern match. This is actually extremely useful, and adds a lot to the expressiveness of comprehensions. Let's see how that works with a slightly more sophisticated example. Prelude provides a <code>Maybe</code> type which has the following constructors:

<source lang="haskell">
data Maybe a = Nothing | Just a
</source>

It is typically used to hold values resulting from an operation which may or may not succeed, such as a lookup in a list (if the operation succeeds, the Just constructor is used and the value is passed to it; otherwise Nothing is used). The utility function <code>catMaybes</code> (which is available from Data.Maybe library module) takes a list of Maybes (which may contain both "Just" and "Nothing" Maybes), and retrieves the contained values by filtering all the <code>Just x</code> and getting rid of the <code>Just</code> wrappers. Writing it with list comprehensions is very straightforward:

<source lang="haskell">
catMaybes :: [Maybe a] -> [a]
catMaybes ms = [ x | Just x <- ms ]
</source>

Another nice thing about using a list comprehension for this task is that if the pattern match fails (that is, it meets a Nothing) it just moves on to the next element in <code>ms</code>,<ref>''Advanced note for the curious:'' More formally, as list comprehensions are just the list monad, a failed pattern match invokes <code>fail</code>, which is the empty list in this case, and so gets ignored.</ref> thus avoiding the need of explicitly handling each constructor with alternate function definitions.

=== Other places ===
There are a few other situations in which patterns can be used that we'll meet later on. Here's a list in case you're very eager already:

* <code>where</code> clauses, which are an alternative to <code>let</code> bindings;
* lambdas, which are anonymous function definitions;
* In <code>p <- x</code> assignments in do-blocks, <code>p</code> can be a pattern.
* Similarly, with let bindings in do-blocks, you can pattern match analogously to 'real' let bindings.

== Notes ==
<references/>

{{Haskell navigation|chapter=Elementary Haskell}}
{{Auto category}}
