>﻿{{Haskell minitoc|chapter=Elementary Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Elementary Haskell}}
As functions are absolutely essential to functional programming, there are some nice features you can use to make using functions easier.

== Private functions - let and where ==

Remember the <code>sumStr</code> function from the chapter on list processing.  It used another function called <code>addStr</code>:

<source lang="haskell">
addStr :: Float -> String -> Float
addStr x str = x + read str

sumStr :: [String] -> Float
sumStr = foldl addStr 0.0
</source>

So you could find that

<!-- use HaskellGHCi -->
<source lang="haskell">
   addStr 4.3 "23.7"
</source>

gives 28.0, and

<!-- use HaskellGHCi -->
<source lang="haskell">
   sumStr ["1.2", "4.3", "6.0"]
</source>

gives 11.5.

But maybe you don't want <code>addStr</code> cluttering up the top level of your program if it is only used as part of the <code>sumStr</code>.  Haskell lets you nest declarations in two subtly different ways: either using the <tt>let</tt> bindings we already know:

<source lang="haskell">
sumStr =
   let addStr x str = x + read str
   in foldl addStr 0.0
</source>

or with <tt>where</tt> clauses, which work in pretty much the same way but come ''after'' the expression which use the function.

<source lang="haskell">
sumStr = foldl addStr 0.0
   where addStr x str = x + read str
</source>

You can also use <tt>let</tt> and <tt>where</tt> in <tt>case</tt> expression and similar control structures, just as you can in functions:

<source lang="haskell">
describeColour c = 
   "This colour is "
   ++ case c of
          Black -> "black"
          White -> "white"
          RGB red green blue -> "the average of the components is " ++ show av
             where av = (red + green + blue) `div` 3
   ++ ", yeah?"
</source>

In this example, the indentation of the <tt>where</tt> clause sets the ''scope'' of the <tt>av</tt> variable so that it only exists as far as the <tt>RGB red green blue</tt> case is concerned. Placing it at the same indentation of the cases would make it available for all cases. Here is an example of such an usage with guards:

<source lang="haskell">
doStuff :: Int -> String
doStuff x
  | x < 3     = report "less than three"
  | otherwise = report "normal"
  where
    report y = "the input is " ++ y
</source>

== Anonymous Functions - lambdas ==

An alternative to creating a private named function like <code>addStr</code> is to create an anonymous function, also known as a <code>lambda function</code>.  For example, <code>sumStr</code> could have been defined like this:

<source lang="haskell">
sumStr = foldl (\x str -> x + read str) 0.0
</source>

The expression in the parentheses is a lambda function.  The backslash is used as the nearest ASCII equivalent to the Greek letter lambda (&lambda;). This example is a lambda function with two arguments, <code>x</code> and <code>str</code>, which evaluates to "x + read str". So, the <code>sumStr</code> presented just above is precisely the same as the one that used <code>addStr</code> in a let binding.

Lambdas are handy for writing one-off functions to be used with maps, folds and their siblings, especially where the function in question is simple.  The example above is about as complicated as you'll want them to be - cramming more verbose constructs like conditionals in such a compact syntactic form could get messy.

Since variables are being bound in a lambda expression (to the arguments, just like in a regular function definition), pattern matching can be used in them as well. A trivial example would be redefining <code>head</code> with a lambda:

<source lang="haskell">
head = (\(x:xs) -> x)
</source>

== Infix versus Prefix ==

As we noted previously, you can take an operator and turn it into a function by surrounding it in brackets:

<source lang="haskell">
2 + 4
(+) 2 4
</source>

This is called making the operator ''prefix'': you're using it before its arguments, so it's known as a prefix function. We can now formalise the term 'operator': it's a function which entirely consists of non-alphanumeric characters, and is used infix (normally). You can define your own operators just the same as functions; just don't use any alphanumeric characters. For example, here's the set-difference definition from Data.List:

<source lang="haskell">
(\\) :: Eq a => [a] -> [a] -> [a]
xs \\ ys = foldl (\zs y -> delete y zs) xs ys
</source>

Note that aside from just using operators infix, you can define them infix as well. This is a point that most newcomers to Haskell miss. I.e., although one could have written:

<source lang="haskell">
(\\) xs ys = foldl (\zs y -> delete y zs) xs ys
</source>

It's more common to define operators infix. However, do note that in type declarations, you have to surround the operators by parentheses.

You can use a variant on this parentheses style for 'sections':

<source lang="haskell">
(2+) 4
(+4) 2
</source>

These sections are functions in their own right. <code>(2+)</code> has the type <code>Int -> Int</code>, for example, and you can pass sections to other functions, e.g. <code>map (+2) [1..4] == [3..7]</code>.

If you have a (prefix) function, and want to use it as an operator, simply surround it by backticks:

<source lang="haskell">
1 `elem` [1..4]
</source>

This is called making the function ''infix'': you're using it in between its arguments. It's normally done for readability purposes: <code>1 `elem` [1..4]</code> reads better than <code>elem 1 [1..4]</code>. You can also define functions infix:

<source lang="haskell">
elem :: Eq a => a -> [a] -> Bool
x `elem` xs = any (==x) xs
</source>

But once again notice that in the type signature you have to use the prefix style.

Sections even work with infix functions:

<source lang="haskell">
(1 `elem`) [1..4]
(`elem` [1..4]) 1
</source>

You can only make binary functions (those that take two arguments) infix. Think about the functions you use, and see which ones would read better if you used them infix.

{{Exercises|1=
* Lambdas are a nice way to avoid defining unnecessary separate functions. Convert the following let- or where-bindings to lambdas:
** <code>map f xs where f x = x * 2 + 3</code>
** <code>let f x y = read x + y in foldr f 1 xs</code>
* Sections are just syntactic sugar for lambda operations. I.e. <code>(+2)</code> is equivalent to <code>\x -> x + 2</code>. What would the following sections 'desugar' to? What would be their types?
** <code>(4+)</code>
** <code>(1 `elem`)</code>
** <code>(`notElem` "abc")</code>
}}

{{Haskell navigation|chapter=Elementary Haskell}}

{{Auto category}}
