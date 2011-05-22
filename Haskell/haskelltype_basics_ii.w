>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Haskell Basics|noexercises=1}}
Up to now we have shrewdly avoided number types in our examples. In one exercise, we even went as far as asking you to "pretend" the arguments to <tt>(+)</tt> had to be an <tt>Int</tt>. So what are we hiding from?

The main theme of this module will be how numerical types are handled in Haskell. While doing so, we will introduce some important features of the type system. Before diving into the text, though, pause for a moment and consider the following question: what should be the type of the function <tt>(+)</tt> ?<ref>If you followed our recommendations in "Type basics", chances are you have already seen the rather exotic answer by testing with <tt>:t</tt>... if that is the case, consider the following analysis as a path to understanding the meaning of that signature.</ref>

== The <tt>Num</tt> class ==

As far as everyday Mathematics is concerned, there are very few restrictions on which kind of numbers we can add together. <math>2 + 3</math> (two natural numbers), <math>(-7) + 5.12</math> (a negative integer and a rational number), <math>\frac{1}{7} + \pi</math> (a rational and an irrational)... all of these are valid - indeed, any two real numbers can be added together. In order to capture such generality in the simplest way possible we would like to have a very general <tt>Number</tt> type in Haskell, so that the signature of <tt>(+)</tt> would be simply

<source lang = "haskell">
(+) :: Number -> Number -> Number
</source>

That design, however, does not fit well with the way computers perform arithmetic. While integer numbers in programs can be quite straightforwardly handled as sequences of binary digits in memory, that approach does not work for non-integer real numbers<ref>One of the reasons being that between any two real numbers there are infinite real numbers - and that can't be directly mapped into a representation in memory no matter what we do.</ref>, thus making it necessary for a more involved encoding to support them: [[w:Floating point|floating point numbers]]. While floating point provides a reasonable way to deal with real numbers in general, it has some inconveniences (most notably, loss of precision) which make it worthy to keep using the simpler encoding for integer values. We are thus left with at least two different ways of storing numbers, one for integers and another one for general real numbers, which should correspond to different Haskell types. Furthermore, computers are only able to perform operations like <tt>(+)</tt> on a pair of numbers if they are in the same format. That should put an end to our hopes of using a universal <tt>Number</tt> type - or even having <tt>(+)</tt> working with both integers and floating-point numbers...

It is easy, however, to see reality is not that bad. We ''can'' use <tt>(+)</tt> with both integers and floating point numbers:

{{HaskellGHCi|1=
Prelude>3 + 4
7
Prelude>4.34 + 3.12
7.46
}}

When discussing lists and tuples, we saw that functions can accept arguments of different types if they are made ''polymorphic''. In that spirit, one possible type signature for <tt>(+)</tt> that would account for the facts above would be:

<source lang = "haskell">
(+) :: a -> a -> a
</source>

<tt>(+)</tt> would then take two arguments of the same type <tt>a</tt> (which could be integers or floating-point numbers) and evaluate to a result of type <tt>a</tt>. There is a problem with that solution, however. As we saw before, the type variable <tt>a</tt> can stand for ''any'' type at all. If <tt>(+)</tt> really had that type signature we would be able to add up two <tt>Bool</tt>, or two <tt>Char</tt>, which would make no sense - and is indeed impossible. Rather, the actual type signature of <tt>(+)</tt> takes advantage of a language feature that allows us to express the semantic restriction that <tt>a</tt> can be any type ''as long as it is a number type'':

<source lang = "haskell">
(+) :: (Num a) => a -> a -> a
</source>

<tt>Num</tt> is a '''typeclass''' - a group of types which includes all types which are regarded as numbers<ref>That is a very loose definition, but will suffice until we are ready to discuss typeclasses in more detail.</ref>. The <tt>(Num a) =></tt> part of the signature restricts <tt>a</tt> to number types - or, more accurately, ''instances'' of <tt>Num</tt>.

== Numeric types ==

But what are the ''actual'' number types - the instances of <tt>Num</tt> that <tt>a</tt> stands for in the signature? The most important numeric types are
<tt>Int</tt>, <tt>Integer</tt> and <tt>Double</tt>:

* <tt>Int</tt> corresponds to the vanilla integer type found in most languages. It has fixed precision, and thus maximum and minimum values (in 32-bit machines the range goes from -2147483648 to 2147483647).

* <tt>Integer</tt> also is used for integer numbers, but unlike <tt>Int</tt> it supports arbitrarily large values - at the cost of some efficiency.

* <tt>Double</tt> is the double-precision floating point type, and what you will want to use for real numbers in the overwhelming majority of cases (there is also <tt>Float</tt>, the single-precision counterpart of <tt>Double</tt>, which in general is not an attractive option due to the loss of precision).

These types are available by default in Haskell, and are the ones you will generally deal with in everyday tasks.

=== Polymorphic guesswork ===

There is one thing we haven't explained yet, though. If you tried the examples of addition we mentioned at the beginning you know that something like this is perfectly valid:

{{HaskellGHCi|1=
Prelude> (-7) + 5.12
-1.88
}}

Here, it seems we are adding two numbers of different types - an integer and a non-integer. Shouldn't the type of <tt>(+)</tt> make that impossible?

To answer that question we have to see what the types of the numbers we entered actually are:

{{HaskellGHCi|1=
Prelude> :t (-7)
(-7) :: (Num a) => a
}}

And, lo and behold, <tt>(-7)</tt> is neither <tt>Int</tt> or <tt>Integer</tt>! Rather, it is a ''polymorphic constant'', which can "morph" into any number type if need be. The reason for that becomes clearer when we look at the other number...

{{HaskellGHCi|1=
Prelude> :t 5.12
5.12 :: (Fractional t) => t
}}

<tt>5.12</tt> is also a polymorphic constant, but one of the <tt>Fractional</tt> class, which is more restrictive than <tt>Num</tt> - every <tt>Fractional</tt> is a <tt>Num</tt>, but not every <tt>Num</tt> is a <tt>Fractional</tt> (for instance, <tt>Int</tt>s and <tt>Integer</tt>s are not).

When a Haskell program evaluates <tt>(-7) + 5.12</tt>, it must settle for an actual type for the numbers. It does so by performing type inference while accounting for the class specifications. <tt>(-7)</tt> can be any <tt>Num</tt>, but there are extra restrictions for <tt>5.12</tt>, so its type will define what <tt>(-7)</tt> will become. Since there is no other clues to what the types should be, <tt>5.12</tt> will assume the default <tt>Fractional</tt> type, which is <tt>Double</tt>; and, consequently, <tt>(-7)</tt> will become a <tt>Double</tt> as well, allowing the addition to proceed normally and return a <tt>Double</tt><ref>''For seasoned programmers:'' This appears to have the same effect of what programs in C (and many other languages) would manage with an ''implicit cast'' - in which case the integer literal would be silently converted to a double. The difference is that in C the conversion is done behind your back, while in Haskell it only occurs if the variable/literal is explicitly made a polymorphic constant. The difference will become clearer shortly, when we show a counter-example.</ref>.

There is a nice quick test you can do to get a better feel of that process. In a source file, define

<source lang = "haskell">
x = 2
</source>

then load the file in GHCi and check the type of <tt>x</tt>. Then, add an <tt>y</tt> variable

<source lang = "haskell">
x = 2
y = x + 3
</source>

and check the types of <tt>x</tt> and <tt>y</tt>. Finally, modify <tt>y</tt> to

<source lang = "haskell">
x = 2
y = x + 3.1
</source>

and see what happens with the types of both variables.

=== Monomorphic trouble ===

The sophistication of the numerical types and classes occasionally leads to some complications. Consider, for instance, the common division operator <tt>(/)</tt>. It has the following type signature:

<source lang = "haskell">
(/) :: (Fractional a) => a -> a -> a
</source>

Restricting <tt>a</tt> to fractional types is a must because the division of two integer numbers in general will not result in an integer. Nevertheless, we can still write something like

{{HaskellGHCi|1=
Prelude> 4 / 3
1.3333333333333333
}}

because the literals <tt>4</tt> and <tt>3</tt> are polymorphic constants and therefore assume the type <tt>Double</tt> at the behest of <tt>(/)</tt>. Suppose, however, we want to divide a number by the length of a list<ref>A reasonable scenario - think of computing an average of the values in a list.</ref>. The obvious thing to do would be using the <tt>length</tt> function:

{{HaskellGHCi|1=
Prelude> 4 / length [1,2,3]
}}

Unfortunately, that blows up:

{{HaskellGHCi|1=
<interactive>:1:0:
    No instance for (Fractional Int)
      arising from a use of `/' at <interactive>:1:0-17
    Possible fix: add an instance declaration for (Fractional Int)
    In the expression: 4 / length [1, 2, 3]
    In the definition of `it': it = 4 / length [1, 2, 3]
}}

As usual, the problem can be understood by looking at the type signature of <tt>length</tt>:

<source lang = "haskell">
length :: [a] -> Int
</source>

The result of <tt>length</tt> is not a polymorphic constant, but an <tt>Int</tt>; and since an <tt>Int</tt> is not a <tt>Fractional</tt> it can't fit the signature of <tt>(/)</tt>.

There is a handy function which provides a way of escaping from this problem. Before following on with the text, try to guess what it does only from the name and signature:

<source lang = "haskell">
fromIntegral :: (Integral a, Num b) => a -> b
</source>

<tt>fromIntegral</tt> takes an argument of some <tt>Integral</tt> type (like <tt>Int</tt> or <tt>Integer</tt>) and makes it a polymorphic constant. By combining it with <tt>length</tt> we can make the length of the list fit into the signature of <tt>(/)</tt>:

{{HaskellGHCi|1=
Prelude> 4 / fromIntegral (length [1,2,3])
1.3333333333333333
}}

While this complication may look spurious at first, this way of doing things makes it easier to be rigorous when manipulating numbers. If you define a function that takes an <tt>Int</tt> argument you can be entirely sure that it will never be converted to an <tt>Integer</tt> or a <tt>Double</tt> unless you explicitly tell the program to do so (for instance, by using <tt>fromIntegral</tt>). As a direct consequence of the refinement of the type system, there is a surprising diversity of classes and functions for dealing with numbers in Haskell<ref>A more descriptive overview of the main ones will be made in a module which is currently under development.</ref>.

== Classes beyond numbers ==

There are many other use cases for typeclasses beyond arithmetic. For example, the type signature of <tt>(==)</tt> is:

<source lang = "haskell">
(==) :: (Eq a) => a -> a -> Bool
</source>

Like <tt>(+)</tt> or <tt>(/)</tt>, <tt>(==)</tt> is a polymorphic function. It compares two values of the same type, which must belong to the class <tt>Eq</tt>, and returns a <tt>Bool</tt>. <tt>Eq</tt> is simply the class of types of values which can be compared for equality, and includes all of the basic non-functional types.

Typeclasses are a very general language feature which adds a lot to the power of the type system. Later in the book we will return to this topic to see how to use them in custom ways, which will allow you to appreciate their usefulness in its fullest extent.

== Notes ==

<references/>

{{Haskell navigation|chapter=Haskell Basics|noexercises=1}}

{{BookCat}}
