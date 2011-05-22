>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Haskell Basics}}

''(All the examples in this chapter can be typed into a Haskell source file and evaluated by loading that file into GHC or Hugs. Remember not to type the prompts at the beginning of input. If there are prompts at the beginning, then you can type it in an environment like GHCi. If not, then you should put it in a file and run it.'')

== Variables ==

We've already seen how to use the GHCi program as a calculator. Of course, this is only practical for very short calculations. For longer calculations and for writing Haskell programs, we need to keep track of intermediate results.

Intermediate results can be stored in ''variables'', to which we refer by their name. A variable contains a ''value'', which is substituted for the variable name when you use a variable. For instance, consider the following calculation

{{HaskellGHCi|1=
ghci> 3.1416 * 5^2
78.53999999999999
}}

This is the area of a circle with radius <code>5</code>, according to the formula <math>A = \pi r^2</math>. It is very cumbersome to type in the digits of <math>\pi \approx 3.1416</math>, or even to remember them at all. In fact, an important aspect of programming, if not the whole point, is to delegate mindless repetition and rote memorization to a machine. In this case, Haskell has already ''defined'' a variable named <code>pi</code> that stores over a dozen digits of <math>\pi</math> for us.

{{HaskellGHCi|1=
ghci> pi
3.141592653589793
ghci> pi * 5^2
78.53981633974483
}}

Notice that the variable <code>pi</code> and its value, <code>3.141592653589793</code>, can be used interchangeably in calculations.

== Haskell source files ==

Now, we want to define our own variables to help us in our calculations. This is done in a ''Haskell source file'', which contains Haskell code.

Create a new file called <tt>Varfun.hs</tt> in your favorite [[w:text editor|text editor]] (the file extension <tt>.hs</tt> stands for "Haskell") and type/paste in the following definition:

<source lang = "haskell">
r = 5.0
</source>

Make sure that there are no spaces at the beginning of the line because Haskell is a whitespace sensitive language (more about indentation later).

Now, open GHCi, move to the directory (folder) where you saved your file with the <code>:cd YourDirectoryHere</code> command, and use the <code>:load YourFileHere</code> (or <code>:l YourFileHere</code>) command:

{{HaskellGHCi|1=
Prelude> :cd c:\myDirectory
Prelude> :load Varfun.hs
Compiling Main             ( Varfun.hs, interpreted )
Ok, modules loaded: Main.
*Main>
}}

Loading a Haskell source file will make all its definitions available in the GHCi prompt. Source files generally include ''modules'' (units of storage for code) to organize them or indicate where the program should start running (the <tt>Main</tt> module) when you use many files. In this case, because you did not indicate any <tt>Main</tt> module, it created one for you.

If GHCi gives an error like <code>Could not find module 'Varfun.hs'</code>, you probably are in the wrong directory.

Now,+ you can use the newly defined variable <code>r</code> in your calculations.

{{HaskellGHCi|1=
*Main> r
5
*Main> pi * r^2
78.53981633974483
}}

So, to calculate the area of a circle of radius 5, we simply define <code>r = 5.0</code> and then type in the well-known formula <math>\pi r^2</math> for the area of a circle. There is no need to write the numbers out every time; that's very convenient!

Since this was so much fun, let's add another definition: Change the contents of the source file to

<source lang = "haskell">
r = 5
area = pi * r ^ 2
</source>

Save the file and type the <code>:reload</code> (or <code>:r</code>) command in GHCi to load the new contents (note that this is a continuation of the last session):

{{HaskellGHCi|1=
*Main> :reload
Compiling Main             ( Varfun.hs, interpreted )
Ok, modules loaded: Main.
*Main>
}}

Now we have two variables <code>r</code> and <code>area</code> available

{{HaskellGHCi|1=
*Main> area
78.53981633974483
*Main> area / r
15.707963267948966
}}

{{body note|It is also possible to define variables directly at the GHCi prompt, without a source file. Skipping the details, the syntax for doing so uses the <code>let</code> ''keyword'' (a word with a special meaning) and looks like:
 <nowiki>Prelude> let area = pi * 5 ^ 2</nowiki>
Although we will occasionally do such definitions for expediency in the examples, it will quickly become obvious, as we move into slightly more complex tasks, that this practice is not really convenient. That is why we are emphasizing the use of source files from the very start.
}}

{{body note|''To experienced programmers:'' GHC can also be used as a compiler (that is, you could use GHC to convert your Haskell files into a stand-alone program that can be run without running the interpreter). How to do so will be explained much later.}}

== Comments ==
Before we continue, it is good to understand that it is possible to include text in a program without having it treated as code. This is achieved by use of ''comments''. In Haskell, a comment can be started with <code>--</code> and continues until the end of the line:

<source lang = "haskell">
x = 5     -- The variable x is 5.
y = 6     -- The variable y is 6.
-- z = 7
</source>

In this case, <code>x</code> and <code>y</code> are defined, but <code>z</code> is not. Comments can also go anywhere using the alternative syntax <code>{- ... -}</code>:

<source lang = "haskell">
x = {- Don't do this just because you can. -} 5
</source>

Comments are generally used for explaining parts of a program that may be somewhat confusing to readers otherwise. However, do not use them too much, like in the examples I gave, or they will serve to make a program illegible.

== Variables in imperative languages ==
If you are already familiar with imperative programming languages like C, you may have noticed that variables in Haskell are quite different from variables as you know them. We now explain why and how.

If you have no programming experience, skip this section and continue reading with [[#Functions|Functions]].

{{Side note|side=right|Variables do not vary}}

Unlike in imperative languages, variables in Haskell ''do not vary''. Once defined, their value never changes; they are ''immutable''. For instance, the following code does not work:

<source lang = "haskell">
r = 5
r = 2
</source>

because it means to define one thing twice, which is silly. The compiler will complain something about "multiple declarations of <code>r</code>". People more familiar with ''imperative programming'' (explicitly telling the computer what to do) may be accustomed to read this as first setting <code>r = 5</code> and then changing it to <code>r = 2</code>, but that's not how Haskell works. Variables in Haskell are more like abbreviations for long expressions, much like in a calculation on paper, than locations in a changing computer memory.

Another example that doesn't work this way is

<source lang = "haskell">
r = r + 1
</source>

Instead of "incrementing the variable <code>r</code>", this is actually a recursive definition of <code>r</code> in terms of itself (we will explain [[../Recursion|recursion]] in detail later on; just remember that this does something very different from what you might think).

Because you don't need to worry about changing values, the order in which variables are defined is irrelevant. For example, the following fragments of code do exactly the same thing:

{|border="0"
|-
||<source lang = "haskell">
 y = x * 2
 x = 3
</source>
||<source lang = "haskell">
 x = 3
 y = x * 2
</source>
|}

We can write things in any order that we want, there is no notion of "<code>x</code> being declared before <code>y</code>" or the other way around. This is also why you can't declare something more than once; it would be ambiguous otherwise. Of course, using <code>y</code> will still require a value for <code>x</code>, but this is unimportant until you need a specific numeric value.

By now, you probably look very incredulous and wonder how you can actually do anything at all in Haskell where variables don't change. But trust us; as we hope to show you in the rest of this book, you can write every program under the sun without ever changing a single variable! In fact, variables that don't change make life so much easier because it makes programs much more predictable. It's a key feature of purely functional programming, not a bug. If you think this is a good time to stop, recall that pure functional programming takes a very different approach from imperative programming and requires a different mindset.

== Functions ==
Now, suppose that we have multiple circles with different radii whose areas we want to calculate. For instance, to  calculate the area of another circle with radius 3, we would have to include new variables <code>r2</code> and <code>area2</code><ref>As you can see, the names of variables may also contain numbers. Variables ''must'' begin with a lowercase letter, but for the rest, any string consisting of letter, numbers, underscore (_) or tick (') is allowed.</ref> in our source file:

<source lang = "haskell">
r  = 5
area  = pi*r^2
r2 = 3
area2 = pi*r2^2
</source>

Clearly, this is unsatisfactory because we are repeating the formula for the area of a circle verbatim. To eliminate this mindless repetition, we would prefer to write it down only once and then apply it to different radii. That's exactly what ''functions'' allow us to do.

A ''function'' takes an ''argument'' value (or ''parameter'') and gives a result value, like a variable, that takes its place. (If you are already familiar with mathematical functions, they are essentially the same.) Defining functions in Haskell is simple: It is like defining a variable, except that we take note of the function argument that we put on the left hand side. For instance, the following is the definition of a function <code>area</code> which depends on a argument which we name <code>r</code>:

<source lang = "haskell">
area r = pi * r^2
</source>

The syntax here is important to note: the function name comes first, followed by the argument after a space (parentheses are not used). We can fit this pattern into the function definition, without any other special syntax required, just like a variable.

Now, we can plug in different values for the argument in these three ''calls'' to the function; load this definition into GHCi and try the following:

{{HaskellGHCi|1=
*Main> area 5
78.53981633974483
*Main> area 3
28.274333882308138
*Main> area 17
907.9202768874502
}}

As you can see, we can call this function with different radii to calculate their area of the corresponding circles.

You likely know functions from mathematics already. Our function here is defined mathematically as
:<math>A(r) = \pi \cdot r^2</math>
In mathematics, the parameter is enclosed between parentheses, and we would use it like <math>A(5) = 78.54</math> or <math>A(3) = 28.27</math>. In Haskell, these parentheses are omitted; the function name and the argument will be separated by just a space. Since Haskell is a ''functional'' language, we will call a lot of functions, so we had better use a very brief notation.

Parentheses are still used for grouping ''expressions'' (any code that gives a value) to be evaluated together, however. For example,

<source lang = "haskell">
area (5+3)
</source>

means to first calculate <code>(5+3)</code> (which is <code>8</code>, of course), and then calculate the area of that. However,

<source lang = "haskell">
area 5 + 3
</source>

means to calculate <code>area 5</code> first and add <code>3</code> to that. This shows that function calls take ''precedence'' over operators like <code>+</code> the same way multiplication is done before addition in mathematics.

=== Evaluation ===
Let us try to understand what exactly happens when you enter an expression into GHCi. After you press the enter key, GHCi will ''evaluate'' the expression you have given. This means that it will replace each function with its definition and calculate the results until a single value is left. For example, the evaluation of <code>area 5</code> proceeds as follows:

    area 5
 =>    { replace the left-hand side  area r = ...  by the right-hand side  ... = pi * r^2 }
    pi * 5^2
 =>    { replace  pi  by its numerical value }
    3.141592653589793 * 5^2
 =>    { apply exponentiation (^) }
    3.141592653589793 * 25
 =>    { apply multiplication (*) }
    78.53981633974483

As this shows, to ''apply'' or ''call'' a function means to replace the left-hand side of its definition by its right-hand side. As a last step, GHCi prints the final result on the screen.

Here are some more functions:

<source lang = "haskell">
double x    = 2*x
quadruple x = double (double x)
square x    = x*x
half   x     = x / 2
</source>

{{Exercises|1=
* Explain how GHCi evaluates <code>quadruple 5</code>.
* Define a function that subtracts 12 from half its argument.}}

=== Multiple parameters ===

Of course, functions can also have more than one argument. For example, here is a function for calculating the area of a rectangle given its length and its width:

<source lang = "haskell">
areaRect l w = l * w
</source>

{{HaskellGHCi|1=
*Main> areaRect 5 10
50
}}

Another example that calculates the area of a right triangle <math>\left(A = \frac{bh}{2}\right)</math>:

<source lang="haskell">
areaRightTriangle b h = (b * h) / 2
</source>

{{HaskellGHCi|1=
*Main> areaRightTriangle 3 9
13.5
}}

As you can see, multiple arguments are separated by spaces. That's also why you sometimes have to use parentheses to group expressions. For instance, to quadruple a value <code>x</code>, you can't write

<source lang = "haskell">
quadruple x = double double x
</source>

because that would mean to apply a function named <code>double</code> to the two arguments <code>double</code> and <code>x</code> because ''functions can be arguments to other functions'' (and you will see why later). Instead, you have to put parentheses around the argument:

<source lang = "haskell">
quadruple x = double (double x)
</source>

Arguments are always passed in the order given. For example:

<source lang = "haskell">
subtract x y = x - y
</source>

{{HaskellGHCi|1=
*Main> subtract 10 5
5
*Main> subtract 5 10
-5
}}

Here, <code>subtract 10 5</code> evaluates to <code>10 - 5</code>, but <code>subtract 5 10</code> evaluates to <code>5 - 10</code> because the order changes.

{{Exercises|1=
* Write a function to calculate the volume of a box.
* Approximately how many stones are the famous pyramids at Giza made up of? Use GHCi for your calculations.
}}

=== Remark on combining functions ===

It goes without saying that you can use functions that you have already defined to define new functions, just like you can use the predefined functions like addition <code>(+)</code> or multiplication <code>(*)</code> (operators are defined as functions in Haskell). For example, to calculate the area of a square, we can reuse our function that calculates the area of a rectangle

<source lang = "haskell">
areaRect l w = l * w
areaSquare s = areaRect s s
</source>

{{HaskellGHCi|1=
*Main> areaSquare 5
25
}}

After all, a square is just a rectangle with equal sides.

This principle may seem innocent enough, but it is really powerful, in particular when we start to calculate with other objects instead of numbers.

{{Exercises|1=
* Write a function to calculate the volume of a cylinder.
}}

== Local definitions ==
=== <code>where</code> clauses ===
When defining a function, it is not uncommon to define intermediate results that are ''local'' to the function. For instance, consider [[w:Heron's formula|Heron's formula]] <math>(A = \sqrt{s(s-a)(s-b)(s-c)})</math> for calculating the area of a triangle with sides <code>a</code>, <code>b</code>, and <code>c</code>:

<source lang = "haskell">
heron a b c = sqrt (s*(s-a)*(s-b)*(s-c))
    where
    s = (a+b+c) / 2
</source>

The variable <code>s</code> is half the perimeter of the triangle and it would be tedious to write it out four times in the argument of the square root function <code>sqrt</code>.

It would be wrong to just write the definitions in sequence

<source lang = "haskell">
heron a b c = sqrt (s*(s-a)*(s-b)*(s-c))  -- s is not defined here
s = (a+b+c) / 2                           -- a, b, and c are not defined here
</source>

because the variables <code>a</code>, <code>b</code>, <code>c</code> are only available in the right-hand side of the function <code>heron</code>, but the definition of <code>s</code> as written here is not part of the right-hand side of <code>heron</code>. To make it part of the right-hand side, we have to use the <code>where</code> keyword.

Note that both the <code>where</code> and the local definitions are ''indented'' by 4 spaces, to distinguish them from subsequent definitions. Here another example that shows a mix of local and top-level definitions:

<source lang = "haskell">
areaTriangleTrig  a b c = c * height / 2   -- use trigonometry
    where
    cosa   = (b^2 + c^2 - a^2) / (2*b*c)
    sina   = sqrt (1 - cosa^2)
    height = b*sina
areaTriangleHeron a b c = result           -- use Heron's formula
    where
    result = sqrt (s*(s-a)*(s-b)*(s-c))
    s      = (a+b+c)/2
</source>

=== Scope ===
If you look closely at the previous example, you'll notice that we have used the variable names <code>a</code>, <code>b</code>, <code>c</code> twice, once for each of the area functions. How does that work?

Fortunately, the following fragment of code does not contain any unpleasant surprises:

{{HaskellGHCi|1=
Prelude> let r = 0
Prelude> let area r = pi * r ^ 2
Prelude> area 5
78.53981633974483
}}

An "unpleasant surprise" here would have been getting <code>0</code> for the area because of the <code>let r = 0</code> definition getting in the way. That does not happen because when you defined <code>r</code> the second time you are talking about a ''different'' <code>r</code>. This is something that happens in real life as well. How many people do you know that have the name John? What's interesting about people named John is that most of the time, you can talk about "John" to your friends, and depending on the context, your friends will know which John you are referring to. Programming has something similar to context, called '''[[Wikipedia:Scope (programming)|scope]]'''. We won't explain the technicalities behind scope (at least not now), but Haskell's lexical scope is the magic that lets us define two different <code>r</code> and always get the right one back depending on context.

Thanks to scope, the value of a parameter is strictly what you pass in when you call the function. Informally, we could say the <code>r</code> in <code>let r = 0</code> is not the same <code>r</code> as the one inside our defined function <code>area</code> - the <code>r</code> inside <code>area</code> overrides the other <code>r</code>; you can think of it as Haskell picking the most specific version of <code>r</code> there is. If you have many friends all named John, you go with the one which just makes more sense and is specific to the context; similarly, what value of <code>r</code> we get depends on the scope.

== Summary ==

# Variables store values. In fact, they store any arbitrary Haskell expressions.
# Variables do not change.
# Functions help you write reusable code.
# Functions can accept more than one parameter.

We also learned that comments allow non-code text to be stored in a source file.

== Notes ==
<references/>

{{Haskell navigation|chapter=Haskell Basics}}

{{Auto category}}

[[pt:Haskell/Variáveis e funções]]
