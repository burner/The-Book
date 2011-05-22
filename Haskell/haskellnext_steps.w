>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Haskell Basics|noexercises=1}}

Working with actual source code files instead of typing things into the interpreter makes it convenient to define much more substantial functions than those we've seen up to now. Let's flex some Haskell muscle here and examine the kinds of things we can do with our functions.

== Conditional expressions ==

=== if / then / else ===

Haskell supports standard conditional expressions. For instance, we could define a function that returns <math>-1</math> if its argument is less than <math>0</math>; <math>0</math> if its argument ''is'' <math>0</math>; and <math>1</math> if its argument is greater than <math>0</math>. Actually, such a function already exists (called the <code>signum</code> function), but let's define one of our own, what we'll call <code>mySignum</code>.

{{HaskellExample|The signum function.|
<source lang="haskell">
mySignum x =
    if x < 0 then 
        -1
    else if x > 0 then 
        1
    else 
        0
</source>
You can experiment with this as:

{{HaskellGHCi|1=
*Main> mySignum 5
1
*Main> mySignum 0
0
*Main> mySignum (5-10)
-1
*Main> mySignum (-1)
-1
}}}}

Note that the parenthesis around "-1" in the last example are required; if missing, the system will think you are trying to subtract the value "1" from the value "mySignum", which is ill-typed.

The if/then/else construct in Haskell looks very similar to that of most other programming languages; however, you must have ''both'' a <tt>'''then'''</tt> ''and'' an <tt>'''else'''</tt> clause because it must result in a value (everything is an expression). It evaluates the condition (in this case <code>x < 0</code>) and, if this evaluates to <code>True</code>, it evaluates the <tt>'''then'''</tt> expression; if the condition evaluated to <code>False</code>, it evaluates the <tt>'''else'''</tt> expression.

You can test this program by editing the file and loading it back into your interpreter. Instead of typing <tt>:l Varfun.hs</tt> again, you can simply type <tt>:reload</tt> or just <tt>:r</tt> to reload the current file. This is usually much faster.

=== case ===
Haskell, like many other languages, also supports <tt>'''case'''</tt> constructions. These are used when there are multiple values that you want to check against (case expressions are actually quite a bit more powerful than this -- see the [[../Pattern matching/]] chapter for all of the details).

Suppose we wanted to define a function that had a value of <math>1</math> if its argument were <math>0</math>; a value of <math>5</math> if its argument were <math>1</math>; a value of <math>2</math> if its argument were <math>2</math>; and a value of <math>-1</math> in all other instances. Writing this function using <tt>'''if'''</tt> statements would be long and very unreadable; so we write it using a <tt>'''case'''</tt> statement as follows (we call this function <code>f</code>):

{{HaskellExample|1=|2=<source lang="haskell">
f x =
    case x of
      0 -> 1
      1 -> 5
      2 -> 2
      _ -> -1
</source>}}

In this program, we're defining <code>f</code> to take an argument <code>x</code> and then inspecting the value of <code>x</code>. If it matches <math>0</math>, the value of <code>f</code> is <math>1</math>. If it matches <math>1</math>, the value of <code>f</code> is <math>5</math>. If it matches <math>2</math>, then the value of <code>f</code> is <math>2</math>; and if it hasn't matched anything by that point, the value of <code>f</code> is <math>-1</math> (the underscore can be thought of as a "wildcard" -- it will match anything).

The indentation here is important. Haskell uses a system called "layout" to structure its code (the programming language Python uses a similar system). The layout system allows you to write code without the explicit semicolons and braces that other languages like C and Java require.

{{warning|1=
Because whitespace matters in Haskell, you need to be careful about whether you are using tabs or spaces. If you can configure your editor to never use tabs, that's probably better. If not, make sure your tabs are always 8 spaces long, or you're likely to run into problems.
}}

== Indentation ==

The general rule for layout is that an opening ''brace'' (the <tt>{</tt> character) is inserted after the keywords <tt>'''where'''</tt>, <tt>'''let'''</tt>, <tt>'''do'''</tt> and <tt>'''of'''</tt>, and the column position at which the next command appears is remembered. From then on, a semicolon is inserted before every new line that is indented the same amount. If a following line is indented less, a closing brace (<tt>}</tt>) is inserted. This may sound complicated, but if you follow the general rule of indenting after each of those keywords, you'll never have to remember it (see the [[../Indentation/]] chapter for a more complete discussion of layout).

Some people prefer not to use layout and write the braces and semicolons explicitly. This is perfectly acceptable. In this style, the above function might look like:

<source lang="haskell">
f x = case x of
        { 0 -> 1 ; 1 -> 5 ; 2 -> 2 ; _ -> -1 }
</source>
Of course, if you write the braces and semicolons explicitly, you're free to structure the code as you wish. The following is also equally valid:

<source lang="haskell">
f x =
    case x of { 0 -> 1 ;
      1 -> 5 ; 2 -> 2
   ; _ -> -1 }
</source>
However, structuring your code like this only serves to make it unreadable (in this case).

== Defining one function for different parameters ==

Functions can also be defined piece-wise, meaning that you can write one version of your function for certain parameters and then another version for other parameters. For instance, the above function <code>f</code> could also be written as:

{{HaskellExample|1=|2=<source lang="haskell">
f 0 = 1
f 1 = 5
f 2 = 2
f _ = -1
</source>}}

Just like in the <code>case</code> situation above, order is important. If we had put the last line first, it would have matched every argument, and <code>f</code> would return <code>-1</code>, regardless of its argument (most compilers will warn you about this, though, saying something about overlapping patterns). If we had not included this last line, <code>f</code> would produce an error if anything other than 0, 1 or 2 were applied to it (most compilers will warn you about this, too, saying something about incomplete patterns). This style of piece-wise definition is very popular and will be used quite frequently throughout this tutorial. These two definitions of <code>f</code> are actually equivalent -- this piece-wise version is translated into the case expression.

== Function composition ==

More complicated functions can be built from simpler functions using ''function composition''. Function composition is simply taking the result of the application of one function and using that as an argument for another. We've already seen this back in the [[../Getting set up/]] chapter, when we wrote <code>5*4+3</code>. In this, we were evaluating <math>5 \cdot 4</math> and then applying <math>+3</math> to the result. We can do the same thing with our <code>square</code> and <code>f</code> functions:

{{HaskellExample||
<source lang="haskell">
square x = x^2
</source>

{{HaskellGHCi|1=
*Main> square (f 1)
25
*Main> square (f 2)
4
*Main> f (square 1)
5
*Main> f (square 2)
-1
}}}}

The result of each of these function applications is fairly straightforward. The parentheses around the inner function are necessary; otherwise, in the first line, the interpreter would think that you were trying to get the value of ''<code>square f</code>'', which has no meaning. Function application like this is fairly standard in most programming languages.

There is another, more mathematical way to express function composition: the (<code>.</code>) enclosed period function. This (<code>.</code>) function is modeled after the (<math>\circ</math>) operator in mathematics.

{{body note|1=
In mathematics we write <math>f \circ g</math> to mean "f following g."  In Haskell , we write <code>f . g</code> also to mean "f following g."

The meaning of <math>f \circ g</math> is simply that <math>(f \circ g)(x) = f(g(x))</math>. That is, applying the function <math>f \circ g</math> to the value <math>x</math> is the same as applying <math>g</math> to <math>x</math>, taking the result, and then applying <math>f</math> to that.
}}

The (<code>.</code>) function (called the function composition function), takes two functions and makes them into one. For instance, if we write <code>(square . f)</code>, this means that it creates a new function that takes an argument, applies <code>f</code> to that argument and then applies <code>square</code> to the result. Conversely, <code>(f . square)</code> means that a new function is created that takes an argument, applies <code>square</code> to that argument and then applies <code>f</code> to the result.  We can see this by testing it as before:

{{HaskellGHCiExample|1=|2=
*Main> (square . f) 1
25
*Main> (square . f) 2
4
*Main> (f . square) 1
5
*Main> (f . square) 2
-1
}}

Here, we must enclose the function composition in parentheses; otherwise, the Haskell compiler will think we're trying to compose <code>square</code> with the value <code>f 1</code> in the first line, which makes no sense since <code>f 1</code> isn't even a function.

{{body note|1=
At this point, it would be wise to be aware of ''Prelude'', which is a set of functions and other facilities automatically made available to every Haskell program. Undoubtedly, at some point, you will accidentally rewrite some already-existing function (I've done it more times than I can count). While doing so may be a very instructive experience as you begin to learn Haskell, once you start to tackle more complex tasks avoiding reimplementing existing functionality saves a lot of time. If you are curious you can check the [http://www.haskell.org/onlinereport/standard-prelude.html Prelude specification] right now (don't worry about all the unfamiliar syntax you'll see there, we'll catch up with that further on).
}}

== Let Bindings ==

Often we wish to provide local declarations for use in our functions. For instance, if you remember back to your grade school mathematics courses, the following formula is used to find the roots (zeros) of a polynomial of the form <math>ax^2+bx+c</math>:
: <math>x = \frac {-b \pm \sqrt{b^2-4ac}} {2a}</math>.
We could write the following function to compute the two values of <math>x</math>:

<source lang="haskell">
roots a b c =
    ((-b + sqrt(b*b - 4*a*c)) / (2*a),
     (-b - sqrt(b*b - 4*a*c)) / (2*a))
</source>

Notice that our definition here has a bit of redundancy. It is not quite as nice as the mathematical definition because we have needlessly repeated the code for <code>sqrt(b*b - 4*a*c)</code>. To remedy this problem, Haskell allows for local bindings. That is, we can create values inside of a function that only that function can see. For instance, we could create a local binding for <code>sqrt(b*b-4*a*c)</code> and call it, say, <code>sdisc</code> (short for square root of discriminant) and then use that in both places where <code>sqrt(b*b - 4*a*c)</code> occurred. We can do this using a <tt>'''let'''</tt>/<tt>'''in'''</tt> declaration:

<source lang="haskell">
roots a b c =
    let sdisc = sqrt (b*b - 4*a*c)
    in  ((-b + sdisc) / (2*a),
         (-b - sdisc) / (2*a))
</source>
In fact, you can provide multiple declarations inside a let. Just make sure they're indented the same amount, or you will have layout problems:

<source lang="haskell">
roots a b c =
    let sdisc = sqrt (b*b - 4*a*c)
        twice_a = 2*a
    in  ((-b + sdisc) / twice_a,
         (-b - sdisc) / twice_a)
</source>

{{Haskell navigation|chapter=Haskell Basics|noexercises=1}}

{{Auto category}}
