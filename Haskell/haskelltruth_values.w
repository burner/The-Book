>{{Haskell minitoc|chapter=Haskell Basics|noexercises=1}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Haskell Basics|noexercises=1}}

== Equality and other comparisons ==

So far we have seen how to use the equals sign to define variables and functions in Haskell. Writing

<source lang = "haskell">
r = 5
</source>

will cause occurrences of <tt>r</tt> to be replaced by <tt>5</tt> in all places where it makes sense to do so according to the scope of the definition. Similarly,

<source lang = "haskell">
f x = x + 3
</source>

causes occurrences of <tt>f</tt> followed by a number (which is taken as <tt>f</tt>'s argument) to be replaced by that number plus three.

In Mathematics, however, the equals sign is also used in a subtly different and equally important way. For instance, consider this simple problem:

{{HaskellExample|Solve the following equation:|<math>x+3=5</math>}}

When we look at a problem like this one, our immediate concern is not the ability to represent the value <math>5</math> as <math>x+3</math>, or vice-versa. Instead, we read the <math>x+3=5</math> equation as a ''proposition'', which says that some number <math>x</math> gives 5 as result when added to 3. Solving the equation means finding which, if any, values of <math>x</math> make that proposition true. In this case, using elementary algebra we can convert the equation into <math>x=5-3</math> and finally to <math>x=2</math>, which is the solution we were looking for. The fact that it makes the equation true can be verified by replacing <math>x</math> with 2 in the original equation, leading us to <math>2+3=5</math>, which is evidently true.

The ability of comparing values to see if they are equal turns out to be extremely useful in programming. Haskell allows us to write such tests in a very natural way that looks just like an equation. The main difference is that, since the equals sign is already used for defining things, we use a ''double'' equals sign, <tt>==</tt>. To see it at work, you can start GHCi and enter the proposition we wrote above like this:

{{HaskellGHCi|1=
Prelude> 2 + 3 == 5
True
}}

GHCi returns "True" lending further confirmation of the fact that <math>2 + 3</math> is equal to 5. Of course, 2 is the only value that satisfies the equation <math>x+3=5</math>, and therefore we would expect to obtain different results with other numbers.

{{HaskellGHCi|1=
Prelude> 7 + 3 == 5
False
}}

Nice and coherent. Another thing to point out is that nothing stops us from using our own functions in these tests. Let us try it with the function <tt>f</tt> we mentioned at the start of the module:

{{HaskellGHCi|1=
Prelude> let f x = x + 3
Prelude> f 2 == 5
True
}}

Just as expected, since <tt>f 2</tt> is just <tt>2 + 3</tt>.

In addition to tests for equality, we can just as easily compare two numerical values to see which one is larger. Haskell provides a number of tests including: <tt><</tt> (less than), <tt>></tt> (greater than), <tt><=</tt> (less than or equal to) and <tt>>=</tt>  (greater than or equal to), which work in exactly the same way as <tt>==</tt> (equal to). For a simple application, we could use <tt><</tt> alongside the <tt>area</tt> function from the previous module to see whether a circle of a certain radius would have an area smaller than some value.

{{HaskellGHCi|1=
Prelude> let area r = pi * r^2
Prelude> area 5 < 50
False
}}

== Boolean values ==

At this point, GHCi might look like some kind of oracle (or not) which can tell you if propositions are true or false. That's all fine and dandy, but how could that help us to write programs? And what is actually going on when GHCi "answers" such "questions"?

To understand that, we will start from a different but related question. If we enter an arithmetical expression in GHCi the expression gets ''evaluated'', and the resulting numerical value is displayed on the screen:

{{HaskellGHCi|1=
Prelude> 2 + 2
4
}}

If we replace the arithmetical expression with an equality comparison, something similar seems to happen:

{{HaskellGHCi|1=
Prelude> 2 == 2
True
}}

But ''what'' is that "True" that gets displayed? It certainly does not look like a number. We can think of it is as something that tells us about the veracity of the proposition <tt>2 == 2</tt>. From that point of view, it makes sense to regard it as a ''value'' - except that instead of representing some kind of count, quantity, etc. it stands for the truth of a proposition. Such values are called '''truth values''', or '''boolean values'''<ref>The term is a tribute to the mathematician and philosopher [[w:George Boole|George Boole]].</ref>. Naturally, there are only two possible boolean values - <tt>True</tt> and <tt>False</tt>.

=== An introduction to types ===

When we say <tt>True</tt> and <tt>False</tt> are values, we are not just making an analogy. Boolean values have the same status as numerical values in Haskell, and indeed you can manipulate them just as well. One trivial example would be equality tests on truth values:

{{HaskellGHCi|1=
Prelude> True == True
True
Prelude> True == False
False
}}

<tt>True</tt> is indeed equal to <tt>True</tt>, and <tt>True</tt> is not equal to <tt>False</tt>. Now, quickly: can you answer whether <tt>2</tt> is equal to <tt>True</tt>?

{{HaskellGHCi|1=
Prelude> 2 == True

<interactive>:1:0:
    No instance for (Num Bool)
      arising from the literal `2' at <interactive>:1:0
    Possible fix: add an instance declaration for (Num Bool)
    In the first argument of `(==)', namely `2'
    In the expression: 2 == True
    In the definition of `it': it = 2 == True
}}

The correct answer is you ''can't'', no matter if quickly or with hours of reasoning, because the question just does not make sense. It is impossible to compare a number with something that is not a number, or a boolean with something that is not a boolean. Haskell incorporates that notion, and the ugly error message we got is, in essence, stating exactly that. Ignoring all of the obfuscating clutter (which we will get to understand eventually) what the message tells us is that, since there was a number (<tt>Num</tt>) on the left side of the <tt>==</tt>, some kind of number was expected on the right side. But a boolean value (<tt>Bool</tt>) is not a number, and so the equality test exploded into flames.

The general concept, therefore, is that values have '''types''', and these types define what we can or cannot do with the values. In this case, for instance, <tt>True</tt> is a value of type <tt>Bool</tt>, just like <tt>False</tt> (as for the <tt>2</tt>, while there is a well-defined concept of number in Haskell the situation is slightly more complicated, so we will defer the explanations for a little while). Types are a very powerful tool  because they provide a way to regulate the behaviour of values with rules which ''make sense'', making it easier to write programs that work correctly. We will come back to the topic of types many times as they are very important to Haskell, starting with the very next module of this book.

== Infix operators ==

What we have seen so far leads us to the conclusion that an equality test like <tt>2 == 2</tt> is an expression just like <tt>2 + 2</tt>, and that it also evaluates to a value in pretty much the same way. That fact is actually given a passing mention on the ugly error message we got on the previous example:

{{HaskellGHCi|1=
    In the expression: 2 == True
}}

Therefore, when we type <tt>2 == 2</tt> in the prompt and GHCi "answers" <tt>True</tt> it is just evaluating an expression. But there is a deeper truth involved in this process. A hint is provided by the very same error message: 

{{HaskellGHCi|1=
     In the first argument of `(==)', namely `2'
}}

GHCi called <tt>2</tt> the first ''argument'' of <tt>(==)</tt>. In the previous module we used the term argument to describe the values we feed a function with so that it evaluates to a result. It turns out that <tt>==</tt> is just a function, which takes two arguments, namely the left side and the right side of the equality test. The only special thing about it is the syntax: Haskell allows two-argument functions with names composed only of non-alphanumeric characters to be used as ''infix operators'', that is, placed between their arguments. The only caveat is that if you wish to use such a function in the "standard" way (writing the function name before the arguments, as a ''prefix operator'') the function name must be enclosed in parentheses. So the following expressions are completely equivalent:

{{HaskellGHCi|1=
Prelude> 4 + 9 == 13
True
Prelude> (==) (4 + 9) 13
True
}}

Writing the expression in this alternative style further drives the point that <tt>(==)</tt> is a function with two arguments just like <tt>areaRect</tt> in the previous module was. What's more, the same considerations apply to the other ''relational operators'' we mentioned (<tt><</tt>, <tt>></tt>, <tt><=</tt>, <tt>>=</tt>) and to the arithmetical operators (<tt>+</tt>, <tt>*</tt>, etc.) - all of them are just functions. This generality is an illustration of one of the strengths of Haskell - there are quite few "special cases", and that helps to keep things simple. In general, we could say that all tangible things in Haskell are either values, variables or functions.<ref>In case you found this statement to be quite bold, don't worry - we will go even further in due course.</ref>

== Boolean operations ==

One nice and useful way of seeing both truth values and infix operators in action are the boolean operations, which allows us to manipulate truth values as in logic propositions. Haskell provides us three basic functions for that purpose:

* <tt>(&&)</tt> performs the ''and'' operation. Given two boolean values, it evaluates to <tt>True</tt> if both the first and the second are <tt>True</tt>, and to <tt>False</tt> otherwise.

{{HaskellGHCi|1=
Prelude> (3 < 8) && (False == False)
True
Prelude> (&&) (6 <= 5) (1 == 1) 
False
}}

* <tt>(||)</tt> performs the ''or'' operation. Given two boolean values, it evaluates to <tt>True</tt> if either the first or the second are <tt>True</tt> (or if both are true), and to <tt>False</tt> otherwise.

{{HaskellGHCi|1=
Prelude> (2 + 2 == 5) || (2 > 0)
True
Prelude> (||) (18 == 17) (9 >= 11)
False
}}

* <tt>not</tt> performs the negation of a boolean value; that is, it converts <tt>True</tt> to <tt>False</tt> and vice-versa.

{{HaskellGHCi|1=
Prelude> not (5 * 2 == 10)
False
}}

One relational operator we didn't mention so far in our discussions about comparison of values is the ''not equal to'' operator. It is also provided by Haskell as the <tt>(/=)</tt> function, but if we had to implement it a very natural way of doing so would be:

<source lang = "haskell">
x /= y = not (x == y)
</source>

Note that it is perfectly legal syntax to write the operators infix, even when defining them. Another detail to note is that completely new operators can be created out of ASCII symbols (basically, those that are found on the keyboard).

== Guards ==

Earlier on in this module we proposed two questions about the operations involving truth values: what was actually going on when we used them and how they could help us in the task of writing programs. While we now have a sound initial answer for the first question, the second one could well look a bit nebulous to you at this point, as we did little more than testing one-line expressions here. We will tackle this issue by introducing a feature that relies on boolean values and operations and allows us to write more interesting and useful functions: ''guards''.

To show how guards work, we are going to implement the absolute value function. The absolute value of a number is the number with its sign discarded<ref>Technically, that just covers how to get the absolute value of a ''real'' number, but let's ignore this detail for now.</ref>; so if the number is negative (that is, smaller than zero) the sign is inverted; otherwise it remains unchanged. We could write the definition as:

<math>|x| = \begin{cases} x, & \mbox{if }  x \ge 0  \\ -x,  & \mbox{if } x < 0. \end{cases} </math> 

The key feature of the definition is that the actual expression to be used for calculating <math>|x|</math> depends on a set of propositions made about <math>x</math>. If <math>x \ge 0</math> we use the first expression, but if <math>x < 0</math> we use the second one instead. If we are going to implement the absolute value function in Haskell we need a way to express this decision process. That is exactly what guards help us to do. Using them, the implementation could look like this:<ref><tt>abs</tt> is also provided by Haskell, so in a real-world situation you don't need to worry about providing an implementation yourself.</tt></ref>

{{HaskellExample|1=The abs function.|2=<source lang="haskell">
abs x
    | x < 0     = 0 - x
    | otherwise = x
</source>}}

Impressively enough, the above code is almost as readable as the corresponding mathematical definition. In order to see how the guard syntax fits with the rest of the Haskell constructs, let us dissect the components of the definition:

* We start just like in a normal function definition, providing a name for the function, <tt>abs</tt>, and saying it will take a single parameter, which we will name <tt>x</tt>.

* Instead of just following with the <tt>=</tt> and the right-hand side of the definition, we entered a line break, and, following it, the two alternatives, placed in separate lines.<ref>We ''could'' have joined the lines and written everything in a single line, but in this case it would be a lot less readable.</ref> These alternatives are the ''guards'' proper. An important observation is that the whitespace is not there just for aesthetic reasons, but it is necessary for the code to be parsed correctly.

* Each of the guards begins with a pipe character, <tt>|</tt>. After the pipe, we put a expression which evaluates to a boolean (also called a boolean condition or a ''predicate''), which is followed by the rest of the definition - the equals sign and the right-hand side which should be used if the predicate evaluates to <tt>True</tt>.

* The <tt>otherwise</tt> deserves some additional explanation. If none of the preceding predicates evaluates to <tt>True</tt>, the <tt>otherwise</tt> guard will be deployed by default. In this case, if <tt>x</tt> is not smaller than it must be greater or equal than zero, so it makes no difference to use <tt>x >= 0</tt> or <tt>otherwise</tt> as the last predicate.

{{body note |There is no syntactical magic behind <tt>otherwise</tt>. It is defined alongside the default variables and functions of Haskell as simply

<source lang = "haskell">otherwise = True</source>

This definition makes for a catch-all guard since evaluation of the guard predicates is sequential, and so the always true <tt>otherwise</tt> predicate will only be reached if none of the other ones evaluates to <tt>True</tt> (that is, assuming you place it as the last guard!). In general it is a good idea to always provide an <tt>otherwise</tt> guard, as if none of the predicates is true for some input a rather ugly runtime error will be produced. 
}}

{{body note |You might be wondering why we wrote <tt>0 - x</tt> and not simply <tt>-x</tt> to denote the sign inversion. Truth is, we could have written the first guard as

<source lang = "haskell">    | x < 0    = -x </source>

and it would have worked just as well. The only issue is that this way of expressing sign inversion is actually one of the few "special cases" in Haskell, in that this <tt>-</tt> is ''not'' a function that takes one argument and evaluates to <tt>0 - x</tt>, but just a syntactical abbreviation. While very handy, this shortcut occasionally conflicts with the usage of <tt>(-)</tt> as an actual function (the subtraction operator), which is a potential source of annoyance (for one of several possible issues, try writing three minus minus four without using any parentheses for grouping). In any case, the only reason we wrote <tt>0 - x</tt> explicitly on the example was so that we could have an opportunity to make this point perfectly clear in this brief digression.
}}

=== <code>where</code> and Guards ===

<code>where</code> clauses are particularly handy when used with guards. For instance, consider this function, which computes the number of (real) solutions for a [[w:Quadratic equation|quadratic equation]], <math>ax^2 + bx + c = 0</math>: <!-- I don't like this example, but still couldn't think of anything more meaningful with only bare numbers. Suggestions are welcome... -->

<source lang = "haskell">
numOfSolutions a b c
    | disc > 0  = 2
    | disc == 0 = 1
    | otherwise = 0
        where
        disc = b^2 - 4*a*c
</source>

The <tt>where</tt> definition is within the scope of all of the guards, sparing us from repeating the expression for <tt>disc</tt>.

== Notes ==

<references/>


{{Haskell navigation|chapter=Haskell Basics|noexercises=1}}

{{Auto category}}
