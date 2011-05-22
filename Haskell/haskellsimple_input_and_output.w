>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Haskell Basics}}

So far this tutorial has discussed functions that return values, which is well and good.  But how do we write "Hello world"?  To give you a first taste of it, here is a small variant of the "Hello world" program:

{{HaskellExample|1=Hello! What is your name?|2=<source lang="haskell">
main = do
  putStrLn "Please enter your name: "
  name <- getLine
  putStrLn ("Hello, " ++ name ++ ", how are you?")
</source>}}

At the very least, what should be clear is that dealing with input and output (IO) in Haskell is not a lost cause! Pure functional languages have always had a problem with input and output because IO requires ''side effects''. Pure functions always have to return the same results for the same arguments. But how can such a function "getLine" return the same value every time it is called?

Before we give the solution, let's take a step back and think about the difficulties inherent in such a task.

Any IO library should provide a host of functions, containing (at a minimum) operations like:

*  print a string to the screen
*  read a string from a keyboard
*  write data to a file
*  read data from a file

There are two issues here.  Let's first consider the initial two examples and think about what their types should be.  Certainly the first procedure should take a <code>String</code> argument and produce something, but what should it produce?  It could produce a unit <code>()</code>, since there is essentially no return value from printing a string.  The second operation, similarly, should return a <code>String</code>, but it doesn't seem to require an argument.

We want both of these operations to be functions, but they are by definition not functions.  The item that reads a string from the keyboard cannot be a function, as it will not return the same <code>String</code> every time.  if the first function simply returns <code>()</code> every time, then referential transparency tells us we should have no problem with replacing it with a function <code>f _ = ()</code>, but clearly this does not have the desired result because the function has a ''side effect'': it prints the argument.

== Actions ==

The breakthrough for solving this problem came when Philip Wadler realized
that monads would be a good way to think about IO computations.  In
fact, monads are able to express much more than just the simple
operations described above; we can use them to express a variety of
constructions like concurrence, exceptions, IO, non-determinism and much
more.  Moreover, there is nothing special about them; they can be
defined ''within'' Haskell with no special handling from the compiler
(though compilers often choose to optimize monadic operations).  Monads
also have a somewhat undeserved reputation of being difficult to
understand.  So we're going to leave things at that &mdash; knowing simply
that IO somehow makes use of monads without necessarily understanding
the gory details behind them (they really aren't so gory).  So for now,
we can forget that monads even exist.

As pointed out before, we cannot think of things like "print a string
to the screen" or "read data from a file" as functions, since they
are not (in the pure mathematical sense).  Therefore, we give them
another name: ''actions''.  Not only do we give them a special
name; we give them a special type to complement it.  One particularly useful action is
<code>putStrLn</code>, which prints a string to the screen.  This action has
type:

<source lang="haskell">
putStrLn :: String -> IO ()
</source>
As expected, <code>putStrLn</code> takes a string argument.  What it
returns is of type <code>IO ()</code>.  This means that this function is
actually an action (that is what the <code>IO</code> means).
Furthermore, when this action is ''evaluated'' (or "run") , the result
will have type <code>()</code>.

{{body note|1=
Actually, this type means that <code>putStrLn</code> is an action "within
the IO monad", but we will gloss over this for now.
}}
You can probably already guess the type of <code>getLine</code>:

<source lang="haskell">
getLine :: IO String
</source>
This means that <code>getLine</code> is an IO action that, when run, will
have type <code>String</code>.

The question immediately arises: "how do you 'run' an action?".
This is something that is left up to the compiler.  You cannot
actually run an action yourself; instead, a program is, itself, a
single action that is run when the compiled program is executed.
Thus, the compiler requires that the <code>main</code> function have type
<code>IO ()</code>, which means that it is an IO action that returns nothing.
The compiled code then executes this action.

However, while you are not allowed to run actions yourself, you
''are'' allowed to <code>combine</code> actions.  There are two ways
to go about this.  The one we will focus on in this chapter is the
'''do''' notation, which provides a convenient means of putting
actions together, and allows us to get useful things done in Haskell
without having to understand what ''really'' happens.  Lurking behind
the do notation is the more explicit approach using the (>>=) operator,
but we will not be ready to cover this until the chapter [[../Understanding monads/]].

{{body note|1=
<tt>'''Do'''</tt> notation is just syntactic sugar for <code>(>>=)</code>.  If you have experience with higher order functions, it might be worth starting with the latter approach and coming back here to see how <tt>'''do'''</tt> notation gets used.
}}

Let's consider the following name program:

{{HaskellExample|1=What is your name?|2=<source lang="haskell">
main = do
  putStrLn "Please enter your name: "
  name <- getLine
  putStrLn ("Hello, " ++ name ++ ", how are you?")
</source>}}

We can consider the <tt>'''do'''</tt> notation as a way to combine a sequence of
actions.  Moreover, the <code><-</code> notation is a way to get the value out
of an action.  So, in this program, we're sequencing three actions: a <code>putStrLn</code>, 
a <code>getLine</code> and another <code>putStrLn</code>.  The <code>putStrLn</code> action has 
type <code>String -> IO ()</code>, so we provide it a <code>String</code>, and the fully applied 
action has type <code>IO ()</code>.  This is something that we are allowed to run as a program.

{{Exercises|1=
Write a program which asks the user for the base and height of a
right angled triangle, calculates its area and prints it to the screen.  The
interaction should look something like:
<pre>
The base?
3.3
The height?
5.4
The area of that triangle is 8.91
</pre>
Hint: you can use the function <code>read</code> to convert user strings like "3.3" into numbers like 3.3 and function <code>show</code> to convert a number into string.
}}

=== Left arrow clarifications ===

==== <code><-</code> is optional ====

While we are allowed to get a value out of certain actions like <code>getLine</code>, we certainly are not obliged to do so.  For example, we could very well have written something like this:

{{HaskellExample|1=executing <code>getLine</code> directly|2=<source lang="haskell">
main = do
  putStrLn "Please enter your name: "
  getLine
  putStrLn ("Hello, how are you?")
</source>}}

Clearly, that isn't very useful: the whole point of prompting the user for his or her name was so that we could do something with the result. That being said, it is conceivable that one might wish to read a line
and completely ignore the result.  Omitting the <code><-</code> will allow for that; the action will happen, but the data won't be stored anywhere.

In order to get the value out of the action, we write <code>name <- getLine</code>, which basically means "run <code>getLine</code>, and put the results in the variable called <code>name</code>."

==== <code><-</code> can be used with any action but the last ====

On the flip
side, there are also very few restrictions on which actions can have values
obtained from them.  Consider the following example, where we put the
results of each action into a variable (except the last...  more on that
later):

{{HaskellExample|1=putting all results into a variable|2=<source lang="haskell">
main = do
  x <- putStrLn "Please enter your name: "
  name <- getLine
  putStrLn ("Hello, " ++ name ++ ", how are you?")
</source>}}

The variable <code>x</code> gets the value out
of its action, but that isn't very interesting because
the action returns the unit value <code>()</code>.  So while we could technically get the value out
of any action, it isn't always worth it.  But wait, what about that last
action?  Why can't we get a value out of that?  Let's see what happens
when we try:

{{HaskellExample|1=getting the value out of the last action|2=
<source lang="haskell">
main = do
  x <- putStrLn "Please enter your name: "
  name <- getLine
  y <- putStrLn ("Hello, " ++ name ++ ", how are you?")
</source>

Whoops!
{{HaskellGHCi|1=
YourName.hs:5:2:
    The last statement in a 'do' construct must be an expression
}}
}}

This is a much more interesting example, but it requires a somewhat
deeper understanding of Haskell than we currently have.  Suffice it to
say, whenever you use <code><-</code> to get the value of an action,
Haskell is always expecting another action to follow it.  So the very
last action better not have any <code><-</code>s.

=== Controlling actions ===

Normal Haskell constructions like <tt>'''if/then/else'''</tt> and <tt>'''case/of'''</tt>
can be used within the <tt>'''do'''</tt> notation, but you need to be somewhat
careful.  For instance, in a simple "guess the number" program, we have:

<source lang="haskell">
doGuessing num = do
   putStrLn "Enter your guess:"
   guess <- getLine
   if (read guess) < num
     then do putStrLn "Too low!"
             doGuessing num
     else if (read guess) > num
            then do putStrLn "Too high!"
                    doGuessing num
            else do putStrLn "You Win!"
</source>
If we think about how the <tt>'''if/then/else'''</tt> construction works, it
essentially takes three arguments: the condition, the "then" branch,
and the "else" branch.  The condition needs to have type <code>Bool</code>,
and the two branches can have any type, provided that they have the
''same'' type.  The type of the entire <tt>'''if/then/else'''</tt>
construction is then the type of the two branches.

In the outermost comparison, we have <code>(read guess) < num</code> as the
condition.  This clearly has the correct type.  Let's just consider
the "then" branch.  The code here is:

<source lang="haskell">
          do putStrLn "Too low!"
             doGuessing num
</source>
Here, we are sequencing two actions: <code>putStrLn</code> and
<code>doGuessing</code>.  The first has type <code>IO ()</code>, which is fine.  The
second also has type <code>IO ()</code>, which is fine.  The type result of the
entire computation is precisely the type of the final computation.
Thus, the type of the "then" branch is also <code>IO ()</code>.  A similar
argument shows that the type of the "else" branch is also
<code>IO ()</code>.  This means the type of the entire <tt>'''if/then/else'''</tt>
construction is <code>IO ()</code>, which is just what we want.

{{body note|1=
In this code, the last line is <code>else do putStrLn "You Win!"</code>.
This is somewhat overly verbose.  In fact, <code>else putStrLn "You
Win!"</code> would have been sufficient, since <tt>'''do'''</tt> is only necessary
to sequence actions.  Since we have only one action here, it is
superfluous.
}}
It is ''incorrect'' to think to yourself "Well, I already started
a <tt>'''do'''</tt> block; I don't need another one," and hence write something
like:

<source lang="haskell">
    do if (read guess) < num
         then putStrLn "Too low!"
              doGuessing num
         else ...
</source>
Here, since we didn't repeat the <tt>'''do'''</tt>, the compiler doesn't know
that the <code>putStrLn</code> and <code>doGuessing</code> calls are supposed to be
sequenced, and the compiler will think you're trying to call
<code>putStrLn</code> with three arguments: the string, the function
<code>doGuessing</code> and the integer <code>num</code>.  It will certainly complain
(though the error may be somewhat difficult to comprehend at this
point).

We can write the same <code>doGuessing</code> function using a <tt>'''case'''</tt>
statement.  To do this, we first introduce the Prelude function
<code>compare</code>, which takes two values of the same type (in the <code>Ord</code>
class) and returns a value of type <code>Ordering</code>, namely one of 
<code>GT</code>, <code>LT</code>, <code>EQ</code>, depending on
whether the first is greater than, less than or equal to the second.

<source lang="haskell">
doGuessing num = do
  putStrLn "Enter your guess:"
  guess <- getLine
  case compare (read guess) num of
    LT -> do putStrLn "Too low!"
             doGuessing num
    GT -> do putStrLn "Too high!"
             doGuessing num
    EQ -> putStrLn "You Win!"
</source>
Here, again, the <tt>'''do'''</tt>s after the <code>-></code>s are necessary on the
first two options, because we are sequencing actions.

If you're used to programming in an imperative language like C or Java, you might think that <tt>'''return'''</tt> will exit you from the current function.  This is not so in Haskell.  In Haskell, <tt>'''return'''</tt> simply
takes a normal value (for instance, one of type <code>Int</code>) and makes
it into an action that when evaluated, gives the given value (for the same example, the
action would be of type <code>IO Int</code>). In particular, in an imperative
language, you might write this function as:

<source lang="c">
void doGuessing(int num) {
  print "Enter your guess:";
  int guess = atoi(readLine());
  if (guess == num) {
    print "You win!";
    return ();
  }

  // we won't get here if guess == num
  if (guess < num) {
    print "Too low!";
    doGuessing(num);
  } else {
    print "Too high!";
    doGuessing(num);
  }
}
</source>
Here, because we have the <code>return ()</code> in the first <code>if</code> match,
we expect the code to exit there (and in most imperative languages, it
does).  However, the equivalent code in Haskell, which might look
something like:

<source lang="haskell">
doGuessing num = do
  putStrLn "Enter your guess:"
  guess <- getLine
  case compare (read guess) num of
    EQ -> do putStrLn "You win!"
             return ()

  -- we don't expect to get here if guess == num
  if (read guess < num)
    then do print "Too low!";
            doGuessing num
    else do print "Too high!";
            doGuessing num
</source>
First of all, if you guess correctly, it will first print "You win!," but it won't exit, and it
will check whether <code>guess</code> is less than <code>num</code>.  Of course it is
not, so the else branch is taken, and it will print "Too high!" and
then ask you to guess again.

On the other hand, if you guess incorrectly, it will try to evaluate
the case statement and get either <code>LT</code> or <code>GT</code> as the result of
the <code>compare</code>.  In either case, it won't have a pattern that
matches, and the program will fail immediately with an exception.

{{Exercises|1=
What does the following program print out?
<source lang="haskell">
main =
 do x <- getX
    putStrLn x

getX =
 do return "hello"
    return "aren't"
    return "these"
    return "returns"
    return "rather"
    return "pointless?"
</source>
Why?
}}

{{Exercises|1=
Write a program that asks the user for his or her name.  If the name is
one of Simon, John or Phil, tell the user that you think Haskell is a
great programming language.  If the name is Koen, tell them that you
think debugging Haskell is fun (Koen Classen is one of the people who
works on Haskell debugging); otherwise, tell the user that you don't
know who he or she is.

Write two different versions of this program, one using <tt>'''if'''</tt>
statements, the other using a <tt>'''case'''</tt> statement.
}}

== Actions under the microscope ==

Actions may look easy up to now, but they are actually a common stumbling block for new Haskellers.  If you have run into trouble working with actions, you might consider looking to see if one of your problems or questions matches the cases below.  It might be worth skimming this section now, and coming back to it when you actually experience trouble.

=== Mind your action types ===

One temptation might be to simplify our program for getting a name and printing it back out.  Here is one unsuccessful attempt:

{{HaskellExample|Why doesn't this work?|
<source lang="haskell">
main =
 do putStrLn "What is your name? "
    putStrLn ("Hello " ++ getLine)
</source>

Ouch!

{{HaskellGHCi|1=
YourName.hs:3:26:
    Couldn't match expected type `[Char]'
           against inferred type `IO String'
}}
}}

Let us boil the example above down to its simplest form.  Would you expect this program to compile?

{{HaskellExample|1=This still does not work|2=<source lang="haskell">
main =
 do putStrLn getLine
</source>}}

For the most part, this is the same (attempted) program, except that we've stripped off the superflous "What is your name" prompt as well as the polite "Hello".  One trick to understanding this is to reason about it in terms of types.  Let us compare:
<source lang="haskell">
 putStrLn :: String -> IO ()
 getLine  :: IO String
</source>
We can use the same mental machinery we learned in [[../Type basics/]] to figure how everything went wrong.  Simply put, putStrLn is expecting a <code>String</code> as input.  We do not have a <code>String</code>, but something tantalisingly close, an <code>IO String</code>.  This represents an action that will ''give'' us a <code>String</code> when it's run.  To obtain the <code>String</code> that <code>putStrLn</code> wants, we need to run the action, and we do that with the ever-handy left arrow, <code><-</code>.

{{HaskellExample|1=This time it works|2=
<source lang="haskell">
main =
 do name <- getLine
    putStrLn name
</source>

Working our way back up to the fancy example:

<source lang="haskell">
main =
 do putStrLn "What is your name? "
    name <- getLine
    putStrLn ("Hello " ++ name)
</source>
}}

Now the name is the String we are looking for and everything is rolling again.

=== Mind your expression types too ===

Fine, so we've made a big deal out of the idea that you can't use actions in situations that don't call for them.  The converse of this is that you can't use non-actions in situations that DO expect actions.  Say we want to greet the user, but this time we're so excited to meet them, we just have to SHOUT their name out:
{{HaskellExample|1=Exciting but incorrect.  Why?|2=
<source lang="haskell">
import Data.Char (toUpper)

main =
 do name <- getLine
    loudName <- makeLoud name
    putStrLn ("Hello " ++ loudName ++ "!")
    putStrLn ("Oh boy! Am I excited to meet you, " ++ loudName)

-- Don't worry too much about this function; it just capitalises a String
makeLoud :: String -> String
makeLoud s = map toUpper s
</source>

This goes wrong...
{{HaskellGHCi|1=
    Couldn't match expected type `IO' against inferred type `[]'
      Expected type: IO t
      Inferred type: String
    In a 'do' expression: loudName <- makeLoud name
}}
}}

This is quite similar to the problem we ran into above: we've got a mismatch between something that is expecting an IO type, and something which is not.  This time, the cause is our use of the left arrow <code><-</code>; we're trying to left arrow a value of <code>makeLoud name</code>, which really isn't left arrow material.  It's basically the same mismatch we saw in the previous section, except now we're trying to use regular old String (the loud name) as an IO String, when those clearly are not the same thing.  The latter is an action, something to be run, whereas the former is just an expression minding its own business.  Note that we cannot simply use <code>loudName = makeLoud name</code> because a <code>do</code> sequences ''actions'', and <code>loudName = makeLoud name</code> is not an action.

So how do we extricate ourselves from this mess?  We have a number of options:
* We could find a way to turn <code>makeLoud</code> into an action, to make it return <code>IO String</code>.  But this is not desirable, because the whole point of functional programming is to cleanly separate our side-effecting stuff (actions) from the pure and simple stuff.  For example, what if we wanted to use makeLoud from some other, non-IO, function?  An IO <code>makeLoud</code> is certainly possible (how?), but missing the point entirely.
* We could use <code>return</code> to promote the loud name into an action, writing something like <code>loudName <- return (makeLoud name)</code>.  This is slightly better, in that we are at least leaving the <code>makeLoud</code> function itself nice and IO-free, whilst using it in an IO-compatible fashion.  But it's still moderately clunky, because by virtue of left arrow, we're implying that there's action to be had -- how exciting! -- only to let our reader down with a somewhat anticlimactic <code>return</code>
* Or we could use a let binding...

It turns out that Haskell has a special extra-convenient syntax for let bindings in actions.  It looks a little like this:

{{HaskellExample|1=<code>let</code> bindings in <code>do</code> blocks.|2=<source lang="haskell">
main =
 do name <- getLine
    let loudName = makeLoud name
    putStrLn ("Hello " ++ loudName ++ "!")
    putStrLn ("Oh boy! Am I excited to meet you, " ++ loudName)
</source>}}

If you're paying attention, you might notice that the let binding above is missing an <code>in</code>.  This is because <code>let</code> bindings in <code>do</code> blocks do not require the <code>in</code> keyword.  You could very well use it, but then you'd have to make a mess of your do blocks.  For what it's worth, the following two blocks of code are equivalent.

{|class="wikitable"
!|sweet
!|unsweet
|-
||
<source lang="haskell">
 do name <- getLine
    let loudName = makeLoud name
    putStrLn ("Hello " ++ loudName ++ "!")
    putStrLn ("Oh boy! Am I excited to meet you, " ++ loudName)
</source>
||
<source lang="haskell">
 do name <- getLine
    let loudName = makeLoud name
    in  do putStrLn ("Hello " ++ loudName ++ "!")
           putStrLn ("Oh boy! Am I excited to meet you, " ++ loudName)
</source>
|}

{{Exercises|
# Why does the unsweet version of the let binding require an extra <code>do</code> keyword?
# Do you always need the extra <code>do</code>?
# (extra credit) Curiously, <code>let</code> without <code>in</code> is exactly how we wrote things when we were playing with the interpreter in the beginning of this book.  Why can you omit the <code>in</code> keyword in the interpreter, when you'd have to put it in when typing up a source file?
}}

<!--
''TODO''
:''TODO: find a simpler example with no new concepts
-->

== Learn more ==

At this point, you should have the fundamentals needed to do some fancier input/output.  Here are some IO-related topics you may want to check in parallel with the main track of the course.
* You could continue the sequential track, by learning more about [[../Type Declarations/|types]] and eventually [[../Understanding monads/|monads]].
* Alternately: you could start learning about building graphical user interfaces in the [[../GUI/]] chapter
* For more IO-related functionality, you could also consider learning more about the [[../Hierarchical libraries/IO|System.IO library]]

{{Haskell navigation|chapter=Haskell Basics}}

{{Auto category}}
