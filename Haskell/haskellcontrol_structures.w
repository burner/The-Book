>﻿{{Haskell minitoc|chapter=Elementary Haskell|noexercises=1}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Elementary Haskell|noexercises=1}}

Haskell offers several ways of expressing a choice between different values.  This section will describe them all and explain what they are for:

== <tt>if</tt> Expressions ==

You have already seen these.  The full syntax is:

<source lang="haskell">
if <condition> then <true-value> else <false-value>
</source>

Here, condition is an expression which evaluates to a boolean. If the <tt><condition></tt> is <tt>True</tt> then the <tt><true-value></tt> is returned, otherwise the <tt><false-value></tt> is returned.  Note that in Haskell <tt>if</tt> is an expression (which is converted to a value) - and not a statement (which is executed), as usual for imperative languages. A very important consequence of this fact is that in Haskell the <tt>else</tt> is ''required''. Since <tt>if</tt> is an expression, it must evaluate to a result, and the <tt>else</tt> ensures this. For the same reason, the usual indentation is different from imperative languages.  If you need to split an <tt>if</tt> expression across multiple lines then you should indent it like one of these:

<source lang="haskell">
if <condition>
   then <true-value>
   else <false-value>

if <condition>
   then
      <true-value>
   else
      <false-value>
</source>

Here is a simple example:

<source lang="haskell">
message42 :: Integer -> String
message42 n =
   if n == 42
      then "The Answer is forty two."
      else "The Answer is not forty two."
</source>

== <tt>case</tt> Expressions ==

One interesting way of thinking about <tt>case</tt> expressions is seeing them as a generalization of <tt>if</tt> expressions. We could even write a clone of <tt>if</tt> as a <tt>case</tt>:

<source lang="haskell">
alternativeIf :: Bool -> a -> a -> a
alternativeIf cond ifTrue ifFalse =
  case cond of
    True  -> ifTrue
    False -> ifFalse
</source>

First, this checks <tt>cond</tt> for a pattern match against <tt>True</tt>. If there is a match, the whole expression will evaluate to <tt>ifTrue</tt>, otherwise it will evaluate to <tt>ifFalse</tt> (since a Bool can only be True or False there is no need for a default case). <tt>case</tt> is more general than <tt>if</tt> because the pattern matching in which case selection is based allows it to work with expressions which evaluate to values of any type. <ref>Again, this is quite different from what happens in most imperative languages, in which switch/case statements are restricted to equality tests, often only on integral primitive types.</ref> In fact, the left hand side of any case branch is just a pattern, so it can also be used for binding:

<source lang="haskell">
describeString :: String -> String
describeString str = 
  case str of
    (x:xs) -> "The first character is: " ++ [x] ++ "; the rest of the string is: " ++ xs
    ""     -> "This is an empty string."
</source>

This expression tells you whether <tt>str</tt> is an empty string or something else. Of course, you could just do this with an if-statement (with a condition of <code>str == []</code>), but using a case binds variables to the head and tail of our list, which is convenient in this instance.

=== Equations and Case Expressions ===

Remember you can use multiple equations as an alternative to <tt>case</tt> expressions. The <tt>describeString</tt> function above could be written like this:

<source lang="haskell">
describeString :: String -> String
describeString (x:xs) = "The first character is " ++ [x] ++ "; the rest of the string is " ++ xs
describeString ""     = "This is the empty string."
</source>

Named functions and case expressions at the top level are completely interchangeable. In fact the function definition form shown here is just syntactic sugar for a <tt>case</tt> expression.  

One handy thing about <tt>case</tt> expressions, and indeed about Haskell control structures in general, is that since they evaluate to values they can go inside other expressions just like an ordinary expression would. For example, this <tt>case</tt> expression evaluates to a string which is then concatenated with two other strings, all inside a single expression:

<source lang="haskell">
data Colour = Black | White | RGB Int Int Int

describeColour :: Colour -> String
describeColour c = 
  "This colour is "
  ++ case c of
       Black           -> "black"
       White           -> "white"
       RGB 0 0 0       -> "black"
       RGB 255 255 255 -> "white"
       _               -> "freaky, man, sort of in between"
  ++ ", yeah?"
</source>

Writing this function in an equivalent way using the multiple equations style would need a named function inside something like a <tt>let</tt> block.

== Guards ==

As shown, if we have a top-level <tt>case</tt> expression, we can just give multiple equations for the function instead, which is often neater. Is there an analogue for <tt>if</tt> expressions? It turns out there is. We use some additional syntax known as "guards".  A guard is a boolean condition, like this:

<source lang="haskell">
describeLetter :: Char -> String
describeLetter c
   | c >= 'a' && c <= 'z' = "Lower case"
   | c >= 'A' && c <= 'Z' = "Upper case"
   | otherwise            = "Not a letter"
</source>

Note the lack of an <tt>=</tt> before the first <tt>|</tt>. Guards are evaluated in the order they appear. That is, if you have a set up similar to the following:

<source lang="haskell">
f (pattern1) | predicate1 = w
             | predicate2 = x
f (pattern2) | predicate3 = y
             | predicate4 = z
</source>

Then the input to f will be pattern-matched against pattern1. If it succeeds, then predicate1 will be evaluated. If this is true, then w is returned. If not, then predicate2 is evaluated. If ''this'' is true, then x is returned. Again, if not, then we jump out of this 'branch' of f and try to pattern match against pattern2, repeating the guards procedure with predicate3 and predicate4. Unlike the <tt>else</tt> in <tt>if</tt> statements, <tt>otherwise</tt> is not mandatory. Still, if no guards match an error will be produced at runtime, so it's always a good idea to provide the 'otherwise' guard, even if it is just to handle the "But this can't happen!" case (which normally ''does'' happen anyway...).

The <tt>otherwise</tt> you saw above is actually just a normal value defined in the Standard Prelude as:

<source lang="haskell">
otherwise :: Bool
otherwise = True
</source>

This works because of the sequential evaluation described a couple of paragraphs back: if none of the guards previous to your 'otherwise' one are true, then your otherwise will definitely be true and so whatever is on the right-hand side gets returned. It's just nice for readability's sake.

== Notes ==

<references/>

{{Haskell navigation|chapter=Elementary Haskell|noexercises=1}}

{{Auto category}}
