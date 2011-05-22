>﻿{{Haskell minitoc|chapter=Intermediate Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Intermediate Haskell}}

Haskell relies on indentation to reduce the verbosity of your code, but working with the indentation rules can be a bit confusing.  The rules may seem many and arbitrary, but the reality of things is that there are only one or two layout rules, and all the seeming complexity and arbitrariness comes from how these rules interact with your code.  So to take the frustration out of indentation and layout, the simplest solution is to get a grip on these rules.

== The golden rule of indentation ==
Whilst the rest of this chapter will discuss in detail Haskell's indentation system, you will do fairly well if you just remember a single rule: Code which is part of some expression should be indented further in than the line containing the beginning of that expression

What does that mean? The easiest example is a let binding group. The equations binding the variables are part of the let expression, and so should be indented further in than the beginning of the binding group: the let keyword. So,

 let
  x = a
  y = b

Although you actually only need to indent by one extra space, it's more normal to place the first line alongside the 'let' and indent the rest to line up:

 let x = a
     y = b

Here are some more examples:

 do foo
    bar
    baz
 
 where x = a
       y = b
 
 case x of
   p  -> foo
   p' -> baz

Note that with 'case' it's less common to place the next expression on the same line as the beginning of the expression, as with 'do' and 'where'. Also note we lined up the arrows here: this is purely aesthetic and isn't counted as different layout; only ''indentation'', whitespace beginning on the far-left edge, makes a difference to layout. 

Things get more complicated when the beginning of the expression doesn't start at the left-hand edge. In this case, it's safe to just indent further than the line containing the expression's beginning. So,

 myFunction firstArgument secondArgument = do -- the 'do' doesn't start at the left-hand edge
   foo                                        -- so indent these commands more than the beginning of the line containing the 'do'.
   bar
   baz

Here are some alternative layouts to the above which would have also worked:

 myFunction firstArgument secondArgument = 
   do foo
      bar
      baz
 
 myFunction firstArgument secondArgument = do foo
                                              bar
                                              baz

== A mechanical translation ==

Did you know that indentation layout is optional?  It is entirely possible to write in Haskell as in a "one-dimensional" language like C, using semicolons to separate things and curly braces to group them back. Not only it can be occasionally useful to write code in this style, but also understanding how to convert from one style to the other can help understand the indentation rules. To do so, you need to understand two things: where we need semicolons/braces, and how to get there from layout. The entire layout process can be summed up in three translation rules (plus a fourth one that doesn't come up very often):

# If you see one of the layout keywords, (<code>let</code>, <code>where</code>, <code>of</code>, <code>do</code>), insert an open curly brace (right before the stuff that follows it)
# If you see something indented to the SAME level, insert a semicolon
# If you see something indented LESS, insert a closing curly brace
# If you see something unexpected in a list, like <code>where</code>, insert a closing brace before instead of a semicolon.

{{Exercises|1=
Answer in one word: what happens if you see something indented MORE?
}}

<!-- ''to be completed: work through an example'' -->

{{Exercises|1=
Translate the following layout into curly braces and semicolons.  Note: to underscore the mechanical nature of this process, we deliberately chose something which is probably not valid Haskell:<pre>
  of a
     b
      c
     d
  where
  a
  b
  c
  do
 you
  like
 the
way
 i let myself
        abuse
       these
 layout rules</pre>
}}

== Layout in action ==

{|border="1"
!|Wrong
!|Right
|-
||
  do first thing
  second thing
  third thing
||
  do first thing
     second thing 
     third thing
|}

=== do within if ===

What happens if we put a <code>do</code> expression with an <code>if</code>?  Well, as we stated above, the keywords <code>if</code> <code>then</code> <code>else</code>, and anything else but the four layout keywords do <em>not</em> affect layout.  So things remain exactly the same:

{|border="1"
!|Wrong
!|Right
|-
||
  if foo
     then do first thing
          second thing
          third thing
     else do something else
||
  if foo
     then do first thing
             second thing
             third thing
     else do something else
|}

=== Indent to the first ===

Remember that, due to the "golden rule of indentation" described above, although the keyword <code>do</code> tells Haskell to insert a curly brace where the curly braces goes depends not on the <code>do</code>, but the thing that immediately follows it.  For example, this weird-looking block of code is totally acceptable:

          do
 first thing
 second thing
 third thing

As a result, you could also write combined if/do combination like this:

{|border="1"
!|Wrong
!|Right
|-
||
  if foo
     then do first thing
          second thing
          third thing
     else do something else
||
  if foo
     then do 
      first thing
      second thing
      third thing
     else do something else
|}

This is also the reason why you can write things like this
 main = do
  first thing
  second thing
instead of 
 main = 
  do first thing
     second thing
Both are acceptable

=== <code>if</code> within <code>do</code> ===

This is a combination which trips up many Haskell programmers.  Why does the following block of code not work?

 -- why is this bad?
 do first thing
    if condition
    then foo
    else bar
    third thing

Just to reiterate, the <code>if then else</code> block is not at fault for this problem.  Instead, the issue is that the <code>do</code> block notices that the <code>then</code> part is indented to the same column as the <code>if</code> part, so it is not very happy, because from its point of view, it just found a new statement of the block.  It is as if you had written the unsugared version on the right:

{|border="1"
!|sweet (layout)
!|unsweet
|-
||
 -- why is this bad?
 do first thing
    if condition
    then foo
    else bar
    third thing
||
 -- still bad, just explicitly so
 do { first thing
    ; if condition
    ; then foo
    ; else bar
    ; third thing }
|}

Naturally, the Haskell compiler is confused because it thinks that you never finished writing your <code>if</code> expression, before writing a new statement.  The compiler sees that you have written something like <code>if condition;</code>, which is clearly bad, because it is unfinished.  So, in order to fix this, we need to indent the bottom parts of this if block a little bit inwards

{|border="1"
!|sweet (layout)
!|unsweet
|-
||
 -- whew, fixed it!
 do first thing
    if condition
      then foo
      else bar
    third thing
||
 -- the fixed version without sugar
 do { first thing
    ; if condition
       then foo
       else bar
    ; third thing }
|}

This little bit of indentation prevents the do block from misinterpreting your <code>then</code> as a brand new expression. Of course, you might as well prefer to always add indentation before <code>then</code> and <code>else</code>, even when it is not really necessary. That wouldn't hurt legibility, and would avoid bad surprises like this one. 

{{Exercises|1=
The if-within-do issue has tripped up so many Haskellers that one programmer has posted a [http://hackage.haskell.org/trac/haskell-prime/ticket/23 proposal] to the Haskell prime initiative to add optional semicolons between <code>if then else</code>. How would that help?}}

== References ==

* [http://www.haskell.org/onlinereport/lexemes.html#sect2.7 The Haskell Report (lexemes)] - see 2.7 on layout

{{Haskell navigation|chapter=Intermediate Haskell}}

[[Category:Haskell|Indentation]]
