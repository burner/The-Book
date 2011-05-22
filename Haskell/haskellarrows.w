>﻿{{Haskell minitoc|chapter=Advanced Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Advanced Haskell}}

== Introduction ==

Arrows are a generalization of monads: every monad gives rise to an arrow, but not all arrows give rise to monads. They serve much the same purpose as monads -- providing a common structure for libraries -- but are more general. In particular they allow notions of computation that may be partially static (independent of the input) or may take multiple inputs. If your application works fine with monads, you might as well stick with them. But if you're using a structure that's very like a monad, but isn't one, maybe it's an arrow.

== <code>proc</code> and the arrow tail ==

Let's begin by getting to grips with the arrows notation.  We'll work with the simplest possible arrow there is (the function) and build some toy programs strictly in the aims of getting acquainted with the syntax.

Fire up your text editor and create a Haskell file, say toyArrows.hs:

<source lang = "haskell">
{-# LANGUAGE Arrows #-}

import Control.Arrow (returnA)

idA :: a -> a
idA = proc a -> returnA -< a

plusOne :: Int -> Int
plusOne = proc a -> returnA -< (a+1)
</source>

These are our first two arrows.  The first is the identity function in arrow form, and second, slightly more exciting, is an arrow that adds one to its input.  Load this up in GHCi, using the -XArrows extension and see what happens.

<pre>
% ghci -XArrows toyArrows.hs   
   ___         ___ _
  / _ \ /\  /\/ __(_)
 / /_\// /_/ / /  | |      GHC Interactive, version 6.4.1, for Haskell 98.
/ /_\\/ __  / /___| |      http://www.haskell.org/ghc/
\____/\/ /_/\____/|_|      Type :? for help.

Loading package base-1.0 ... linking ... done.
Compiling Main             ( toyArrows.hs, interpreted )
Ok, modules loaded: Main.
*Main> idA 3
3
*Main> idA "foo"
"foo"
*Main> plusOne 3
4
*Main> plusOne 100
101
</pre>

Thrilling indeed.  Up to now, we have seen three new constructs in the arrow notation:
* the keyword <code>proc</code>
* <code>-<</code>
* the imported function <code>returnA</code>

Now that we know how to add one to a value, let's try something twice as difficult: adding TWO:

<source lang = "haskell">
 plusOne = proc a -> returnA -< (a+1)
 plusTwo = proc a -> plusOne -< (a+1)
</source>

One simple approach is to feed (a+1) as input into the <code>plusOne</code> arrow.  Note the similarity between <code>plusOne</code> and <code>plusTwo</code>.  You should notice that there is a basic pattern here which goes a little something like this: proc FOO -> SOME_ARROW -< (SOMETHING_WITH_FOO)

{{Exercises|1=
# <code>plusOne</code> is an arrow, so by the pattern above <code>returnA</code> must be an arrow too.  What do you think <code>returnA</code> does?
}}

== <code>do</code> notation ==

Our current implementation of <code>plusTwo</code> is rather disappointing actually... shouldn't it just be <code>plusOne</code> twice?  We can do better, but to do so, we need to introduce the do notation:

<source lang = "haskell">
 plusTwoBis = 
  proc a -> do b <- plusOne -< a
               plusOne -< b
</source>

Now try this out in GHCi:

<pre>
Prelude> :r
Compiling Main             ( toyArrows.hs, interpreted )
Ok, modules loaded: Main.
*Main> plusTwoBis 5
7
</pre>

You can use this do notation to build up sequences as long as you would like:

<source lang = "haskell">
plusFive =
 proc a -> do b <- plusOne -< a
              c <- plusOne -< b
              d <- plusOne -< c
              e <- plusOne -< d
              plusOne -< e
</source>

== Monads and arrows ==

:''FIXME: I'm no longer sure, but I believe the intention here was to show what the difference is having this proc notation instead to just a regular chain of dos''

<!-- ----------- -->

{{Haskell stub|sectiononly=1}}

{{Haskell navigation|chapter=Advanced Haskell}}
{{Auto category}}

[[ru:Haskell/Стрелки]]
