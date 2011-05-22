>{{Haskell minitoc|chapter=Advanced Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Advanced Haskell}}

''We have permission to import material from the [http://www.haskell.org/arrows Haskell arrows page].  See the talk page for details.''

== The factory and conveyor belt metaphor ==

In this tutorial, we shall present arrows from the perspective of stream processors, using the factory metaphor from the [[../Understanding monads|monads module]] as a support.  Let's get our hands dirty right away.  

You are a factory owner, and as before you own a set of '''processing machines'''.  Processing machines are just a metaphor for functions; they accept some input and produce some output.  Your goal is to combine these processing machines so that they can perform richer, and more complicated tasks.  Monads allow you to combine these machines in a pipeline.  Arrows allow you to combine them in more interesting ways.  The result of this is that you can perform certain tasks in a less complicated and more efficient manner.  

In a monadic factory, we took the approach of wrapping the outputs of our machines in containers.  The arrow factory takes a completely different route: rather than wrapping the outputs in containers, we wrap ''the machines themselves''.  More specifically, in an arrow factory, we attach a pair of conveyor belts to each machine, one for the input and one for the output.

So given a function of type <code>b -> c</code>, we can construct an equivalent <code>a</code> arrow by attaching a <code>b</code> and <code>c</code> conveyor belt to the machine.  The equivalent arrow is of type <code>a b c</code>, which we can pronounce as an arrow <code>a</code> from <code>b</code> to <code>c</code>.

== Plethora of robots ==

We mentioned earlier that arrows give you more ways to combine machines together than monads did.  Indeed, the arrow type class provides six distinct '''robots''' (compared to the two you get with monads).  
 
=== <code>arr</code> ===

The simplest robot is <code>arr</code> with the type signature <code>arr :: (b -> c) -> a b c</code>.  In other words, the arr robot takes a processing machine of type <code>b -> c</code>, and adds conveyor belts to form an <code>a</code> arrow from <code>b</code> to <code>c</code>.

[[Image:ArrowsConveyors arr.png|center|100px|the <code>arr</code> robot]]

=== <code>(>>>)</code> ===

The next, and probably the most important, robot is <code>(>>>)</code>.  This is basically the arrow equivalent to the monadic bind robot <code>(>>=)</code>.  The arrow version of bind <code>(>>>)</code> puts two arrows into a sequence.  That is, it connects the output conveyor belt of the first arrow to the input conveyor belt of the second one.  

[[Image:ArrowsConveyors bind.png|center|200px|the <code>(>>>)</code> robot]]

What we get out of this is a new arrow.  One consideration to make, though is what input and output types our arrows may take.  Since we're connecting output and the input conveyor belts of the first and second arrows, the second arrow must accept the same kind of input as what the first arrow outputs.  If the first arrow is of type <code>a b c</code>, the second arrow must be of type <code>a c d</code>.  Here is the same diagram as above, but with things on the conveyor belts to help you see the issue with types.

[[Image:ArrowsConveyors bind2.png|center|200px|running the combined arrow]]

{{Exercises|What is the type of the combined arrow?}}

=== <code>first</code> ===

Up to now, our arrows can only do the same things that monads can.  Here is where things get interesting!  The arrows type class provides functions which allow arrows to work with ''pairs'' of input.  As we will see later on, this leads us to be able to express parallel computation in a very succinct manner.  The first of these functions, naturally enough, is <code>first</code>.  

If you are skimming this tutorial, it is probably a good idea to slow down at least in this section, because the <code>first</code> robot is one of the things that makes arrows truly useful.

[[Image:ArrowsConveyors first.png|center|200px|The <code>first</code> robot]]

Given an arrow <code>f</code>, the <code>first</code> robot attaches some conveyor belts and extra machinery to form a new, more complicated arrow.  The machines that bookend the input arrow split the input pairs into their component parts, and put them back together. The idea behind this is that the first part of every pair is fed into the <code>f</code>, whilst the second part is passed through on an empty conveyor belt.  When everything is put back together, we have same pairs that we fed in, except that the first part of every pair has been replaced by an equivalent output from <code>f</code>.

[[Image:ArrowsConveyors first2.png|center|200px|The combined arrow from the <code>first</code> robot]]

Now the question we need to ask ourselves is that of types.  Say that the input tuples are of type <code>(b,d)</code> and the input arrow is of type <code>a b c</code> (that is, it is an arrow from <code>b</code> to <code>c</code>).  What is the type of the output?  Well, the arrow converts all <code>b</code>s into <code>c</code>s, so when everything is put back together, the type of the output must be <code>(c,d)</code>.

{{Exercises|What is the type of the <code>first</code> robot?}}

=== <code>second</code> ===

If you understand the <code>first</code> robot, the <code>second</code> robot is a piece of cake.  It does the same exact thing, except that it feeds the second part of every input pair into the given arrow <code>f</code> instead of the first part.

[[Image:ArrowsConveyors second2.png|center|200px|the <code>second</code> robot with things running]]

What makes the <code>second</code> robot interesting is that it can be derived from the previous robots!  Strictly speaking, the only robots you need for arrows are <code>arr</code>, <code>(>>>)</code> and <code>first</code>.  The rest can be had "for free". 

{{Exercises|
# Write a function to swap two components of a tuple.
# Combine this helper function with the robots <code>arr</code>, <code>(>>>)</code> and <code>first</code> to implement the <code>second</code> robot}}

=== <code>***</code> ===

One of the selling points of arrows is that you can use them to express parallel computation.  The <code>(***)</code> robot is just the right tool for the job.  Given two arrows, <code>f</code> and <code>g</code>, the <code>(***)</code> combines them into a new arrow using the same bookend-machines we saw in the previous two robots

[[Image:ArrowsConveyors_star.png|center|200px|The <code>(***)</code> robot.]]

Conceptually, this isn't very much different from the robots <code>first</code> and <code>second</code>.  As before, our new arrow accepts ''pairs'' of inputs.  It splits them up, sends them on to separate conveyor belts, and puts them back together.  The only difference here is that, rather than having one arrow and one empty conveyor belt, we have two distinct arrows.  But why not?

[[Image:ArrowsConveyors_star2.png|center|200px|The <code>(***)</code> robot: running the combined arrow]]

{{Exercises|
# What is the type of the <code>(***)</code> robot?
# Given the <code>(>>>)</code>, <code>first</code> and <code>second</code> robots, implement the <code>(***)</code> robot.}}

=== <code>&&&</code> ===

The final robot in the Arrow class is very similar to the <code>(***)</code> robot, except that the resulting arrow accepts a single input and not a pair.  Yet, the rest of the machine is exactly the same.  How can we work with two arrows, when we only have one input to give them?

[[Image:ArrowsConveyors ampersand.png|center|200px|The <code>&&&</code> robot]]

The answer is simple: we clone the input and feed a copy into each machine!

[[Image:ArrowsConveyors ampersand2.png|center|200px|The <code>&&&</code> robot: the resulting arrow with inputs]]

{{Exercises|1=
# Write a simple function to clone an input into a pair.
# Using your cloning function, as well as the robots <code>arr</code>, <code>(>>>)</code> and <code>***</code>, implement the <code>&&&</code> robot
# Similarly, rewrite the following function without using <code>&&&</code>:
<pre>addA f g = f &&& g >>> arr (\ (y, z) -> y + z)
</pre>
}}

== Functions are arrows ==

Now that we have presented the 6 arrow robots, we would like to make sure that you have a more solid grasp of them by walking through a simple implementations of the Arrow class.  As in the monadic world, there are many different types of arrows.  What is the simplest one you can think of? Functions.

Put concretely, the type constructor for functions <code>(->)</code> is an instance of <code>Arrow</code>
 instance Arrow (->) where
   arr f = f
   f >>> g  = g . f
   first  f = \(x,y) -> (f x, y)

Now let's examine this in detail:
* <code>arr</code> - Converting a function into an arrow is trivial.  In fact, the function already is an arrow.
* <code>(>>>)</code> - we want to feed the output of the first function into the input of the second function.  This is nothing more than function composition.
* <code>first</code> - this is a little more elaborate.  Given a function <code>f</code>, we return a function which accepts a pair of inputs <code>(x,y)</code>, and runs <code>f</code> on <code>x</code>, leaving <code>y</code> untouched.

And that, strictly speaking, is all we need to have a complete arrow, but the arrow typeclass also allows you to make up your own definition of the other three robots, so let's have a go at that:

   first  f = \(x,y) -> (f x,   y) -- for comparison's sake
   second f = \(x,y) -> (  x, f y) -- like first
   f *** g  = \(x,y) -> (f x, g y) -- takes two arrows, and not just one
   f &&& g  = \x     -> (f x, g x) -- feed the same input into both functions

And that's it!  Nothing could be simpler.

<small>Note that this is not the official instance of functions as arrows.  You should take a look at the [http://darcs.haskell.org/packages/base/Control/Arrow.hs haskell library] if you want the real deal.</small>

== The arrow notation ==

In the introductory [[../Arrows/]] chapter, we introduced the <code>proc</code> and <code>-<</code> notation.  How does this tie in with all the arrow robots we just presented?  Sadly, it's a little bit less straightforward than do-notation, but let's have a look.

{{Haskell stub|sectiononly=1}}

[[Haskell/StephensArrowTutorial|Stephen's Arrow Tutorial]] I've written a tutorial that covers arrow notation. How might we integrate it into this page?

== Maybe functor ==

{{Haskell stub|sectiononly=1}}

It turns out that any monad can be made into arrow.  We'll go into that later on, but for now, ''FIXME: transition''

(This seems inconsistent with Typeclassopedia p19? google for typeclassopedia )

== Using arrows ==

At this point in the tutorial, you should have a strong enough grasp of the arrow machinery that we can start to meaningfully tackle the question of what arrows are good for.

=== Stream processing ===

=== Avoiding leaks ===

Arrows were originally motivated by an efficient parser design found by Swierstra & Duponcheel<ref>Swierstra, Duponcheel. ''Deterministic, error correcting parser combinators''. [http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.29.2760]</ref>.

To describe the benefits of their design, let's examine exactly how monadic parsers work.

If you want to parse a single word, you end up with several monadic parsers stacked end to end. Taking Parsec as an example, the parser string "word" can also be viewed as

<pre>
word = do char 'w' >> char 'o' >> char 'r' >> char 'd'
          return "word"
</pre>

Each character is tried in order, if "worg" is the input, then the first three parsers will succeed, and the last one will fail, making the entire string "word" parser fail.

If you want to parse one of two options, you create a new parser for each and they are tried in order. The first one must fail and then the next will be tried with the same input.

<pre>
ab = do char 'a' <|> char 'b' <|> char 'c'
</pre>

To parse "c" successfully, both 'a' and 'b' must have been tried.

<pre>
one = do char 'o' >> char 'n' >> char 'e'
      return "one"

two = do char 't' >> char 'w' >> char 'o'
      return "two"

three = do char 't' >> char 'h' >> char 'r' >> char 'e' >> char 'e'
        return "three"

nums = do one <|> two <|> three
</pre>

With these three parsers, you can't know that the string "four" will fail the parser nums until the last parser has failed.

If one of the options can consume much of the input but will fail, you still must descend down the chain of parsers until the final parser fails. All of the input that can possibly be consumed by later parsers must be retained in memory in case one of them does consume it. That can lead to much more space usage than you would naively expect, this is often called a space leak.

The general pattern of monadic parsers is that each option must fail or one option must succeed.

==== So what's better? ====

Swierstra & Duponcheel (1996) noticed that a smarter parser could immediately fail upon seeing the very first character. For example, in the nums parser above, the choice of first letter parsers was limited to either the letter 'o' for "one" or the letter 't' for both "two" and "three". This smarter parser would also be able to garbage collect input sooner because it could look ahead to see if any other parsers might be able to consume the input, and drop input that could not be consumed. This new parser is a lot like the monadic parsers with the major difference that it exports static information. It's like a monad, but it also tells you what it can parse.

There's one major problem. This doesn't fit into the monadic interface. Monads are (a -> m b), they're based around functions only. There's no way to attach static information. You have only one choice, throw in some input, and see if it passes or fails.

The monadic interface has been touted as a general purpose tool in the functional programming community, so finding that there was some particularly useful code that just couldn't fit into that interface was something of a setback.  This is where Arrows come in.  John Hughes's <i>Generalising monads to arrows</i> proposed the arrows abstraction as new, more flexible tool.  

==== Static and dynamic parsers ====

Let us examine Swierstra & Duponcheel's parser in greater detail, from the perspective of arrows. The parser has two components: a fast, static parser which tells us if the input is worth trying to parse; and a slow, dynamic parser which does the actual parsing work.

<pre>
data Parser s a b = P (StaticParser s) (DynamicParser s a b)
data StaticParser s = SP Bool [s]
newtype DynamicParser s a b = DP ((a,[s]) -> (b,[s]))
</pre>

The static parser consists of a flag, which tells us if the parser can accept the empty input, and a list of possible '''starting characters'''.  For example, the static parser for a single character would be as follows:
 spCharA :: Char -> StaticParser Char
 spCharA c = SP False [c]
It does not accept the empty string (<code>False</code>) and the list of possible starting characters consists only of <code>c</code>.

{{Warning|The rest of this section needs to be verified}}

The dynamic parser needs a little more dissecting : what we see is a function that goes from <code>(a,[s])</code> to <code>(b,[s])</code>.  It is useful to think in terms of sequencing two parsers : Each parser consumes the result of the previous parser (<code>a</code>), along with the remaining bits of input stream (<code>[s]</code>), it does something with <code>a</code> to produce its own result <code>b</code>, consumes a bit of string and returns ''that''.  Ooof.  So, as an example of this in action, consider a dynamic parser <code>(Int,String) -> (Int,String)</code>, where the <code>Int</code> represents a count of the characters parsed so far.  The table belows shows what would happen if we sequence a few of them together and set them loose on the string "cake" :

{|class="wikitable"
!|||result||remaining
|-
||before||0||cake
|-
||after first parser ||1||ake
|-
||after second parser||2||ke
|-
||after third parser||3||e
|}

<!-- the problem with the above illustration is that it makes this paragraph harder to understand... huh? I return the character? what happened to counting? -->
So the point here is that a dynamic parser has two jobs : it does something to the output of the previous parser (informally, <code>a -> b</code>), and it consumes a bit of the input string, (informally, <code>[s] -> [s]</code>), hence the type <code>DP ((a,[s]) -> (b,[s]))</code>.  Now, in the case of a dynamic parser for a single character, the first job is trivial.  We ignore the output of the previous parser.  We return the character we have parsed.  And we consume one character off the stream :

 dpCharA :: Char -> DynamicParser Char Char Char
 dpCharA c = DP (\(_,x:xs) -> (c,xs))

This might lead you to ask a few questions.  For instance, what's the point of accepting the output of the previous parser if we're just going to ignore it?  The best answer we can give right now is "wait and see".  If you're comfortable with monads, consider the bind operator <code>(>>=)</code>.  While bind is immensely useful by itself, sometimes, when sequencing two monadic computations together, we like to ignore the output of the first computation by using the anonymous bind <code>(>>)</code>.  This is the same situation here.  We've got an interesting little bit of power on our hands, but we're not going to use it quite yet.

The next question, then, shouldn't the dynamic parser be making sure that the current character off the stream matches the character to be parsed? Shouldn't <code>x == c</code> be checked for?  No.  And in fact, this is part of the point; the work is not necessary because the check would already have been performed by the static parser.  

Anyway, let us put this together.  Here is our S+D style parser for a single character:
 charA :: Char -> Parser Char Char Char
 charA c = P (SP False [c]) (DP (\(_,x:xs) -> (c,xs)))

==== Arrow combinators (robots) ====

Up to this point, we have explored two somewhat independent trains of thought.  On the one hand, we've taken a look at some arrow machinery, the combinators/robots from above, although we don't exactly know what it's for.  On the other hand, we have introduced a type of parser using the Arrow class.  We know that the goal is to avoid space leaks and that it somehow involves separating a fast static parser from its slow dynamic part, but we don't really understand how that ties in to all this arrow machinery.  In this section, we will attempt to address both of these gaps in our knowledge and merge our twin trains of thought into one. We're going to implement the Arrow class for <code>Parser s</code>, and by doing so, give you a glimpse of what makes arrows useful.  So let's get started:

 instance Arrow (Parser s) where

[[Image:ArrowsConveyors_arr.png|50px|right]]
One of the simplest things we can do is to convert an arbitrary function into a parsing arrow.  We're going to use "parse" in the loose sense of the term: our resulting arrow accepts the empty string, and ''only the empty string'' (its set of first characters is <code>[]</code>).  Its sole job is take the output of the previous parsing arrow and do something with it.  Otherwise, it does not consume any input.

  arr f = P (SP True []) (DP (\(b,s) -> (f b,s))

[[Image:ArrowsConveyors first.png|100px|right]]
Likewise, the <code>first</code> combinator is relatively straightforward.  Recall the conveyor belts from above.  Given a parser, we want to produce a new parser that accepts a pair of inputs <code>(b,d)</code>.  The first part of the input <code>b</code>, is what we actually want to parse.  The second part is passed through completely untouched:
  first (P sp (DP p)) = (P sp (DP (\((b,d),s) -> let (c, s') = p (b,s) in ((c,d),s'))))

[[Image:ArrowsConveyors bind.png|100px|right]]
On the other hand, the implementation of <code>(>>>)</code> requires a little more thought.  We want to take two parsers, and returns a combined parser incorporating the static and dynamic parsers of both arguments:
  (P (SP empty1 start1) (DP p1)) >>>
  (P (SP empty2 start2) (DP p2)) =
    P (SP (empty1 && empty2)
          (if not empty1 then start1 else start1 `union` start2))
      (DP (p2.p1))

Combining the dynamic parsers is easy enough; we just do function composition.  Putting the static parsers together requires a little bit of thought.  First of all, the combined parser can only accept the empty string if ''both'' parsers do.  Fair enough, now how about the starting symbols?  Well, the parsers are supposed to be in a sequence, so the starting symbols of the second parser shouldn't really matter.  If life were simple, the starting symbols of the combined parser would only be <code>start1</code>.  Alas, life is NOT simple, because parsers could very well accept the empty input.  If the first parser accepts the empty input, then we have to account for this possibility by accepting the starting symbols from both the first and the second parsers.

{{Exercises|
# Consider the <code>charA</code> parser from above.  What would <code>charA 'o' >>> charA 'n' >>> charA 'e'</code> result in?
# Write a simplified version of that combined parser.  That is: does it accept the empty string?  What are its starting symbols?  What is the dynamic parser for this?
}}

==== So what do arrows buy us in all this? ====

If you look back at our Parser type and blank out the static parser section, you might notice that this looks a lot like the arrow instance for functions.

<source lang = "haskell">
 arr f = \(b, s) -> (f b, s)
 first p = \((b, d), s) ->
             let (c, s') = p (b, s)
             in  ((c, d), s'))
 p1 >>> p2 = p2 . p1
</source>

There's the odd s variable out for the ride, which makes the definitions look a little strange, but you can roughly see the outline of the conveyor belts and computing machines. Actually, what you see here is roughly the arrow instance for the State monad (let ''f :: b -> c'', ''p :: b -> State s c'' and ''.'' actually be ''<=<''.

That's fine, but we could have done that with bind in classic monadic style, and ''first'' would have just been an odd helper function that you could have easily pattern matched. But remember, our Parser type is not just the dynamic parser; it also contains the static parser.

<source lang = "haskell">
 arr f = SP True []
 first sp = sp
 (SP empty1 start1) >>> (SP empty2 start2) = (SP (empty1 && empty2)
         (if not empty1 then start1 else start1 `union` start2))
</source>

This is not at all a function, it's just pushing around some data types. But the arrow metaphor works for it too, and we wrap it up with the same names. And when we combine the two types, we get a two-for-one deal; the static parser data structure goes along for the ride along with the dynamic parser. The Arrow interface lets us transparently simultaneously compose and manipulate the two parsers as a unit, which we can then run as a traditional, unified function.

== Monads can be arrows too ==

:''The real flexibility with arrows comes with the ones that aren't monads, otherwise it's just a clunkier syntax'' -- Philippa Cowderoy

It turns out that all monads can be made into arrows.  Here's a central quote from the original arrows papers:
<blockquote>Just as we think of a monadic type m a as representing a 'computation delivering an a '; so we think of an arrow type a b c, (that is, the application of the parameterised type a to the two parameters b and c) as representing 'a computation with input of type b delivering a c'; arrows make the dependence on input explicit.</blockquote>

One way to look at arrows is the way the English language allows you to noun a verb, for example, "I had a chat with them" versus "I chatted with them." Arrows are much like that, they turn a function from a to b into a value. This value is a first class transformation from a to b.

{{Haskell stub|sectiononly=1}}

<!--
''Explain intuition behind this''

''Show K arrows''
-->

== Arrows in practice == 

Arrows are a relatively new abstraction, but they already found a number of uses in the Haskell world

* Hughes' arrow-style parsers were first described in his 2000 paper, but a usable implementation wasn't available until May 2005. Einar Karttunen wrote an implementation called PArrows that approaches the features of standard Haskell parser combinator library, Parsec.
* The Fudgets library for building graphical interfaces ''FIXME: complete this paragraph''
* Yampa - ''FIXME: talk briefly about Yampa''
* The Haskell XML Toolbox ([http://www.fh-wedel.de/~si/HXmlToolbox/index.html HXT]) uses arrows for processing XML. There is a Wiki page in the Haskell Wiki with a somewhat [http://www.haskell.org/haskellwiki/HXT Gentle Introduction to HXT].

== See also ==
* Generalising Monads to Arrows - John Hughes 
* http://www.haskell.org/arrows/biblio.html

== References ==

<references />

== Acknowledgements ==

This module uses text from <i>An Introduction to Arrows</i> by Shae Erisson, originally written for The Monad.Reader 4

----

{{Haskell navigation|chapter=Advanced Haskell}}
{{Auto category}}
