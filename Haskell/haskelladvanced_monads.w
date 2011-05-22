>{{Haskell minitoc|chapter=Monads}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Monads}}

This chapter follows on from [[../Understanding monads/]], and explains a few more of the more advanced concepts.

== Monads as computations ==

=== The concept ===

A metaphor we explored in the last chapter was that of ''monads as containers''. That is, we looked at what monads are in terms of their structure. What was touched on but not fully explored is ''why we use monads''. After all, monads structurally can be very simple, so why bother at all?

The secret is in the view that each monad represents a ''different type of computation''. Here, and in the rest of this chapter, a 'computation' is simply a function call: we're computing the result of this function. In a minute, we'll give some examples to explain what we mean by this, but first, let's re-interpret our basic monadic operators:

==== <code>>>=</code> ====
The <code>>>=</code> operator is used to ''sequence two monadic computations''. That means it combines them so that when the combined computation is run, the first computation will run and its output will be fed into the second which will then run using (or dependent upon) that value.

==== <code>return</code> ====
<code>return x</code>, in computation-speak, is simply a computation that, when run, will produce the result <code>x</code> as-is, but in its own (the computation's) specific way. The meaning of the latter phrase will become clearer when we look at State below.

So how does the computations analogy work in practice? Let's look at some examples.

=== The Maybe monad ===
Computations in the Maybe monad (that is, function calls which result in a type wrapped up in a Maybe) represent ''computations that might fail''. The easiest example is with lookup tables. A lookup table is a table which relates ''keys'' to ''values''. You ''look up'' a value by knowing its key and using the lookup table. For example, you might have a lookup table of contact names as keys to their phone numbers as the values in a phonebook application. One way of implementing lookup tables in Haskell is to use a list of pairs: <code>[(a, b)]</code>. Here <code>a</code> is the type of the keys, and <code>b</code> the type of the values. Here's how the phonebook lookup table might look:

 phonebook :: [(String, String)]
 phonebook = [ ("Bob",   "01788 665242"),
               ("Fred",  "01624 556442"),
               ("Alice", "01889 985333"),
               ("Jane",  "01732 187565") ]

The most common thing you might do with a lookup table is look up values! However, this computation might fail. Everything's fine if we try to look up one of "Bob", "Fred", "Alice" or "Jane" in our phonebook, but what if we were to look up "Zoe"? Zoe isn't in our phonebook, so the lookup has failed. Hence, the Haskell function to look up a value from the table is a <code>Maybe</code> computation:

 lookup :: Eq a => a  -- a key
        -> [(a, b)]   -- the lookup table to use
        -> Maybe b    -- the result of the lookup

Lets explore some of the results from lookup:

 Prelude> lookup "Bob" phonebook
 Just "01788 665242"
 Prelude> lookup "Jane" phonebook
 Just "01732 187565"
 Prelude> lookup "Zoe" phonebook
 Nothing

Now let's expand this into using the full power of the monadic interface. Say, we're now working for the government, and once we have a phone number from our contact, we want to look up this phone number in a big, government-sized lookup table to find out the registration number of their car. This, of course, will be another <code>Maybe</code>-computation. But if they're not in our phonebook, we certainly won't be able to look up their registration number in the governmental database! So what we need is a function that will take the results from the first computation, and put it into the second lookup, but only if we didn't get <code>Nothing</code> the first time around. If we ''did'' indeed get <code>Nothing</code> from the first computation, or if we get <code>Nothing</code> from the second computation, our final result should be <code>Nothing</code>.

 comb :: Maybe a -> (a -> Maybe b) -> Maybe b
 comb Nothing  _ = Nothing
 comb (Just x) f = f x

Observant readers may have guessed where we're going with this one. That's right, comb is just <code>>>=</code>, but restricted to Maybe-computations. So we can chain our computations together:

 getRegistrationNumber :: String       -- their name
                       -> Maybe String -- their registration number
 getRegistrationNumber name = 
   lookup name phonebook <code>>>=</code> 
     (\number -> lookup number governmentalDatabase)

If we then wanted to use the result from the governmental database lookup in a third lookup (say we want to look up their registration number to see if they owe any car tax), then we could extend our <code>getRegistrationNumber</code> function:

 getTaxOwed :: String       -- their name
            -> Maybe Double -- the amount of tax they owe
 getTaxOwed name = 
   lookup name phonebook <code>>>=</code> 
     (\number -> lookup number governmentalDatabase) <code>>>=</code> 
       (\registration -> lookup registration taxDatabase)

Or, using the <code>do</code>-block style:

 getTaxOwed name = do
   number       <- lookup name phonebook
   registration <- lookup number governmentalDatabase
   lookup registration taxDatabase

Let's just pause here and think about what would happen if we got a <code>Nothing</code> anywhere. Trying to use <code>>>=</code> to combine a <code>Nothing</code> from one computation with another function will result in the <code>Nothing</code> being carried on and the second function ignored (refer to our definition of comb above if you're not sure). That is, a <code>Nothing</code> at ''any stage'' in the large computation will result in a <code>Nothing</code> overall, regardless of the other functions! Thus we say that the structure of the <code>Maybe</code> monad ''propagates failures''.

An important thing to note is that we're not by any means restricted to lookups! There are many, many functions whose results could fail and therefore use <code>Maybe</code>. You've probably written one or two yourself. Any computations in <code>Maybe</code> can be combined in this way.

==== Summary ====
The important features of the <code>Maybe</code> monad are that:

# It represents computations that could fail.
# It propagates failure.

=== The List monad ===

Computations that are in the list monad (that is, they end in a type [a]) represent ''computations with zero or more valid answers''. For example, say we are modelling the game of noughts and crosses (known as tic-tac-toe in some parts of the world). An interesting (if somewhat contrived) problem might be to find all the possible ways the game could progress: find the possible states of the board 3 turns later, given a certain board configuration (i.e. a game in progress).

Here is the instance declaration for the list monad:

 instance Monad [] where
   return a = [a]
   xs <code>>>=</code> f = concat (map f xs)

As monads are only really useful when we're chaining computations together, let's go into more detail on our example. The problem can be boiled down to the following steps:

# Find the list of possible board configurations for the next turn.
# Repeat the computation for each of these configurations: replace each configuration, call it ''C'', with the list of possible configurations of the turn after ''C''.
# We will now have a list of lists (each sublist representing the turns after a previous configuration), so in order to be able to repeat this process, we need to collapse this list of lists into a single list.

This structure should look similar to the monadic instance declaration above. Here's how it might look, without using the list monad:

 getNextConfigs :: Board -> [Board]
 getNextConfigs = undefined -- details not important

 tick :: [Board] -> [Board]
 tick bds = concatMap getNextConfigs bds

 find3rdConfig :: Board -> [Board]
 find3rdConfig bd = tick $ tick $ tick [bd]

(<code>concatMap</code> is a handy function for when you need to concat the results of a map: <code>concatMap f xs = concat (map f xs)</code>.) Alternatively, we could define this with the list monad:

 find3rdConfig :: Board -> [Board]
 find3rdConfig bd0 = do
   bd1 <- getNextConfigs bd0
   bd2 <- getNextConfigs bd1
   bd3 <- getNextConfigs bd2
   return bd3

==== List comprehensions ====
An interesting thing to note is how similar list comprehensions and the list monad are. For example, the classic function to find Pythagorean triples:

 pythags = [ (x, y, z) | z <- [1..], x <- [1..z], y <- [x..z], x^2 + y^2 == z^2 ]

This can be directly translated to the list monad:

 import Control.Monad (guard)
 
 pythags = do
   z <- [1..]
   x <- [1..z]
   y <- [x..z]
   guard (x^2 + y^2 == z^2)
   return (x, y, z)

The only non-trivial element here is <code>guard</code>. This is explained in the next module, [[../MonadPlus|Additive monads]].

=== The State monad ===

The State monad actually makes a lot more sense when viewed as a computation, rather than a container. Computations in State represents computations that ''depend on and modify some internal state''. For example, say you were writing a program to model the [[w:Three body problem#Three-body problem|three body problem]]. The internal state would be the positions, masses and velocities of all three bodies. Then a function, to, say, get the acceleration of a specific body would need to reference this state as part of its calculations.

The other important aspect of computations in State is that they can modify the internal state. Again, in the three-body problem, you could write a function that, given an acceleration for a specific body, updates its position.

The State monad is quite different from the Maybe and the list monads, in that it doesn't represent the ''result'' of a computation, but rather a certain property of the computation itself.

What we do is model computations that depend on some internal state as functions which take a state parameter. For example, if you had a function <code>f :: String -> Int -> Bool</code>, and we want to modify it to make it depend on some internal state of type <code>s</code>, then the function becomes <code>f :: String -> Int -> s -> Bool</code>. To allow the function to change the internal state, the function returns a pair of (return value, new state). So our function becomes <code>f :: String -> Int -> s -> (Bool, s)</code>

It should be clear that this method is a bit cumbersome. However, the types aren't the worst of it: what would happen if we wanted to run two stateful computations, call them <code>f</code> and <code>g</code>, one after another, passing the result of <code>f</code> into <code>g</code>? The second would need to be passed the new state from running the first computation, so we end up 'threading the state':

 fThenG :: (s -> (a, s)) -> (a -> s -> (b, s)) -> s -> (b, s)
 fThenG f g s =
   let (v,  s' ) = f s    -- run f with our initial state s.
       (v', s<nowiki>''</nowiki>) = g v s' -- run g with the new state s' and the result of f, v.
   in (v', s<nowiki>''</nowiki>)           -- return the latest state and the result of g

All this 'plumbing' can be nicely hidden by using the State monad. The type constructor <code>State</code> takes two type parameters: the type of its environment (internal state), and the type of its output. (Even though the new state comes ''last'' in the result pair, the state type must come ''first'' in the type parameters, since the 'real' monad is bound to some particular type of state but lets the result type vary.) So <code>State s a</code> indicates a stateful computation which depends on, and can modify, some internal state of type <code>s</code>, and has a result of type <code>a</code>. How is it defined? Well, simply as a function that takes some state and returns a pair of (value, new state):

 newtype State s a = State (s -> (a, s))

The above example of <code>fThenG</code> is, in fact, the definition of <code>>>=</code> for the State monad, which you probably remember from the first monads chapter.

=== The meaning of return ===
We mentioned right at the start that <code>return x</code> was the computation that 'did nothing' and just returned <code>x</code>. This idea only really starts to take on any meaning in monads with side-effects, like State. That is, computations in State have the opportunity to change the outcome of later computations by modifying the internal state. It's a similar situation with IO (because, of course, IO is just a special case of State). 

<code>return x</code> doesn't do this. A computation produced by <code>return</code> generally won't have any side-effects. The monad law <tt>return x >>= f == f x<tt> basically guarantees this, for most uses of the term 'side-effect'.

== Further reading ==
* [http://members.chello.nl/hjgtuyl/tourdemonad.html A tour of the Haskell Monad functions] by Henk-Jan van Tuyl
* [http://www.haskell.org/all_about_monads/html/index.html All about monads] by Jeff Newbern explains well the concept of monads as computations, using good examples. It also has a section outlining all the major monads, explains each one in terms of this computational view, and gives a full example.
* [http://spbhug.folding-maps.org/wiki/MonadsEn Monads] by Eugene Kirpichov attempts to give a broader and more intuitive understanding of monads by giving non-trivial examples of them

{{Haskell navigation|chapter=Monads}}

[[Category:Haskell|Advanced monads]]
