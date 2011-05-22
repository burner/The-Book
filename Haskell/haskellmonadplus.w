>﻿{{Haskell minitoc|chapter=Monads}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Monads}}

MonadPlus is a typeclass whose instances are monads which represent a number of computations.

== Introduction ==

You may have noticed, whilst studying monads, that the Maybe and list monads are quite similar, in that they both represent the number of results a computation can have. That is, you use Maybe when you want to indicate that a computation can fail somehow (i.e. it can have 0 or 1 result), and you use the list monad when you want to indicate a computation could have many valid answers (i.e. it could have 0 results -- a failure -- or many results).

Given two computations in one of these monads, it might be interesting to amalgamate these: find ''all'' the valid solutions. I.e. given two lists of valid solutions, to find all of the valid solutions, you simply concatenate the lists together. It's also useful, especially when working with folds, to require a 'zero results' value (i.e. failure). For lists, the empty list represents zero results.

We combine these two features into a typeclass:

 class Monad m => MonadPlus m where
   mzero :: m a
   mplus :: m a -> m a -> m a

Here are the two instance declarations for Maybe and the list monad:

 instance MonadPlus [] where
   mzero = []
   mplus = (++)
 
 instance MonadPlus Maybe where
   mzero                   = Nothing
   Nothing `mplus` Nothing = Nothing -- 0 solutions + 0 solutions = 0 solutions
   Just x  `mplus` Nothing = Just x  -- 1 solution  + 0 solutions = 1 solution
   Nothing `mplus` Just x  = Just x  -- 0 solutions + 1 solution  = 1 solution
   Just x  `mplus` Just y  = Just x  -- 1 solution  + 1 solution  = 2 solutions,
                                     -- but as Maybe can only have up to one
                                     -- solution, we disregard the second one.

Also, if you import Control.Monad.Error, then (Either e) becomes an instance:

 instance (Error e) => MonadPlus (Either e) where
   mzero            = Left noMsg
   Left _  `mplus` n = n
   Right x `mplus` _ = Right x

Remember that (Either e) is similar to Maybe in that it represents computations that can fail, but it allows the failing computations to include an error message. Typically, <code>Left s</code> means a failed computation with error message <code>s</code>, and <code>Right x</code> means a successful computation with result <code>x</code>.

== Example ==
A traditional way of parsing an input is to write functions which consume it, one character at a time. That is, they take an input string, then chop off ('consume') some characters from the front if they satisfy certain criteria (for example, you could write a function which consumes one uppercase character). However, if the characters on the front of the string don't satisfy these criteria, the parsers have ''failed'', and therefore they make a valid candidate for a Maybe.

Here we use <code>mplus</code> to run two parsers ''in parallel''. That is, we use the result of the first one if it succeeds, but if not, we use the result of the second. If that too fails, then our whole parser returns <code>Nothing</code>.

 -- | Consume a digit in the input, and return the digit that was parsed. We use
 --   a do-block so that if the pattern match fails at any point, fail of the
 --   the Maybe monad (i.e. Nothing) is returned.
 digit :: Int -> String -> Maybe Int
 digit i s | i > 9 || i < 0 = Nothing
           | otherwise      = do
   let (c:_) = s
   if read [c] == i then Just i else Nothing
 
 -- | Consume a binary character in the input (i.e. either a 0 or an 1)
 binChar :: String -> Maybe Int
 binChar s = digit 0 s `mplus` digit 1 s

== The MonadPlus laws ==
Instances of MonadPlus are required to fulfill several rules, just as instances of Monad are required to fulfill the three monad laws. Unfortunately, these laws aren't set in stone anywhere and aren't fully agreed on. The most essential are that mzero and mplus form a monoid, i.e.:

 mzero `mplus` m  =  m
 m `mplus` mzero  =  m
 m `mplus` (n `mplus` o)  =  (m `mplus` n) `mplus` o

The [http://haskell.org/ghc/docs/latest/html/libraries/base/Control-Monad.html#t%3AMonadPlus Haddock documentation] for Control.Monad quotes additional laws:

 mzero >>= f  =  mzero
 m >> mzero   =  mzero

And the [http://www.haskell.org/haskellwiki/MonadPlus HaskellWiki page] cites another (with controversy):

 (m `mplus` n) >>= k  =  (m >>= k) `mplus` (n >>= k)

There are even more sets of laws available, and therefore you'll sometimes see monads like IO being used as a MonadPlus. [http://www.haskell.org/all_about_monads/html/laws.html#zero All About Monads] and the [http://www.haskell.org/haskellwiki/MonadPlus Haskell Wiki page] for MonadPlus have more information on this. ''TODO: should that information be copied here?''

== Useful functions ==
Beyond the basic <code>mplus</code> and <code>mzero</code> themselves, there are a few functions you should know about:

=== msum ===

A very common task when working with instances of MonadPlus is to take a list of the monad, e.g. <code>[Maybe a]</code> or <code><nowiki>[[a]]</nowiki></code>, and fold down the list with <code>mplus</code>. <code>msum</code> fulfills this role:

 msum :: MonadPlus m => [m a] -> m a
 msum = foldr mplus mzero

A nice way of thinking about this is that it generalises the list-specific <code>concat</code> operation. Indeed, for lists, the two are equivalent. For Maybe it finds the first <code>Just x</code> in the list, or returns <code>Nothing</code> if there aren't any.

=== guard ===

This is a very nice function which you have almost certainly used before, without knowing about it. It's used in list comprehensions, as we saw in the previous chapter. List comprehensions can be decomposed into the list monad, as we saw:

 pythags = [ (x, y, z) | z <- [1..], x <- [1..z], y <- [x..z], x^2 + y^2 == z^2 ]

The previous can be considered syntactic sugar for:

 pythags = do
   z <- [1..]
   x <- [1..z]
   y <- [x..z]
   guard (x^2 + y^2 == z^2)
   return (x, y, z)

<code>guard</code> looks like this:

 guard :: MonadPlus m => Bool -> m ()
 guard True  = return ()
 guard False = mzero

Concretely, <code>guard</code> will reduce a do-block to <code>mzero</code> if its predicate is <code>False</code>. By the very first law stated in the 'MonadPlus laws' section above, an <code>mzero</code> on the left-hand side of an <code>>>=</code> operation will produce <code>mzero</code> again. As do-blocks are decomposed to lots of expressions joined up by <code>>>=</code>, an mzero at any point will cause the entire do-block to become mzero.

To further illustrate that, we will examine <code>guard</code> in the special case of the list monad, extending on the <code>pythags</code> function above. First, here is <code>guard</code> defined for the list monad:

 guard :: Bool -> [()]
 guard True  = [()]
 guard False = []

<code>guard</code> ''blocks off'' a route. For example, in <code>pythags</code>, we want to block off all the routes (or combinations of <code>x</code>, <code>y</code> and <code>z</code>) where <code>x^2 + y^2 == z^2</code> is <code>False</code>. Let's look at the expansion of the above <code>do</code>-block to see how it works:

 pythags =
   [1..] <code>>>=</code> \z ->
   [1..z] <code>>>=</code> \x ->
   [x..z] <code>>>=</code> \y ->
   guard (x^2 + y^2 == z^2) <code>>>=</code> \_ ->
   return (x, y, z)

Replacing <code>>>=</code> and <code>return</code> with their definitions for the list monad (and using some let-bindings to make things prettier), we obtain:

 pythags =
  let ret x y z = [(x, y, z)]
      gd  z x y = concatMap (\_ -> ret x y z) (guard $ x^2 + y^2 == z^2)
      doY z x   = concatMap (gd  z x) [x..z]
      doX z     = concatMap (doY z  ) [1..z]
      doZ       = concatMap (doX    ) [1..]
  in doZ

Remember that <code>guard</code> returns the empty list in the case of its argument being <code>False</code>. Mapping across the empty list produces the empty list, no matter what function you pass in. So the empty list produced by the call to <code>guard</code> in the binding of <code>gd</code> will cause <code>gd</code> to be the empty list, and therefore <code>ret</code> to be the empty list.

To understand why this matters, think about list-computations as a tree. With our Pythagorean triple algorithm, we need a branch starting from the top for every choice of <code>z</code>, then a branch from each of these branches for every value of <code>x</code>, then from each of these, a branch for every value of <code>y</code>. So the tree looks like this:

    start
    |__________________...
    | |     |
 z  1 2     3
    | |____ |__________
    | |   | |     |   |
 x  1 1   2 1     2   3
    | |__ | |____ |__ |
    | | | | | | | | | |
 y  1 1 2 2 1 2 3 2 3 3

Each combination of x, y and z represents a route through the tree. Once all the functions have been applied, each branch is concatenated together, starting from the bottom. Any route where our predicate doesn't hold evaluates to an empty list, and so has no impact on this concat operation.

== Exercises ==
<ol>
<li>Prove the MonadPlus laws for Maybe and the list monad.</li>
<li> We could augment our above parser to involve a parser for any character:
<pre>
 -- | Consume a given character in the input, and return the character we 
 --   just consumed, paired with rest of the string. We use a do-block  so that
 --   if the pattern match fails at any point, fail of the Maybe monad (i.e.
 --   Nothing) is returned.
 char :: Char -> String -> Maybe (Char, String)
 char c s = do
   let (c':s') = s
   if c == c' then Just (c, s') else Nothing
</pre>
It would then be possible to write a <code>hexChar</code> function which parses any valid hexidecimal character (0-9 or a-f). Try writing this function (hint: <code>map digit [0..9] :: [String -> Maybe Int]</code>).</li>
<li> More to come...</li>
</ol>

== Relationship with Monoids ==
''TODO: is this at all useful? --- I really thought so. Gave me some idea about Monoids. It's really lacking in documentation (Random Haskell newbie)''
(If you don't know anything about the Monoid data structure, then don't worry about this section. It's just a bit of a muse.)

Monoids are a data structure with two operations defined: an identity (or 'zero') and a binary operation (or 'plus'), which satisfy some axioms.

 class Monoid m where 
   mempty  :: m
   mappend :: m -> m -> m

For example, lists form a simple monoid:

 instance Monoid [a] where
   mempty  = []
   mappend = (++)

Note the usage of [a], not [], in the instance declaration. Monoids are not necessarily 'containers' of anything. For example, the integers (or indeed even the naturals) form two possible monoids:

 newtype AdditiveInt       = AI Int
 newtype MultiplicativeInt = MI Int
 
 instance Monoid AdditiveInt where
   mempty              = AI 0
   AI x `mappend` AI y = AI (x + y)
 
 instance Monoid MultiplicativeInt where
   mempty              = MI 1
   MI x `mappend` MI y = MI (x * y)

(A nice use of the latter is to keep track of probabilities.)

Monoids, then, look very similar to MonadPlus instances. Both feature concepts of a zero and plus, and indeed MonadPlus can be a subclass of Monoid (the following is not Haskell 98, but works with <tt>-fglasgow-exts</tt>):

 instance MonadPlus m => Monoid (m a) where
   mempty  = mzero
   mappend = mplus

However, they work at different levels. As noted, there is no requirement for monoids to be any kind of container. More formally, monoids have kind *, but instances of MonadPlus, as they're Monads, have kind * -> *.

{{Haskell navigation|chapter=Monads}}

[[Category:Haskell|MonadPlus]]
