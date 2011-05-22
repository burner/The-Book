>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Advanced Haskell}}

'''Continuation Passing Style''' is a format for expressions such that no function ever returns, instead they pass control onto a continuation. Conceptually a continuation is what happens next, for example the continuation for x in (x+1)*2 is add one then multiply by two.

== When do you need this? ==

Classically, continuations have been used primarily to dramatically alter the control flow of a program. For example, returning early from a procedure can be implemented with continuations. Exceptions and failure can also be signaled using continuations - pass in a continuation for success, another continuation for fail, and simply invoke the appropriate continuation.  If your function-that-can-fail itself calls another function-that-can-fail, you create a new success continuation, but pass the fail continuation you got.

Also, continuations allow you to "suspend" a computation, returning to that computation at another time.  They can be used to implement simple forms of concurrency (notably, one Haskell implementation, Hugs, uses continuations to implement cooperative concurrency).  Other control flows are possible.

In Haskell, continuations can be used in a similar fashion, for implementing interesting behavior in some monads. (Note that there are usually other techniques for this available, too, especially in tandem with laziness) They can be used to improve performance by eliminating certain construction-pattern matching sequences (i.e. a function returns a complex structure which the caller will at some point deconstruct), though a sufficiently smart compiler "''should''" be able to eliminate this.

<!-- re performance and CPS: see attoparsec, parsec, http://www.haskell.org/haskellwiki/Performance/Monads#Use__Continuation_Passing_Style.  I admit newer GHC's are getting better at analyzing non-CPS, but really, there is very little other application for CPS currently -->

== Starting simple ==
To begin with, we're going to explore two simple examples which illustrate what CPS and continuations are. Firstly a 'first order' example (meaning there are no higher order functions in to CPS transform), then a higher order one.

=== <code>square</code> ===
{{HaskellExample|A simple module, no continuations|
<pre>
-- We assume some primitives add and square for the example:

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)
</pre>
}}

And the same function pythagoras, written in CPS looks like this:

{{HaskellExample|A simple module, using continuations|
<pre>
-- We assume CPS versions of the add and square primitives,
-- (note: the actual definitions of add'cps and square'cps are not
-- in CPS form, they just have the correct type)

add'cps :: Int -> Int -> (Int -> r) -> r
add'cps x y k = k (add x y)

square'cps :: Int -> (Int -> r) -> r
square'cps x k = k (square x)

pythagoras'cps :: Int -> Int -> (Int -> r) -> r
pythagoras'cps x y k =
 square'cps x $ \x'squared ->
 square'cps y $ \y'squared ->
 add'cps x'squared y'squared $ \sum'of'squares ->
 k sum'of'squares
</pre>
}}

How the pythagoras'cps example operates is:

# square x and throw the result into the (\x'squared -> ...) continuation
# square y and throw the result into the (\y'squared -> ...) continuation
# add x'squared and y'squared and throw the result into the (\sum'of'squares -> ...) continuation
# throw the sum'of'squares into the toplevel/program continuation

And one can try it out:
<pre>
*Main> pythagoras'cps 3 4 print
25
</pre>

=== <code>thrice</code> ===

{{HaskellExample|A simple higher order function, no continuations|
<pre>
thrice :: (o -> o) -> o -> o
thrice f x = f (f (f x))
</pre>}}

<pre>
*Main> thrice tail "foobar"
"bar"
</pre>

Now the first thing to do, to CPS convert thrice, is compute the type of the CPSd form. We can see that <code>f :: o -> o</code>, so in the CPSd version, <code>f'cps :: o -> (o -> r) -> r</code>, and the whole type will be <code>thrice'cps :: (o -> (o -> r) -> r) -> o -> (o -> r) -> r</code>. Once we have the new type, that can help direct you how to write the function.

{{HaskellExample|A simple higher order function, with continuations|
<pre>
thrice'cps :: (o -> (o -> r) -> r) -> o -> (o -> r) -> r
thrice'cps f'cps x k =
 f'cps x $ \f'x ->
 f'cps f'x $ \f'f'x ->
 f'cps f'f'x $ \f'f'f'x ->
 k f'f'f'x
</pre>}}

{{Exercises|1=
''FIXME: write some exercises''
}}

== Using the <code>Cont</code> monad ==
By now, you should be used to the (meta-)pattern that whenever we find a pattern we like (here the pattern is using continuations), but it makes our code a little ugly, we use a monad to encapsulate the 'plumbing'. Indeed, there is a monad for modelling computations which use CPS.

{{HaskellExample|The Cont monad|
<pre>
newtype Cont r a = Cont { runCont :: (a -> r) -> r }
</pre>
}}

Removing the newtype and record cruft, we obtain that <code>Cont r a</code> expands to <code>(a -> r) -> r</code>. So how does this fit with our idea of continuations we presented above? Well, remember that a function in CPS basically took an extra parameter which represented 'what to do next'. So, here, the type of <code>Cont r a</code> expands to be an extra function (the continuation), which is a function from things of type <code>a</code> (what the result of the function would have been, if we were returning it normally instead of throwing it into the continuation), to things of type <code>r</code>, which becomes the final result type of our function.

{{HaskellExample|The <code>pythagoras</code> example, using the Cont monad</code>|
<pre>
import Control.Monad.Cont

add'cont :: Int -> Int -> Cont r Int
add'cont x y = return (add x y)

square'cont :: Int -> Cont r Int
square'cont x = return (square x)

pythagoras'cont :: Int -> Int -> Cont r Int
pythagoras'cont x y =
    do x'squared <- square'cont x
       y'squared <- square'cont y
       sum'of'squares <- add'cont x'squared y'squared
       return sum'of'squares
</pre>
}}

<pre>
*Main> runCont (pythagoras'cont 3 4) print
25
</pre>

Every function that returns a Cont-value actually takes an extra parameter, which is the continuation. Using <code>return</code> simply throws its argument into the continuation.

How does the Cont implementation of <code>(>>=)</code> work, then? It's easiest to see it at work:

{{HaskellExample|1=The <code>(>>=)</code> function for the Cont monad|2=
<pre>
square'C :: Int -> Cont r Int
square'C x = return (x ^ 2)

addThree'C :: Int -> Cont r Int
addThree'C x = return (x + 3)

main = runCont (square'C 4 >>= addThree'C) print
{- Result: 19 -}
</pre>
}}

The Monad instance for (Cont r) is given below:

 instance Monad (Cont r) where
   return n = Cont (\k -> k n)
   m >>= f  = Cont (\k -> runCont m (\a -> runCont (f a) k))

So <code>return n</code> is a Cont-value that throws <code>n</code> straight away into whatever continuation it is applied to. <code>m >>= f</code> is a Cont-value that runs <code>m</code> with the continuation <code>\a -> f a k</code>, which maybe, receive the result of computation inside <code>m</code> (the result is bound to <code>a</code>) , then applies that result to <code>f</code> to get another Cont-value. This is then called with the continuation we got at the top level (the continuation is bound to <code>k</code>); in essence <code>m >>= f</code> is a Cont-value that takes the result from <code>m</code>, applies it to <code>f</code>, then throws that into the continuation.

{{Exercises|To come.}}

== <code>callCC</code> ==
By now you should be fairly confident using the basic notions of continuations and Cont, so we're going to skip ahead to the next big concept in continuation-land. This is a function called <code>callCC</code>, which is short for 'call with current continuation'. We'll start with an easy example.

{{HaskellExample|<code>square</code> using <code>callCC</code>|
<pre>
-- Without callCC
square :: Int -> Cont r Int
square n = return (n ^ 2)

-- With callCC
square :: Int -> Cont r Int
square n = callCC $ \k -> k (n ^ 2)
</pre>
}}

We pass a ''function'' to <code>callCC</code> that accepts one parameter that is in turn a function. This function (<code>k</code> in our example) is our tangible continuation: we can see here we're throwing a value (in this case, <code>n ^ 2</code>) into our continuation. We can see that the <code>callCC</code> version is equivalent to the <code>return</code> version stated above because we stated that <code>return n</code> is just a Cont-value that throws <code>n</code> into whatever continuation that it is given. Here, we use <code>callCC</code> to bring the continuation 'into scope', and immediately throw a value into it, just like using <code>return</code>.

However, these versions look remarkably similar, so why should we bother using <code>callCC</code> at all? The power lies in that we now have precise control of exactly when we call our continuation, and with what values. Let's explore some of the surprising power that gives us.

=== Deciding when to use <code>k</code> ===
We mentioned above that the point of using <code>callCC</code> in the first place was that it gave us extra power over what we threw into our continuation, and when. The following example shows how we might want to use this extra flexibility.

{{HaskellExample|Our first proper <code>callCC</code> function|2=
<pre>
foo :: Int -> Cont r String
foo n =
  callCC $ \k -> do
    let n' = n ^ 2 + 3
    when (n' > 20) $ k "over twenty"
    return (show $ n' - 4)
</pre>
}}

<code>foo</code> is a slightly pathological function that computes the square of its input and adds three; if the result of this computation is greater than 20, then we return from the function immediately, throwing the String value <code>"over twenty"</code> into the continuation that is passed to <code>foo</code>. If not, then we subtract four from our previous computation, <code>show</code> it, and throw it into the computation. If you're used to imperative languages, you can think of <code>k</code> like the 'return' statement that immediately exits the function. Of course, the advantages of an expressive language like Haskell are that <code>k</code> is just an ordinary first-class function, so you can pass it to other functions like <code>when</code>, or store it in a <code>Reader</code>, etc.

Naturally, you can embed calls to <code>callCC</code> within do-blocks:

{{HaskellExample|More developed <code>callCC</code> example involving a do-block|
<pre>
bar :: Char -> String -> Cont r Int
bar c s = do
  msg <- callCC $ \k -> do
    let s' = c : s
    when (s' == "hello") $ k "They say hello."
    let s'' = show s'
    return ("They appear to be saying " ++ s'')
  return (length msg)
</pre>
}}

When you call <code>k</code> with a value, the entire <code>callCC</code> call takes that value. In other words, <code>k</code> is a bit like a 'goto' statement in other languages: when we call <code>k</code> in our example, it pops the execution out to where you first called <code>callCC</code>, the <code>msg <- callCC $ ...</code> line. No more of the argument to <code>callCC</code> (the inner do-block) is executed. Hence the following example contains a useless line:

{{HaskellExample|Popping out a function, introducing a useless line|
<pre>
bar :: Cont r Int
bar = callCC $ \k -> do
  let n = 5
  k n
  return 25
</pre>
}}

<code>bar</code> will always return <code>5</code>, and never <code>25</code>, because we pop out of <code>bar</code> before getting to the <code>return 25</code> line.

=== A note on typing ===

Why do we exit using <code>return</code> rather than <code>k</code> the second time within the <code>foo</code> example? It's to do with types. Firstly, we need to think about the type of <code>k</code>. We mentioned that we can throw something into <code>k</code>, and nothing after that call will get run (unless <code>k</code> is run conditionally, like when wrapped in a <code>when</code>). So the return type of <code>k</code> doesn't matter; we can never do anything with the result of running <code>k</code>. Actually, k never compute the continuation argument of return Cont-value of k. We say, therefore, that the type of <code>k</code> is:

 k :: a -> Cont r b

Inside <code>Cont r b</b>, because k never computes that continuation, type <code>b</code> which is the parameter type of that continuation can be anything independent of type <code>a</code>. We universally quantify the return type of <code>k</code>. This is possible for the aforementioned reasons, and the reason it's advantageous is that we can do whatever we want with the result of computation inside <code>k</code>. In our above code, we use it as part of a <code>when</code> construct:

 when :: Monad m => Bool -> m () -> m ()

As soon as the compiler sees <code>k</code> being used in this <code>when</code>, it infers that we want a <code>()</code> argument type of the continuation taking from the return value of <code>k</code>. The return Cont-value of <code>k</code> has type <code>Cont r ()</code>. This argument type <code>b</code> is independent of the argument type <code>a</code> of <code>k</code>. <ref>Type <code>a</code> infers a monomorphic type because <code>k</code> is bound by a lambda expression, and things bound by lambdas always have monomorphic types. See [[Haskell/Polymorphism|Polymorphism]].</ref>. The return Cont-value of <code>k</code> doesn't use the continuation which is argument of this Cont-value itself, it use the continuation which is argument of return Cont-value of the <code>callCC</code>. So that <code>callCC</code> has return type <code>Cont r String</code>. Because the final expression in inner do-block has type <code>Cont r String</code>, the inner do-block has type <code>Cont r String</code>. There are two possible execution routes: either the condition for the <code>when</code> succeeds, <code>k</code> doesn't use continuation providing by the inner do-block which finally takes the continuation which is argument of return Cont-value of the <code>callCC</code>, <code>k</code> uses directly the continuation which is argument of return Cont-value of the <code>callCC</code>, expressions inside do-block after <code>k</code> will totally not be used, because Haskell is lazy, unused expressions will not be executed. If the condition fails, the <code>when</code> returns <code>return ()</code> which use the continuation providing by the inner do-block, so execution passes on.

If you didn't follow any of that, just make sure you use <code>return</code> at the end of a do-block inside a call to <code>callCC</code>, not <code>k</code>.

=== The type of <code>callCC</code> ===
We've deliberately broken a trend here: normally when we've introduced a function, we've given its type straight away, but in this case we haven't. The reason is simple: the type is rather horrendously complex, and it doesn't immediately give insight into what the function does, or how it works. Nevertheless, you should be familiar with it, so now you've hopefully understood the function itself, here's it's type:

 callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a

This seems like a really weird type to begin with, so let's use a contrived example.

 callCC $ \k -> k 5

You pass a ''function'' to <code>callCC</code>. This in turn takes a parameter, <code>k</code>, which is another function. <code>k</code>, as we remarked above, has the type:

 k :: a -> Cont r b

The entire argument to <code>callCC</code>, then, is a function that takes something of the above type and returns <code>Cont r t</code>, where <code>t</code> is whatever the type of the argument to <code>k</code> was. So, <code>callCC</code>'s argument has type:

 (a -> Cont r b) -> Cont r a

Finally, <code>callCC</code> is therefore a function which takes that argument and returns its result. So the type of <code>callCC</code> is:

 callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a

=== The implementation of <code>callCC</code> ===
So far we have looked at the use of <code>callCC</code> and its type. This just leaves its implementation, which is:

  callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k

This code is far from obvious.  However, the amazing fact is that the implementations for <code>callCC f</code>, <code>return n </code> and <code> m >>= f </code> can all be produced automatically from their type signatures - Lennart Augustsson's Djinn [http://lambda-the-ultimate.org/node/1178] is a program that will do this for you.  See Phil Gossart's Google tech talk: [http://video.google.com/videoplay?docid=-4851250372422374791] for background on the theory behind Djinn; and Dan Piponi's article: [http://www.haskell.org/sitewiki/images/1/14/TMR-Issue6.pdf] which uses Djinn in deriving Continuation Passing Style.

== Example: a complicated control structure ==
This example was originally taken from the 'The Continuation monad' section of the [http://www.haskell.org/all_about_monads/html/index.html All about monads tutorial], used with permission.

{{HaskellExample|Using Cont for a complicated control structure|
<pre>
{- We use the continuation monad to perform "escapes" from code blocks.
   This function implements a complicated control structure to process
   numbers:

   Input (n)       Output                       List Shown
   =========       ======                       ==========
   0-9             n                            none
   10-199          number of digits in (n/2)    digits of (n/2)
   200-19999       n                            digits of (n/2)
   20000-1999999   (n/2) backwards              none
   >= 2000000      sum of digits of (n/2)       digits of (n/2)
-}  
fun :: Int -> String
fun n = (`runCont` id) $ do
        str <- callCC $ \exit1 -> do                        -- define "exit1"
          when (n < 10) (exit1 $ show n)
          let ns = map digitToInt (show $ n `div` 2)
          n' <- callCC $ \exit2 -> do                       -- define "exit2"
            when (length ns < 3) (exit2 $ length ns)
            when (length ns < 5) (exit2 n)
            when (length ns < 7) $ do 
              let ns' = map intToDigit (reverse ns)
              exit1 (dropWhile (=='0') ns')                 -- escape 2 levels
            return $ sum ns
          return $ "(ns = " ++ show ns ++ ") " ++ show n'
        return $ "Answer: " ++ str
</pre>
}}

Because it isn't initially clear what's going on, especially regarding the usage of <code>callCC</code>, we will explore this somewhat.

=== Analysis of the example ===
Firstly, we can see that <code>fun</code> is a function that takes an integer <code>n</code>. We basically implement a control structure using Cont and <code>callCC</code> that does different things based on the range that <code>n</code> falls in, as explained with the comment at the top of the function. Let's dive into the analysis of how it works.

# Firstly, the <code>(`runCont` id)</code> at the top just means that we run the Cont block that follows with a final continuation of <code>id</code>. This is necessary as the result type of <code>fun</code> doesn't mention Cont.
# We bind <code>str</code> to the result of the following <code>callCC</code> do-block:
## If <code>n</code> is less than 10, we exit straight away, just showing <code>n</code>.
## If not, we proceed. We construct a list, <code>ns</code>, of digits of <code>n `div` 2</code>.
## <code>n'</code> (an Int) gets bound to the result of the following inner <code>callCC</code> do-block.
### If <code>length ns < 3</code>, i.e., if <code>n `div` 2</code> has less than 3 digits, we pop out of this inner do-block with the number of digits as the result.
### If <code>n `div` 2</code> has less than 5 digits, we pop out of the inner do-block returning the original <code>n</code>.
### If <code>n `div` 2</code> has less than 7 digits, we pop out of ''both'' the inner and outer do-blocks, with the result of the digits of <code>n `div` 2</code> in reverse order (a String).
### Otherwise, we end the inner do-block, returning the sum of the digits of <code>n `div` 2</code>.
## We end this do-block, returning the String <code>"(ns = X) Y"</code>, where X is <code>ns</code>, the digits of <code>n `div` 2</code>, and Y is the result from the inner do-block, <code>n'</code>.
# Finally, we return out of the entire function, with our result being the string "Answer: Z", where Z is the string we got from the <code>callCC</code> do-block.

== Example: exceptions ==
One use of continuations is to model exceptions. To do this, we hold on to two continuations: one that takes us out to the handler in case of an exception, and one that takes us to the post-handler code in case of a success. Here's a simple function that takes two numbers and does integer division on them, failing when the denominator is zero.

{{HaskellExample|An exception-throwing <code>div</code>|2=
<pre>
 divExcpt :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
 divExcpt x y handler =
   callCC $ \ok -> do
     err <- callCC $ \notOk -> do
       when (y == 0) $ notOk "Denominator 0"
       ok $ x `div` y
     handler err
 
 {- For example,
 runCont (divExcpt 10 2 error) id  -->  5
 runCont (divExcpt 10 0 error) id  -->  *** Exception: Denominator 0
 -}
</pre>
}}

How does it work? We use two nested calls to <code>callCC</code>. The first labels a continuation that will be used when there's no problem. The second labels a continuation that will be used when we wish to throw an exception. If the denominator isn't 0, <code>x `div` y</code> is thrown into the <code>ok</code> continuation, so the execution pops right back out to the top level of <code>divExcpt</code>. If, however, we were passed a zero denominator, we throw an error message into the <code>notOk</code> continuation, which pops us out to the inner do-block, and that string gets assigned to <code>err</code> and given to <code>handler</code>.

A more general approach to handling exceptions can be seen with the following function. Pass a computation as the first parameter (which should be a function taking a continuation to the error handler) and an error handler as the second parameter. This example takes advantage of the generic MonadCont class which covers both <code>Cont</code> and <code>ContT</code> by default, plus any other continuation classes the user has defined.

{{HaskellExample|General <code>try</code> using continuations.|2=
<pre>
 tryCont :: MonadCont m => ((err -> m a) -> m a) -> (err -> m a) -> m a
 tryCont c h =
   callCC $ \ok -> do
     err <- callCC $ \notOk -> do x <- c notOk; ok x
     h err
</pre>
}}

For an example using <code>try</code>, see the following program.

{{HaskellExample|Using <code>try</code>|2=
<pre>
data SqrtException = LessThanZero deriving (Show, Eq)

sqrtIO :: (SqrtException -> ContT r IO ()) -> ContT r IO ()
sqrtIO throw = do 
  ln <- lift (putStr "Enter a number to sqrt: " >> readLn)
  when (ln < 0) (throw LessThanZero)
  lift $ print (sqrt ln)

main = runContT (tryCont sqrtIO (lift . print)) return
</pre>
}}

== Example: coroutines ==
{{Haskell stub|sectiononly=1}}

== Notes ==
<references />

{{Haskell navigation|chapter=Advanced Haskell}}
{{Auto category}}
