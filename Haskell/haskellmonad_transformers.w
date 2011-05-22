>﻿{{Haskell minitoc|chapter=Monads}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Monads}}

By this point you should have grasped the concept of monad, and what different monads are used for: <code>IO</code> for impure functions, <code>Maybe</code> for values that can be there or not, and so on.
A common practical problem is that, sometimes, you would like to have a monad with ''several'' of these characteristic at once.
Indeed you can use things like <code>IO (Maybe a)</code>, but then you have to start doing pattern matching within <code>do</code> blocks to extract the values you want: the point of monads was also to get rid of that.

Enter '''monad transformers''': these are special types that, when applied to a monad, generate a new, combined monad, that shares the behaviour of both.

== Motivation ==
Consider a common real-life problem for IT staff worldwide: to get their users to select passwords that are not easy to guess.
A typical strategy is to force the user to enter a password with a minimum length, and at least one letter, one number and similar irritating requirements.

A Haskell function to acquire a password from a user could look like:
<source lang="haskell">
getPassword :: IO (Maybe String)
getPassword = do s <- getLine
                 if isValid s then return $ Just s
                              else return Nothing
</source>
We need the <code>IO</code> monad because the function will not return always the same result, and the <code>Maybe</code> monad because we intend to return <code>Nothing</code> in case the password does not pass some test.

For the <code>isValid</code> function, you can use whatever you want; as an example, consider:
<source lang="haskell">
isValid :: String -> Bool
isValid s = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s
</source>

The true motivation for monad transformers is not only to make it easier to write <code>getPassword</code> (which it nevertheless does), but rather to simplify all the code instances in which we use it:
<source lang="haskell">
askPassword :: IO ()
askPassword = do putStrLn "Insert your new password:"
                 maybe_value <- getPassword
                 if isJust maybe_value 
                     then do putStrLn "Storing in database..."
                     -- ... other stuff, including 'else'
</source>
We need one line to generate the <code>maybe_value</code> variable, and then we have to do some further checking to figure out whether our password is OK or not.

With monad combinators, we will be able to extract the password in one go, without any pattern matching.
The gains for our simple example may seem small, but will scale up for more complex ones.

== A Simple Monad Transformer: <code>MaybeT</code> ==
To simplify the code for the <code>getPassword</code> function and the code that uses it, we will define a ''monad transformer'' that gives the <code>IO</code> monad some characteristics of the <code>Maybe</code> monad; we will call it <code>MaybeT</code>, following the convention that monad transformers have a "<code>T</code>" appended to the name of the monad whose characteristics they provide.

<code>MaybeT</code> is essentially a wrapper around <code>m (Maybe a)</code>, where <code>m</code> can be any monad (for our particular case, we are interested in <code>IO</code>):
<source lang="haskell">
newtype (Monad m) => MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
</source>
using the accessor function <code>runMaybeT</code> we can access the underlying representation.

''Monad transformers are monads themselves'', so we need to make <code>MaybeT m</code> an instance of the <code>Monad</code> class:
<source lang="haskell">
instance Monad m => Monad (MaybeT m) where
    return  = MaybeT . return . Just
</source>
<code>return</code> is implemented by <code>Just</code>, which injects into the <code>Maybe</code> monad, a generic <code>return</code> that injects into <code>m</code> (whatever it is), and the <code>MaybeT</code> constructor.
It would also have been possible (though arguably less readable) to write <code>return  = MaybeT . return . return</code>.

<source lang="haskell">
    x >>= f = MaybeT $ do maybe_value <- runMaybeT x
                          case maybe_value of
                               Nothing    -> return Nothing
                               Just value -> runMaybeT $ f value
</source>
The bind operator, which is the most important code snippet to understand how the transformer works, extracts the <code>Maybe</code> value in the <code>do</code> block, and based on whether it is a <code>Nothing</code> or <code>Just</code> value it returns accordingly either <code>m Nothing</code> or <code>m (Just value)</code>, wrapped in the <code>MaybeT</code> constructor.

You may wonder why we are using the <code>MaybeT</code> constructor before the <code>do</code> block, when inside it we use the accessor <code>runMaybeT</code>: however, the <code>do</code> block must be in the <code>m</code> monad, not in <code>MaybeT m</code>, since for the latter we have not yet defined the bind operator.

Technically, this is all we need; however, it is convenient to make <code>MaybeT</code> an instance of a few other classes:
<source lang="haskell">
instance Monad m => MonadPlus (MaybeT m) where
    mzero     = MaybeT $ return Nothing
    mplus x y = MaybeT $ do maybe_value <- runMaybeT x
                            case maybe_value of
                                 Nothing    -> runMaybeT y
                                 Just value -> runMaybeT x

instance MonadTrans MaybeT where
    lift = MaybeT . (liftM Just)
</source>

The latter class, <code>MonadTrans</code>, implements the <code>lift</code> function, which is very useful to take functions from the <code>m</code> monad and bring them into the <code>MaybeT m</code> monad, so that we can use them in <code>do</code> blocks inside the <code>MaybeT m</code> monad.


=== Application to Password Example ===
After having done all this, here is how the previous example of password management looks like:
<source lang="haskell">
getValidPassword :: MaybeT IO String
getValidPassword = do s <- lift getLine
                      guard (isValid s)
                      return s

askPassword :: MaybeT IO ()
askPassword = do lift $ putStrLn "Insert your new password:"
                 value <- getValidPassword
                 lift $ putStrLn "Storing in database..."
</source>
The code is now simpler, especially in the user function <code>askPassword</code>, and what is most important in that we do not need to manually check whether the result is <code>Nothing</code> or <code>Just</code>: the bind operator takes care of it for us.

Note how we use <code>lift</code> to bring functions <code>getLine</code> and <code>putStrLn</code> into the <code>MaybeT IO</code> monad. Also, since <code>MaybeT IO</code> is an instance of <code>MonadPlus</code>, checking for password validity can be taken care of by a <code>guard</code> statement, which will return <code>mzero</code> (i.e. <code>IO Nothing</code>) in case of a bad password.

Incidentally, it also becomes very easy to ask the user ''ad infinitum'' for a valid password:
<source lang="haskell">
askPassword :: MaybeT IO ()
askPassword = do lift $ putStrLn "Insert your new password:"
                 value <- msum $ repeat getValidPassword
                 lift $ putStrLn "Storing in database..."
</source>

==Introduction==
{{side note|Monad transformers are monads too!}}</div> Monad transformers are special variants of standard monads that facilitate the combining of monads. For example, <code>ReaderT Env IO a<code> is a computation which can read from some environment of type <code>Env</code>, can do some <code>IO</code> and returns a type <code>a</code>. Their type constructors are parameterized over a monad type constructor, and they produce combined monadic types. In this tutorial, we will assume that you understand the internal mechanics of the monad abstraction, what makes monads "tick". If, for instance, you are not comfortable with the bind operator (<code>>>=</code>), we would recommend that you first read [[../Understanding monads|Understanding monads]].

===Transformers are cousins===
A useful way to look at transformers is as ''cousins'' of some '''base monad'''. For example, the monad <code>ListT</code> is a cousin of its base monad <code>List</code>. Monad transformers are typically implemented almost exactly the same way that their cousins are, only more complicated because they are trying to thread some inner monad through.

The standard monads of the monad template library all have transformer versions which are defined consistently with their non-transformer versions. However, it is not the case that all monad transformers apply the same transformation. We have seen that the <code>ContT</code> transformer
turns continuations of the form <code>(a -> r) -> r</code> into continuations of the form <code>(a -> m r) -> m r</code>. The <code>StateT</code> transformer is different. It turns state transformer functions of the form <code>s -> (a, s)</code> into state transformer functions of the
form <code>s -> m (a, s)</code>. In general, there is no magic formula to create a transformer version of a monad&mdash;the form of each transformer depends on what makes sense in the context of its non-transformer type.


{|align="center" border="1" cellpadding="3"
|- bgcolor="#cc9999"
!|Standard Monad
!|Transformer Version
!|Original Type
!|Combined Type
|-
||Error || ErrorT || <code>Either e a</code> || <code>m (Either e a)</code>
|-
||State || StateT || <code>s -> (a, s)</code> || <code>s -> m (a, s)</code>
|-
||Reader || ReaderT || <code>r -> a</code> ||<code>r -> m a</code>
|-
||Writer ||WriterT || <code>(a, w)</code> || <code>m (a, w)</code>
|-
||Cont || ContT ||<code>(a -> r) -> r</code> ||<code>(a -> m r) -> m r</code>
|}


In the table above, most of the transformers <code>FooT</code> differ from their base monad <code>Foo</code> by the wrapping of the result type (right-hand side of the <code>-></code> for function kinds, or the whole type for non-function types) in the threaded monad (<code>m</code>). The <code>Cont</code> monad has two "results" in its type (it maps functions to values), and so <code>ContT</code> wraps both in the threaded monad. In other words, the commonality between all these transformers is like so, with some abuse of syntax:


{|align="center" border="1" cellpadding="3"
|- bgcolor="#cc9999"
!|Original Kind
!|Combined Kind
|-
||<code>*</code> || <code>m *</code>
|-
||<code>* -> * </code> || <code>* -> m *</code>
|-
||<code>(* -> *) -> *</code> ||<code>(* -> m *) -> m *</code>
|}

==Implementing transformers==
The key to understanding how monad transformers work is understanding how they implement the bind (<code>>>=</code>) operator. You'll notice that this implementation very closely resembles that of their standard, non-transformer cousins.

===Transformer type constructors===
Type constructors play a fundamental role in Haskell's monad support.
Recall that <code>Reader&r&a</code> is the type of values of type <code>a</code> within a Reader monad with environment of type <code>r</code>. The type constructor <code>Reader&r</code> is an instance of the <code>Monad</code> class, and the <code>runReader :: Reader r a -> r -> a</code> function
performs a computation in the Reader monad and returns the result of type <code>a</code>.

A transformer version of the Reader monad, called <code>ReaderT</code>, exists which adds a monad type constructor as an addition parameter. <code>ReaderT&r&m&a</code> is the type of values of
the combined monad in which Reader is the '''base monad''' and <code>m</code> is the '''inner monad'''.

<code>ReaderT&r&m</code> is an instance of the monad class, and the <code>runReaderT :: ReaderT r m a -> r -> m a</code> function performs a computation in the combined monad and returns a result of type <code>m&a</code>.

===The Maybe transformer===
We begin by defining the data type for the Maybe transformer. Our <code>MaybeT</code> constructor takes a single argument. Since transformers have the same data as their non-transformer cousins, we will use the <code>newtype</code> keyword. We could very well have chosen to use <code>data</code>, but that introduces needless overhead.

<source lang="haskell">
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
</source>

{{side note|side=right|Records are just syntactic sugar}}
This might seem a little off-putting at first, but it's actually simpler than it looks. The constructor for MaybeT takes a single argument, of type <code>m (Maybe a)</code>. That is all. We use some syntactic sugar so that you can see MaybeT as a record, and access the value of this single argument by calling <code>runMaybeT</code>. One trick to understanding this is to see monad transformers as sandwiches: the bottom slice of the sandwich is the '''base''' monad (in this case, <code>Maybe</code>). The filling is the '''inner''' monad, <code>m</code>. And the top slice is the monad transformer <code>MaybeT</code>. The purpose of the <code>runMaybeT</code> function is simply to remove this top slice from the sandwich. What is the type of <code>runMaybeT</code>? It is <code>(MaybeT m a) -> m (Maybe a)</code>.

As we mentioned in the beginning of this tutorial, monad transformers are monads too. Here is a partial implementation of the <code>MaybeT</code> monad. To understand this implementation, it really helps to know how its simpler cousin <code>Maybe</code> works. For comparison's sake, we put the two monad implementations side by side

{{body note|Note the use of 't', 'm' and 'b' to mean 'top', 'middle', 'bottom' respectively}}

{| align="center" border="1"
!| Maybe
!| MaybeT
|-
|valign="top"|
<source lang="haskell">
instance Monad Maybe where
 b_v >>= f = case b_v of
               Nothing -> Nothing
               Just v -> f v
</source>
|valign="top"|
<source lang="haskell">
instance (Monad m) => Monad (MaybeT m) where
 tmb_v >>= f =
   MaybeT $ runMaybeT tmb_v
            >>= \b_v -> case b_v of
                          Nothing -> return Nothing
                          Just v -> runMaybeT $ f v
</source>
|}

You'll notice that the <code>MaybeT</code> implementation looks a lot like the <code>Maybe</code> implementation of bind, with the exception that <code>MaybeT</code> is doing a lot of extra work. This extra work consists of unpacking the two extra layers of monadic sandwich (note the convention <code>tmb</code> to reflect the sandwich layers) and packing them up. If you really want to cut into the meat of this, read on. If you think you've understood up to here, why not try the following exercises:

{{Exercises|1=
# Implement the return function for the <code>MaybeT</code> monad
# Rewrite the implementation of the bind operator <code>>>=</code> to be more concise.
}}

====Dissecting the bind operator====
So what's going on here? You can think of this as working in three phases: first we remove the sandwich layer by layer, and then we apply a function to the data, and finally we pack the new value into a new sandwich

'''Unpacking the sandwich''':
Let us ignore the <code>MaybeT</code> constructor for now, but note that everything that's going on after the <code>$</code> is happening within the <code>m</code> monad and not the <code>MaybeT</code> monad!

# The first step is to remove the top slice of the sandwich by calling <code>runMaybeT tmb_v</code>
# We use the bind operator (<code>>>=</code>) to remove the second layer of the sandwich -- remember that we are working in the confines of the <code>m</code> monad.
# Finally, we use <tt>case</tt> and pattern matching to strip off the bottom layer of the sandwich, leaving behind the actual data with which we are working

'''Packing the sandwich back up''':
* If the bottom layer was <code>Nothing</code>, we simply <code>return Nothing</code> (which gives us a 2-layer sandwich). This value then goes to the <code>MaybeT</code> constructor at the very beginning of this function, which adds the top layer and gives us back a full sandwich.
* If the bottom layer was <code>Just v</code> (note how we have pattern-matched that bottom slice of monad off): we apply the function <code>f</code> to it. But now we have a problem: applying <code>f</code> to <code>v</code> gives a full three-layer sandwich, which would be absolutely perfect except for the fact that we're now going to apply the <code>MaybeT</code> constructor to it and get a type clash! So how do we avoid this? By first running <code>runMaybeT</code> to peel the top slice off so that the <code>MaybeT</code> constructor is happy when you try to add it back on.

===The List transformer===
Just as with the <code>Maybe</code> transformer, we create a datatype with a constructor that takes one argument:
<source lang="haskell">
newtype ListT m a = ListT { runListT :: m [a] }
</source>

The implementation of the <code>ListT</code> monad is also strikingly similar to its cousin, the <code>List</code> monad. We do exactly the same things for <code>List</code>, but with a little extra support to operate within the inner monad <code>m</code>, and to pack and unpack the monadic sandwich <code>ListT</code> - <code>m</code> - <code>List</code>.

{| align="center" border="1"
!| List
!| ListT
|-
|valign="top"|
<source lang="haskell">
instance Monad [] where
 b_v >>= f =
 --
 let x = map f b_v
 in concat x
</source>
|valign="top"|
<source lang="haskell">
instance (Monad m) => Monad (ListT m) where
 tmb_v >>= f =
 ListT $ runListT tmb_v
 >>= \b_v -> mapM (runListT . f) b_v
 >>= \x -> return (concat x)
</source>
|}

{{Exercises|1=
# Dissect the bind operator for the (ListT m) monad.  For example, why do we now have mapM and return?
# Now that you have seen two simple monad transformers, write a monad transformer <code>IdentityT</code>, which would be the transforming cousin of the <code>Identity</code> monad.
# Would <code>IdentityT SomeMonad</code> be equivalent to <code>SomeMonadT Identity</code> for a given monad and its transformer cousin?
}}

==Lifting==
''FIXME: insert introduction''

===liftM===
We begin with a notion which, strictly speaking, isn't about monad transformers. One small and surprisingly useful function in the standard library is <code>liftM</code>, which as the API states, is meant for lifting non-monadic functions into monadic ones. Let's take a look at that type:
<source lang="haskell">
liftM :: Monad m => (a1 -> r) -> m a1 -> m r
</source>

So let's see here, it takes a function <code>(a1 -> r)</code>, takes a monad with an a1 in it, applies that function to the a1, and returns the result. In my opinion, the best way to understand this function is to see how it is used. The following pieces of code all mean the same thing.

{|align="center" border="1"
!|do notation
!|liftM
!|liftM as an operator
|-
|valign="top"|
<source lang="haskell">
do foo <- someMonadicThing
return (myFn foo)
</source>
|valign="top"|
<source lang="haskell">
liftM myFn someMonadicThing
</source>
|valign="top"|
<source lang="haskell">
myFn `liftM` someMonadicThing
</source>
|}

What made the light bulb go off for me is this third example, where we use <code>liftM</code> as an operator. <code>liftM</code> is just a monadic version of <code>($)</code>!

{|align="center" border="1"
!|non monadic
!|monadic
|-
||
<source lang="haskell">
myFn $ aNonMonadicThing
</source>
||
<source lang="haskell">
myFn `liftM` someMonadicThing
</source>
|}

{{Exercises|1=
# How would you write <code>liftM</code>? You can inspire yourself from the first example.
}}

===lift===
When using combined monads created by the monad transformers, we avoid having to explicitly manage the inner monad types, resulting in clearer, simpler code. Instead of creating additional do-blocks within the computation to manipulate values in the inner monad type, we can use lifting operations to bring
functions from the inner monad into the combined monad.

Recall the <code>liftM</code> family of functions which are used to lift non-monadic functions into a monad. Each monad transformer provides a <code>lift</code> function that is used to lift a monadic computation into a combined monad.

The <code>MonadTrans</code> class is defined in {{Haskell lib|p=mtl|v=1.1.0.2|Control|Monad|Trans}} and provides the single function <code>lift</code>. The <code>lift</code> function lifts a monadic computation in the inner monad into the combined monad.

<source lang="haskell">
class MonadTrans t where
 lift :: (Monad m) => m a -> t m a
</source>

Monads which provide optimized support for lifting IO operations are defined as members of the <code>MonadIO</code> class, which defines the <code>liftIO</code> function.

<source lang="haskell">
class (Monad m) => MonadIO m where
 liftIO :: IO a -> m a
</source>

===Using <code>lift</code>===
===Implementing <code>lift</code>===
Implementing <code>lift</code> is usually pretty straightforward. Consider the transformer <code>MaybeT</code>:

<source lang="haskell">
instance MonadTrans MaybeT where
 lift mon = MaybeT (mon >>= return . Just)
</source>

We begin with a monadic value (of the inner monad), the middle layer, if you prefer the monadic sandwich analogy. Using the bind operator and a type constructor for the base monad, we slip the bottom slice (the base monad) under the middle layer. Finally we place the top slice of our sandwich by using the constructor <code>MaybeT</code>. So using the lift function, we have transformed a lowly piece of sandwich filling into a bona-fide three-layer monadic sandwich.

<small>As with our implementation of the <code>Monad</code> class, the bind operator is working within the confines of the inner monad.</small>

{{Exercises|1=
# Why is it that the <code>lift</code> function has to be defined seperately for each monad, where as <code>liftM</code> can be defined in a universal way?
# Implement the <code>lift</code> function for the <code>ListT</code> transformer.
# How would you lift a regular function into a monad transformer? Hint: very easily.
}}

==The State monad transformer==
Previously, we have pored over the implementation of two very simple monad transformers, <code>MaybeT</code> and <code>ListT</code>. We then took a short detour to talk about lifting a monad into its transformer variant. Here, we will bring the two ideas together by taking a detailed look at the implementation of one of the more interesting transformers in the standard library, <code>StateT</code>. Studying this transformer will build insight into the transformer mechanism that you can call upon when using monad transformers in your code. You might want to review the section on the [[Haskell/Advanced monads#The State monad|State monad]] before continuing.

Just as the State monad was built upon the definition <code>newtype State s a = State { runState :: (s -> (a,s)) }</code> the StateT transformer is built upon the definition 
<source lang="haskell">
newtype StateT s m a = StateT { runStateT :: (s -> m (a,s)) }</source>

<code>State&s</code> is an instance of both the <code>Monad</code> class and the <code>MonadState&s</code> class, so

<code>StateT&s&m</code> should also be members of the <code>Monad</code> and <code>MonadState&s</code> classes. Furthermore, if <code>m</code> is an instance of <code>MonadPlus</code>,

<code>StateT&s&m</code> should also be a member of <code>MonadPlus</code>.

To define <code>StateT&s&m</code> as a <code>Monad</code> instance:

{|border="1"
!|State
!|StateT
|-valign="top"
||<source lang="haskell">
newtype State s a = State { runState :: (s -> (a,s)) }

instance Monad (State s) where
 return a        = State $ \s -> (a,s)
 (State x) >>= f = State $ \s ->
     let (v,s') = x s
     in runState (f v) s'
</source>
||<source lang="haskell">
newtype StateT s m a = StateT { runStateT :: (s -> m (a,s)) }

instance (Monad m) => Monad (StateT s m) where
 return a         = StateT $ \s -> return (a,s)
 (StateT x) >>= f = StateT $ \s -> do
     (v,s') <- x s          -- get new value and state
     runStateT (f v) s'     -- pass them to f
</source>
|}

Our definition of <code>return</code> makes use of the <code>return</code> function of the inner monad, and the binding operator uses a do-block to perform a computation in the inner monad.

We also want to declare all combined monads that use the <code>StateT</code> transformer to be instances of the <code>MonadState</code> class, so we will have to give definitions for <code>get</code> and <code>put</code>: 

<source lang="haskell">
instance (Monad m) => MonadState s (StateT s m) where
 get   = StateT $ \s -> return (s,s)
 put s = StateT $ \_ -> return ((),s)
</source>

Finally, we want to declare all combined monads in which <code>StateT</code> is used with an instance of <code>MonadPlus</code> to be instances of <code>MonadPlus</code>:
<source lang="haskell">
instance (MonadPlus m) => MonadPlus (StateT s m) where
 mzero = StateT $ \s -> mzero
 (StateT x1) `mplus` (StateT x2) = StateT $ \s -> (x1 s) `mplus` (x2 s)
</source>

The final step to make our monad transformer fully integrated with Haskell's monad classes is to make <code>StateT s</code> an instance of the <code>MonadTrans</code> class by providing a <code>lift</code> function:
<source lang="haskell">
instance MonadTrans (StateT s) where
 lift c = StateT $ \s -> c >>= (\x -> return (x,s))
</source>

The <code>lift</code> function creates a <code>StateT</code> state transformation function that binds the computation in the inner monad to a function that packages the result with the input state. The result is that, if for instance we apply StateT to the List monad, a function that returns a list (i.e., a computation in the List monad) can be lifted
into <code>StateT&s&[]</code>, where it becomes a function that returns a <code>StateT&(s&->&[(a,s)])</code>. That is, the lifted computation produces <em>multiple</em> (value,state) pairs from its input state. The effect of this is to "fork" the computation in StateT, creating a different branch of the computation for each value in the list returned by the lifted function. Of course, applying <code>StateT</code> to a different monad will produce different semantics for the <code>lift</code> function.

==Acknowledgements==
This module uses a large amount of text from ''All About Monads'' with permission from its author Jeff Newbern.

{{Haskell navigation|chapter=Monads}}

[[Category:Haskell|Monad transformers]]
