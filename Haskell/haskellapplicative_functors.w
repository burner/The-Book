>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Advanced Haskell}}

Applicative functors are functors with some extra properties, the most important one is that it allows you to apply functions inside the functor (hence the name) to other values. First we do a quick recap of functors, next we will see the applicative functors, and for what structure they are used. After that, we'll see that we can get an applicative functor out of every monad, and then we'll see an example of an applicative functor that is not a monad, but still very useful.

== Functors ==

Functors, or instances of the typeclass <code>Functor</code>, are some of the most often used structures in Haskell. Typically, they are structures that "can be mapped over". Here is the class definition of <code>Functor</code>:

  class Functor f where
    fmap :: (a -> b) -> f a -> f b

The most well-known functor is the list, where <code>fmap</code> corresponds to <code>map</code>. Another example is <code>Maybe</code>:

  instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap _ Nothing  = Nothing

Typically, for all tree-like structures you can write an instance of <code>Functor</code>.

{{Exercises|1=
Define instances of <code>Functor</code> for the following types:
# The rose tree type <code>Tree</code>, defined by: <pre>data Tree a = Node a [Tree a]</pre>
# <code>Either e</code> for a fixed <code>e</code>.
# The function type <code>((->) t)</code>. In this case, <code>f a</code> will be <code>(t -> a)</code>
}}

== Applicative Functors ==

Applicative functors are functors with extra properties: you can apply functions inside a functor to values that can be inside the functor or not. We will first look at the definition, then at some instances of applicative functors and their most important use.

=== Definition ===

  class (Functor f) => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

The <code>pure</code> function lifts any value inside the functor. <code>(<*>)</code> changes a function inside the functor to a function over values of the functor. The functor should satisfy some laws:

  pure id <*> v = v                            -- Identity
  pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
  pure f <*> pure x = pure (f x)               -- Homomorphism
  u <*> pure y = pure ($ y) <*> u              -- Interchange

And the <code>Functor</code> instance should satisfy the following law:

  fmap f x = pure f <*> x                      -- Fmap

=== Instances ===

As we have seen the <code>Functor</code> instance of <code>Maybe</code>, let's try to make it <code>Applicative</code> as well.

The definition of <code>pure</code> is easy. It is <code>Just</code>. Now the definition of <code>(<*>)</code>. If any of the two arguments is <code>Nothing</code>, the result should be <code>Nothing</code>. Otherwise, we extract the function and its argument from the two <code>Just</code> values, and return <code>Just</code> the function applied to its argument:

  instance Applicative Maybe where
    pure = Just
    (Just f) <*> (Just x) = Just (f x)
    _        <*> _        = Nothing

{{Exercises|1=
# Check that the Applicative laws hold for this instance for <code>Maybe</code>
# Write <code>Applicative</code> instances for 
:# <code>Either e</code>, for a fixed <code>e</code>
:# <code>((->) t)</code>, for a fixed <code>t</code>
}}

=== Using Applicative Functors ===

The use of <code>(<*>)</code> may not be immediately clear, so let us look at some example that you may have come across yourself.

Suppose we have the following function:

  f :: Int -> Int -> Int
  f x y = 2 * x + y

But instead of <code>Int</code>s, we want to apply this function to values of type <code>Maybe Int</code>. Because you've seen this problem before, you decide to write a function <code>fmap2</code>:

  fmap2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
  fmap2 f (Just x) (Just y) = Just (f x y)
  fmap2 _ _        _        = Nothing

You are happy for a while, but then you find that <code>f</code> really needs another <code>Int</code> argument. But now, <code>fmap2</code> isn't sufficient anymore. You need another function to accommodate for this extra parameter:

  fmap3 :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
  fmap3 f (Just x) (Just y) (Just z) = Just (f x y z)
  fmap3 _ _        _        _        = Nothing

This is all good as such, but what if <code>f</code> suddenly needs 4 arguments, or 5, or 10?

Here is where <code>(<*>)</code> comes in. Look at what happens if you write <code>fmap f</code>:

  f :: (a -> b -> c)
  fmap :: Functor f => (d -> e) -> f d -> f e
  fmap f :: Functor f => f a -> f (b -> c)    -- Identify d with a, and e with (b -> c)

Now the use of <code>(<*>)</code> becomes clear. Once we have the final <code>f (b -> c)</code>, we can use it to get a <code>(f b -> f c)</code>. And indeed:

  fmap2 f a b = f `fmap` a <*> b
  fmap3 f a b c = f `fmap` a <*> b <*> c
  fmap4 f a b c d = f `fmap` a <*> b <*> c <*> d

To make it look more pretty, the {{Haskell lib|Control|Applicative}} library defines <code>(<$>)</code> as a synonym of <code>fmap</code>. The ultimate result is that the code is much cleaner to read and easier to adapt and reuse.

  fmap2 f a b = f <$> a <*> b
  fmap3 f a b c = f <$> a <*> b <*> c
  fmap4 f a b c d = f <$> a <*> b <*> c <*> d

Anytime you feel the need to define different higher order functions to accommodate for function-arguments with a different number of arguments, think about how defining a proper instance of <code>Applicative</code> can make your life easier.

Of course, {{Haskell lib|Control|Applicative}} provides the above functions as convenience, under the names of <code>liftA</code> to <code>liftA3</code>.

== Monads and Applicative Functors ==

The type of <code>pure</code> may look familiar. Indeed, if you change its typeclass restriction from:

  Applicative f => a -> f a

to 

  Monad m => a -> m a

it has exactly the same type as <code>return</code>.

As a matter of fact, every instance of <code>Monad</code> can also be made an instance of <code>Applicative</code>. Here are the definitions that can be used:

  pure  = return
  (<*>) = ap
  
Here, <code>ap</code> is defined as:

  ap f a = do
    f' <- f
    a' <- a
    return (f' a')

It is also defined in {{Haskell lib|Control|Monad}}

{{Exercises|1=
Check that the <code>Applicative</code> instance of <code>Maybe</code> can be obtained by the above transformation.
}}

== ZipLists ==

Let's come back now to the idea that <code>Applicative</code> can make life easier.  Perhaps the best known example of this are the different <code>zipWithN</code> functions of {{Haskell lib|Data|List}}. It looks exactly like the kind of pattern that an applicative functor would be useful for.

For technical reasons, we can not define a <code>Applicative</code> instance for <code>[]</code>, as it already has one defined. This instance does something completely different. <code>fs <*> xs</code> takes all functions from <code>fs</code> and applies them to all values in <code>xs</code>. To remedy this, we create a wrapper:

  newtype ZipList a = ZipList [a]
  
  instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (map f xs)

To make this an instance of <code>Applicative</code> with the expected behaviour, we shall first look at the definition of <code>(<*>)</code>, as it follows quite straightforward from what we want to do. The <code>(<*>)</code> operator takes a list of functions and a list of values, and it should apply the functions to the values in a pairwise way. This sounds a lot like <code>zipWith ($)</code>, we just need to add the <code>ZipList</code> wrapper:

  instance Applicative ZipList where
    (ZipList fs) <*> (ZipList xs) = ZipList $ zipWith ($) fs xs
    pure                          = undefined

Now we only need to define <code>pure</code>. If we define it like this:

  pure x = ZipList [x]

it won't satisfy the Identity law, <code>pure id <*> v = v</code>, since <code>v</code> can contain more than one element, and <code>zipWith</code> only returns a list of the smaller of the two input sizes. Since we don't know how many elements <code>v</code> has, the safest way is to let <code>pure</code> return a list of infinite elements. Now our instance declaration is complete:

  instance Applicative ZipList where
    (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)
    pure x                        = ZipList (repeat x)

== References ==

{{Haskell lib|Control|Applicative}}

{{Haskell navigation|chapter=Advanced Haskell}}
{{Auto category}}
