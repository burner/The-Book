>{{Haskell minitoc|chapter=Fun with Types}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Fun with Types}}

Existential types, or 'existentials' for short, are a way of 'squashing' a group of types into one, single type. 

Firstly, a note to those of you following along at home: existentials are part of GHC's ''type system extensions''. They aren't part of Haskell98, and as such you'll have to either compile any code that contains them with an extra command-line parameter of <code>-XExistentialQuantification</code>, or put <code>{-# LANGUAGE ExistentialQuantification #-}</code> at the top of your sources that use existentials.

== The <code>forall</code> keyword ==
The <code>forall</code> keyword is used to explicitly bring type variables into scope. For example, consider something you've innocuously seen written a hundred times so far:

{{HaskellExample|A polymorphic function|
 map :: (a -> b) -> [a] -> [b]
}}

But what are these <code>a</code> and <code>b</code>? Well, they're type variables, you answer. The compiler sees that they begin with a lowercase letter and as such allows any type to fill that role. Another way of putting this is that those variables are 'universally quantified'. If you've studied formal logic, you will have undoubtedly come across the quantifiers: 'for all' (or <math>\forall</math>) and 'exists' (or <math>\exists</math>). They 'quantify' whatever comes after them: for example, <math>\exists x</math> means that whatever follows is true for at least one value of ''x''. <math>\forall x</math> means that what follows is true for every ''x'' you could imagine. For example, <math>\forall x, \, x^2 \geq 0</math> and <math>\exists x, \, x^3 = 27</math>.

The <code>forall</code> keyword quantifies ''types'' in a similar way. We would rewrite <code>map</code>'s type as follows:

{{HaskellExample|Explicitly quantifying the type variables|
 map :: forall a b. (a -> b) -> [a] -> [b]
}}

So we see that for any <code>a</code> and <code>b</code> we can imagine, <code>map</code> takes the type <code>(a -> b) -> [a] -> [b]</code>. For example, we might choose <code>a = Int</code> and <code>b = String</code>. Then it's valid to say that <code>map</code> has the type <code>(Int -> String) -> [Int] -> [String]</code>. We are ''instantiating'' the general type of <code>map</code> to a more specific type. 

However, in Haskell, as we know, any use of a lowercase type implicitly begins with a <code>forall</code> keyword, so the two type declarations for <code>map</code> are equivalent, as are the declarations below:

{{HaskellExample|Two equivalent type statements|
 id :: a -> a
 id :: forall a . a -> a
}}

What makes life really interesting is that you can override this default behaviour by explicitly telling Haskell where the <code>forall</code> keyword goes.  One use of this is for building '''existentially quantified types''', also known as existential types, or simply existentials. But wait... isn't <code>forall</code> the ''universal'' quantifier? How do you get an existential type out of that? We look at this in a later section. However, first, let's dive right into the deep end by seeing an example of the power of existential types in action.

== Example: heterogeneous lists ==
The premise behind Haskell's typeclass system is grouping types that all share a common property. So if you know a type instantiates some class <code>C</code>, you know certain things about that type. For example, <code>Int</code> instantiates <code>Eq</code>, so we know that elements of <code>Int</code> can be compared for equality.

Suppose we have a group of values, and we don't know if they are all the same type, but we do know they all instantiate some class, i.e. we know all the values have a certain property. It might be useful to throw all these values into a list. We can't do this normally because lists are homogeneous with respect to types: they can only contain a single type. However, existential types allow us to loosen this requirement by defining a 'type hider' or 'type box':

{{HaskellExample|Constructing a heterogeneous list|<pre>
 data ShowBox = forall s. Show s => SB s
 
 heteroList :: [ShowBox]
 heteroList = [SB (), SB 5, SB True]
</pre>}}

We won't explain precisely what we mean by that datatype definition, but its meaning should be clear to your intuition. The important thing is that we're calling the constructor on three values of different types, and we place them all into a list, so we must end up with the same type for each one. Essentially this is because our use of the <code>forall</code> keyword gives our constructor the type <code>SB :: forall s. Show s => s -> ShowBox</code>. If we were now writing a function to which we intend to pass <code>heteroList</code>, we couldn't apply any functions like <code>not</code> to the values inside the <code>SB</code> because they might not be Bools. But we do know something about each of the elements: they can be converted to a string via <code>show</code>. In fact, that's pretty much the only thing we know about them.

{{HaskellExample|Using our heterogeneous list|<pre>
 instance Show ShowBox where
  show (SB s) = show s        -- (*) see the comment in the text below
 
 f :: [ShowBox] -> IO ()
 f xs = mapM_ print xs

 main = f heteroList
</pre>}}

Let's expand on this a bit more. In the definition of <code>show</code> for <code>ShowBox</code> – the line marked with <code>(*) see the comment in the text below </code> – we don't know the type of <code>s</code>. But as we mentioned, we ''do'' know that the type is an instance of Show due to the constraint on the <code>SB</code> constructor. Therefore, it's legal to use the function <code>show</code> on <code>s</code>, as seen in the right-hand side of the function definition. 

As for <code>f</code>, recall the type of print:

{{HaskellExample|Types of the functions involved|<pre>
 print :: Show s => s -> IO () -- print x = putStrLn (show x)
 mapM_ :: (a -> m b) -> [a] -> m ()
 mapM_ print :: Show s => [s] -> IO ()
</pre>}}

As we just declared <code>ShowBox</code> an instance of <code>Show</code>, we can print the values in the list.

== Explaining the term ''existential'' ==
{{Side note|side=right|Since you can get existential types with <code>forall</code>, Haskell forgoes the use of an <code>exists</code> keyword, which would just be redundant.}}

Let's get back to the question we asked ourselves a couple of sections back. Why are we calling these existential types if <code>forall</code> is the universal quantifier?

Firstly, <code>forall</code> really does mean 'for all'. One way of thinking about types is as sets of values with that type, for example, Bool is the set {True, False, &perp;} (remember that bottom, &perp;, is a member of every type!), Integer is the set of integers (and bottom), String is the set of all possible strings (and bottom), and so on. <code>forall</code> serves as an intersection over those sets. For example, <code>forall a. a</code> is the intersection over all types, which must be {&perp;}, that is, the type (i.e. set) whose only value (i.e. element) is bottom. Why? Think about it: how many of the elements of Bool appear in, for example, String? Bottom is the only value common to all types.

A few more examples:

# <code>[forall a. a]</code> is the type of a list whose elements all have the type <code>forall a. a</code>, i.e. a list of bottoms.
# <code>[forall a. Show a => a]</code> is the type of a list whose elements all have the type <code>forall a. Show a => a</code>. The Show class constraint limits the sets you intersect over (here we're only intersecting over instances of Show), but <math>\perp</math> is still the only value common to all these types, so this too is a list of bottoms.
# <code>[forall a. Num a => a]</code>. Again, the list where each element is a member of all types that instantiate Num. This could involve numeric literals, which have the type <code>forall a. Num a => a</code>, as well as bottom.
# <code>forall a. [a]</code> is the type of the list whose elements have some (the same) type a, which can be assumed to be any type at all by a callee (and therefore this too is a list of bottoms).

We see that most intersections over types just lead to combinations of bottoms in some ways, because types don't have a lot of values in common. 

Recall that in the last section, we developed a heterogeneous list using a 'type hider'. Ideally, we'd like the type of a heterogeneous list to be <code>[exists a. a]</code>, i.e. the list where all elements have type <code>exists a. a</code>. This '<code>exists</code>' keyword (which isn't present in Haskell) is, as you may guess, a ''union'' of types, so that <code>[exists a. a]</code> is the type of a list where all elements could take any type at all (and the types of different elements needn't be the same).

But we got almost the same behaviour above using datatypes. Let's declare one.

{{HaskellExample|An existential datatype|<pre>
 data T = forall a. MkT a
</pre>}}

This means that:

{{HaskellExample|The type of our existential constructor|
 MkT :: forall a. a -> T
}}

So we can pass any type we want to <code>MkT</code> and it'll convert it into a T. So what happens when we deconstruct a <code>MkT</code> value?

{{HaskellExample|Pattern matching on our existential constructor|<pre>
 foo (MkT x) = ... -- what is the type of x?
</pre>}}

As we've just stated, <code>x</code> could be of any type. That means it's a member of some arbitrary type, so has the type <code>x :: exists a. a</code>. In other words, our declaration for T is isomorphic to the following one:

{{HaskellExample|An equivalent version of our existential datatype (pseudo-Haskell)|<pre>
 data T = MkT (exists a. a)
</pre>}}

And suddenly we have existential types. Now we can make a heterogeneous list:

{{HaskellExample|Constructing the hetereogeneous list|<pre>
 heteroList = [MkT 5, MkT (), MkT True, MkT map]
</pre>}}

Of course, when we pattern match on <code>heteroList</code> we can't do anything with its elements<ref>Actually, we can apply them to functions whose type is <code>forall a. a -> ''R''</code>, for some arbitrary <code>''R''</code>, as these accept values of any type as a parameter. Examples of such functions: <code>id</code>, <code>const k</code> for any <code>k</code>, <code>seq</code>. So technically, we can't do anything ''useful'' with its elements, except reduce them to WHNF.</ref>, as all we know is that they have some arbitrary type. However, if we are to introduce class constraints:

{{HaskellExample|A new existential datatype, with a class constraint|<pre>
 data T' = forall a. Show a => MkT' a
</pre>}}

Which is isomorphic to:

{{HaskellExample|The new datatype, translated into 'true' existential types|<pre>
 data T' = MkT' (exists a. Show a => a)
</pre>}}

Again the class constraint serves to limit the types we're unioning over, so that now we know the values inside a <code>MkT'</code> are elements of some arbitrary type ''which instantiates Show''. The implication of this is that we can apply <code>show</code> to a value of type <code>exists a. Show a => a</code>. It doesn't matter exactly which type it turns out to be.

{{HaskellExample|Using our new heterogenous setup|<pre>
 heteroList' = [MkT' 5, MkT' (), MkT' True, MkT' "Sartre"]
 main = mapM_ (\(MkT' x) -> print x) heteroList'

 {- prints:
 5
 ()
 True
 "Sartre"
 -}
</pre>}}

To summarise, the interaction of the universal quantifier with datatypes produces existential types. As most interesting applications of <code>forall</code>-involving types use this interaction, we label such types 'existential'. Whenever you want existential types, you must wrap them up in a datatype constructor, they can't exist "out in the open" like with <code>[exists a. a]</code>.

== Example: <code>runST</code> ==
One monad that you may not have come across so far is the ST monad. This is essentially the <code>State</code> monad on steroids: it has a much more complicated structure and involves some more advanced topics. It was originally written to provide Haskell with IO. As we mentioned in the [[../Understanding monads/]] chapter, IO is basically just a State monad with an environment of all the information about the real world. In fact, inside GHC at least, ST is used, and the environment is a type called <code>RealWorld</code>.

To get out of the State monad, you can use <code>runState</code>. The analogous function for ST is called <code>runST</code>, and it has a rather particular type:

{{HaskellExample|The <code>runST</code> function|
 runST :: forall a. (forall s. ST s a) -> a
}}

This is actually an example of a more complicated language feature called rank-2 polymorphism, which we don't go into detail here. It's important to notice that there is no parameter for the initial state. Indeed, ST uses a different notion of state to State; while State allows you to <code>get</code> and <code>put</code> the current state, ST provides an interface to ''references''. You create references, which have type <code>STRef</code>, with <code>newSTRef :: a -> ST s (STRef s a)</code>, providing an initial value, then you can use <code>readSTRef :: STRef s a -> ST s a</code> and <code>writeSTRef :: STRef s a -> a -> ST s ()</code> to manipulate them. As such, the internal environment of a ST computation is not one specific value, but a mapping from references to values. Therefore, you don't need to provide an initial state to runST, as the initial state is just the empty mapping containing no references.

However, things aren't quite as simple as this. What stops you creating a reference in one ST computation, then using it in another? We don't want to allow this because (for reasons of thread-safety) no ST computation should be allowed to assume that the initial internal environment contains any specific references. More concretely, we want the following code to be invalid:

{{HaskellExample|Bad ST code|<pre>
 let v = runST (newSTRef True)
 in runST (readSTRef v)
</pre>}}

What would prevent this? The effect of the rank-2 polymorphism in <code>runST</code>'s type is to ''constrain the scope of the type variable <code>s<code>'' to be within the first parameter. In other words, if the type variable <code>s</code> appears in the first parameter it cannot also appear in the second. Let's take a look at how exactly this is done. Say we have some code like the following:

{{HaskellExample|Briefer bad ST code|
 ... runST (newSTRef True) ...
}}

The compiler tries to fit the types together:

{{HaskellExample|The compiler's typechecking stage|
 newSTRef True :: forall s. ST s (STRef s Bool)
 runST :: forall a. (forall s. ST s a) -> a
 together, forall a. (forall s. ST s (STRef s Bool)) -> STRef s Bool
}}

The importance of the <code>forall</code> in the first bracket is that we can change the name of the <code>s</code>. That is, we could write:

{{HaskellExample|A type mismatch!|
 together, forall a. (forall s'. ST s' (STRef s' Bool)) -> STRef s Bool
}}

This makes sense: in mathematics, saying <math>\forall x. x > 5</math> is precisely the same as saying <math>\forall y. y > 5</math>; you're just giving the variable a different label. However, we have a problem with our above code. Notice that as the <code>forall</code> does ''not'' scope over the return type of <code>runST</code>, we don't rename the <code>s</code> there as well. But suddenly, we've got a type mismatch! The result type of the ST computation in the first parameter must match the result type of <code>runST</code>, but now it doesn't!

The key feature of the existential is that it allows the compiler to generalise the type of the state in the first parameter, and so the result type cannot depend on it. This neatly sidesteps our dependence problems, and 'compartmentalises' each call to <code>runST</code> into its own little heap, with references not being able to be shared between different calls.

== Quantification as a primitive ==
Universal quantification is useful for defining data types that aren't already defined.
Suppose there was no such thing as pairs built into haskell.
Quantification could be used to define them.
 newtype Pair a b=Pair (forall c.(a->b->c)->c)

== Notes ==
<references />

== Further reading ==
* GHC's user guide contains [http://haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#existential-quantification useful information] on existentials, including the various limitations placed on them (which you should know about).
* ''[http://citeseer.ist.psu.edu/launchbury94lazy.html Lazy Functional State Threads], by Simon Peyton-Jones and John Launchbury, is a paper which explains more fully the ideas behind ST.

{{Haskell navigation|chapter=Fun with Types}}

{{Auto category}}
