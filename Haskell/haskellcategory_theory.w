>﻿{{Haskell minitoc|chapter=Wider Theory}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Wider Theory}}

This article attempts to give an overview of category theory, in so far as it applies to Haskell. To this end, Haskell code will be given alongside the mathematical definitions. Absolute rigour is not followed; in its place, we seek to give the reader an intuitive feel for what the concepts of category theory are and how they relate to Haskell.

== Introduction to categories ==
[[Image:Simple-cat.png|left|frame|A simple category, with three objects ''A'', ''B'' and ''C'', three identity morphisms <math>\mathit{id}_A</math>, <math>\mathit{id}_B</math> and <math>\mathit{id}_C</math>, and two other morphisms <math>f : C \to B</math> and <math>g : A \to B</math>. The third element (the specification of how to compose the morphisms) is not shown.]]

A category is, in essence, a simple collection. It has three components:

* A collection of '''objects'''.
* A collection of '''morphisms''', each of which ties two objects (a ''source object'' and a ''target object'') together. (These are sometimes called '''arrows''', but we avoid that term here as it has other connotations in Haskell.) If ''f'' is a morphism with source object ''A'' and target object ''B'', we write <math>f : A \to B</math>.
* A notion of '''composition''' of these morphisms. If ''h'' is the composition of morphisms ''f'' and ''g'', we write <math>h = f \circ g</math>.

Lots of things form categories. For example, '''Set''' is the category of all sets with morphisms as standard functions and composition being standard function composition. (Category names are often typeset in bold face.) '''Grp''' is the category of all groups with morphisms as functions that preserve group operations (the group homomorphisms), i.e. for any two groups ''G'' with operation ''*'' and ''H'' with operation ''·'', a function <math>f : G \to H</math> is a morphism in '''Grp''' iff:

: <math>f(u * v) = f(u) \cdot f(v)</math>

It may seem that morphisms are always functions, but this needn't be the case. For example, any partial order (''P'', <math>\leq</math>) defines a category where the objects are the elements of ''P'', and there is a morphism between any two objects ''A'' and ''B'' iff <math>A \leq B</math>. Moreover, there are allowed to be multiple morphisms with the same source and target objects; using the '''Set''' example, <math>\sin</math> and <math>\cos</math> are both functions with source object <math>\mathbb{R}</math> and target object <math>[-1,1]</math>, but they’re most certainly not the same morphism!

=== Category laws ===

There are three laws that categories need to follow. Firstly, and most simply, the composition of morphisms needs to be '''''associative'''''. Symbolically,

: <math>f \circ (g \circ h) = (f \circ g) \circ h</math>

Secondly, the category needs to be '''''closed''''' under the composition operation. So if <math>f : B \to C</math> and <math>g : A \to B</math>, then there must be some morphism <math>h : A \to C</math> in the category such that <math>h = f \circ g</math>. We can see how this works using the following category:

[[Image:Composition-ex.png|center]]

''f'' and ''g'' are both morphisms so we must be able to compose them and get another morphism in the category. So which is the morphism <math>f \circ g</math>? The only option is <math>\mathit{id}_A</math>. Similarly, we see that <math>g \circ f = \mathit{id}_B</math>.

Lastly, given a category ''C'' there needs to be for every object <code>A</code> an '''''identity''' morphism'', <math>\mathit{id}_A : A \to A</math> that is an identity of composition with other morphisms. Put precisely, for every morphism <math>g : A \to B</math>:

: <math>g \circ \mathit{id}_A = \mathit{id}_B \circ g = g</math>

=== '''Hask''', the Haskell category ===

The main category we’ll be concerning ourselves with in this article is '''Hask''', the category of Haskell types and Haskell functions as morphisms, using <code>(.)</code> for composition: a function <code>f :: A -> B</code> for types <code>A</code> and <code>B</code> is a morphism in '''Hask'''. We can check the first and second law easily: we know <code>(.)</code> is an associative function, and clearly, for any <code>f</code> and <code>g</code>, <code>f . g</code> is another function. In '''Hask''', the identity morphism is <code>id</code>, and we have trivially:

: <code>id . f = f . id = f</code>

<ref>Actually, there is a subtlety here: because <code>(.)</code> is a lazy function, if <code>f</code> is <code>undefined</code>, we have that <code>id . f = \_ -> _|_</code>. Now, while this may seem equivalent to <code>_|_</code> for all intents and purposes, you can actually tell them apart using the strictifying function <code>seq</code>, meaning that the last category law is broken. We can define a new strict composition function, <code>f .! g = ((.) $! f) $! g</code>, that makes '''Hask''' a category. We proceed by using the normal <code>(.)</code>, though, and attribute any discrepancies to the fact that <code>seq</code> breaks an awful lot of the nice language properties anyway.</ref> This isn't an exact translation of the law above, though; we’re missing subscripts. The function <code>id</code> in Haskell is ''polymorphic'' - it can take many different types for its domain and range, or, in category-speak, can have many different source and target objects. But morphisms in category theory are by definition ''monomorphic'' - each morphism has one specific source object and one specific target object. A polymorphic Haskell function can be made monomorphic by specifying its type (''instantiating'' with a monomorphic type), so it would be more precise if we said that the identity morphism from '''Hask''' on a type <code>A</code> is <code>(id :: A -> A)</code>. With this in mind, the above law would be rewritten as:

: <code>(id :: B -> B) . f = f . (id :: A -> A) = f</code>

However, for simplicity, we will ignore this distinction when the meaning is clear.

{{Exercises|
* As was mentioned, any partial order (''P'', <math>\leq</math>) is a category with objects as the elements of ''P'' and a morphism between elements ''a'' and ''b'' iff a <math>\leq</math> b. Which of the above laws guarantees the transitivity of <math>\leq</math>?
* (Harder.) If we add another morphism to the above example, it fails to be a category. Why? Hint: think about associativity of the composition operation.
[[Image:Not-a-cat.png|center]]}}

== Functors ==
[[Image:Functor.png|frame|right|A functor between two categories, <math>\mathbf{C}</math> and <math>\mathbf{D}</math>. Of note is that the objects ''A'' and ''B'' both get mapped to the same object in <math>\mathbf{D}</math>, and that therefore ''g'' becomes a morphism with the same source and target object (but isn't necessarily an identity), and <math>id_A</math> and <math>id_B</math> become the same morphism. The arrows showing the mapping of objects are shown in a dotted, pale olive. The arrows showing the mapping of morphisms are shown in a dotted, pale blue.]]

So we have some categories which have objects and morphisms that relate our objects together. The next Big Topic in category theory is the '''functor''', which relates categories together. A functor is essentially a transformation between categories, so given categories ''C'' and ''D'', a functor <math>F : C \to D</math>:

* Maps any object ''A'' in ''C'' to <math>F(A)</math>, in ''D''.
* Maps morphisms <math>f : A \to B</math> in ''C'' to <math>F(f) : F(A) \to F(B)</math> in ''D''.

One of the canonical examples of a functor is the forgetful functor <math>\mathbf{Grp} \to \mathbf{Set}</math> which maps groups to their underlying sets and group morphisms to the functions which behave the same but are defined on sets instead of groups. Another example is the power set functor <math>\mathbf{Set} \to \mathbf{Set}</math> which maps sets to their power sets and maps functions <math>f : X \to Y</math> to functions <math>\mathcal{P}(X) \to \mathcal{P}(Y)</math> which take inputs <math>U \subset X</math> and return <math>f(U)</math>, the image of ''U'' under ''f'', defined by <math>f(U) = \{ \, f(u) : u \in U \, \}</math>. For any category ''C'', we can define a functor known as the identity functor on ''C'', or <math>1_C : C \to C</math>, that just maps objects to themselves and morphisms to themselves. This will turn out to be useful in the [[#The monad laws and their importance|monad laws]] section later on.

Once again there are a few axioms that functors have to obey. Firstly, given an identity morphism <math>id_A</math> on an object ''A'', <math>F(id_A)</math> must be the identity morphism on <math>F(A)</math>, i.e.:

: <math>F(id_A) = id_{F(A)}</math>

Secondly functors must distribute over morphism composition, i.e.

: <math>F(f \circ g) = F(f) \circ F(g)</math>

{{Exercises|For the diagram given on the right, check these functor laws.}}

=== Functors on '''Hask''' ===
The Functor typeclass you will probably have seen in Haskell does in fact tie in with the categorical notion of a functor. Remember that a functor has two parts: it maps objects in one category to objects in another and morphisms in the first category to morphisms in the second. Functors in Haskell are from '''Hask''' to ''func'', where ''func'' is the subcategory of '''Hask''' defined on just that functor's types. E.g. the list functor goes from '''Hask''' to '''Lst''', where '''Lst''' is the category containing only ''list types'', that is, <code>[T]</code> for any type <code>T</code>. The morphisms in '''Lst''' are functions defined on list types, that is, functions <code>[T] -> [U]</code> for types <code>T</code>, <code>U</code>. How does this tie into the Haskell typeclass Functor? Recall its definition:

 class Functor (f :: * -> *) where
   fmap :: (a -> b) -> (f a -> f b)

Let's have a sample instance, too:

 instance Functor Maybe where
   fmap f (Just x) = Just (f x)
   fmap _ Nothing  = Nothing

Here's the key part: the ''type constructor'' Maybe takes any type <code>T</code> to a new type, <code>Maybe T</code>. Also, <code>fmap</code> restricted to Maybe types takes a function <code>a -> b</code> to a function <code>Maybe a -> Maybe b</code>. But that's it! We've defined two parts, something that takes objects in '''Hask''' to objects in another category (that of Maybe types and functions defined on Maybe types), and something that takes morphisms in '''Hask''' to morphisms in this category. So Maybe is a functor.

A useful intuition regarding Haskell functors is that they represent types that can be mapped over. This could be a list or a Maybe, but also more complicated structures like trees. A function that does some mapping could be written using <code>fmap</code>, then any functor structure could be passed into this function. E.g. you could write a generic function that covers all of Data.List.map, Data.Map.map, Data.Array.IArray.amap, and so on.

What about the functor axioms? The polymorphic function <code>id</code> takes the place of <math>id_A</math> for any ''A'', so the first law states:

 fmap id = id

With our above intuition in mind, this states that mapping over a structure doing nothing to each element is equivalent to doing nothing overall. Secondly, morphism composition is just <code>(.)</code>, so

 fmap (f . g) = fmap f . fmap g

This second law is very useful in practice. Picturing the functor as a list or similar container, the right-hand side is a two-pass algorithm: we map over the structure, performing <code>g</code>, then map over it again, performing <code>f</code>. The functor axioms guarantee we can transform this into a single-pass algorithm that performs <code>f . g</code>. This is a process known as ''fusion''.

{{Exercises|Check the laws for the Maybe and list functors.}}

=== Translating categorical concepts into Haskell ===

Functors provide a good example of how category theory gets translated into Haskell. The key points to remember are that:

* We work in the category '''Hask''' and its subcategories.
* Objects are types.
* Morphisms are functions.
* Things that take a type and return another type are type constructors.
* Things that take a function and return another function are higher-order functions.
* Typeclasses, along with the polymorphism they provide, make a nice way of capturing the fact that in category theory things are often defined over a number of objects at once.

== Monads ==

[[Image:Unit-join.png|frame|right|''unit'' and ''join'', the two morphisms that must exist for every object for a given monad.]]

Monads are obviously an extremely important concept in Haskell, and in fact they originally came from category theory. A ''monad'' is a special type of functor, from a category to that same category, that supports some additional structure. So, down to definitions. A monad is a functor <math>M : C \to C</math>, along with two morphisms<ref>Experienced category theorists will notice that we're simplifying things a bit here; instead of presenting ''unit'' and ''join'' as natural transformations, we treat them explicitly as morphisms, and require naturality as extra axioms alongside the [[#The monad laws|the standard monad laws]] (laws 3 and 4). The reasoning is simplicity; we are not trying to teach category theory as a whole, simply give a categorical background to some of the structures in Haskell. You may also notice that we are giving these morphisms names suggestive of their Haskell analogues, because the names <math>\eta</math> and <math>\mu</math> don’t provide much intuition.</ref> for every object ''X'' in ''C'':

* <math>\mathit{unit}^M_X : X \to M(X)</math>
* <math>\mathit{join}^M_X : M(M(X)) \to M(X)</math>

When the monad under discussion is obvious, we’ll leave out the ''M'' superscript for these functions and just talk about <math>\mathit{unit}_X</math> and <math>\mathit{join}_X</math> for some ''X''.

Let’s see how this translates to the Haskell typeclass Monad, then.

 class Functor m => Monad m where
   return :: a -> m a
   (>>=)  :: m a -> (a -> m b) -> m b

The class constraint of <code>Functor m</code> ensures that we already have the functor structure: a mapping of objects and of morphisms. <code>return</code> is the (polymorphic) analogue to <math>\mathit{unit}_X</math> for any ''X''. But we have a problem. Although <code>return</code>’s type looks quite similar to that of ''unit''; the other function, <code>(>>=)</code>, often called ''bind'', bears no resemblance to ''join''. There is however another monad function, <code>join :: Monad m => m (m a) -> m a</code>, that looks quite similar. Indeed, we can recover <code>join</code> and <code>(>>=)</code> from each other:

 join :: Monad m => m (m a) -> m a
 join x = x >>= id
 
 (>>=) :: Monad m => m a -> (a -> m b) -> m b
 x >>= f = join (fmap f x)

So specifying a monad’s <code>return</code>, <code>fmap</code>, and <code>join</code> is equivalent to specifying its <code>return</code> and <code>(>>=)</code>. It just turns out that the normal way of defining a monad in category theory is to give ''unit'' and ''join'', whereas Haskell programmers like to give <code>return</code> and <code>(>>=)</code>.<ref>This is perhaps due to the fact that Haskell programmers like to think of monads as a way of sequencing computations with a common feature, whereas in category theory the container aspect of the various structures is emphasised. <code>join</code> pertains naturally to containers (squashing two layers of a container down into one), but <code>(>>=)</code> is the natural sequencing operation (do something, feeding its results into something else).</ref> Often, the categorical way makes more sense. Any time you have some kind of structure ''M'' and a natural way of taking any object ''X'' into <math>M(X)</math>, as well as a way of taking <math>M(M(X))</math> into <math>M(X)</math>, you probably have a monad. We can see this in the following example section.

=== Example: the powerset functor is also a monad ===
The power set functor <math>P : \mathbf{Set} \to \mathbf{Set}</math> described above forms a monad. For any set ''S'' you have a <math>\mathit{unit}_S(x) = \{x\}</math>, mapping elements to their singleton set. Note that each of these singleton sets are trivially a subset of ''S'', so <math>\mathit{unit}_S</math> returns elements of the powerset of ''S'', as is required. Also, you can define a function <math>\mathit{join}_S</math> as follows: we receive an input <math>L \in \mathcal{P}(\mathcal{P}(S))</math>. This is:

* A member of the powerset of the powerset of ''S''.
* So a member of the set of all subsets of the set of all subsets of ''S''.
* So a set of subsets of ''S''

We then return the union of these subsets, giving another subset of ''S''. Symbolically, 

: <math>\mathit{join}_S(L) = \bigcup L</math>. 

Hence ''P'' is a monad <ref>If you can prove that certain laws hold, which we'll explore in the next section.</ref>.

In fact, ''P'' is almost equivalent to the list monad; with the exception that we're talking lists instead of sets, they're almost the same. Compare:

{| class="wikitable" width="100%"

! colspan="2" width="50%" | Power set functor on '''Set'''
! colspan="2" width="50%" | List monad from Haskell
|-
! Function type
! Definition
! Function type
! Definition

|-

| colspan="2" | Given a set ''S'' and a morphism <math>f : A \to B</math>:
| colspan="2" | Given a type <code>T</code> and a function <code>f :: A -> B</code>

|-

| <math>P(f) : \mathcal{P}(A) \to \mathcal{P}(B)</math>
| <math>(P(f))(S) = \{ f(a) : a \in S \}</math>

| <code>fmap f :: [A] -> [B]</code>
| <code>fmap f xs = <nowiki>[ f a | a <- xs ]</nowiki></code>

|-

| <math>\mathit{unit}_S : S \to \mathcal{P}(S)</math>
| <math>\mathit{unit}_S(x) = \{ x \}</math>

| <code>return :: T -> [T]</code>
| <code>return x = [x]</code>

|-

| <math>\mathit{join}_S : \mathcal{P}(\mathcal{P}(S)) \to \mathcal{P}(S)</math>
| <math>\mathit{join}_S(L) = \bigcup L</math>

| <code>join :: <nowiki>[[T]]</nowiki> -> [T]</code>
| <code>join xs = concat xs</code>

|}

== The monad laws and their importance ==
Just as functors had to obey certain axioms in order to be called functors, monads have a few of their own. We'll first list them, then translate to Haskell, then see why they’re important.

Given a monad <math>M : C \to C</math> and a morphism <math>f : A \to B</math> for <math>A, B \in C</math>,
# <math>\mathit{join} \circ M(\mathit{join}) = \mathit{join} \circ \mathit{join}</math>
# <math>\mathit{join} \circ M(\mathit{unit}) = \mathit{join} \circ \mathit{unit} = \mathit{id}</math>
# <math>\mathit{unit} \circ f = M(f) \circ \mathit{unit}</math>
# <math>\mathit{join} \circ M(M(f)) = M(f) \circ \mathit{join}</math>

By now, the Haskell translations should be hopefully self-explanatory:

# <code>join . fmap join     = join . join</code>
# <code>join . fmap return   = join . return = id</code>
# <code>return . f           = fmap f . return</code>
# <code>join . fmap (fmap f) =  fmap f . join</code>

(Remember that <code>fmap</code> is the part of a functor that acts on morphisms.) These laws seem a bit impenetrable at first, though. What on earth do these laws mean, and why should they be true for monads? Let’s explore the laws.

=== The first law ===
<code>join . fmap join = join . join</code>
[[Image:Monad-law-1-lists.png|frame|right|A demonstration of the first law for lists. Remember that <code>join</code> is <code>concat</code> and <code>fmap</code> is <code>map</code> in the list monad.]]

In order to understand this law, we'll first use the example of lists. The first law mentions two functions, <code>join . fmap join</code> (the left-hand side) and <code>join . join</code> (the right-hand side). What will the types of these functions be? Remembering that <code>join</code>’s type is <code><nowiki>[[a]]</nowiki> -> [a]</code> (we’re talking just about lists for now), the types are both <code>[[[a]]] -> [a]</code> (the fact that they’re the same is handy; after all, we’re trying to show they’re completely the same function!). So we have a list of list of lists. The left-hand side, then, performs <code>fmap join</code> on this 3-layered list, then uses <code>join</code> on the result. <code>fmap</code> is just the familiar <code>map</code> for lists, so we first map across each of the list of lists inside the top-level list, concatenating them down into a list each. So afterward, we have a list of lists, which we then run through <code>join</code>. In summary, we 'enter' the top level, collapse the second and third levels down, then collapse this new level with the top level.

What about the right-hand side? We first run <code>join</code> on our list of list of lists. Although this is three layers, and you normally apply a two-layered list to <code>join</code>, this will still work, because a <code>[[[a]]]</code> is just <code><nowiki>[[b]]</nowiki></code>, where <code>b = [a]</code>, so in a sense, a three-layered list is just a two layered list, but rather than the last layer being 'flat', it is composed of another list. So if we apply our list of lists (of lists) to <code>join</code>, it will flatten those outer two layers into one. As the second layer wasn't flat but instead contained a third layer, we will still end up with a list of lists, which the other <code>join</code> flattens. Summing up, the left-hand side will flatten the inner two layers into a new layer, then flatten this with the outermost layer. The right-hand side will flatten the outer two layers, then flatten this with the innermost layer. These two operations should be equivalent. It’s sort of like a law of associativity for <code>join</code>.

<code>Maybe</code> is also a monad, with

 return :: a -> Maybe a
 return x = Just x
 
 join :: Maybe (Maybe a) -> Maybe a
 join Nothing         = Nothing
 join (Just Nothing)  = Nothing
 join (Just (Just x)) = Just x

So if we had a ''three''-layered Maybe (i.e., it could be <code>Nothing</code>, <code>Just Nothing</code>, <code>Just (Just Nothing)</code> or <code>Just (Just (Just x))</code>), the first law says that collapsing the inner two layers first, then that with the outer layer is exactly the same as collapsing the outer layers first, then that with the innermost layer. 

{{Exercises|Verify that the list and Maybe monads do in fact obey this law with some examples to see precisely how the layer flattening works.}}

=== The second law ===
<code>join . fmap return = join . return = id</code>

What about the second law, then? Again, we'll start with the example of lists. Both functions mentioned in the second law are functions <code>[a] -> [a]</code>. The left-hand side expresses a function that maps over the list, turning each element <code>x</code> into its singleton list <code>[x]</code>, so that at the end we’re left with a list of singleton lists. This two-layered list is flattened down into a single-layer list again using the <code>join</code>. The right hand side, however, takes the entire list <code>[x, y, z, ...]</code>, turns it into the singleton list of lists <code><nowiki>[[x, y, z, ...]]</nowiki></code>, then flattens the two layers down into one again. This law is less obvious to state quickly, but it essentially says that applying <code>return</code> to a monadic value, then <code>join</code>ing the result should have the same effect whether you perform the <code>return</code> from inside the top layer or from outside it.

{{Exercises|Prove this second law for the Maybe monad.}}

=== The third and fourth laws ===
<code>return . f = fmap f . return</code>

<code>join . fmap (fmap f) = fmap f . join</code>

The last two laws express more self evident fact about how we expect monads to behave. The easiest way to see how they are true is to expand them to use the expanded form:

# <code>\x -> return (f x)  =  \x -> fmap f (return x)</code>
# <code>\x -> join (fmap (fmap f) x)  =  \x -> fmap f (join x)</code>

{{Exercises|Convince yourself that these laws should hold true for any monad by exploring what they mean, in a similar style to how we explained the first and second laws.}}

=== Application to do-blocks ===
Well, we have intuitive statements about the laws that a monad must support, but why is that important? The answer becomes obvious when we consider do-blocks. Recall that a do-block is just syntactic sugar for a combination of statements involving <code>(>>=)</code> as witnessed by the usual translation:

 do { x }                 -->  x
 do { let { y = v }; x }  -->  let y = v in do { x }
 do { v <- y; x }         -->  y >>= \v -> do { x }
 do { y; x }              -->  y >>= \_ -> do { x }

Also notice that we can prove what are normally quoted as the monad laws using <code>return</code> and <code>(>>=)</code> from our above laws (the proofs are a little heavy in some cases, feel free to skip them if you want to):

<ol><li>

<code>return x >>= f  =  f x</code>. Proof:
<pre>   return x >>= f
 = join (fmap f (return x)) -- By the definition of (>>=)
 = join (return (f x))      -- By law 3
 = (join . return) (f x)
 = id (f x)                 -- By law 2
 = f x
</pre>

</li><li>

<code>m >>= return  =  m</code>. Proof:
   m >>= return
 = join (fmap return m)    -- By the definition of (>>=)
 = (join . fmap return) m
 = id m                    -- By law 2
 = m

</li><li>

<code>(m >>= f) >>= g  =  m >>= (\x -> f x >>= g)</code>. Proof (recall that <code>fmap f . fmap g = fmap (f . g)</code>):
   (m >>= f) >>= g
 = (join (fmap f m)) >>= g                          -- By the definition of (>>=)
 = join (fmap g (join (fmap f m)))                  -- By the definition of (>>=)
 = (join . fmap g) (join (fmap f m))
 = (join . fmap g . join) (fmap f m)
 = (join . join . fmap (fmap g)) (fmap f m)         -- By law 4
 = (join . join . fmap (fmap g) . fmap f) m
 = (join . join . fmap (fmap g . f)) m              -- By the distributive law of functors
 = (join . join . fmap (\x -> fmap g (f x))) m
 = (join . fmap join . fmap (\x -> fmap g (f x))) m -- By law 1
 = (join . fmap (join . (\x -> fmap g (f x)))) m    -- By the distributive law of functors
 = (join . fmap (\x -> join (fmap g (f x)))) m
 = (join . fmap (\x -> f x >>= g)) m                -- By the definition of (>>=)
 = join (fmap (\x -> f x >>= g) m)
 = m >>= (\x -> f x >>= g)                          -- By the definition of (>>=)

</li></ol>

These new monad laws, using <code>return</code> and <code>(>>=)</code>, can be translated into do-block notation.

{| class="wikitable" style="margin: 1em auto"

! Points-free style
! Do-block style

|-

| <code>return x >>= f = f x</code>
| <code>do { v <- return x; f v } = do { f x }</code>

|-

| <code>m >>= return = m</code>
| <code>do { v <- m; return v } = do { m }</code>

|-

| <code>(m >>= f) >>= g = m >>= (\x -> f x >>= g)</code>
| 
<pre>   do { y <- do { x <- m; f x };
        g y }
 =
   do { x <- m;
        y <- f x;
        g y }
</pre>

|}

The monad laws are now common-sense statements about how do-blocks should function. If one of these laws were invalidated, users would become confused, as you couldn't be able to manipulate things within the do-blocks as would be expected. The monad laws are, in essence, usability guidelines.

{{Exercises|1=
In fact, the two versions of the laws we gave:
 -- Categorical:
 join . fmap join = join . join
 join . fmap return = join . return = id
 return . f = fmap f . return
 join . fmap (fmap f) = fmap f . join
 
 -- Functional:
 m >>= return = m
 return m >>= f = f m
 (m >>= f) >>= g = m >>= (\x -> f x >>= g)
are entirely equivalent. We showed that we can recover the functional laws from the categorical ones. Go the other way; show that starting from the functional laws, the categorical laws hold. It may be useful to remember the following definitions:
 join m = m >>= id
 fmap f m = m >>= return . f
Thanks to Yitzchak Gale for suggesting this exercise.
}}

== Summary ==
We've come a long way in this chapter. We've looked at what categories are and how they apply to Haskell. We've introduced the basic concepts of category theory including functors, as well as some more advanced topics like monads, and seen how they're crucial to idiomatic Haskell. We haven't covered some of the basic category theory that wasn't needed for our aims, like natural transformations, but have instead provided an intuitive feel for the categorical grounding behind Haskell's structures.

== Notes ==
<references />

{{Haskell navigation|chapter=Wider Theory}}
[[Category:Haskell|Category theory]]
