>{{Haskell minitoc|chapter=Fun with Types}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Fun with Types}}

== Parametric Polymorphism ==
Section goal = short, enables reader to read code (ParseP) with &forall; and use libraries (ST) without horror. Question [[Talk:Haskell/The_Curry-Howard_isomorphism#Polymorphic types]] would be solved by this section.

Link to the following paper: Luca Cardelli: [http://lucacardelli.name/Papers/OnUnderstanding.A4.pdf On Understanding Types, Data Abstraction, and Polymorphism].

=== <code>forall a</code> ===
As you may know, a '''polymorphic''' function is a function that works for many different types. For instance,

<source lang="haskell">
  length :: [a] -> Int
</source>

can calculate the length of any list, be it a string <code>String = [Char]</code> or a list of integers <code>[Int]</code>. The '''type variable''' <code>a</code> indicates that <code>length</code> accepts any element type. Other examples of polymorphic functions are

<source lang="haskell">
  fst :: (a, b) -> a
  snd :: (a, b) -> b
  map :: (a -> b) -> [a] -> [b]
</source>

Type variables always '''begin in lowercase''' whereas concrete types like <code>Int</code> or <code>String</code> always start with an uppercase letter, that's how we can tell them apart.

There is a more '''explicit''' way to indicate that <code>a</code> can be any type

<source lang="haskell">
 length :: forall a. [a] -> Int
</source>

In other words, "for all types <code>a</code>, the function <code>length</code> takes a list of elements of type <code>a</code> and returns an integer". You should think of the old signature as an abbreviation for the new one with the <code>forall</code><ref>Note that the keyword <code>forall</code> is not part of the Haskell 98 standard, but any of the language extensions <code>ScopedTypeVariables</code>, <code>Rank2Types</code> or <code>RankNTypes</code> will enable it in the compiler. A future Haskell standard will incorporate one of these.</ref>. That is, the compiler will internally insert any missing <code>forall</code> for you. Another example: the types signature for <code>fst</code> is really a shorthand for

<source lang="haskell">
  fst :: forall a. forall b. (a,b) -> a
</source>

or equivalently

<source lang="haskell">
  fst :: forall a b. (a,b) -> a
</source>

Similarly, the type of <code>map</code> is really

<source lang="haskell">
  map :: forall a b. (a -> b) -> [a] -> [b]
</source>

The idea that something is applicable to every type or holds for everything is called '''[[w:Universal quantification|universal quantification]]'''. In mathematical logic, the symbol &forall;<ref>The <code>UnicodeSyntax</code> extension allows you to use the symbol &forall; instead of the <code>forall</code> keyword in your Haskell source code.</ref> (an upside-down A, read as "forall") is commonly used for that, it is called the '''universal quantifier'''.

=== Higher rank types ===
With explicit <code>forall</code>, it now becomes possible to write functions that expect '''polymorphic arguments''', like for instance

<source lang="haskell">
  foo :: (forall a. a -> a) -> (Char,Bool)
  foo f = (f 'c', f True)
</source>

Here, <code>f</code> is a polymorphic function, it can be applied to anything. In particular, <code>foo</code> can apply it to both the character <code>'c'</code> and the boolean <code>True</code>.

It is not possible to write a function like <code>foo</code> in Haskell98, the type checker will complain that <code>f</code> may only be applied to values of either the type <code>Char</code> or the type <code>Bool</code> and reject the definition. The closest we could come to the type signature of <code>foo</code> would be

<source lang="haskell">
  bar :: (a -> a) -> (Char, Bool)
</source>

which is the same as

<source lang="haskell">
  bar :: forall a. ((a -> a) -> (Char, Bool))
</source>

But this is very different from <code>foo</code>. The <code>forall</code> at the outermost level means that <code>bar</code> promises to work with any argument <code>f</code> as long as <code>f</code> has the shape <code>a -> a</code> for some type <code>a</code> unknown to <code>bar</code>. Contrast this with <code>foo</code>, where it's the argument <code>f</code> who promises to be of shape <code>a -> a</code> for all types <code>a</code> at the same time , and it's <code>foo</code> who makes use of that promise by choosing both <code>a = Char</code> and <code>a = Bool</code>.

Concerning nomenclature, simple polymorphic functions like <code>bar</code> are said to have a rank-1 type while the type <code>foo</code> is classified as '''rank-2 type'''. In general, a '''rank-n type''' is a function that has at least one rank-(n-1) argument but no arguments of even higher rank.


The theoretical basis for higher rank types is '''[[w:System F|System F]]''', also known as the second-order lambda calculus. We will detail it in the section [[#System F|System F]] in order to better understand the meaning of <code>forall</code> and its placement like in <code>foo</code> and <code>bar</code>.

Haskell98 is based on the [[w:Hindley-Milner|Hindley-Milner]] type system, which is a restriction of System F and does not support <code>forall</code> and rank-2 types or types of even higher rank. You have to enable the <code>RankNTypes</code><ref>Or enable just <code>Rank2Types</code> if you only want to use rank-2 types</ref> language extension to make use of the full power of System F.

But of course, there is a good reason that Haskell98 does not support higher rank types: type inference for the full System F is undecidable, the programmer would have to write down all type signatures himself. Thus, the early versions of Haskell have adopted the Hindley-Milner type system which only offers simple polymorphic function but enables complete type inference in return. Recent advances in research have reduced the burden of writing type signatures and made rank-n types practical in current Haskell compilers.

=== <code>runST</code> ===
For the practical Haskell programmer, the [http://www.haskell.org/haskellwiki/Monad/ST ST monad] is probably the first example of a rank-2 type in the wild. Similar to the IO monad, it offers mutable references

<source lang="haskell">
  newSTRef   :: a -> ST s (STRef s a)
  readSTRef  :: STRef s a -> ST s a
  writeSTRef :: STRef s a -> a -> ST s ()
</source>

and mutable arrays. The type variable <code>s</code> represents the state that is being manipulated. But unlike IO, these stateful computations can be used in pure code. In particular, the function

<source lang="haskell">
  runST :: (forall s. ST s a) -> a
</source>

sets up the initial state, runs the computation, discards the state and returns the result. As you can see, it has a rank-2 type. Why?

The point is that mutable references should be local to one <code>runST</code>. For instance,

<source lang="haskell">
  v   = runST (newSTRef "abc")
  foo = runST (readVar v)
</source>

is wrong because a mutable reference created in the context of one <code>runST</code> is used again in a second <code>runST</code>. In other words, the result type <code>a</code> in <code>(forall s. ST s a) -> a</code> may not be a reference like <code>STRef s String</code> in the case of <code>v</code>. But the rank-2 type guarantees exactly that! Because the argument must be polymorphic in <code>s</code>, it has to return one and the same type <code>a</code> for all states <code>s</code>; the result <code>a</code> may not depend on the state. Thus, the unwanted code snippet above contains a type error and the compiler will reject it.

You can find a more detailed explanation of the ST monad in the original paper [http://www.dcs.gla.ac.uk/fp/papers/lazy-functional-state-threads.ps.Z Lazy functional state threads]<ref>{{cite paper|author=John Launchbury|coauthor=Simon Peyton Jones|title=Lazy functional state threads|date=1994-??-??|pages=24-35|publisher=ACM Press"|url=http://www.dcs.gla.ac.uk/fp/papers/lazy-functional-state-threads.ps.Z}}</ref>.

=== Impredicativity ===

* ''predicative'' = type variables instantiated to ''monotypes''. ''impredicative'' = also ''polytypes''. Example: <code>length [id :: forall a . a -> a]</code> or <code>Just (id :: forall a. a -> a)</code>. Subtly different from higher-rank.

* relation of polymorphic types by their generality, i.e. `isInstanceOf`.
* [http://thread.gmane.org/gmane.comp.lang.haskell.cafe/40508/focus=40610 haskell-cafe: RankNTypes short explanation.]

== System F ==
Section goal = a little bit lambda calculus foundation to prevent brain damage from implicit type parameter passing.

* System F = Basis for all this &forall;-stuff.
* Explicit type applications i.e. <code>map Int (+1) [1,2,3]</code>. &forall; similar to the function arrow ->.
* Terms depend on types. Big &Lambda; for type arguments, small &lambda; for value arguments.

== Examples ==
Section goal = enable reader to judge whether to use data structures with &forall; in his <u>own</u> code.

* Church numerals, Encoding of arbitrary recursive types (positivity conditions): <code>&forall x. (F x -> x) -> x</code>
* Continuations, Pattern-matching: <code>maybe</code>, <code>either</code> and <code>foldr</code>

I.e. &forall; can be put to good use for implementing data types in Haskell.

== Other forms of Polymorphism ==
Section goal = contrast polymorphism in OOP and stuff. how type classes fit in.

* ''ad-hoc polymorphism'' = different behavior depending on type s. => Haskell type classes.
* ''parametric polymorphism'' = ignorant of the type actually used. => &forall;
* ''subtyping''


== Free Theorems ==
Secion goal = enable reader to come up with free theorems. no need to prove them, intuition is enough.
* free theorems for parametric polymorphism.

== Footnotes ==
<references/>

== See also ==
* Luca Cardelli. [http://lucacardelli.name/Papers/OnUnderstanding.A4.pdf On Understanding Types, Data Abstraction, and Polymorphism].

{{Haskell stub}}
{{Haskell navigation|chapter=Fun with Types}}
{{Auto category}}
