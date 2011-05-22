>{{Haskell minitoc|chapter=Haskell Performance}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Haskell Performance}}

{{Cquote|Hard work pays off later. Laziness pays off now! – Steven Wright}}

== Introduction ==
By now you are aware that Haskell uses lazy evaluation in the sense that nothing is evaluated until necessary.  The problem is what exactly does "until necessary" mean?  In this chapter, we will see how lazy evaluation works (how little black magic there is), what exactly it means for functional programming, and how to make the best use of it.  But first, let's consider the reasons and implications of lazy evaluation.  At first glance, it is tempting to think that lazy evaluation is meant to make programs more efficient.  After all, what can be more efficient than not doing anything?  This is only true in a superficial sense.  In practice, laziness often introduces an overhead that leads programmers to hunt for places where they can make their code more strict.  The real benefit of laziness is not merely that it makes things efficient, but that ''it makes the right things '''efficient enough'''''.  Lazy evaluation allows us to write simple, elegant code which would simply not be practical in a strict environment. 

{{Haskell stub|sectiononly=1}}

=== Nonstrictness versus Laziness ===
There is a slight difference between ''laziness'' and ''nonstrictness''. '''Nonstrict semantics''' refers to a given property of Haskell programs that you can rely on: nothing will be evaluated until it is needed. '''Lazy evaluation''' is how you implement nonstrictness, using a device called '''thunks''' which we explain in the next section. However, these two concepts are so closely linked that it is beneficial to explain them both together: a knowledge of thunks is useful for understanding nonstrictness, and the semantics of nonstrictness explains why you use lazy evaluation in the first place. As such, we introduce the concepts simultaneously and make no particular effort to keep them from intertwining, with the exception of getting the terminology right.

== Thunks and Weak head normal form ==
There are two principles you need to understand to get how programs execute in Haskell. First, we have the property of nonstrictness: we evaluate as little as possible for as long as possible. Second, Haskell values are highly layered; 'evaluating' a Haskell value could mean evaluating down to any one of these layers. To see what this means, let's walk through a few examples using a pair.

 let (x, y) = (length [1..5], reverse "olleh") in ...

(We'll assume that in the 'in' part, we use <code>x</code> and <code>y</code> somewhere. Otherwise, we're not forced to evaluate the let-binding at all; the right-hand side could have been <code>undefined</code> and it would still work if the 'in' part doesn't mention <code>x</code> or <code>y</code>. This assumption will remain for all the examples in this section.) What do we know about <code>x</code>? Looking at it we can see it's pretty obvious <code>x</code> is <code>5</code> and <code>y</code> "hello"</code>, but remember the first principle: we don't want to evaluate the calls to <code>length</code> and <code>reverse</code> until we're forced to. So okay, we can say that <code>x</code> and <code>y</code> are both '''thunks''': that is, they are ''unevaluated values'' with a ''recipe'' that explains how to evaluate them. For example, for <code>x</code> this recipe says 'Evaluate <code>length [1..5]</code>'. However, we are actually doing some pattern matching on the left hand side. What would happen if we removed that?

 let z = (length [1..5], reverse "olleh") in ...

Although it's still pretty obvious to us that <code>z</code> is a pair, the compiler sees that we're not trying to deconstruct the value on the right-hand side of the '=' sign at all, so it doesn't really care what's there. It lets <code>z</code> be a thunk on its own. Later on, when we try to use <code>z</code>, we'll probably need one or both of the components, so we'll have to evaluate <code>z</code>, but for now, it can be a thunk.

Above, we said Haskell values were layered. We can see that at work if we pattern match on <code>z</code>:

 let z     = (length [1..5], reverse "olleh")
    (n, s) = z 
 in ...

After the first line has been executed, <code>z</code> is simply a thunk. We know nothing about the sort of value it is because we haven't been asked to find out yet. In the second line, however, we pattern match on <code>z</code> using a pair pattern. The compiler thinks 'I better make sure that pattern does indeed match <code>z</code>, and in order to do that, I need to make sure <code>z</code> is a pair.' Be careful, though. We're not as of yet doing anything with the component parts (the calls to <code>length</code> and <code>reverse</code>), so they can remain unevaluated. In other words, <code>z</code>, which was just a thunk, gets evaluated to something like <code>(*thunk*, *thunk*)</code>, and <code>n</code> and <code>s</code> become thunks which, when evaluated, will be the component parts of the original <code>z</code>.

Let's try a slightly more complicated pattern match:

 let z     = (length [1..5], reverse "olleh")
    (n, s) = z 
    'h':ss = s
 in ...

[[Image:Thunk-layers.png|left|frame|Evaluating the value <code>(4, [1, 2])</code> step by step. The first stage is completely unevaluated; all subsequent forms are in WHNF, and the last one is also in normal form.]]

The pattern match on the second component of <code>z</code> causes some evaluation. The compiler wishes to check that the <code>'h':ss</code> pattern matches the second component of the pair. So, it:

* Evaluates the top level of <code>s</code> to ensure it's a cons cell: <code>s = *thunk* : *thunk*</code>. (If <code>s</code> had been an empty list we would encounter an pattern match failure error at this point.)
* Evaluates the first thunk it just revealed to make sure it's 'h': <code>s = 'h' : *thunk*</code>
* The rest of the list stays unevaluated, and <code>ss</code> becomes a thunk which, when evaluated, will be the rest of this list.

So it seems that we can 'partially evaluate' (most) Haskell values. Also, there is some sense of the minimum amount of evaluation we can do. For example, if we have a pair thunk, then the minimum amount of evaluation takes us to the pair constructor with two unevaluated components: <code>(*thunk*, *thunk*)</code>. If we have a list, the minimum amount of evaluation takes us either to a cons cell <code>*thunk* : *thunk*</code> or an empty list <code>[]</code>. Note that in the second case, no more evaluation can be performed on the value; it is said to be in '''normal form'''. If we are at any of the intermediate steps so that we've performed at least some evaluation on a value, it is in '''weak head normal form''' (WHNF). (There is also a 'head normal form', but it's not used in Haskell.) ''Fully'' evaluating something in WHNF reduces it to something in normal form; if at some point we needed to, say, print <code>z</code> out to the user, we'd need to fully evaluate it, including those calls to <code>length</code> and <code>reverse</code>, to <code>(5, "hello")</code>, where it is in normal form. Performing any degree of evaluation on a value is sometimes called '''forcing''' that value.

Note that for some values there is only one. For example, you can't partially evaluate an integer. It's either a thunk or it's in normal form. Furthermore, if we have a constructor with strict components (annotated with an exclamation mark, as with <code>data MaybeS a = NothingS | JustS !a</code>), these components become evaluated as soon as we evaluate the level above. I.e. we can never have <code>JustS *thunk*</code>, as soon as 
we get to this level the strictness annotation on the component of <code>JustS</code> forces us to evaluate the component part.

So in this section we've explored the basics of laziness. We've seen that nothing gets evaluated until it is needed (in fact the ''only'' place that Haskell values get evaluated is in pattern matches, and inside certain primitive IO functions), and that this principle even applies to evaluating values — we do the minimum amount of work on a value that we need to compute our result.

== Lazy and strict functions ==
Functions can be lazy or strict 'in an argument'. Most functions need to do something with their arguments, and this will involve evaluating these arguments to different levels. For example, <code>length</code> needs to evaluate only the cons cells in the argument you give it, not the contents of those cons cells — <code>length *thunk*</code> might evaluate to something like <code>length (*thunk* : *thunk* : *thunk* : [])</code>, which in turn evaluates to <code>3</code>. Others need to evaluate their arguments fully, like <code>(length . show)</code>. If you had <code>length $ show *thunk*</code>, there's no way you can do anything other than evaluate that thunk to normal form.

So some functions evaluate their arguments more fully than others. Given two functions of one parameter, <code>f</code> and <code>g</code>, we say <code>f</code> is stricter than <code>g</code> if <code>f x</code> evaluates <code>x</code> to a deeper level than <code>g x</code>. Often we only care about WHNF, so a function that evaluates its argument to at least WHNF is called ''strict'' and one that performs no evaluation is ''lazy''. What about functions of more than one parameter? Well, we can talk about functions being strict in one parameter, but lazy in another. For example, given a function like the following:

 f x y = length $ show x

Clearly we need to perform no evaluation on <code>y</code>, but we need to evaluate <code>x</code> fully to normal form, so <code>f</code> is strict in its first parameter but lazy in its second.

{{Exercises|1=
# Which is the stricter function?
<pre>
f x = length [head x]
g x = length (tail x)
</pre>
}}

''TODO: explain that it's also about how much of the input we need to consume before we can start producing output. E.g. foldr (:) [] and foldl (flip (:)) [] both evaluate their arguments to the same level of strictness, but foldr can start producing values straight away, whereas foldl needs to evaluate cons cells all the way to the end before it starts anything.''
{{sectstub}}

=== Black-box strictness analysis ===
[[Image:Black-box-strictness.png|right|frame|If <code>f</code> returns an error when passed undefined, it must be strict. Otherwise, it's lazy.]]

Imagine we're given some function <code>f</code> which takes a single parameter. We're not allowed to look at its source code, but we want to know whether <code>f</code> is strict or not. How might we do this? Probably the easiest way is to use the standard Prelude value <code>undefined</code>. Forcing <code>undefined</code> to any level of evaluation will halt our program and print an error, so all of these print errors:

 let (x, y) = undefined in x
 length undefined
 head undefined
 JustS undefined -- Using MaybeS as defined in the last section

So if a function is strict, passing it undefined will result in an error. Were the function lazy, passing it undefined will print no error and we can carry on as normal. For example, none of the following produce errors:

 let (x, y) = (4, undefined) in x
 length [undefined, undefined, undefined]
 head (4 : undefined)
 Just undefined

So we can say that <code>f</code> is a strict function if, and only if, <code>f undefined</code> results in an error being printed and the halting of our program.

=== In the context of nonstrict semantics ===
What we've presented so far makes sense until you start to think about functions like <code>id</code>. Is <code>id</code> strict? Our gut reaction is probably to say "No! It doesn't evaluate its argument, therefore its lazy". However, let's apply our black-box strictness analysis from the last section to <code>id</code>. Clearly, <code>id undefined</code> is going to print an error and halt our program, so shouldn't we say that <code>id</code> is strict? The reason for this mixup is that Haskell's nonstrict semantics makes the whole issue a bit murkier. 

Nothing gets evaluated if it doesn't need to be, according to nonstrictness. In the following code, will <code>length undefined</code> be evaluated?

 [4, 10, length undefined, 12]

If you type this into GHCi, it seems so, because you'll get an error printed. However, our question was something of a trick one; it doesn't make sense to say whether a value gets evaluated, unless we're doing something to this value. Think about it: if we type in <code>head [1, 2, 3]</code> into GHCi, the only reason we have to do any evaluation whatsoever is because GHCi has to print us out the result. Typing <code>[4, 10, length undefined, 12]</code> again requires GHCi to print that list back to us, so it must evaluate it to normal form. In your average Haskell program, nothing at all will be evaluated until we come to perform the IO in <code>main</code>. So it makes no sense to say whether something is evaluated or not unless we know what it's being passed to, one level up.

So when we say "Does <code>f x</code> force <code>x</code>?" what we really mean is "Given that we're forcing <code>f x</code>, does <code>x</code> get forced as a result?". Now we can turn our attention back to <code>id</code>. If we force <code>id x</code> to normal form, then <code>x</code> will be forced to normal form, so we conclude that <code>id</code> is strict. <code>id</code> itself doesn't evaluate its argument, it just hands it on to the caller who will. One way to see this is in the following code:

 -- We evaluate the right-hand of the let-binding to WHNF by pattern-matching
 -- against it.
 let (x, y) = undefined in x -- Error, because we force undefined.
 let (x, y) = id undefined in x -- Error, because we force undefined.

<code>id</code> doesn't "stop" the forcing, so it is strict. Contrast this to a clearly lazy function, <code>const (3, 4)</code>:

 let (x, y) = undefined in x -- Error, because we force undefined.
 let (x, y) = const (3, 4) undefined -- No error, because const (3, 4) is lazy.

=== The denotational view on things ===
If you're familiar with denotational semantics (perhaps you've read the [[Haskell/Denotational semantics|wikibook chapter]] on it?), then the strictness of a function can be summed up very succinctly:

<center>''f'' ⊥ = ⊥ ⇔ ''f'' is strict</center>

Assuming that you say that everything with type <code>forall a. a</code>, including <code>undefined</code>, <code>error "any string"</code>, <code>throw</code> and so on, has denotation &perp;.

== Lazy pattern matching ==
You might have seen pattern matches like the following in Haskell sources.

{{HaskellExample|A lazy pattern match|
<pre>
-- From Control.Arrow
(***) f g ~(x, y) = (f x, g y)
</pre>
}}

The question is: what does the tilde sign (~) mean in the above pattern match? ~ makes a ''lazy pattern'' or ''irrefutable pattern''. Normally, if you pattern match using a constructor as part of the pattern, you have to evaluate any argument passed into that function to make sure it matches the pattern. For example, if you had a function like the above, the third argument would be evaluated when you call the function to make sure the value matches the pattern. (Note that the first and second arguments won't be evaluated, because the patterns <code>f</code> and <code>g</code> match anything. Also it's worth noting that the ''components'' of the tuple won't be evaluated: just the 'top level'. Try <code>let f (Just x) = 1 in f (Just undefined)</code> to see the this.)

However, prepending a pattern with a tilde sign delays the evaluation of the value until the component parts are actually used. But you run the risk that the value might not match the pattern — you're telling the compiler 'Trust me, I know it'll work out'. (If it turns out it doesn't match the pattern, you get a runtime error.) To illustrate the difference:

{{HaskellExample|How ~ makes a difference|
<pre>

Prelude> let f (x,y) = 1
Prelude> f undefined
*** Exception: Prelude.undefined

Prelude> let f ~(x,y) = 1
Prelude> f undefined
1

</pre>
}}

In the first example, the value is evaluated because it has to match the tuple pattern. You evaluate undefined and get undefined, which stops the proceedings. In the latter example, you don't bother evaluating the parameter until it's needed, which turns out to be never, so it doesn't matter you passed it <code>undefined</code>. To bring the discussion around in a circle back to <code>(***)</code>:

{{HaskellExample|How ~ makes a difference with <code>(***)</code>)|
<pre>
Prelude> (const 1 *** const 2) undefined
(1,2)
</pre>
}}

If the pattern weren't irrefutable, the example would have failed.

=== When does it make sense to use lazy patterns? ===
Essentially, when you only have the single constructor for the type, e.g. tuples. Multiple equations won't work nicely with irrefutable patterns. To see this, let's examine what would happen were we to make <code>head</code> have an irrefutable pattern:

{{HaskellExample|Lazier <code>head</code>|2=
<pre>
head' :: [a] -> a
head' ~[]     = undefined
head' ~(x:xs) = x
</pre>
}}

The fact we're using one of these patterns tells us not to evaluate even the top level of the argument until absolutely necessary, so we don't know whether it's an empty list or a cons cell. As we're using an ''irrefutable'' pattern for the first equation, this will match, and the function will always return undefined.

{{Exercises|
* Why won't changing the order of the equations to <code>head'</code> help here?
* If the first equation is changed to use an ordinary refutable pattern, will the behavior of <code>head'</code> still be different from that of <code>head</code>? If so, how?
* More to come...
}}

== Benefits of nonstrict semantics ==
We've been explaining lazy evaluation so far, how nonstrict semantics are actually implemented in terms of thunks. But why is Haskell a nonstrict language in the first place? What advantages are there? In this section, we give some of the benefits of nonstrictness.

{{Haskell stub|sectiononly=1}}

=== Separation of concerns without time penality ===
Lazy evaluation encourages a kind of "what you see is what you get" mentality when it comes to coding. For example, let's say you wanted to find the lowest three numbers in a long list. In Haskell this is achieved extremely naturally: <code>take 3 (sort xs)</code>. However that code in a strict language would be a very bad idea! It would involve computing the entire sorted list, then chopping off all but the first three elements. However, with lazy evaluation we stop once we've sorted the list up to the third element, so this natural definition turns out to be efficient, too (depending on the implementation of sort).

To give a second example, the function <code>[http://haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#v%3AisInfixOf isInfixOf]</code> from Data.List allows you to see if one string is a substring of another string. It's easily definable as:

 isInfixOf :: Eq a => [a] -> [a] -> Bool
 isInfixOf x y = any (isPrefixOf x) (tails y)

Again, this would be suicide in a strict language as to compute all the tails of a list would be very time-consuming. However here we only compute as many as is necessary: once we've found one that <code>x</code> is a prefix of we can stop and return <code>True</code> once away, "shortcutting" the rest of the work. In fact there is a further advantage: we can use an infinite list for <code>y</code>. So, for example, finding out whether a given list is a sequence of consecutive natural numbers is easy: <code>isInfixOf xs [1..]</code>. FIXME: this doesn't work because if xs is NOT a sequence of consecutive natural numbers, the function isInfixOf will run endlessly. ENDFIXME

There are many more examples along these lines.<ref>In general expressions like <code>prune . generate</code>, where <code>generate</code> produces a list of items and <code>prune</code> cuts them down, will be much more efficient in a nonstrict language.</ref> So we can write code that looks and feels natural but doesn't actually incur any time penalty. As one of the highest aims of programming is to write code that is maintainable and clear, this is a big bonus!

However, as always in Computer Science (and in life), a tradeoff exists (in particular, a space-time tradeoff). Using a thunk instead of a plain <code>Int</code>, for a trivial calculation like <code>2+2</code>, can be a waste.
For more examples, see the page on [[../Strictness]].

=== Improved code reuse ===

:''TODO: integrate in.
:''Maybe the above TODO talks about the above section, doesn't it?

Often code reuse is far better.

To show an example, we take again (but in more detail) <code>[http://haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#v%3AisInfixOf isInfixOf]</code> from Data.List. Let's look at the full definition 

{{HaskellExample|1=Laziness helps code reuse|2=
<pre>
-- From the Prelude
or = foldr (||) False
any p = or . map p 
 
-- From Data.List
isPrefixOf []     _      = True
isPrefixOf _      []     = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys 

tails []         = [[]]
tails xss@(_:xs) = xss : tails xs

-- Our function
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf x y = any (isPrefixOf x) (tails y)
</pre>
}}

Where <tt>any</tt>, <tt>isPrefixOf</tt> and <tt>tails</tt> are the functions taken from the <tt>Data.List</tt> library. This function determines if its first parameter, <tt>x</tt> occurs as a subsequence of its second, <tt>y</tt>; when applied on <tt>String</tt>'s (i.e. <tt>[Char]</tt>), it checks if <tt>x</tt> is a substring of <tt>y</tt>. Read in a strict way, it forms the list of all the tails of <tt>y</tt>, then checks them all to see if any of them have <tt>x</tt> as a prefix. In a strict language, writing this function this way (relying on the already-written programs <tt>any</tt>, <tt>isPrefixOf</tt>, and <tt>tails</tt>) would be silly, because it would be far slower than it needed to be. You'd end up doing direct recursion again, or in an imperative language, a couple of nested loops. You might be able to get some use out of <tt>isPrefixOf</tt>, but you certainly wouldn't use <tt>tails</tt>. You might be able to write a usable shortcutting <tt>any</tt>, but it would be more work, since you wouldn't want to use <tt>foldr</tt> to do it.

Now, in a lazy language, all the shortcutting is done for you. You don't end up rewriting foldr to shortcut when you find a solution, or rewriting the recursion done in tails so that it will stop early again. You can reuse standard library code better. Laziness isn't just a constant-factor speed thing, it makes a qualitative impact on the code which is reasonable to write. In fact, it's commonplace to define infinite structures, and then only use as much as is needed, rather than having to mix up the logic of constructing the data structure with code that determines whether any part is needed. Code modularity is increased, as laziness gives you more ways to chop up your code into small pieces, each of which does a simple task of generating, filtering, or otherwise manipulating data.

[http://www.md.chalmers.se/~rjmh/Papers/whyfp.html Why Functional Programming Matters] — largely focuses on examples where laziness is crucial, and provides a strong argument for lazy evaluation being the default.

=== Infinite data structures ===
<i> Examples:

 fibs = 1:1:zipWith (+) fibs (tail fibs)
 "rock-scissors-paper" example from Bird&Wadler
 prune . generate

Infinite data structures usually tie a knot, too, but the Sci-Fi-Explanation of that is better left to the next section. One could move the next section before this one but I think that infinite data structures are simpler than tying general knots</i>

== Common nonstrict idioms ==

=== Tying the knot ===
<i>More practical examples?
 repMin

Sci-Fi-Explanation: "You can borrow things from the future as long as you don't try to change them". Advanced: the "Blueprint"-technique. Examples: the one from the haskellwiki, the one from the mailing list.
</i>

At first a pure functional language seems to have a problem with circular data structures.  Suppose I have a data type like this:

  data Foo a = Foo {value :: a, next :: Foo a}

If I want to create two objects "x" and "y" where "x" contains a reference to "y" and "y" contains a reference to "x" then in a conventional language this is straightforward: create the objects and then set the relevant fields to point to each other:

  -- Not Haskell code
  x := new Foo;
  y := new Foo;
  x.value := 1;
  x.next := y;
  y.value := 2
  y.next := x;

In Haskell this kind of modification is not allowed.  So instead we depend on lazy evaluation:

 circularFoo :: Foo Int 
 circularFoo = x
    where
       x = Foo 1 y
       y = Foo 2 x

This depends on the fact that the "Foo" constructor is a function, and like most functions it gets evaluated lazily.
Only when one of the fields is required does it get evaluated.

It may help to understand what happens behind the scenes here.  When a lazy value is created, for example by a call to "Foo", the compiler generates an internal data structure called a "thunk" containing the function call and arguments.  When the value of the function is demanded the function is called, as you would expect.  But then the thunk data structure is replaced with the return value.  Thus anything else that refers to that value gets it straight away without the need to call the function.

(Note that the Haskell language standard makes no mention of thunks: they are an implementation mechanism.  From the mathematical point of view this is a straightforward example of mutual recursion)

So when I call "circularFoo" the result "x" is actually a thunk.  One of the arguments is a reference to a second thunk representing "y".  This in turn has a reference back to the thunk representing "x".  If I then use the value "next x" this forces the "x" thunk to be evaluated and returns me a reference to the "y" thunk.  If I use the value "next $ next x" then I force the evaluation of both thunks.  So now both thunks have been replaced with the actual "Foo" structures, referring to each other.  Which is what we wanted.

This is most often applied with constructor functions, but it isn't limited just to constructors.  You can just as readily write:

   x = f y
   y = g x

The same logic applies.

=== Memoization, Sharing and Dynamic Programming ===
<i>Dynamic programming with immutable arrays. DP with other finite maps, Hinze's paper "Trouble shared is Trouble halved". Let-floating <code>\x-> let z = foo x in \y -> ... </code>.</i>

== Conclusions about laziness ==

{{Haskell stub|sectiononly=1}}
<i>Move conclusions to the introduction?</i>

* Can make qualitative improvements to performance!
* Can hurt performance in some other cases.
* Makes code simpler.
* Makes hard problems conceivable.
* Allows for separation of concerns with regard to generating and processing data.

== Notes ==
<references />

== References ==

* [http://www.haskell.org/haskellwiki/Performance/Laziness Laziness on the Haskell wiki]
* [http://www.haskell.org/haskellwiki/Haskell/Lazy_Evaluation Lazy evaluation tutorial on the Haskell wiki]

{{Haskell navigation|chapter=Haskell Performance}}
{{Auto category}}
