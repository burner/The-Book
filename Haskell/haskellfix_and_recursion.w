>{{Haskell minitoc|chapter=Wider Theory}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Wider Theory}}

The <code>fix</code> function is a particularly weird-looking function when you first see it. However, it is useful for one main theoretical reason: introducing it into the (typed) lambda calculus as a primitive allows you to define recursive functions.

== Introducing <code>fix</code> ==
Let's have the definition of <code>fix</code> before we go any further:

 fix :: (a -> a) -> a
 fix f = f (fix f)

This immediately seems quite magical. Surely <code>fix f</code> will yield an infinite application stream of <code>f</code>s: <code>f (f (f (... )))</code>? The resolution to this is our good friend, ''lazy evaluation''. Essentially, this sequence of applications of <code>f</code> will converge to a value if (and only if) <code>f</code> is a lazy function. Let's see some examples:

{{HaskellExample|<code>fix</code> examples|
 Prelude> :m Control.Monad.Fix
 Prelude Control.Monad.Fix> fix (2+)
 *** Exception: stack overflow
 Prelude Control.Monad.Fix> fix (const "hello")
 "hello"
 Prelude Control.Monad.Fix> fix (1:)
 [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,...
}}

We first import the <code>Control.Monad.Fix</code> module to bring <code>fix</code> into scope (this is also available in the <code>Data.Function</code>). Then we try some examples. Since the definition of <code>fix</code> is so simple, let's expand our examples to explain what happens:

   fix (2+)
 = 2 + (fix (2+))
 = 2 + (2 + fix (2+))
 = 4 + (fix (2+))
 = 4 + (2 + fix (2+))
 = 6 + fix (2+)
 = ...

It's clear that this will never converge to any value. Let's expand the next example:

   fix (const "hello")
 = const "hello" (fix (const "hello"))
 = "hello"

This is quite different: we can see after one expansion of the definition of <code>fix</code> that because <code>const</code> ignores its second argument, the evaluation concludes. The evaluation for the last example is a little different, but we can proceed similarly:

   fix (1:)
 = 1 : fix (1:)
 = 1 : (1 : fix (1:))
 = 1 : (1 : (1 : fix (1:)))

Although this similarly looks like it'll never converge to a value, keep in mind that when you type <code>fix (1:)</code> into GHCi, what it's really doing is applying <code>show</code> to that. So we should look at how <code>show (fix (1:))</code> evaluates (for simplicity, we'll pretend <code>show</code> on lists doesn't put commas between items):

   show (fix (1:))
 = "[" ++ map show (fix (1:)) ++ "]"
 = "[" ++ map show (1 : fix (1:)) ++ "]"
 = "[" ++ "1" ++ map show (fix (1:)) ++ "]"
 = "[" ++ "1" ++ "1" ++ map show (fix (1:)) ++ "]"

So although the <code>map show (fix (1:))</code> will never terminate, it does produce output: GHCi can print the beginning of the string, <code>"[" ++ "1" ++ "1"</code>, and continue to print more as <code>map show (fix (1:))</code> produces more. This is lazy evaluation at work: the printing function doesn't need to consume its entire input string before beginning to print, it does so as soon as it can start.

{{Exercises|
What, if anything, will the following expressions converge to?
* <code>fix ("hello"++)</code>
* <code>fix (\x -> cycle (1:x))</code>
* <code>fix reverse</code>
* <code>fix id</code>
* <code>fix (\x -> take 2 $ cycle (1:x))</code>
}}

== <code>fix</code> and fixed points ==
A ''fixed point'' of a function <code>f</code> is a value <code>a</code> such that <code>f a == a</code>. For example, <code>0</code> is a fixed point of the function <code>(* 3)</code> since <code>0 * 3 == 0</code>. This is where the name of <code>fix</code> comes from: it finds the ''least-defined fixed point'' of a function. (We'll come to what "least defined" means in a minute.) Notice that for both of our examples above that converge, this is readily seen:

 const "hello" "hello" -> "hello"
 (1:) [1,1,..]         -> [1,1,...]

And since there's no number <code>x</code> such that <code>2+x == x</code>, it also makes sense that <code>fix (2+)</code> diverges.

{{Exercises|
For each of the functions <code>f</code> in the above exercises for which you decided that <code>fix f</code> converges, verify that <code>fix f</code> finds a fixed point.}}

In fact, it's obvious from the definition of <code>fix</code> that it finds a fixed point. All we need to do is write the equation for <code>fix</code> the other way around:

 f (fix f) = fix f

Which is precisely the definition of a fixed point! So it seems that <code>fix</code> should always find a fixed point. But sometimes <code>fix</code> seems to fail at this, as sometimes it diverges. We can repair this property, however, if we bring in some [[Haskell/Denotational semantics|denotational semantics]]. Every Haskell type actually include a special value called bottom, written <code>⊥</code>. So the values with type, for example, <code>Int</code> include, in fact, <code>⊥</code> as well as <code>1, 2, 3</code> etc.. Divergent computations are denoted by a value of <code>⊥</code>, i.e., we have that <code>fix (2+) = ⊥</code>.

The special value <code>undefined</code> is also denoted by this <code>⊥</code>. Now we can understand how <code>fix</code> finds fixed points of functions like <code>(2+)</code>:

{{HaskellExample|Fixed points of <code>(2+)</code>|2=
<pre>
Prelude> (2+) undefined
*** Exception: Prelude.undefined
</pre>
}}

So feeding <code>undefined</code> (i.e., <code>⊥</code>) to <code>(2+)</code> gives us <code>undefined</code> back. So <code>⊥</code> is a fixed point of <code>(2+)</code>!

In the case of <code>(2+)</code>, it is the only fixed point. However, there are other functions <code>f</code> with several fixed points for which <code>fix f</code> still diverges: <code>fix (*3)</code> diverges, but we remarked above that <code>0</code> is a fixed point of that function. This is where the "least-defined" clause comes in. Types in Haskell have a [http://en.wikipedia.org/wiki/Partial_order partial order] on them called ''definedness''. In any type, <code>⊥</code> is the least-defined value (hence the name "bottom"). For simple types like <code>Int</code>, the only pairs in the partial order are <code>⊥ ≤ 1</code>, <code>⊥ ≤ 2</code> and so on. We do not have <code>m ≤ n</code> for any non-bottom <code>Int</code>s <code>m</code>, <code>n</code>. Similar comments apply to other simple types like <code>Bool</code> and <code>()</code>. For "layered" values such as lists or <code>Maybe</code>, the picture is more complicated, and we refer to the chapter on [[Haskell/Denotational semantics|denotational semantics]].

So since <code>⊥</code> is the least-defined value for all types and <code>fix</code> finds the least-defined fixed point, if <code>f ⊥ = ⊥</code>, we will have <code>fix f = ⊥</code> (and the converse is also true). If you've read the denotational semantics article, you will recognise this as the criterion for a ''strict function'': <code>fix f</code> diverges if and only if <code>f</code> is strict.

== Recursion ==
If you've come across examples of <code>fix</code> on the internet, or on the [http://www.haskell.org/haskellwiki/IRC_channel #haskell IRC channel], the chances are that you've seen examples involving <code>fix</code> and recursion. Here's a classic example:

{{HaskellExample|Encoding recursion with <code>fix</code>|2=
<pre>
Prelude> let fact n = if n == 0 then 1 else n * fact (n-1) in fact 5
120
Prelude> fix (\rec n -> if n == 0 then 1 else n * rec (n-1)) 5
120
</pre>
}}

Here we have used <code>fix</code> to "encode" the factorial function: note that (if we regard <code>fix</code> as a language primitive) our second definition of <code>fact</code> doesn't involve recursion at all. In a language like the typed lambda calculus that doesn't feature recursion, we can introduce <code>fix</code> in to write recursive functions in this way. Here are some more examples:

{{HaskellExample|More <code>fix</code> examples|2=
<pre>
Prelude> fix (\rec f l -> if null l then [] else f (head l) : rec f (tail l)) (+1) [1..3]
[2,3,4]
Prelude> map (fix (\rec n -> if n == 1 || n == 2 then 1 else rec (n-1) + rec (n-2))) [1..10]
[1,1,2,3,5,8,13,21,34,55]
</pre>
}}

So how does this work? Let's first approach it from a denotational point of view with our <code>fact</code> function. For brevity let's define:

 fact' rec n = if n == 0 then 1 else n * rec (n-1)

So that we're computing <code>fix fact' 5</code>. <code>fix</code> will find a fixed point of <code>fact'</code>, i.e. the ''function'' <code>f</code> such that <code>f == fact' f</code>. But let's expand what this means:

 f = fact' f
   = \n -> if n == 0 then 1 else n * f (n-1)

All we did was substitute <code>rec</code> for <code>f</code> in the definition of <code>fact'</code>. But this looks exactly like a ''recursive'' definition of a factorial function! <code>fix</code> feeds <code>fact'</code> ''itself'' as its first parameter in order to create a recursive function out of a higher-order function.

We can also consider things from a more operational point of view. Let's actually expand the definition of <code>fix fact'</code>:

   fix fact'
 = fact' (fix fact')
 = (\rec n -> if n == 0 then 1 else n * rec (n-1)) (fix fact')
 = \n -> if n == 0 then 1 else n * fix fact' (n-1)
 = \n -> if n == 0 then 1 else n * fact' (fix fact') (n-1)
 = \n -> if n == 0 then 1
         else n * (\rec n' -> if n' == 0 then 1 else n' * rec (n'-1)) (fix fact') (n-1)
 = \n -> if n == 0 then 1
         else n * (if n-1 == 0 then 1 else (n-1) * fix fact' (n-2))
 = \n -> if n == 0 then 1
         else n * (if n-1 == 0 then 1
                   else (n-1) * (if n-2 == 0 then 1
                                 else (n-2) * fix fact' (n-3)))
 = ...

Notice that the use of <code>fix</code> allows us to keep "unravelling" the definition of <code>fact'</code>: every time we hit the <code>else</code> clause, we product another copy of <code>fact'</code> via the evaluation rule <code>fix fact' = fact' (fix fact')</code>, which functions as the next call in the recursion chain. Eventually we hit the <code>then</code> clause and bottom out of this chain.

{{Exercises|
# Expand the other two examples we gave above in this sense. You may need a lot of paper for the Fibonacci example!
# Write non-recursive versions of <code>filter</code> and <code>foldr</code>.
}}

== The typed lambda calculus ==
In this section we'll expand upon a point mentioned a few times in the previous section: how <code>fix</code> allows us to encode recursion in the typed lambda calculus. It presumes you've already met the typed lambda calculus. Recall that in the lambda calculus, there is no <code>let</code> clause or top-level bindings. Every program is a simple tree of lambda abstractions, applications and literals. Let's say we want to write a <code>fact</code> function. Assuming we have a type called <code>Nat</code> for the natural numbers, we'd start out something like the following:

 λn:Nat. if iszero n then 1 else n * <blank> (n-1)

The problem is, how do we fill in the <code><blank></code>? We don't have a name for our function, so we can't call it recursively. The only way to bind names to terms is to use a lambda abstraction, so let's give that a go:

 (λf:Nat→Nat. λn:Nat. if iszero n then 1 else n * f (n-1))
   (λm:Nat. if iszero m then 1 else m * <blank> (m-1))

This expands to:

 λn:Nat. if iszero n then 1
         else n * (if iszero n-1 then 1 else (n-1) * <blank> (n-2))

We still have a <code><blank></code>. We could try to add one more layer in:
   
 (λf:Nat→Nat. λn:Nat. if iszero n then 1 else n * f (n-1)
   ((λg:Nat→Nat. λm:Nat. if iszero n' then 1 else n' * g (m-1))
     (λp:Nat. if iszero p then 1 else p * <blank> (p-1))))
 
 ->
 
 λn:Nat. if iszero n then 1
         else n * (if iszero n-1 then 1
                   else (n-1) * (if iszero n-2 then 1 else (n-2) * <blank> (n-3)))

It's pretty clear we're never going to be able to get rid of this <code><blank></code>, no matter how many levels of naming we add in. Never, that is, unless we use <code>fix</code>, which, in essence, provides an object from which we can always unravel one more layer of recursion and still have what we started off:

 fix (λf:Nat→Nat. λn:Nat. if iszero n then 1 else n * f (n-1))

This is a perfect factorial function in the typed lambda calculus plus <code>fix</code>.

<code>fix</code> is actually slightly more interesting than that in the context of the typed lambda calculus: if we introduce it into the language, then every type becomes inhabited, because given some concrete type <code>T</code>, the following expression has type <code>T</code>:

 fix (λx:T. x)

This, in Haskell-speak, is <code>fix id</code>, which is denotationally <code>⊥</code>. So we see that as soon as we introduce <code>fix</code> to the typed lambda calculus, the property that every well-typed term reduces to a value is lost.

== Fix as a data type ==
It is also possible to make a fix data type.

There are three ways of defining it.
 newtype Fix f=Fix (f (Fix f))

and
 newtype Mu f=Mu (forall a.(f a->a)->a)
 data Nu f=forall a.Nu a (a->f a)

Mu and Nu help generalize folds, unfolds and refolds.
 fold :: (f a -> a) -> Mu f -> a
 fold g (Mu f)=f g
 unfold :: (a -> f a) -> a -> Nu f
 unfold f x=Nu x f
 refold :: (a -> f a) -> (g a-> a) -> Mu f -> Nu g
 refold f g=unfold g . fold f

Mu and Nu are restricted versions of Fix.
Mu is used for making inductive noninfinite data and Nu is used for making coinductive infinite data.
Eg)
 newpoint Stream a=Stream (Nu ((,) a)) -- forsome b. (b,b->(a,b))
 newpoint Void a=Void (Mu ((,) a)) -- forall b.((a,b)->b)->b

Unlike the fix point function the fix point types do not lead to bottom.
In the following code Bot is perfectly defined. It is equivalent to the unit type ().
 newtype Id a=Id a
 newtype Bot=Bot (Fix Id) -- equals          newtype Bot=Bot Bot
 -- There is only one allowable term. Bot $ Bot $ Bot $ Bot ..,

The Fix data type cannot model all forms of recursion.
Take for instance this nonregular data type.
 data Node a=Two a a|Three a a a
 data FingerTree a=U a|Up (FingerTree (Node a))
It is not easy to implement this using Fix.

{{Haskell navigation|chapter=Wider Theory}}
[[Category:Haskell|Fix and recursion]]
