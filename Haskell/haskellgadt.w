>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{wikipedia}}
{{Haskell minitoc|chapter=Fun with Types}}

== Introduction ==
Generalized Algebraic Datatypes are, as the name suggests, a generalization of Algebraic Data Types that you are already familiar with. Basically, they allow you to explicitly write down the types of the constructors. In this chapter, you'll learn why this is useful and how to declare your own.  

We begin with an example of building a simple embedded domain specific language (EDSL) for simple arithmetical expressions, which is put on a sounder footing with GADTs.  This is followed by a review of the syntax for GADTs, with simpler illustrations, and a different application to construct a safe list type for which the equivalent of <code>head []</code> fails to typecheck and thus does not give the usual runtime error:   <code>*** Exception: Prelude.head: empty list</code>.

== Understanding GADTs ==
So, what are GADTs and what are they useful for? GADTs are mainly used to implement domain specific languages and this section will introduce them with a corresponding example.

=== Arithmetic expressions ===
Let's consider a small language for arithmetic expressions, given by the data type

    data Expr = I Int            -- integer constants
              | Add Expr Expr    -- add two expressions
              | Mul Expr Expr    -- multiply two expressions

In other words, this data type corresponds to the abstract syntax tree, an arithmetic term like <code>(5+1)*7</code> would be represented as <code>(I 5 `Add` I 1) `Mul` I 7 :: Expr</code>.

Given the abstract syntax tree, we would like to do something with it; we want to compile it, optimize it and so on. For starters, let's write an evaluation function that takes an expressions and calculates the integer value it represents. The definition is straightforward:
    
    eval :: Expr -> Int
    eval (I n)       = n
    eval (Add e1 e2) = eval e1 + eval e2
    eval (Mul e1 e2) = eval e1 * eval e2

=== Extending the language ===
Now, imagine that we would like to extend our language with other types than just integers. For instance, let's say we want to represent equality tests, so we need booleans as well. We augment the `Expr` type to become

    data Expr = I Int
              | B Bool           -- boolean constants
              | Add Expr Expr
              | Mul Expr Expr
              | Eq  Expr Expr    -- equality test

The term <code>5+1 == 7<code> would be represented as <code>(I 5 `Add` I 1) `Eq` I 7</code>.

As before, we want to write a function <code>eval</code> to evaluate expressions. But this time, expressions can now represent either integers or booleans and we have to capture that in the return type

    eval :: Expr -> Either Int Bool

The first two cases are straightforward

    eval (I n) = Left n
    eval (B b) = Right b

but now we get in trouble. We would like to write

    eval (Add e1 e2) = eval e1 + eval e2  -- ???

but this doesn't type check: the addition function <code>+</code> expects two integer arguments, but <code>eval e1</code> is of type <code>Either Int Bool</code> and we'd have extract the <code>Int</code> from that.

Even worse, what happens if <code>e1</code> actually represents a ''boolean''? The following is a valid expression

    B True `Add` I 5 :: Expr

but clearly, it doesn't make any sense; we can't add booleans to integers! In other words, evaluation may return integers or booleans, but it may also ''fail'' because the expression makes no sense. We have to incorporate that in the return type:

    eval :: Expr -> Maybe (Either Int Bool)

Now, we could write this function just fine, but that would still be unsatisfactory, because what we ''really'' want to do is to have Haskell's type system rule out any invalid expressions; we don't want to check types ourselves while deconstructing the abstract syntax tree.

Exercise: Despite our goal, it may still be instructional to implement the <code>eval</code> function; do this.

=== Phantom types ===
The so-called ''phantom types'' are the first step towards our goal. The technique is to augment the <code>Expr</code> with a type variable, so that it becomes

    data Expr a = I Int
                | B Bool
                | Add (Expr a) (Expr a)
                | Mul (Expr a) (Expr a)
                | Eq  (Expr a) (Expr a)

Note that an expression <code>Expr a</code> does not contain a value <code>a</code> at all; that's why <code>a</code> is called a ''phantom type'', it's just a dummy variable. Compare that with, say, a list <code>[a]</code> which does contain a bunch of <code>a</code>'s.

The key idea is that we're going to use <code>a</code> to track the type of the expression for us. Instead of making the constructor

    Add :: Expr a -> Expr a -> Expr a

available to users of our small language, we are only going to provide a ''smart constructor'' with a more restricted type

    add :: Expr Int -> Expr Int -> Expr Int
    add = Add

The implementation is the same, but the types are different. Doing this with the other constructors as well, 

    i :: Int  -> Expr Int
    i = I
    b :: Bool -> Expr Bool
    b = B

the previously problematic expression

    b True `add` i 5

no longer type checks! After all, the first arguments has the type <code>Expr Bool</code> while <code>add</code> expects an <code>Expr Int</code>. In other words, the phantom type <code>a</code> marks the intended type of the expression. By only exporting the smart constructors, the user cannot create expressions with incorrect types.

As before, we want to implement an evaluation function. With our new marker <code>a</code>, we might hope to give it the type

    eval :: Expr a -> a

and implement the first case like this

    eval (I n) = n

But alas, this does not work: how would the compiler know that encountering the constructor <code>I</code> means that <code>a = Int</code>? Granted, this will be case for all the expression that were created by users of our language because they are only allowed to use the smart constructors. But internally, an expression like

    I 5 :: Expr String

is still valid. In fact, as you can see, <code>a</code> doesn't even have to be <code>Int</code> or <code>Bool</code>, it could be anything.

What we need is a way to restrict the return types of the ''constructors proper'', and that's exactly what generalized data types do.

=== GADTs ===
The obvious notation for restricting the type of a constructor is to write down its type, and that's exactly how GADTs are defined:

    data Expr a where
        I   :: Int  -> Expr Int
        B   :: Bool -> Expr Bool
        Add :: Expr Int -> Expr Int -> Expr Int
        Mul :: Expr Int -> Expr Int -> Expr Int
        Eq  :: Expr Int -> Expr Int -> Expr Bool

In other words, we simply list the type signatures of all the constructors. In particular, the marker type <code>a</code> is specialised to <code>Int</code> or <code>Bool</code> according to our needs, just like we would have done with smart constructors.

And the great thing about GADTs is that we now ''can'' implement an evaluation function that takes advantage of the type marker:

    eval :: Expr a -> a
    eval (I n) = n
    eval (B b) = b
    eval (Add e1 e2) = eval e1 + eval e2
    eval (Mul e1 e2) = eval e1 * eval e2
    eval (Eq  e1 e2) = eval e1 == eval e2

In particular, in the first case

    eval (I n) = n

the compiler is now able infer that <code>a=Int</code> when we encounter a constructor <code>I</code> and that it is legal to return the <code>n :: Int<code>; similarly for the other cases.

To summarise, GADTs allows us to restrict the return types of constructors and thus enable us to take advantage of Haskell's type system for our domain specific languages. Thus, we can implement more languages and their implementation becomes simpler.

== Summary ==
=== Syntax ===
Here a quick summary of how the syntax for declaring GADTs works.

First, consider the following ordinary algebraic datatypes: the familiar <code>List</code> and <code>Maybe</code> types, and a simple tree type, <code>RoseTree</code>:

<div align="left">
{|border="1"
!|Maybe
!|List
!|Rose Tree
|-
||
<pre>
data Maybe a =  
    Nothing |   
    Just a            
</pre>
||
<pre>
data List a = 
    Nil |  
    Cons a (List a)              
</pre>
||
<pre>
data RoseTree a = 
     RoseTree a [RoseTree a]                   
     
</pre>
|}
</div>

Remember that the constructors introduced by these declarations can be used both for pattern matches to deconstruct values and as functions to construct values. (<code>Nothing</code> and <code>Nil</code> are functions with "zero arguments".) We can ask what the types of the latter are:

<div align="left">
{|border="1"
!|Maybe
!|List
!|Rose Tree
|-
||
<pre>
> :t Nothing
Nothing :: Maybe a
> :t Just
Just :: a -> Maybe a  
</pre>
||
<pre>
> :t Nil
Nil :: List a
> :t Cons
Cons :: a -> List a -> List a    
</pre>
||
<pre>

> :t RoseTree
RoseTree :: a -> [RoseTree a] -> RoseTree a    

</pre>
|}
</div>

It is clear that this type information about the constructors for <code>Maybe</code>, <code>List</code> and <code>RoseTree</code> respectively is equivalent to the information we gave to the compiler when declaring the datatype in the first place. In other words, it's also conceivable to declare a datatype by simply listing the types of all of its constructors, and that's exactly what the GADT syntax does:

<div align="left">
{|border="1"
!|Maybe
!|List
!|Rose Tree
|-
||
<pre>
data Maybe a where
   Nothing  :: Maybe a
   Just :: a -> Maybe a
</pre>
||
<pre>
data List a where
   Nil  :: List a
   Cons :: a -> List a -> List a
</pre>
||
<pre>
data RoseTree a where 
   RoseTree :: a ->  [RoseTree a] -> RoseTree a

</pre>
|}
</div>

This syntax is made available by the language option <code>{-#LANGUAGE GADTs #-}</code>. It should be familiar to you in that it closely resembles the syntax type class declarations. It's also easy to remember if you already like to think of constructors as just being functions. Each constructor is just defined by a type signature.

=== New possibilities ===

Note that when we asked the <code>GHCi</code> for the types of <code>Nothing</code> and <code>Just</code> it returned <code>Maybe a</code> and <code>a -> Maybe a</code> as the types.  In these and the other cases, the type of the final output of the function associated with a constructor is the type we were initially defining - <code>Maybe a</code>, <code>List a</code> or <code>RoseTree a</code>.  In general, in standard Haskell, the constructor functions for <code>Foo a</code> have <code>Foo a</code> as their final return type.   If the new syntax were to be strictly equivalent to the old, we would have to place this restriction on its use for valid type declarations.

So what do GADTs add for us?  The ability to control exactly what kind of <code>Foo</code> you return.  With GADTs, a constructor for <code>Foo a</code> is not obliged to return <code>Foo a</code>; it can return any <code>Foo blah</code> that you can think of.  In the code sample below, for instance, the <code>GadtedFoo</code> constructor returns a <code>GadtedFoo Int</code> even though it is for the type <code>GadtedFoo x</code>.

{{HaskellExample|GADT gives you more control|
<pre>
data FooInGadtClothing a where
 MkFooInGadtClothing :: a -> FooInGadtClothing a

--which is no different from:  data Haskell98Foo a = MkHaskell98Foo a ,

--by contrast, consider:
 
data TrueGadtFoo a where
  MkTrueGadtFoo :: a -> TrueGadtFoo Int

--This has no Haskell 98 equivalent.
</pre>
}}

But note that you can only push the generalization so far... if the datatype you are declaring is a <code>Foo</code>, the constructor functions ''must'' return some kind of <code>Foo</code> or another.  Returning anything else simply wouldn't work

{{HaskellExample|Try this out.  It doesn't work|
<pre>
data Bar where
  BarNone :: Bar -- This is ok

data Foo where
  MkFoo :: Bar Int-- This will not typecheck
</pre>
}}

== Examples ==
=== Safe Lists ===

:'''Prerequisite:''' ''We assume in this section that you know how a List tends to be represented in functional languages''
:'''Note:''' ''The examples in this article additionally require the extensions EmptyDataDecls and KindSignatures to be enabled''

We've now gotten a glimpse of the extra control given to us by the GADT syntax.  The only thing new is that you can control exactly what kind of data structure you return.  Now, what can we use it for?  Consider the humble Haskell list.  What happens when you invoke <code>head []</code>?  Haskell blows up. Have you ever wished you could have a magical version of <code>head</code> that only accepts lists with at least one element, lists on which it will never blow up?  

To begin with, let's define a new type, <code>SafeList x y</code>.  The idea is to have something similar to normal Haskell lists <code>[x]</code>, but with a little extra information in the type.  This extra information (the type variable <code>y</code>) tells us whether or not the list is empty.  Empty lists are represented as <code>SafeList x Empty</code>, whereas non-empty lists are represented as <code>SafeList x NonEmpty</code>. 

<pre>
-- we have to define these types
data Empty
data NonEmpty

-- the idea is that you can have either 
--    SafeList a Empty
-- or SafeList a NonEmpty
data SafeList a b where
-- to be implemented
</pre>

Since we have this extra information, we can now define a function <code>safeHead</code> on only the non-empty lists!  Calling <code>safeHead</code> on an empty list would simply refuse to type-check.  

<pre>
safeHead :: SafeList a NonEmpty -> a
</pre>

So now that we know what we want, <code>safeHead</code>, how do we actually go about getting it?  The answer is GADT.  The key is that we take advantage of the GADT feature to return two ''different'' list-of-a types, <code>SafeList a Empty</code> for the <code>Nil</code> constructor, and <code>SafeList a NonEmpty</code> for the <code>Cons</code> constructor:

<pre>
data SafeList a b where
  Nil  :: SafeList a Empty
  Cons :: a -> SafeList a b -> SafeList a NonEmpty
</pre>

This wouldn't have been possible without GADT, because all of our constructors would have been required to return the same type of list; whereas with GADT we can now return different types of lists with different constructors.  Anyway, let's put this all together, along with the actual definition of <code>SafeHead</code>:

{{HaskellExample|safe lists via GADT|
<pre>
{-#LANGUAGE GADTs, EmptyDataDecls #-}
-- (the EmptyDataDecls pragma must also appear at the very top of the module,
-- in order to allow the Empty and NonEmpty datatype declarations.)

data Empty
data NonEmpty

data SafeList a b where
     Nil :: SafeList a Empty
     Cons:: a -> SafeList a b -> SafeList a NonEmpty

safeHead :: SafeList a NonEmpty -> a
safeHead (Cons x _) = x
</pre>
}}

Copy this listing into a file and load in <code>ghci -fglasgow-exts</code>.  You should notice the following difference, calling <code>safeHead</code> on a non-empty and an empty-list respectively:
{{HaskellExample|<code>safeHead</code> is... safe|
<pre>
Prelude Main> safeHead (Cons "hi" Nil)
"hi"
Prelude Main> safeHead Nil

<interactive>:1:9:
    Couldn't match `NonEmpty' against `Empty'
      Expected type: SafeList a NonEmpty
      Inferred type: SafeList a Empty
    In the first argument of `safeHead', namely `Nil'
    In the definition of `it': it = safeHead Nil
</pre>
}}

This complaint is a good thing: it means that we can now ensure during compile-time if we're calling <code>safeHead</code> on an appropriate list.  However, this is a potential pitfall that you'll want to look out for.

Consider the following function.  What do you think its type is?

{{HaskellExample|Trouble with GADTs|
<pre>
silly 0 = Nil
silly 1 = Cons 1 Nil
</pre>
}}

Now try loading the example up in GHCi.  You'll notice the following complaint:
{{HaskellExample|Trouble with GADTs - the complaint|
<pre>
Couldn't match `Empty' against `NonEmpty'
     Expected type: SafeList a Empty
     Inferred type: SafeList a NonEmpty
   In the application `Cons 1 Nil'
   In the definition of `silly': silly 1 = Cons 1 Nil
</pre>
}}

By liberalizing the type of a <code>Cons</code>, we are able to use it to
construct both safe and unsafe lists. The functions that operate on the lists
must carry the safety constraint (we can create <code>type</code> declarations to make this easy for clients to our module).


{{HaskellExample|A different approach|
<pre>
{-#LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}
-- here we add the KindSignatures pragma,
-- which makes the GADT declaration a bit more elegant.

data NotSafe
data Safe


data MarkedList             ::  * -> * -> * where
  Nil                       ::  MarkedList t NotSafe
  Cons                      ::  a -> MarkedList a b -> MarkedList a c


safeHead                    ::  MarkedList a Safe -> a
safeHead (Cons x _)          =  x


silly 0                      =  Nil
silly 1                      =  Cons () Nil
silly n                      =  Cons () $ silly (n-1)
</pre>
}}


{{Exercises|1=
# Could you implement a <code>safeTail</code> function?
}}

=== A simple expression evaluator ===

:''Insert the example used in Wobbly Types paper... I thought that was quite pedagogical''
:''This is already covered in the first part of the tutorial.''

== Discussion ==

:''More examples, thoughts''

:''From FOSDEM 2006, I vaguely recall that there is some relationship between GADT and the below... what?''

=== Phantom types ===
See [[../Phantom types/]].

=== Existential types ===

If you like [[../Existentially quantified types/]], you'd probably want to notice that they are now subsumbed by GADTs.  As the GHC manual says, the following two type declarations give you the same thing.

  data TE a = forall b. MkTE b (b->a)
  data TG a where { MkTG :: b -> (b->a) -> TG a }

Heterogeneous lists are accomplished with GADTs like this:

  data TE2 = forall b. Show b => MkTE2 [b]
  data TG2 where
    MkTG2 :: Show b => [b] -> TG2

=== Witness types ===

== References ==

{{Haskell stub}}

{{Haskell import Haskell wiki|Generalised_algebraic_datatype|Generalised algebraic datatype}}

<div style="clear:both">
{{Haskell navigation|chapter=Fun with Types}}
</div>

{{Auto category}}
