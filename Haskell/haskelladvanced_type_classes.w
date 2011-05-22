>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Fun with Types}}

Type classes may seem innocuous, but research on the subject has resulted in several advancements and generalisations which make them a very powerful tool.

== Multi-parameter type classes ==
Multi-parameter type classes are a generalisation of the single parameter [[Class_Declarations|type classes]], and are supported by some Haskell implementations.  

Suppose we wanted to create a 'Collection' type class that could be used with a variety of concrete data types, and supports two operations -- 'insert' for adding elements, and 'member' for testing membership.  A first attempt might look like this:

{{HaskellExample|The <code>Collection</code> type class (wrong)|<pre>
 class Collection c where
     insert :: c -> e -> c
     member :: c -> e -> Bool

 -- Make lists an instance of Collection:
 instance Collection [a] where
     insert xs x = x:xs
     member = flip elem
</pre>}}

This won't compile, however.  The problem is that the 'e' type variable in the Collection operations comes from nowhere -- there is nothing in the type of an instance of Collection that will tell us what the 'e' actually is, so we can never define implementations of these methods.  Multi-parameter type classes solve this by allowing us to put 'e' into the type of the class.  Here is an example that compiles and can be used:

{{HaskellExample|The <code>Collection</code> type class (right)|<pre>
 class Eq e => Collection c e where
     insert :: c -> e -> c
     member :: c -> e -> Bool

 instance Eq a => Collection [a] a where
     insert = flip (:)
     member = flip elem
</pre>}}

== Functional dependencies ==

A problem with the above example is that, in this case, we have extra information that the compiler doesn't know, which can lead to false ambiguities and over-generalised function signatures.  In this case, we can see intuitively that the type of the collection will always determine the type of the element it contains - so if <code>c</code> is <code>[a]</code>, then <code>e</code> will be <code>a</code>.  If <code>c</code> is <code>Hashmap a</code>, then <code>e</code> will be <code>a</code>.  (The reverse is not true:  many different collection types can hold the same element type, so knowing the element type was e.g. <code>Int</code>, would not tell you the collection type).

In order to tell the compiler this information, we add a '''functional dependency''', changing the class declaration to

{{HaskellExample|A functional dependency|<pre>
 class Eq e => Collection c e | c -> e where ...
</pre>}}

A functional dependency is a constraint that we can place on type class parameters.  Here, the extra <code>| c -> e</code> should be read '<code>c</code> uniquely identifies <code>e</code>', meaning for a given <code>c</code>, there will only be one <code>e</code>.   You can have more than one functional dependency in a class -- for example you could have <code>c -> e, e -> c</code> in the above case. And you can have more than two parameters in multi-parameter classes.

== Examples ==

=== Matrices and vectors ===
Suppose you want to implement some code to perform simple linear algebra:

{{HaskellExample|The <code>Vector</code> and <code>Matrix</code> datatypes|<pre>
 data Vector = Vector Int Int deriving (Eq, Show)
 data Matrix = Matrix Vector Vector deriving (Eq, Show)
</pre>}}

You want these to behave as much like numbers as possible.  So you might start by overloading Haskell's Num class:

{{HaskellExample|Instance declarations for <code>Vector</code> and <code>Matrix</code>|<pre>
instance Num Vector where
  Vector a1 b1 + Vector a2 b2 = Vector (a1+a2) (b1+b2)
  Vector a1 b1 - Vector a2 b2 = Vector (a1-a2) (b1-b2)
  {- ... and so on ... -}

instance Num Matrix where
  Matrix a1 b1 + Matrix a2 b2 = Matrix (a1+a2) (b1+b2)
  Matrix a1 b1 - Matrix a2 b2 = Matrix (a1-a2) (b1-b2)
  {- ... and so on ... -}
</pre>}}

The problem comes when you want to start multiplying quantities.  You really need a multiplication function which overloads to different types:

{{HaskellExample|What we need|<pre>
(*) :: Matrix -> Matrix -> Matrix
(*) :: Matrix -> Vector -> Vector
(*) :: Matrix -> Int -> Matrix
(*) :: Int -> Matrix -> Matrix
{- ... and so on ... -}
</pre>}}

How do we specify a type class which allows all these possibilities?

We could try this:

{{HaskellExample|An ineffective attempt (too general)|<pre>
class Mult a b c where
  (*) :: a -> b -> c

instance Mult Matrix Matrix Matrix where
  {- ... -}

instance Mult Matrix Vector Vector where
  {- ... -}
</pre>}}

That, however, isn't really what we want.  As it stands, even a simple expression like this has an ambiguous type unless you supply an additional type declaration on the intermediate expression:

{{HaskellExample|Ambiguities lead to more verbose code|<pre>
m1, m2, m3 :: Matrix
(m1 * m2) * m3              -- type error; type of (m1*m2) is ambiguous
(m1 * m2) :: Matrix * m3    -- this is ok
</pre>}}

After all, nothing is stopping someone from coming along later and adding another instance:

{{HaskellExample|A nonsensical instance of <code>Mult</code>|<pre>
instance Mult Matrix Matrix (Maybe Char) where
  {- whatever -}
</pre>}}

The problem is that <code>c</code> shouldn't really be a free type variable.  When you know the types of the things that you're multiplying, the result type should be determined from that information alone.

You can express this by specifying a functional dependency:

{{HaskellExample|The correct definition of <code>Mult</code>|<pre>
class Mult a b c | a b -> c where
  (*) :: a -> b -> c
</pre>}}

This tells Haskell that <code>c</code> is uniquely determined from <code>a</code> and <code>b</code>.

<div style="clear:both;">
{{Haskell import Haskell wiki|Functional_dependencies|Functional depedencies}}
</div>


<div style="clear:both;">
{{Haskell navigation|chapter=Fun with Types}}
</div>
{{Auto category}}
