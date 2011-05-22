>{{Haskell minitoc|chapter=Intermediate Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Intermediate Haskell}}

==Trees==

Now let's look at one of the most important datastructures: Trees. A tree is an example of a recursive datatype. Typically, its definition will look like this:

 data Tree a = Leaf a | Branch (Tree a) (Tree a)

As you can see, it's parameterised, so we can have trees of <tt>Int</tt>s, trees of <tt>String</tt>s, trees of <tt>Maybe Int</tt>s, even trees of <tt>(Int, String)</tt> pairs, if you really want. What makes it special is that <tt>Tree</tt> appears in the definition of itself. We will see how this works by using an already known example: the list.

===Lists as Trees===

As we have seen in [[Haskell/More about lists|More about lists]] and [[Haskell/List_Processing|List Processing]], we break lists down into two cases: An empty list (denoted by <tt>[]</tt>), and an element of the specified type, with another list (denoted by <tt>(x:xs)</tt>). This gives us valuable insight about the definition of lists:

 data [a] = [] | (a:[a]) -- Pseudo-Haskell, will not work properly.

Which is sometimes written as (for Lisp-inclined people):

 data List a = Nil | Cons a (List a)

As you can see this is also recursive, like the tree we had. Here, the constructor functions are <tt>[]</tt> and <tt>(:)</tt>. They represent what we have called <tt>Leaf</tt> and <tt>Branch</tt>. We can use these in pattern matching, just as we did with the empty list and the <tt>(x:xs)</tt>:

===Maps and Folds===

We already know about maps and folds for lists. With our realisation that a list is some sort of tree, we can try to write map and fold functions for our own type <tt>Tree</tt>. To recap:

 data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)
 data [a]    = []     | (:)    a [a]              
   -- (:) a [a] would be the same as (a:[a]) with prefix instead of infix notation.

We consider map first, then folds.

{{body note|Deriving is explained later on in the section [[Haskell/Class_declarations|Class Declarations]]. For now, understand it as telling Haskell (and by extension your interpreter) how to display a Tree instance.}}

====Map====

Let's take a look at the definition of <tt>map</tt> for lists:

 map :: (a -> b) -> [a] -> [b]
 map _ [] = []
 map f (x:xs) = f x : map f xs

First, if we were to write <tt>treeMap</tt>, what would its type be? Defining the function is easier if you have an idea of what its type should be.

We want it to work on a <tt>Tree</tt> of some type, and it should return another <tt>Tree</tt> of some type. What <tt>treeMap</tt> does is applying a function on each element of the tree, so we also need a function. In short:

 treeMap :: (a -> b) -> Tree a -> Tree b

See how this is similar to the list example?

Next, we should start with the easiest case. When talking about a <tt>Tree</tt>, this is obviously the case of a <tt>Leaf</tt>. A <tt>Leaf</tt> only contains a single value, so all we have to do is apply the function to that value and then return a <tt>Leaf</tt> with the altered value:

 treeMap :: (a -> b) -> Tree a -> Tree b
 treeMap f (Leaf x) = Leaf (f x)

Also, this looks a lot like the empty list case with <tt>map</tt>. Now if we have a <tt>Branch</tt>, it will include two subtrees; what do we do with them? When looking at the list-<tt>map</tt>, you can see it uses a call to itself on the tail of the list. We also shall do that with the two subtrees. The complete definition of treeMap is as follows:

 treeMap :: (a -> b) -> Tree a -> Tree b
 treeMap f (Leaf x) = Leaf (f x)
 treeMap f (Branch left right) = Branch (treeMap f left) (treeMap f right)

We can make this a bit more readable by noting that <tt>treeMap f</tt> is itself a function with type <tt>Tree a -> Tree b</tt>, and what we really need is a recursive definition of <tt>treeMap f</tt>.  This gives us the following revised definition:

 treeMap :: (a -> b) -> Tree a -> Tree b
 treeMap f = g where
   g (Leaf x) = Leaf (f x)
   g (Branch left right) = Branch (g left) (g right)

If you don't understand it just now, re-read it. Especially the use of pattern matching may seem weird at first, but it is essential to the use of datatypes. The most important thing to remember is that pattern matching happens on constructor functions.

If you understand it, read on for folds.

====Fold====

Now we've had the <tt>treeMap</tt>, let's try to write a <tt>treeFold</tt>. Again, let's take a look at the definition of <tt>foldr</tt> for lists, as it is easier to understand.

 foldr :: (a -> b -> b) -> b -> [a] -> b
 foldr f z [] = z
 foldr f z (x:xs) = f x (foldr f z xs)

Recall that lists have two constructors:

 (:) :: a -> [a] -> [a]  -- two arguments
 [] :: [a]  -- zero arguments

Thus <tt>foldr</tt> takes two arguments corresponding to the two constructors:

 f :: a -> b -> b  -- a two-argument function
 z :: b  -- like a zero-argument function

We'll use the same strategy to find a definition for <tt>treeFold</tt> as we did for <tt>treeMap</tt>. First, the type.  We want <tt>treeFold</tt> to transform a tree of some type into a value of some other type; so in place of <tt>[a] -> b</tt> we will have <tt>Tree a -> b</tt>.  How do we specify the transformation?  First note that <tt>Tree a</tt> has two constructors:

 Branch :: Tree a -> Tree a -> Tree a
 Leaf :: a -> Tree a

So <tt>treeFold</tt> will have two arguments corresponding to the two constructors:

  fbranch :: b -> b -> b
  fleaf :: a -> b

Putting it all together we get the following type definition:

 treeFold :: (b -> b -> b) -> (a -> b) -> Tree a -> b

That is, the first argument, of type <tt>(b -> b -> b)</tt>, is a function specifying how to combine subtrees; the second argument, of type <tt>a -> b</tt>, is a function specifying what to do with leaves; and the third argument, of type <tt>Tree a</tt>, is the tree we want to "fold".

As with <tt>treeMap</tt>, we'll avoid repeating the arguments <tt>fbranch</tt> and <tt>fleaf</tt> by introducing a local function <tt>g</tt>:

 treeFold :: (b -> b -> b) -> (a -> b)  -> Tree a -> b
 treeFold fbranch fleaf = g where
   -- definition of g goes here

The argument <tt>fleaf</tt> tells us what to do with <tt>Leaf</tt> subtrees:

 g (Leaf x) = fleaf x

The argument <tt>fbranch</tt> tells us how to combine the results of "folding" two subtrees:

 g (Branch left right) = fbranch (g left) (g right)

Our full definition becomes:

 treeFold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
 treeFold fbranch fleaf = g where
   g (Leaf x) = fleaf x
   g (Branch left right) = fbranch (g left) (g right)

For examples of how these work, copy the <tt>Tree</tt> data definition and the <tt>treeMap</tt> and <tt>treeFold</tt> functions to a Haskell file, along with the following:

 tree1 :: Tree Integer
 tree1 = 
     Branch
        (Branch 
            (Branch 
                (Leaf 1) 
                (Branch (Leaf 2) (Leaf 3))) 
            (Branch 
                (Leaf 4) 
                (Branch (Leaf 5) (Leaf 6)))) 
        (Branch
            (Branch (Leaf 7) (Leaf 8)) 
            (Leaf 9))
 
 doubleTree = treeMap (*2)  -- doubles each value in tree
 sumTree = treeFold (+) id -- sum of the leaf values in tree
 fringeTree = treeFold (++) (: [])  -- list of the leaves of tree

Then load it into your favourite Haskell interpreter, and evaluate:

 doubleTree tree1
 sumTree tree1
 fringeTree tree1

==== Unfolds ====
Dually it is also possible to write unfolds.
UnfoldTree is of type
 (b -> Either (b,b) a ) -> b -> Tree a
and can be implemented as
 unfoldTree f x = 
   case f x of
     Right a    -> Leaf a
     Left (l,r) -> Branch (unfoldTree f l, unfoldTree f r)

==Other datatypes==

map and fold functions can be defined for any kind of data type. In order to generalize the strategy applied for lists and trees, in this final section we will work out a map and a fold for a very strange, intentionally contrived, datatype:

 data Weird a b = First a
                | Second b
                | Third [(a,b)]
                | Fourth (Weird a b)

It can be a useful exercise to write the functions as you follow the examples, trying to keep the coding one step ahead of your reading. 

===General Map===

Again, we will begin with <tt>weirdMap</tt>. The first important difference in working with this <tt>Weird</tt> type is that it has ''two'' type parameters. For that reason, we will want the map function to take two functions as arguments, one to be applied on the elements of type <tt>a</tt> and another for the elements of type <tt>b</tt>. With that accounted for, we can write the type signature of <tt>weirdMap</tt>:

 weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d

Next step is writing the definitions for <tt>weirdMap</tt>. The key point is that maps preserve the ''structure'' of a datatype, so the function must evaluate to a <tt>Weird</tt> which uses the same constructor than the one used for the original <tt>Weird</tt>. For that reason, we need one definition to handle each constructor, and these constructors are used as patterns for writing them. As before, to avoid repeating the <tt>weirdMap</tt> argument list over and over again a '''where''' clause comes in handy. A sketch of the function is below (note we already prepared a template for the list of tuples in <tt>Third</tt>.

 weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
 weirdMap fa fb = g
   where
     g (First x)          = --More to follow
     g (Second y)         = --More to follow
     g (Third z)          = --More to follow
     g (Fourth w)         = --More to follow

The first two cases are fairly straightforward, as there is just a single element of <tt>a</tt> or <tt>b</tt> type inside the <tt>Weird</tt>.

 weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
 weirdMap fa fb = g
   where
     g (First x)          = First (fa x)
     g (Second y)         = Second (fb y)
     g (Third z)          = --More to follow
     g (Fourth w)         = --More to follow

<tt>Third</tt> is trickier because it contains another data structure (a list) whose elements are themselves data structures (the tuples). So we need to navigate the nested data structures, apply <tt>fa</tt> and <tt>fb</tt> on all elements of type <tt>a</tt> and <tt>b</tt> inside it and eventually (as a map must preserve structure) produce a list of tuples - <tt>[(c,d)]</tt> - to be used with the constructor. The simplest approach might seem to be just breaking down the list inside the <tt>Weird</tt> and playing with the patterns:

    g (Third []) = Third []
    g (Third ((x,y):zs)) = Third ( (fa x, fb y) : ( (\(Third z) -> z) (g (Third zs)) ) )

This appears to be written as a typical recursive function for lists. We start by applying the functions of interest to the first element in order to obtain the head of the new list, <tt>(fa x, fb y)</tt>. But to what we will cons it to? As <tt>g</tt> requires a <tt>Weird</tt> argument we need to make a <tt>Weird</tt> using the list tail in order to make the recursive call. But then <tt>g</tt> will give a <tt>Weird</tt> and not a list, so we have to retrieve the modified list from that - that's the role of the lambda function. And finally, there is also the empty list base case to be defined as well.

After all of that, we are left with a messy function. Every recursive call of <tt>g</tt> requires wrapping <tt>xs</tt> into a <tt>Weird</tt>, while what we really wanted to do was to build a list with <tt>(fa x, fb y)</tt> and the modified <tt>xs</tt>. The problem with this solution is that <tt>g</tt> can (thanks to pattern matching) act directly on the list head but (due to its type signature) can't be called directly on the list tail. For that reason, it would be better to apply <tt>fa</tt> and <tt>fb</tt> without breaking down the list with pattern matching (as far as <tt>g</tt> is directly concerned, at least). But there ''was'' a way to directly modify a list element-by-element...

     g (Third z) = Third ( map (\(x, y) -> (fa x, fb y) ) z)

...our good old <tt>map</tt> function, which modifies all tuples in the list <tt>z</tt> using a lambda function. In fact, the first attempt at writing the definition looked just like an application of the list map except for the spurious <tt>Weird</tt> packing and unpacking. We got rid of these by having the pattern splitting of <tt>z</tt> done by <tt>map</tt>, which works directly with regular lists. You could find it useful to expand the map definition inside <tt>g</tt> for seeing a clearer picture of that difference. Finally, you may prefer to write this new version in an alternative, very clean way using list comprehension syntax:

    g (Third z) = Third [ (fa x, fb y) | (x,y) <- z ] 

Adding the Third function, we only have the Fourth left to do:

 weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
 weirdMap fa fb = g
   where
     g (First x)          = First (fa x)
     g (Second y)         = Second (fb y)
     g (Third z)          = Third ( map (\(x, y) -> (fa x, fb y) ) z)
     g (Fourth w)         = --More to follow

Dealing with the recursive <tt>Fourth</tt> constructor is actually really easy. Just apply <tt>g</tt> recursively!

 weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
 weirdMap fa fb = g
   where
     g (First x)          = First (fa x)
     g (Second y)         = Second (fb y)
     g (Third z)          = Third ( map (\(x, y) -> (fa x, fb y) ) z)
     g (Fourth w)         = Fourth (g w)

===General Fold===

While we were able to define a map by specifying as arguments a function for every separate type, this isn't enough for a fold. For a fold, we'll need a function for every constructor function. This is also the case with lists! Remember the constructors of a list are <tt>[]</tt> and <tt>(:)</tt>. The <tt>z</tt> argument in the <tt>foldr</tt> function corresponds to the <tt>[]</tt> constructor. The <tt>f</tt> argument in the <tt>foldr</tt> function corresponds to the <tt>(:)</tt> constructor. The <tt>Weird</tt> datatype has four constructors, so we need four functions - one for handling the internal structure of the datatype specified by each constructor. Next, we have an argument of the <tt>Weird a b</tt> type, and finally we want the whole fold function to evaluate to a value of some other, arbitrary, type. Additionally, each individual function we pass to <tt>weirdFold</tt> must evaluate to the same type <tt>weirdFold</tt> does. That allows us to make a mock type signature and sketch the definition:

 weirdFold :: (something1 -> c) -> (something2 -> c) -> (something3 -> c) -> (something4 -> c) -> Weird a b -> c
 weirdFold f1 f2 f3 f4 = g
   where
     g (First x)          = --Something of type c here
     g (Second y)         = --Something of type c here
     g (Third z)          = --Something of type c here
     g (Fourth w)         = --Something of type c here

Now we need to figure out to which types <tt>something1</tt>, <tt>something2</tt>, <tt>something3</tt> and <tt>something4</tt> correspond to. That is done by analysing the constructors, since the functions must take as arguments the elements of the datatype (whose types are specified by the constructor type signature). Again, the types and definitions of the first two functions are easy to find. The third one isn't difficult either, as for the purposes of folding the list of <tt>(a,b)</tt> tuples is no different from a simple type - unlike in the map example, its ''internal'' structure does not concern us now. The fourth constructor, however, is recursive, and we have to watch out. As in the case of <tt>weirdMap</tt>, we also need to recursively call the <tt>g</tt> function. This brings us to the following, final, definition:

 weirdFold :: (a -> c) -> (b -> c) -> ([(a,b)] -> c) -> (c -> c) -> Weird a b -> c
 weirdFold f1 f2 f3 f4 = g
   where
     g (First x)          = f1 x
     g (Second y)         = f2 y
     g (Third z)          = f3 z
     g (Fourth w)         = f4 (g w)

{{body note|1=
If you were expecting very complex expressions in the weirdFold above and is surprised by the immediacy of the solution, it might be helpful to have a look on what the common foldr would look like if we wrote it in this style and didn't have the special square-bracket syntax of lists to distract us:
<pre>
 -- List a is [a], Cons is (:) and Nil is []
 data List a = Cons a (List a) | Nil 
 listFoldr :: (a -> b -> b) -> (b) -> List a -> b
 listFoldr fCons fNil = g
   where
     g (Cons x xs) = fCons x (g xs)
     g Nil         = fNil
</pre>
Now it is easier to see the parallels. The extra complications are that <tt>Cons</tt> (that is, <tt>(:)</tt>) takes two arguments (and, for that reason, so does <tt>fCons</tt>) and is recursive, requiring a call to <tt>g</tt>. Also, <tt>fNil</tt> is of course not really a function, as it takes no arguments. 
}}

====Folds on recursive datatypes====

As far as folds are concerned <tt>Weird</tt> was a fairly nice datatype to deal with. Just one recursive constructor, which isn't even nested inside other structures. What would happen if we added a truly complicated fifth constructor?

   Fifth [Weird a b] a (Weird a a, Maybe (Weird a b))

A valid, and tricky, question. In general, the following rules apply:

* A function to be supplied to a fold has the same number of arguments as the corresponding constructor.
* The type of the arguments of such a function match the types of the constructor arguments, ''except'' if the constructor is recursive (that is, takes an argument of its own type).
* If a constructor is recursive, any recursive argument of the constructor will correspond to an argument of the type the fold evaluates to. <ref>This sort of recursiveness, in which the function used for folding can take the result of another fold as an argument, is what confers the folds of data structures such as lists and trees their "accumulating" functionality.</ref>
* If a constructor is recursive, the complete fold function should be (recursively) applied to the recursive constructor arguments.
* If a recursive element appears inside another data structure, the appropriate map function for that data structure should be used to apply the fold function to it.

So <tt>f5</tt> would have the type:

 f5 :: [c] -> a -> (Weird a a, Maybe c) -> c

as the type of <tt>Fifth</tt> is:

 Fifth :: [Weird a b] -> a -> (Weird a a, Maybe (Weird a b)) -> Weird a b

The definition of <tt>g</tt> for the <tt>Fifth</tt> constructor will be:

     g (Fifth list x (waa, mc)) = f5 (map g list) x (waa, maybeMap g mc)
       where
         maybeMap f Nothing = Nothing
         maybeMap f (Just w) = Just (f w)

Now note that nothing strange happens with the <tt>Weird a a</tt> part. No <tt>g</tt> gets called. What's up? This is a recursion, right? Well... not really. <tt>Weird a a</tt> and <tt>Weird a b</tt> are different types, so it isn't a real recursion. It isn't guaranteed that, for example, <tt>f2</tt> will work with something of type 'a', where it expects a type 'b'. It can be true for some cases, but not for everything.

Also look at the definition of <tt>maybeMap</tt>. Verify that it is indeed a map function as:

* It preserves structure.
* Only types are changed.

== Notes ==

<references/>

{{Haskell navigation|chapter=Intermediate Haskell}}

[[Category:Haskell|Other data structures]]
