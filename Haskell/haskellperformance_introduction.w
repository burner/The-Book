>{{Haskell minitoc|chapter=Haskell Performance}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Haskell Performance}}

== Execution Model ==
=== Introducing Lazy Evaluation ===
Programming is not only about writing programs that work but also about programs that require little memory and time to execute on a computer. While both time and memory use are relatively straightforward to predict in an imperative programming language or in ''strict'' functional languages like LISP or ML, things are different here. In Haskell, expressions are evaluated on demand. For instance, the expression

 head (map (2 *) [1 .. 10])

will be evaluated as follows

  &rArr; head (map (2 *) (1 : [2 .. 10]))      ([1 .. 10])
  &rArr; head (2 * 1 : map (2 *) [2 .. 10])    (map)
  &rArr; 2 * 1                                 (head)
  &rArr; 2                                     (*)

The function <code>head</code> demands only the first element of the list and consequently, the remaining part <code>map (2 *) [2 .. 10]</code> of the list is never evaluated. Since this strategy only performs as much evaluation as necessary, it's called '''lazy evaluation'''. The chapter [[../Graph reduction/]] will present it in detail.

While lazy evaluation is the commonly employed implementation technique for Haskell, the [http://www.haskell.org/onlinereport/ language standard] only specifies that Haskell has '''non-strict''' [[../Denotational semantics|denotational semantics]] without fixing a particular execution model. A function <code>f</code> with one argument is said to be ''strict'' if it doesn't terminate or yields an error whenever the evaluation of its argument will loop forever or is otherwise undefined. Writing &perp; for the "result" of an infinite loop, the definition for strictness is

<center>A function <code>f</code> is called '''strict''' if <code>f &perp; = &perp;</code> .</center>

For example, trying to add <code>1</code> to a number that loops forever will still loop forever, so <code>&perp;+1 = &perp;</code> and the addition function <code>(+1)</code> is strict. The function <code>head</code> is also strict since the first element of a list won't be available if the whole list is undefined. But it may well be that the first element is well-defined while the remaining list is not. In this case, we have

  head (x : &perp;) = x

This equation means that <code>head</code> does not evaluate the remaining list, otherwise it would loop as well. Thus, such purely algebraic strictness properties are a great help for reasoning about execution time and memory. In strict languages like LISP or ML, we would always have <code>head (x:&perp;) = &perp;</code>, whereas Haskell being "non-strict" means that we can write functions which are not strict, like the above property of <code>head</code> or the simplest example <code>(const 1) &perp; = 1</code>. With the help of the constant <code>undefined</code> from the {{Haskell lib|Prelude}}, we can even explore strictness interactively

 > head (1 : undefined)
 1
 > head undefined
 *** Exception: Prelude.undefined

Strictness and &perp; will be used thorough these chapters. [[../Graph reduction/]] presents further introductory examples and the denotational point of view is elaborated in [[../Denotational semantics/]].

=== Example: fold ===
The best way to get a first feeling for lazy evaluation is to study an example. Therefore, we will  consider some prototypical use case of <code>foldr<code> and its variants. The needed basics for studying the time and space complexity of programs are recapped in chapter [[../Algorithm complexity/]].

==== Time ====
Consider the following function <code>isPrime</code> that examines whether a number is a prime number or not

  isPrime n     = not $ any (`divides` n) [2..n-1]
  d `divides` n = n `mod` d == 0
  any p         = or . map p
  or            = foldr (||) False

The helper function <code>any</code> from the Haskell {{Haskell lib|Prelude}} checks whether there is at least one element in the list that satisfies some property <code>p</code>. For <code>isPrime</code> , the property is "being a divisor of <code>n</code>".

The amount of time it takes to evaluate an expression is of course measured by the number of reduction steps. If <code>n</code> is a prime number, the above algorithm will examine the full list of numbers from 2 to <code>n-1</code> and thus has a worst-case running time of <math>O(n)</math> reductions. However, if <code>n</code> is not a prime number, we do not need to loop through every one of these numbers, we can stop as soon as we found one divisor and report <code>n</code> as being composite. The joy of lazy evaluation is that this behavior is already ''built-in'' into the logical disjunction <code>||</code>!

 isPrime 42
  &rArr; not $ any (`divides` 42) [2..41]                               
  &rArr; not ( or (map (`divides` 42) [2..41]                         ) )
  &rArr; not ( or ((42 `mod` 2 == 0) :      map (`divides` 42) [3..41]) )
  &rArr; not (     (42 `mod` 2 == 0) || or (map (`divides` 42) [3..41]) )
  &rArr; not (                 True  || or (map (`divides` 42) [3..41]) )
  &rArr; not True
  &rArr; False

The function returns <code>False</code> after seeing that 42 is even, <code>||</code> does not look at its second argument when the first one determines the result to be <code>True</code>. In other words, we have the following strictness property

  True || &perp; = True

Of course, the above algorithm can be implemented with a custom loop. But the crux of lazy evaluation is that we could formulate the algorithm in a transparent way by reusing the standard <code>foldr</code> and still get early bail-out. Put differently, lazy evaluation is about formulating fast algorithms in a modular way. An extreme example is to use ''infinite'' data structures to efficiently modularize generate & prune - algorithms. This and many other neat techniques with lazy evaluation will be detailed in the chapter [[../Laziness/]].

It's neither feasible nor necessary to perform a detailed graph reduction to analyze execution time. Shortcuts are available, like non-strictness or the fact that lazy evaluation will always take fewer reduction steps than eager evaluation. They will be presented in [[../Graph reduction/]].

==== Space ====
While execution time is modeled by the number of reduction steps, memory usage is modeled by the size of an expression during evaluation. Unfortunately, it's harder to predict and deviating from the normal course lazy evaluation by more strictness can ameliorate it. More details in [[../Graph reduction/]]. Here, we will present the prototypical example for unexpected space behavior.

Naively summing a huge number of integers
 
 > foldr (+) 0 [1..1000000]
 *** Exception: stack overflow

produces a stack overflow. What happens? The evaluation proceeds as follows

 foldr (+) 0 [1..1000000]
  &rArr; 1+(foldr (+) 0 [2..1000000])
  &rArr; 1+(2+(foldr (+) 0 [3..1000000]))
  &rArr; 1+(2+(3+(foldr (+) 0 [4..1000000]))

and so on. We see that the expression grows larger and larger, needing more and more memory. In this case, the memory is allocated on the stack for performing the pending additions after the recursive calls to <code>foldr</code> return. At some point, the memory needed exceeds the maximum possible stack size raising the "stack overflow" error. Don't worry whether it's stack or heap, the thing to keep in mind is that the size of the expression corresponds to the memory used and we see that in general, evaluating <code>foldr (+) 0 [1..n]</code> needs <math>O(n)</math> memory.

But it should be possible to do it in <math>O(1)</math> space by keeping an accumulated sum of numbers seen so far, exploiting that <code>+</code> is associative. (Some readers may notice that this means to make the function tail recursive.) This is what foldl does:

 foldl (+) 0 [1..n]
  &rArr; foldl (+) (0+1) [2..n]
  &rArr; foldl (+) ((0+1)+2) [3..n]
  &rArr; foldl (+) (((0+1)+2)+3) [4..n]

But much to our horror, the accumulated sum will not be reduced any further! It will grow on the heap until the end of the list is reached

  &rArr; ... &rArr; foldl (+) ((((0+1)+2)+3)+...) []
  &rArr; ((((0+1)+2)+3)+...)

and subsequent reduction of this huge unevaluated sum will fail with a stack overflow, too. (So, just introducing an accumulating parameter doesn't make it tail recursive.)

The problem is that the unevaluated sum is an overly large representation for a single integer and it's cheaper to evaluate it eagerly. This is what <code>foldl'</code> does:

 foldl' f z []     = z
 foldl' f z (x:xs) = z `seq` foldl' f (f z x) xs

Here, evaluating <code>a `seq` b</code> will reduce <code>a</code> to [[../Graph reduction#Weak Head Normal Form|weak head normal form]] before proceeding with the reduction of <code>b</code>. With this, the sum proceeds as

 foldl' (+) 0 [1..n]
  &rArr; foldl' (+) (0+1) [2..n]
  &rArr; foldl' (+) (1+2) [3..n]
  &rArr; foldl' (+) (3+3) [4..n]
  &rArr; foldl' (+) (6+4) [5..n]
  &rArr; ...

in constant space.

For more details and examples, read the chapter [[../Graph reduction/]].

=== Low-level Overhead ===
Compared to eager evaluation, lazy evaluation adds a considerable overhead, even integers or characters have to be stored as pointers since they might be &perp;. An array of 1-byte characters is several times more compact than a <code>String = [Char]</code> of similar length. With strictness annotations, unboxed types and automatic strictness analysis, the overhead can be reduced. The Haskell wiki is a good resource [http://www.haskell.org/haskellwiki/Performance] concerning these low-level details, the wikibook currently doesn't cover them.

== Algorithms & Data Structures ==
=== Algorithms ===
While this wikibook is not a general book on algorithms, there are many techniques of writing efficient programs unique to functional programming. The chapter [[../Algorithm complexity/]] recaps the big-O notation and presents a few examples from practice.

In [[../Laziness/]], the focus is on exploiting lazy evaluation to write efficient algorithms in a modular way.

A common theme of [[../Program derivation/]] and [[../Equational reasoning/]] is to derive an efficient program from a specification by applying and proving equations like

 map f . reverse  = reverse . map f
 filter p . map f = map f . filter (p . f)

This quest has given rise to a gemstone, namely a purely algebraic approach to dynamic programming which will be introduced in ''some chapter with a good name''.

Another equational technique known as '''fusion''' or '''deforestation''' aims to remove intermediate data structures in function compositions. For instance, the composition on the left hand side of

  map f . map g = map (f . g)

constructs and deconstructs an intermediate list whereas the right hand side is a single pass over the list. The general theme here is to fuse constructor-deconstructor pairs like

  case (Just y) of { Just x -> f x; Nothing -> ...; } = f y

This will be detailed in ''some chapter with a good name''.

=== Data structures ===
Choosing the right data structure is key to success. While lists are common and can lead surprisingly far, they are more a kind of materialized loops than a classical data structure with fast access and updates. Many languages have special support for particular data structures of that kind like arrays in C, hash tables in Perl or lists in LISP. Natively, Haskell favors any kind of tree. But thanks to parametric polymorphism and type classes, any abstract type like balanced binary trees is easy to use and reuse! The chapter [[../Data structures/]] details the natural choices of data structures for common problems.

Because Haskell is purely functional, data structures share the common trait of being '''persistent'''. This means that older version of a data structure are still available like in

  foo set = (newset, set)
    where newset = insert "bar" set

Compared to that, the default in imperative languages is '''ephemeral''', i.e. data is updated in place and old versions are overridden. For example, arrays are ephemeral. That's why they are either immutable or require monads to use in Haskell. Fortunately, many ephemeral structures like queues or heaps have persistent counterparts and the chapter [[../Data structures/]] will also gently introduce some design principles in that direction, for instance concerning amortization.

== Parallelism ==
The goal of parallelism is to run an algorithm on multiple cores / computers in parallel for faster results. Because Haskell doesn't impose an execution order thanks to its purity, it is well-suited for formulating parallel algorithms.

Currently, parallelism in Haskell is still a research topic and subject to experimentation. There are combinators for controlling the execution order {{Haskell lib|p=parallel|v=1.0.0.0|Control|Parallel|Strategies}}, but they are rather fine-grained. Research on a less ambitious but more practical alternative [http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell Data Parallel Haskell] is ongoing.

The chapter [[../Parallelism/]] is not yet written but is intended to be a gentle introduction to parallel algorithms and current possibilities in Haskell.

{{Haskell navigation|chapter=Haskell Performance}}

{{Auto category}}
