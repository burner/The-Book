>{{Haskell minitoc|chapter=Haskell Performance}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Haskell Performance}}

== Notes and TODOs ==
<i>
* TODO: Pour lazy evaluation explanation from [[../Laziness/]] into this mold.
* TODO: better section names.
* TODO: ponder the graphical representation of graphs.
** No grapical representation, do it with <code>let .. in</code>. Pro: Reduction are easiest to perform in that way anyway. Cons: no graphic.
** ASCII art / line art similar to the one in Bird&Wadler? Pro: displays only the relevant parts truly as graph, easy to perform on paper. Cons: Ugly, no large graphs with that.
** Full blown graphs with @-nodes? Pro: look graphy. Cons: nobody needs to know @-nodes in order to understand graph reduction. Can be explained in the implementation section.
** Graphs without @-nodes. Pro: easy to understand. Cons: what about currying?
* ! Keep this chapter short. The sooner the reader knows how to evaluate Haskell programs by hand, the better.
* First sections closely follow Bird&Wadler
</i>

== Introduction ==
Programming is not only about writing correct programs, answered by denotational semantics, but also about writing fast ones that require little memory. For that, we need to know how they're executed on a machine, commonly given by operational semantics. This chapter explains how Haskell programs are commonly executed on a real computer and thus serves as foundation for analyzing time and space usage. Note that the Haskell standard deliberately does <em>not</em> give operational semantics, implementations are free to choose their own. But so far, every implementation of Haskell more or less closely follows the execution model of <em>lazy evaluation</em>.

In the following, we will detail lazy evaluation and subsequently use this execution model to explain and exemplify the reasoning about time and memory complexity of Haskell programs.

== Evaluating Expressions by Lazy Evaluation ==
=== Reductions ===
Executing a functional program, i.e. evaluating an expression, means to repeatedly apply function definitions until all function applications have been expanded. Take for example the expression <code>pythagoras 3 4</code> together with the definitions

       square x = x * x
 pythagoras x y = square x + square y

One possible sequence of such <b>reduction</b>s is

 pythagoras 3 4
  &rArr; square 3 + square 4   (pythagoras)
  &rArr;    (3*3) + square 4   (square)
  &rArr;        9 + square 4   (*)
  &rArr;        9 + (4*4)      (square)
  &rArr;        9 + 16         (*)
  &rArr;          25

Every reduction replaces a subexpression, called <b>reducible expression</b> or <b>redex</b> for short, with an equivalent one, either by appealing to a function definition like for <code>square</code> or by using a built-in function like <code>(+)</code>. An expression without redexes is said to be in <b>normal form</b>. Of course, execution stops once reaching a normal form which thus is the result of the computation.

Clearly, the fewer reductions that have to be performed, the faster the program runs. We cannot expect each reduction step to take the same amount of time because its implementation on real hardware looks very different, but in terms of asymptotic complexity, this number of reductions is an accurate measure.

=== Reduction Strategies ===
There are many possible reduction sequences and the number of reductions may depend on the order in which reductions are performed. Take for example the expression <code>fst (square 3, square 4)</code>. One systematic possibility is to evaluate all function arguments before applying the function definition

 fst (square 3, square 4)
  &rArr; fst (3*3, square 4)   (square)
  &rArr; fst ( 9 , square 4)   (*)
  &rArr; fst ( 9 , 4*4)        (square)
  &rArr; fst ( 9 , 16 )        (*)
  &rArr; 9                     (fst)

This is called an <b>innermost reduction</b> strategy and an <b>innermost redex</b> is a redex that has no other redex as subexpression inside.

Another systematic possibility is to apply all function definitions first and only then evaluate arguments:

 fst (square 3, square 4)
  &rArr;  square 3             (fst)
  &rArr;  3*3                  (square)
  &rArr;  9                    (*)

which is named <b>outermost reduction</b> and always reduces <b>outermost redex</b>es that are not inside another redex. Here, the outermost reduction uses fewer reduction steps than the innermost reduction. Why? Because the function <code>fst</code> doesn't need the second component of the pair and the reduction of <code>square 4</code> was superflous.

=== Termination ===
For some expressions like

 loop = 1 + loop

no reduction sequence may terminate and program execution enters a neverending loop, those expressions do not have a normal form. But there are also expressions where some reduction sequences terminate and some do not, an example being

 fst (42, loop)
  &rArr;  42                   (fst)
 
 fst (42, loop)
  &rArr;  fst (42,1+loop)      (loop)
  &rArr;  fst (42,1+(1+loop))  (loop)
  &rArr;  ...

The first reduction sequence is outermost reduction and the second is innermost reduction which tries in vain to evaluate the <code>loop</code> even though it is ignored by <code>fst</code> anyway. The ability to evaluate function arguments only when needed is what makes outermost optimal when it comes to termination:

;Theorem (Church Rosser II): If there is one terminating reduction, then outermost reduction will terminate, too.

=== Graph Reduction ===
Despite the ability to discard arguments, outermost reduction doesn't always take fewer reduction steps than innermost reduction:

 square (1+2)
  &rArr;  (1+2)*(1+2)          (square)
  &rArr;  (1+2)*3              (+)
  &rArr;      3*3              (+)
  &rArr;       9               (*)

Here, the argument <code>(1+2)</code> is duplicated and subsequently reduced twice. But because it is one and the same argument, the solution is to share the reduction <code>(1+2) &rArr; 3</code> with all other incarnations of this argument. This can be achieved by representing expressions as <em>graphs</em>. For example, 

  __________
 |   |     &darr;
 &loz; * &loz;     (1+2)

represents the expression <code>(1+2)*(1+2)</code>. Now, the <b>outermost graph reduction</b> of <code>square (1+2)</code> proceeds as follows

 square (1+2)
  &rArr;  __________           (square)
     |   |     &darr;
     &loz; * &loz;     (1+2)
  &rArr;  __________           (+)
     |   |     &darr;
     &loz; * &loz;      3
 
  &rArr; 9                     (*)

and the work has been shared. In other words, outermost graph reduction now reduces every argument at most once. For this reason, it always takes fewer reduction steps than the innermost reduction, a fact we will prove when [[#Reasoning about Time|reasoning about time]].

Sharing of expressions is also introduced with <code>let</code> and <code>where</code> constructs. For instance, consider [[w:Heron's formula|Heron's formula]] for the area of a triangle with sides <code>a</code>,<code>b</code> and <code>c</code>:

 area a b c = let s = (a+b+c)/2 in
      sqrt (s*(s-a)*(s-b)*(s-c))

Instantiating this to an equilateral triangle will reduce as

 area 1 1 1
  &rArr;        _____________________             (area)
           |  |    |     |      &darr;
     sqrt (&loz;*(&loz;-a)*(&loz;-b)*(&loz;-c))  ((1+1+1)/2)
  &rArr;        _____________________             (+),(+),(/)
           |  |    |     |      &darr;
     sqrt (&loz;*(&loz;-a)*(&loz;-b)*(&loz;-c))  1.5
  &rArr;
     ...
  &rArr;
     0.433012702

which is <math>\sqrt3/4</math>. Put differently, <code>let</code>-bindings simply give names to nodes in the graph. In fact, one can dispense entirely with a graphical notation and solely rely on <code>let</code> to mark sharing and express a graph structure.<ref>{{cite journal | author = John Maraist, Martin Odersky, and Philip Wadler
 | year = 1998
 | month = May
 | title = The call-by-need lambda calculus
 | journal = Journal of Functional Programming
 | volume = 8
 | issue = 3
 | pages = 257-317
 | url = http://homepages.inf.ed.ac.uk/wadler/topics/call-by-need.html#need-journal
 }}</ref>

Any implementation of Haskell is in some form based on outermost graph reduction which thus provides a good model for reasoning about the asympotic complexity of time and memory allocation. The number of reduction steps to reach normal form corresponds to the execution time and the size of the terms in the graph corresponds to the memory used.

{{Exercises|1=
<ol><li> Reduce <code>square (square 3)</code> to normal form with innermost, outermost and outermost graph reduction.
</li><li> Consider the fast exponentiation algorithm
 power x 0 = 1
 power x n = x' * x' * (if n `mod` 2 == 0 then 1 else x)
   where x' = power x (n `div` 2)
that takes <code>x</code> to the power of <code>n</code>. Reduce <code>power 2 5</code> with innermost and outermost graph reduction. How many reductions are performed? What is the asymptotic time complexity for the general <code>power 2 n</code>? What happens to the algorithm if we use "graphless" outermost reduction?
</li></ol> }}

=== Pattern Matching ===
So far, our description of outermost graph reduction is still underspecified when it comes to pattern matching and data constructors. Explaining these points will enable the reader to trace most cases of the reduction strategy that is commonly the base for implementing non-strict functional languages like Haskell. It is called <b>call-by-need</b> or <b>lazy evaluation</b> in allusion to the fact that it "lazily" postpones the reduction of function arguments to the last possible moment. Of course, the remaining details are covered in subsequent chapters.

To see how pattern matching needs specification, consider for example the boolean disjunction

 or True  y = True
 or False y = y

and the expression

 or (1==1) loop

with a non-terminating <code>loop = not loop</code>. The following reduction sequence 
 
 or (1==1) loop
  &rArr; or (1==1) (not loop)        (loop)
  &rArr; or (1==1) (not (not loop))  (loop)
  &rArr; ...

only reduces outermost redexes and therefore is an outermost reduction. But

 or (1==1) loop
  &rArr; or True   loop              (or)
  &rArr; True

makes much more sense. Of course, we just want to apply the definition of <code>or</code> and are only reducing arguments to decide which equation to choose. This intention is captured by the following rules for pattern matching in Haskell:
* Left hand sides are matched from top to bottom
* When matching a left hand side, arguments are matched from left to right
* Evaluate arguments only as much as needed to decide whether they match or not.
Thus, for our example <code>or (1==1) loop</code>, we have to reduce the first argument to either <code>True</code> or <code>False</code>, then evaluate the second to match a variable <code>y</code> pattern and then expand the matching function definition. As the match against a variable always succeeds, the second argument will not be reduced at all. It is the second reduction section above that reproduces this behavior.

With these preparations, the reader should now be able to evaluate most Haskell expressions. Here are some random encounters to test this ability:
{{Exercises|1=Reduce the following expressions with lazy evaluation to normal form. Assume the standard function definitions from the Prelude.
*<code>length [42,42+1,42-1]</code>
*<code>head (map (2*) [1,2,3])</code>
*<code>head $ [1,2,3] ++ (let loop = tail loop in loop)</code>
*<code>zip [1..3] (iterate (+1) 0)</code>
*<code>head $ concatMap (\x -> [x,x+1]) [1,2,3]</code>
*<code>take (42-6*7) $ map square [2718..3146]</code>
}}

=== Higher Order Functions ===
The remaining point to clarify is the reduction of higher order functions and currying. For instance, consider the definitions

 id x = x
 a = id (+1) 41

 twice f = f . f
 b = twice (+1) (13*3)

where both <code>id</code> and <code>twice</code> are only defined with one argument. The solution is to see multiple arguments as subsequent applications to one argument, this is called <b>currying</b>

 a = (id    (+1)) 41
 b = (twice (+1)) (13*3)

To reduce an arbitrary application <code><i>expression<sub>1</sub></i> <i>expression<sub>2</sub></i></code>, call-by-need first reduce <i>expression<sub>1</sub></i> until this becomes a function whose definition can be unfolded with the argument <code><i>expression<sub>2</sub></i></code>. Hence, the reduction sequences are

 a
  &rArr; (id (+1)) 41          (a)
  &rArr; (+1) 41               (id)
  &rArr; 42                    (+)
 
 b
  &rArr; (twice (+1)) (13*3)   (b)
  &rArr; ((+1).(+1) ) (13*3)   (twice)
  &rArr; (+1) ((+1) (13*3))    (.)
  &rArr; (+1) ((+1)  39)       (*)
  &rArr; (+1) 40               (+)
  &rArr; 41                    (+)

Admittedly, the description is a bit vague and the next section will detail a way to state it  clearly.

While it may seem that pattern matching is the workhorse of time intensive computations and higher order functions are only for capturing the essence of an algorithm, functions are indeed useful as data structures. One example are difference lists (<code>[a] -> [a]</code>) that permit concatenation in <math>O(1)</math> time, another is the representation of a stream by a fold. In fact, all data structures are represented as functions in the pure lambda calculus, the root of all functional programming languages.

<i>Exercises! Or not? Diff-Lists Best done with <code>foldl (++)</code> but this requires knowledge of the fold example. Oh, where do we introduce the foldl VS. foldr example at all? Hm, Bird&Wadler sneak in an extra section "Meet again with fold" for the (++) example at the end of "Controlling reduction order and space requirements" :-/ The complexity of (++) is explained when arguing about <code>reverse</code>.</i>

=== Weak Head Normal Form ===
To formulate precisely how lazy evaluation chooses its reduction sequence, it is best to abandon equational function definitions and replace them with an expression-oriented approach. In other words, our goal is to translate function definitions like <code>f (x:xs) = ...</code> into the form <code>f = <i>expression</i></code>. This can be done with two primitives, namely case-expressions and lambda abstractions.

In their primitive form, case-expressions only allow the discrimination of the outermost constructor. For instance, the primitive case-expression for lists has the form

 case <i>expression</i> of
   []   -> ...
   x:xs -> ...

Lambda abstractions are functions of one parameter, so that the following two definitions are equivalent

 f x = <i>expression</i>
 f   = \x -> <i>expression</i>

Here is a translation of the definition of <code>zip</code>

 zip :: [a] -> [a] -> [(a,a)]
 zip []      ys      = []
 zip xs      []      = []
 zip (x:xs') (y:ys') = (x,y):zip xs' ys'

to case-expressions and lambda-abstractions:

 zip = \xs -> \ys ->
    case xs of
       []    -> []
       x:xs' ->
          case ys of
             []    -> []
             y:ys' -> (x,y):zip xs' ys'

Assuming that all definitions have been translated to those primitives, every redex now has the form of either
* a function application <code> (\<i>variable</i>-><i>expression<sub>1</sub></i>) <i>expression<sub>2</sub></i> </code>
* or a case-expression <code>case <i>expression</i> of { ... } </code>

<em>lazy evaluation</em>.

;Weak Head Normal Form:An expression is in weak head normal form, iff it is either
*a constructor (eventually applied to arguments) like <code>True</code>, <code>Just (square 42)</code> or <code>(:) 1</code>
*a built-in function applied to too few arguments (perhaps none) like <code>(+) 2</code> or <code>sqrt</code>.
*or a lambda abstraction <code>\x -> <i>expression</i></code>.

<i>functions types cannot be pattern matched anyway, but the devious seq can evaluate them to WHNF nonetheless. "weak" = no reduction under lambdas. "head" = first the function application, then the arguments.
</i>

=== Strict and Non-strict Functions ===
<i>A non-strict function doesn't need its argument. A strict function needs its argument in WHNF, as long as we do not distinguish between different forms of non-termination (<code>f x = loop</code> doesn't need its argument, for example).</i>

== Controlling Space ==

"Space" here may be better visualized as traversal of a graph. Either a data structure, or an induced dependencies graph. For instance : Fibonacci(N) depends on : Nothing if N = 0 or N = 1 ; Fibonacci(N-1) and Fibonacci(N-2) else. As Fibonacci(N-1) depends on Fibonacci(N-2), the induced graph is not a tree. Therefore, there is a correspondence between implementation technique and data structure traversal :
{| class="wikitable"
|-
! Corresponding Implementation technique !! Data Structure Traversal
|-
| Memoization || Depth First Search (keep every intermediary result in memory)
|-
| Parallel evaluation || Bread First Search (keep every intermediary result in memory, too)
|-
| Sharing || Directed acyclic graph traversal (Maintain only a "frontier" in memory.)
|-
| Usual recursion || Tree traversal (Fill a stack)
|-
| Tail recursion || List traversal / Greedy Search (Constant space)
|}

The classical :

  fibo 0 = 1
  fibo 1 = 1
  fibo n = fibo (n-1) + fibo (n-2)

Is a tree traversal applied to a directed acyclic graph for the worse. The optimized version :

 fibo n = 
  let f a b m =
     if m = 0 then a
     if m = 1 then b
     f b (a+b) (m-1)
 in f 1 1 n

Uses a DAG traversal. Luckily, the frontier size is constant, so it's a tail recursive algorithm.

<i>NOTE: The chapter [[../Strictness]] is intended to elaborate on the stuff here.

NOTE: The notion of strict function is to be introduced before this section.

Now's the time for the space-eating fold example:

 foldl (+) 0 [1..10]

Introduce <code>seq</code> and <code>$!</code> that can force an expression to WHNF. => <code>foldl'</code>.


Tricky space leak example:

 (\xs -> head xs + last xs) [1..n]
 (\xs -> last xs + head xs) [1..n]

The first version runs on O(1) space. The second in O(n).
</i>

=== Sharing and CSE ===
<i>NOTE: overlaps with section about time. Hm, make an extra memoization section?

How to share

 foo x y = -- s is not shared
 foo x = \y -> s + y
   where s = expensive x -- s is shared

"Lambda-lifting", "Full laziness". The compiler should not do full laziness.

A classic and important example for the trade between space and time:

 sublists []      = [[]]
 sublists (x:xs)  = sublists xs ++ map (x:) (sublists xs)
 sublists' (x:xs) = let ys = sublists' xs in ys ++ map (x:) ys

That's why the compiler should not do common subexpression elimination as optimization. (Does GHC?).
</i>

=== Tail recursion ===
<i>NOTE: Does this belong to the space section? I think so, it's about stack space.

Tail recursion in Haskell looks different.
</i>

== Reasoning about Time ==
<i>Note: introducing strictness before the upper time bound saves some hassle with explanation?</i>

=== Lazy eval < Eager eval ===
<i>When reasoning about execution time, naively performing graph reduction by hand to get a clue on what's going is most often infeasible. In fact, the order of evaluation taken by lazy evaluation is difficult to predict by humans, it is much easier to trace the path of eager evaluation where arguments are reduced to normal form before being supplied to a function. But knowing that lazy evaluation always performs less reduction steps than eager evaluation (present the proof!), we can easily get an upper bound for the number of reductions by pretending that our function is evaluated eagerly.

Example:
 or = foldr (||) False
 isPrime n = not $ or $ map (\k -> n `mod` k == 0) [2..n-1]

=> eager evaluation always takes n steps, lazy won't take more than that. But it will actually take fewer.
</i>

=== Throwing away arguments ===
<i>Time bound exact for functions that examine their argument to normal form anyway. The property that a function needs its argument can concisely be captured by denotational semantics:

 f &perp; = &perp;

Argument in WHNF only, though. Operationally: non-termination -> non-termination. (this is an approximation only, though because f anything = &perp; doesn't "need" its argument). Non-strict functions don't need their argument and eager time bound is not sharp. But the information whether a function is strict or not can already be used to great benefit in the analysis.

 isPrime n = not $ or $ (n `mod` 2 == 0) : (n `mod` 3 == 0) : ...

It's enough to know <code>or True &perp; = True</code>.

Other examples:
*<code>foldr (:) []</code> vs. <code>foldl (flip (:)) []</code> with &perp;.
* Can <code>head . mergesort</code> be analyzed only with &perp;? In any case, this example is too involed and belongs to [[../Laziness]].
</i>

=== Persistence & Amortisation ===
<i>
NOTE: this section is better left to a data structures chapter because the subsections above cover most of the cases a programmer not focussing on data structures / amortization will encounter.

Persistence = no updates in place, older versions are still there.
Amortisation = distribute unequal running times across a sequence of operations.
Both don't go well together in a strict setting. Lazy evaluation can reconcile them. Debit invariants. Example: incrementing numbers in binary representation.
</i>

== Implementation of Graph reduction ==
<i>Small talk about G-machines and such. Main definition:

closure = thunk = code/data pair on the heap. What do they do? Consider <math>(\lambda x.\lambda y.x+y) 2</math>. This is a function that returns a function, namely <math>\lambda y.2+y</math> in this case. But when you want to compile code, it's prohibitive to actually perform the substitution in memory and replace all occurrences of <math>x</math> by 2. So, you return a closure that consists of the function code <math>\lambda y.x+y</math> and an environment <math>\{x=2\}</math> that assigns values to the free variables appearing in there.</i>

<i>
GHC (?, most Haskell implementations?) avoid free variables completely and use supercombinators. In other words, they're supplied as extra-parameters and the observation that lambda-expressions with too few parameters don't need to be reduced since their WHNF is not very different.

Note that these terms are technical terms for implementation stuff, lazy evaluation happily lives without them. Don't use them in any of the sections above.
</i>

==References==
<references/>
* {{cite book
 |title=Introduction to Functional Programming using Haskell
 |last=Bird|first=Richard
 |publisher=Prentice Hall
 |year=1998
 |id=ISBN 0-13-484346-0
 }}
* {{cite book
 |title=The Implementation of Functional Programming Languages
 |last=Peyton-Jones|first=Simon
 |publisher=Prentice Hall
 |year=1987
 |url=http://research.microsoft.com/~simonpj/papers/slpj-book-1987/
 }}

{{Haskell navigation|chapter=Haskell Performance}}

[[Category:Haskell|{{SUBPAGENAME}}]]
