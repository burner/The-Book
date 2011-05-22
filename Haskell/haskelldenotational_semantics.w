>{{info|'''New readers: Please report stumbling blocks!''' While the material on this page is intended to explain clearly, there are always mental traps that innocent readers new to the subject fall in but that the authors are not aware of.  Please report any tricky passages to the [[Talk:Haskell/Denotational_semantics|Talk]] page or the #haskell IRC channel so that the style of exposition can be improved.}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{info|'''New readers: Please report stumbling blocks!''' While the material on this page is intended to explain clearly, there are always mental traps that innocent readers new to the subject fall in but that the authors are not aware of.  Please report any tricky passages to the [[Talk:Haskell/Denotational_semantics|Talk]] page or the #haskell IRC channel so that the style of exposition can be improved.}}

{{Haskell minitoc|chapter=Wider Theory}}

== Introduction ==
This chapter explains how to formalize the meaning of Haskell programs, the '''denotational semantics'''. It may seem to be nit-picking to formally specify that the program <code>square x = x*x</code> means the same as the mathematical  square function that maps each number to its square, but what about the meaning of a program like <code>f x = f (x+1)</code> that loops forever? In the following, we will exemplify the approach first taken by Scott and Strachey to this question and obtain a foundation to reason about the correctness of functional programs in general and recursive definitions in particular. Of course, we will concentrate on those topics needed to understand Haskell programs<ref>In fact, there are no written down and complete denotational semantics of Haskell. This would be a tedious task void of additional insight and we happily embrace the folklore and common sense semantics.</ref>.

Another aim of this chapter is to illustrate the notions '''strict''' and '''lazy''' that capture the idea that a function needs or needs not to evaluate its argument. This is a basic ingredient to predict the course of evaluation of Haskell programs and hence of primary interest to the programmer. Interestingly, these notions can be formulated concisely with denotational semantics alone, no reference to an execution model is necessary. They will be put to good use in [[Haskell/Graph reduction|Graph Reduction]], but it is this chapter that will familiarize the reader with the denotational definition and involved notions such as &perp; ("Bottom"). The reader only interested in strictness may wish to poke around in section [[#Bottom and Partial Functions|Bottom and Partial Functions]] and quickly head over to [[#Strict and Non-Strict Semantics|Strict and Non-Strict Semantics]].

=== What are Denotational Semantics and what are they for? ===
What does a Haskell program mean? This question is answered by the '''denotational semantics''' of Haskell. In general, the denotational semantics of a programming language map each of its programs to a mathematical object (denotation), that represents the ''meaning'' of the program in question. As an example, the mathematical object for the Haskell programs <code>10</code>, <code>9+1</code>, <code>2*5</code> and <code>sum [1..4]</code> can be represented by the integer ''10''. We say that all those programs '''denote''' the integer ''10''. The collection of such mathematical objects is called the '''semantic domain'''.

The mapping from program codes to a semantic domain is commonly written down with double square brackets ("Oxford brackets") around  program code. For example,
<center><math>[\![\texttt{2*5}]\!] = 10.</math></center>
Denotations are ''compositional'', i.e. the meaning of a program like <code>1+9</code> only depends on the meaning of its constituents:
<center><math>[\![\texttt{a+b}]\!] = [\![\texttt{a}]\!]+[\![\texttt{b}]\!].</math></center>
The same notation is used for types, i.e.
<center><math>[\![\texttt{Integer}]\!]=\mathbb{Z}.</math></center>
For simplicity however, we will silently identify expressions with their semantic objects in subsequent chapters and use this notation only when clarification is needed.

It is one of the key properties of ''purely functional'' languages like Haskell that a direct mathematical interpretation like "<code>1+9</code> denotes ''10''" carries over to functions, too: in essence, the denotation of a program of type <code>Integer -> Integer</code> is a mathematical function <math>\mathbb{Z}\to\mathbb{Z}</math> between integers. While we will see that this expression needs refinement generally, to include non-termination, the situation for ''imperative languages'' is clearly worse: a procedure with that type denotes something that changes the state of a machine in possibly unintended ways. Imperative languages are tightly tied to '''operational semantics''' which describes their way of execution on a machine. It is possible to define a denotational semantics for imperative programs and to use it to reason about such programs, but the semantics often has operational nature and sometimes must be extended in comparison to the denotational semantics for functional languages.<ref>Monads are one of the most successful ways to give denotational semantics to imperative programs. See also [[Haskell/Advanced monads]].</ref> In contrast, the meaning of purely functional languages is ''by default'' completely independent from their way of execution. The Haskell98 standard even goes as far as to specify only Haskell's non-strict denotational semantics, leaving open how to implement them.

In the end, denotational semantics enables us to develop formal proofs that programs indeed do what we want them to do mathematically. Ironically, for proving program properties in day-to-day Haskell, one can use [[Haskell/Equational reasoning|Equational reasoning]], which transforms programs into equivalent ones without seeing much of the underlying mathematical objects we are concentrating on in this chapter. But the denotational semantics actually show up whenever we have to reason about non-terminating programs, for instance in [[#Recursive Data Types and Infinite Lists|Infinite Lists]].

Of course, because they only state what a program is, denotational semantics cannot answer questions about how long a program takes or how much memory it eats; this is governed by the ''evaluation strategy'' which dictates how the computer calculates the normal form of an expression. On the other hand, the implementation has to respect the semantics, and to a certain extent, it is the semantics that determine how Haskell programs must be evaluated on a machine. We will elaborate on this in [[#Strict and Non-Strict Semantics|Strict and Non-Strict Semantics]].

=== What to choose as Semantic Domain? ===
We are now looking for suitable mathematical objects that we can attribute to every Haskell program. In case of the example <code>10</code>, <code>2*5</code> and <code>sum [1..4]</code>, it is clear that all expressions should denote the integer ''10''. Generalizing, every value <code>x</code> of type <code>Integer</code> is likely to be an element of the set <math>\mathbb{Z}</math>. The same can be done with values of type <code>Bool</code>. For functions like <code>f :: Integer -> Integer</code>, we can appeal to the mathematical definition of "function" as a set of (argument,value)-pairs, its ''graph''.

But interpreting functions as their graph was too quick, because it does not work well with recursive definitions. Consider the definition

 shaves :: Integer -> Integer -> Bool
 1 `shaves` 1 = True
 2 `shaves` 2 = False
 0 `shaves` x = not (x `shaves` x)
 _ `shaves` _ = False

We can think of <code>0</code>,<code>1</code> and <code>2</code> as being male persons with long beards and the question is who shaves whom. Person <code>1</code> shaves himself, but <code>2</code> gets shaved by the barber <code>0</code> because evaluating the third equation yields <code>0 `shaves` 2 == True</code>. In general, the third line says that the barber <code>0</code> shaves all persons that do not shave themselves.

What about the barber himself, is <code>0 `shaves` 0</code> true or not? If it is, then the third equation says that it is not. If it is not, then the third equation says that it is. Puzzled, we see that we just cannot attribute <code>True</code> or <code>False</code> to <code>0 `shaves` 0</code>, the graph we use as interpretation for the function <code>shaves</code> must have an empty spot. We realize that our semantic objects must be able to incorporate '''partial functions''', functions that are undefined for some arguments.

It is well known that this famous example gave rise to serious foundational problems in set theory. It's an example of an '''impredicative''' definition, a definition which uses itself, a logical circle. Unfortunately for recursive definitions, the circle is not the problem but the feature.

== Bottom and Partial Functions ==
=== &perp; Bottom ===
To define partial functions, we introduce a special value <math>\perp</math>, named '''bottom''' and commonly written <code>_|_</code> in typewriter font. We say that <math>\perp</math> is the completely '''"undefined" value''' or function. Every basic data type like <code>Integer</code> or <code>()</code> contains one <math>\perp</math> besides their usual elements. So the possible values of type <code>Integer</code> are

<center><math>\perp, 0, \pm 1, \pm 2, \pm 3, \dots</math></center>

Adding <math>\perp</math> to the set of values is also called '''lifting'''. This is often depicted by a subscript like in <math>\mathbb{Z}_\perp</math>. While this is the correct notation for the mathematical set "lifted integers", we prefer to talk about "values of type <code>Integer</code>". We do this because <math>\mathbb{Z}_\perp</math> suggests that there are "real" integers <math>\mathbb{Z}</math>, but inside Haskell, the "integers" are <code>Integer</code>.

As another example, the type <code>()</code> with only one element actually has two inhabitants:

<center><math>\perp, ()</math></center>

For now, we will stick to programming with <code>Integer</code>s. Arbitrary algebraic data types will be treated in section [[#Algebraic Data Types|Algebraic Data Types]] as strict and non-strict languages diverge on how these include <math>\perp</math>.

In Haskell, the expression <code>undefined</code> denotes <math>\perp</math>. With its help, one can indeed verify some semantic properties of actual Haskell programs. <code>undefined</code> has the polymorphic type <code>forall a . a</code> which of course can be specialized to <code>undefined :: Integer</code>, <code>undefined :: ()</code>, <code>undefined :: Integer -> Integer</code> and so on. In the Haskell Prelude, it is defined as

<center><code>undefined = error "Prelude.undefined"</code></center>

As a side note, it follows from [[Haskell/The Curry-Howard isomorphism|the Curry-Howard isomorphism]] that any value of the polymorphic type <code>forall a . a</code> must denote <math>\perp</math>.

=== Partial Functions and the Semantic Approximation Order ===
Now, <math>\perp</math> (''bottom type'') gives us the possibility to denote partial functions:

<center><math>f(n) = \begin{cases}
  1 & \mbox{ if } n \mbox{ is } 0 \\
  -2 & \mbox{ if } n \mbox{ is } 1 \\
  \perp & \mbox{ else }
\end{cases}
</math></center>

Here, <math>f(n)</math> yields well defined values for <math>n=0</math> and <math>n=1</math> but gives <math>\perp</math> for all other <math>n</math>. Note that the type <math>\perp</math> is universal, as <math>\perp</math> has no value: the function <math>\perp</math><code>:: Integer -> Integer</code> is given by

<center><math>\perp(n) = \perp</math> for all <math>n</math></center>

where the <math>\perp</math> on the right hand side denotes a value of type <code>Integer</code>.

To formalize, '''partial functions''' say, of type <code>Integer -> Integer</code> are at least mathematical mappings from the lifted integers <math>\mathbb{Z}_\perp=\{\perp, 0, \pm 1, \pm 2, \pm 3, \dots\}</math> to the lifted integers. But this is not enough, since it does not acknowledge the special role of <math>\perp</math>. For example, the definition

<center><math>g(n) = \begin{cases}
  1 & \mbox{ if } n \mbox{ is } \perp \\
  \perp & \mbox{ else }
\end{cases}
</math></center>

looks counterintuitive, and, in fact, is wrong. Why does <math>g(\perp)</math> yield a defined value whereas <math>g(1)</math> is undefined? The intuition is that every partial function <math>g</math> should yield more defined answers for more defined arguments. To formalize, we can say that every concrete number is '''more defined''' than <math>\perp</math>:

<center><math>\perp\ \sqsubset 1\ ,\ \perp\ \sqsubset 2\ , \dots</math></center>

Here, <math>a\sqsubset b</math> denotes that <math>b</math> is more defined than <math>a</math>. Likewise, <math>a\sqsubseteq b</math> will denote that either <math>b</math> is more defined than <math>a</math> or both are equal (and so have the same definedness). <math>\sqsubset</math> is also called the '''semantic approximation order''' because we can approximate defined values by less defined ones thus interpreting "more defined" as "approximating better". Of course, <math>\perp</math> is designed to be the least element of a data type, we always have that <math>\perp\ \sqsubset x</math> for all <math>x</math>, except the case when <math>x</math> happens to denote <math>\perp</math> itself:

<center><math>\forall x\neq\perp\ \ \ \perp\sqsubset x
</math></center>

As no number is ''more defined'' than another, the mathematical relation <math>\sqsubset</math> is false for any pair of numbers:

<center><math> 1 \sqsubset 1 </math> does not hold.</center>
<center>neither <math>1 \sqsubset 2</math> nor <math>2 \sqsubset 1</math> hold.</center>

This is contrasted to ordinary order predicate <math>\le</math>, which can compare any two numbers. A quick way to remember this is the sentence: "<math>1</math> and <math>2</math> are different in terms of ''information content'' but are equal in terms of ''information quantity''". That's another reason why we use a different symbol: <math>\sqsubseteq</math>.
<center>neither <math>1 \sqsubseteq 2</math> nor <math>2 \sqsubseteq 1</math> hold,</center>
<center>but <math>1 \sqsubseteq 1</math> holds.</center>

One says that <math>\sqsubseteq</math> specifies a '''partial order''' and that the values of type <code>Integer</code> form a '''partially ordered set''' ('''poset''' for short). A partial order is characterized by the following three laws
* ''Reflexivity'', everything is just as defined as itself: <math>x \sqsubseteq x</math> for all <math>x</math>
* ''Transitivity'': if <math>x \sqsubseteq y</math> and <math>y \sqsubseteq z</math>, then <math>x \sqsubseteq z</math>
* ''Antisymmetry'': if both <math>x \sqsubseteq y</math> and <math>y \sqsubseteq x</math> hold, then <math>x</math> and <math>y</math> must be equal: <math>x=y</math>.
{{Exercises|1=Do the integers form a poset with respect to the order <math>\le</math>?}}

We can depict the order <math>\sqsubseteq</math> on the values of type <code>Integer</code> by the following graph

[[Image:Int-graph.png]]

where every link between two nodes specifies that the one above is more defined than the one below. Because there is only one level (excluding <math>\perp</math>), one says that <code>Integer</code> is a ''flat domain''. The picture also explains the name of <math>\perp</math>: it's called ''bottom'' because it always sits at the bottom.

=== Monotonicity ===
Our intuition about partial functions now can be formulated as following: every partial function <math>f</math> is a '''monotone''' mapping between partially ordered sets. More defined arguments will yield more defined values:

<center><math> x\sqsubseteq y \Longrightarrow f(x)\sqsubseteq f(y) </math></center>

In particular, a function <math>h</math> with <math>h(\perp)=1</math> is constant: <math>h(n)=1</math> for all <math>n</math>. Note that here it is crucial that <math>1 \sqsubseteq 2</math> etc. don't hold.

Translated to Haskell, monotonicity means that we cannot use <math>\perp</math> as a condition, i.e. we cannot pattern match on <math>\perp</math>, or its equivalent <code>undefined</code>. Otherwise, the example <math>g</math> from above could be expressed as a Haskell program. As we shall see later, <math>\perp</math> also denotes non-terminating programs, so that the inability to observe <math>\perp</math> inside Haskell is related to the halting problem.

Of course, the notion of ''more defined than'' can be extended to partial functions by saying that a function is more defined than another if it is so at every possible argument:

<center><math>f \sqsubseteq g \mbox{ if } \forall x. f(x) \sqsubseteq g(x)</math></center>

Thus, the partial functions also form a poset, with the undefined function <math>\perp(x)=\perp</math> being the least element.

== Recursive Definitions as Fixed Point Iterations ==
=== Approximations of the Factorial Function ===
Now that we have a means to describe partial functions, we can give an interpretation to recursive definitions. Lets take the prominent example of the factorial function <math>f(n)=n!</math> whose recursive definition is

<center><math>f(n) = \mbox{ if } n == 0 \mbox{ then } 1
\mbox{ else } n \cdot f(n-1)</math></center>

Although we saw that interpreting this directly as a set description may lead to problems, we intuitively know that in order to calculate <math>f(n)</math> for every given <math>n</math> we have to iterate the right hand side. This iteration can be formalized as follows: we calculate a sequence of functions <math>f_k</math> with the property that each one consists of the right hand side applied to the previous one, that is

<center><math>f_{k+1}(n) = \mbox{ if } n == 0 \mbox{ then } 1
\mbox{ else } n \cdot f_k(n-1)</math></center>

We start with the undefined function <math>f_0(n) = \perp</math>, and the resulting sequence of partial functions reads:

<center><math>f_1(n) = \begin{cases}
  1 & \mbox{ if } n \mbox{ is } 0 \\
  \perp & \mbox{ else }
\end{cases} \ ,\ 
f_2(n) = \begin{cases}
  1 & \mbox{ if } n \mbox{ is } 0 \\
  1 & \mbox{ if } n \mbox{ is } 1 \\
  \perp & \mbox{ else }
\end{cases} \ ,\ 
f_3(n) = \begin{cases}
  1 & \mbox{ if } n \mbox{ is } 0 \\
  1 & \mbox{ if } n \mbox{ is } 1 \\
  2 & \mbox{ if } n \mbox{ is } 2 \\
  \perp & \mbox{ else }
\end{cases}
</math></center>

and so on. Clearly,

<center><math>\perp=f_0 \sqsubseteq f_1 \sqsubseteq f_2 \sqsubseteq \dots </math></center>

and we expect that the sequence converges to the factorial function.

The iteration follows the well known scheme of a fixed point iteration

<center><math> x_0, g(x_0), g(g(x_0)), g(g(g(x_0))), \dots </math></center>

In our case, <math>x_0</math> is a function and <math>g</math> is a ''functional'', a mapping between functions. We have

<center><math> x_0 = \perp</math> and
<math> g(x) = n\mapsto\mbox{ if } n == 0 \mbox{ then } 1 \mbox{ else } n*x(n-1) \,</math>
</center>

If we start with <math>x_0 = \perp</math>, the iteration will yield increasingly defined approximations to the factorial function 

<center><math> \perp\ \sqsubseteq g(\perp)\sqsubseteq g(g(\perp))\sqsubseteq g(g(g(\perp)))\sqsubseteq \dots </math></center>

(Proof that the sequence increases: The first inequality <math>\perp\ \sqsubseteq g(\perp)</math> follows from the fact that <math>\perp</math> is less defined than anything else. The second inequality follows from the first one by applying <math>g</math> to both sides and noting that <math>g</math> is monotone. The third follows from the second in the same fashion and so on.)


It is very illustrative to formulate this iteration scheme in Haskell. As functionals are just ordinary higher order functions, we have

 g :: (Integer -> Integer) -> (Integer -> Integer)
 g x = \n -> if n == 0 then 1 else n * x (n-1)
 
 x0 :: Integer -> Integer
 x0 = undefined
 
 (f0:f1:f2:f3:f4:fs) = iterate g x0

We can now evaluate the functions <code>f0,f1,...</code> at sample arguments and see whether they yield <code>undefined</code> or not:

  > f3 0
  1
  > f3 1
  1
  > f3 2
  2
  > f3 5
  *** Exception: Prelude.undefined
  > map f3 [0..]
  [1,1,2,*** Exception: Prelude.undefined
  > map f4 [0..]
  [1,1,2,6,*** Exception: Prelude.undefined
  > map f1 [0..]
  [1,*** Exception: Prelude.undefined

Of course, we cannot use this to check whether f4 is really undefined for all arguments.

=== Convergence ===
To the mathematician, the question whether this sequence of approximations converges is still to be answered. For that, we say that a poset is a '''directed complete partial order''' ('''dcpo''') iff every monotone sequence <math>x_0\sqsubseteq x_1\sqsubseteq \dots</math> (also called ''chain'') has a least upper bound (supremum)

<center><math>\sup_{\sqsubseteq} \{x_0\sqsubseteq x_1\sqsubseteq \dots\} = x</math>.</center>

If that's the case for the semantic approximation order, we clearly can be sure that monotone sequence of functions approximating the factorial function indeed has a limit. For our denotational semantics, we will only meet dcpos which have a least element <math>\perp</math> which are called '''complete partial order'''s ('''cpo''').

The <code>Integer</code>s clearly form a (d)cpo, because the monotone sequences consisting of more than one element must be of the form 

<center><math>\perp\ \sqsubseteq\dots\sqsubseteq\ \perp\ \sqsubseteq n\sqsubseteq n\sqsubseteq \dots\sqsubseteq n</math></center>

where <math>n</math> is an ordinary number. Thus, <math>n</math> is already the least upper bound.

For functions <code>Integer -> Integer</code>, this argument fails because monotone sequences may be of infinite length. But because <code>Integer</code> is a (d)cpo, we know that for every point <math>n</math>, there is a least upper bound

<center><math>\sup_{\sqsubseteq} \{\perp=f_0(n) \sqsubseteq f_1(n) \sqsubseteq f_2(n) \sqsubseteq \dots\} =: f(n)</math>.</center>

As the semantic approximation order is defined point-wise, the function <math>f</math> is the supremum we looked for.

These have been the last touches for our aim to transform the impredicative definition of the factorial function into a well defined construction. Of course, it remains to be shown that <math>f(n)</math> actually yields a defined value for every <math>n</math>, but this is not hard and far more reasonable than a completely ill-formed definition.

=== Bottom includes Non-Termination ===
It is instructive to try our newly gained insight into recursive definitions on an example that does not terminate:

<center><math>f(n) = f(n+1)</math></center>

The approximating sequence reads

<center><math>f_0 = \perp, f_1 = \perp, \dots</math></center>

and consists only of <math>\perp</math>. Clearly, the resulting limit is <math>\perp</math> again. From an operational point of view, a machine executing this program will loop indefinitely. We thus see that <math>\perp</math> may also denote a '''non-terminating''' function or value. Hence, given the halting problem, pattern matching on <math>\perp</math> in Haskell is impossible.

=== Interpretation as Least Fixed Point ===
Earlier, we called the approximating sequence an example of the well known "fixed point iteration" scheme. And of course, the definition of the factorial function <math>f</math> can also be thought as the specification of a fixed point of the functional <math>g</math>:

<center><math>f = g(f) = n\mapsto\mbox{ if } n == 0 \mbox{ then } 1
\mbox{ else } n\cdot f(n-1)</math></center>

However, there might be multiple fixed points. For instance, there are several <math>f</math> which fulfill the specification

<center><math>f = n\mapsto\mbox{ if } n == 0 \mbox{ then } 1
\mbox{ else } f(n+1)</math>,</center>

Of course, when executing such a program, the machine will loop forever on <math>f(1)</math> or <math>f(2)</math> and thus not produce any valuable information about the value of <math>f(1)</math>. This corresponds to choosing the ''least defined'' fixed point as semantic object <math>f</math> and this is indeed a canonical choice. Thus, we say that

<center><math>f=g(f)</math>,</center>

defines the '''least fixed point''' <math>f</math> of <math>g</math>. Clearly, ''least'' is with respect to our semantic approximation order <math>\sqsubseteq</math>.

The existence of a least fixed point is guaranteed by our iterative construction if we add the condition that <math>g</math> must be '''continuous''' (sometimes also called "chain continuous"). That simply means that <math>g</math> respects suprema of monotone sequences:

<center><math>\sup_{\sqsubseteq}\{g(x_0)\sqsubseteq g(x_1) \sqsubseteq\dots\} = g\left(\sup_{\sqsubseteq}\{x_0\sqsubseteq x_1\sqsubseteq\dots\}\right)</math></center>

We can then argue that with

<center><math>f=\sup_{\sqsubseteq}\{x_0\sqsubseteq g(x_0)\sqsubseteq g(g(x_0))\sqsubseteq\dots\}</math></center>

we have

<center><math>\begin{array}{lcl}
g(f) &=& g\left(\sup_{\sqsubseteq}\{x_0\sqsubseteq g(x_0)\sqsubseteq g(g(x_0))\sqsubseteq\dots\}\right)\\
&=& \sup_{\sqsubseteq}\{g(x_0)\sqsubseteq g(g(x_0))\sqsubseteq\dots\}\\
&=& \sup_{\sqsubseteq}\{x_0 \sqsubseteq g(x_0)\sqsubseteq g(g(x_0))\sqsubseteq\dots\}\\
&=& f
\end{array}</math></center>

and the iteration limit is indeed a fixed point of <math>g</math>. You may also want to convince yourself that the fixed point iteration yields the ''least'' fixed point possible.
{{Exercises|1=Prove that the fixed point obtained by fixed point iteration starting with <math>x_0=\perp</math> is also the least one, that it is smaller than any other fixed point. (Hint: <math>\perp</math> is the least element of our cpo and <math>g</math> is monotone)}}


By the way, how do we know that each Haskell function we write down indeed is continuous? Just as with monotonicity, this has to be enforced by the programming language. Admittedly, these properties can somewhat be enforced or broken at will, so the question feels a bit void. But intuitively, monotonicity is guaranteed by not allowing pattern matches on <math>\perp</math>. For continuity, we note that for an arbitrary type <code>a</code>, every simple function <code>a -> Integer</code> is automatically continuous because the monotone sequences of <code>Integer</code>'s are of finite length. Any infinite chain of values of type <code>a</code> gets mapped to a finite chain of <code>Integer</code>s and respect for suprema becomes a consequence of monotonicity. Thus, all functions of the special case <code>Integer -> Integer</code> must be continuous. For functionals like <math>g</math><code>::(Integer -> Integer) -> (Integer -> Integer)</code>, the continuity then materializes due to currying, as the type is isomorphic to <code>::((Integer -> Integer), Integer) -> Integer</code> and we can take <code>a=((Integer -> Integer), Integer)</code>.

In Haskell, the fixed interpretation of the factorial function can be coded as

<center><code>factorial = fix g</code></center>

with the help of the fixed point combinator

<center><code>fix :: (a -> a) -> a</code>.</center>

We can define it by

<center><code>fix f = let x = f x in x</code></center>

which leaves us somewhat puzzled because when expanding <math>factorial</math>, the result is not anything different from how we would have defined the factorial function in Haskell in the first place. But of course, the construction this whole section was about is not at all present when running a real Haskell program. It's just a means to put the mathematical interpretation a Haskell programs to a firm ground. Yet it is very nice that we can explore these semantics in Haskell itself with the help of <code>undefined</code>.

== Strict and Non-Strict Semantics ==
After having elaborated on the denotational semantics of Haskell programs, we will drop the mathematical function notation <math>f(n)</math> for semantic objects in favor of their now equivalent Haskell notation <code>f n</code>.

=== Strict Functions ===
A function <code>f</code> with one argument is called '''strict''', if and only if

<center><code>f &perp; = &perp;</code>.</center>

Here are some examples of strict functions

 id     x = x
 succ   x = x + 1
 power2 0 = 1
 power2 n = 2 * power2 (n-1)

and there is nothing unexpected about them. But why are they strict? It is instructive to prove that these functions are indeed strict. For <code>id</code>, this follows from the definition. For <code>succ</code>, we have to ponder whether <code>&perp; + 1</code> is <code>&perp;</code> or not. If it was not, then we should for example have <code>&perp; + 1 = 2</code> or more general <code>&perp; + 1 = ''k''</code> for some concrete number ''k''. We remember that every function is ''monotone'', so we should have for example

<center><code>2 = &perp; + 1 ⊑ 4 + 1 = 5</code></center>

as <code>&perp; ⊑ 4</code>. But neither of <code>2 ⊑ 5</code>, <code>2 = 5</code> nor <code>2 ⊒ 5</code> is valid so that ''k'' cannot be 2. In general, we obtain the contradiction

<center><code>''k'' = &perp; + 1 ⊑ ''k'' + 1 = ''k'' + 1</code>.</center>

and thus the only possible choice is

<center><code>succ &perp; = &perp; + 1 = &perp;</code></center>

and <code>succ</code> is strict.
{{Exercises|1=Prove that <code>power2</code> is strict. While one can base the proof on the "obvious" fact that <code>power2 ''n''</code> is <math>2^n</math>, the latter is preferably proven using fixed point iteration.}}

=== Non-Strict and Strict Languages ===
Searching for '''non-strict''' functions, it happens that there is only one prototype of a non-strict function of type <code>Integer -> Integer</code>:

 one x = 1

Its variants are <code>constk x = ''k''</code> for every concrete number <code>''k''</code>. Why are these the only ones possible? Remember that <code>one ''n''</code> can be no less defined than <code>one &perp;</code>. As <code>Integer</code> is a flat domain, both must be equal. 

Why is <code>one</code> non-strict? To see that it is, we use a Haskell interpreter and try

 > one (undefined :: Integer)
 1

which is not &perp;. This is reasonable as <code>one</code> completely ignores its argument. When interpreting &perp; in an operational sense as "non-termination", one may say that the non-strictness of <code>one</code> means that it does not force its argument to be evaluated and therefore avoids the infinite loop when evaluating the argument &perp;. But one might as well say that every function must evaluate its arguments before computing the result which means that <code>one &perp;</code> should be &perp;, too. That is, if the program computing the argument does not halt, <code>one</code> should not halt as well.<ref> Strictness as premature evaluation of function arguments is elaborated in the chapter [[Haskell/Graph Reduction|Graph Reduction]].</ref> It shows up that one can ''choose freely'' this or the other design for a functional programming language. One says that the language is ''strict'' or ''non-strict'' depending on whether functions are strict or non-strict by default. The choice for Haskell is non-strict. In contrast, the functional languages ML and Lisp choose strict semantics.

=== Functions with several Arguments ===
The notion of strictness extends to functions with several variables. For example, a function <code>f</code> of two arguments is ''strict in the second argument'' if and only if

<center><code> f x &perp; = &perp;</code></center>

for every <code>x</code>. But for multiple arguments, mixed forms where the strictness depends on the given value of the other arguments, are much more common. An example is the conditional

 cond b x y = if b then x else y

We see that it is strict in <code>y</code> depending on whether the test <code>b</code> is <code>True</code> or <code>False</code>:

 cond True  x &perp; = x
 cond False x &perp; = &perp;

and likewise for <code>x</code>. Apparently, <code>cond</code> is certainly &perp; if both <code>x</code> and <code>y</code> are, but not necessarily when at least one of them is defined. This behavior is called '''joint strictness'''.

Clearly, <code>cond</code> behaves like the if-then-else statement where it is crucial not to evaluate both the <code>then</code> and the <code>else</code> branches:

 if null xs then 'a' else head xs
 if n == 0  then  1  else 5 / n

Here, the else part is &perp; when the condition is met. Thus, in a non-strict language, we have the possibility to wrap primitive control statements such as if-then-else into functions like <code>cond</code>. This way, we can define our own control operators. In a strict language, this is not possible as both branches will be evaluated when calling <code>cond</code> which makes it rather useless. This is a glimpse of the general observation that non-strictness offers more flexibility for code reuse than strictness. See the chapter [[Haskell/Laziness|Laziness]]<ref>The term ''Laziness'' comes from the fact that the prevalent implementation technique for non-strict languages is called ''lazy evaluation''</ref> for more on this subject.

=== Not all Functions in Strict Languages are Strict ===

'''This section is wrong.'''

It is important to note that even in a strict language, not all functions are strict. The choice whether to have strictness and non-strictness by default only applies to certain argument data types. Argument types that solely contain data like <code>Integer</code>, <code>(Bool,Integer)</code> or <code>Either String [Integer]</code> impose strictness, but functions are not necessarily strict in ''function types'' like <code>Integer -> Bool</code>. Thus, in a hypothetical strict language with Haskell-like syntax, we would have the interpreter session 

 !> let const1 _ = 1
 
 !> const1 (undefined :: Integer)
 !!! Exception: Prelude.undefined
 
 !> const1 (undefined :: Integer -> Bool)
 1

Why are strict languages not strict in arguments of function type? If they were, fixed point iteration would crumble to dust! Remember the fixed point iteration

<center><math> \perp\ \sqsubseteq g(\perp)\sqsubseteq g(g(\perp)) \dots </math></center>

for a functional <math>g</math><code>::(Integer -> Integer) -> (Integer -> Integer)</code>. If <math>g</math> would be strict, the sequence would read

<center><math> \perp\ \sqsubseteq\ \perp\ \sqsubseteq\ \perp\ \sqsubseteq \dots </math></center>

which obviously converges to a useless <math>\perp</math>. It is crucial that <math>g</math> makes the argument function more defined. This means that <math>g</math> must not be strict in its argument to yield a useful fixed point.

As a side note, the fact that things must be non-strict in function types can be used to recover some non-strict behavior in strict languages. One simply replaces a data type like <code>Integer</code> with <code>() -> Integer</code> where <code>()</code> denotes the well known singleton type. It is clear that every such function has the only possible argument <code>()</code> (besides &perp;) and therefore corresponds to a single integer. But operations may be non-strict in arguments of type <code>() -> Integer</code>.
{{Exercises|1=It's tedious to lift every <code>Integer</code> to a <code>() -> Integer</code> for using non-strict behavior in strict languages. Can you write a function

<center><code>lift :: Integer -> (() -> Integer)</code></center>

that does this for us? Where is the trap?}}

== Algebraic Data Types ==
After treating the motivation case of partial functions between <code>Integer</code>s, we now want to extend the scope of denotational semantics to arbitrary algebraic data types in Haskell.

A word about nomenclature: the collection of semantic objects for a particular type is usually called a '''domain'''. This term is more a generic name than a particular definition and we decide that our domains are cpos (complete partial orders), that is sets of values together with a relation ''more defined'' that obeys some conditions to allow fixed point iteration. Usually, one adds additional conditions to the cpos that ensure that the values of our domains can be represented in some finite way on a computer and thereby avoiding to ponder the twisted ways of uncountable infinite sets. But as we are not going to prove general domain theoretic theorems, the conditions will just happen to hold by construction.

=== Constructors ===
Let's take the example types

 data Bool    = True | False
 data Maybe a = Just a | Nothing

Here, <code>True</code>, <code>False</code> and <code>Nothing</code> are nullary constructors whereas <code>Just</code> is an unary constructor. The inhabitants of <code>Bool</code> form the following domain:

[[Image:Bool-graph.png]]

Remember that &perp; is added as least element to the set of values <code>True</code> and <code>False</code>, we say that the type is '''lifted'''<ref>The term ''lifted'' is somewhat overloaded, see also [[#Unboxed Types|Unboxed Types]].</ref>. A domain whose poset diagram consists of only one level is called a '''flat domain'''. We already know that <math>Integer</math> is a flat domain as well, it's just that the level above &perp; has an infinite number of elements.

What are the possible inhabitants of <code>Maybe Bool</code>? They are

 &perp;, Nothing, Just &perp;, Just True, Just False

So the general rule is to insert all possible values into the unary (binary, ternary, ...) constructors as usual but without forgetting &perp;. Concerning the partial order, we remember the condition that the constructors should be monotone just as any other functions. Hence, the partial order looks as follows

[[Image:Maybe-graph.png]]

But there is something to ponder: why isn't <code>Just &perp; = &perp;</code>? I mean "Just undefined" is as undefined as "undefined"! The answer is that this depends on whether the language is strict or non-strict. In a strict language, all constructors are strict by default, i.e. <code>Just &perp; = &perp;</code> and the diagram would reduce to

[[Image:Maybe-graph-strict.png]]

As a consequence, all domains of a strict language are flat.

But in a non-strict language like Haskell, constructors are non-strict by default and <code>Just &perp;</code> is a new element different from &perp;, because we can write a function that reacts differently to them:

 f (Just _) = 4
 f Nothing  = 7

As <code>f</code> ignores the contents of the <code>Just</code> constructor, <code>f (Just &perp;)</code> is <code>4</code> but <code>f &perp;</code> is <code>&perp;</code> (intuitively, if f is passed &perp;, 
it will not be possible to tell whether to take the Just branch or the 
Nothing branch, and so &perp; will be returned). 

This gives rise to '''non-flat domains''' as depicted in the former graph. What should these be of use for? In the context of [[Haskell/Graph Reduction|Graph Reduction]], we may also think of &perp; as an unevaluated expression. Thus, a value <code>x = Just &perp;</code> may tell us that a computation (say a lookup) succeeded and is not <code>Nothing</code>, but that the true value has not been evaluated yet. If we are only interested in whether <code>x</code> succeeded or not, this actually saves us from the unnecessary work to calculate whether <code>x</code> is <code>Just True</code> or <code>Just False</code> as would be the case in a flat domain. The full impact of non-flat domains will be explored in the chapter [[Haskell/Laziness|Laziness]], but one prominent example are infinite lists treated in section [[#Recursive Data Types and Infinite Lists|Recursive Data Types and Infinite Lists]].

=== Pattern Matching ===
In the section [[#Strict Functions|Strict Functions]], we proved that some functions are strict by inspecting their results on different inputs and insisting on monotonicity. However, in the light of algebraic data types, there can only be one source of strictness in real life Haskell: pattern matching, i.e. <code>case</code> expressions. The general rule is that pattern matching on a constructor of a <code>data</code>-type will force the function to be strict, i.e. matching &perp; against a constructor always gives &perp;. For illustration, consider

 const1 _ = 1

 const1' True  = 1
 const1' False = 1

The first function <code>const1</code> is non-strict whereas the <code>const1'</code> is strict because it decides whether the argument is <code>True</code> or <code>False</code> although its result doesn't depend on that. Pattern matching in function arguments is equivalent to <code>case</code>-expressions

 const1' x = case x of
    True  -> 1
    False -> 1

which similarly impose strictness on <code>x</code>: if the argument to the <code>case</code> expression denotes &perp; the while <code>case</code> will denote &perp;, too. However, the argument for case expressions may be more involved as in

 foo k table = case lookup ("Foo." ++ k) table of
   Nothing -> ...
   Just x  -> ...

and it can be difficult to track what this means for the strictness of <code>foo</code>.

An example for multiple pattern matches in the equational style is the logical <code>or</code>:

 or True _ = True
 or _ True = True
 or _ _    = False

Note that equations are matched from top to bottom. The first equation for <code>or</code> matches the first argument against <code>True</code>, so <code>or</code> is strict in its first argument. The same equation also tells us that <code>or True x</code> is non-strict in <code>x</code>. If the first argument is <code>False</code>, then the second will be matched against <code>True</code> and <code>or False x</code> is strict in <code>x</code>. Note that while wildcards are a general sign of non-strictness, this depends on their position with respect to the pattern matches against constructors.
{{Exercises|1=
#Give an equivalent discussion for the logical <code>and</code>
#Can the logical "excluded or" (<code>xor</code>) be non-strict in one of its arguments if we know the other?}}

There is another form of pattern matching, namely '''irrefutable patterns''' marked with a tilde <code>~</code>. Their use is demonstrated by 

 f ~(Just x) = 1
 f Nothing   = 2

An irrefutable pattern always succeeds (hence the name) resulting in <code>f &perp; = 1</code>. But when changing the definition of <code>f</code> to

 f ~(Just x) = x + 1
 f Nothing   = 2      -- this line may as well be left away

we have

 f &perp;       = &perp; + 1 = &perp;
 f (Just 1) = 1 + 1 = 2

If the argument matches the pattern, <code>x</code> will be bound to the corresponding value. Otherwise, any variable like <code>x</code> will be bound to &perp;.

By default, <code>let</code> and <code>where</code> bindings are non-strict, too:

 foo key map = let Just x = lookup key map in ...

is equivalent to

 foo key map = case (lookup key map) of ~(Just x) -> ...


{{Exercises|1=
<ol><li> The [http://www.haskell.org/onlinereport/ Haskell language definition] gives the detailed [http://www.haskell.org/onlinereport/exps.html#case-semantics semantics of pattern matching] and you should now be able to understand it. So go on and have a look!
</li><li> Consider a function <code>or</code> of two <code>Bool</code>ean arguments with the following properties:

 or &perp;     &perp;    = &perp;
 or True  &perp;    = True
 or &perp;     True = True
 
 or False y     = y
 or x False     = x

This function is another example of joint strictness, but a much sharper one: the result is only &perp; if both arguments are (at least when we restrict the arguments to <code>True</code> and &perp;). Can such a function be implemented in Haskell?
</li></ol>}}

=== Recursive Data Types and Infinite Lists ===
The case of recursive data structures is not very different from the base case. Consider a list of unit values

 data List = [] | () : List

Though this seems like a simple type, there is a surprisingly complicated number of ways you can fit <math>\perp</math> in here and there, and therefore the corresponding graph is complicated. The bottom of this graph is shown below. An ellipsis indicates that the graph continues along this direction. A red ellipse behind an element indicates that this is the end of a chain; the element is in normal form.

[[Image:List-graph.png]]

and so on. But now, there are also chains of infinite length like

<center><code>&perp; <math>\sqsubseteq</math> ():&perp; <math>\sqsubseteq</math> ():():&perp; <math>\sqsubseteq</math> ...</code></center>

This causes us some trouble as we noted in section [[#Convergence|Convergence]] that every monotone sequence must have a least upper bound. This is only possible if we allow for '''infinite lists'''. Infinite lists (sometimes also called ''streams'') turn out to be very useful and their manifold use cases are treated in full detail in chapter [[Haskell/Laziness|Laziness]]. Here, we will show what their denotational semantics should be and how to reason about them. Note that while the following discussion is restricted to lists only, it easily generalizes to arbitrary recursive data structures like trees.

In the following, we will switch back to the standard list type

 data [a] = [] | a : [a]

to close the syntactic gap to practical programming with infinite lists in Haskell.
{{Exercises|1=
# Draw the non-flat domain corresponding <code>[Bool]</code>.
# How is the graphic to be changed for <code>[Integer]</code>?}}

Calculating with infinite lists is best shown by example. For that, we need an infinite list

 ones :: [Integer]
 ones = 1 : ones

When applying the fixed point iteration to this recursive definition, we see that <code>ones</code> ought to be the supremum of

<center><code>&perp; <math>\sqsubseteq</math> 1:&perp; <math>\sqsubseteq</math> 1:1:&perp; <math>\sqsubseteq</math> 1:1:1:&perp; <math>\sqsubseteq</math>...</code>,</center>

that is an infinite list of <code>1</code>. Let's try to understand what <code>take 2 ones</code> should be. With the definition of <code>take</code>

 take 0 _      = []
 take n (x:xs) = x : take (n-1) xs
 take n []     = []

we can apply <code>take</code> to elements of the approximating sequence of <code>ones</code>:

 take 2 &perp;       ==>  &perp;
 take 2 (1:&perp;)   ==>  1 : take 1 &perp;      ==>  1 : &perp;
 take 2 (1:1:&perp;) ==>  1 : take 1 (1:&perp;)  ==>  1 : 1 : take 0 &perp;
                 ==>  1 : 1 : []

We see that <code>take 2 (1:1:1:&perp;)</code> and so on must be the same as <code>take 2 (1:1:&perp;) = 1:1:[] </code> because <code>1:1:[]</code> is fully defined. Taking the supremum on both the sequence of input lists and the resulting sequence of output lists, we can conclude

 take 2 ones = 1:1:[]

Thus, taking the first two elements of <code>ones</code> behaves exactly as expected.

Generalizing from the example, we see that reasoning about infinite lists involves considering the approximating sequence and passing to the supremum, the truly infinite list. Still, we did not give it a firm ground. The solution is to identify the infinite list with the whole chain itself and to formally add it as a new element to our domain: the infinite list ''is'' the sequence of its approximations. Of course, any infinite list like <code>ones</code> can compactly depicted as

 ones = 1 : 1 : 1 : 1 : ...

what simply means that

 ones = (&perp; <math>\sqsubseteq</math> 1:&perp; <math>\sqsubseteq</math> 1:1:&perp; <math>\sqsubseteq</math> ...)

{{Exercises|1=
# Of course, there are more interesting infinite lists than <code>ones</code>. Can you write recursive definition in Haskell for
## the natural numbers <code>nats = 1:2:3:4:...</code>
## a cycle like <code>cycle123 = 1:2:3: 1:2:3 : ...</code>
# Look at the Prelude functions <code>repeat</code> and <code>iterate</code> and try to solve the previous exercise with their help.
# Use the example from the text to find the value the expression <code>drop 3 nats</code> denotes.
# Assume that the work in a strict setting, i.e. that the domain of <code>[Integer]</code> is flat. What does the domain look like? What about infinite lists? What value does <code>ones</code> denote?}}

What about the puzzle of how a computer can calculate with infinite lists? It takes an infinite amount of time, after all? Well, this is true. But the trick is that the computer may well finish in a finite amount of time if it only considers a finite part of the infinite list. So, infinite lists should be thought of as ''potentially'' infinite lists. In general, intermediate results take the form of infinite lists whereas the final value is finite. It is one of the benefits of denotational semantics that one treat the intermediate infinite data structures as truly infinite when reasoning about program correctness.
{{Exercises|1=
<ol><li> To demonstrate the use of infinite lists as intermediate results, show that
<pre>take 3 (map (+1) nats) = take 3 (tail nats)</pre>
by first calculating the infinite sequence corresponding to <code>map (+1) nats</code>.
</li><li> Of course, we should give an example where the final result indeed takes an infinite time. So, what does

 filter (< 5) nats

denote?
</li><li> Sometimes, one can replace <code>filter</code> with <code>takeWhile</code> in the previous exercise. Why only sometimes and what happens if one does?
</li></ol>}}

As a last note, the construction of a recursive domain can be done by a fixed point iteration similar to recursive definition for functions. Yet, the problem of infinite chains has to be tackled explicitly. See the literature in [[#External Links|External Links]] for a formal construction.

=== Haskell specialities: Strictness Annotations and Newtypes ===
Haskell offers a way to change the default non-strict behavior of data type constructors by ''strictness annotations''. In a data declaration like

 data Maybe' a = Just' !a | Nothing'

an exclamation point <code>!</code> before an argument of the constructor specifies that he should be strict in this argument. Hence we have <code>Just' &perp; = &perp;</code> in our example. Further information may be found in chapter [[Haskell/Strictness|Strictness]].

In some cases, one wants to rename a data type, like in

 data Couldbe a = Couldbe (Maybe a)

However, <code>Couldbe a</code> contains both the elements <code>&perp;</code> and <code>Couldbe &perp;</code>. With the help of a <code>newtype</code> definition

 newtype Couldbe a = Couldbe (Maybe a)

we can arrange that <code>Couldbe a</code> is semantically equal to <code>Maybe a</code>, but different during type checking. In particular, the constructor <code>Couldbe</code> is strict. Yet, this definition is subtly different from

 data Couldbe' a = Couldbe' !(Maybe a)

To explain how, consider the functions

 f  (Couldbe  m) = 42
 f' (Couldbe' m) = 42

Here, <code>f' &perp;</code> will cause the pattern match on the constructor <code>Couldbe'</code> fail with the effect that <code>f' &perp; = &perp;</code>. But for the newtype, the match on <code>Couldbe</code> will never fail, we get <code>f &perp; = 42</code>. In a sense, the difference can be stated as:
* for the strict case, <code>Couldbe' &perp;</code> is a synonym for &perp;
* for the newtype, &perp; is a synonym for <code>Couldbe &perp;</code>
with the agreement that a pattern match on &perp; fails and that a match on <code>''Constructor'' &perp;</code> does not.

Newtypes may also be used to define recursive types. An example is the alternate definition of the list type <code>[a]</code>

  newtype List a = In (Maybe (a, List a))

Again, the point is that the constructor <code>In</code> does not introduce an additional lifting with &perp;.

== Other Selected Topics ==
=== Abstract Interpretation and Strictness Analysis ===
As lazy evaluation means a constant computational overhead, a Haskell compiler may want to discover where inherent non-strictness is not needed at all which allows it to drop the overhead at these particular places. To that extent, the compiler performs '''strictness analysis''' just like we proved in some functions to be strict section [[#Strict Functions|Strict Functions]]. Of course, details of strictness depending on the exact values of arguments like in our example <code>cond</code> are out of scope (this is in general undecidable). But the compiler may try to find approximate strictness information and this works in many common cases like <code>power2</code>.

Now, '''abstract interpretation''' is a formidable idea to reason about strictness: ...

{{TODO|Complete section}}

For more about strictness analysis, see the [http://haskell.org/haskellwiki/Research_papers/Compilation#Strictness research papers about strictness analysis on the Haskell wiki].

=== Interpretation as Powersets ===
So far, we have introduced &perp; and the semantic approximation order <math>\sqsubseteq</math> abstractly by specifying their properties. However, both as well as any inhabitants of a data type like <code>Just &perp;</code> can be interpreted as ordinary sets. This is called the '''powerset construction'''. NOTE: ''i'm not sure whether this is really true. Someone how knows, please correct this.''

The idea is to think of &perp; as the ''set of all possible values'' and that a computation retrieves more information this by choosing a subset. In a sense, the denotation of a value starts its life as the set of all values which will be reduced by computations until there remains a set with a single element only.

As an example, consider <code>Bool</code> where the domain looks like

 {True}  {False}
    \      /
     \    /
    &perp; = {True, False}

The values <code>True</code> and <code>False</code> are encoded as the singleton sets <code>{True}</code> and <code>{False}</code> and &perp; is the set of all possible values.

Another example is <code>Maybe Bool</code>:

  {Just True}   {Just False}
          \     /
           \   /
 {Nothing} {Just True, Just False}
      \      /
       \    /
  &perp; = {Nothing, Just True, Just False}

We see that the semantic approximation order is equivalent to set inclusion, but with arguments switched:

<center><math>x\sqsubseteq y \iff x \supseteq y</math></center>

This approach can be used to give a semantics to exceptions in Haskell<ref>S. Peyton Jones, A. Reid, T. Hoare, S. Marlow, and F. Henderson. [http://research.microsoft.com/~simonpj/Papers/imprecise-exn.htm A semantics for imprecise exceptions.] In Programming Languages Design and Implementation. ACM press, May 1999.</ref>.

=== Naïve Sets are unsuited for Recursive Data Types  ===
In section [[#Naïve Sets are unsuited for Recursive Definitions|Naïve Sets are unsuited for Recursive Definitions]], we argued that taking simple sets as denotation for types doesn't work well with partial functions. In the light of recursive data types, things become even worse as John C. Reynolds showed in his paper ''Polymorphism is not set-theoretic''<ref>John C. Reynolds. ''Polymorphism is not set-theoretic''. INRIA Rapports de Recherche No. 296. May 1984.</ref>.

Reynolds actually considers the recursive type

 newtype U = In ((U -> Bool) -> Bool)

Interpreting <code>Bool</code> as the set <code>{True,False}</code> and the function type <code>A -> B</code> as the set of functions from <code>A</code> to <code>B</code>, the type <code>U</code> cannot denote a set. This is because <code>(A -> Bool)</code> is the set of subsets (powerset) of <code>A</code> which, due to a diagonal argument analogous to Cantor's argument that there are "more" real numbers than natural ones, always has a bigger cardinality than <code>A</code>. Thus, <code>(U -> Bool) -> Bool</code> has an even bigger cardinality than <code>U</code> and there is no way for it to be isomorphic to <code>U</code>. Hence, the set <code>U</code> must not exist, a contradiction.

In our world of partial functions, this argument fails. Here, an element of <code>U</code> is given by a sequence of approximations taken from the sequence of domains

<center><code>&perp;, (&perp; -> Bool) -> Bool,  (((&perp; -> Bool) -> Bool) -> Bool) -> Bool</code> and so on</center>

where &perp; denotes the domain with the single inhabitant &perp;. While the author of this text admittedly has no clue on what such a thing should mean, the constructor gives a perfectly well defined object for <code>U</code>. We see that the type <code>(U -> Bool) -> Bool</code> merely consists of shifted approximating sequences which means that it is isomorphic to <code>U</code>.

As a last note, Reynolds actually constructs an equivalent of <code>U</code> in the second order polymorphic lambda calcus. There, it happens that all terms have a normal form, i.e. there are only total functions when we do not include a primitive recursion operator <code>fix :: (a -> a) -> a</code>. Thus, there is no true need for partial functions and &perp;, yet a naïve set theoretic semantics fails. We can only speculate that this has to do with the fact that not every mathematical function is computable. In particular, the set of computable functions <code>A -> Bool</code> should not have a bigger cardinality than <code>A</code>.


== Footnotes ==
<references/>

== External Links ==
{{Wikipedia|Denotational semantics}}

Online books about Denotational Semantics
* {{cite book
 |title=Denotational Semantics. A Methodology for Language Development
 |last=Schmidt|first=David A.
 |publisher=Allyn and Bacon
 |year=1986
 |url=http://www.cis.ksu.edu/~schmidt/text/densem.html
 }}

{{Haskell navigation|chapter=Wider Theory}}

[[Category:Haskell|{{SUBPAGENAME}}]]
