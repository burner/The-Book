>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Wider Theory}}

The '''Curry-Howard isomorphism''' is a striking relationship connecting two seemingly unrelated areas of mathematics &mdash; type theory and structural logic.

== Introduction ==
The Curry-Howard isomorphism, hereafter referred to as simply C-H, tells us that in order to prove any mathematical theorem, all we have to do is construct a certain type which reflects the nature of that theorem, then find a value that has that type. This seems extremely weird at first: what do types have to do with theorems? However, as we shall see, the two are very closely related. A quick note before we begin: for these introductory paragraphs, we ignore the existence of expressions like <code>error</code> and <code>undefined</code> whose [[Haskell/Denotational semantics|denotational semantics]] are ⊥. These have an extremely important role, but we will consider them separately in due time. We also ignore functions that bypass the type system like <code>unsafeCoerce#</code>.

We can build incredibly complicated types using Haskell's [[Haskell/Higher-order functions and Currying|higher-order functions]] feature. We might want to ask the question: given an arbitrary type, under what conditions does there exist a value with that type (we say the type is ''inhabited'')? A first guess might be 'all the time', but this quickly breaks down under examples. For example, there is no function with type <code>a -> b</code>, because we have no way of turning something of type <code>a</code> into something of a completely different type <code>b</code> (unless we know in advance which types <code>a</code> and <code>b</code> are, in which case we're talking about a monomorphic function, such as <code>ord :: Char -> Int</code>).

Incredibly, it turns out that a type is only inhabited when it corresponds to a true theorem in mathematical logic. But what is the nature of this correspondence? What does a type like <code>a -> b</code> mean in the context of logic?

=== A crash course in formal logic ===
We need some background on formal logic before we can begin to explore its relationship to type theory. This is a ''very'' brief introduction; for a wider grounding we recommend you consult an introductory textbook on the subject matter.

In everyday language we use a lot of 'If... then...' sentences. For example, 'If the weather is nice today, then we'll walk into town'. These kinds of statements also crop up in mathematics; we can say things like 'If ''x'' is positive, then it has a (real) square root'. Formal logic is a way of translating these statements from loose, woolly, ambiguous English into precise symbolism. We use the → sign (read as 'implies') to indicate that something is true if something else is true. For example, our earlier statement could be recast as 'The weather is nice today → we'll walk into town', which means that it is true that we'll walk into town if it is true that the weather is nice, which is just the same as the 'If... then...' version we used earlier. We'll often use letters to stand for entire statements, so for example if ''W'' is the statement 'the weather is nice', and ''T'' is the statement 'we'll walk into town', then our example becomes simply ''W'' → ''T''. 

Notice the crafty way we phrased our definition of →. ''P'' → ''Q'' means that ''if'' ''P'' is true, ''then'' ''Q'' is true. But if ''Q'' is some statement that is always true, no matter what the circumstances &mdash; like 'the sun is hot' &mdash; then it doesn't matter what ''P'' is. ''P'' could even be a false statement, ''Q'' would still be true if ''P'' were true, so ''P'' → ''Q'' would still hold. The fact that ''Q'' would be true if ''P'' isn't true is a whole different matter; we're not asking that question when we say ''P'' → ''Q''. So → doesn't really represent any kind of cause-effect relationship; things like 'the sky is pink → the sun is hot' are still valid statements. <ref>Another way of looking at this is that we're trying to ''define'' our logical operator → such that it captures our intuition of the "if... then" construct in natural language. So we want statements like for all all naturals <math>x</math>, "<math>x</math> is even" → "<math>x+1</math> is odd" to be true. I.e. that implication must hold when we substitute <math>x</math> for any natural including, say, 5. But "5 is even" and "6 is odd" are both false, so we must have that False → False is true. Similarly by considering the statement for all naturals <math>x>3</math>, "<math>x</math> is prime" → "<math>x+1</math> is not prime", we must have that False → True is true. And obviously True → True must be true, and True → False is false. So we have that <math>x</math> → <math>y</math> unless <math>x</math> is true and <math>y</math> false.</ref>

Other things that crop up lots in both everyday language and mathematics are things called conjunctions and disjunctions. The former represent statements involving an 'and', the latter statements involving an 'or'. We could represent the statement 'I will buy this magazine if it's in stock and I have enough money' by the symbolism <math>(M \wedge S) \to B</math>, where ''M'' = 'I have enough money', ''S'' = 'The magazine is in stock', ''B'' = 'I will buy the magazine'. Essentially, one can just read the symbol <math>\wedge</math> as 'and'. Similarly, one can read the symbol <math>\vee</math> as 'or', so that the statement 'I will either walk or get the train to work' could be represented as <math>W \vee T</math>, where ''W'' = 'I will walk to work', and ''T'' = 'I will get the train to work'.

Using these symbols, and a few more which will be introduced as we go, we can produce arbitrarily complicated symbol strings. There are two classes of these symbol strings: those that represent true statements, often called the '''theorems'''; and those which represent false statements, called the '''nontheorems'''. Note that whether a symbol string is a theorem or nontheorem depends on what the letters stand for, so <math>P \vee Q</math> is a theorem if, for example, ''P'' represents the statement 'It is daytime' and ''Q'' represents the statement 'It is night time' (ignoring exceptions like twilight), but it would be a nontheorem if ''P'' were 'Trees are blue' and ''Q'' were 'All birds can fly'. We'll often call a symbol string a '''proposition''' if we don't know whether it's a theorem or not.

There are ''many'' more subtleties to the subject of logic (including the fact that when we say 'If you eat your dinner you'll get dessert' we actually mean 'If and only if you eat your dinner will you get dessert'). If this is a subject that interests you, there are many textbooks around that comprehensively cover the subject.

=== Propositions are types ===
So, given a type <code>a -> b</code>, what does that mean in terms of symbolistic logic? Handily, it simply means that ''a'' → ''b''. Of course, this only makes sense if <code>a</code> and <code>b</code> are types which can further be interpreted in our symbolistic logic. This is the essence of C-H. Furthermore, as we mentioned before, ''a'' → ''b'' is a theorem if and only if <code>a -> b</code> is an inhabited type.

Let's see this using one of the simplest of Haskell functions. <code>const</code> has the type <code>a -> b -> a</code>. Translated into logic, we have that ''a'' → ''b'' → ''a''. This must be a theorem, as the type <code>a -> b -> a</code> is inhabited by the value <code>const</code>. Now, another way of expressing ''a'' → ''b'' is that 'If we assume ''a'' is true, then ''b'' must be true.' So ''a'' → ''b'' → ''a'' means that if we assume ''a'' is true, then if we further assume that ''b'' is true, then we can conclude ''a''. This is of course a theorem; we assumed ''a'', so ''a'' is true under our assumptions.

=== The problem with ⊥ ===
We've mentioned that a type corresponds to a theorem if that type is inhabited. However, in Haskell, every type is inhabited by the value <code>undefined</code>. Indeed, more generally, anything with type <code>forall a. a</code>, a value with denotational semantics of ⊥, is a problem. ⊥ in type theory corresponds to inconsistency in logic; we can prove any theorem using Haskell types because every type is inhabited. Therefore, Haskell's type system actually corresponds to an inconsistent logic system. However, if we work with a limited subset of Haskell's type system, and in particular disallow polymorphic types, we have a consistent logic system we can do some cool stuff in. Hereafter it is assumed we are working in such a type system.

Now that we have the basics of C-H, we can begin to unpack a little more the relationship between types and propositions.

== Logical operations and their equivalents ==
The essence of symbolic logic is a set of propositions, such as ''P'' and ''Q'', and different ways of combining these propositions such as ''Q'' → ''P'' or <math>P \vee Q</math>. These ways of combining propositions can be thought of as operations on propositions. By C-H, propositions correspond to types, so we should have that the C-H equivalents of these proposition combinators are type operations, more normally known as type constructors. We've already seen an example of this: the implication operator → in logic corresponds to the type constructor <code>(->)</code>. The rest of this section proceeds to explore the rest of the proposition combinators and explain their correspondence.

=== Conjunction and Disjunction===
In order for <math>A \wedge B</math> to be a theorem, both ''A'' and ''B'' must be theorems. So a proof for <math>A \wedge B</math> amounts to proving both ''A'' and ''B''. Remember that to prove a proposition ''A'' we find a value of type <code>A</code>, where ''A'' and <code>A</code> are C-H correspondents. So in this instance we wish to find a value that contains two sub-values: the first whose type corresponds to ''A'', and the second whose type corresponds to ''B''. This sounds remarkably like a pair. Indeed, we represent the symbol string <math>A \wedge B</math> by <code>(a, b)</code>, where <code>a</code> corresponds to ''A'' and <code>b</code> corresponds to ''B''.

Disjunction is opposite to conjunction. In order for <math>A \vee B</math> to be a theorem, either ''A'' or ''B'' must be a theorem. Again, we search for a value which contains either a value of type <code>A</code> or a value of type <code>B</code>. This is <code>Either</code>. <code>Either A B</code> is the type which corresponds to the proposition <math>A \vee B</math>.

=== Falsity ===
It is occasionally useful to represent a false statement in our logic system. By definition, a false statement is one that can't be proven. So we're looking for a type which isn't inhabited. Although none of these types exist in the default libraries (don't get confused with the <code>()</code> type, which has precisely one value), we can define one, if we turn on the <code>-fglasgow-exts</code> flag in GHC:

 data Void

The effect of omitting the constructors means that <code>Void</code> is an uninhabited type. So the <code>Void</code> type corresponds to a nontheorem in our logic. There are a few handy corollaries here:

<ol>
<li>
<code>(Void, A)</code> and <code>(A, Void)</code> are both uninhabited types for any type <code>A</code>, corresponding to the fact that <math>F \wedge A</math> and <math>A \wedge F</math> are both nontheorems if ''F'' is a nontheorem.
</li>
<li>
<code>Either Void A</code> and <code>Either A Void</code> are essentially the same as <code>A</code> for any type <code>A</code>, <ref>Technically, the types <code>Either Void A</code> and <code>A</code> are isomorphic. Seeing as you can't have a value of type <code>Void</code>, every value in <code>Either Void A</code> must be a <code>Right</code>-tagged value, so the transformation just strips the <code>Right</code> constructors.</ref> corresponding to the fact that <math>F \vee A</math> and <math>A \vee F</math>, where ''F'' is a nontheorem, are theorems only if ''A'' is a theorem.
</li>
<li>
Any type that corresponds to a nontheorem can be replaced with <code>Void</code>. This is because any nontheorem-type must be uninhabited, so replacing it with <code>Void</code> everywhere doesn't change anything. <code>Void</code> is really equivalent to any nontheorem type<ref>Again, the technical statement is that <code>Void</code> is isomorphic to any type which is a nontheorem.</ref>.
</li>
<li>
<p>As we remarked in the first section, the implication ''P'' → ''Q'' is true if ''Q'' is true, regardless of the truth value of ''P''. So we should be able to find a term with type <code>Void -> a</code>. In fact one does exist, but it's somewhat complicated to explain: the answer is the ''empty function''. We can define a function <code>f :: A -> B</code> as a (probably infinite) set of pairs whose first element is an element of <code>A</code> (the ''domain'') and second element is <code>f</code>'s output on this term, an element of <code>B</code> (the ''range''). For example, the successor function on the naturals is represented as <code>{(0,1), (1,2), (2,3), ...}</code>. Note that in order to be a (total and well-defined) function, we must have precisely one pair <code>(a, f a)</code> for each term <code>a</code> with type <code>A</code>.</p>

<p>The empty function, let's call it <code>empty</code> is represented in this way by the empty set. But as we must have a pair for each element of the domain, and there no pairs in our representation, the domain type must be empty, i.e. <code>Void</code>. What about the range type? <code>empty</code> never produces any output, so there are no restrictions placed on the range type. Thus, it is valid to assume that the range type has any type, so we can say <code>empty :: forall a. Void -> a</code>. Unfortunately, it's not possible to write this function in Haskell; we'd ideally like to write something like:</p>

 empty :: Void -> a

<p>And stop there, but this is illegal Haskell. The closest we can come is the following:</p>

 empty :: Void -> a
 empty _ = undefined

<p>Another reasonable way (also disallowed in Haskell) would be to write:</p>

 empty x = case x of { }

<p>The case statement is perfectly well formed since it handles every possible value of <code>x</code>.</p>

<p>Note that this is perfectly safe, since the right-hand side of this function can never be reached (since we have nothing to pass it). So, the conclusion of all this is that <code>Void -> a</code> is an inhabited type, just as ''P'' → ''Q'' is true if ''P'' is false.</p>
</li>
</ol>

=== Negation ===
The ¬ operation in logic turns theorems into nontheorems and vice versa: if ''A'' is a theorem then ''¬A'' is a nontheorem; if ''A'' is a nontheorem then ''¬A'' is a theorem. How can we represent this in Haskell? The answer's a sneaky one. We define a type synonym:

 type Not a = a -> Void

So for a type <code>A</code>, <code>Not A</code> is just <code>A -> Void</code>. How does this work? Well, if <code>A</code> was a theorem-type, then <code>A -> Void</code> must be uninhabited: there's no way any function could return any value, because the return type, <code>Void</code> has no values (The function has to provide values for all inhabitants of A)! On the other hand, if <code>A</code> was a nontheorem, then <code>A</code> can be replaced with <code>Void</code> as we explored in the last section. Then the function <code>id :: Void -> Void</code> is an inhabitant of <code>Not A</code>, so <code>Not A</code> is a theorem as required (The function doesn't have to provide any values, since there are no inhabitants in its domain. Nevertheless it's a function — with an empty graph).

== Axiomatic logic and the combinatory calculus ==
So far we've only used some very basic features from Haskell's type system. Indeed, most of the features of logic we've mentioned can be explored using a very basic 'programming language', the combinator calculus. To fully appreciate how closely C-H ties together these two areas of mathematics, we need to ''axiomatise'' both our discussion of formal logic and our discussion of programming languages.

=== Axiomatic logic ===
We start with two axioms about how the → operation should behave (from now on, we assume that → is a right-associative function, i.e. ''A'' → ''B'' → ''C'' means ''A'' → (''B'' → ''C'')):

# ''A'' → ''B'' → ''A''
# (''A'' → ''B'' → ''C'') → (''A'' → ''B'') → ''A'' → ''C''

The first axiom says that given any two propositions ''A'' and ''B'', if we assume both ''A'' and ''B'', we know that ''A'' is true. The second says that if ''A'' implies that ''B'' implies ''C'' (or equivalently, if ''C'' is true whenever ''A'' and ''B'' are true), and ''A'' itself implies ''B'', then knowing ''A'' is true would be enough to conclude that ''C'' is true. This may seem complicated, but a bit of thought reveals it to be common sense. Imagine we have a collection of boxes of various colours, some with wheels, some with lids, such that all the red boxes with wheels also have lids, and all the red boxes have wheels. Pick one box. Let ''A'' = 'The box under consideration is red', ''B'' = 'The box under consideration has wheels', ''C'' = 'The box under consideration has a lid'. Then the second law tells us that, as ''A'' → ''B'' → ''C'' (all red boxes with wheels also have lids), and ''A'' → ''B'' (all red boxes have wheels), then if ''A'' (if the box is red), then ''C'' must be true (the box has a lid). 

We also allow one ''inference law'', called ''modus ponens'':

# If ''A'' → ''B'', and ''A'', then ''B''.

This law allows us to create new theorems given old one. It should be fairly obvious; it is essentially the definition of what → means. This small basis provides a simple enough logic system which is expressive enough to cover most of our discussions. Here's a sample proof of the law ''A'' → ''A'' in our system:

Firstly, we know the two axioms to be theorems:
* ''A'' → ''B'' → ''A''
* (''A'' → ''B'' → ''C'') → (''A'' → ''B'') → ''A'' → ''C''
You'll notice that the left-hand side of the second axiom looks a bit like the first axiom. The second axiom guarantees that if we know that ''A'' → ''B'' → ''C'', then we can conclude (''A'' → ''B'') → ''A'' → ''C''. In this case, if we let ''C'' be the same proposition as ''A'', then we have that if ''A'' → ''B'' → ''A'', then (''A'' → ''B'') → ''A'' → ''A''. But we already know ''A'' → ''B'' → ''A'', that was the first axiom. Therefore, we have that (''A'' → ''B'') → ''A'' → ''A'' is a theorem. If we further let ''B'' be the proposition ''C'' → ''A'', for some other proposition ''C'', then we have that if ''A'' → ''C'' → ''A'', then ''A'' → ''A''. But, again, we know that ''A'' → ''C'' → ''A'' (it's the first axiom again), so ''A'' → ''A'', as we wanted.

This example demonstrates that given some simple axioms and a simple way to make new theorems from old, we can derive more complicated theorems. It may take a while to get there &mdash; here we had several lines of reasoning to prove just that the obvious statement ''A'' → ''A'' is a theorem! &mdash; but we get there in the end. This kind of formalisation is attractive because we have essentially defined a very simple system, and it is very easy to study how that system works.

=== Combinator calculus ===
The [[Haskell/Lambda calculus|lambda calculus]] is a way of defining a simple programming language from a very simple basis. If you haven't already read the chapter that was just linked to, we recommend you read at least the introductory sections on the untyped version of the calculus. Here's a refresher in case you're feeling dusty. A lambda term is one of three things:

* A ''value'', ''v''.
* A ''lambda abstraction'' <math>\lambda x. t</math>, where ''t'' is another lambda term.
* An ''application'' <math>(t_1 t_2)</math>, where <math>t_1</math> and <math>t_2</math> are lambda terms.

There is one reduction law, too, called ''beta-reduction'':

* <math>((\lambda x. t_1) t_2)</math> → <math>t_1[x := t_2]</math>, where <math>t_1[x := t_2]</math> means <math>t_1</math> with all the free occurrences of ''x'' replaced with <math>t_2</math>.

As mentioned in the [[Haskell/Lambda calculus|lambda calculus]] article, the difficulty comes when trying to pin down the notion of a free occurrence of an identifier. The combinator calculus was invented by the American mathematician Haskell Curry (after whom a certain programming language is named) because of these difficulties. There are many variants on the basic combinator calculus, but we consider one of the simplest here. We start with two so-called '''combinators''':

* '''K''' takes two values and returns the first. In the lambda calculus, <math>\mathbf{K} = \lambda xy.\ x</math>.
* '''S''' takes a binary function, a unary function and a value, and applies that value and the value passed into the unary function to the binary function. again, in the lambda calculus: <math>\mathbf{S} = \lambda xyz.\ xz(yz)</math>.

The first function you should recognise as <code>const</code>. The second is more complicated, it is the monadic function <code>ap</code> in the <code>((->) e)</code> monad (which is essentially Reader). These two combinators form a complete basis for the entire lambda calculus. Every lambda calculus program can be written using just these two functions.

== Sample proofs ==

== Intuitionistic vs classical logic ==

So far, all of the results we have proved are theorems of intuitionistic logic. Let's see what happens when we try to prove the basic theorem of classical logic, <code> Not Not A -> A </code>. Recall that this translates as <code> ((A -> Void) -> Void) -> A </code>. So, given a function of type <code> (A -> Void) -> Void </code> we need a function of type A. Now a function of type <code> (A -> Void) -> Void </code> exists precisely if type <code> A -> Void </code> is uninhabited, or in other words if type A is inhabited. So we need a function which takes any inhabited type, and returns an element of that type. Although it is simple enough to do this on a computer - we need only find the "simplest" or "first" inhabitant of each type - there is no way to do this using standard lambda-calculus or combinator techniques. So we see that this result cannot be proved using these two techniques, and hence that the underlying logic is intuitionistic rather than classical.

Instead, consider a traditional error handling function which calls <code>throw</code> when an error occurs, transferring computation to <code>catch</code>. The <code>throw</code> function cancels any return value from the original function, so it has type <code>A -> Void</code>, where <code>A</code> is the type of its arguments. The <code>catch</code> function then takes the <code>throw</code> function as its argument, and, if the <code>throw</code> triggers (i.e. returns a <code>Void</code>) will return the argument of the <code>throw</code> function. So the type of <code>catch</code> is <code> ((A -> Void) -> Void) -> A </code>. <ref>This argument is taken from {{cite journal | author = Dan Piponi | title = Adventures in Classical Land | journal = The Monad Reader | issue = 6}}</ref>

== Notes ==
<references />

{{Haskell navigation|chapter=Wider Theory}}

[[Category:Haskell|{{SUBPAGENAME}}]]
