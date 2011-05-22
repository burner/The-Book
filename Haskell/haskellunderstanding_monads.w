>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Monads}}

== First things first: Why, why, why, do I need Monads? ==

Monads, classically, are used for a very important thing: when the program you are building is logically ordered such that you will think, "first, do this, then, do that, then, do that other thing".

For example, in parsing, you might want to say, "first, check that it's a IF keyword.  if so, then look for an expression, then look for a THEN keyword, then look for a statement, then look for an ELSE keyword, then look for a statement".

For another example, you might want to say, "first, write 'Hello rodney, welcome back to HaskHack!', then write a newline, then write our prompt '> ', then get a keyboard press".

For a third example, you might want to say, "first, try to find some entity, then try to find some other entity, and finally, combine both of those entities (and if we succeeded, return Just 'that entity', but if any step failed, return Nothing)".

All three examples are very different: the first involves reading from a stream of input tokens, the next involves interacting with the user, and the third involves attempting (and possibly failing) to perform some operation.  However there is a single common factor: each of them involves '''doing something in sequence'''.  It's that sequencing that is the heart of the term "monad".

Now, one very important thing about monads is that while ''most of the time'' the order in which you write "do this then do that" means that "this" really does occur before "that" in execution time, it is not ''necessarily'' so.  Some monads may be able to arrange to have "that" occur, in execution time, before "this", or may not specify it at all; what matters more is that conceptually, '''you''', the programmer, want to think of it in that very specific order (in some cases the difference between order-of-specification from order-of-execution may be advantageous, <insert example here>).

Monads are extremely general.  They only need to provide three things: how to say "do this then do that", how to say "do this" (and optionally, to say "do this but if something bad happens, FAIL!"), and how to say "I have 'do this then do that', now run it".

== Introduction ==

Monads are a very useful concept in Haskell, but also a relatively difficult one for newcomers.
Since they have so many applications, people have often explained them from a particular point of view, which can make it confusing to understand monads in their full generality.

Historically, monads were first introduced into Haskell as a way to perform input/output. After all, lazy evaluation means that the order of evaluation is rather unpredictable, whereas a determined execution order is crucial for things like reading and writing files. Hence, a method for specifying a determined ''sequence of operations'' was needed and monads are exactly the right abstraction for that.

But monads are by no means limited to input/output; they can model any imperative language. The choice of monad determines the semantics of this language, i.e., whether it supports exceptions, state, non-determinism, continuations, coroutines and so on. Indeed, in Haskell all of these constructs are not a built-in part of the language, but rather defined by standard libraries! Because of this, monadic values are sometimes also referred to as "actions" or "computations". In general, an action can produce a result of a certain type, called the result type of the action; however, it is up to the monad to establish if this has to always happen. For instance, throwing an exception implies returning no value, while a non-deterministic computation produces not just one value, but a list of them.

The present chapter introduces the basic notions with the example of the <code>Maybe</code> monad, the simplest monad for handling exceptions. Beginning Haskell programmers will probably also want to understand the <code>IO</code> monad and then broaden their scope to the many other monads and the effects they represent; this chapter provides the corresponding hyperlinks.

== Definition ==
A ''monad'' is defined by three things:
* a way to produce types of "actions" from the types of their result; formally, a [[Haskell/More on datatypes#Parameterised Types | type constructor]] <code>M</code>, 
* a way to produce actions which simply produce a value; formally a function named <code>return</code>:
<source lang="haskell">
    return :: a -> M a
</source>
From the type, we can read that return produces an action with "result type" <code>a</code>, and as we will see later, the action will have to return exactly the parameter of <code>return</code>, without doing anything else (not even to the control flow).<ref>In imperative languages like C or Java, the <code>return</code> keyword, unrelated to the Haskell <code>return</code> function, causes the containing function to return to its caller. This does not happen in Haskell - <code>return</code> has no effect on the control flow, so don't confound these two.</ref>
* and a way to chain "actions" together, while allowing the result of an action to be used for the second action; formally, an operator <code>(>>=)</code>, which is pronounced "bind":
<source lang="haskell">
    (>>=)  :: M a -> ( a -> M b ) -> M b
</source>
From the type, we can read that this operator takes an action producing a value of type <code>a</code> and a function consuming a value of type <code>a</code> and producing an action with return type <code>b</code>.
Its result is a simple action with return type <code>b</code> - we can already guess that this resulting action will probably feed the result of the first action into the second function.

They are also required to obey [[#Monad Laws|three laws]] that will be explained later on - for now, it suffices to say that our explanation of the meaning of <code>return</code> and <code>(>>=)</code> would not be valid without these laws.

Let's give an example: the <code>Maybe</code> monad, which allows implementing a very simple form of exceptions (more powerful exceptions are also supported). The type constructor is <code>M = Maybe</code> so that <code>return</code> and <code>(>>=)</code> have types

<source lang="haskell">
    return :: a -> Maybe a
    (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b
</source>

They are implemented as

<source lang="haskell">
    return x  = Just x
    (>>=) m g = case m of
                   Nothing -> Nothing
                   Just x  -> g x
</source>

and our task is to explain how and why this definition is useful.

=== Motivation: Maybe ===
To see the usefulness of <code>(>>=)</code> and the <code>Maybe</code> monad, consider the following example: imagine a family database that provides two functions

<source lang="haskell">
    father :: Person -> Maybe Person
    mother :: Person -> Maybe Person
</source>

that look up the name of someone's father/mother or return <code>Nothing</code> if they are not stored in the database. With these, we can query various grandparents. For instance, the following function looks up the maternal grandfather:

<source lang="haskell">
    maternalGrandfather :: Person -> Maybe Person
    maternalGrandfather p =
        case mother p of
            Nothing -> Nothing
            Just m  -> father m                         -- mother's father
</source>

Or consider a function that checks whether both grandfathers are in the database:

<source lang="haskell">
    bothGrandfathers :: Person -> Maybe (Person, Person)
    bothGrandfathers p =
        case father p of
            Nothing -> Nothing
            Just f  ->
                case father f of
                    Nothing -> Nothing
                    Just gf ->                          -- found first grandfather
                        case mother p of
                            Nothing -> Nothing
                            Just m  ->
                                case father m of
                                    Nothing -> Nothing
                                    Just gm ->          -- found second one
                                        Just (gf, gm) 
</source>

What a mouthful! Every single query might fail by returning <code>Nothing</code> and the whole functions must fail with <code>Nothing</code> if that happens.

But clearly, there has to be a better way than repeating the case of <code>Nothing</code> again and again! Indeed, and that's what the <code>Maybe</code> monad is set out to do. For instance, the function retrieving the maternal grandfather has exactly the same structure as the <code>(>>=)</code> operator, and we can rewrite it as

<source lang="haskell">
    maternalGrandfather p = mother p >>= father
</source>

With the help of lambda expressions and <code>return</code>, we can rewrite the mouthful of two grandfathers as well:

<source lang="haskell">
    bothGrandfathers p =
       father p >>=
           (\f -> father f >>=
               (\gf -> mother p >>=
                   (\m -> father m >>=
                       (\gm -> return (gf,gm) ))))
</source>

While these nested lambda expressions may look confusing to you, the thing to take away here is that <code>(>>=)</code> eliminates any mention of <code>Nothing</code>, shifting the focus back to the interesting part of the code. The next chapter will also explain a way to express the same algorithm in a nicer-looking notation, where this sequence of actions looks a bit like a sequence of statements of a more conventional language.

=== Type class ===
In Haskell, the type class <code>Monad</code> is used to implement monads. It is defined in the {{Haskell lib|Control|Monad}} module and part of the {{Haskell lib|Prelude}}:

<source lang="haskell">
    class Monad m where
        return :: a -> m a
        (>>=)  :: m a -> (a -> m b) -> m b
     
        (>>)   :: m a -> m b -> m b
        fail   :: String -> m a
</source>

Aside from return and bind, it defines two additional functions <code>(>>)</code> and <code>fail</code>.

The operator <code>(>>)</code> called "then" is a mere convenience and commonly implemented as
<source lang="haskell">
    m >> n = m >>= \_ -> n
</source>
It is used for sequencing two monadic actions when the second does not care about the result of the first, which is common for monads like <code>IO</code>. 

<source lang="haskell">
    printSomethingTwice :: String -> IO ()
    printSomethingTwice str = putStrLn str >> putStrLn str
</source>

The function <code>fail</code> handles pattern match failures in [[Haskell/do Notation|<code>do</code> notation]]. It's an unfortunate technical necessity and doesn't really have to do anything with monads. You are advised to not use <code>fail</code> directly in your code.

=== Is my Monad a Functor? ===
In case you forgot, a functor is a type to which we can apply the <code>fmap</code> function, which is analogous to the <code>map</code> function for lists.

According to [[category theory]], all monads are by definition functors too. However, GHC thinks it different, and the <code>Monad</code> class has actually nothing to do with the <code>Functor</code> class.
This will likely change in future versions of Haskell, so that every Monad will have its own <code>fmap</code>; until then, you will have to make two separate instances of your monads (as <code>Monad</code> and as <code>Functor</code>) if you want to use a monad as a functor.

== Notions of Computation ==
While you probably agree now that <code>(>>=)</code> and <code>return</code> are very handy for removing boilerplate code that crops up when using <code>Maybe</code>, there is still the question of why this works and what this is all about.

To answer this, we shall write the example with the two grandpas in a very suggestive style:

<source lang="haskell">
    bothGrandfathers p = do {
           f  <- father p;     -- assign the result of  father p  to the variable  f
           gf <- father f;     -- similar
           m  <- mother p;     -- ...
           gm <- father m;     -- ...
           return (gf,gm);     -- return result pair
       }
</source>

If this looks like a code snippet of an imperative programming language to you, that's because it is. In particular, this imperative language supports ''exceptions'' : <code>father</code> and <code>mother</code> are functions that might fail to produce results, i.e. raise an exception, and when that happens, the whole <code>do</code>-block will fail, i.e. terminate with an exception.

In other words, the expression <code>father p</code>, which has type <code>Maybe Person</code>, is interpreted as a statement of an imperative language that returns a <code>Person</code> as result. This is true for all monads: a value of type <code>M a</code> is interpreted as a statement of an imperative language that returns a value of type <code>a</code> as result; and the semantics of this language are determined by the monad <code>M</code>. ''TODO: diagram, picture?''

Now, the bind operator <code>(>>=)</code> is simply a function version of the semicolon. Just like a <code>let</code> expression can be written as a function application,

    let x  = foo in bar     corresponds to      (\x -> bar) foo

an assignment and semicolon can be written as the bind operator:

        x <- foo;   bar     corresponds to      foo >>= (\x -> bar)

The <code>return</code> function lifts a value <code>a</code> to a full-fledged statement <code>M a</code> of the imperative language.


Different semantics of the imperative language correspond to different monads. The following table shows the classic selection that every Haskell programmer should know. Also, if the idea behind monads is still unclear to you, studying each of the following examples in the order suggested will not only give you a well-rounded toolbox but also help you understand the common abstraction behind them.

{|class="wikitable" style="margin:auto"
|-
!Semantics
!Monad
!Wikibook chapter
|-
| Exception (anonymous) || <code>Maybe</code> || [[Haskell/Understanding monads/Maybe]]
|-
| Exception (with error description) || <code>Error</code> || [[Haskell/Understanding monads/Error]]
|-
| Global state ||<code>State</code> || [[Haskell/Understanding monads/State]]
|-
| Input/Output ||<code>IO</code> || [[Haskell/Understanding monads/IO]]
|-
| Nondeterminism || <code>[]</code> (lists) || [[Haskell/Understanding monads/List]]
|-
| Environment ||<code>Reader</code> || [[Haskell/Understanding monads/Reader]]
|-
| Logger ||<code>Writer</code> || [[Haskell/Understanding monads/Writer]]
|}


Furthermore, the semantics do not only occur in isolation but can also be mixed and matched. This gives rise to [[Haskell/Monad transformers|monad transformers]].

Some monads, like [[Haskell/Monadic parser combinators|monadic parser combinators]] have loosened their correspondence to an imperative language.

== Monad Laws ==
We can't just allow any junky implementation of <code>(>>=)</code> and <code>return</code> if we want to interpret them as the primitive building blocks of an imperative language. For that, an implementation has to obey the following three laws:

<source lang="haskell">
   m >>= return     =  m                        -- right unit
   return x >>= f   =  f x                      -- left unit
   
   (m >>= f) >>= g  =  m >>= (\x -> f x >>= g)  -- associativity
</source>

In Haskell, every instance of the <code>Monad</code> type class is expected to obey them.

=== Return as neutral element ===
The behavior of <code>return</code> is specified by the left and right unit laws. They state that <code>return</code> doesn't perform any computation, it just collects values. For instance,

<source lang="haskell">
    maternalGrandfather p = do {
            m  <- mother p;
            gm <- father m;
            return gm;
        }
</source>

is exactly the same as

<source lang="haskell">
    maternalGrandfather p = do {
            m  <- mother p;
            father m;
        }
</source>

by virtue of the right unit law.

These two laws are in analogy to the laws for the neutral element of a [[Haskell/Monoids|monoid]].

=== Associativity of bind ===
The law of associativity makes sure that - just like the semicolon - the bind operator <code>(>>=)</code> only cares about the order of computations, not about their nesting, e.g. the following is equivalent:

<source lang="haskell">
    bothGrandfathers p =
       (father p >>= father) >>=
           (\gf -> (mother p >>= father) >>=
               (\gm -> return (gf,gm) ))
</source>

Again, this law is analogous to the associativity of a [[Haskell/Monoids|monoid]], although it looks a bit different due to the lambda expression <code>(\x -> f x >>= g)</code>. There is the alternative formulation as

<source lang="haskell">
   (f >=> g) >=> h  =  f >=> (g >=> h)
</source>

where <code>(>=>)</code> is the equivalent of function composition <code>(.)</code> for monads and defined as

<source lang="haskell">
   (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
   f >=> g = \x -> f x >>= g
</source>


The associativity of the ''then'' operator <code>(>>)</code> is a special case:

<source lang="haskell">
   (m >> n) >> o  =  m >> (n >> o)
</source>

== Monads and Category Theory ==
Monads originally come from a branch of mathematics called [[Haskell/Category theory|Category Theory]]. Fortunately, it is entirely unnecessary to understand category theory in order to understand and use monads in Haskell. However, the Category Theoretical definition of monads uses a slightly different presentation.  When translated into Haskell, this presentation gives a different, but equivalent definition of a monad which may be useful  to understand.

So far, we have defined monads in terms of <code>>>=</code> and <code>return</code>, but there's also an alternative definition that starts with monads as functors with two additional combinators

<source lang="haskell">
    fmap   :: (a -> b) -> M a -> M b  -- functor

    return :: a -> M a
    join   :: M (M a) -> M a
</source>

A functor <code>M</code> can be thought of as container, so that <code>M a</code> "contains" values of type <code>a</code>.

Under this interpretation, the functions behave as follows:
* <code>fmap</code> applies a given function to every element in a container
* <code>return</code> packages an element into a container,
* <code>join</code> takes a container of containers and turns them into a single container.

With these functions, the bind combinator is defined as follows:
<source lang="haskell">
    m >>= g = join (fmap g m)
</source>

Likewise, we could give a definition of <code>fmap</code> and <code>join</code> in terms of <code>>>=</code>:

<source lang="haskell">
    fmap f x = x >>= (return . f)
    join x   = x >>= id
</source>

For more information on this point of view, see also the [[Haskell/Category theory#Monads|wikibook chapter on Category Theory]].

== Footnotes ==
<references/>


{{Haskell navigation|chapter=Monads}}

[[Category:Haskell|Understanding monads]]
