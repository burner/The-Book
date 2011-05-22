>{{Haskell minitoc|chapter=General Practices}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=General Practices}}

== Quickcheck ==

Consider the following function:

<pre>
getList = find 5 where
     find 0 = return []
     find n = do
       ch <- getChar
       if ch `elem` ['a'..'e'] then do
             tl <- find (n-1)
             return (ch : tl) else
           find n
</pre>

How would we effectively test this function in Haskell? The
solution we turn to is refactoring and QuickCheck.

=== Keeping things pure ===

The reason your getList is hard to test, is that the side effecting monadic code 
is mixed in with the pure computation, making it difficult to test
without moving entirely into a "black box" IO-based testing model.
Such a mixture is not good for reasoning about code.

Let's untangle that, and then test the referentially transparent
parts simply with QuickCheck. We can take advantage of lazy IO firstly,
to avoid all the unpleasant low-level IO handling. 

So the first step is to factor out the IO part of the function into a
thin "skin" layer:

<pre>
-- A thin monadic skin layer
getList :: IO [Char]
getList = fmap take5 getContents

-- The actual worker
take5 :: [Char] -> [Char]
take5 = take 5 . filter (`elem` ['a'..'e'])
</pre>

=== Testing with QuickCheck ===

Now we can test the 'guts' of the algorithm, the take5 function, in isolation. Let's use QuickCheck. First we need an Arbitrary instance for the Char type -- this takes care of generating random Chars for us to test with. I'll restrict it to a range of nice chars just for simplicity:

<pre>
import Data.Char
import Test.QuickCheck

instance Arbitrary Char where
    arbitrary     = choose ('\32', '\128')
    coarbitrary c = variant (ord c `rem` 4)
</pre>

Let's fire up GHCi (or Hugs) and try some generic properties (it's nice that we can use the QuickCheck testing framework directly from the Haskell REPL). An easy one first, a [Char] is equal to itself:

<pre>
*A> quickCheck ((\s -> s == s) :: [Char] -> Bool)
OK, passed 100 tests.
</pre>

What just happened? QuickCheck generated 100 random [Char] values, and
applied our property, checking the result was True for all cases.
QuickCheck ''generated the test sets for us''!

A more interesting property now: reversing twice is the identity:

<pre>
*A> quickCheck ((\s -> (reverse.reverse) s == s) :: [Char] -> Bool)
OK, passed 100 tests.
</pre>

Great!

=== Testing take5 ===

The first step to testing with QuickCheck is to work out some properties
that are true of the function, for all inputs. That is, we need to find
''invariants''.

A simple invariant might be:
<math>\forall s.\; \mathit{length} (\mathit{take}5\; s) = 5</math>

So let's write that as a QuickCheck property:
<pre>
\s -> length (take5 s) == 5
</pre>

Which we can then run in QuickCheck as:
<pre>
*A> quickCheck (\s -> length (take5 s) == 5)
Falsifiable, after 0 tests:
""
</pre>

Ah! QuickCheck caught us out. If the input string contains less than 5
filterable characters, the resulting string will be no more than 5
characters long. So let's weaken the property a bit:
<math>\forall s.\; \mathit{length} (\mathit{take}5\; s) \le 5</math>

That is, take5 returns a string of at most 5 characters long. Let's test
this:    
<pre>
*A> quickCheck (\s -> length (take5 s) <= 5)
OK, passed 100 tests.
</pre>

Good!

=== Another property ===

Another thing to check would be that the correct characters are
returned. That is, for all returned characters, those characters are
members of the set ['a','b','c','d','e'].

We can specify that as:
<math>\forall s. \forall e. (e \in take5\; s) \Rightarrow (e \in \{a, b, c, d, e\}) </math>

And in QuickCheck:
<pre>
*A> quickCheck (\s -> all (`elem` ['a'..'e']) (take5 s))
OK, passed 100 tests.
</pre>

Excellent. So we can have some confidence that the function neither
returns strings that are too long, nor includes invalid characters.

=== Coverage ===

One issue with the default QuickCheck configuration, when testing
[Char], is that the standard 100 tests isn't enough for our situation.
In fact, QuickCheck never generates a String greater than 5 characters
long, when using the supplied Arbitrary instance for Char! We can confirm
this:

<pre>
*A> quickCheck (\s -> length (take5 s) < 5)
OK, passed 100 tests.
</pre>

QuickCheck wastes its time generating different Chars, when what we
really need is longer strings. One solution to this is to modify
QuickCheck's default configuration to test deeper:

<pre>
deepCheck p = check (defaultConfig { configMaxTest = 10000}) p
</pre>

This instructs the system to find at least 10000 test cases before
concluding that all is well. Let's check that it is generating longer
strings:

<pre>
*A> deepCheck (\s -> length (take5 s) < 5)
Falsifiable, after 125 tests:
";:iD^*NNi~Y\\RegMob\DEL@krsx/=dcf7kub|EQi\DELD*"
</pre>

We can check the test data QuickCheck is generating using the
'verboseCheck' hook. Here, testing on integers lists:

<pre>
*A> verboseCheck (\s -> length s < 5)
0: []
1: [0]
2: []
3: []
4: []
5: [1,2,1,1]
6: [2]
7: [-2,4,-4,0,0]
Falsifiable, after 7 tests:
[-2,4,-4,0,0]
</pre>

=== More information on QuickCheck ===

* http://haskell.org/haskellwiki/Introduction_to_QuickCheck
* http://haskell.org/haskellwiki/QuickCheck_as_a_test_set_generator

== HUnit ==

Sometimes it is easier to give an example for a test than to define one from a general rule.  HUnit provides a unit testing framework which helps you to do just this.  You could also abuse QuickCheck by providing a general rule which just so happens to fit your example; but it's probably less work in that case to just use HUnit.

:''TODO: give an example of HUnit test, and a small tour of it''

More details for working with HUnit can be found in its [http://hunit.sourceforge.net/HUnit-1.0/Guide.html user's guide].

{{Haskell stub|sectiononly=1}}

----

{{Haskell import Haskell wiki|Introduction_to_QuickCheck|Introduction to QuickCheck}}
{{-}}
{{Haskell navigation|chapter=General Practices}}
{{Auto category}}
