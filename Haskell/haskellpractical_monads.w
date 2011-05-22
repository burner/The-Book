>{{Haskell minitoc|chapter=Monads}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Monads}}

== Parsing monads ==

In the beginner's track of this book, we saw how monads were used for IO.  We've also started working more extensively with some of the more rudimentary monads like <code>Maybe</code>, <code>List</code> or <code>State</code>.  Now let's try using monads for something quintessentially "practical".  Let's try writing a very simple parser. We'll be using the [http://research.microsoft.com/users/daan/download/parsec/parsec.html Parsec] library, which comes with GHC but may need to be downloaded separately if you're using another compiler.

Start by adding this line to the import section:

 import System
 import Text.ParserCombinators.Parsec hiding (spaces)

This makes the Parsec library functions and getArgs available to us, except the "spaces" function, whose name conflicts with a function that we'll be defining later.

Now, we'll define a parser that recognizes one of the symbols allowed in Scheme identifiers:

 
 symbol :: Parser Char
 symbol = oneOf "!$%&|*+-/:&lt;=&gt;?@^_~"

This is another example of a monad: in this case, the "extra information" that is being hidden is all the info about position in the input stream, backtracking record, first and follow sets, etc. Parsec takes care of all of that for us. We need only use the Parsec library function [http://research.microsoft.com/users/daan/download/parsec/parsec.html#oneOf oneOf], and it'll recognize a single one of any of the characters in the string passed to it. Parsec provides a number of pre-built parsers: for example, [http://research.microsoft.com/users/daan/download/parsec/parsec.html#letter letter] and [http://research.microsoft.com/users/daan/download/parsec/parsec.html#digit digit] are library functions. And as you're about to see, you can compose primitive parsers into more sophisticated productions.

Let's define a function to call our parser and handle any possible errors:

 
 readExpr :: String -&gt; String
 readExpr input = case parse symbol "lisp" input of
     Left err -&gt; "No match: " ++ show err
     Right val -&gt; "Found value"

As you can see from the type signature, readExpr is a function (-&gt;) from a String to a String. We name the parameter <span class="inline_code">input</span>, and pass it, along with the <span class="inline_code">symbol</span> action we defined above and the name of the parser ("lisp"), to the Parsec function [http://research.microsoft.com/users/daan/download/parsec/parsec.html#parse parse].

Parse can return either the parsed value or an error, so we need to handle the error case. Following typical Haskell convention, Parsec returns an [http://www.haskell.org/onlinereport/standard-prelude.html#$tEither Either] data type, using the Left constructor to indicate an error and the Right one for a normal value.

We use a <span class="inline_code">case...of</span> construction to match the result of <span class="inline_code">parse</span> against these alternatives. If we get a Left value (error), then we bind the error itself to <span class="inline_code">err</span> and return "No match" with the string representation of the error. If we get a Right value, we bind it to <span class="inline_code">val</span>, ignore it, and return the string "Found value".

The <span class="inline_code">case...of</span> construction is an example of pattern matching, which we will see in much greater detail [evaluator1.html#primitiveval later on].

Finally, we need to change our main function to call readExpr and print out the result:

 
 main :: IO ()
 main = do args &lt;- getArgs
           putStrLn <span class="changed_code">(readExpr (args !! 0))</span>

To compile and run this, you need to specify "-package parsec" on the command line, or else there will be link errors. For example:

 
 debian:/home/jdtang/haskell_tutorial/code# ghc -package parsec -o simple_parser [../code/listing3.1.hs listing3.1.hs]
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser $
 Found value
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser a
 No match: "lisp" (line 1, column 1):
 unexpected "a"

=== Whitespace ===

Next, we'll add a series of improvements to our parser that'll let it recognize progressively more complicated expressions. The current parser chokes if there's whitespace preceding our symbol:

 
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser "   %"
 No match: "lisp" (line 1, column 1):
 unexpected " "

Let's fix that, so that we ignore whitespace.

First, lets define a parser that recognizes any number of whitespace characters. Incidentally, this is why we included the "hiding (spaces)" clause when we imported Parsec: there's already a function "[http://research.microsoft.com/users/daan/download/parsec/parsec.html#spaces spaces]" in that library, but it doesn't quite do what we want it to. (For that matter, there's also a parser called [http://research.microsoft.com/users/daan/download/parsec/parsec.html#lexeme lexeme] that does exactly what we want, but we'll ignore that for pedagogical purposes.)

 
 spaces :: Parser ()
 spaces = skipMany1 space

Just as functions can be passed to functions, so can actions. Here we pass the Parser action [http://research.microsoft.com/users/daan/download/parsec/parsec.html#space space] to the Parser action [http://research.microsoft.com/users/daan/download/parsec/parsec.html#skipMany1 skipMany1], to get a Parser that will recognize one or more spaces.

Now, let's edit our parse function so that it uses this new parser. Changes are in red:

 
 readExpr input = case parse <span class="changed_code">(spaces &gt;&gt; symbol)</span> "lisp" input of
     Left err -&gt; "No match: " ++ show err
     Right val -&gt; "Found value"

We touched briefly on the &gt;&gt; ("bind") operator in lesson 2, where we mentioned that it was used behind the scenes to combine the lines of a do-block. Here, we use it explicitly to combine our whitespace and symbol parsers. However, bind has completely different semantics in the Parser and IO monads. In the Parser monad, bind means "Attempt to match the first parser, then attempt to match the second with the remaining input, and fail if either fails." In general, bind will have wildly different effects in different monads; it's intended as a general way to structure computations, and so needs to be general enough to accommodate all the different types of computations. Read the documentation for the monad to figure out precisely what it does.

Compile and run this code. Note that since we defined spaces in terms of skipMany1, it will no longer recognize a plain old single character. Instead you ''have to'' precede a symbol with some whitespace. We'll see how this is useful shortly:

 
 debian:/home/jdtang/haskell_tutorial/code# ghc -package parsec -o simple_parser [../code/listing3.2.hs listing3.2.hs]
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser "   %" Found value
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser %
 No match: "lisp" (line 1, column 1):
 unexpected "%"
 expecting space
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser "   abc"
 No match: "lisp" (line 1, column 4):
 unexpected "a"
 expecting space

=== Return Values ===

Right now, the parser doesn't ''do'' much of anything - it just tells us whether a given string can be recognized or not. Generally, we want something more out of our parsers: we want them to convert the input into a data structure that we can traverse easily. In this section, we learn how to define a data type, and how to modify our parser so that it returns this data type.

First, we need to define a data type that can hold any Lisp value:

 
 data LispVal = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool

This is an example of an ''algebraic data type''<nowiki>: it defines a set of possible values that a variable of type LispVal can hold. Each alternative (called a </nowiki>''constructor'' and separated by |) contains a tag for the constructor along with the type of data that that constructor can hold. In this example, a LispVal can be:

# An <span class="inline_code">Atom</span>, which stores a String naming the atom
# A <span class="inline_code">List</span>, which stores a list of other LispVals (Haskell lists are denoted by brackets)
# A <span class="inline_code">DottedList</span>, representing the Scheme form <span class="inline_lisp">(a b . c)</span>. This stores a list of all elements but the last, and then stores the last element as another field
# A <span class="inline_code">Number</span>, containing a Haskell Integer
# A <span class="inline_code">String</span>, containing a Haskell String
# A <span class="inline_code">Bool</span>, containing a Haskell boolean value

Constructors and types have different namespaces, so you can have both a constructor named String and a type named String. Both types and constructor tags always begin with capital letters.

Next, let's add a few more parsing functions to create values of these types. A string is a double quote mark, followed by any number of non-quote characters, followed by a closing quote mark:

 
 parseString :: Parser LispVal
 parseString = do char '"'
                  x &lt;- many (noneOf "\"")
                  char '"'
                  return $ String x

We're back to using the do-notation instead of the &gt;&gt; operator. This is because we'll be retrieving the value of our parse (returned by [http://research.microsoft.com/users/daan/download/parsec/parsec.html#many many] ([http://research.microsoft.com/users/daan/download/parsec/parsec.html#noneOf noneOf] "\"")) and manipulating it, interleaving some other parse operations in the meantime. In general, use &gt;&gt; if the actions don't return a value, &gt;&gt;= if you'll be immediately passing that value into the next action, and do-notation otherwise.

Once we've finished the parse and have the Haskell String returned from <span class="inline_code">many</span>, we apply the String constructor (from our LispVal data type) to turn it into a LispVal. Every constructor in an algebraic data type also acts like a function that turns its arguments into a value of its type. It also serves as a pattern that can be used in the left-hand side of a pattern-matching expression; we saw an example of this in [#symbols Lesson 3.1] when we matched our parser result against the two constructors in the Either data type.

We then apply the built-in function [http://www.haskell.org/onlinereport/standard-prelude.html#$tMonad return] to lift our LispVal into the Parser monad. Remember, each line of a do-block must have the same type, but the result of our String constructor is just a plain old LispVal. Return lets us wrap that up in a Parser action that consumes no input but returns it as the inner value. Thus, the whole parseString action will have type Parser LispVal.

The $ operator is infix function application: it's the same as if we'd written <span class="inline_code">return (String x)</span>, but $ is right-associative, letting us eliminate some parentheses. Since $ is an operator, you can do anything with it that you'd normally do to a function: pass it around, partially apply it, etc. In this respect, it functions like the Lisp function [http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.4 apply].

Now let's move on to Scheme variables. An [http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-5.html#%_sec_2.1 atom] is a letter or symbol, followed by any number of letters, digits, or symbols:

 
 parseAtom :: Parser LispVal
 parseAtom = do first &lt;- letter &lt;|&gt; symbol
                rest &lt;- many (letter &lt;|&gt; digit &lt;|&gt; symbol)
                let atom = [first] ++ rest
                return $ case atom of 
                           "#t" -&gt; Bool True
                           "#f" -&gt; Bool False
                           otherwise -&gt; Atom atom

Here, we introduce another Parsec combinator, the choice operator [http://research.microsoft.com/users/daan/download/parsec/parsec.html#or &lt;|&gt;]. This tries the first parser, then if it fails, tries the second. If either succeeds, then it returns the value returned by that parser. The first parser must fail before it consumes any input: we'll see later how to implement backtracking.

Once we've read the first character and the rest of the atom, we need to put them together. The "let" statement defines a new variable "atom". We use the list concatenation operator ++ for this. Recall that <span class="inline_code">first</span> is just a single character, so we convert it into a singleton list by putting brackets around it. If we'd wanted to create a list containing many elements, we need only separate them by commas.

Then we use a case statement to determine which LispVal to create and return, matching against the literal strings for true and false. The <span class="inline_code">otherwise</span> alternative is a readability trick: it binds a variable named <span class="inline_code">otherwise</span>, whose value we ignore, and then always returns the value of <span class="inline_code">atom</span>.

Finally, we create one more parser, for numbers. This shows one more way of dealing with monadic values:

 
 parseNumber :: Parser LispVal
 parseNumber = liftM (Number . read) $ many1 digit

It's easiest to read this backwards, since both function application ($) and function composition (.) associate to the right. The parsec combinator [http://www.cs.uu.nl/~daan/download/parsec/parsec.html#many1 many1] matches one or more of its argument, so here we're matching one or more digits. We'd like to construct a number LispVal from the resulting string, but we have a few type mismatches. First, we use the built-in function [http://www.haskell.org/onlinereport/standard-prelude.html#$vread read] to convert that string into a number. Then we pass the result to Number to get a LispVal. The function composition operator "." creates a function that applies its right argument and then passes the result to the left argument, so we use that to combine the two function applications.

Unfortunately, the result of <span class="inline_code">many1 digit</span> is actually a Parser LispVal, so our combined <span class="inline_code">Number . read</span> still can't operate on it. We need a way to tell it to just operate on the value inside the monad, giving us back a Parser LispVal. The standard function <span class="inline_code">liftM</span> does exactly that, so we apply liftM to our <span class="inline_code">Number . read</span> function, and then apply the result of that to our Parser.

We also have to import the Monad module up at the top of our program to get access to liftM:

 
 import Monad

This style of programming - relying heavily on function composition, function application, and passing functions to functions - is very common in Haskell code. It often lets you express very complicated algorithms in a single line, breaking down intermediate steps into other functions that can be combined in various ways. Unfortunately, it means that you often have to read Haskell code from right-to-left and keep careful track of the types. We'll be seeing many more examples throughout the rest of the tutorial, so hopefully you'll get pretty comfortable with it.

Let's create a parser that accepts either a string, a number, or an atom:

 
 parseExpr :: Parser LispVal
 parseExpr = parseAtom
         &lt;|&gt; parseString
         &lt;|&gt; parseNumber

And edit readExpr so it calls our new parser:

 
 readExpr :: String -&gt; String
 readExpr input = case parse <span class="changed_code">parseExpr</span> "lisp" input of
     Left err -&gt; "No match: " ++ show err
     Right _ -&gt; "Found value"

Compile and run this code, and you'll notice that it accepts any number, string, or symbol, but not other strings:

 
 debian:/home/jdtang/haskell_tutorial/code# ghc -package parsec -o simple_parser [.../code/listing3.3.hs listing3.3.hs]
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser "\"this is a string\""
 Found value
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser 25 Found value
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser symbol
 Found value
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser (symbol)
 bash: syntax error near unexpected token `symbol'
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser "(symbol)"
 No match: "lisp" (line 1, column 1):
 unexpected "("
 expecting letter, "\"" or digit

{{Exercises|1=
# Rewrite parseNumber using
## do-notation
## explicit sequencing with the [http://www.haskell.org/onlinereport/standard-prelude.html#tMonad &gt;&gt;=] operator
# Our strings aren't quite [http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.5 R5RS compliant], because they don't support escaping of internal quotes within the string. Change parseString so that \" gives a literal quote character instead of terminating the string. You may want to replace <span class="inline_code">noneOf "\""</span> with a new parser action that accepts ''either'' a non-quote character ''or'' a backslash followed by a quote mark.
# Modify the previous exercise to support \n, \r, \t, \\, and any other desired escape characters
# Change parseNumber to support the [http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4 Scheme standard for different bases]. You may find the [http://www.haskell.org/onlinereport/numeric.html#sect14 readOct and readHex] functions useful.
# Add a Character constructor to LispVal, and create a parser for [http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.4 character literals] as described in R5RS.
# Add a Float constructor to LispVal, and support R5RS syntax for [http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4 decimals]. The Haskell function [http://www.haskell.org/onlinereport/numeric.html#sect14 readFloat] may be useful.
# Add data types and parsers to support the [http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.1 full numeric tower] of Scheme numeric types. Haskell has built-in types to represent many of these; check the [http://www.haskell.org/onlinereport/standard-prelude.html#$tNum Prelude]. For the others, you can define compound types that represent eg. a Rational as a numerator and denominator, or a Complex as a real and imaginary part (each itself a Real number).
}}

=== Recursive Parsers: Adding lists, dotted lists, and quoted datums ===

Next, we add a few more parser actions to our interpreter. Start with the parenthesized lists that make Lisp famous:

 
 parseList :: Parser LispVal
 parseList = liftM List $ sepBy parseExpr spaces

This works analogously to parseNumber, first parsing a series of expressions separated by whitespace (<span class="inline_code">sepBy parseExpr spaces</span>) and then apply the List constructor to it within the Parser monad. Note too that we can pass parseExpr to [http://research.microsoft.com/users/daan/download/parsec/parsec.html#sepBy sepBy], even though it's an action we wrote ourselves.

The dotted-list parser is somewhat more complex, but still uses only concepts that we're already familiar with:

 
 parseDottedList :: Parser LispVal
 parseDottedList = do
     head &lt;- endBy parseExpr spaces
     tail &lt;- char '.' &gt;&gt; spaces &gt;&gt; parseExpr
     return $ DottedList head tail

Note how we can sequence together a series of Parser actions with &gt;&gt; and then use the whole sequence on the right hand side of a do-statement. The expression <span class="inline_code">char '.' &gt;&gt; spaces</span> returns a <span class="inline_code">Parser ()</span>, then combining that with parseExpr gives a Parser LispVal, exactly the type we need for the do-block.

Next, let's add support for the single-quote syntactic sugar of Scheme:

 <nowiki>
 parseQuoted :: Parser LispVal
 parseQuoted = do
     char '\''
     x &lt;- parseExpr
     return $ List [Atom "quote", x]
 </nowiki>

Most of this is fairly familiar stuff: it reads a single quote character, reads an expression and binds it to x, and then returns <span class="inline_lisp">(quote x)</span>, to use Scheme notation. The Atom constructor works like an ordinary function: you pass it the String you're encapsulating, and it gives you back a LispVal. You can do anything with this LispVal that you normally could, like put it in a list.

Finally, edit our definition of parseExpr to include our new parsers:

 
 parseExpr :: Parser LispVal
 parseExpr = parseAtom
         &lt;|&gt; parseString
         &lt;|&gt; parseNumber
         <span class="changed_code">&lt;|&gt; parseQuoted
         &lt;|&gt; do char '('
                x &lt;- (try parseList) &lt;|&gt; parseDottedList
                char ')'
                return x</span>

This illustrates one last feature of Parsec: backtracking. parseList and parseDottedList recognize identical strings up to the dot; this breaks the requirement that a choice alternative may not consume any input before failing. The [http://research.microsoft.com/users/daan/download/parsec/parsec.html#try try] combinator attempts to run the specified parser, but if it fails, it backs up to the previous state. This lets you use it in a choice alternative without interfering with the other alternative.

Compile and run this code:

 
 debian:/home/jdtang/haskell_tutorial/code# ghc -package parsec -o simple_parser [../code/listing3.4.hs listing3.4.hs]
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser "(a test)"
 Found value
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser "(a (nested) test)" Found value
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser "(a (dotted . list) test)"
 Found value
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser "(a '(quoted (dotted . list)) test)"
 Found value
 debian:/home/jdtang/haskell_tutorial/code# ./simple_parser "(a '(imbalanced parens)"
 No match: "lisp" (line 1, column 24):
 unexpected end of input
 expecting space or ")"

Note that by referring to parseExpr within our parsers, we can nest them arbitrarily deep. Thus, we get a full Lisp reader with only a few definitions. That's the power of recursion.

{{Exercises|1=
# Add support for the [http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.6 backquote] syntactic sugar: the Scheme standard details what it should expand into (quasiquote/unquote).
# Add support for [http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.6 vectors]. The Haskell representation is up to you: GHC does have an [http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Array.html Array] data type, but it can be difficult to use. Strictly speaking, a vector should have constant-time indexing and updating, but destructive update in a purely functional language is difficult. You may have a better idea how to do this after the section on set!, later in this tutorial.
# Instead of using the try combinator, left-factor the grammar so that the common subsequence is its own parser. You should end up with a parser that matches a string of expressions, and one that matches either nothing or a dot and a single expressions. Combining the return values of these into either a List or a DottedList is left as a (somewhat tricky) exercise for the reader: you may want to break it out into another helper function
}}

== Generic monads ==

:''Write me: The idea is that this section can show some of the benefits of not tying yourself to one single monad, but writing your code for any arbitrary monad m.  Maybe run with the idea of having some elementary monad, and then deciding it's not good enough, so replacing it with a fancier one... and then deciding you need to go even further and just plug in a monad transformer''

For instance: Using the Identity Monad:
 module Identity(Id(Id)) where
 
 newtype Id a = Id a
 instance Monad Id where
     (&gt;&gt;=) (Id x) f = f x
     return = Id
 
 instance (Show a) =&gt; Show (Id a) where
     show (Id x) = show x

In another File

 import Identity
 type M = Id
 
 my_fib :: Integer -> M Integer
 my_fib = my_fib_acc 0 1
 
 my_fib_acc :: Integer -> Integer -> Integer -> M Integer
 my_fib_acc _ fn1 1 = return fn1
 my_fib_acc fn2 _ 0 = return fn2
 my_fib_acc fn2 fn1 n_rem = do
     val <- my_fib_acc fn1 (fn2+fn1) (n_rem - 1)
     return val

Doesn't seem to accomplish much, but It allows to you add debugging facilities to a part of your program on the fly.
As long as you've used return instead of explicit Id constructors, then you can drop in the following monad:

 module PMD (Pmd(Pmd)) where --PMD = Poor Man's Debugging, Now available for haskell
 
 import IO
 
 newtype Pmd a = Pmd (a, IO ())
 
 instance Monad Pmd where
     (&gt;&gt;=)  (Pmd (x, prt)) f = let (Pmd (v, prt')) = f x 
                               in Pmd (v, prt &gt;&gt; prt')
     return x = Pmd (x, return ())
 
 instance (Show a) =&gt; Show (Pmd a) where
     show (Pmd (x, _) ) = show x

If we wanted to debug our Fibonacci program above, We could modify it as follows:

 import Identity
 import PMD
 import IO
 type M = Pmd
 ...
 my_fib_acc :: Integer -> Integer -> Integer -> M Integer
 my_fib_acc _ fn1 1 = return fn1
 my_fib_acc fn2 _ 0 = return fn2
 my_fib_acc fn2 fn1 n_rem =
     val <- my_fib_acc fn1 (fn2+fn1) (n_rem - 1)
     Pmd (val, putStrLn (show fn1))

All we had to change is the lines where we wanted to print something for debugging, and add some code wherever
you extracted the value from the Id Monad to execute the resulting IO () you've returned. Something like

 main :: IO ()
 main = do
     let (Id f25) = my_fib 25
     putStrLn ("f25 is: " ++ show f25)

for the Id Monad vs.
 
 main :: IO ()
 main = do
     let (Pmd (f25, prt)) = my_fib 25
     prt
     putStrLn ("f25 is: " ++ show f25)

For the Pmd Monad. Notice that we didn't have to touch any of the functions that we weren't debugging.

== Stateful monads for concurrent applications ==

You're going to have to know about [[../Monad transformers | Monad transformers ]] before you can do these things.
Although the example came up because of [[../Concurrency | Concurrency ]], if you realize a TVar is a mutable variable of some kind, why this example came up might make some sense to you.

This is a little trick that I find makes writing stateful concurrent applications easier, especially for network applications.  Lets look at an imaginary stateful server.

Each currently connected client has a thread allowing the client to update the state.

The server also has a main logic thread which also transforms the state.

So you want to allow the client to update the state of the program

It's sometimes really simple and easy to expose the whole state of the program in a TVar, but I find this can get really messy, especially when the definition of the state changes!

Also it can be very annoying if you have to do anything conditional.

So to help tidy things up ( Say your state is called World )

=== Make a monad over state ===

First, make a monad over the World type

 import Control.Monad.State.Lazy

 -- heres yer monad
 -- it can liftIO too
 type WorldM
  = StateT World IO

 data World =
   World { objects :: [ WorldObject ] }

Now you can write some accessors in WorldM

 -- maybe you have a bunch of objects each with a unique id
 import Data.Unique
 import Data.Maybe
 import Prelude hiding ( id )

 data WorldObject =
    WorldObject { id :: Unique }

 -- check Control.Monad.State.Lazy if you are confused about get and put
 addObject :: WorldObject -> WorldM ( )
 addObject wO = do
    wst <- get
    put $ World $ wO : ( objects wst )

 -- presuming unique id
 getObject :: Unique -> WorldM ( Maybe WorldObject )
 getObject id1 = do
    wst <- get
    return $ listToMaybe $ filter ( \ wO -> id wO == id1 )
                                  ( objects wst )

now heres a type representing a change to the World

   data WorldChange = NewObj WorldObject |
                      UpdateObj WorldObject | -- use the unique ids as replace match
                      RemoveObj Unique -- delete obj with named id

What it looks like all there's left to do is to

   type ChangeBucket = TVar [ WorldChange ]

   mainLoop :: ChangeBucket -> WorldM ( )
   mainLoop cB =
      -- do some stuff
         -- it's probably fun
            -- using your cheeky wee WorldM accessors
      mainLoop cB -- recurse on the shared variable
      

Remember, your main thread is a transformer of World and IO so it can run 'atomically' and read the changeBucket.

Now, presuming you have a function that can incorporate a WorldChange into the existing WorldM your 'wait-for-client-input' thread can communicate with the main thread of the program, and it doesn't look too nasty.

=== Make the external changes to the state monadic themselves ===

However!  Since all the state inside your main thread is now hidden from the rest of the program and you communicate through a one way channel --- data goes from the client to the server, but the mainLoop keeps its state a secret --- your client thread is never going to be able to make conditional choices about the environment - the client thread runs in IO but the main thread runs in WorldM.

So the REAL type of your shared variable is
   
   type ChangeBucket = 
      TVar [ WorldM ( Maybe WorldChange ) ]

This can be generated from the client-input thread, but you'll be able to include conditional statements inside the code, which is only evaluated against the state when it is run from your main thread

It all sounds a little random, but it's made my life a lot easier.  Heres some real working code, based on this idea 

* this takes commands from a client, and attempts change the object representing the client inside the game's state
* the output from this function is then written to a ChangeBucket ( using the ChangeBucket definition in this section, above ) and run inside the DState of the game's main loop.

( you might want to mentally substitute DState for WorldM ) 

   -- cmd is a command generated from parsing network input
   mkChange :: Unique -> Command -> DState ( Maybe WorldChange )
   mkChange oid cmd = do
      mp <- getObject oid -- this is maybe an object, as per the getObject definition earlier in the article
      -- enter the maybe monad
      return $ do p <- mp -- if its Nothing, the rest will be nothing
                  case cmd of
                     -- but it might be something
                     Disconnect ->
                        Just $ RemoveObject oid
                     Move x y -> 
                        Just $ UpdateObject $ DO ( oid )
                                                 ( name p )
                                                 ( speed p )
                                                 ( netclient p )
                                                 ( pos p )
                                                 [ ( x , y ) ]
                                                 ( method p )

==== A note and some more ideas. ====

Another design might just have
  type ChangeBucket = TVar [ WorldM ( ) ]

And so just update the game world as they are run.  I have other uses for the WorldM ( Maybe Change ) type.

So I conclude - All I have are my monads and my word so go use your monads imaginatively and write some computer games ;)

{{Haskell stub|sectiononly=1}}

{{Haskell/Navigation|chapter=Monads}}
