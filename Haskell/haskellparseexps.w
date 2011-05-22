>== Parsing Mathematical Expressions ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== Parsing Mathematical Expressions ==

This chapter discusses how to turn strings of text such as "3*sin x + y" into an abstract syntactic representation like Plus (Times (Number 3) (Apply "sin" (Variable "x"))) (Variable "y").

We are going to use [http://hackage.haskell.org/packages/archive/base/4.3.0.0/doc/html/Text-ParserCombinators-ReadP.html ReadP] throughout, so you will need to have the reference open to refer to.

=== First Warmup ===

    import Text.ParserCombinators.ReadP

For a warmup, to get started on the problem, we first try an easier problem. A language where the symbols are just the letter "o", a single operator "&" and brackets. First define a data type for these trees:

    data Tree = Branch Tree Tree | Leaf deriving Show

Now a parser for leaves is defined using the ReadP library:

    leaf = do char 'o'
              return Leaf

now to define a parser for the branches, made up by "&" operator we need to choose an associativity. That is, whether o&o&o should be the same as (o&o)&o or o&(o&o) - let us pick the latter.

For a first approximation we can forget about brackets, adding them in after the first "milestone":

    branch = do a <- leaf
                char '&'
                b <- tree
                return (Branch a b)
    
    tree = leaf +++ branch

It's now possible to test this out and see if it acts properly on a few inputs:

    *Main> readP_to_S tree "o"
    [(Leaf,"")]
    *Main> readP_to_S tree "o&o"
    [(Leaf,"&o"),(Branch Leaf Leaf,"")]
    *Main> readP_to_S tree "o&o&o"
    [(Leaf,"&o&o"),(Branch Leaf Leaf,"&o"),(Branch Leaf (Branch Leaf Leaf),"")]

Since that worked fine we can proceed to add support for parenthesis. Brackets are defined generally, so that we can reuse it later on

    brackets p = do char '('
                    r <- p
                    char ')'
                    return r

We can now update the branch and tree parsers to support brackets:

    branch = do a <- leaf +++ brackets tree
                char '&'
                b <- tree
                return (Branch a b)
    
    tree = leaf +++ branch +++ brackets tree

A bit of testing shows that it seems to work

    *Main> readP_to_S tree "((o&((o&o)))&o&((o&o)&o)&o)"
    [(Branch (Branch Leaf (Branch Leaf Leaf)) (Branch Leaf (Branch (Branch (Branch Leaf Leaf) Leaf) Leaf)),"")]

=== Adaptation ===

This gives a good starting point for adaptation. The first modification towards the ultimate goal, which is quite easy to do, is changing the leaves from just "o" to any string. To do this we have change to `Leaf` to `Leaf String` in the data type and update the leaf function:

    data Tree = Branch Tree Tree | Leaf String deriving Show
    
    leaf = do s <- many1 (choice (map char ['a'..'z']))
              return (Leaf s)

For the next adaptation we try and add a new operation "|" which binders weaker than "&". I.e. "foo&bar|baz" should parse as "(foo&bar)|baz". First we need to update the data type representing syntax:

    data Operator = And | Or deriving Show
    
    data Tree = Branch Operator Tree Tree | Leaf String deriving Show

The obvious thing to do is duplicate the `branch` function and call it `andBranch` and `orBranch`, and give or precedence using the left choice operator `<++`:

    andBranch = do a <- leaf +++ brackets tree
                   char '&'
                   b <- tree
                   return (Branch And a b)
    
    orBranch = do a <- leaf +++ brackets tree
                  char '|'
                  b <- tree
                  return (Branch Or a b)
    
    tree = leaf +++ (orBranch <++ andBranch) +++ brackets tree

This modification does not work though, if we think of an expression such as "a&b&c&d|e&f&g&h|i&j&k|l&m&n&o|p&q&r|s" as a tree "X|Y|Z|W|P|Q" (which we already know how to parse!) except that the leaves are a more complicated form (but again, one we already know how to parse) then we can compose a working parser:

    andBranch = do a <- leaf +++ brackets tree
                   char '&'
                   b <- andTree
                   return (Branch And a b)
    
    andTree = leaf +++ brackets tree +++ andBranch
    
    orBranch = do a <- andTree +++ brackets tree
                  char '|'
                  b <- orTree
                  return (Branch Or a b)
    
    orTree = andTree +++ brackets tree +++ orBranch
    
    tree = orTree

While this approach does work, for example:

    *Main> readP_to_S tree "(foo&bar|baz)"
    [(Leaf "","(foo&bar|baz)"),(Branch Or (Branch And (Leaf "foo") (Leaf "bar")) (Leaf "baz"),""),(Branch Or (Branch And (Leaf "foo") (Leaf "bar")) (Leaf "baz"),"")]
    *Main> readP_to_S tree "(foo|bar&baz)"
    [(Leaf "","(foo|bar&baz)"),(Branch Or (Leaf "foo") (Branch And (Leaf "bar") (Leaf "baz")),""),(Branch Or (Leaf "foo") (Branch And (Leaf "bar") (Leaf "baz")),"")]

it parses ambiguously, which is undesirable for efficiency reasons as well as hinting that we may have done something unnatural. Both `andTree` and `orTree` functions have `brackets tree` in them, since `orTree` contains `andTree` this is where the ambiguity creeps in. To solve it we simply delete from `orTree`.

    orTree = andTree +++ orBranch

=== Structure Emerges ===

All the previous fiddling and playing has actually caused a significant portion of the structure of our final program to make its-self clear. Looking back at what was written we could quite easily extend it to add another operator, and another after that (Exercise for the reader: if it is not clear exactly how this would be done, figure it out and do it). A moments meditation suggests that we might complete this pattern and abstract it out, given an arbitrarily long list of operators

    operators = [(Or,"|"),(And,"+")]

or perhaps

    data Operator = Add | Mul | Exp deriving Show
    
    operators = [(Add,"+"),(Mul,"*"),(Exp,"^")]

the parser should be computed from it, nesting it (as we did manually in the past) so that parses happen correctly without ambiguity.

The seasoned haskell programmer will have already seen, in her minds eye, the following:

    tree = foldr (\(op,name) p ->
                   let this = p +++ do a <- p +++ brackets tree
                                       char name
                                       b <- this
                                       return (Branch op a b)
                    in this)
                 (leaf +++ brackets tree)
                 operators

which is then tested.

    *Main> readP_to_S tree "(x^e*y+w^e*z^e)"
    [(Leaf "","(x^e*y+w^e*z^e)"),(Branch Add (Branch Mul (Branch Exp (Leaf "x") (Leaf "e")) (Leaf "y")) (Branch Mul (Branch Exp (Leaf "w") (Leaf "e")) (Branch Exp (Leaf "z") (Leaf "e"))),"")]

This is a good checkpoint to pause, in summary we have distilled the embryonic parser down to the following script:

    import Text.ParserCombinators.ReadP
    
    brackets p = do char '('
                    r <- p
                    char ')'
                    return r
    
    data Operator = Add | Mul | Exp deriving Show
    operators = [(Add,'+'),(Mul,'*'),(Exp,'^')]
    
    data Tree = Branch Operator Tree Tree | Leaf String deriving Show
    
    leaf = do s <- many1 (choice (map char ['a'..'z']))
              return (Leaf s)
    
    tree = foldr (\(op,name) p ->
                   let this = p +++ do a <- p +++ brackets tree
                                       char name
                                       b <- this
                                       return (Branch op a b)
                    in this)
                 (leaf +++ brackets tree)
                 operators

=== Whitespace and applicative notation ===

Since both the functional/applicative notation and ignoring whitespace depend on some of the same characters (space characters) it is a useful question to ask which should be implemented first, or whether it is not important which should be programmed first.

Considering the expression "f  x", suggests that we should find how to parse whitespace before handling applicative notation, since once it has been dealt with function application should just correspond to simple juxtaposition (as intended).

There is a technical difficultly making our current parser ignore whitespace: if we were to make a `skipWhitespace` parser, and put it everywhere that whitespace could occur we would be inundated with ambiguous parses. Hence it is necessary to skip whitespace only in certain crucial places, for example we could pick the convention that whitespace is always skipped *before* reading a token. Then " a + b * c " would be seen by the parser chunked in the following way "[ a][ +][ b][ *][ c][ ]". Which convention we choose is arbitrary, but ignoring whitespace before seems slightly neater, since it handles " a" without any complaints.

We define the following:

    skipWhitespace = do many (choice (map char [' ','\n']))
                        return ()

and update all the parses written before, so that they follow the new convention

    brackets p = do skipWhitespace
                    char '('
                    r <- p
                    skipWhitespace
                    char ')'
                    return r
    
    leaf = do skipWhitespace
              s <- many1 (choice (map char ['a'..'z']))
              return (Leaf s)
    
    tree = foldr (\(op,name) p ->
                   let this = p +++ do a <- p +++ brackets tree
                                       skipWhitespace
                                       char name
                                       b <- this
                                       return (Branch op a b)
                    in this)
                 (leaf +++ brackets tree)
                 operators

In order to add applicative support clearly the syntax needs to allow for it:

    data Tree = Apply Tree Tree | Branch Operator Tree Tree | Leaf String deriving Show

This syntax tree will allow for sentences such as "(x + y) foo", while this not correct other sentences like "(f . g) x" are commonplace in haskell - it should be the job of the type-checker to decide which is meaningful and which is not: This separation of concerns lets our problem (parsing) remain simple and homogeneous.

Our parser is essentially just two functions `leaf` and `tree` (`skipWhitespace` and `brackets` being considered "library" or helper functions). The function `tree` eats up all the operators it can, attaching leaves onto them as it can. While the `leaf` function could be thought of as reading in anything which doesn't have operators in it. Given this view of the program it is clear that to support applicative notation one needs to replace leaf with something that parses a chain of functional applications.

The obvious thing to try is then,

    leaf = chainl1 (do skipWhitespace
                       s <- many1 (choice (map char ['a'..'z']))
                       return (Leaf s))
                   (return Apply)

and it is easily extended to support the "commonplace" compound sentences discussed earlier:

    leaf = chainl1 (brackets tree
                    +++ do skipWhitespace
                           s <- many1 (choice (map char ['a'..'z']))
                           return (Leaf s))
                   (return Apply)

This is the problem completely solved! Our original goal is completed, one only needs to specify the operators they would like to have (in order) and write a traversal function converts the `Tree` into say mathematical expressions -- giving errors if unknown functions were used etc.

=== Making it Modular ===

The algorithms written are general enough to be useful in different circumstances, and even if they only had a single use -- if we were planning on using them in a larger program it is essential that we isolate the internals from the extenals (its interface).

<code>
 module Parser
  ( Tree(..), parseExpression
  ) where
 
 import Data.Maybe
 import Text.ParserCombinators.ReadP
 
 skipWhitespace = do many (choice (map char [' ','\n']))
                     return ()
 
 brackets p = do skipWhitespace
                 char '('
                 r <- p
                 skipWhitespace
                 char ')'
                 return r
 
 data Tree op = Apply (Tree op) (Tree op) | Branch op (Tree op) (Tree op) | Leaf String deriving Show
 
 parseExpression operators = listToMaybe . map fst . filter (null .snd) . readP_to_S tree where
  leaf = chainl1 (brackets tree
                  +++ do skipWhitespace
                         s <- many1 (choice (map char ['a'..'z']))
                         return (Leaf s))
                 (return Apply)
  tree = foldr (\(op,name) p ->
                 let this = p +++ do a <- p +++ brackets tree
                                     skipWhitespace
                                     char name
                                     b <- this
                                     return (Branch op a b)
                  in this)
               (leaf +++ brackets tree)
               operators
</code>

{{BookCat}}
