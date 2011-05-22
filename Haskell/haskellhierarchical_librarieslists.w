>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Libraries Reference}}

The '''List''' datatype (see [http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html Data.List]) is the fundamental data structure in Haskell — this is the basic building-block of data storage and manipulation. In computer science terms it is a singly-linked list. In the hierarchical library system the List module is stored in <code>Data.List</code>; but this module only contains utility functions. The definition of list itself is integral to the Haskell language.

== Theory ==

A singly-linked list is a set of values in a defined order. The list can only be traversed in one direction (i.e., you cannot move back and forth through the list like tape in a cassette machine).

The list of the first 5 positive integers is written as
 [ 1, 2, 3, 4, 5 ]
We can move through this list, examining and changing values, from left to right, but not in the other direction. This means that the list
 [ 5, 4, 3, 2, 1 ]
is not just a trivial change in perspective from the previous list, but the result of significant computation (''O(n)'' in the length of the list).

== Definition ==

The polymorphic list datatype can be defined with the following recursive definition:
 data [a] = []
          | a : [a]
The "base case" for this definition is <code>[]</code>, the empty list. In order to put something into this list, we use the <code>(:)</code> constructor
 emptyList = []
 oneElem = 1:[]
The <code>(:)</code> (pronounced ''cons'') is right-associative, so that creating multi-element lists can be done like
 manyElems = 1:2:3:4:5:[]
or even just
 manyElems' = [1,2,3,4,5]

== Basic list usage ==
=== Prepending ===
It's easy to hard-code lists without cons, but run-time list creation will use cons. For example, to push an argument onto a simulated stack, we would use:
 push :: Arg -> [Arg] -> [Arg]
 push arg stack = arg:stack

=== Pattern-matching ===
If we want to examine the top of the stack, we would typically use a peek function. We can try pattern-matching for this.
 peek :: [Arg] -> Maybe Arg
 peek [] = Nothing
 peek (a:as) = Just a
The <code>a</code> before the ''cons'' in the pattern matches the head of the list. The <code>as</code> matches the tail of the list. Since we don't actually want the tail (and it's not referenced anywhere else in the code), we can tell the compiler this explicitly, by using a wild-card match, in the form of an underscore:
 peek (a:_) = Just a

== List utilities ==
''FIXME: is this not covered in the chapter on [[Haskell/List processing| list manipulation]]?''
=== Maps ===
=== Folds, unfolds and scans ===
=== Length, head, tail etc. ===

{{Haskell navigation|chapter=Libraries Reference}}

{{Auto category}}
