>Lists and tuples are the two most fundamental ways of manipulating several values together, by grouping them into a single value.
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">Lists and tuples are the two most fundamental ways of manipulating several values together, by grouping them into a single value.

{{Haskell minitoc|chapter=Haskell Basics}}
==Lists==

Functions are one of the two major building blocks of any Haskell program. The other is the list. So let's switch over to the interpreter and build lists:

{{HaskellGHCi|1=
Prelude> let numbers = [1,2,3,4]
Prelude> let truths  = [True, False, False]
Prelude> let strings = ["here", "are", "some", "strings"]
}}

The square brackets delimit the list, and individual elements are separated by commas. The only important restriction is that all elements in a list must be of the same type. Trying to define a list with mixed-type elements results in a typical type error: <!-- alliteration intended -->

{{HaskellGHCi|1=
Prelude> let mixed = [True, "bonjour"]

<interactive>:1:19:
    Couldn't match `Bool' against `[Char]'
      Expected type: Bool
      Inferred type: [Char]
    In the list element: "bonjour"
    In the definition of `mixed': mixed = [True, "bonjour"]
}}

=== Building lists ===

In addition to specifying the whole list at once using square brackets and commas, you can build them up piece by piece using the <tt>(:)</tt> operator. This process is often referred to as '''consing'''<ref>You might object that "cons" isn't even a proper word. Well, it isn't. LISP programmers invented the verb "to cons" to refer to this specific task of appending an element to the front of a list. "cons" happens to be a mnemonic for "constructor". Later on we will see why that makes sense.</ref>.

{{HaskellGHCiExample|1=Consing something on to a list|2=
Prelude> let numbers = [1,2,3,4]
Prelude> numbers
[1,2,3,4]
Prelude> 0:numbers
[0,1,2,3,4]
}}

When you cons something on to a list (<tt>something:someList</tt>), what you get back is another list. Therefore you can keep on consing for as long as you wish. It is important to note that the cons operator evaluates from right to left. Another (more general) way to think of it is that it takes the first value to its left and the whole expression to its right.

{{HaskellGHCiExample|1=Consing lots of things to a list|2=
Prelude> 1:0:numbers
[1,0,1,2,3,4]
Prelude> 2:1:0:numbers
[2,1,0,1,2,3,4]
Prelude> 5:4:3:2:1:0:numbers
[5,4,3,2,1,0,1,2,3,4]
}}

In fact, this is how lists are actually built, by consing all elements to the ''empty list'', <tt>[]</tt>. The commas-and-brackets notation is just ''syntactic sugar'', a more pleasant way to write code. So <tt>[1,2,3,4,5]</tt> is exactly equivalent to <tt>1:2:3:4:5:[]</tt>

You will, however, want to watch out for a potential pitfall in list construction. Whereas something like <tt>True:False:[]</tt> is perfectly good Haskell, <tt>True:False</tt> is ''not'':

{{HaskellGHCiExample|1=Whoops!|2=
Prelude> True:False

<interactive>:1:5:
    Couldn't match `[Bool]' against `Bool'
      Expected type: [Bool]
      Inferred type: Bool
    In the second argument of `(:)', namely `False'
    In the definition of `it': it = True : False
}}

<tt>True:False</tt> produces a familiar-looking type error message, which tells us that the cons operator <tt>(:)</tt> (which is really just a function) expected a list as its second argument but we gave it another <tt>Bool</tt> instead. <tt>(:)</tt> only knows how to stick things onto lists.<ref>At this point you might be justified in seeing types as an annoying thing. In a way they are, but more often than not they are actually a lifesaver. In any case, when you are programming in Haskell and something blows up, you'll probably want to think "type error".</ref>

Summarizing:

* The elements of the list must have the same type.
* You can only cons <code>(:)</code> something onto a list.


{{Exercises|1=
# Would the following piece of Haskell work: <code>3:[True,False]</code>? Why or why not?
# Write a function <code>cons8</code> that takes a list and conses <code>8</code> on to it. Test it out on the following lists by doing:
## <code>cons8 []</code>
## <code>cons8 [1,2,3]</code>
## <code>cons8 [True,False]</code>
## <code>let foo = cons8 [1,2,3]</code>
## <code>cons8 foo</code>
# Write a function that takes two arguments, a list and a thing, and conses the thing onto the list. You should start out with:
&#8195;&#8195;&#8195;&#8195;&#8195;<code>let myCons list thing =</code>
}}

=== Strings are just lists ===

As we briefly mentioned in the Type Basics module, strings in Haskell are just lists of characters. That means values of type String can be manipulated just like any other list. For instance, instead of entering strings directly as a sequence of characters enclosed in double quotation marks, they may also be constructed through a sequence of Char values, either linked with <tt>(:)</tt> and terminated by an empty list or using the commas-and-brackets notation.

{{HaskellGHCi|1=
Prelude>"hey" == ['h','e','y']
True
Prelude>"hey" == 'h':'e':'y':[]
True
}}

Using double-quoted strings is just more syntactic sugar.

=== Lists within lists ===

Lists can contain ''anything'', just as long as they are all of the same type. Because lists are things too, lists can contain other lists! Try the following in the interpreter:

{{HaskellGHCiExample|1=Lists can contain lists|2=
Prelude> let listOfLists = [[1,2],[3,4],[5,6]]
Prelude> listOfLists
[[1,2],[3,4],[5,6]]
}}

Lists of lists can be pretty tricky sometimes, because a list of things does not have the same type as a thing all by itself; the type <tt>Int</tt>, for example, is different from <tt>[Int]</tt>. Let's sort through these implications with a few exercises:

{{Exercises|1=
#Which of these are valid Haskell and which are not? Rewrite in cons notation.
## <code>[1,2,3,[]]</code>
## <code>[1,[2,3],4]</code>
## <code>[[1,2,3],[]]</code>
#Which of these are valid Haskell, and which are not? Rewrite in comma and bracket notation.
## <code>[]:[[1,2,3],[4,5,6]]</code>
## <code>[]:[]</code>
## <code>[]:[]:[]</code>
## <code>[1]:[]:[]</code>
## <code>["hi"]:[1]:[]</code>
#Can Haskell have lists of lists of lists? Why or why not?
#Why is the following list invalid in Haskell?
## <code>[[1,2],3,[4,5]]</code>
}}

Lists of lists are extremely useful, because they allow you to express some very complicated, structured data (two-dimensional matrices, for example). They are also one of the places where the Haskell type system truly shines. Human programmers, or at least this wikibook co-author, get confused ''all'' the time when working with lists of lists, and having restrictions of types often helps in wading through the potential mess.

== Tuples ==

=== A different notion of many ===

'''Tuples''' are another way of storing multiple values in a single value. There are two key differences between tuples and lists:

* They have a ''fixed'' number of elements (''immutable''), and so you can't cons to a tuple. Therefore, it makes sense to use tuples when you ''know'' in advance how many values are to be stored. For example, we might want a type for storing 2D coordinates of a point. We know exactly how many values we need for each point (two - the ''x'' and ''y'' coordinates), so tuples are applicable.

* The elements of a tuple do not need to be all of the same type. For instance, in a phonebook application we might want to handle the entries by crunching three values into one: the name, phone number, and the address of each person. In such a case, the three values won't have the same type, so lists wouldn't help, but tuples would.

Tuples are created within parentheses with elements delimited by commas. Let's look at some sample tuples:

{{HaskellExample|1=Some tuples|2=<source lang="haskell">
(True, 1)
("Hello world", False)
(4, 5, "Six", True, 'b')
</source>}}

The first example is a tuple containing two elements: the first one is True, and the second is 1. The next example again has two elements: z the first is "Hello world", and the second is False. The third example is a tuple consisting of ''five'' elements: the first is 4 (a number), the second is 5 (another number), the third is "Six" (a string), the fourth is True (a boolean value), and the fifth is 'b' (a character).

A quick note on nomenclature: In general you use ''n-tuple'' to denote a tuple of size ''n''. 2-tuples (that is, tuples with 2 elements) are normally called ''pairs'' and 3-tuples ''triples''. Tuples of greater sizes aren't actually all that common, but if you were to logically extend the naming system, you'd have ''quadruples'', ''quintuples'' and so on, hence the general term ''tuple''.

{{Exercises|
# Write down the 3-tuple whose first element is 4, second element is "hello" and third element is True.
# Which of the following are valid tuples ?
## <code>(4, 4)</code>
## <code>(4, "hello")</code>
## <code>(True, "Blah", "foo")</code>
# Lists can be built by consing new elements onto them: you cons a number onto a list of numbers, and get back a list of numbers. It turns out that there is no such way to build up tuples.
## Why do you think that is?
## Say for the sake of argument, that there was such a function. What would you get if you "consed" something on a tuple?
}}


Tuples are also handy when you want to return more than one value from a function. In most languages trying to return two or more things at once means wrapping them up in a special data structure, maybe one that only gets used in that function. In Haskell, you have the alternative of just returning them as a tuple<ref>An alternative interpretation of that would be that tuples ''are'', indeed, as a primitive kind of data structure. But we digress.</ref>.

=== Getting data out of tuples ===
In this section, we focus on pairs for simplicity's sake, but also because they are far and away the most commonly used size of tuple.

Okay, so we've seen how we can put values into tuples, simply by using the <code>(x, y)</code> syntax. How can we get them out again? For example, a typical use of pairs is to store the (''x'', ''y'') coordinates of a point: imagine you have a chess board, and want to specify a specific square. You could do this by labeling all the rows from 1 to 8, and similarly with the columns, then letting, say, (2, 5) represent the square in row 2 and column 5. Say we want to define a function for finding all the pieces in a given row. One way of doing this would be to find the coordinates of all the pieces, then look at the row part and see whether it's equal to whatever row we're being asked to examine. This function would need, once it had the coordinate pair <code>(x, y)</code> of a piece, to extract the <code>x</code> (the row part). To do that there are two functions, <code>fst</code> and <code>snd</code>, that retrieve<ref>Or, more technically,  which ''project''. In math-speak, a function that gets some data out of a structure is called a projection.</ref> the first and second elements out of a pair, respectively. Let's see some examples:

{{HaskellGHCiExample|1=Using <code>fst</code> and <code>snd</code>|2=
Prelude> fst (2, 5)
2
Prelude> fst (True, "boo")
True
Prelude> snd (5, "Hello")
"Hello"
}}

Note that these functions ''only'' work on pairs. Why? Yet again, it has to do with types. Pairs and triples (and quadruples, etc.) have necessarily different types, and <code>fst</code> and <code>snd</code> only accepts pairs as arguments.

{{Exercises|
# Use a combination of <code>fst</code> and <code>snd</code> to extract the 4 from the tuple <code>(("Hello", 4), True)</code>.
# Normal chess notation is somewhat different to ours: it numbers the rows from 1-8 and the columns a-h; and the column label is customarily given first. Could we label a specific point with a character and a number, like <code>('a', 4)</code>? What important difference with lists does this illustrate?
}}

<!-- This must have its own section -->
The general technique for extracting a component out of a tuple is pattern matching (yep, we'll talk about it in great detail later on, as this is one of the distinctive features of functional languages). Without making things more complicated than they ought to be at this point, let me just show you how I can write a function named <code>first</code> that does the same thing as <code>fst</code>:

{{HaskellGHCiExample|1=The definition of <code>first</code>|2=
Prelude> let first (x, y) = x
Prelude> first (3, True)
3
}}

This just says that given a pair <code>(x, y)</code>, <code>first (x, y)</code> gives you <code>x</code>.

=== Tuples within tuples (and other combinations) ===
We can apply the same reasoning to tuples about storing lists within lists. Tuples are things too, so you can store tuples with tuples (within tuples up to any arbitrary level of complexity). Likewise, you could also have lists of tuples, tuples of lists, all sorts of combinations along the same lines.

{{HaskellExample|1=Nesting tuples and lists|2=<source lang="haskell">
((2,3), True)
((2,3), [2,3])
[(1,2), (3,4), (5,6)]
</source>}}

<!-- ''Some discussion about this - what you get out of this, maybe, what's the big idea behind grouping things together'' -->
There is one bit of trickiness to watch out for, however. The type of a tuple is defined not only by its size, but by the types of objects it contains. For example, the tuples <code>("Hello",32)</code> and <code>(47,"World")</code> are fundamentally different. One is of type <code>(String,Int)</code>, whereas the other is <code>(Int,String)</code>. This has implications for building up lists of tuples. We could very well have lists like <code>[("a",1),("b",9),("c",9)]</code>, but having a list like <code>[("a",1),(2,"b"),(9,"c")]</code> is right out. Can you spot the difference?

{{Exercises|1=
# Which of these are valid Haskell, and why?
#* <code>fst [1,2]</code>
#* <code>1:(2,3)</code>
#* <code>(2,4):(2,3)</code>
#* <code>(2,4):[]</code>
#* <code>[(2,4),(5,5),('a','b')]</code>
#* <code>([2,4],[2,2])</code>
}}

== Polymorphic types ==

As you may have found out already by playing with <tt>:t</tt>, the type of a list depends on the types of its elements, and is denoted by enclosing it in square brackets:

{{HaskellGHCi|1=
Prelude>:t [True, False]
[True, False] :: [Bool]
Prelude>:t ["hey", "my"]
["hey", "my"] :: [[Char]]
}}

Therefore, lists of <tt>Bool</tt> have different types than lists of <tt>[Char]</tt> (that is, strings), of <tt>Int</tt> and so on. Since we have seen that functions only accept arguments of the types specified in the type of the function, that leads to some practical complication. For example, imagine a function that finds the length of a list. But since <tt>[Int]</tt>, <tt>[Bool]</tt> and <tt>[String]</tt> are different types it seems we would need separate functions for each case - <code>lengthInts :: [Int] -> Int</code>, as well as a <code>lengthBools :: [Bool] -> Int</code>, as well as a <code>lengthStrings :: [String] -> Int</code>, as well as a...

That would be horribly annoying, and frustrating too, because intuitively it would seem that counting how many things there are in a list should be independent of what the things actually are. Fortunately, it does not work like that: there is a single function <tt>length</tt>, which works on all lists. But how can that possibly work? As usual, checking the type of <tt>length</tt> provides a good hint that there is something different going on...

{{HaskellGHCiExample|1=Our first polymorphic type|2=
Prelude>:t length
length :: [a] -> Int
}}

{{Side note|side=right|We'll look at the theory behind polymorphism in much more detail later in the course.}} The <tt>a</tt> in the square brackets is ''not'' a type - remember that type names always start with uppercase letters. Instead, it is a '''type variable'''. When Haskell sees a type variable, it allows any type to take its place. This is exactly what we want. In type theory (a branch of mathematics), this is called ''polymorphism'': functions or values with only a single type are called ''monomorphic'', and things that use type variables to admit more than one type are ''polymorphic''.

It is important to note that, in a single type signature, all cases of a certain type variable must be of the same type. For example,

<source lang = "haskell">
f :: a -> a
</source>

means that <tt>f</tt> takes an argument of any type and gives something of the ''same type as the argument'', as opposed to

<source lang = "haskell">
f :: a -> b
</source>

which means that <tt>f</tt> takes an argument of any type and gives an argument of another type, which is ''not necessarily the same type, but can be''.

=== Example: <code>fst</code> and <code>snd</code> ===

As we saw, you can use the <code>fst</code> and <code>snd</code> functions to extract parts of pairs. By this time you should already be developing the habit of wondering "what type is that function?" about every function you come across. Let's consider the cases of <code>fst</code> and <code>snd</code>
. These two functions take a pair as their argument and return one element of this pair. First of all, the type of a pair depends on the type of its elements, just as with lists, so the functions need to be polymorphic. Also it is important to keep in mind that pairs, and tuples in general, don't have to be homogeneous with respect to types; their different parts can be different types. So if we were to say:

<source lang="haskell">fst :: (a, a) -> a</source>

That would mean <tt>fst</tt> would only work if the first and second part of the pair given as input had the same type. So what is the correct type? Simply:

{{HaskellExample|1=The types of <code>fst</code> and <code>snd</code>|2=<source lang="haskell">
fst :: (a, b) -> a
snd :: (a, b) -> b
</source>}}

<tt>b</tt> is a second type variable, which stands for a type which may or may not be equal to the one which replaces <tt>a</tt>.

Note that if you knew nothing about <tt>fst</tt> and <tt>snd</tt> other than the type signatures you might guess that they return the first and second parts of a pair, respectively. Although that is correct, it is not necessarily true as all the signatures say is that they just have to return something ''with the same type'' of the first and second parts of the pair.

== Summary ==

We have introduced two new notions in this chapter, lists and tuples. To sum up:

# Lists are defined by square brackets and commas : <code>[1,2,3]</code>.
#* They can contain ''anything'' as long as all the candidate elements of the list are of the same type
#* They can also be built by the cons operator, <code>(:)</code>, but you can only cons things onto lists
# Tuples are defined by parentheses and commas : <code>("Bob",32)</code>
#* They can contain ''anything'', even things of different types
#* Their length is encoded in their type. That is, two tuples with different lengths will have different types.
# Lists and tuples can be combined in any number of ways: lists within lists, tuples with lists, etc, but their criteria must still be fulfilled for the combinations to be valid.

== Notes ==
<references/>

{{Haskell navigation|chapter=Haskell Basics}}


{{Auto category}}

[[pt:Haskell/Listas e n-uplas]]
