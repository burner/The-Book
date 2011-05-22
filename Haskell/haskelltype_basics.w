>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Haskell Basics}}

'''Types''' in programming are a way of grouping similar values into categories. In Haskell, the type system is a powerful way of ensuring there are fewer mistakes in your code.

== Introduction ==
Programming deals with different sorts of entities. For example, consider adding two numbers together:

<math>2 + 3</math>

What are 2 and 3? We can quite simply describe them as numbers. And what about the plus sign in the middle? That's certainly not a number, but it stands for an operation which we can do with two numbers - namely, addition.

Similarly, consider a program that asks you for your name and then greets you with a "Hello" message. Neither your name nor the word Hello are numbers. What are they then? We might refer to all words and sentences and so forth as text. In fact, it's more normal in programming to use a slightly more esoteric word: ''String''.

{{side note|In Haskell, the rule is that all type names have to begin with a capital letter. We shall adhere to this convention henceforth.}}

Databases illustrate clearly the concept of types. For example, say we had a table in a database to store details about a person's contacts; a kind of personal telephone book. The contents might look like this:

{|class="wikitable"
| First Name
| Last Name
| Telephone number
| Address
|- 
| Sherlock
| Holmes
| 743756
| 221B Baker Street London
|-
| Bob
| Jones
| 655523
| 99 Long Road Street Villestown
|}

The fields in each entry contain values. <tt>Sherlock</tt> is a value as is <tt>99 Long Road Street Villestown</tt> as well as <tt>655523</tt>. As we've said, types are a way of categorizing data, so let us see how we could classify the values in this example. The first three fields seem straightforward enough. "First Name" and "Last Name" contain text, so we say that the values are of type String, while "Telephone Number" is clearly a number.

At first glance one may be tempted to classify address as a String. However, the semantics behind an innocent address are quite complex. There are a whole lot of human conventions that dictate how we interpret it. For example, if the beginning of the address text contains a number it is likely the number of the house. If not, then it's probably the name of the house - except if it starts with "PO Box", in which case it's just a postal box address and doesn't indicate where the person lives at all.

Clearly, there's more going on here than just text, as each part of the address has its own meaning. In principle there is nothing wrong with saying addresses are Strings, but when we describe something as a String all that we are saying is that it is a sequence of letters, numbers, etc. Claiming they're of some more specialized type, say, Address, is far more meaningful. If we know something is an Address, we instantly know much more about the piece of data - for instance, that we can interpret it using the "human conventions" that give meaning to addresses.

In retrospect, we might also apply this rationale to the telephone numbers. It could be a good idea to speak in terms of a TelephoneNumber type. Then, if we were to come across some arbitrary sequence of digits which happened to be of type TelephoneNumber we would have access to a lot more information than if it were just a Number - for instance, we could start looking for things such as area and country codes on the initial digits.

Another reason to not consider the telephone numbers as just Numbers is that doing arithmetics with them makes no sense. What is the meaning and expected effect of, say, adding 1 to a TelephoneNumber? It would not allow calling anyone by phone. That's a good reason for using a more specialized type than Number. Also, each digit comprising a telephone number is important; it's not acceptable to lose some of them by rounding it or even by omitting leading zeroes.

=== Why types are useful ===

So far, it seems that all what we've done was to describe and categorize things, and it may not be obvious why all of this talk would be so important for writing actual programs. Starting with this module, we will explore how Haskell uses types to the programmer's benefit, allowing us to incorporate the semantics behind, say, an address or a telephone number seamlessly in the code.

== Using the interactive <code>:type</code> command ==

The best way to explore how types work in Haskell is from GHCi. The type of any expression can be checked with the immensely useful <code>:type</code> (or <code>:t</code>) command. Let us test it on the boolean values from the previous module:

{{HaskellGHCiExample|1=Exploring the types of boolean values in GHCi|2=
Prelude> :type True
True :: Bool
Prelude> :type False
False :: Bool
Prelude> :t (3 < 5)
(3 < 5) :: Bool
}}

Usage of <tt>:type</tt> is straightforward: enter the command into the prompt followed by whatever you want to find the type of. On the third example, we use <tt>:t</tt>, which we will be using from now on. GHCi will then print the type of the expression. The symbol <tt>::</tt>, which will appear in a couple other places, can be read as simply "is of type", and indicates a ''type signature''.

<tt>:type</tt> reveals that truth values in Haskell are of type <tt>Bool</tt>, as illustrated above for the two possible values, <tt>True</tt> and <tt>False</tt>, as well as for a sample expression that will evaluate to one of them. It is worthy to note at this point that boolean values are not just for value comparisons. <tt>Bool</tt> captures in a very simple way the semantics of a yes/no answer, and so it can be useful to represent any information of such kind - say, whether a name was found in a spreadsheet, or whether a user has toggled an on/off option.

=== Characters and strings ===

Now let us try <tt>:t</tt> on something new. Literal characters are entered by enclosing them with single quotation marks. For instance, this is the single letter H:

{{HaskellGHCiExample|1=Using the :type command in GHCi on a literal character|2=
Prelude> :t 'H'
'H' :: Char
}}

Literal character values, then, have type <tt>Char</tt>. Single quotation marks, however, only work for individual characters. If we need to enter actual text - that is, a string of characters - we use ''double'' quotation marks instead:

{{HaskellGHCiExample|1=Using the :t command in GHCi on a literal string|2=
Prelude> :t "Hello World"
"Hello World" :: [Char]
}}

Seeing this output, a pertinent question would be "why did we get <tt>Char</tt>" again? The difference is in the square brackets. <tt>[Char]</tt> means a number of characters chained together, forming a ''list''. That is what text strings are in Haskell - lists of characters.<ref>Lists, be they of characters or of other things, are very important entities in Haskell, and we will cover them in more detail in a little while.</ref>

{{Exercises|1=
# Try using <code>:type</code> on the literal value <code>"H"</code> (notice the double quotes). What happens? Why?
# Try using <code>:type</code> on the literal value <code>'Hello World'</code> (notice the single quotes). What happens? Why?}}

A nice thing to be aware of is that Haskell allows for ''type synonyms'', which work pretty much like synonyms in human languages (words that mean the same thing - say, 'fast' and 'quick'). In Haskell, type synonyms are alternate names for types. For instance, <tt>String</tt> is defined as a synonym of <tt>[Char]</tt>, and so we can freely substitute one with the other. Therefore, to say:

<source lang="haskell">
"Hello World" :: String
</source>

is also perfectly valid, and in many cases a lot more readable. From here on we'll mostly refer to text values as <tt>String</tt>, rather than <tt>[Char]</tt>.

== Functional types ==
So far, we have seen how values (strings, booleans, characters, etc.) have types and how these types help us to categorize and describe them. Now, the big twist, and what makes the Haskell's type system truly powerful: Not only values, but ''functions'' have types as well<ref>The deeper truth is that functions ''are'' values, just like all the others.</ref>.<!-- This stays as just a footnote for the moment --> Let's look at some examples to see how that works.

=== Example: <code>not</code> ===

Consider <tt>not</tt>, that negates boolean values (changing <tt>True</tt> to false and vice-versa). To figure out the type of a function we consider two things: the type of values it takes as its input and the type of value it returns. In this example, things are easy. <tt>not</tt> takes a <tt>Bool</tt> (the <tt>Bool</tt> to be negated), and returns a <tt>Bool</tt> (the negated <tt>Bool</tt>). The notation for writing that down is:

{{HaskellExample|1=Type signature for <code>not</code>|2=<source lang="haskell">
not :: Bool -> Bool
</source>}}

You can read this as "<code>not</code> is a function from things of type <tt>Bool</tt> to things of type <tt>Bool</tt>". In case you are wondering about using <tt>:t</tt> on a function...

{{HaskellGHCi|1=
Prelude> :t not
not :: Bool -> Bool
}}

... it will work just as expected. This description of the type of a function in terms of the types of argument(s), and it shows that functions, ''being values in Haskell'', also have type signatures.

=== Example: <code>chr</code> and <code>ord</code> ===
Text presents a problem to computers. Once everything is reduced to its lowest level, all a computer knows how to deal with are 1s and 0s: computers work in binary. As working with binary numbers isn't at all convenient, humans have come up with ways of making computers store text. Every character is first converted to a number, then that number is converted to binary and stored. Hence, a piece of text, which is just a sequence of characters, can be encoded into binary. Normally, we're only interested in how to encode characters into their numerical representations, because the computer generally takes care of the conversion to binary numbers without our intervention.

The easiest way of converting characters to numbers is simply to write all the possible characters down, then number them. For example, we might decide that 'a' corresponds to 1, then 'b' to 2, and so on. This is exactly what a thing called the ASCII standard is: 128 of the most commonly-used characters, numbered. Of course, it would be a bore to sit down and look up a character in a big lookup table every time we wanted to encode it, so we've got two functions that can do it for us, <code>chr</code> (pronounced 'char') and <code>ord</code> <ref>This isn't quite what <code>chr</code> and <code>ord</code> do, but that description fits our purposes well, and it's close enough.</ref>:

{{HaskellExample|1=Type signatures for <code>chr</code> and <code>ord</code>|2=<source lang="haskell">
chr :: Int  -> Char
ord :: Char -> Int
</source>}}

We already know what <tt>Char</tt> means. The new type on the signatures above, <tt>Int</tt>, amounts to integer numbers, and is one of quite a few different types of numbers. <ref>In fact, it is not even the only type for integers! We will meet its relatives in a short while.</ref> The type signature of <tt>chr</tt> tells us that it takes an argument of type <tt>Int</tt>, an integer number, and evaluates to a result of type Char. The converse is the case with <tt>ord</tt>: It takes things of type Char and returns things of type Int. With the info from the type signatures, it becomes immediately clear which of the functions encodes a character into a numeric code (<tt>ord</tt>) and which does the decoding back to a character (<tt>chr</tt>).

To make things more concrete, here are a few examples of function calls to <code>chr</code> and <code>ord</code>. Notice that the two functions aren't available by default; so before trying them in GHCi you need to use the <tt>:module Data.Char</tt> (or <tt>:m Data.Char</tt>) command to load the Data.Char module, where they are defined.

{{HaskellGHCiExample|1=Function calls to <code>chr</code> and <code>ord</code>|2=
Prelude> :m Data.Char
Prelude Data.Char> chr 97
'a'
Prelude Data.Char> chr 98
'b'
Prelude Data.Char> ord 'c'
99
}}

=== Functions in more than one argument ===

The style of type signatures we have been using works fine enough for functions of one argument. But what would be the type of a function like this one?

{{HaskellExample|1=A function with more than one argument|2=<source lang="haskell">
f x y = x + 5 + 2 * y
</source>}}

(As we mentioned above, there's more than one type for numbers, but for now we're going to cheat and pretend that <code>x</code> and <code>y</code> have to be <tt>Int</tt>s.)

The general technique for forming the type of a function that accepts more than one argument is simply to write down all the types of the arguments in a row, in order (so in this case <code>x</code> first then <code>y</code>), then link them all with <code>-></code>. Finally, add the type of the result to the end of the row and stick a final <code>-></code> in just before it.<ref>This method might seem just a trivial hack by now, but actually there are very deep reasons behind it, which we'll cover in the chapter on [[../Higher-order functions and Currying|Currying]].</ref> In this example, we have:
 
{{HaskellExample|1=The type signature of a function with more than one argument|2=<source lang="haskell">
f :: (Num a) => a -> a -> a
</source>}} 

<!-- There's a problem with using continuing list numbering, <pre>, and wiki syntax all together -->
<ol>
<li> 
Write down the types of the arguments. We've already said that <code>x</code> and <code>y</code> have to be Ints, so it becomes: 
<pre>
Int                   Int
^^ x is an Int        ^^ y is an Int as well
</pre>
</li>
<li>Fill in the gaps with <code>-></code>:
<pre>Int -> Int</pre>
</li>
<li>Add in the result type and a final <code>-></code>. In our case, we're just doing some basic arithmetic so the result remains an Int.
<pre>
Int -> Int -> Int
              ^^ We're returning an Int
           ^^ There's the extra -> that got added in </pre>
</li>
</ol>

=== Real-World Example: <code>openWindow</code> ===
{{Side note|side=right|A library is a collection of common code used by many programs.}} As you'll learn in the Practical Haskell section of the course, one popular group of Haskell libraries are the GUI ('''G'''raphical '''U'''ser '''I'''nterface) ones. These provide functions for dealing with all the parts of Windows, Linux, or Mac OS you're familiar with: opening and closing application windows, moving the mouse around, etc. One of the functions from one of these libraries is called <code>openWindow</code>, and you can use it to open a new window in your application. For example, say you're writing a word processor, and the user has clicked on the 'Options' button. You need to open a new window which contains all the options that they can change. Let's look at the type signature for this function <ref>This has been somewhat simplified to fit our purposes. Don't worry, the essence of the function is there.</ref>:

{{HaskellExample|1=<code>openWindow</code>|2=<source lang="haskell">
openWindow :: WindowTitle -> WindowSize -> Window
</source>}}

Don't panic! Here are a few more types you haven't come across yet. But don't worry, they're quite simple. All three of the types there, <tt>WindowTitle</tt>, <tt>WindowSize</tt> and <tt>Window</tt> are defined by the GUI library that provides <code>openWindow</code>. As we saw when constructing the types above, because there are two arrows, the first two types are the types of the parameters, and the last is the type of the result. <tt>WindowTitle</tt> holds the title of the window (what appears in the bar at the very top of the window, left of the close/minimize/etc. buttons), and <tt>WindowSize</tt> specifies how big the window should be. The function then returns a value of type <tt>Window</tt> which represents the actual window.

One key point illustrated by this example, as well as the <tt>chr</tt>/<tt>ord</tt> one is that, even if you have never seen the function or don't know how it actually works, a type signature can immediately give you a good general idea of what the function is supposed to do. For that reason, a very useful habit to acquire is testing every new function you meet with <tt>:t</tt>. If you start doing so right now you'll not only learn about the standard library Haskell functions quite a bit quicker but also develop a useful kind of intuition. Curiosity pays off :)

{{Exercises|
Finding types for functions is a basic Haskell skill that you should become very familiar with. What are the types of the following functions?

# The <code>negate</code> function, which takes an Int and returns that Int with its sign swapped. For example, <code><nowiki>negate 4 = -4</nowiki></code>, and <code><nowiki>negate (-2) = 2</nowiki></code> <!-- work around buggy MediaWiki parsing -->
# The <code>(&&)</code> function, pronounced 'and', that takes two Bools and returns a third Bool which is True if both the arguments were, and False otherwise.
# The <code>({{!}}{{!}})</code> function, pronounced 'or', that takes two Bools and returns a third Bool which is True if either of the arguments were, and False otherwise. <!-- more MediaWiki badness -->

For any functions hereafter involving numbers, you can just assume the numbers are Ints.

# <code><nowiki>f x y = not x && y</nowiki></code>
# <code><nowiki>g x = (2*x - 1)^2</nowiki></code>
# <code><nowiki>h x y z = chr (x - 2)</nowiki></code> }}

== Type signatures in code ==

Now we've explored the basic theory behind types as well as how they apply to Haskell. The key way in which type signatures are used is for ''annotating'' functions. For an applied example, let us have a look at a very small ''module''. (If you don't remember them from earlier, modules in Haskell are just a neat way of making a library by packing functions for usage in other programs. No need to worry with details now.)

{{HaskellExample|1=Module without type signatures|2=<source lang="haskell">
module StringManip where

import Data.Char

uppercase = map toUpper
lowercase = map toLower
capitalise x =
  let capWord []     = []
      capWord (x:xs) = toUpper x : xs
  in unwords (map capWord (words x))
</source>}}

This tiny library provides three handy string manipulation functions. <code>uppercase</code> converts a string to uppercase, <code>lowercase</code> to lowercase, and <code>capitalize</code> capitalizes the first letter of every word. Each of these functions takes a <tt>String</tt> as argument and evaluates to another <tt>String</tt>. For now, don't worry with how the code actually works, as it is obviously full of features we haven't discussed yet. Anyway - and especially because we don't understand the implementation yet - stating the type for these functions explicitly would make their roles more obvious. Most Haskellers would write the above module using type signatures, like this:

{{HaskellExample|1=Module with type signatures|2=<source lang="haskell">
module StringManip where

import Data.Char

uppercase, lowercase :: String -> String
uppercase = map toUpper
lowercase = map toLower

capitalise :: String -> String
capitalise x =
  let capWord []     = []
      capWord (x:xs) = toUpper x : xs
  in unwords (map capWord (words x))
</source>}}

The added signatures inform the type of the functions to the human readers and, crucially, ''to the compiler/interpreter as well''. Note that you can write a single type signature if the two or more functions share the same type (like <code>uppercase</code> and <code>lowercase</code>) by delimiting their names with commas.

=== Type inference ===
We just said that type signatures tell the interpreter (or compiler) what the function type is. However, up to this point you wrote perfectly good Haskell code without any signatures, and it has been accepted by GHC/GHCi. That shows that it is not strictly mandatory to add type signatures. But that doesn't mean that if you don't add them Haskell simply forgets about types altogether! Instead, when you didn't tell Haskell the types of your functions and variables it ''figured them out'' through a process called ''type inference''. In essence, the compiler performs inference by starting with the types of things it knows and then working out the types of the rest of the things. Let's see how that works with a general example.

{{HaskellExample|1=Simple type inference|2=<source lang="haskell">
-- We're deliberately not providing a type signature for this function
isL c = c == 'l'
</source>}}

<tt>isL</tt> is a function that takes an argument <tt>c</tt> and returns the result of evaluating <tt>c == 'l'</tt>. If we don't provide a type signature the compiler, in principle, does not know the type of <tt>c</tt>, nor the type of the result. In the expression <tt>c == 'l'</tt>, however, it does know that <tt>'l'</tt> is a <tt>Char</tt>. Since <tt>c</tt> and <tt>'l'</tt> are being compared with equality with <tt>(==)</tt> and both arguments of <tt>(==)</tt> must have the same type<ref>As we discussed in "Truth values". That fact is actually stated by the type signature of <tt>(==)</tt> - if you are curious you can check it, although you will have to wait a little bit more for a full explanation of the notation used in it.</ref>, it follows that <tt>c</tt> must be a <tt>Char</tt>. Finally, since <tt>isL c</tt> is the result of <tt>(==)</tt> it must be a <tt>Bool</tt>. And thus we have a signature for the function:

{{HaskellExample|1=<code>isL</code> with a type|2=<source lang="haskell">
isL :: Char -> Bool
isL c = c == 'l'
</source>}}

And, indeed, if you leave out the type signature the Haskell compiler will discover it through this process. You can verify that by using <tt>:t</tt> on <tt>isL</tt> with or without a signature.

So if type signatures are optional why bother with them at all? Here are a few reasons:

* '''Documentation''': type signatures make your code easier to read. With most functions, the name of the function along with the type of the function is sufficient to guess what the function does. Of course, you should always comment your code properly too, but having the types clearly stated helps a lot, too.<!-- add a discreet note on Haddock later -->
* '''Debugging''': if you annotate a function with a type signature and then make a typo in the body of the function which changes the type of a variable the compiler will tell you, ''at compile-time'', that your function is wrong. Leaving off the type signature could have the effect of allowing your function to compile, and the compiler would assign it an erroneous type. You wouldn't know until you ran your program that it was wrong. In fact, since this is such a crucial point we will explore it a bit further.

=== Types prevent errors ===

The role of types in preventing errors is central to typed languages. When passing expressions around you have to make sure the types match up like they did here. If they don't, you'll get ''type errors'' when you try to compile; your program won't pass the ''typecheck''. This is really how types help you to keep your programs bug-free. To take a very trivial example:

{{HaskellExample|1=A non-typechecking program|2=<source lang="haskell">
"hello" + " world"
</source>}}

Having that line as part of your program will make it fail to compile, because you can't add two strings together! In all likelihood the intention was to use the similar-looking string concatenation operator, which joins two strings together into a single one:

{{HaskellExample|1=Our erroneous program, fixed|2=<source lang="haskell">
"hello" ++ " world"
</source>}}

An easy typo to make, but because you use Haskell, it was caught when you tried to compile. You didn't have to wait until you ran the program for the bug to become apparent.

This was only a simple example. However, the idea of types being a system to catch mistakes works on a much larger scale too. In general, when you make a change to your program, you'll change the type of one of the elements. If this change isn't something that you intended, or has unforeseen consequences, then it will show up immediately. A lot of Haskell programmers remark that once they have fixed all the type errors in their programs, and their programs compile, that they tend to "just work": run flawlessly at the first time, with only minor problems. ''Run-time errors'', where your program goes wrong when you run it rather than when you compile it, are much rarer in Haskell than in other languages. This is a huge advantage of having a strong type system like Haskell does.

== Notes ==
<references/>

{{Haskell navigation|chapter=Haskell Basics}}

{{Auto category}}
