>﻿{{Haskell minitoc|chapter=Elementary Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Elementary Haskell}}
You're not restricted to working with just the types provided by default with the language. Haskell allows you to define new types. Reasons for doing so include that

* It allows for code to be written in terms of the problem being solved, making programs easier to design, write and understand. 
* It allows for handling related pieces of data together in ways more convenient and meaningful than say, simply putting and getting values from lists or tuples. 
* It makes it possible to use two powerful features of Haskell, pattern matching and the type system, to their fullest extent by making them work with your custom types. 

How these things are achieved and why they are so important will gradually become clear as you progress on this course. 

Haskell has three basic ways to declare a new type:

* The <tt>data</tt> declaration, which defines new data types.
* The <tt>type</tt> declaration for type synonyms.
* The <tt>newtype</tt> declaration, which is a cross between the other two.

In this chapter, we will discuss the most essential way, <code>data</code>. We'll also mention <code>type</code>, which is a convenience feature.  You'll find out about <code>newtype</code> later on, but don't worry too much about it; it's there mainly for optimisation.

== <code>data</code> and constructor functions ==

<tt>data</tt> is used to define new data types using existing ones as building blocks. Here's a data structure for elements in a simple list of anniversaries:

<source lang="haskell">
data Anniversary = Birthday String Int Int Int       -- name, year, month, day
                 | Wedding String String Int Int Int -- spouse name 1, spouse name 2, year, month, day
</source>

A type declaration like this has two effects:

*  First, it declares a new data type <tt>Anniversary</tt>, which can be either a Birthday or a Wedding.  A Birthday ''contains'' one string and three integers and a Wedding ''contains'' two strings and three integers. The definitions of the two possibilities are separated by the vertical bar. The text after the "--" are just comments, explaining to readers of the code what the fields actually mean. 

* Moreover, it defines two ''constructor functions'' for <tt>Anniversary</tt>, called, appropriately enough, <tt>Birthday</tt> and <tt>Wedding</tt>. These functions provide a way to build a new <tt>Anniversary</tt>, be it a "Birthday" or a "Wedding".

Types defined by <tt>data</tt> declarations are often referred to as ''algebraic data types''. We will eventually return to this topic to address the theory behind such a name and the practical implications of said theory.

As usual with Haskell the case of the first letter is important: type names and constructor functions must always start with capital letters. Other than this syntactic detail, constructor functions work pretty much like the "conventional" functions we met so far. In fact, if you use <tt>:t</tt> in ghci to query the type of, say, <tt>Birthday</tt>, you'll get:

{{HaskellGHCi|1=
*Main> :t Birthday
Birthday :: String -> Int -> Int -> Int -> Anniversary
}}

Meaning it's just a function which takes one String and three Int as arguments and ''evaluates to'' an <tt>Anniversary</tt>. This anniversary will contain the four arguments we passed as specified by the <tt>Birthday</tt> constructor.

Calling the constructor functions is no different from calling other functions. For example, suppose we have John Smith born on 3rd July 1968:

<source lang="haskell">
johnSmith :: Anniversary
johnSmith = Birthday "John Smith" 1968 7 3
</source>

He married Jane Smith on 4th March 1987:

<source lang="haskell">
smithWedding :: Anniversary
smithWedding = Wedding "John Smith" "Jane Smith" 1987 3 4
</source>

These two anniversaries can, for instance, be put in a list:

<source lang="haskell">
anniversariesOfJohnSmith :: [Anniversary]
anniversariesOfJohnSmith = [johnSmith, smithWedding]
</source>

Or you could just as easily have called the constructors straight away when building the list (although the resulting code looks a bit cluttered).

<source lang="haskell">
anniversariesOfJohnSmith = [Birthday "John Smith" 1968 7 3, Wedding "John Smith" "Jane Smith" 1987 3 4]
</source>

== Deconstructing types ==

So far we've seen how to define a new type and create values of that type. If the new data types are to be of any use, however, we must have a way to access its contents, that is, the values we used at construction. For instance, one very basic operation with the anniversaries defined above would be extracting the names and dates they contain as a String. Let us see how a function which does that, <tt>showAnniversary</tt>, could be written and analyse what's going on in it (you'll see that, in the sake of code clarity, we used an auxiliary <tt>showDate</tt> function but let's ignore it for a moment and focus on <tt>showAnniversary</tt>).

<source lang="haskell">
showDate :: Int -> Int -> Int -> String
showDate y m d = show y ++ "-" ++ show m ++ "-" ++ show d

showAnniversary :: Anniversary -> String

showAnniversary (Birthday name year month day) =
   name ++ " born " ++ showDate year month day

showAnniversary (Wedding name1 name2 year month day) =
   name1 ++ " married " ++ name2 ++ " on " ++ showDate year month day
</source>

This example shows what is truly special about constructor functions: they can also be used to deconstruct the values they build.  <tt>showAnniversary</tt> takes a single argument of type <tt>Anniversary</tt>. Instead of just providing a name for the argument on the left side of the definition, however, we specify one of the constructor functions and give names to each argument of the constructor - which correspond to the contents of the Anniversary. A more formal way of describing this "giving names" process is to say we are ''binding variables'' - "binding" is being used in the sense of assigning a variable to each of the values so that we can refer to them on the right side of the function definition.

For such an approach be able to handle both "Birthday" and "Wedding" Anniversaries we needed to provide ''two'' function definitions, one for each constructor. When <tt>showAnniversary</tt> is called, if the argument is a <tt>Birthday</tt> Anniversary the first version is used and the variables <tt>name</tt>, <tt>month</tt>, <tt>date</tt> and <tt>year</tt> are bound to its contents. If the argument is a <tt>Wedding</tt> Anniversary then the second version is used and the variables are bound in the same way. This process of using a different version of the function depending on the type of constructor used to build the <tt>Anniversary</tt> is pretty much like what happens when we use a <tt>case</tt> statement or define a function piece-wise.

An important observation on syntax is that the parentheses around the constructor name and the bound variables are mandatory; otherwise the compiler or interpreter would not take them as a single argument. Also, it is important to have it absolutely clear that the expression inside the parentheses is ''not'' a call to the constructor function, even though it may look just like one.

{{Exercises|1=
''Note: There are spoilers for this exercise near the end of the module, so we recommend that you attempt it before getting there.''</br>
Reread the function definitions above, now having a closer look at the <tt>showDate</tt> helper function. In spite of us saying it was provided "in the sake of code clarity", there is a certain clumsiness in the way it is used. You have to pass three separate Int arguments to it, but these arguments are always linked to each other in that they are part of a single date. It would make no sense to do things like passing the year, month and day values of the Anniversary in a different order, or to pass the month value twice and omit the day.
* Could we use what we've seen in this chapter so far to reduce this clumsiness?
* Declare a <tt>Date</tt> type which is composed of three Int, corresponding to year, month and date. Then, rewrite <tt>showDate</tt> so that it uses the new data type, and picture the changes needed in <tt>showAnniversary</tt> and the <tt>Anniversary</tt> for them to make use of it as well.
}}

== <code>type</code> for making type synonyms ==

As mentioned in the introduction of this module, code clarity is one of the motivations for using custom types. In that spirit, it could be nice to make it clear that the Strings in the Anniversary type are being used as names while still being able to manipulate them like ordinary Strings. The <tt>type</tt> declaration allows us to do that.

<source lang="haskell">
type Name = String
</source>

The code above says that a <tt>Name</tt> is now a synonym for a <tt>String</tt>.  Any function that takes a <tt>String</tt> will now take a <tt>Name</tt> as well (and vice-versa: functions that take <tt>Name</tt> will accept any <tt>String</tt>). The right hand side of a <tt>type</tt> declaration can be a more complex type as well. For example, <tt>String</tt> itself is defined in the standard libraries as

<source lang="haskell">
type String = [Char]
</source>

We can do something similar for the list of anniversaries we made use of:

<source lang="haskell">
type AnniversaryBook = [Anniversary]
</source>

Since type synonyms are mostly a convenience feature for making the roles of types clearer or providing an alias to, for instance, a complicated list or tuple type, it is largely a matter of personal discretion to decide how they should be deployed. Abuse of synonyms might even, on occasion, make code confusing (for instance, picture a long program using multiple names for common types like Int or String simultaneously, all of course having the same functionality).

Incorporating the suggested type synonyms and the <tt>Date</tt> type we proposed in the exercise(*) of the previous section the code we've written so far looks like this:
</br>
</br>
</br>
((*) '''last chance to try that exercise without looking at the spoilers...''')
</br>
</br>
</br>

<source lang="haskell">
type Name = String

data Anniversary = 
   Birthday Name Date
   | Wedding Name Name Date

data Date = Date Int Int Int   -- Year, Month, Day

johnSmith :: Anniversary
johnSmith = Birthday "John Smith" (Date 1968 7 3)

smithWedding :: Anniversary
smithWedding = Wedding "John Smith" "Jane Smith" (Date 1987 3 4)

type AnniversaryBook = [Anniversary]

anniversariesOfJohnSmith :: AnniversaryBook
anniversariesOfJohnSmith = [johnSmith, smithWedding]

showDate :: Date -> String
showDate (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d 

showAnniversary :: Anniversary -> String
showAnniversary (Birthday name date) =
   name ++ " born " ++ showDate date
showAnniversary (Wedding name1 name2 date) =
   name1 ++ " married " ++ name2 ++ " on " ++ showDate date
</source>

Even in a simple example like this one, there is a very noticeable gain in terms of simplicity and clarity when compared to what would be needed to do the same task using only Ints, Strings, and corresponding lists.

A final observation on syntax: note that the <tt>Date</tt> type has a constructor function which is called <tt>Date</tt> as well. That is perfectly valid and indeed giving the constructor the same name of the type when there is just one constructor is good practice, as a simple way of making the role of the function obvious.

{{body note|1=
After these initial examples, the mechanics of using constructor functions may look a bit unwieldy, particularly if you're familiar with analogous features in other languages. There are syntactical constructs that make dealing with constructors more convenient. These will be dealt with later on, when we return to the topic of constructors and data types to explore them in detail (in sections like [[Haskell/Pattern matching|Pattern matching]] and [[Haskell/More on datatypes|More on datatypes]] - no need to rush over there right now, though).
}}

{{Haskell navigation|chapter=Elementary Haskell}}

{{Auto category}}
