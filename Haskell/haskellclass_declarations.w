>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Intermediate Haskell}}

'''Type classes''' are a way of ensuring certain operations defined on inputs. For example, if you know a certain type ''instantiates'' the class Fractional, then you can find its reciprocal.

{{body note|'''Programmers coming from object-oriented languages like C++ or Java''': A "class" in Haskell is ''not'' what you expect! Don't let the terms confuse you. In Java's terms, a Haskell class is a little like an interface (but nothing like a Java class). In C++ terms, think how the Standard Template Library talks informally about "categories" (such as InputIterator, EqualityComparable, ...). If you're familiar with C++ 0x, a Haskell class is like a "concept".
}}

Indeed, OO languages use nominal subtyping; Haskell classes use structural subtyping. Haskell classes also are not types, but categories of types. They cannot be used to hold any object fulfilling the interface, but only to parametrize arguments.

== Introduction ==
Haskell has several numeric types, including <tt>Int</tt>, <tt>Integer</tt> and <tt>Float</tt>.  You can add any two numbers of the same type together, but not numbers of different types.  You can also compare two numbers of the same type for equality.  You can also compare two values of type <tt>Bool</tt> for equality, but you cannot add them together. <!-- I feel this gives the wrong idea about numeric types... Signed, Duplode-->

The Haskell type system expresses these rules using classes.  A class is a template for types: it specifies the operations that the types must support.  A type is said to be an "instance" of a class if it supports these operations.

For instance, here is the definition of the "Eq" class from the Standard Prelude.  It defines the <tt>==</tt> and <tt>/=</tt> functions.

 class  Eq a  where
    (==), (/=) :: a -> a -> Bool
 
        -- Minimal complete definition:
        --      (==) or (/=)
    x /= y     =  not (x == y)
    x == y     =  not (x /= y)

This says that a type <tt>a</tt> is an instance of <tt>Eq</tt> if it supports these two functions.  It also gives default definitions of the functions in terms of each other.  This means that if an instance of <tt>Eq</tt> defines one of these functions then the other one will be defined automatically.

Here is how we declare that a type is an instance of <tt>Eq</tt>:

 data Foo = Foo {x :: Integer, str :: String}
 
 instance Eq Foo where
    (Foo x1 str1) == (Foo x2 str2) = (x1 == x2) && (str1 == str2)

There are several things to notice about this:

* The class <tt>Eq</tt> is defined in the Standard Prelude.  This code sample defines the type <tt>Foo</tt> and then declares it to be an instance of <tt>Eq</tt>.  The three definitions (class, data type and instance) are completely separate and there is no rule about how they are grouped.  You could just as easily create a new class <tt>Bar</tt> and then declare the type <tt>Integer</tt> to be an instance of it.

* Types and classes are not the same thing.  A class is a "template" for types.  Again, this is unlike most OO languages, where a class is also itself a type.

* The definition of <tt>==</tt> depends on the fact that <tt>Integer</tt> and <tt>String</tt> are also members of <tt>Eq</tt>.  In fact almost all types in Haskell (the most notable exception being functions) are members of <tt>Eq</tt>.

* You can only declare types to be instances of a class if they were defined with <tt>data</tt> or <tt>newtype</tt>. (Type synonyms defined with <tt>type</tt> are not allowed.)

== Deriving ==

Since equality tests between values are commonplace, in all likelihood most of the data types you create in any real program should be members of Eq, and for that matter a lot of them will also be members of other Standard Prelude classes such as <tt>Ord</tt> and <tt>Show</tt>.  This would require large amounts of boilerplate for every new type, so Haskell has a convenient way to declare the "obvious" instance definitions using the keyword <tt>deriving</tt>.  Using it, <tt>Foo</tt> would be written as:

 data Foo = Foo {x :: Integer, str :: String}
    deriving (Eq, Ord, Show)

This makes <tt>Foo</tt> an instance of <tt>Eq</tt> with an automatically generated definition of <tt>==</tt> exactly equivalent to the one we just wrote, and also makes it an instance of <tt>Ord</tt> and <tt>Show</tt> for good measure.  If you are only deriving from one class then you can omit the parentheses around its name, e.g.:

 data Foo = Foo {x :: Integer, str :: String}
    deriving Eq

You can only use <tt>deriving</tt> with a limited set of built-in classes.  They are:

; Eq : Equality operators <tt>==</tt> and <tt>/=</tt>

; Ord : Comparison operators <tt>< <= > >=</tt>.  Also <tt>min</tt> and <tt>max</tt>.

; Enum : For enumerations only.  Allows the use of list syntax such as <tt>[Blue .. Green]</tt>.

; Bounded : Also for enumerations, but can also be used on types that have only one constructor.  Provides <tt>minBound</tt> and <tt>maxBound</tt>, the lowest and highest values that the type can take.

; Show : Defines the function <tt>show</tt> (note the letter case of the class and function names) which converts the type to a string.  Also defines some other functions that will be described later.

; Read : Defines the function <tt>read</tt> which parses a string into a value of the type.  As with <tt>Show</tt> it also defines some other functions as well.

The precise rules for deriving the relevant functions are given in the language report.  However they can generally be relied upon to be the "right thing" for most cases.  The types of elements inside the data type must also be instances of the class you are deriving.

This provision of special "magic" function synthesis for a limited set of predefined classes goes against the general Haskell philosophy that "built in things are not special".  However it does save a lot of typing.  Experimental work with Template Haskell is looking at how this magic (or something like it) can be extended to all classes.

== Class Inheritance ==

Classes can inherit from other classes.  For example, here is the definition of the class <tt>Ord</tt> from the Standard Prelude, for types that have comparison operators:

 class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min             :: a -> a -> a

The actual definition is rather longer and includes default implementations for most of the functions.  The point here is that <tt>Ord</tt> inherits from <tt>Eq</tt>.  This is indicated by the <tt>=></tt> symbol in the first line.  It says that in order for a type to be an instance of <tt>Ord</tt> it must also be an instance of <tt>Eq</tt>, and hence must also implement the <tt>==</tt> and <tt>/=</tt> operations.

A class can inherit from several other classes: just put all the ancestor classes in the parentheses before the <tt>=></tt>.  Strictly speaking those parentheses can be omitted for a single ancestor, but including them acts as a visual prompt that this is not the class being defined and hence makes for easier reading.

== Standard Classes ==

This diagram, copied from the Haskell Report, shows the relationships between the classes and types in the Standard Prelude.  
The names in bold are the classes.  The non-bold text is the types that are instances of each class.  The <tt>(->)</tt> refers to functions and the <tt>[]</tt> refers to lists.

[[Image:Classes.png]]


{{Haskell navigation|chapter=Intermediate Haskell}}

[[Category:Haskell|Class declarations]]
