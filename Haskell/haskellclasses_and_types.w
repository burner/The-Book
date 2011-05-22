>﻿{{Haskell minitoc|chapter=Intermediate Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Intermediate Haskell}}

== Classes and Types ==

=== Simple Type Constraints ===
So far we have seen how to declare classes, how to declare types, and how to declare that types are instances of classes.  But there is something missing.  How do we declare the type of a simple arithmetic function?

 plus x y = x + y

Obviously <tt>x</tt> and <tt>y</tt> must be of the same type because you can't add different types of numbers together.  So how about:

 plus :: a -> a -> a

which says that <tt>plus</tt> takes two values and returns a new value, and all three values are of the same type.  But there is a problem: the arguments to <tt>plus</tt> need to be of a type that supports addition.  Instances of the class <tt>Num</tt> support addition, so we need to limit the type signature to just that class.  The syntax for this is:

 plus :: (Num a) => a -> a -> a

This says that the type of the arguments to <tt>plus</tt> must be an instance of <tt>Num</tt>, which is what we want.

You can put several limits into a type signature like this:

 foo :: (Num a, Show a, Show b) => a -> a -> b -> String
 foo x y t = 
    show x ++ " plus " ++ show y ++ " is " ++ show (x+y) ++ ".  " ++ show t

This says that the arguments <tt>x</tt> and <tt>y</tt> must be of the same type, and that type must be an instance of both <tt>Num</tt> and <tt>Show</tt>.  Furthermore the final argument <tt>t</tt> must be of some (possibly different) type that is also an instance of <tt>Show</tt>.

You can omit the parentheses for a single constraint, but they are required for multiple constraints.  Actually it is common practice to put even single constraints in parentheses because it makes things easier to read.

=== More Type Constraints ===

You can put a type constraint in almost any type declaration.  The only exception is a <tt>type</tt> synonym declaration.  The following is not legal:

 type (Num a) => Foo a = a -> a -> a

But you can say:

 data (Num a) => Foo a = F1 a | F2 Integer

This declares a type <tt>Foo</tt> with two constructors.  <tt>F1</tt> takes any numeric type, while <tt>F2</tt> takes an integer.

You can also use type parameters in <tt>newtype</tt> and <tt>instance</tt> declarations.  Class inheritance (see the previous section) also uses the same syntax.

{{Haskell navigation|chapter=Intermediate Haskell}}

[[Category:Haskell|Classes and types]]
