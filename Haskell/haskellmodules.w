>﻿{{Haskell minitoc|chapter=Intermediate Haskell|noexercises=1}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Intermediate Haskell|noexercises=1}}

== Modules ==

Haskell modules are a useful way to group a set of related functionalities into a single package and manage a set of different functions that have the same name. The module definition is the first thing that goes in your Haskell file.  

Here is what a basic module definition looks like:
<source lang="haskell">
module YourModule where
</source>

Note that
# The name of the module begins with a capital letter
# Each file contains only one module.

The name of the file must be that of the module but with a <tt>.hs</tt> file extension. Any dots '<tt>.</tt>' in the module name are changed for directories. So the module <tt>YourModule</tt> would be in the file <tt>YourModule.hs</tt> while a module <tt>Foo.Bar</tt> would be in the file <tt>Foo/Bar.hs</tt> or <tt>Foo\Bar.hs</tt>. Since the module name must begin with a capital letter, the file name must also start with a capital letter.

== Importing ==

One thing your module can do is import functions from other modules. That is, in between the module declaration and the rest of your code, you may include some import declarations such as

<source lang="haskell">
-- import only the functions toLower and toUpper from Data.Char
import Data.Char (toLower, toUpper)

-- import everything exported from Data.List
import Data.List
 
-- import everything exported from MyModule
import MyModule
</source>

Imported datatypes are specified by their name, followed by a list of imported constructors in parenthesis.  For example:

<source lang="haskell">
-- import only the Tree data type, and its Node constructor from Data.Tree
import Data.Tree (Tree(Node))
</source>

Now what to do if you import some modules, but some of them have overlapping definitions? Or if you import a module, but want to overwrite a function yourself? There are three ways to handle these cases: Qualified imports, hiding definitions and renaming imports.

=== Qualified imports ===

Say MyModule and MyOtherModule both have a definition for <tt>remove_e</tt>, which removes all instances of ''e'' from a string. However, MyModule only removes lower-case e's, and MyOtherModule removes both upper and lower case. In this case the following code is ambiguous:

<source lang="haskell">
-- import everything exported from MyModule
import MyModule

-- import everything exported from MyOtherModule
import MyOtherModule

-- someFunction puts a c in front of the text, and removes all e's from the rest
someFunction :: String -> String
someFunction text = 'c' : remove_e text
</source>

It isn't clear which <tt>remove_e</tt> is meant! To avoid this, use the '''qualified''' keyword:

<source lang="haskell">
import qualified MyModule
import qualified MyOtherModule

someFunction text = 'c' : MyModule.remove_e text -- Will work, removes lower case e's
someOtherFunction text = 'c' : MyOtherModule.remove_e text -- Will work, removes all e's
someIllegalFunction text = 'c' : remove_e text -- Won't work, remove_e isn't defined.
</source>

See the difference? In the latter code snippet, the function <tt>remove_e</tt> isn't even defined. Instead, we call the functions from the imported modules by prefixing them with the module's name. Note that <tt>MyModule.remove_e</tt> also works if the qualified keyword isn't included. The difference lies in the fact that <tt>remove_e</tt> is ambiguously defined in the first case, and undefined in the second case. If we have a <tt>remove_e</tt> defined in the current module, then using <tt>remove_e</tt> without any prefix will call this function.

{{body note|There is an ambiguity between a qualified name like <code>MyModule.remove_e</code> and [[Haskell/Next_steps#Function composition|function composition]] (.).  Writing <code>reverse.MyModule.remove_e</code> is bound to confuse your Haskell compiler.  One solution is stylistic: to always use spaces for function composition, for example, <code>reverse . remove_e</code> or <code>Just . remove_e</code> or even <code>Just . MyModule.remove_e</code>}}

=== Hiding definitions ===

Now suppose we want to import both <tt>MyModule</tt> and <tt>MyOtherModule</tt>, but we know for sure we want to remove all e's, not just the lower cased ones. It will become really tedious to add <tt>MyOtherModule</tt> before every call to <tt>remove_e</tt>. Can't we just ''not'' import <tt>remove_e</tt> from <tt>MyModule</tt>? The answer is: yes we can.

<source lang="haskell">
-- Note that I didn't use qualified this time.
import MyModule hiding (remove_e)
import MyOtherModule

someFunction text = 'c' : remove_e text
</source>

This works. Why? Because of the word '''hiding''' on the import line. Followed by it is a list of functions that shouldn't be imported. Hiding more than one function works like this:

<source lang="haskell">
import MyModule hiding (remove_e, remove_f)
</source>

Note that algebraic datatypes and type synonyms cannot be hidden. These are always imported. If you have a datatype defined in more modules, you must use qualified names.

=== Renaming imports ===

This is not really a technique to allow for overwriting, but it is often used along with the qualified flag. Imagine:

<source lang="haskell">
import qualified MyModuleWithAVeryLongModuleName

someFunction text = 'c' : MyModuleWithAVeryLongModuleName.remove_e $ text
</source>

Especially when using qualified, this gets irritating. What we can do about it, is using the '''as''' keyword:

<source lang="haskell">
import qualified MyModuleWithAVeryLongModuleName as Shorty

someFunction text = 'c' : Shorty.remove_e $ text
</source>

This allows us to use <tt>Shorty</tt> instead of <tt>MyModuleWithAVeryLongModuleName</tt> as prefix for the imported functions.
As long as there are no ambiguous definitions, the following is also possible:

<source lang="haskell">
import MyModule as My
import MyCompletelyDifferentModule as My
</source>

In this case, both the functions in <tt>MyModule</tt> and the functions in <tt>MyCompletelyDifferentModule</tt> can be prefixed with My.

== Exporting ==

In the examples at the start of this article, the words "import ''everything exported'' from MyModule" were used. This raises a question. How can we decide which functions are exported and which stay "internal"? Here's how:

<source lang="haskell">
module MyModule (remove_e, add_two) where

add_one blah = blah + 1

remove_e text = filter (/= 'e') text

add_two blah = add_one . add_one $ blah
</source>

In this case, only <tt>remove_e</tt> and <tt>add_two</tt> are exported. While <tt>add_two</tt> is allowed to make use of <tt>add_one</tt>, functions in modules that import <tt>MyModule</tt> cannot use <tt>add_one</tt>, as it isn't exported.

Datatype export specifications are written quite similarly to import.  You name the type, and follow with the list of constructors in parenthesis:

<source lang="haskell">
module MyModule2 (Tree(Branch, Leaf)) where

data Tree a = Branch {left, right :: Tree a} 
            | Leaf a
</source>

In this case, the module declaration could be rewritten "MyModule2 (Tree(..))", declaring that all constructors are exported.

Note: maintaining an export list is good practice not only because it reduces namespace pollution, but also because it enables certain [http://www.haskell.org/haskellwiki/Performance/GHC#Inlining compile-time optimizations] which are unavailable otherwise.


== Combining Renaming with Limited Import ==

Sometimes you have to use the import directive twice for the same module to achieve the desired effect. A common usage is as follows

<source lang="haskell">
import qualified Data.Set as Set
import Data.Set (Set, empty, insert)
</source>

This give access to all of the Data.Set module via the alias "Set", and also lets you access a few selected functions (empty, insert, and the constructor) without using the "Set" prefix. [Discussion on the Haskell-cafe mailing list indicates this is the way to do it, as of 2008]

== Notes ==

In Haskell98, the last standardised version of Haskell before Haskell 2010, the module system was fairly conservative, but recent common practice consists of employing an hierarchical module system, using periods to section off namespaces.

Mutual recursive modules are possible but need some special treatment. For how to do it in GHC see:
http://www.haskell.org/ghc/docs/latest/html/users_guide/separate-compilation.html#mutual-recursion

A module may export functions that it imports.

See the Haskell report for more details on the module system:
 
* http://www.haskell.org/onlinereport/modules.html

{{Haskell navigation|chapter=Intermediate Haskell|noexercises=1}}

[[Category:Haskell|Modules]]
