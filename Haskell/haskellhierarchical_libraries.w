>{{Haskell minitoc|chapter=Libraries Reference}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Libraries Reference}}

Haskell has a rich and growing set of function libraries.  They fall into several groups:

* The Standard Prelude (often referred to as just "the Prelude") is defined in the Haskell 98 standard and imported automatically to every module you write.  This defines standard types such as strings, lists and numbers and the basic functions on them, such as arithmetic, <tt>map</tt> and <tt>foldr</tt>

* The Standard Libraries are also defined in the Haskell 98 standard, but you have to import them when you need them.  The reference manuals for these libraries are at http://www.haskell.org/onlinereport/

* Since 1998 the Standard Libraries have been gradually extended, and the resulting de-facto standard is known as the Base libraries.  The same set is available for both HUGS and GHC.

* Other libraries may be included with your compiler, or can be installed using the Cabal mechanism.

When Haskell 98 was standardised modules were given a flat namespace.  This has proved inadequate and a hierarchical namespace has been added by allowing dots in module names.  For backward compatibility the standard libraries can still be accessed by their non-hierarchical names, so the modules <tt>List</tt> and <tt>Data.List</tt> both refer to the standard list library.

For details of how to import libraries into your program, see [[../Modules|Modules and libraries]].  For an explanation of the Cabal system for packaging Haskell software see [[../Packaging]].

== Haddock Documentation ==

Library reference documentation is generally produced using the Haddock tool.  The libraries shipped with GHC are documented using this mechanism.  You can view the documentation at http://www.haskell.org/ghc/docs/latest/html/libraries/index.html, and if you have installed GHC then there should also be a local copy.

Haddock produces hyperlinked documentation, so every time you see a function, type or class name you can click on it to get to the definition.  The sheer wealth of libraries available can be intimidating, so this tutorial will point out the highlights.

One thing worth noting with Haddock is that types and classes are cross-referenced by instance.  So for example in the <tt>[http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Maybe.html Data.Maybe]</tt> library the <tt>Maybe</tt> data type is listed as an instance of <tt>Ord</tt>:

 Ord a => Ord (Maybe a)

This means that if you declare a type <tt>Foo</tt> is an instance of <tt>Ord</tt> then the type <tt>Maybe Foo</tt> will automatically be an instance of <tt>Ord</tt> as well.  If you click on the word <tt>Ord</tt> in the document then you will be taken to the definiton of the <tt>Ord</tt> class and its (very long) list of instances.  The instance for <tt>Maybe</tt> will be down there as well.

{{Haskell navigation|chapter=Libraries Reference}}

[[Category:Haskell]]
