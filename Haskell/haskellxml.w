>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Specialised Tasks}}

There are several Haskell libraries for XML work, and additional ones for HTML. For more web-specific work, you may want to refer to the [[Haskell/Web programming]] chapter.

===Libraries for parsing XML===

* [http://www.fh-wedel.de/~si/HXmlToolbox/ The Haskell XML Toolbox (hxt)] is a collection of tools for parsing XML, aiming at a more general approach than the other tools.
* [http://www.cs.york.ac.uk/fp/HaXml/ HaXml] is a collection of utilities for parsing, filtering, transforming, and generating XML documents using Haskell.
* [http://www.flightlab.com/~joe/hxml/ HXML] is a non-validating, lazy, space efficient parser that can work as a drop-in replacement for HaXml.

===Libraries for generating XML===

* HSXML represents XML documents as statically typesafe s-expressions.

===Other options===

* [http://www.cs.york.ac.uk/fp/darcs/tagsoup/tagsoup.htm tagsoup] is a library for parsing unstructured HTML, i.e. it does not assume validity or even well-formedness of the data.

==Getting acquainted with HXT==

In the following, we are going to use the Haskell XML Toolbox for our examples.  You should have a working [[Haskell/Getting set up|installation of GHC]], including GHCi, and you should have downloaded and installed HXT according to [http://www.fh-wedel.de/~si/HXmlToolbox/#install the instructions].

With those in place, we are ready to start playing with HXT.  Let's bring the XML parser into scope, and parse a simple XML-formatted string:

  Prelude> ''':m + Text.XML.HXT.Parser.XmlParsec'''
  Prelude Text.XML.HXT.Parser.XmlParsec> '''xread "<foo>abc<bar/>def</foo>"'''
  [NTree (XTag (QN {namePrefix = "", localPart = "foo", namespaceUri = ""}) [])
  [NTree (XText "abc") [],NTree (XTag (QN {namePrefix = "", localPart = "bar",
  namespaceUri = ""}) []) [],NTree (XText "def") []]]

We see that HXT represents an XML document as a list of trees, where the nodes can be constructed as an XTag containing a list of subtrees, or an XText containing a string.  With GHCi, we can explore this in more detail:

  Prelude Text.XML.HXT.Parser.XmlParsec Text.XML.HXT.DOM> ''':i NTree
  data NTree a = NTree a (NTrees a)  
                                  -- Defined in Data.Tree.NTree.TypeDefs
  Prelude Text.XML.HXT.Parser.XmlParsec Text.XML.HXT.DOM> ''':i NTrees
  type NTrees a = [NTree a]       -- Defined in Data.Tree.NTree.TypeDefs

As we can see, an NTree is a general tree structure where a node stores its children in a list, and some more browsing around will tell us that XML documents are trees over an XNode type, defined as:

  data XNode
    = XText String
    | XCharRef Int
    | XEntityRef String
    | XCmt String
    | XCdata String
    | XPi QName XmlTrees
    | XTag QName XmlTrees
    | XDTD DTDElem Attributes
    | XAttr QName
    | XError Int String

Returning to our example, we notice that while HXT successfully parsed our input, one might desire a more lucid presentation for human consumption.  Lucky for us, the DOM module supplies this.  Notice that xread returns a list of trees, while the formatting function works on a single tree.

  Prelude Text.XML.HXT.Parser.XmlParsec> :m + Text.XML.HXT.DOM
  Prelude Text.XML.HXT.Parser.XmlParsec Text.XML.HXT.DOM> '''putStrLn $ formatXmlTree $ head $ xread "<foo>abc<bar/>def</foo>"
  ---XTag "foo"
     |
     +---XText "abc"
     |
     +---XTag "bar"
     |
     +---XText "def"

This representation makes the structure obvious, and it is easy to see the relationship to our input string.  Let's proceed to extend our XML document with some attributes (taking care to escape the quotes, of course):

  Prelude Text.XML.HXT.Parser.XmlParsec> '''xread "<foo a1=\"my\" b2=\"oh\">abc<bar/>def</foo>"
  [NTree (XTag (QN {namePrefix = "", localPart = "foo", namespaceUri = ""}) [NTree (XAttr (QN
  {namePrefix = "", localPart = "a1", namespaceUri = ""})) [NTree (XText "my") []],NTree (XAttr
  (QN {namePrefix = "", localPart = "b2", namespaceUri = ""})) [NTree (XText "oh") []]]) [NTree
  (XText "abc") [],NTree (XTag (QN {namePrefix = "", localPart = "bar", namespaceUri = ""}) [])
  [],NTree (XText "def") []]]

Notice that attributes are stored as regular NTree nodes with the XAttr content type, and (of course) no children.  Feel free to pretty-print this expression, as we did above.

For a trivial example of data extraction, consider this small example using [http://en.wikipedia.org/wiki/XPath XPath]:

  Prelude> :set prompt "> "
  > :m + Text.XML.HXT.Parser.XmlParsec Text.XML.HXT.XPath.XPathEval
  > let xml = "<foo><a>A</a><c>C</c></foo>"
  > let xmltree = head $ xread xml
  > let result = getXPath "//a" xmltree
  > result
  > [NTree (XTag (QN {namePrefix = "", localPart = "a", namespaceUri = ""}) []) [NTree (XText "A") []]]
  > :t result
  > result :: NTrees XNode

{{Haskell navigation|chapter=Specialised Tasks}}
{{Auto category}}
