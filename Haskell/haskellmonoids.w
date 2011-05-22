>{{Haskell minitoc|chapter=Advanced Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Advanced Haskell}}

{{Haskell stub}}

== Introduction ==

A monoid is any data type which has a "zero" value and a binary "append" operation satisfying certain laws.  For example, in a list, the "zero" is <code>[]</code>, while the "append" is <code>++</code>.

In Haskell's <code>Monoid</code> [[Haskell/Class_declarations|type class]], the "zero" is called <code>mzero</code> while the "append" is called <code>mappend</code>.

An example instance for integers might be
 instance Monoid Integer where
  mzero=0
  mappend=(+)

A monoid must satisfy this law:
 forall x, mappend mzero x = mappend x mzero = x
  
Sigfpe:
[http://sigfpe.blogspot.com/2009/01/haskell-monoids-and-their-uses.html blog post]
[http://www.haskell.org/pipermail/haskell-cafe/2009-January/053798.html intuition on associativity]

[http://groups.google.com/group/bahaskell/browse_thread/thread/4cf0164263e0fd6b/42b621f5a4da6019 Many monoid related links]

== Examples ==

[http://www.haskell.org/pipermail/haskell-cafe/2009-January/053602.html Usage in Cabal] [http://www.haskell.org/pipermail/haskell-cafe/2009-January/053626.html] [http://www.haskell.org/pipermail/haskell-cafe/2009-January/053721.html]. "Package databases are monoids. Configuration files are monoids. Command line flags and sets of command line flags are monoids. Package build information is a monoid."

[http://www.haskell.org/pipermail/haskell-cafe/2009-January/053603.html Usage in Xmonad]. "xmonad configuration hooks are monoidal".

[http://www.haskell.org/pipermail/haskell-cafe/2009-January/053689.html FingerTrees].

== Homomorphisms ==
A function <code>f :: A -> B</code> between two monoids <code>A</code> and <code>B</code> is called a '''homomorphism''' if it preserves the monoid structure

  f empty           = empty
  f (x `mappend` y) = f x `mappend` f y

For example, <code>length</code> is a homomorphism between ([],++) and (Int,+)

  length []         = 0
  length (xs ++ ys) = length x + length y


[http://www.nabble.com/Re%3A-Comments-from-OCaml-Hacker-Brian-Hurt-p21496412.html Google Protocol Buffers example]. The property that

    MyMessage message;
    message.ParseFromString(str1 + str2); 

is equivalent to

    MyMessage message, message2;
    message.ParseFromString(str1);
    message2.ParseFromString(str2);
    message.MergeFrom(message2); 

means that <code>parse</code> is a homomorphism:

    parse :: String -> Message
    parse []         = mempty
    parse (xs ++ ys) = parse xs `merge` parse ys

Well, not quite because parse can fail, but more or less.

== See also ==

{{Haskell navigation|chapter=Advanced Haskell}}
