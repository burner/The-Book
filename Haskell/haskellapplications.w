>{{Haskell minitoc|chapter=General Practices}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=General Practices}}

So you want to build a simple application -- a piece of standalone software -- with Haskell.  

== The Main module ==

The basic requirement behind this is to have a module '''Main''' with a main function '''main''' 
<pre>
 -- thingamie.hs
 module Main where

 main = do
   putStrLn "Bonjour, world!"
</pre>

Using GHC, you may compile and run this file as follows:
<pre>
 $ ghc --make -o bonjourWorld thingamie.hs
 $ ./bonjourWorld
 Bonjour, world!
</pre>

Voilà! You now have a standalone application built in Haskell.

== Other modules? ==

Invariably your program will grow to be complicated enough that you want to split it across different files. Here is an example of an application which uses two modules.

<pre>
-- hello.hs
module Hello where

hello = "Bonjour, world!"
</pre>

<pre>
-- thingamie.hs
module Main where

import Hello

main = putStrLn hello
</pre>

We can compile this fancy new program in the same way.  Note that the --make flag to ghc is rather handy because it tells ghc to automatically detect dependencies in the files you are compiling.  That is, since thingamie.hs imports a module 'Hello', ghc will search the haskell files in the current directory for files that implement Hello and also compile that.  If Hello depends on yet other modules, ghc will automatically detect those dependencies as well.

<pre>
 $ ghc --make -o bonjourWorld thingamie.hs
 $ ./bonjourWorld
 Bonjour, world!
</pre>

If you want to search in other places for source files, including a nested structure of files and directories, you can add the starting point for the dependency search with the -i flag. This flag takes multiple, colon-separated directory names as its argument.

As a contrived example, the following program has three files all stored in a src/ directory. The directory structure looks like:

<pre>
HaskellProgram/
   src/
      Main.hs
      GUI/
           Interface.hs
      Functions/
           Mathematics.hs
</pre>

The Main module imports its dependencies by searching a path analogous to the module name — so that '''import GUI.Interface''' would search for '''GUI/Interface''' (with the appropriate file extension).

To compile this program from within the HaskellProgram directory, invoke ghc with:

<pre>
 $ ghc --make -i src -o sillyprog Main.hs
</pre>

{{Haskell navigation|chapter=General Practices}}

[[Category:Haskell|Applications]]
