>﻿{{Haskell minitoc|chapter=General Practices}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=General Practices}}

The "Scrap your boilerplate" approach, "described" in [http://www.cs.vu.nl/boilerplate/], is a way to allow your data structures to be traversed by so-called "generic" functions: that is, functions that abstract over the specific data constructors being created or modified, while allowing for the addition of cases for specific types.


For instance if you want to serialize all the structures in your code, but you want to write only one serialization function that operates over any instance of the Data.Data.Data class (which can be derived with -XDeriveDataTypeable).


== Serialization Example ==
The goal is to convert all our data into a format below:

  data Tag = Con String | Val String


== Comparing haskell ASTs ==
haskell-src-exts parses haskell into a quite complicated syntax tree. Let's say we want to check if two source files that are nearly identical.

To start:

  import System.Environment
  import Language.Haskell.Exts

  main = do
     -- parse the filenames given by the first two command line arguments,
     -- proper error handling is left as an exercise
     ParseOk moduleA: ParseOk moduleb:_ <- mapM parseFile . take 2 =<< getArgs
     
     putStrLn $ if moduleA == moduleB
          then "Your modules are equal"
          else "Your modules differ"

From a bit of testing, it will be apparent that identical files with different names will not be equal to (==). However, to correct the fact, without resorting to lots of boilerplate, we can use generic programming:

== TODO ==
describe using   Data.Generics.Twins.gzip*? to write a function to find where there are differences?

Or use it to write a variant of geq that ignores the specific cases that are unimportant (the SrcLoc elements) (i.e. syb doesn't allow generic extension... contrast it with other libraries?).

Or just explain this hack (which worked well enough) to run before (==), or geq::

  everyWhere (mkT $ \ _ -> SrcLoc "" 0 0) :: Data a => a -> a

Or can we develop this into writing something better than sim_mira (for hs code), found here: http://www.cs.vu.nl/~dick/sim.html

{{Haskell navigation|chapter=General Practices}}

{{BookCat}}
