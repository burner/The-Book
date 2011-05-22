>{{Haskell minitoc|chapter=General Practices|noexercises=1}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=General Practices|noexercises=1}}

A guide to the best practice for creating a new Haskell project or
program.

== Recommended tools ==

Almost all new Haskell projects use the following tools.  Each is
intrinsically useful, but using a set of common tools also benefits
everyone by increasing productivity, and you're more likely to get
patches.

=== Revision control ===

Use [http://darcs.net darcs] unless you have a specific reason not to.
It's written in Haskell, and it's the standard for Haskell developers.
See the wikibook [[Understanding darcs]] to get started.

=== Build system ===

Use [http://haskell.org/cabal Cabal].
You should read at least the start of section 2 of the [http://www.haskell.org/ghc/docs/latest/html/Cabal/index.html Cabal User's Guide].

=== Documentation ===

For libraries, use [http://haskell.org/haddock Haddock]. We recommend using recent versions of haddock (2.8 or above, as of December 2010).

=== Testing ===

Pure code can be tested using [http://www.md.chalmers.se/~rjmh/QuickCheck/ QuickCheck] or [http://www.mail-archive.com/haskell@haskell.org/msg19215.html SmallCheck], impure code with [http://hunit.sourceforge.net/ HUnit].  

To get started, try [[Haskell/Testing]].  For a slightly more advanced introduction, [http://blog.codersbase.com/2006/09/simple-unit-testing-in-haskell.html Simple Unit Testing in Haskell] is a blog article about creating a testing framework for QuickCheck using some Template Haskell.

== Structure of a simple project ==

The basic structure of a new Haskell project can be adopted from [http://semantic.org/hnop/ HNop], the minimal Haskell project. It consists of the following files, for the mythical project "haq".

* Haq.hs    -- the main haskell source file
* haq.cabal -- the cabal build description
* Setup.hs  -- build script itself
* _darcs    -- revision control
* README    -- info
* LICENSE   -- license

You can of course elaborate on this, with subdirectories and multiple
modules.

Here is a transcript on how you'd create a minimal darcs-using and cabalised Haskell project, for the cool new Haskell program "haq", build it, install it and release.

The new tool 'mkcabal' automates all this for you, but it's important that you understand all the parts first. 

We will now walk through the creation of the infrastructure for a simple Haskell executable. Advice for libraries follows after.

=== Create a directory ===

Create somewhere for the source:

<pre>
$ mkdir haq
$ cd haq
</pre>

=== Write some Haskell source ===

Write your program:

<pre>
$ cat > Haq.hs
--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
import System.Environment

-- 'main' runs the main program
main :: IO ()
main = getArgs >>= print . haqify . head

haqify s = "Haq! " ++ s
</pre>

=== Stick it in darcs ===

Place the source under revision control:

<pre>
$ darcs init
$ darcs add Haq.hs 
$ darcs record
addfile ./Haq.hs
Shall I record this change? (1/?)  [ynWsfqadjkc], or ? for help: y
hunk ./Haq.hs 1
+--
+-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
+-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
+--
+import System.Environment
+
+-- | 'main' runs the main program
+main :: IO ()
+main = getArgs >>= print . haqify . head
+
+haqify s = "Haq! " ++ s
Shall I record this change? (2/?)  [ynWsfqadjkc], or ? for help: y
What is the patch name? Import haq source
Do you want to add a long comment? [yn]n
Finished recording patch 'Import haq source'
</pre>

And we can see that darcs is now running the show:

<pre>
$ ls
Haq.hs _darcs
</pre>

=== Add a build system ===

Create a .cabal file describing how to build your project:

<pre>
$ cat > haq.cabal
Name:                haq
Version:             0.0
Synopsis:            Super cool mega lambdas
Description:         My super cool, indeed, even mega lambdas 
                     will demonstrate a basic project. You will marvel.
License:             GPL
License-file:        LICENSE
Author:              Don Stewart
Maintainer:          Don Stewart <dons@cse.unsw.edu.au>
Build-Depends:       base

Executable:          haq
Main-is:             Haq.hs
</pre>

(If your package uses other packages, e.g. <tt>array</tt>, you'll need to add them to the <tt>Build-Depends:</tt> field.)
Add a <tt>Setup.lhs</tt> that will actually do the building:

<pre>
$ cat > Setup.lhs
#! /usr/bin/env runhaskell

> import Distribution.Simple
> main = defaultMain
</pre>
Cabal allows either <tt>Setup.hs</tt> or <tt>Setup.lhs</tt>; as long as the format is appropriate, it doesn't matter which one you choose. But it's a good idea to always include the <code>#! /usr/bin/env runhaskell</code> line; because it follows the [[wiki:Shebang (Unix)|shebang]] convention, you could execute the Setup.hs directly in a Unix shell instead of always manually calling <tt>runhaskell</tt> (assuming the Setup file is marked executable, of course).

Record your changes:

<pre>
$ darcs add haq.cabal Setup.lhs
$ darcs record --all
What is the patch name? Add a build system
Do you want to add a long comment? [yn]n
Finished recording patch 'Add a build system'
</pre>

=== Build your project ===

Now build it!

<pre>
$ runhaskell Setup.lhs configure --prefix=$HOME --user
$ runhaskell Setup.lhs build
$ runhaskell Setup.lhs install
</pre>

=== Run it ===

And now you can run your cool project:
<pre>
$ haq me
"Haq! me"
</pre>

You can also run it in-place, avoiding the install phase:
<pre>
$ dist/build/haq/haq you
"Haq! you"
</pre>

=== Build some haddock documentation ===

Generate some API documentation into dist/doc/*

<pre>
$ runhaskell Setup.lhs haddock
</pre>

which generates files in dist/doc/ including:

<pre>
$ w3m -dump dist/doc/html/haq/Main.html
 haq Contents Index
 Main

 Synopsis
 main :: IO ()

 Documentation

 main :: IO ()
 main runs the main program

 Produced by Haddock version 0.7
</pre>

No output?  Make sure you have actually installed haddock.  It is a separate program, not something that comes with the Haskell compiler, like Cabal.

=== Add some automated testing: QuickCheck ===

We'll use QuickCheck to specify a simple property of our Haq.hs code. Create a tests module, Tests.hs, with some QuickCheck boilerplate:

<pre>
$ cat > Tests.hs
import Char
import List
import Test.QuickCheck
import Text.Printf

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

instance Arbitrary Char where
    arbitrary     = choose ('\0', '\128')
    coarbitrary c = variant (ord c `rem` 4)
</pre>

Now let's write a simple property:

<pre>
$ cat >> Tests.hs 
-- reversing twice a finite list, is the same as identity
prop_reversereverse s = (reverse . reverse) s == id s
    where _ = s :: [Int]

-- and add this to the tests list
tests  = [("reverse.reverse/id", test prop_reversereverse)]
</pre>

We can now run this test, and have QuickCheck generate the test data:

<pre>
$ runhaskell Tests.hs
reverse.reverse/id       : OK, passed 100 tests.
</pre>

Let's add a test for the 'haqify' function:

<pre>
-- Dropping the "Haq! " string is the same as identity
prop_haq s = drop (length "Haq! ") (haqify s) == id s
    where haqify s = "Haq! " ++ s

tests  = [("reverse.reverse/id", test prop_reversereverse)
        ,("drop.haq/id",        test prop_haq)]
</pre>

and let's test that:

<pre>
$ runhaskell Tests.hs
reverse.reverse/id       : OK, passed 100 tests.
drop.haq/id              : OK, passed 100 tests.
</pre>

Great!

=== Running the test suite from darcs ===

We can arrange for darcs to run the test suite on every commit:

<pre>
$ darcs setpref test "runhaskell Tests.hs"
Changing value of test from '' to 'runhaskell Tests.hs'
</pre>

will run the full set of QuickChecks. (If your test requires it you may need to ensure other things are built too e.g.: <tt>darcs setpref test "alex Tokens.x;happy Grammar.y;runhaskell Tests.hs"</tt>).

Let's commit a new patch:

<pre>
$ darcs add Tests.hs
$ darcs record --all
What is the patch name? Add testsuite
Do you want to add a long comment? [yn]n
Running test...
reverse.reverse/id       : OK, passed 100 tests.
drop.haq/id              : OK, passed 100 tests.
Test ran successfully.
Looks like a good patch.
Finished recording patch 'Add testsuite'
</pre>

Excellent, now patches must pass the test suite before they can be
committed.

=== Tag the stable version, create a tarball, and sell it! ===

Tag the stable version:

<pre>
$ darcs tag
What is the version name? 0.0
Finished tagging patch 'TAG 0.0'
</pre>

==== Advanced Darcs functionality: lazy get ====

As your repositories accumulate patches, new users can become annoyed at how long it takes to accomplish the initial <tt>darcs get</tt>. (Some projects, like [[w:Yi (editor)|yi]] or GHC, can have thousands of patches.) Darcs is quick enough, but downloading thousands of individual patches can still take a while. Isn't there some way to make things more efficient?

Darcs provides the <tt>--lazy</tt> option to <tt>darcs get</tt>. This enables to download only the latest version of the repository. Patches are later downloaded on demand if needed.


==== Distribution ====
When distributing your Haskell program, you have roughly three options:
# distributing via a Darcs repository
# distributing a tarball
## a Darcs tarball
## a Cabal tarball

With a Darcs repository, if it is public, than you are done. However: perhaps you don't have a server with Darcs, or perhaps your computer isn't set up for people to <tt>darcs pull</tt> from it. In which case you'll need to distribute the source via tarball.

===== Tarballs via darcs =====

Darcs provides a command where it will make a compressed tarball, and it will place a copy of all the files it manages into it. (Note that nothing in _darcs will be included - it'll just be your source files, no revision history.)

<pre>
$ darcs dist -d haq-0.0
Created dist as haq-0.0.tar.gz
</pre>

And you're all set up! 

===== Tarballs via Cabal =====

Since our code is cabalised, we can create a tarball with Cabal
directly:

<pre>
$ runhaskell Setup.lhs sdist
Building source dist for haq-0.0...
Source tarball created: dist/haq-0.0.tar.gz
</pre>

This has advantages and disadvantages compared to a Darcs-produced tarball. The primary ''advantage'' is that Cabal will do more checking of our repository, and more importantly, it'll ensure that the tarball has the structure needed by HackageDB and cabal-install.

However, it does have a disadvantage: it packages up only the files needed to build the project. It will deliberately fail to include other files in the repository, even if they turn out to be necessary at some point<ref>This is actually a good thing, since it allows us to do things like create an elaborate test suite which doesn't get included in the tarball, so users aren't bothered by it. It also can reveal hidden assumptions and omissions in our code - perhaps your code was only building and running because of a file accidentally generated.</ref>. To include other files (such as <tt>Test.hs</tt> in the above example), we need to add lines to the cabal file like:

<pre>
extra-source-files: Tests.hs
</pre>

If we had them, we could make sure files like AUTHORS or the README get included as well:

<pre>
data-files: AUTHORS, README
</pre>

=== Summary ===

The following files were created:

    $ ls
    Haq.hs           Tests.hs         dist             haq.cabal
    Setup.lhs        _darcs           haq-0.0.tar.gz

== Libraries ==

The process for creating a Haskell library is almost identical. The differences
are as follows, for the hypothetical "ltree" library:

=== Hierarchical source ===

The source should live under a directory path that fits into the
existing [http://www.haskell.org/~simonmar/lib-hierarchy.html module layout guide].
So we would create the following directory structure, for the module
Data.LTree:

    $ mkdir Data
    $ cat > Data/LTree.hs 
    module Data.LTree where

So our Data.LTree module lives in Data/LTree.hs

=== The Cabal file ===

Cabal files for libraries list the publically visible modules, and have
no executable section:

    $ cat ltree.cabal 
    Name:                ltree
    Version:             0.1
    Description:         Lambda tree implementation
    License:             BSD3
    License-file:        LICENSE
    Author:              Don Stewart
    Maintainer:          dons@cse.unsw.edu.au
    Build-Depends:       base
    Exposed-modules:     Data.LTree

We can thus build our library:

    $ runhaskell Setup.lhs configure --prefix=$HOME --user
    $ runhaskell Setup.lhs build    
    Preprocessing library ltree-0.1...
    Building ltree-0.1...
    [1 of 1] Compiling Data.LTree       ( Data/LTree.hs, dist/build/Data/LTree.o )
    /usr/bin/ar: creating dist/build/libHSltree-0.1.a

and our library has been created as a object archive. On *nix systems, you should 
probably add the --user flag to the configure step (this means you want to update
your local package database during installation). Now install it:

    $ runhaskell Setup.lhs install
    Installing: /home/dons/lib/ltree-0.1/ghc-6.6 & /home/dons/bin ltree-0.1...
    Registering ltree-0.1...
    Reading package info from ".installed-pkg-config" ... done.
    Saving old package config file... done.
    Writing new package config file... done.

And we're done! You can use your new library from, for example, ghci:

    $ ghci -package ltree
    Prelude> :m + Data.LTree
    Prelude Data.LTree> 

The new library is in scope, and ready to go.

=== More complex build systems ===

For larger projects it is useful to have source trees stored in
subdirectories. This can be done simply by creating a directory, for
example, "src", into which you will put your src tree.

To have Cabal find this code, you add the following line to your Cabal
file:

    hs-source-dirs: src

Cabal can set up to also run configure scripts, along with a range of
other features. For more information consult the
[http://www.haskell.org/ghc/docs/latest/html/Cabal/index.html Cabal documentation].

==== Internal modules ====

If your library uses internal modules that are not exposed, do not forget to list them in the ''other-modules'' field:

    other-modules: My.Own.Module

Failing to do so (as of GHC 6.8.3) may lead to your library deceptively building without errors but actually being unusable from applications, which would fail at build time with a linker error.

== Automation ==

A tool to automatically populate a new cabal project is available
(beta!):

    darcs get http://code.haskell.org/~dons/code/mkcabal

'''N.B. This tool does not work in Windows.''' The Windows version of GHC does not include the readline package that this tool needs.

Usage is:

<pre>
$ mkcabal
Project name: haq
What license ["GPL","LGPL","BSD3","BSD4","PublicDomain","AllRightsReserved"] ["BSD3"]: 
What kind of project [Executable,Library] [Executable]: 
Is this your name? - "Don Stewart " [Y/n]: 
Is this your email address? - "<dons@cse.unsw.edu.au>" [Y/n]: 
Created Setup.lhs and haq.cabal
$ ls
Haq.hs    LICENSE   Setup.lhs _darcs    dist      haq.cabal
</pre>

which will fill out some stub Cabal files for the project 'haq'. 

To create an entirely new project tree:

<pre>
$ mkcabal --init-project
Project name: haq
What license ["GPL","LGPL","BSD3","BSD4","PublicDomain","AllRightsReserved"] ["BSD3"]: 
What kind of project [Executable,Library] [Executable]: 
Is this your name? - "Don Stewart " [Y/n]: 
Is this your email address? - "<dons@cse.unsw.edu.au>" [Y/n]: 
Created new project directory: haq
$ cd haq
$ ls
Haq.hs    LICENSE   README    Setup.lhs haq.cabal
</pre>

== Licenses ==

Code for the common base library package must be BSD licensed or something more Free/Open<!--I say Freer, because I doubt public domain code, eg, would be rejected. -->. Otherwise, it is entirely up to you as the author. 

Choose a licence (inspired by [http://www.dina.dk/~abraham/rants/license.html this]). Check the licences of things you use, both other Haskell packages and C libraries, since these may impose conditions you must follow.

Use the same licence as related projects, where possible. The Haskell community is split into 2 camps, roughly, those who release everything under BSD or public domain, and the GPL/LGPLers (this split roughly mirrors the copyleft/noncopyleft divide in Free software communities_. Some Haskellers recommend specifically avoiding the LGPL, due to cross module optimisation issues.  Like many licensing questions, this advice is controversial.  Several Haskell projects (wxHaskell, HaXml, etc.) use the LGPL with an extra permissive clause to avoid the cross-module optimisation problem.

== Releases ==

It's important to release your code as stable, tagged tarballs. Don't
just [http://web.archive.org/web/20070627103346/http://awayrepl.blogspot.com/2006/11/we-dont-do-releases.html rely on darcs for distribution].

* '''darcs dist''' generates tarballs directly from a darcs repository

For example:

 $ cd fps
 $ ls       
 Data      LICENSE   README    Setup.hs  TODO      _darcs    cbits dist      fps.cabal tests
 $ darcs dist -d fps-0.8
 Created dist as fps-0.8.tar.gz

You can now just post your fps-0.8.tar.gz

You can also have darcs do the equivalent of 'daily snapshots' for you by using a post-hook.

put the following in _darcs/prefs/defaults:
  apply posthook darcs dist
  apply run-posthook

Advice:
* Tag each release using '''darcs tag'''. For example:

 $ darcs tag 0.8
 Finished tagging patch 'TAG 0.8'

Then people can <tt>darcs get --lazy --tag 0.8</tt>, to get just the tagged version (and not the entire history).

== Hosting ==

You can host public and private Darcs repositories on http://patch-tag.com/ for free.
Otherwise, a Darcs repository can be published simply by making it available from a web page.
Another option is to host on the Haskell Community Server at http://code.haskell.org/. You can request an account via http://community.haskell.org/admin/.

== Example ==

[http://www.cse.unsw.edu.au/~dons/blog/2006/12/11#release-a-library-today A complete example] of writing, packaging and releasing a new Haskell library under this process has been documented.


{{Haskell import Haskell wiki|How_to_write_a_Haskell_program|How to write a Haskell program|3=
Note also that the original tutorial contains extra information about announcing your software and joining the Haskell community, which may be of interest to you.}}


==References==
{{reflist}}

{{Haskell navigation|chapter=General Practices|noexercises=1}}
{{Auto category}}
