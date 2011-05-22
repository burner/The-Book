>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Haskell Basics|noexercises=1}}

This chapter will explore how to install the programs you'll need to start coding in Haskell.

== Installing Haskell ==
Haskell is a ''programming language'', i.e. a language in which humans can express how computers should behave. It's like writing a cooking recipe: you write the recipe and the computer cooks it.

To write Haskell programs, you need a program called a Haskell ''compiler''. A compiler is a program that takes code written in Haskell and translates it into ''machine code'', a second, more primitive language which only computers can understand. Using the above analogy, the compiler is the oven that bakes your batter (code) into a cookie (executable file), and you can't get the recipe from the cookie afterward.

Anyway, to start learning Haskell, '''download and install the [http://hackage.haskell.org/platform/ Haskell platform]'''. It will contain the "Glasgow Haskell Compiler", GHC for short, and everything else you need.

{{body note|1=
UNIX users:

If you are a person who prefers to compile from source: This might be a bad idea with GHC, especially if it's the first time you install it. GHC is itself mostly written in Haskell, so trying to bootstrap it by hand from source is very tricky. Besides, the build takes a very long time and consumes a lot of disk space. If you are sure that you want to build GHC from the source, see [http://hackage.haskell.org/trac/ghc/wiki/Building Building and Porting GHC at the GHC homepage].

In short, we strongly recommend downloading the Haskell Platform instead of compiling from source.
}}

== Very first steps ==
After you have installed the [http://hackage.haskell.org/platform/ Haskell Platform], it's now time to write your first Haskell code.

For that, you will use the program called '''GHCi''' (the 'i' stands for 'interactive'). Depending on your operating system, perform the following steps:

* On Windows: Click Start, then Run, then type 'cmd' and hit Enter, then type <code>ghci</code> and hit Enter once more.
* On MacOS: Open the application "Terminal" found in the "Applications/Utilities" folder, type the letters <code>ghci</code> into the window that appears and hit the Enter key.
* On Linux: Open a terminal and run the <code>ghci</code> program.

You should get output that looks something like the following:
<!-- HaskellGHCi is difficult here because of |s -->
<pre>
   ___         ___ _
  / _ \ /\  /\/ __(_)
 / /_\// /_/ / /  | |      GHC Interactive, version 6.6, for Haskell 98.
/ /_\\/ __  / /___| |      http://www.haskell.org/ghc/
\____/\/ /_/\____/|_|      Type :? for help.

Loading package base ... linking ... done.
Prelude>
</pre>

The first bit is GHCi's logo. It then informs you it's loading the base package, so you'll have access to most of the built-in functions and modules that come with GHC. Finally, the <code>Prelude&gt;</code> bit is known as the ''prompt''. This is where you enter commands, and GHCi will respond with what they evaluate to.

Now you're ready to write your first Haskell code. In particular, let's try some basic arithmetic:

{{HaskellGHCi|1=
Prelude> 2 + 2
4
Prelude> 5 * 4 + 3
23
Prelude> 2 ^ 5
32
}}

The operators are similar to what they are in other languages: <code>+</code>  is addition, <code>*</code> is multiplication, and <code>^</code> is exponentiation (raising to the power of, or <math>a ^ b</math>).

Now you know how to use Haskell as a calculator. A key idea of the Haskell language is that it will always be like a calculator, except that it will become really powerful when we calculate not only with numbers, but also with other objects like characters, lists, functions, trees and even other programs (if you aren't familiar with these yet, don't worry).

GHCi is a very powerful development environment. As we progress through the course, we'll learn how we can load source files into GHCi, and evaluate different bits of them. 

The next chapter will introduce some of the basic concepts of Haskell. Let's dive into that and have a look at our first Haskell functions.

{{Haskell navigation|chapter=Haskell Basics|noexercises=1}}

{{Auto category}}

[[pt:Haskell/Instalação e aritmética]]
