>﻿{{Haskell minitoc|chapter=Elementary Haskell|noexercises=1}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Elementary Haskell|noexercises=1}}

GHCi offers several ways to make you work faster. This section will describe them and explain what they are for:

==User interface==

===Tab completion===

Whenever you are typing anything into the GHCi, you can hit Tab and you will be presented with a list of all possible names that start with what you've written so far. And if there is only one way how to continue, the string will be auto-completed. For example fol<tt><Tab></tt> will append letter d, another <tt>Tab</tt> will list four functions: fold foldl1 foldr and foldr1. Only functions and other defined objects (e.g. Data types) from modules that are currently imported are offered.

Tab completion works also when you are loading a file with your program into GHCi. After typing :l fi<tt><Tab></tt>, you will be presented with all files that start with "fi" and are present in the current directory (the one you were in when you launched GHCi)

===": commands"===
On GHCi command line, commands for the interpreter start with the character ":" (colon).
* :help - prints a list of all available commands 
* :load myfile.hs - loads a file "myfiile.hs" into GHCi
* :reload - reloads the file that has been loaded last time, so changes are visible from GHCi
* :type haskell expression - prints the type of a given expression, e.g. function that has been defined previously 

:commands can be abbreviated, writing :l for :load or :r for :reload.

==Programming efficiently==

It can be helpful to know how are the built-in function you use implemented. One possibility is to study the source codes, which are available with GHC installation,

===Most notable examples===
Some operations on lists are slow. If you ever need to do those on big Lists, use Arrays instead. Most importantly, indexing lists takes linear time. That means printing the 100th item in a list (mylist !! 99) takes twice the time printing 50th item does (mylist !! 49). That is caused by the way how lists are implemented, that the !! function has to "drill through" the list one element by one, from the beginning to the one we want to get to. For that reason, use Array, which has constant access time for any element it contains.

== Notes ==

<references/>

{{Haskell navigation|chapter=Elementary Haskell|noexercises=1}}

{{Auto category}}
