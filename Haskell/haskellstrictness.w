>﻿{{Haskell minitoc|chapter=Haskell Performance}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=Haskell Performance}}

== Difference between strict and lazy evaluation ==

Strict evaluation, or eager evaluation, is an evaluation strategy where expressions are evaluated as soon as they are bound to a variable. For example, with strict evaluation, when <code>x = 3 * 7</code> is read, 3 * 7 is immediately computed and 21 is bound to x. Conversely, with [[Haskell/Laziness|lazy evaluation]] values are only computed when they are needed. In the example <code>x = 3 * 7</code>, 3 * 7 isn't evaluated until it's needed, like if you needed to output the value of x.

== Why laziness can be problematic ==

Lazy evaluation often involves objects called thunks.  A thunk is a placeholder object, specifying not the data itself, but rather how to compute that data.  An entity can be replaced with a thunk to compute that entity.  When an entity is copied, whether or not it is a thunk doesn't matter - it's copied as is (on most implementations, a pointer to the data is created).  When an entity is evaluated, it is first checked if it is thunk; if it's a thunk, then it is executed, otherwise the actual data is returned.  It is by the magic of thunks that laziness can be implemented.

Generally, in the implementation the thunk is really just a pointer to a piece of (usually static) code, plus another pointer to the data the code should work on.  If the entity computed by the thunk is larger than the pointer to the code and the associated data, then a thunk wins out in memory usage.  But if the entity computed by the thunk is smaller, the thunk ends up using more memory.

As an example, consider an infinite length list generated using the expression <code>iterate (+ 1) 0</code>.  The size of the list is infinite, but the code is just an add instruction, and the two pieces of data, 1 and 0, are just two Integers.  In this case, the thunk representing that list takes much less memory than the actual list, which would take infinite memory.

However, as another example consider the number generated using the expression <code>4 * 13 + 2</code>.  The value of that number is 54, but in thunk form it is a multiply, an add, and three numbers.  In such a case, the thunk loses in terms of memory.

Often, the second case above will consume so much memory that it will consume the entire heap and force the garbage collector.  This can slow down the execution of the program significantly.  And that, in fact, is the main reason why laziness can be problematic.

Additionally, if the resulting value ''is'' used, no computation is saved; instead, a slight overhead (of a constanct factor) for building the thunk is paid. However, this overhead is not something the programmer should deal with most of the times; more important factors must be considered and may give a much bigger improvements; additionally, optimizing Haskell compilers like GHC can perform 'strictness analysis' and remove that slight overhead.

== Strictness annotations ==

== seq ==

=== DeepSeq ===

== References ==

* [http://www.haskell.org/haskellwiki/Performance/Strictness Strictness on the Haskell wiki]

{{Haskell stub}}

{{Haskell navigation|chapter=Haskell Performance}}

{{Auto category}}
