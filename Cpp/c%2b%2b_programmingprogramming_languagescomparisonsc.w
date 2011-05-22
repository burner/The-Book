>==== C 89/99 ====
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">==== C 89/99 ====
[[Subject:C programming language|C]] was essentially the core language of C++ when Bjarne Stroustrup, decided to create a "better C". Many of the syntax conventions and rules still hold true and so we can even state that C was a subset of C++, most recent C++ compilers will also compile C code taking into consideration the small incompatibilities, since [[w:C99|C99]] and C++ 2003 are not compatible any more.  You can also check more information about the C language on the [[C Programming|C Programming Wikibook]].
 
{{NOTE|In practice, much C99 code will still compile with a C++ compiler, but the language is no longer a proper subset.  Compatibility is not guaranteed.}}

C++ as defined by the ANSI standard in 98 (called C++98 at times) is very nearly, but not quite, a superset of the C language as it was defined by its first ANSI standard in 1989 (known as C89).  There are a number of ways in which C++ is not a strict superset, in the sense that not all valid C89 programs are valid C++ programs, but the process of converting C code to valid C++ code is fairly trivial (avoiding reserved words, getting around the stricter C++ type checking with casts, declaring every called function, and so on).

In 1999, C was revised and many new features were added to it. As of 2004, most of these new "C99" features are not there in C++. Some (including Stroustrup himself) have argued that the changes brought about in C99 have a philosophy distinct from what C++98 adds to C89, and hence these C99 changes are directed towards increasing incompatibility between C and C++.

The merging of the languages seems a dead issue as coordinated actions by the C and C++ standards committees leading to a practical result did not happen and it can be said that the languages started even to diverge.
 
Some of the differences are:
* C++ supports function overloading, this is absent in C (Especially in C89, it can be argued, depending on how loosely function overloading is defined, that it is possible to some degree to emulate these capabilities using the [[w:C99|C99]] standard).
* C++ supports [[C++ Programming/Programming Languages/Paradigms/Inheritance|inheritance]] and [[C++ Programming/Programming Languages/Paradigms/Polymorphism|polymorphism]].
* C++ adds keyword '''class''', but keeps '''struct''' from C, with compatible semantics.
* C++ supports access control for class members.
* C++ supports generic programming through the use of [[C++ Programming/Templates|templates]].
* C++ extends the C89 standard library with its own standard library.
* C++ and C99 offer different complex number facilities.
* C++ has '''bool''' and '''wchar_t''' as primitive types, while in C they are typedefs.
* C++ comparison operators returns '''bool''', while C returns '''int'''.
* C++ supports overloading of operators.
* C++ character constants have type '''char''', while C character constants have type '''int'''.
* C++ has specific [[C++ Programming/Programming Languages/C++/Code/Statements/Variables/Type Casting|cast operators]] ({{C++ Programming/kw|static_cast}}, {{C++ Programming/kw|dynamic_cast}}, {{C++ Programming/kw|const_cast}} and {{C++ Programming/kw|reinterpret_cast}}).
* C++ adds '''mutable''' keyword to address the imperfect match between physical and logical constness.
* C++ extends the type system with ''references''.
* C++ supports '''member functions''', '''constructors''' and '''destructors''' for user-defined types to establish invariants and to manage resources.
* C++ supports [[C++ Programming/RTTI|runtime type identification]] (RTTI), via '''typeid''' and {{C++ Programming/kw|dynamic_cast}}.
* C++ includes [[C++ Programming/Exception Handling|exception handling]].
* C++ has <tt>std::vector</tt> as part of its standard library instead of variable-length arrays as in C.
* C++ treats {{C++ Programming/kw|sizeof}} operator as compile time operation, while C allows it be a runtime operation.
* C++ has '''new''' and '''delete''' operators, while C uses '''malloc''' and '''free''' library functions exclusively.
* C++ supports object-oriented programming without extensions.
* C++ does not require use of macros and careful information-hiding and abstraction for code portability.
* C++ supports per-line comments denoted by '''//'''.  (C99 started official support for this comment system, and most compilers supported this as an extension.)
* C++ {{C++ Programming/kw|register}} is semantically different to C's implementation.

===== Reasons to chose one of the languages over the other =====
It is not uncommon to find someone defending C over C++ or vice versa or complain about some of those languages features. There is no scientific evidence to put most languages above another in general terms, the only reasons that do have some traction is if the language is still very recent and prone to deep changes or as yet unknown bugs, in the case of C or C++ this is not the case, both languages are very mature even if both are still evolving, the new features keep a high level of compatibility with old code, making the use of those new constructs a programmer's decision. It is not uncommon to establish rules in a project to limit the use of parts of a language (such as RTTI, exceptions, or virtual-functions in inner loops), due the proficiency of the programmers or the needs of the project, as it is also common for new hardware to support lower level languages first.  Due to C being less extensive and lower level than C++, it is easier to check and comply with strict industry guidelines and automate those steps. Another benefit is that it is easier for the programmer to do low level optimizations, even if most C++ compilers can guarantee near perfect optimizations automatically, a human can still do more and C has less complex structures.
 
Any of the valid reasons to choose a language over another is mostly due to programmers choice that indirectly deals with choosing the best tool for the job and having the resources needed to complete it. It would be hard to validate selecting C++ for a project if the available programmers only knew C. Even though in the reverse case it might be expected for a C++ programmer to produce functional C code, the mindset and experience needed are not the same. The same rationale is valid for C programmers and ASM, this is due to the close relations that exist in the languages structure and historic evolution.

One could argue that using the C subset of C++, in a C++ compiler, is the same as using C but in reality we find that it will generate slightly different results depending on the compiler used.

{{BookCat}}
