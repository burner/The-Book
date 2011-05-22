>=== Linker ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== Linker ===
The '''linker''' is a program that makes executable files. The linker resolves linkage issues, such as the use of symbols or identifiers which are defined in one translation unit and are needed from other translation units. Symbols or identifiers which are needed outside a single translation unit have ''external linkage''. In short, the linker's job is to resolve references to undefined symbols by finding out which other object defines a symbol in question, and replacing placeholders with the symbol's address. Of course, the process is more complicated than this; but the basic ideas apply.

Linkers can take objects from a collection called a library. Depending on the library (system or language or external libraries) and options passed, they may only include its symbols that are referenced from other object files or libraries. Libraries for diverse purposes exist, and one or more system libraries are usually linked in by default. We will take a closer look into libraries on the [[C++ Programming/Compiler/Linker/Libraries|Libraries Section]] of this book.

==== Linking ====
The process of connecting or combining object files produced by a compiler with the libraries necessary to make a working executable program (or a library) is called ''linking''. ''Linkage'' refers to the way in which a program is built out of a number of [[w:translation unit (computer science)|translation units]]. 

C++ programs can be compiled and linked with programs written in other languages, such as C, Fortran, assembly language, and Pascal.
* The appropriate compiler compiles each module separately. A C++ compiler compiles each ".cpp" file into a ".o" file, an assembler assembles each ".asm" file into a ".o" file, a Pascal compiler compiles each ".pas" file into a ".o" file, etc.
* The linker links all the ".o" files together in a separate step, creating the final executable file.

==== Linkage ====
Every function has either external or internal linkage.

A function with internal linkage is only visible inside one translation unit.
When the compiler compiles a function with internal linkage, the compiler writes the machine code for that function at some address and puts that address in all calls to that function (which are all in that one translation unit), but strips out all mention of that function in the ".o" file.
If there is some call to a function that apparently has internal linkage, but doesn't appear to be defined in this translation unit, the compiler can immediately tell the programmer about the problem (error).
If there is some function with internal linkage that never gets called, the compiler can do "dead code elimination" and leave it out of the ".o" file.

The linker never hears about those functions with internal linkage, so it knows nothing about them.

A function declared with external linkage is visible inside several translation units.
When a compiler compiles a call to that function in one translation unit, it does not have any idea where that function is, so it leaves a placeholder in all calls to that function, and instructions in the ".o" file to replace that placeholder with the address of a function with that name.
If that function is never defined, the compiler can't possibly know that, so the programmer doesn't get a warning about the problem (error) until much later.

When a compiler compiles (the definition of) a function with external linkage (in some other translation unit), the compiler writes the machine code code of that function at some address, and puts that address and the name of the function in the ".o" file where the linker can find it.
The compiler assumes that the function will be called from some other translation unit (some other ".o" file), and must leave that function in this ".o" file, even if it ends up that the function is never called from any translation unit.

Most code conventions specify that header files contain only declarations, not definitions.
Most code conventions specify that implementation files (".cpp" files) contain only definitions and local declarations, not external declarations.

This results in the "extern" keyword being used only in header files, never in implementation files.
This results in internal linkage being indicated only in implementation files, never in header files.
This results in the "static" keyword being used only in implementation files, never in header files, except when "static" is used inside a class definition inside a header file, where it indicates something other than internal linkage.

We discuss header files and implementation files in more detail later in the [[C++ Programming/Programming Languages/C++/Code/File Organization|File Organization Section]] of the book.

=====Internal=====
{{TODO|Complete, use global const case as example}}

====== {{C++ Programming/kw|static}} ======
{{:C++ Programming/Programming Languages/C++/Code/Keywords/static}}
{{:C++ Programming/Programming Languages/C++/Code/Keywords/static/Internal Linkage}}

=====External=====
All entities in the C++ Standard Library have external linkage.

====== {{C++ Programming/kw|extern}} ======
{{:C++ Programming/Programming Languages/C++/Code/Keywords/extern}}
{{BookCat}}
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
