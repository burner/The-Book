>=== File organization ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== File organization ===
Most operating systems require files to be designated by a name followed by a specific extension. The C++ standard does not impose any specific rules on how files are named or organized.

The specific conventions for the file organizations has both technical reasons and organizational benefits, very similar to the '''''[[C++ Programming/Programming Languages/C++/Code/Style Conventions|code style conventions]]''''' we will examine later. Most of the conventions governing files derive from historical preferences and practices, that are especially related with lower level languages that preceded C++. This is especially true when we take into consideration that C++ was built over the C89 ANSI standard, with compatibility in mind, this has lead to most practices remaining static, except for the operating systems improved support for files and greater ease of management of file resources.

One of the evolutions when dealing with filenames on the language standard was that the default include files would have no extension. Most implementations still provide the old C style headers that use C's file extension ".h" for the C Standard Library, but C++-specific header filenames that were terminated in the same fashion now have no extension (e.g. iostream.h is now iostream). This change to old C++ headers was simultaneous with the implementation of [[C++ Programming/Programming Languages/C++/Code/Statements/Scope/Namespaces|namespaces]], in particular the std {{C++ Programming/kw|namespace}}.

{{NOTE|Please note that file names and extensions do not include quotes; the quotes were added for clarity in this text.}}

==== File names ====
Selecting a file name shares the same issues to naming variables, functions and in general all things. A name is an identifier that eases not only communication but how things are structured and organized.

Most of the considerations in naming files are commonsensical:
*Names should share the same language: in this, internationalization of the project should be a factor.
*Names should be descriptive, and shared by the related header, the extension will provide the needed distinction.
*Names will be case sensitive, remember to be consistent.

;Do not reuse a standard header file name
As you will see later, the C++ Standard defines a [[C++ Programming/Standard Headers|list of headers]]. The behavior is undefined if a file with the same name as a standard header is placed in the search path for included source files.

===== Extensions =====
The extension serves one purpose: to indicate to the Operating System, the IDE or the compiler what resides within the file. By itself an extension will not serve as a guarantee for the content.

Since the C language sources usually have the extension ".c" and ".h", in the beginning it was common for C++ source files to share the same extensions or use a distinct variation to clearly indicate the C++ code file. Today this is the practice, most C++ implementation files will use the ".cpp" extension and ".h" for the declaration or header files (the last one is still shared across most assembler and C compilers).

There are other common extensions variations, such as, ".cc", ".C", ".cxx", and ".c++" for "implementation" code. For header files, the same extension variations are used, but the first letter of the extension is usually replaced with an "h" as in, ".hh", ".H", ".hxx", "hpp", ".h++" etc...

Header files will be discussed with more detail later in the [[C++ Programming/Programming Languages/C++/Code/Compiler/Preprocessor|Preprocessor Section]] when introducing the #include directive and the standard headers, but in general terms a header file is a special kind of [[C++ Programming/Programming Languages/C++/Code|source code]] file that is included (by the [[C++ Programming/Programming Languages/C++/Code/Compiler/Preprocessor|preprocessor]]) by way of the [[C++ Programming/Programming Languages/C++/Code/Compiler/Preprocessor#Inclusion of Header Files (#include)|#include]] directive, traditionally used at the beginning of a ".cpp" file.

==== Source code ====
C++ programs would be compilable even if using a single file, but any complex project will benefit from being split into several source files in order to be manageable and permit re-usability of the code. The beginning programmer sees this as an extra complication, where the benefits are obscure, especially since most of the first attempts will probably result in problems. This section will cover not only the benefits and best practices but also explain how a standardized method will avoid and reduce complexity.   

;Why split code into several files?
Simple programs will fit into a single source file or at least two, other than that programs can be split across several files in order to:

*Increase organization and better code structure.
*Promote code reuse, on the same project and across projects.
*Facilitate multiple and often simultaneous edits.
*Improve compilation speed.

;Source file types
Some authors will refer to files with a .cpp extension as "source files" and files with the .h extension as "header files".  However, both of those qualify as source code. As a convention for this book, all code, whether contained within a .cpp extension (where a programmer would put it), or within a .h extension (for headers), will be called source code. Any time we're talking about a .cpp file, we'll call it an "implementation file", and any time we're referring to a header file, we'll call it a "declaration file". You should check the editor/IDE or alter the configuration to a setup that best suits you and others that will read and use this files. 

; Declaration vs Definition
In general terms a declaration specifies for the linker, the identifier, type and other aspects of language elements such as variables and functions. It is used to announce the existence of the element to the compiler which require variables to be declared before use. 

The definition assigns values to an area of memory that was reserved during the declaration phase. For functions, definitions supply the function body. While a variable or function may be declared many times, it is typically defined once. 

An object may be declared many times but may only be defined one time.

This concept will be further explained and with some particulars noted (such as {{C++ Programming/kw|inline}}) as we introduce other components. Here are some examples, ignore them for now if some of the concepts are beyond you and came back later to see the distinctions.

<source lang="cpp">
    int an_integer;                                 // defines an_integer
    extern const int a = 1;                         // defines a
    int function( int b ) { return b+an_integer; }  // defines function and defines b
    struct a_struct { int a; int b; };              // defines a_struct, a_struct::a, and a_struct::b
    struct another_struct {                         // defines another_struct
      int a;                                        // defines nonstatic data member a
      static int b;                                 // declares static data member b
      another_struct(): a(0) { } };                 // defines a constructor of another_struct
    int another_struct::b = 1;                      // defines another_struct::b
    enum { right, left };                           // defines right and left 
    namespace FirstNamespace { int a; }             // defines FirstNamespace  and FirstNamespace::a
    namespace NextNamespace = FirstNamespace ;      // defines NextNamespace 
    another_struct MySruct;                         // defines MySruct
    extern int b;                                   // declares b
    extern const int c;                             // declares c
    int another_function( int );                    // declares another_function
    struct aStruct;                                 // declares aStruct
    typedef int MyInt;                              // declares MyInt
    extern another_struct yet_another_struct;       // declares yet_another_struct
    using NextNamespace::a;                         // declares NextNamespace::a
</source>

===== .cpp =====
An implementation file includes the specific details, that is the definitions, for what is done by the program.  While the header file for the light declared what a light could do, the light's .cpp file defines how the light acts.

We will go into much more detail on class definition later; here is a preview:

[[Image:Nuvola mimetypes source cpp.png|right|95x||.cpp files]]
<source lang="cpp">
#include "light.h"

Light::Light () : on(false) {
}

void Light::toggle() {
  on = (!on);
}

bool Light::isOn() const {
  return on;
}
</source>

===== .h =====
'''Header files''' contain mostly declarations, to be used in the rest of the program.  The skeleton of a class is usually provided in a header file, while an accompanying implementation file provides the definitions to put the meat on the bones of it.  Header files are not compiled, but rather provided to other parts of the program through the use of <code>#include</code>.

[[Image:Nuvola mimetypes source h.png|right|||.cpp files]]
A typical header file looks like the following:

<source lang="cpp">
// Inside sample.h
#ifndef SAMPLE_H
#define SAMPLE_H

// Contents of the header file are placed here.

#endif /* SAMPLE_H */
</source>

Since header files are included in other files, problems can occur if they are included more than once.  This often results in the use of "header guards" using the '''''[[C++ Programming/Programming Languages/C++/Code/Compiler/Preprocessor|preprocessor directives]]''''' (#ifndef, #define, and #endif).  #ifndef checks to see if SAMPLE_H has appeared already, if it has not, the header becomes included and SAMPLE_H is defined.  If SAMPLE_H was originally defined, then the file has already been included, and is not included again.

[[Image:Nuvola mimetypes source h.png|right|||.cpp files]]
Classes are usually declared inside header files.
We will go into much more detail on class declaration later; here is a preview:

<source lang="cpp">
// Inside light.h
#ifndef LIGHT_H
#define LIGHT_H

// A light which may be on or off.
class Light {
  private:
    bool on;

  public:
    Light ();       // Makes a new light.
    void toggle (); // If light is on, turn it off, if off, turn it on
    bool isOn();    // Is the light on?
};

#endif /* LIGHT_H - comment indicating which if this goes with */
</source>

This header file "light.h" declares that there is going to be a light class, and gives the properties of the light, and the methods provided by it.  Other programmers can now include this file by typing <code>#include "light.h"</code> in their implementation files, which allows them to use this new class.  Note how these programmers do not include the actual .cpp file that goes with this class that contains the details of how the light actually works.  We'll return to this case study after we discuss implementation files.

==== Object files ====
An object file is a temporary file used by the compiler as an intermediate step between the source code and the final executable file.

All other source files that are not or resulted from source code, the support data needed for the build (creation) of the program. The extensions of these files may vary from system to system, since they depend on the IDE/Compiler and necessities of the program, they may include graphic files, or raw data formats.

=====Object code=====
The compiler produces machine code equivalent (object code) of the source code, contain the ''[[w:Binary and text files|binary]] language'' (''machine language'') instruction to be used by the computer to do as was instructed in the ''source code'', that can then be linked into the final program. This step ensures that the code is valid and will sequence into an executable program. Most object files have the file extension (.o) with the same restrictions explained above for the (.cpp/.h) files.

=====Libraries=====
Libraries are commonly distributed in binary form, using the (.lib) extension and header (.h) that provided the interface for its utilization. Libraries can also be dynamically linked and in that case the extension may depend on the target OS, for instance windows libraries as a rule have the (.dll) extension, this will be covered later on in the book in the [[C++ Programming/Compiler/Linker/Libraries|libraries section]] of this book.

==== Makefiles ====
It is common for source code to come with a specific script file named "Makefile" (without a standard extension or a standard interpreter). This type of script files is not covered by the C++ Standard, even though it is in common use.

In some projects, especially if dealing with a high level of external dependencies or specific configurations, like supporting special hardware, there is need to automate a vast number of incompatible compile sequences. This scripts are intended to alleviate the task. Explaining in detail the myriad of variations and of possible choices a programmer may make in using (or not) such a system goes beyond the scope of this book. You should check the documentation of the IDE, make tool or the information available on the source you are attempting to compile.

{{TODO|If someone wants to tackle this problem please change the text and point it to the relevant section, it was on the TODO list, do attempt to cover at least two distinct ones or the most used...|C++ Programming}}


* The [[Apache Ant]] Wikibook describes how to write and use a "build.xml", one way to automate the build process.
* [[make | The "make" Wikibook]] describes how to write and use a "Makefile", another way to automate the build process.
* ... many IDEs have a "build" button ...


{{BookCat}}
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
