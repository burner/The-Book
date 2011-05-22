>=== The Preprocessor ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== The Preprocessor ===
The [[w:preprocessor|preprocessor]] is either a separate program invoked by the [[w:compiler|compiler]] or part of the compiler itself. It performs intermediate operations that modify the original source code and internal compiler options before the compiler tries to compile the resulting source code.

The instructions that the preprocessor [[w:parsing|parses]] are called '''directives''' and come in two forms: preprocessor and compiler directives. '''Preprocessor directives''' direct the preprocessor on how it should process the source code, and '''compiler directives''' direct the compiler on how it should modify internal compiler options. Directives are used to make writing source code easier (by making it more portable, for instance) and to make the source code more understandable. They are also the only valid way to make use of facilities (classes, functions, templates, etc.) provided by the C++ Standard Library.

{{NOTE|Check the documentation of your compiler/preprocessor for information on how it implements the preprocessing phase and for any additional features not covered by the standard that may be available. For in depth information on the subject of parsing, you can read "[[Compiler Construction|Compiler Construction]]" (http://en.wikibooks.org/wiki/Compiler_Construction)
}}

All directives start with '#' at the beginning of a line. The standard directives are:
{{:C++ Programming/Programming Languages/C++/Code/Compiler/Preprocessor/Preprocessor Directives}}
==== Inclusion of Header Files (#include) ====
The '''#include''' directive allows a programmer to include contents of one file inside another file. This is commonly used to separate information needed by more than one part of a program into its own file so that it can be included again and again without having to re-type all the source code into each file.

C++ generally requires you to ''declare'' what will be used before using it. So, files called '''[[w:Header (Information Technology)|headers]]''' usually include declarations of what will be used in order for the compiler to successfully compile source code. This is further explained in the [[C++ Programming/Programming Languages/C++/Code/File Organization|File Organization Section]] of the book. The '''standard library''' (the repository of code that is available with every standards-compliant C++ compiler) and 3rd party libraries make use of headers in order to allow the inclusion of the needed declarations in your source code, allowing you to make use of features or resources that are not part of the language itself.

The first lines in any source file should usually look something like this:

<source lang=cpp>
#include <iostream>
#include "other.h"
</source>

The above lines cause the contents of the files ''iostream'' and ''other.h'' to be included for use in your program. Usually this is implemented by just inserting into your program the contents of ''iostream'' and ''other.h''. When angle brackets (<tt><nowiki><></nowiki></tt>) are used in the directive, the preprocessor is instructed to search for the specified file in a compiler-dependent location. When double quotation marks (<tt>" "</tt>) are used, the preprocessor is expected to search in some additional, usually user-defined, locations for the header file and to fall back to the standard include paths only if it is not found in those additional locations. Commonly when this form is used, the preprocessor will also search in the same directory as the file containing the <tt>#include</tt> directive.

The <tt>iostream</tt> header contains various declarations for input/output (I/O) using an abstraction of I/O mechanisms called '''streams'''. For example there is an output stream object called <tt>std::cout</tt> (where "cout" is short for "console output") which is used to output text to the standard output, which usually displays the text on the computer screen.

{{NOTE|When including standard libraries, compilers are allowed to make an exception as to whether a header file by a given name actually exists as a physical file or is simply a logical entity that causes the preprocessor to modify the source code, with the same end result as if the entity existed as a physical file. Check the documentation of your preprocessor/compiler for any vendor-specific implementation of the #include directive and for specific search locations of standard and user-defined headers. This can lead to portability problems and confusion.
}}

A list of standard C++ header files is listed below:

{{:C++ Programming/Standard Headers}}

{{NOTE|Before standardization of the headers, they were presented as separated files, like <iostream.h> and so on. This is probably still a requirement on very old (non-standards-compliant) compilers, but newer compilers will accept both methods. There is also no requirement in the standard that headers should exist in a file form. The old method of referring to standard libraries as separate files is obsolete.}}

==== #pragma ====
The '''pragma''' (pragmatic information) directive is part of the standard, but the meaning of any pragma directive depends on the software implementation of the standard that is used.

Pragma directives are used within the source program.

<source lang=cpp>
#pragma token(s)
</source>

You should check the software implementation of the C++ standard you intend to use for a list of the supported tokens. 

For example, one of the most widely used preprocessor pragma directives, <code>#pragma once</code>, when placed at the beginning of a header file, indicates that the file where it resides will be skipped if included several times by the preprocessor.

{{NOTE|Another method exists, commonly referred to as '''include guards''', that provides this same functionality but uses other include directives.<br><br>
In the GCC documentation, <code>#pragma once</code> has been described as an obsolete preprocessor directive.}}

==== Macros ====
The C++ preprocessor includes facilities for defining "macros", which roughly means the ability to replace a use of a named macro with one or more tokens.  This has various uses from defining simple constants (though '''const''' is more often used for this in C++), conditional compilation, code generation and more -- macros are a powerful facility, but if used carelessly can also lead to code that is hard to read and harder to debug!

{{NOTE|
Macros don't depend only on the C++ Standard or your actions. They may exist due to the use of external frameworks, libraries or even due the compiler you are using and the specific OS.  We will not cover that information on this book but you may find more information in the ''Pre-defined C/C++ Compiler Macros'' page at ( http://predef.sourceforge.net/ ) the project maintains a complete list of macros that are compiler and OS agnostic.
}}    

===== #define and #undef =====
The '''#define''' directive is used to define values or macros that are used by the preprocessor to manipulate the program source code before it is compiled:

<source lang=cpp>
#define USER_MAX (1000)
</source>

The '''#undef''' directive deletes a current macro definition:

<source lang=cpp>
#undef USER_MAX
</source>

It is an error to use '''#define''' to change the definition of a macro, but it is not an error to use '''#undef''' to try to undefine a macro name that is not currently defined.  Therefore, if you need to override a previous macro definition, first '''#undef''' it, and then use '''#define''' to set the new definition.

{{NOTE|note=
Because preprocessor definitions are substituted before the compiler acts on the source code, any errors that are introduced by <tt>#define</tt> are difficult to trace. For example using value or macro names that are the same as some existing identifier can create subtle errors, since the preprocessor will substitute the identifier names in the source code.

Today, for this reason, <tt>#define</tt> is primarily used to handle compiler and platform differences. E.g, a define might hold a constant which is the appropriate error code for a system call. The use of <tt>#define</tt> should thus be limited unless absolutely necessary; typedef statements, constant variables, enums, templates and [[C++ Programming/Code/Statements/Functions#Inline|inline functions]] can often accomplish the same goal more efficiently and safely.

By convention, values defined using <tt>#define</tt> are named in uppercase with "_" separators, this makes it clear to readers that the values is not alterable and in the case of macros, that the construct requires care. Although doing so is not a requirement, it is considered very bad practice to do otherwise. This allows the values to be easily identified when reading the source code.

Try to use <code>const</code> and {{C++ Programming/kw|inline}} instead of <code>#define</code>.
}}

===== \ (line continuation) =====
If for some reason it is needed to break a given statement into more than one line, use the '''\''' (backslash) symbol to "escape" the line ends.  For example, 

 #define MULTIPLELINEMACRO \
         will use what you write here \
         and here etc...

is equivalent to

<source lang=cpp>
#define MULTIPLELINEMACRO will use what you write here and here etc...
</source>

because the preprocessor joins lines ending in a backslash ("\") to the line after them. That happens even before directives (such as #define) are processed, so it works for just about all purposes, not just for macro definitions.  The backslash is sometimes said to act as an "escape" character for the newline, changing its interpretation.

In some (fairly rare) cases macros can be more readable when split across multiple lines.  Good modern C++ code will use macros only sparingly, so the need for multi-line macro definitions won't arise often.

It's certainly possible to overuse this feature.  It's quite legal but entirely indefensible, for example, to write

<source lang=cpp>
int ma\
in//ma/
()/*ma/
in/*/{}
</source>

That's an abuse of the feature though: while an escaped newline ''can'' appear in the middle of a token, there should never be any reason to use it there.  Don't try to write code that looks like it belongs in the International Obfuscated C Code Competition.

Warning: there is one occasional "gotcha" with using escaped newlines: if there are any invisible characters after the backslash, the lines will not be joined, and there will almost certainly be an error message produced later on, though it might not be at all obvious what caused it.

===== Function-like Macros =====
Another feature of the #define command is that it can take arguments, making it rather useful as a pseudo-function creator. Consider the following code: 

<source lang=cpp>
#define ABSOLUTE_VALUE( x ) ( ((x) < 0) ? -(x) : (x) )
// ...
int x = -1;
while( ABSOLUTE_VALUE( x ) ) {
// ...
}
</source>

{{NOTE|
It's generally a good idea to use extra parentheses for macro parameters, it avoids the parameters from being parsed in a unintended ways. But there are some exceptions to consider:
# Since comma operator have lower precedence than any other, this removes the possibility of problems, no need for the extra parentheses.
# When concatenating tokens with the ## operator, converting  to strings using the # operator, or concatenating adjacent string literals, parameters cannot be individually parenthesized.
}}

Notice that in the above example, the variable "x" is always within its own set of parentheses. This way, it will be evaluated in whole, before being compared to 0 or multiplied by -1. Also, the entire macro is surrounded by parentheses, to prevent it from being contaminated by other code. If you're not careful, you run the risk of having the compiler misinterpret your code.

Macros replace each occurrence of the macro parameter used in the text with the literal contents of the macro parameter without any validation checking. Badly written macros can result in code which won't compile or create hard to discover bugs. Because of side-effects it is considered a very bad idea to use macro functions as described above. However as with any rule, there may be cases where macros are the most efficient means to accomplish a particular goal.

<source lang=cpp>
int z = -10;
int y = ABSOLUTE_VALUE( z++ );
</source>

If ABSOLUTE_VALUE() was a real function 'z' would now have the value of '-9', but because it was an argument in a macro '''z++''' was expanded 3 times (in this case) and thus (in this situation) executed twice, setting z to -8, and y to 9.  In similar cases it is very easy to write code which has "undefined behavior", meaning that what it does is completely unpredictable in the eyes of the C++ Standard.

<source lang=cpp>
// ABSOLUTE_VALUE( z++ ); expanded
( ((z++) < 0 ) ? -(z++) : (z++) );
</source>

and

<source lang=cpp>
// An example on how to use a macro correctly

#include <iostream>
 
#define SLICES 8
#define PART(x) ( (x) / SLICES ) // Note the extra parentheses around '''x'''
 
int main() {
   int b = 10, c = 6;
   
   int a = PART(b + c);
   std::cout << a;
   
   return 0;
}
</source>
-- the result of "a" should be "2" (b + c passed to PART -> ((b + c) / SLICES) -> result is "2")

{{NOTE|
'''Variadic Macros'''<br>
A variadic macro is a feature of the preprocessor whereby a macro is declared to accept a varying number of arguments (similar to a variadic function).

They are currently not part of the C++ programming language, though many recent C++ implementations support variable-argument macros as an extension (ie: GCC, MS Visual Studio C++), and it is expected that variadic macros may be added to C++ at a later date.

Variable-argument macros were introduced in the ISO/IEC 9899:1999 (C99) revision of the C Programming Language standard in 1999. 
}}

===== # and ## =====
The '''#''' and '''##''' operators are used with the <tt>#define</tt> macro. Using # causes the first argument after the '''#''' to be returned as a string in quotes. For example 

 #define as_string( s ) # s

will make the compiler turn

 std::cout << as_string( Hello  World! ) << std::endl;
		
into 

 std::cout << "Hello World!" << std::endl;

{{NOTE|Observe the leading and trailing whitespace from the argument to <tt>#</tt> is removed, and consecutive sequences of whitespace between tokens are converted to single spaces.}}
		
Using '''##''' concatenates what's before the '''##''' with what's after it; the result must be a well-formed preprocessing token. For example

 #define concatenate( x, y ) x ## y
 ...
 '''int''' xy = 10;
 ...
	
will make the compiler turn

 std::cout << concatenate( x, y ) << std::endl;

into 

 std::cout << xy << std::endl;

which will, of course, display <tt>10</tt> to standard output.

String literals cannot be concatenated using '''##''', but the good news is that this isn't a problem: just writing two adjacent string literals is enough to make the preprocessor concatenate them.

===== The dangers of macros =====
To illustrate the dangers of macros, consider this naive macro

<source lang=cpp>
#define MAX(a,b) a>b?a:b
</source>

and the code

<source lang=cpp>
i = MAX(2,3)+5;
j = MAX(3,2)+5;
</source>

Take a look at this and consider what the value after execution might be.  The statements are turned into

<source lang=cpp> 
int i = 2>3?2:3+5;
int j = 3>2?3:2+5;
</source>

Thus, after execution <tt>i=8</tt> and <tt>j=3</tt> instead of the expected result of <tt>i=j=8</tt>! This is why you were cautioned to use an extra set of parenthesis above, but even with these, the road is fraught with dangers. The alert reader might quickly realize that if <tt>a,b</tt> contains expressions, the definition must parenthesize every use of <tt>a,b</tt> in the macro definition, like this:

<source lang=cpp>
#define MAX(a,b) ((a)>(b)?(a):(b))
</source>

This works, provided <tt>a,b</tt> have no side effects. Indeed,

<source lang=cpp> 
 i = 2;
 j = 3;
 k = MAX(i++, j++);
</source>

would result in <tt>k=4</tt>, <tt>i=3</tt> and <tt>j=5</tt>. This would be highly surprising to anyone expecting <tt>MAX()</tt> to behave like a function.

So what is the correct solution? The solution is not to use macro at all. A global, inline function, like this
<source lang=cpp>inline max(int a, int b) { return a>b?a:b }</source>
has none of the pitfalls above, but will not work with all types. A template (see below) takes care of this
<source lang=cpp>template<typename T> inline max(const T& a, const T& b) { return a>b?a:b }</source>
Indeed, this is (a variation of) the definition used in STL library for std::max(). This library is included with all conforming C++ compilers, so the ideal solution would be to use this. 
 std::max(3,4);

Another danger on working with macro is that they are excluded form type checking.
In the case of the MAX macro, if used with a string type variable, it will not generate a compilation error.

<source lang=cpp>MAX("hello","world")</source>

It is then preferable to use a inline function, which will be type checked. Permitting the compiler to generate a meaningful error message if the inline function is used as stated above.

==== String literal concatenation ====
One minor function of the preprocessor is in joining strings together, "string literal concatenation" -- turning code like

<source lang=cpp>std::cout << "Hello " "World!\n";</source>

into

<source lang=cpp>std::cout << "Hello World!\n";</source>

Apart from obscure uses, this is most often useful when writing long messages, as it's not legal in C++ (at this time) to have a string literal which spans multiple lines in your source code (i.e., one which has a newline character inside it).  It also helps to keep program lines down to a reasonable length; we can write

  function_name("This is a very long string literal, which would not fit "
                "onto a single line very nicely -- but with string literal "
                "concatenation, we can split it across multiple lines and "
                "the preprocessor will glue the pieces together");

Note that this joining happens before compilation; the compiler sees only one string literal here, and there's no work done at runtime, i.e., your program won't run any slower at all because of this joining together of strings.

Concatenation also applies to wide string literals (which are prefixed by an L):

  L"this " L"and " L"that"

is converted by the preprocessor into

  L"this and that".

{{NOTE|For completeness, note that C99 has different rules for this than C++98, and that C++0x seems almost certain to match C99's more tolerant rules, which allow joining of a narrow string literal to a wide string literal, something which was not valid in C++98.}}

==== Conditional compilation ====
Conditional compilation is useful for two main purposes:
* To allow certain functionality to be enabled/disabled when compiling a program
* To allow functionality to be implemented in different ways, such as when compiling on different platforms

It is also used sometimes to temporarily "comment-out" code, though using a version control system is often a more effective way to do so.

*'''Syntax''':
 #if ''condition''
   ''statement(s)''
 #elif ''condition2''
   ''statement(s)''
 ...
 #elif ''condition''
   ''statement(s)''
 #else
   ''statement(s)''
 #endif
 
 #ifdef ''defined-value''
   ''statement(s)''
 #else
   ''statement(s)''
 #endif
 
 #ifndef ''defined-value''
   ''statement(s)''
 #else
   ''statement(s)''
 #endif

===== #if =====
The '''#if''' directive allows compile-time conditional checking of preprocessor values such as created with [[##define and #undef|#define]]. If 
''condition'' is non-zero the preprocessor will include all ''statement(s)'' up to the '''#else''', '''#elif''' or '''#endif''' directive in the 
output for processing. Otherwise if the '''#if''' ''condition'' was false, any '''#elif''' directives will be checked in order and the first 
''condition'' which is true will have its ''statement(s)'' included in the output. Finally if the ''condition'' of the '''#if''' directive and 
any present '''#elif''' directives are all false the ''statement(s)'' of the '''#else''' directive will be included in the output if present; otherwise, nothing gets included.

The expression used after '''#if''' can include boolean and integral constants and arithmetic operations as well as macro names.  The allowable expressions are a subset of the full range of C++ expressions (with one exception), but are sufficient for many purposes.  The one extra operator available to '''#if''' is the '''defined''' operator, which can be used to test whether a macro of a given name is currently defined.

===== #ifdef and #ifndef =====
The '''#ifdef''' and '''#ifndef''' directives are short forms of ''''#if''' defined(''defined-value'')' and ''''#if''' 
!defined(''defined-value'')' respectively. '''defined'''(''identifier'') is valid in any expression evaluated by the preprocessor, and returns true (in this context, equivalent to 1) if a preprocessor variable by the name 
''identifier'' was defined with #define and false (in this context, equivalent to 0) otherwise.  In fact, the parentheses are optional, and it is also valid to write '''defined''' ''identifier'' without them.

(Possibly the most common use of '''#ifndef''' is in creating "include guards" for header files, to ensure that the header files can safely be included multiple times.  This is explained in the section on header files.)

===== #endif =====
The '''#endif''' directive ends '''#if''', '''#ifdef''', '''#ifndef''', '''#elif''' and '''else''' directives.

*'''Example''':
<source lang=cpp>
 #if defined(__BSD__) || defined(__LINUX__)
 #include <unistd.h>
 #endif
</source>

This can be used for example to provide multiple platform support or to have one common source file set for different program versions. Another example of use is using this instead of the (non-standard) '''#pragma once'''.

*'''Example''':
foo.hpp:
<source lang=cpp>
 #'''ifndef''' FOO_HPP
 # '''define''' FOO_HPP
 
  // code here...
 
 #'''endif''' // FOO_HPP
</source>

bar.hpp:
<source lang=cpp>
 #'''include''' "foo.h"
 
  // code here...
</source>

foo.cpp:
<source lang=cpp>
 #'''include''' "foo.hpp"
 #'''include''' "bar.hpp"
 
  // code here
</source>

When we compile '''foo.cpp''', only one copy of '''foo.hpp''' will be included due to the use of include guard. When the preprocessor reads the line <code>#'''include''' "foo.hpp"</code>, the content of '''foo.hpp''' will be expanded. Since this is the first time which '''foo.hpp''' is read (and assuming that there is no existing declaration of macro '''FOO_HPP''') '''FOO_HPP''' will not yet be declared, and so the code will be included normally. When the preprocessor read the line <code>#'''include''' "bar.hpp"</code> in foo.cpp, the content of '''bar.hpp''' will be expanded as usual, and the file '''foo.h''' will be expanded again. Owing to the previous declaration of '''FOO_HPP''', no code in '''foo.hpp''' will be inserted. Therefore, this can achieve our goal - avoiding the content of the file being included more than one time.

==== Compile-time warnings and errors ====
*'''Syntax''':
<source lang=cpp>
 #warning message
 #error message
</source>

===== #error and #warning =====
The '''#error''' directive causes the compiler to stop and spit out the line number and a message given when it is encountered. The '''#warning''' directive causes the compiler to spit out a warning with the line number and a message given when it is encountered. These directives are mostly used for debugging. 

{{NOTE|'''#error''' is part of Standard C++, whereas '''#warning''' is not (though it is widely supported).}}

*'''Example''':
<source lang=cpp>
 #if defined(__BSD___)
 #warning Support for BSD is new and may not be stable yet
 #endif
 
 #if defined(__WIN95__)
 #error Windows 95 is not supported
 #endif
</source>

==== Source file names and line numbering macros ====
The current filename and line number where the preprocessing is being performed can be retrieved using the predefined macros __FILE__ and __LINE__. Line numbers are measured ''before'' any escaped newlines are removed.  The current values of __FILE__ and __LINE__ can be overridden using the '''#line''' directive; it is very rarely appropriate to do this in hand-written code, but can be useful for code generators which create C++ code base on other input files, so that (for example) error messages will refer back to the original input files rather than to the generated C++ code.
{{BookCat}}
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
