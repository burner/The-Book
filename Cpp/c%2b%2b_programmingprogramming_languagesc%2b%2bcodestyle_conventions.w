>=== Coding style conventions ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== Coding style conventions ===
The use of a guide or set of convention gives programmers a set of rules for code normalization or coding ''style'' that establishes how to format code, name variables, place comments or any other non language dependent structural decision that is used on the code. This is very important, as you share a project with others. Agreeing to a common set of coding standards and recommendations saves time and effort, by enabling a greater understandings and transparency of the code base, providing a common ground for undocumented structures, making for easy debugging, and increasing code maintainability. These rules may also be referred to as ''Source Code Style'', ''Code Conventions'', ''Coding Standards'' or a variation of those.

Many organizations have published C++ style guidelines. A list of different approaches can be found on the [[C++ Programming/Weblinks#C++ Coding Conventions| C++ coding conventions Reference Section]]. The most commonly used style in C++ programming is ANSI or Allman while much C programming is still done in the Kernighan and Ritchie (K&R) style. You should be warned that this should be one of the first decisions you make on a project and in a democratic environment, a consensus can be very hard to achieve. 

Programmers tend to stick to a coding style, they have it automated and any deviation can be very hard to conform with, if you don't have a favorite style try to use the smallest possible variation to a common one or get as broad a view as you can get, permitting you to adapt easily to changes or defend your approach. There is software that can help to format or ''beautify'' the code, but automation can have its drawbacks. As seen earlier, indentation and the use of white spaces or tabs are completely ignored by the compiler. A coding style should vary depending on the lowest common denominator of the needs to standardize.

Another factor, even if yet to a minimal degree, for the selection of a coding style convention is the IDE (or the code editor) and its capabilities, this can have for instance an influence in determining how verbose code should be, the maximum the length of lines, etc. Some editors now have extremely useful features like word completion, refactoring functionalities and other that can make some specifications unnecessary or outright outdated. This will make the adoption of a coding style dependent also on the target code user available software.

Field impacted by the selection of a Code Style are:
*Re-usability
**Self documenting code
**Internationalization
**Maintainability
**Portability
*Optimization
*Build process
*Error avoidance
*Security

;Standardization is important
No matter which particular coding style you pick, once it is selected, it should be kept throughout the same project. Reading code that follows different styles can become very difficult. In the next sections we try to explain why some of the options are common practice without forcing you to adopt a specific style.

{{NOTE|Using a bad Coding Style is worse than having no Coding Style at all, since you will be extending bad practices to all the code base.}}

==== 25 lines 80 columns ====

This rule is a commonly recommended, but often countered with argument that the rule is outdated.  The rule originates from the time when text-based computer terminals and dot-matrix printers often could display at most 80 columns of text.  As such, greater than 80-column text would either inconveniently wrap to the next line, or worse, not display at all.

The physical limitations of the devices asides, this rule often still suggested under the argument that '''if you are writing code that will go further than 80 columns or 25 lines, it's time to think about splitting the code into functions'''.   Smaller chunks of encapsulated code helps in reviewing the code as it can be seen all at once without scrolling up or down.  This modularizes, and thus eases, the programmer mental representation of the project. This practice will save you precious time when you have to return to a project you haven't been working on for 6 months.

For example, you may want to split long output statements across multiple lines:
<source lang=c>
    fprintf(stdout,"The quick brown fox jumps over the lazy dog. "
                   "The quick brown fox jumps over the lazy dog.\n"
                   "The quick brown fox jumps over the lazy dog - %d", 2);
</source>


This recommended practice relates also to the [[C++ Programming/Code/Statements/Functions#0_means_success|''0 means success'']] convention for functions, that we will cover on the [[C++ Programming/Code/Statements/Functions|Functions Section]] of this book.

==== Whitespace and indentation ====
{{NOTE|Spaces, tabs and newlines (line breaks) are called ''whitespace''. Whitespace is required to separate adjacent words and numbers; they are ignored everywhere else except within quotes and preprocessor directives}}

Conventions followed when using whitespace to improve the readability of code is called an '''indentation style'''. Every block of code and every definition should follow a consistent indention style. This usually means everything within <code>{</code> and <code>}</code>. However, the same thing goes for one-line code blocks.

Use a fixed number of spaces for indentation. Recommendations vary; 2, 3, 4, 8 are all common numbers. If you use tabs for indention you have to be aware that editors and printers may deal with, and expand, tabs differently. The K&R standard recommends an indentation size of 4 spaces.
<ref>

Apparently studies at Rice University have shown that "4 spaces" is the best indentation size for C programs.
[http://www.oualline.com/vim-cook.html#drawing]

Several programmers recommend "use spaces for indentation. Do not use tabs in your code. You should set your editor to emit spaces when you hit the tab key."
[http://google-styleguide.googlecode.com/svn/trunk/cppguide.xml]
[http://www.jwz.org/doc/tabs-vs-spaces.html]

Other programmers disagree
[http://diagrammes-modernes.blogspot.com/2006/04/tab-versus-spaces.html]
[http://www.derkarl.org/why_to_tabs.html]

</ref>

For example, a program could as well be written using as follows:
<source lang=cpp>
// Using an indentation size of 2
if ( a > 5 )  { b=a; a++; } 
</source>
However, the same code could be made much more readable with proper indentation:
<source lang=cpp>
// Using an indentation size of 2
if ( a > 5 )  {
  b = a;
  a++;
}

// Using an indentation size of 4
if ( a > 5 )
{
    b = a;
    a++;
}
</source>

==== Placement of braces ([[w:Curly bracket programming language|curly brackets]]) ====
As we have seen early on the [[C++ Programming/Programming Languages/C++/Code/Statements|Statements Section]], ''compound statement'' are very important in C++, they also are subject of different coding styles, that recommend different placements of opening and closing braces (<code>{</code> and <code>}</code>). Some recommend putting the opening brace on the line with the statement, at the end ([[w:The C Programming Language (book)|K&R]]). Others recommend putting these on a line by itself, but not indented (ANSI C++). GNU recommends putting braces on a line by itself, and indenting them half-way. We recommend picking one brace-placement style and sticking with it.

Examples:

<source lang=cpp>
if (a > 5) {
  // This is K&R style
}

if (a > 5) 
{
  // This is ANSI C++ style
}

if (a > 5) 
  {
    // This is GNU style
  }
</source>

{{:C++ Programming/Code/Style Conventions/Comments}}

{{:C++ Programming/Programming Languages/C++/Code/Style Conventions/Naming Identifiers}}

==== Explicitness or implicitness ====
This can be defended both ways. If defaulting to implicitness, this means less typing but also may create wrong assumptions on the human reader and for the compiler (depending on the situation) to do extra work, on the other hand if you write more keywords and are explicit on your intentions the resulting code will be clearer and reduces errors (enabling hidden errors to be found), or more defined (self documented) but this may also lead to added limitations to the code's evolution (like we will see with the use of const). This is a thin line were an equilibrium must be reached in accord to the projects nature, and the available capabilities of the editor, code completion, syntax coloring and hovering tooltips reduces much of the work. The important fact is to be consistent as with any other rule.

===== {{C++ Programming/kw|inline}} =====
The choice of using of {{C++ Programming/kw|inline}} even if the member function is implicitly inlined.

===== const =====
Unless you plan on modifying it, you're arguably better off using const data types. The compiler can easily optimize more with this restriction, and you're unlikely to accidentally corrupt the data. Ensure that your methods take const data types unless you absolutely have to modify the parameters. Similarly, when implementing accessors for private member data, you should in most cases {{C++ Programming/kw|return}} a const. This will ensure that if the object that you're operating on is passed as const, methods that do not affect the data stored in the object still work as they should and can be called. For example, for an object containing a person, a getName() should {{C++ Programming/kw|return}} a const data type where as walk() might be non-const as it might change some internal data in the Person such as tiredness.

===== {{C++ Programming/kw|typedef}} =====
It is common practice to avoid using the {{C++ Programming/kw|typedef}} keyword since it can obfuscate code if not properly used or it can cause programmers to accidentally misuse large structures thinking them to be simple types. If used, define a set of rules for the types you rename and be sure to document them.

===== volatile =====
This keyword informs the compiler that the variable it is qualifying as volatile (can change at anytime) is excluded from any optimization techniques. Usage of this variable should be reserved for variables that are known to be modified due to an external influence of a program (whether it's hardware update, third party application, or another thread in the application).

Since the volatile keyword impacts performance, you should consider a different design that avoids this situation: most platforms where this keyword is necessary provide an alternative that helps maintain scalable performance.

Note that using volatile was not intended to be used as a threading or synchronization primitive, nor are operations on a  volatile variable guaranteed to be atomic.

==== Pointer declaration ====
Due to historical reasons some programmers refer to a specific use as: 

<source lang="cpp">
// C code style
int *z;

// C++ code style
int* z;
</source>

The second variation is by far the preferred by C++ programmers and will help identify a C programmer or legacy code.

One argument against the C++ code style version is when chaining declarations of more than one item, like:
<source lang="cpp">
// C code style
int *ptrA, *ptrB;

// C++ code style
int* ptrC, ptrD;
</source>

As you can see, in this case, the C code style makes it more obvious that ptrA and ptrB are pointers to int, and the C++ code style makes it less obvious that ptrD is an int, not a pointer to int.

It is rare to use chains of multiple objects in C++ code with the exception of the basic types and even so it is a not often used and it is extremely rare to see it used in pointers or other complex types, since it will make it harder to for a human to visually parse the code.
<source lang="cpp">
// C++ code style
int* ptrC;
int D;
</source>


{{BookCat}}
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
