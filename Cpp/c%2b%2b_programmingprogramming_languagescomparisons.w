>=== Language comparisons ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== Language comparisons ===
There isn't a perfect language. It all depends on the resources (tools, people even available time) and the objective.

==== The ideal language depends on the specific problem ====

All programming languages are designed to be ''general mechanisms for expressing problem solving algorithms''.   In other words, it is a language - rather than simply an expression - because it is capable of expressing solutions more than one specific problem.

The level of generality in a programming language varies.  There are [http://en.wikipedia.org/wiki/Domain-specific_language domain-specific languages] (DSLs)  such as regular expression syntax which is designed specifically for pattern matching and string manipulation problems.   There are also general-purpose programming languages such as C++. 

Ultimately, there is no perfect language.  There are some languages that are more suited to specific classes of problems than others.  Each language makes trade-offs, favoring efficiency in one area for inefficiencies in other areas.   Furthermore, efficiency may not only mean ''runtime performance'' but also includes factors such as development time, code maintainability, and other considerations that affect software development.   The best language is dependent on the specific objectives of the programmers.

Furthermore, another very practical consideration when selecting a language is the number and quality of tools available to the programmer for that language.  No matter how good a language is in theory, if there is no set of reliable tools on the desired platform, that language is not the best choice.

The optimal language (in terms of run-time performance) is machine code but [[w:Machine code|machine code]] (binary) is the least efficient programming language in terms of coder time. The complexity of writing large systems is enormous with high-level languages, and beyond human capabilities with machine code. In the next sections C++ will be compared with other closely related languages like [[C++ Programming/Programming Languages/Comparisons/C|C]], [[C++ Programming/Programming Languages/Comparisons/Java|Java]], [[C++ Programming/Programming Languages/Comparisons/C Sharp|C#]], [[C++ Programming/Programming Languages/Comparisons/Managed C++|C++/CLI]] and [[C++ Programming/Programming Languages/Comparisons/D|D]].
<!-- Full path is needed so not to break the print version --->

{{SAMPLE|sampletext=''«When someone says "I want a programming language in which I need only say what I wish done," give him a lollipop.»''|caption=<small>--published in SIGPLAN Notices Vol. 17, No. 9, September 1982</small>}}

The quote above is shown to indicate that no programming language at present can translate directly concepts or ideas into useful code, there are solutions that will help. We will cover the use of [[w:Computer-aided software engineering (CASE)|Computer-aided software engineering (CASE)]] tools that will address part of this problem but its use does require planning and some degree of complexity.

The intention of these sections is not to promote one language above another; each has its applicability. Some are better in specific tasks, some are simpler to learn, others only provide a better level of control to the programmer. This all may depend also on the level of control the programmer has of a given language.

==== Garbage collection ====
In C++ garbage collection is optional rather than required. In the [[C++ Programming/Compiler/Linker/Libraries/Garbage Collection|Garbage Collection Section]] of this book we will cover this issue deeply.

==== Why doesn't C++ include a <code>finally</code> keyword? ====
As we will see in the [[C++ Programming/RAII|Resource Acquisition Is Initialization (RAII) Section]] of the book, RAII can be used to provide a better solution for most issues.  When <code>finally</code> is used to clean up, it has to be written by the clients of a class each time that class is used (for example, clients of a ''fileClass class'' have to do I/O in a <code>try</code>/<code>catch</code>/<code>finally</code> block so that they can guarantee that the ''fileClass'' is closed).  With RAII, the destructor of the ''fileClass'' can make that guarantee. Now the cleanup code has to be coded only once &mdash; in the destructor of ''fileClass''; the users of the class don't need to do anything.

{{TODO|Split this explanation to RAII and only provide the reference|C++ Programming}}

==== Mixing languages ====
{{TODO|Add relevant information|C++ Programming}}
By default, the C++ compiler normally "mangles" the names of functions in order to facilitate function overloading and generic functions.  In some cases, you need to gain access to a function that wasn't created in a C++ compiler.  For this to occur, you need to use the {{C++ Programming/kw|extern}} keyword to declare that function as external:
<source lang=cpp>
extern "C" void LibraryFunction();
</source>

{{BookCat}}
