>== The Compiler ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== The Compiler ==
A '''[[w:compiler|compiler]]''' is a program that translates a [[w:computer program|computer program]] written in one  [[Programming languages bookshelf|computer language]] (the [[C++ Programming/Programming Languages/C++/Code#Source_code|source code]]) into an equivalent program written in the computer's native [[w:machine language|machine language]]. This process of translation, that includes several distinct steps is called '''compilation'''.  Since the compiler is a program, itself written in a computer language, the situation may seem a paradox akin to the [[w:Chicken or the egg|chicken and egg dilemma]]. A compiler may not be created with the resulting compilable language but with a previous available language or even in machine code.  

=== Compilation ===
The '''compilation''' output of a compiler is the result from translating or ''compiling'' a program. The most important part of the output is saved to a file called an '''''[[C++ Programming/Programming Languages/C++/Code|object file]]''''', it consists of the transformation of source files into object files.

{{NOTE|Some files may be created/needed for a successful compilation, that data is not part of the C++ language or may result from the compilation of external code (an example would be a library), this may depend on the specific compiler you use (MS Visual Studio for example adds several extra files to a project), in that case you should check the documentation or it can part of a specific framework that needs to be accessed. Be aware that some of this constructs may limit the portability of the code.}}

The instructions of this ''compiled'' program can then be run (executed) by the computer if the object file is in an executable format. However, there are additional steps that are required for a compilation: preprocessing and linking.

==== Compile-time ====
Defines the time and operations performed by a compiler (i.e., ''compile-time operations'') during a build (creation) of a program (executable or not). Most of the uses of "static" on the C++ language is directly related to compile-time information.

The operations performed at compile time usually include ''lexical analysis'', ''syntax analysis'', various kinds of [[w:Semantic analysis (computer science)|semantic analysis]] (e.g., [[w:datatype|type checks]], some of the [[C++ Programming/Programming Languages/C++/Code/Statements/Variables/Type Casting|type casts]], and [[w:instantiation of template|instantiation of template]]) and [[w:code generation (compiler)|code generation]].

The definition of a programming language will specify compile time requirements that source code must meet to be successfully compiled.

Compile time occurs before [[w:link time|link time]] (when the output of one or more compiled files are joined together) and runtime (when a program is executed).  In some programming languages it may be necessary for some compilation and linking to occur at runtime.

;Run-time
'''Run-time''', or '''execution time''', starts at the moment the program starts to execute and end as it exits. At this stage the compiler is irrelevant and has no control. This is the most important location in regards to optimizations (a program will only compile once but run many times) and debugging (tracing and interaction will only be possible at this stage). But it is also in run-time that some of the [[C++ Programming/Programming Languages/C++/Code/Statements/Variables/Type Casting|type casting may occur]] and that [[C++ Programming/RTTI|Run-Time Type Information (RTTI)]] has relevance. The concept of runtime will be mentioned again when relevant.

===== Lexical analysis =====
This is alternatively known as scanning or ''tokenisation''. It happens before syntax analysis and converts the code into [[Compiler Construction#What_is_a_token|tokens]], which are the parts of the code that the program will actually use. The source code as expressed as characters (arranged on lines) into a sequence of special tokens for each reserved keyword, and tokens for data types and identifiers and values. The lexical analyzer is the part of the compiler which removes whitespace and other non compilable characters from the source code. It uses whitespace to separate different tokens, and ignores the whitespace.

To give a simple illustration of the process:

<source lang=cpp>
int main()
{
    std::cout << "hello world" << std::endl;
    return 0;
}
</source>

Depending on the lexical rules used it might be '''tokenized''' as:

<pre>
1 = string "int"
2 = string "main"
3 = opening parenthesis
4 = closing parenthesis
5 = opening brace
6 = string "std"
7 = namespace operator
8 = string "cout"
9 = << operator
10 = string ""hello world""
11 = string "endl"
12 = semicolon
13 = string "return"
14 = number 0
15 = closing brace
</pre>

And so for this program the lexical analyzer might send something like:

<pre>
1 2 3 4 5 6 7 8 9 10 9 6 7 11 12 13 14 12 15
</pre>

To the syntactical analyzer, which is talked about next, to be parsed. It is easier for the syntactical analyzer to apply the rules of the language when it can work with numerical values and can distinguish between language syntax (such as the semicolon) and everything else, and knows what data type each thing has.

===== Syntax analysis =====
This step (also called sometimes syntax checking) ensures that the code is valid and will sequence into an executable program. The syntactical analyzer applies rules to the code, checking to make sure that each opening brace has a corresponding closing brace, and that each declaration has a type, and that the type exists, and that.... syntax analysis is more complicated that lexical analysis =). 

As an example:
<source lang=cpp>
int main()
{
    std::cout << "hello world" << std::endl;
    return 0;
}
</source>

*The syntax analyzer would first look at the string "int", check it against defined keywords, and find that it is a type for integers. *The analyzer would then look at the next token as an identifier, and check to make sure that it has used a valid identifier name.
*It would then look at the next token. Because it is an opening parenthesis it will treat "main" as a function, instead of a declaration  of a variable if it found a semicolon or the initialization of an integer variable if it found an equals sign.
*After the opening parenthesis it would find a closing parenthesis, meaning that the function has 0 parameters.
*Then it would look at the next token and see it was an opening brace, so it would think that this was the implementation of the function main, instead of a declaration of main if the next token had been a semicolon, even though you can not declare main in c++. It would probably create a counter also to keep track of the level of the statement blocks to make sure the braces were in pairs. *After that it would look at the next token, and probably not do anything with it, but then it would see the :: operator, and check that "std" was a valid {{C++ Programming/kw|namespace}}.
*Then it would see the next token "cout" as the name of an identifier in the {{C++ Programming/kw|namespace}} "std", and see that it was a template. 
*The analyzer would see the << operator next, and so would check that the << operator could be used with cout, and also that the next token could be used with the << operator.
*The same thing would happen with the next token after the ""hello world""  token. Then it would get to the "std" token again, look past it to see the :: operator token and check that the {{C++ Programming/kw|namespace}} existed again, then check to see if "endl" was in the {{C++ Programming/kw|namespace}}.
*Then it would see the semicolon and so it would see that as the end of the statement.
*Next it would see the keyword {{C++ Programming/kw|return}}, and then expect an integer value as the next token because main returns an integer, and it would find 0, which is an integer.
*Then the next symbol is a semicolon so that is the end of the statement.
*The next token is a closing brace so that is the end of the function. And there are no more tokens, so if the syntax analyzer did not find any errors with the code, it would send the tokens to the compiler so that the program could be converted to machine language. 

This is a simple view of syntax analysis, and real syntax analyzers do not really work this way, but the idea is the same.

Here are some keywords which the syntax analyzer will look for to make sure you are not using any of these as identifier names, or to know what type you are defining your variables as or what function you are using which is included in the C++ language.

==== Compile speed ====
There are several factors that dictate how fast a compilation proceeds, like:

*Hardware
**Resources (Slow CPU, low memory and even a slow HDD can have an influence)

*Software
**The compiler itself, new is always better, but may depend on how portable you want the project to be.
**The design selected for the program (structure of object dependencies, includes) will also factor in.

Experience tells that most likely if you are suffering from slow compile times, the program you are trying to compile is poorly designed, take the time to structure your own code to minimize re-compilation after changes. Large projects will always compile slower. Use pre-compiled headers and external header guards. We will discuss ways to reduce compile time in the [[C++ Programming/Optimization#Compile time|Optimization]] Section of this book.

{{:C++ Programming/Compiler/Where to get}}
{{BookCat}}
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
