>== The code ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== The code ==
Code is the string of symbols interpreted by a computer in order to execute a given objective.  As with natural languages, code is the result of all the conventions and rules that govern a language. It is what permits implementation of projects in a standard, compilable way.  Correctly written code is used to create projects that serve as intermediaries for natural language in order to express meanings and ideas.  This, theoretically and actually, allows a computer program to solve any explicitly-defined problem.

=== Programming ===
The task of programming, while not easy in its execution, is actually fairly simple in its goals.  A programmer will envision, or be tasked with, a specific goal.  Goals are usually provided in the form of "I want a program that will perform...''fill in the blank''..."  The job of the programmer then is to come up with a "working model" (a model that may consist of one or more [[w:Algorithm|algorithms]]).  That "working model" is sort of an idea of ''how'' a program will accomplish the goal set out for it.  It gives a programmer an idea of what to write in order to turn the idea in to a working program.

Once the programmer has an idea of the structure their program will need to take in order to accomplish the goal, they set about actually writing the program itself, using the selected ''[[C++ Programming/Programming Languages|programming language(s)]] keywords'', ''functions'' and ''syntax''. The code that they write is what actually implements the program, or causes it to perform the necessary task, and for that reason, it is sometimes called "implementation code".

=== What is a program? ===
To restate the definition, a program is just a sequence of instructions, written in some form of programming language, that tells a computer what to do, and generally how to do it.  Everything that a typical user does on a computer is handled and controlled by programs.  Programs can contain anything from instructions to solve math problems or send emails, to how to behave when a character is shot in a video game. The computer will follow the instructions of a program one line at a time from the start to the end.

==== Types of programs ====
There are all kinds of different programs used today, for all types of purposes. All programs are written with some form of programming language and C++ can be used for in any type of application.  Examples of different types of programs, (also called software), include:

{{TODO|Correct the examples to indicate real life application of the C++ language, if possible with projects that users can examine the source code.}}

;''Operating Systems'': An operating system is responsible for making sure that everything on a computer works the way that it should.  It is especially concerned with making certain that your computer's "hardware", (i.e. disk drives, video card and sound card, and etc.) interfaces properly with other programs you have on your computer. Microsoft Windows and Linux are examples of PC operating systems.

;''Office Programs'': This is a general category for a collection of programs that allow you to compose, view, print or otherwise display different kinds of documents. Often such "suites" come with a word processor for composing letters or reports, a spreadsheet application and a slide-show creator of some kind among other things.  Popular examples of Office Suites are Microsoft Office and OpenOffice.org 

;''Web Browsers & Email Clients'': A web-browser is a program that allows you to type in an Internet address and then displays that page for you.  An email client is a program that allows you to send, receive and compose email messages outside of a web-browser. Often email clients have some capability as a web-browser as well, and some web-browsers have integrated email clients. Well-known web-browsers are Internet Explorer and Firefox, and Email Clients include Microsoft Outlook and Thunderbird. Most are programmed using C++, you can access some as Open-source projects, for instance (http://www.mozilla.org/projects/firefox/) will help you download and compile Firefox.

;''Audio/Video Software'': These types of software include media players, sound recording software, burning/ripping software, DVD players, etc. Many applications such as Windows Media Player, a popular media player programmed by Microsoft, are examples of audio/video software.

;''Computer Games'': There are countless software titles that are either games or designed to assist with playing games. The category is so wide that it would be impossible to get in to a detailed discussion of all the different kinds of game software without creating a different book! Gaming is one of the most popular activities to engage in on a computer.

;''Development Software'': Development software is software used specifically for programming.  It includes software for composing programs in a computer language (sometimes as simple as a text editor like Notepad), for checking to make sure that code is stable and correct (called a debugger), and for compiling that source code into executable programs that can be run later (these are called compilers). Oftentimes, these three separate programs are combined in to one bigger program called an IDE (Integrated Development Environment). There are all kinds of IDEs for every programming language imaginable. A popular C++ IDE for Windows and Linux is the [http://www.codeblocks.org/ Code::Blocks] IDE ([http://www.codeblocks.org/features.shtml Free and Open Source]). The one type of software that you will learn the most about in this book is Development Software.

==== Types of instructions ====
As mentioned already, programs are written in many different languages, and for every language, the words and statements used to tell the computer to execute specific commands are different. No matter what words and statements are used though, just about every programming language will include statements that will accomplish the following:

;''Input'': Input is the act of getting information from a keyboard or mouse, or sometimes another program.

;''Output'': Output is the opposite of input; it gives information to the computer monitor or another device or program. 

;''Math''/''Algorithm'': All computer processors (the brain of the computer), have the ability to perform basic mathematical computation, and every programming language has some way of telling it to do so. 

;''Testing'': Testing involves telling the computer to check for a certain condition and to do something when that condition is true or false. Conditionals are one of the most important concepts in programming, and all languages have some method of testing conditions. 

;''Repetition'': Perform some action repeatedly, usually with some variation. 

Believe it or not, that's pretty much all there is to it. Every program you've ever used, no matter how complicated, is made up of functions that look more or less like these. Thus, one way to describe programming is the process of breaking a large, complex task up into smaller and smaller subtasks until eventually the subtasks are simple enough to be performed with one of these simple functions.

==== Program execution ====
Execution starts on '''''[[C++ Programming/Code/Statements/Functions#main|main function]]''''', the ''entry point'' of any (standard-compliant) C++ program. We will cover it when we introduce '''''[[C++ Programming/Code/Statements/Functions|functions]]'''''.

''Execution control'' or simply ''control'', means the process and the location of execution of a program, this has a direct link to '''''[[C++ Programming/Programming Languages/Paradigms#Procedural programming|procedural programming]]'''''. You will note the mention of ''control'' as we proceed, as it is necessary concept to explain the order of execution of code and its interpretation by the computer.

==== Core vs Standard Library ====
{{TODO|Complete}}

==== Program organization ====
How the instructions of a program are written out and stored is generally not a concept determined by a programming language. Punch cards used to be in common use, however under most modern operating systems the instructions are commonly saved as plain text files that can be edited with any text editor. These files are the source of the instructions that make up a program and so are sometimes referred to as '''source files''' but a more exclusive definition is '''source code'''.

When referring to '''source code''' or just '''source''', you are considering only the files that contain code, the actual text that makes up the functions (actions) for computer to execute. By referring to '''source files''' you are extending the idea to not only the files with the instructions that make up the program but all the raw files resources that together can '''build''' the program. The [[C++ Programming/Programming Languages/C++/Code/File Organization|File Organization Section]] will cover the different files used in C++ programming and best practices on handling them.

=== Keywords and identifiers ===
{{TODO|Complete Keywords,Specifier,Modifier, directives }}
''[[w:identifiers|Identifiers]]'' are names given to variables, functions, objects, etc. to refer to them in the program. C++ identifiers must start with a letter or an underscore character "<code>_</code>", possibly followed by a series of letters, underscores or digits. None of the C++ programming language keywords can be used as identifiers. Identifiers with successive underscores are reserved for use in the header files or by the compiler for special purpose, e.g. name mangling.

Some keywords exists to directly control the compiler's behavior, these keywords are very powerful and must be used with care, they may make a huge difference on the program's compile time and running speed. In the C++ Standard, these keywords are called '''''Specifiers'''''. 

Special considerations must be given when creating your own identifiers, this will be covered in [[C++ Programming/Programming Languages/C++/Code/Style Conventions/Naming Identifiers|Code Style Conventions Section]]. 

{{:C++ Programming/Programming Languages/C++/Code/Keywords}}

; Source code
Source code is the halfway point between human language and machine code. As mentioned before, it can be read by people to an extent, but it can also be parsed (converted) into machine code by a computer. The machine code, represented by a series of 1's and 0's, is the only code that the computer can directly understand and act on.

In a small program, you might have as little as a few dozen lines of code at the most, whereas in larger programs, this number might stretch into the thousands or even millions. For this reason, it is sometimes more practical to split large amounts of code across many files.  This makes it easier to read, as you can do it bit by bit, and it also reduces compile time of each source file. It takes much less time to compile a lot of small source files than it does to compile a single massive source file.

Managing size is not the only reason to split code, though. Often, especially when a piece of software is being developed by a large team, source code is split. Instead of one massive file, the program is divided into separate files, and each individual file contains the code to perform one particular set of tasks for the overall program. This creates a condition known as ''Modularity''. Modularity is a quality that allows source code to be changed, added to, or removed a piece at a time. This has the advantage of allowing many people to work on separate aspects of the same program, thereby allowing it to move faster and more smoothly. Source code for a large project should always be written with modularity in mind. Even when working with small or medium sized projects, it is good to get in the habit of writing code with ease of editing and use in mind.
 
C++ source code is [[w:Case sensitivity|case sensitive]]. This means that it distinguishes between lowercase and capital letters, so that it sees the words "hello," "Hello," and "HeLlO" as being totally different things.  This is important to remember and understand, it will be discussed further in the [[C++ Programming/Programming Languages/C++/Code/Style Conventions|Coding style conventions Section]].
{{BookCat}}
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
