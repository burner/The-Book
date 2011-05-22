>== What is a programming language? ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== What is a programming language? ==
In the most basic terms, a "programming language" is a means of communication between a human being (programmer) and a computer. A programmer uses this means of communication in order to give the computer instructions.  These instructions are called "programs".

Like the many languages we use to communicate with each other, there are many languages that a programmer can use to communicate with a computer.  Each language has its own set of words and rules, called semantics.  If you're going to write a program, you have to follow the semantics of the language you're writing in, or you won't be understood.

Programming languages can basically be divided in to two categories:  [[w:Low-level programming language|Low-Level]] and [[w:High-level programming language|High-level]], next we will introduce you to these concepts and their relevance to C++.

=== Low-level ===
[[Image:TaxonomyofProgrammingLanguages.png|thumb|right|200px|Image shows most programming languages and their relations from mid 18 hundreds up to 2003 ([[Media:TaxonomyofProgrammingLanguages.png|click here for full size]]).]]
The lower level in computer "languages" are: 

'''Machine code''' (also called binary) is the lowest form of a low-level language. Machine code consists of a string of 0s and 1s, which combine to form meaningful instructions that computers can take action on. If you look at a page of binary it becomes apparent why binary is never a practical choice for writing programs; what kind of person would actually be able to remember what a bunch of strings of 1 and 0 mean ? 

'''Assembly language''' (also called ASM), is just above machine code on the scale from low level to high level. It is a human-readable translation of the machine language instructions the computer executes.  For example, instead of referring to processor instructions by their binary representation (0s and 1s), the programmer refers to those instructions using a more memorable (mnemonic) form. These mnemonics are usually short collections of letters that symbolize the action of the respective instruction, such as "ADD" for addition, and "MOV" for moving values from one place to another.

{{NOTE|Assembly language is ''processor specific''. This means that a program written in assembly language will not work on computers with different processor architectures.}}

You do not have to understand assembly language to program in C++, but it does help to have an idea of what's going on "behind-the-scenes". Learning about assembly language will also allow you to have more control as a programmer and help you in debugging and understanding code.

The advantages of writing in a high-level language format far outweigh any drawbacks, due to the size and complexity of most programming tasks, those advantages include:

* Advanced program structure: loops, functions, and objects all have limited usability in low-level languages, as their existence is already considered a "high" level feature; that is, each structure element must be further translated into low-level language.
* Portability: high-level programs can run on different kinds of computers with few or no modifications. Low-level programs often use specialized functions available on only certain processors, and have to be rewritten to run on another computer.
* Ease of use: many tasks that would take many lines of code in assembly can be simplified to several function calls from libraries in high-level programming languages. For example, Java, a high-level programming language, is capable of painting a functional window with about five lines of code, while the equivalent assembly language would take at least four times that amount.

{{NOTE|Using ASM to optimize certain tasks is common for C++ programmers. But remember that ASM is not as portable. }}

=== High-level ===
High-level languages do more with less code, although there is sometimes a loss in performance and less freedom for the programmer. They also attempt to use English language words in a form which can be read and generally interpreted by the average person with little experience in them. A program written in one of these languages is sometimes referred to as "human-readable code".  In general, more abstraction makes it easier for a language be learned. 

No programming language is written in what one might call "plain English" though, (although BASIC comes close). Because of this, the text of a program is sometimes referred to as "code", or more specifically as "source code."  This is discussed in more detail in the [[C++ Programming/Programming Languages/C++/Code|The Code Section]] of the book.

Higher-level languages partially solve the problem of abstraction to the hardware (CPU, co-processors, number of registers etc...) by providing portability of code.

Keep in mind that this classification scheme is evolving. C++ is still considered a high-level language, but with the appearance of newer languages (Java, C#, Ruby etc...), C++ is beginning to be grouped with lower level languages like C.

=== Translating programming languages ===
Since a computer is only capable of understanding machine code, human-readable code must be either interpreted or translated into machine code.

An '''Interpreter''' is a program (often written in a lower level language) that interprets the instructions of a program one instruction at a time into commands that are to be carried out by the interpreter as it happens. Typically each instruction consists of one line of text or provides some other clear means of telling each instruction apart and the program must be reinterpreted again each time the program is run.

A '''[[w:Compiler|Compiler]]''' is a program used to translate the source code, one instruction at a time, into machine code. The translation into machine code may involve splitting one instruction understood by the compiler into multiple machine instructions. The instructions are only translated once and after that the machine can understand and follow the instructions directly whenever it is instructed to do so. A complete examination of the C++ compiler is given in the [[C++ Programming/Programming Languages/C++/Code/Compiler|Compiler Section]] of the book.

The words and statements used to instruct the computer may differ, but no matter what words and statements are used, just about every programming language will include statements that will accomplish the following:

;''Input'': Input is the act of getting information from a device such as a keyboard or mouse, or sometimes another program.

;''Output'': Output is the opposite of input; it gives information to the computer monitor or another device or program. 

;''Math''/''Algorithm'': All computer processors (the brain of the computer), have the ability to perform basic mathematical computation, and every programming language has some way of telling it to do so. 

;''Testing'': Testing involves telling the computer to check for a certain condition and to do something when that condition is true or false. Conditionals are one of the most important concepts in programming, and all languages have some method of testing conditions. 

;''Repetition'': Perform some action repeatedly, usually with some variation.

An further examination is provided on the [[C++ Programming/Programming Languages/C++/Code/Statements|Statements Section]] of the book. 

Believe it or not, that's pretty much all there is to it. Every program you've ever used, no matter how complicated, is made up of functions that look more or less like these. Thus, one way to describe programming is the process of breaking a large, complex task up into smaller and smaller subtasks until eventually the subtasks are simple enough to be performed with one of these simple functions.

C++ is mostly ''compiled'' rather than ''interpreted'' (there are some C++ interpreters), and then "executed" later. As complicated as this may seem, later you will see how easy it really is.

So as we have seen in the [[C++ Programming/Programming Languages/C++|Introducing C++ Section]], C++ evolved from C by adding some levels of abstraction (so we can correctly state that C++ is of a higher level than C). We will learn the particulars of those differences in the [[C++ Programming/Programming Languages/Paradigms|Programming Paradigms Section]] of the book and for some of you that already know some other languages should look into [[C++ Programming/Programming Languages/Comparisons|Programming Languages Comparisons Section]]. 

{{BookCat}}
