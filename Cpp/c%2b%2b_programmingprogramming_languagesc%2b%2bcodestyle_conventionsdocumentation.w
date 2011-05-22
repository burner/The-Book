>=== Document your code ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== Document your code ===
There are a number of good reasons to document your code, and a number of aspects of it that can be documented.  Documentation provides you with a shortcut for obtaining an overview of the system or for understanding the code that provides a particular feature.

{{SAMPLE|sampletext=
"Good code is its own best documentation."|caption=—Steve McConnell}}

==== Why? ====
The purpose of comments is to explain and clarify the source code to anyone examining it (or just as a reminder to yourself). Good commenting conventions are essential to any non-trivial program so that a person reading the code can understand what it is expected to do and to make it easy to follow on the rest of the code. In the next topics some of the most '''How?''' and '''When?''' rules to use comments will be listed for you.

Documentation of programming is essential when programming not just in C++, but in any programming language. Many companies have moved away from the idea of "hero programmers" (i.e., one programmer who codes for the entire company) to a concept of groups of programmers working in a team. Many times programmers will only be working on small parts of a larger project. In this particular case, documentation is essential because:
* Other programmers may be tasked to develop your project;
* Your finished project may be submitted to editors to assemble your code into other projects;
* A person other than you may be required to read, understand, and present your code.

Even if you are not programming for a living or for a company, documentation of your code is still essential. Though many programs can be completed in a few hours, more complex programs can take longer time to complete (days, weeks, etc.). In this case, documentation is essential because:
* You may not be able to work on your project in one session;
* It provides a reference to what was changed the last time you programmed;
* It allows you to record ''why'' you made the decisions you did, including why you chose '''not''' to explore certain solutions;
* It can provide a place to document known limitations and bugs (for the latter a defect tracking system may be the appropriate place for documentation);
* It allows easy searching and referencing within the program (from a non-technical stance);
* It is considered to be good programming practice.

===== Comments should be written for the appropriate audience =====
When writing code to be read by those who are in the initial stages of learning a new programming language, it can be helpful to include a lot of comments about what the code does.  For "production" code, written to be read by professionals, it is considered unhelpful and counterproductive to include comments which say things that are already clear in the code. Some from the [[w:Extreme Programming|Extreme Programming]] community say that excessive commenting is indicative of [[w:code smell|code smell]] -- which is ''not'' to say that comments are bad, but that they are often a clue that code would benefit from [[w:refactoring|refactoring]]. Adding comments as an alternative to writing understandable code is considered poor practice.

==== What? ====
What needs to be documented in a program/source code can be divided into what is documented before the specific program execution (that is before "main") and what is executed ("what is in main").

Documentation before program execution:
* Programmer information and license information (if applicable)
* User defined function declarations
* Interfaces
* Context
* Relevant standards/specifications
* Algorithm steps
* How to convert the source code into executable file(s) (perhaps by using [[make]])

Documentation for code inside main:
* Statements, Loops, and Cases
* Public and Private Sectors within Classes
* Algorithms used
* Unusual features of the implementation
* Reasons why other choices have been avoided
* User defined function implementation

If used carelessly comments can make source code hard to read and maintain and may be even unnecessary if the code is self-explanatory -- but remember that what seems self-explanatory today may not seem the same six months or six years from now.

===== Document decisions  =====
Comments should document decisions. At every point where you had a choice of what to do place a comment describing which choice you made and why. Archaeologists will find this the most useful information.

===== Comment layout =====
Each part of the project should at least have a single comment layout, and it would be better yet to have the complete project share the same layout if possible.<br>

{{TODO|Add more here.|C++ Programming}}

==== How? ====
Documentation can be done within the source code itself through the use of comments (as seen above) in a language understandable to the intended audience. It is good practice to do it in English as the C++ language is itself English based and English being the current [[w:Lingua franca|lingua franca]] of international business, science, technology and aviation, you will ensure support for the broadest audience possible.

Comments are useful in documenting portions of an algorithm to be executed, explaining function calls and variable names, or providing reasons as to why a specific choice or method was used. Block comments are used as follows:

<source lang=cpp>
/*
get timepunch algorithm - this algorithm gets a time punch for use later
1. user enters their number and selects "in" or "out"
2. time is retrieved from the computer
3. time punch is assigned to user
*/
</source>

Alternately, line comments can be used as follows:

<source lang=cpp>
GetPunch(user_id, time, punch); //this function gets the time punch
</source>

An example of a full program using comments as documentation is:

<source lang=cpp>
/*
Chris Seedyk
BORD Technologies
29 December 2006
Test
*/
int main()
{
 cout << "Hello world!" << endl; //predefined cout prints stuff in " " to screen
 return 0;
}
</source>

It should be noted that while comments are useful for in-program documentation, it is also a good idea to have an external form of documentation separate from the source code as well, but remember to think first on how the source will be distributed before making references to external information on the code comments.

Commenting code is also no substitute for well-planned and meaningful variable, function, and class names. This is often called "self-documenting code," as it is easy to see from a carefully chosen and descriptive name what the variable, function, or class is meant to do. To illustrate this point, note the relatively equal simplicity with which the following two ways of documenting code, despite the use of comments in the first and their absence in the second, are understood. The first style is often encountered in very old C source by people who understood well what they were doing and had no doubt anyone else might not comprehend it. The second style is more "human-friendly" and while much easier to read is nevertheless not as frequently encountered.

<source lang=cpp>
// Returns the area of a triangle cast as an int
int area_ftoi(float a, float b) { return (int) a * b / 2; }

int iTriangleArea(float fBase, float fHeight)
{
   return (int) fBase * fHeight / 2;
}
</source>

Both functions perform the same task, however the second has such practical names chosen for the function and the variables that its purpose is clear even without comments. As the complexity of the code increases, well-chosen naming schemes increase vastly in importance.

Regardless of what method is preferred, comments in code are helpful, save time (and headaches), and ensure that both the author and others understand the layout and purpose of the program fully.

===== Automatic documentation =====
Various tools are available to help with documenting C++ code; [[w:Literate Programming|Literate Programming]] is a whole school of thought on how to approach this, but a very effective tool is [http://www.doxygen.org Doxygen] (also supports several languages), it can even use hand written comments in order to generate more than the bare structure of the code, bringing Javadoc-like documentation comments to C++ and can generate documentation in HTML, PDF and other formats.

===== Comments should tell a story =====
Consider your comments a story describing the system. Expect your comments to be extracted by a robot and formed into a manual page. Class comments are one part of the story, method signature comments are another part of the story, method arguments another part, and method implementation yet another part. All these parts should weave together and inform someone else at another point of time just exactly what you did and why.

; Do not use comments for flowcharts or pseudo-code
You should refrain from using comments to do ASCII art or pseudo-code (some programmers attempt to explain their code with an ASCII-art flowchart). If you want to flowchart or otherwise model your design there are tools that will do a better job at it using standardized methods. See for example: [[w:Unified Modeling Language|UML]].
{{BookCat}}
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
