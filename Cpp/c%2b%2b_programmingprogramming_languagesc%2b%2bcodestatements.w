>=== Statements ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== Statements ===
Most, if not all, programming languages share the concept of a ''statement'', also referred to as an ''expression''.
. A statement is a command the programmer gives to the computer.
<source lang=cpp>
// Example of a single statement
cout << "Hi there!";
</source>

Each valid C++ statement is terminated by a semicolon (<code>;</code>).  The above statement will be examined in detail later on, for now consider that this statement has a subject (the noun "<code>cout</code>"), a verb ("<code><<</code>", meaning "output" or "print"), and, in the sense of English grammar, an object (what to print). In this case, the subject "<code>cout</code>" means "the standard console output device", and the verb "<code><<</code>" means "output the object" &mdash; in other words, the command "<code>cout</code>" means "send to the standard output stream," (in this case we assume the default, the console).

The programmer either enters the statement directly to the computer (by typing it while running a special program, called interpreter), or creates a text file with the command in it (you can use any text editor for that), that is latter used with a [[C++ Programming/Programming Languages/C++/Code/Compiler|compiler]]. You could create a file called "hi.txt", put the above command in it, and save that file on the computer.

If one were to write multiple statements, it is recommended that each statement be entered on a separate line.

<source lang=cpp>
cout << "Hi there!";                   // a statement
cout << "Strange things are afoot..."; // another statement
</source>

However, there is no problem writing the code this way:

<source lang=cpp>
cout << "Hi there!"; cout << "Strange things are afoot...";
</source>

The former code gathers appeal in the developer circles. Writing statements as in the second example only makes your
code look more complex and incomprehensible. We will speak of this deeply in the [[C++ Programming/Programming Languages/C++/Code/Style Conventions|Coding style conventions Section]] of the book.

If you have more than one statement in the file, each will be performed in order, top to bottom. 

The computer will perform each of these statements sequentially. It is invaluable to be able to "play computer" when programming. Ask yourself, "If I were the computer, what would I do with these statements?" If you're not sure what the answer is, then you are very likely to write incorrect code. Stop and check the manual for the programming language you're using.

In the above case, the computer will look at the first statement, determine that it is a cout statement, look at what needs to be printed, and display that text on the computer screen. It'll look like this:

<pre>Hi there!</pre>

Note that the quotation marks are not there. Their purpose in the program is to tell the computer where the text begins and ends, just like in English prose. The computer will then continue to the next statement, perform its command, and the screen will look like this:

<pre>Hi there!Strange things are afoot...</pre>

When the computer gets to the end of the text file, it stops. There are many different kinds of statements, depending on which programming language is being used. For example, there could be a beep statement that causes the computer to output a beep on its speaker, or a window statement that causes a new window to pop up.

Also, the way statements are written will vary depending on the programming language. These differences are fairly superficial. The set of rules like the first two is called a programming language's syntax. The set of verbs is called its library.

<source lang=cpp>
cout << "Hi there!";
</source>

==== Compound statement ====
Also referred to as ''statement blocks'' or ''code blocks'', consist of one or more statements or commands that are contained between a pair of curly braces <tt>{ }</tt>. Such a block of statements can be named or be provided a condition for execution. Below is how you'd place a series of statements in a block.

<source lang=cpp>
// Example of a compound statement
{
  int a = 10;
  int b = 20;
  int result = a + b;
}
</source>

Blocks are used primarily in loops, conditionals and functions.  Blocks can be nested inside one another, for instance as an <tt>if</tt> structure inside of a loop inside of a function.

{{NOTE|Statement blocks also create a [[C++ Programming/Programming Languages/C++/Code/Statements/Scope|local scope]].}}

; Program Control Flow
As seen above the statements are evaluated in the order as they occur (sequentially). The execution of flow begins at the top most statement and proceed downwards till the last statement is encountered. Any single statement can be substituted by a compound statement. There are special statements that can redirect the execution flow based on a condition, those statements are called ''branching'' statements, described in detail in the [[C++ Programming/Programming Languages/C++/Code/Statements/Flow Control|Control Flow Construct Statements Section]] of the book.

<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
{{BookCat}}
