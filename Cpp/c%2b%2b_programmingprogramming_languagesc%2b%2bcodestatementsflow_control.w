>== Control flow statements ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== Control flow statements ==
Usually a program is not a linear sequence of instructions. It may repeat code or take decisions for a given path-goal relation. Most programming languages have '''control flow''' statements (constructs) which provide some sort of control structures that serve to specify order to what has to be done to perform our program that allow variations in this sequential order:
* statements may only be obeyed under certain conditions (conditionals),
* statements may be obeyed repeatedly under certain conditions (loops),
* a group of remote statements may be obeyed (subroutines).

;Logical Expressions as conditions : Logical expressions can use logical operators in loops and conditional statements as part of the conditions to be met.

===Exceptional and unstructured control flow===
Some instructions have no particular structure but will have an exceptional usefulness in shaping how other control flow statements are structured, a special care must be taken to prevent unstructured and confusing programming.

====break====
A '''break''' will force the exiting of the present loop iteration into the next statement outside of the loop. It has no usefulness outside of a loop structure except for the '''switch''' control statement.

====continue====
The '''continue''' instruction is used inside loops where it will stop the current loop iteration, initiating the next one.

==== {{C++ Programming/kw|goto}} ====
{{:C++ Programming/Programming Languages/C++/Code/Keywords/goto}}

====abort(), exit() and atexit()====
As we will see later the [[C++ Programming/Code/Standard C Library|Standard C Library]] that is included in C++ also supplies some useful functions that can alter the flow control. Some will permits you to terminate the execution of a program and enables you to set up a return value or initiate special tasks upon the termination request. You will have to jump ahead into the [[C++ Programming/Code/Standard C Library/Functions/abort|abort()]] - [[C++ Programming/Code/Standard C Library/Functions/exit|exit()]] - [[C++ Programming/Code/Standard C Library/Functions/atexit|atexit()]] sections for more information.

=== Conditionals ===
There is likely no meaningful program written in which a computer does not demonstrate basic decision-making skills. It can actually be argued that there is no meaningful human activity in which no decision-making, instinctual or otherwise, takes place. For example, when driving a car and approaching a traffic light, one does not think, "I will continue driving through the intersection." Rather, one thinks, "I will stop if the light is red, go if the light is green, and if yellow go only if I am traveling at a certain speed a certain distance from the intersection." These kinds of processes can be simulated using conditionals.

A conditional is a statement that instructs the computer to execute a certain block of code or alter certain data only if a specific condition has been met.

The most common conditional is the if-else statement, with conditional expressions and switch-case statements typically used as more shorthanded methods.

==== if (Fork branching) ====
The if-statement allows one possible path choice depending on the specified conditions.

'''Syntax'''
<source lang=cpp>
if (condition)
{
  statement;
}
</source>

'''Semantic'''

First, the condition is evaluated:
*if ''condition'' is true, ''statement'' is executed before continuing with the body.
*if ''condition'' is false, the program skips ''statement'' and continues with the rest of the program.

{{NOTE|The condition in an '''if''' statement can be any code that resolves in any expression that will evaluate to either a boolean, or a null/non-null value; you can declare variables, nest statements, etc. This is true to other flow control conditionals (ie: while), but is generally regarded as bad style, since it only benefit is ease of typing by making the code less readable.

This characteristic can easily lead simple errors, like tipping <nowiki>a=b</nowiki> (assign a value) in place of a <nowiki>a==b</nowiki> (condition). This has resulted in the adoption of a coding practice that would automatically put the errors in evidence, by inverting the expression (or using constant variables) the compiler will generate an error.

Recent compilers support the detection of such events and generate compilation warnings.}}

'''Example'''
<source lang=cpp>
if(condition)
{
  int x; // Valid code
  for(x = 0; x < 10; ++x) // Also valid.
    {
      statement;
    }
}
</source>

[[Image:Ficondforless10.png|center|flowchart from the example]]

{{NOTE|If you wish to avoid typing std::cout, std::cin, or std::endl; all the time, you may include
'''using''' '''namespace''' std at the beginning of your program since cout, cin, and endl are members of the ''std'' namespace.}}

Sometimes the program needs to choose one of two possible paths depending on a condition. For this we can use the if-else statement.
<source lang=cpp>
if (user_age < 18)
{
    std::cout << "People under the age of 18 are not allowed." << std::endl;
}
else
{
    std::cout << "Welcome to Caesar's Casino!" << std::endl;
}
</source>

Here we display a message if the user is under 18. Otherwise, we let the user in. The if part is executed only if 'user_age' is less than 18. In other cases (when 'user_age' is greater than or equal to 18), the else part is executed.

if conditional statements may be chained together to make for more complex condition branching. In this example we expand the previous example by also checking if the user is above 64 and display another message if so.
<source lang=cpp>
if (user_age < 18)
{
  std::cout << "People under the age of 18 are not allowed." << std::endl;
}
else if (user_age > 64)
{
  std::cout << "Welcome to Caesar's Casino! Senior Citizens get 50% off." << std::endl;
}
else
{
  std::cout << "Welcome to Caesar's Casino!" << std::endl;
}
</source>

[[Image:Elseifage.png|center|flowchart from the example]]

{{NOTE|
*'''break''' and '''continue''' do not have any relevance to an '''if''' or '''else'''.
*Although you can use multiple '''else if''' statements, when handling many related conditions it is recommended that you use the '''switch''' statement, which we will be discussing next.}}

==== switch (Multiple branching) ====
The switch statement branches based on specific integer values. 

 '''switch''' (''integer expression'') {
     '''case''' ''label1'':
          ''statement(s)''
          '''break''';
     '''case''' ''label2'':
          ''statement(s)''
          '''break''';
     /* ... */
     '''default''':
          ''statement(s)''
 }

As you can see in the above scheme the case and default have a "break;" statement at the end of block.
This expression will cause the program to exit from the switch, if break is not added the program will continue 
execute the code in other cases even when the integer expression is not equal to that case. This can
be exploited in some cases as seen in the next example.

We want to separate an input from digit to other characters.

<source lang=cpp>
 char ch = cin.get(); //get the character
 switch (ch) {
     case '0': 
          // do nothing fall into case 1
     case '1': 
         // do nothing fall into case 2
     case '2': 
        // do nothing fall into case 3
     /* ... */
     case '8': 
        // do nothing fall into case 9
     case '9':  
          std::cout << "Digit" << endl; //print into stream out
          break;
     default:
          std::cout << "Non digit" << endl; //print into stream out
 }
</source>

In this small piece of code for each digit below '9' it will propagate through the cases until it will reach case '9'
and print "digit".

If not it will go straight to the default case there it will print "Non digit"

{{NOTE|note=
* Be sure to use '''break''' commands unless you want multiple conditions to have the same action. Otherwise, it will "fall through" to the next set of commands.
* '''break''' can only break out of the innermost level. If for example you are inside a '''switch''' and need to break out of a enclosing {{C++ Programming/kw|for}} loop you might well consider adding a boolean as a flag, and check the flag after the '''switch''' block instead of the alternatives available. (Though even then, refactoring the code into a separate function and returning from that function might be cleaner depending on the situation, and with inline functions and/or smart compilers there need not be any runtime overhead from doing so.)
* '''continue''' is not relevant to '''switch''' block. Calling '''continue''' within a '''switch''' block will lead to the "continue" of the loop which wraps the '''switch''' block.
}}

=== Loops (iterations) ===
A loop (also referred to as an iteration or repetition) is a sequence of statements which is specified once but which may be carried out several times in succession. The code "inside" the loop (the ''body'' of the loop) is obeyed a specified number of times, or once for each of a collection of items, or until some condition is met.

[[w:iteration|Iteration]] is the repetition of a process, typically within a computer program. Confusingly, it can be used both as a general term, synonymous with repetition, and to describe a specific form of repetition with a [[w:Mutable object|mutable]] state.

When used in the first sense, [[w:Recursion|recursion]] is an example of iteration.

However, when used in the second (more restricted) sense, iteration describes the style of programming used in imperative programming languages. This contrasts with recursion, which has a more declarative approach.

Due to the nature of C++ there may lead to an even bigger problems when differentiating the use of the word, so to simplify things use "'''loops'''" to refer to simple recursions as described in this section and use ''iteration'' or [[w:iterator|''iterator'']] (the "one" that performs an ''iteration'') to class ''iterator'' (or in relation to objects/classes) as used in the STL.    

;'''Infinite Loops'''
Sometimes it is desirable for a program to loop forever, or until an exceptional condition such as an error arises. For instance, an event-driven program may be intended to loop forever handling events as they occur, only stopping when the process is killed by the operator.

More often, an infinite loop is due to a programming error in a condition-controlled loop, wherein the loop condition is never changed within the loop. 

<source lang=cpp>
// as we will see, these are infinite loops...
while (1) { }

// or

for (;;) { }
</source>


{{NOTE|When the compiler optimizes the source code, all statement after the detected infinite loop (that will never run), will be ignored. A compiler warning is generally given on detecting such cases.}}

;'''Condition-controlled loops'''
Most programming languages have constructions for repeating a loop until some condition changes.

Condition-controlled loops are divided into two categories Preconditional or Entry-Condition that place the test at the start of the loop, and Postconditional or Exit-Condition iteration that have the test at the end of the loop.  In the former case the body may be skipped completely, while in the latter case the body is always executed at least once.

In the condition controlled loops, the keywords ''break'' and ''continue'' take significance.  The break keyword causes an exit from the loop, proceeding with the rest of the program.  The ''continue'' keyword terminates the current iteration of the loop, the loop proceeds to the next iteration.

==== while (Preconditional loop) ====

'''Syntax'''
<source lang=cpp>
while (''condition'') ''statement''; ''statement2'';
</source>

'''Semantic'''
First, the condition is evaluated:<br/>
#if ''condition'' is true, ''statement'' is executed and ''condition'' is evaluated again.
#if ''condition'' is false continues with ''statement2'' 
'''Remark''': ''statement''  can be a block of code { ... } with several instructions.

What makes 'while' statements different from the 'if' is the fact that once the body (referred to as ''statement'' above) is executed, it will go back to 'while' and check the condition again. If it is true, it is executed again. In fact, it will execute as many times as it has to until the expression is false.

'''Example 1'''
<source lang=cpp>
#include <iostream>
using namespace std;
 
int main() 
{
  int i=0;
  while (i<10) {
    cout << "The value of i is " << i << endl;
    i++;
  }
  cout << "The final value of i is : " << i << endl;
  return 0;
}
</source>

'''Execution'''
  The value of i is 0
  The value of i is 1
  The value of i is 2
  The value of i is 3
  The value of i is 4
  The value of i is 5
  The value of i is 6
  The value of i is 7
  The value of i is 8
  The value of i is 9
  The final value of i is 10

'''Example 2'''
<source lang=cpp>
// validation of an input
#include <iostream>
using namespace std;
 
int main() 
{
  int a;
  bool ok=false;
  while (!ok) {
    cout << "Type an integer from 0 to 20 : ";
    cin >> a;
    ok = ((a>=0) && (a<=20));
    if (!ok) cout << "ERROR - ";
  }
  return 0;
}
</source>

'''Execution'''
  Type an integer from 0 to 20 : 30
  ERROR - Type an integer from 0 to 20 : 40
  ERROR - Type an integer from 0 to 20 : -6
  ERROR - Type an integer from 0 to 20 : 14

==== do-while (Postconditional loop) ====
'''Syntax'''
<source lang=cpp>
do {
  statement(s)
} while (condition);
 
statement2;
</source>

'''Semantic'''
#''statement(s)'' are executed.
#''condition'' is evaluated.
#if ''condition'' is true goes to 1).
#if ''condition'' is false continues with ''statement2''

The do - while loop is similar in syntax and purpose to the while loop. The construct moves the test that continues condition of the loop to the end of the code block so that the code block is executed at least once before any evaluation.

'''Example'''
<source lang=cpp>
#include <iostream>

using namespace std;
 
int main() 
{
  int i=0;

  do {
    cout << "The value of i is " << i << endl;
    i++;
  } while (i<10);

  cout << "The final value of i is : " << i << endl;
  return 0;
}
</source>

'''Execution'''
 The value of i is 0
 The value of i is 1
 The value of i is 2
 The value of i is 3
 The value of i is 4
 The value of i is 5
 The value of i is 6
 The value of i is 7
 The value of i is 8
 The value of i is 9
 The final value of i is 10

==== {{C++ Programming/kw|for}} (Preconditional and counter-controlled loop) ====
{{:C++ Programming/Programming Languages/C++/Code/Keywords/for}}

{{BookCat}}
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
