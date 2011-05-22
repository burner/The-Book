>== I/O ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== I/O ==
Also commonly referenced as the C++ I/O of the C++ standard since the standard also includes the C Standard library and its I/O implementation, as seen before in the [[C++ Programming/Code/Standard C Library/IO#Standard C I/O|Standard C I/O Section]].

''Input'' and ''output'' are essential for any computer software, as these are the only means by which the program can communicate with the user. The simplest form of input/output is pure textual, i.e. the application displays in console form, using simple ASCII characters to prompt the user for inputs, which are supplied using the keyboard.

There are many ways for a program to gain input and output, including
* File i/o, that is, reading and writing to files
* Console i/o, reading and writing to a console window, such as a terminal in UNIX-based operating systems or a DOS prompt in Windows.
* Network i/o, reading and writing from a network device
* String i/o, reading and writing treating a string as if it were the input or output device

While these may seem unrelated, they work very similarly. In fact, operating systems that follow the POSIX specification deal with files, devices, network sockets, consoles, and many other things all with one type of handle, a file descriptor. However, low-level interfaces provided by the operating system tend to be difficult to use, so C++, like other languages, provide an abstraction to make programming easier. This abstraction is the '''stream'''.

=== Character encoding ===
{{TODO|Complete character encoding info}}

{{:C++ Programming/ASCII}}

=== Streams ===
A stream is a type of object from which we can take values, or to which we can pass values. This is done transparently in terms of the underlying code that demonstrates the use of the <tt>std::cout</tt> stream, known as the ''standard output stream''.
{{:C++ Programming/Examples/Hello World}}

Almost all input and output one ever does can be modeled very effectively as a stream. Having one common model means that one only has to learn it once. If you understand streams, you know the basics of how to output to files, the screen, sockets, pipes, and anything else that may come up.

A stream is an object that allows one to push data in or out of a medium, in order. Usually a stream can only output or can only input. It is possible to have a stream that does both, but this is rare. One can think of a stream as a car driving along a one-way street of information. An output stream can insert data and move on. It (usually) cannot go back and adjust something it has already written. Similarly, an input stream can read the next bit of data and then wait for the one that comes after it. It does not skip data or rewind and see what it had read 5 minutes ago.

The semantics of what a stream's read and write operations do depend on the type of stream. In the case of a file, an input file stream reads the file's contents in order without rewinding, and an output file stream writes to the file in order. For a console stream, output means displaying text, and input means getting input from the user via the console. If the user has not inputted anything, then the program ''blocks'', or waits, for the user to enter in something.

{{TODO|Remember to complete with the String Stream class and include the various io stream format flags.|C++ Programming}}

==== iostream ====
'''<code>iostream</code>''' is a [[C++ Programming/Programming Languages/C++/Code/File Organization#.h|header file]] used for input/output. Part of the C++ standard library. The name stands for '''I'''nput/'''O'''utput '''Stream'''.  In C++ there is no special syntax for streaming data input or output. Instead, these are combined as a [[C++ Programming/Compiler/Linker/Libraries|library]] of functions. Like we have seen with the [[C++ Programming/Code/Standard C Library/IO#Standard C I/O|C Standard Library use of <code><cstdio></code> header]], <code>iostream</code> provides basic OOP services for I/O.

The <iostream> automatically defines and uses a few standard objects:
* '''<code>cin</code>''', an object of the istream class that reads data from the standard input device.
* '''<code>cout</code>''', an object of the ostream class, which displays data to the standard output device.
* '''<code>cerr</code>''', another object of the ostream class that writes unbuffered output to the standard error device.
* '''<code>clog</code>''', like cerr, but uses buffered output.
for sending data to and from the [[w:standard streams|standard streams]] input, output, error (unbuffered), and error (buffered) respectively. As part of the C++ standard library, these objects are a part of the '''''std namespace'''''.

;Standard input, output, and error 
The most common streams one uses are '''<tt>cout</tt>''', '''<tt>cin</tt>''', and '''<tt>cerr</tt>''' (pronounced "c out", "c in", and "c err(or)", respectively). They are defined in the header '''<tt><iostream></tt>'''. Usually, these streams read and write from a console or terminal. In UNIX-based operating systems, such as Linux and Mac OS X, the user can redirect them to other files, or even other programs, for logging or other purposes. They are analogous to <tt>stdout</tt>, <tt>stdin</tt>, and <tt>stderr</tt> found in C. <tt>cout</tt> is used for generic output, <tt>cin</tt> is used for input, and <tt>cerr</tt> is used for printing errors. (<tt>cerr</tt> typically goes to the same place as <tt>cout</tt>, unless one or both is redirected, but it is not buffered and allows the user to fine-tune which parts of the program's output is redirected where.)

===== Output =====
The standard syntax for outputting to a stream, in this case, <tt>cout</tt>, is

<source lang=cpp>cout << some_data << some_more_data;</source>

'''Example'''
<source lang=cpp>
#include <iostream>

using namespace std;

int main()
{
  int a = 1;
  cout << "Hello world! " << a << '\n';

  return 0;
}
</source>

'''Result of Execution'''
 Hello world! 1

To add a line break, send a newline character, <tt>\n</tt> or use <code>std::endl</code>, which writes a newline and flushes the stream's buffer.

'''Example'''
<source lang=cpp>
#include <iostream>
#include <ostream>

using namespace std;

int main()
{
  int a = 1;
  char x = 13;
  cout << "Hello world!" << "\n" << a << endl << x << endl;

  return 0;
}
</source>

'''Execution'''
 Hello world!
 1

It is always a good idea to end your output with a blank line, so as to not mess up with user's terminals.

As seen in the "Hello World!" program, we direct the output to <code>std::cout</code>. This means that it is a ''member'' of the ''standard library''. For now, don't worry about what this means; we will cover the library and namespaces in later chapters.

What you do need to remember is that, in order to use the output stream, you must include a reference to the standard IO library, as shown here:
<source lang=cpp>#include <iostream></source>

This opens up a number of streams, functions and other programming devices which we can now use. For this section, we are interested in two of these; <code>std::cout</code> and <code>std::endl</code>.

Once we have referenced the standard IO library, we can use the output stream very simply. To use a stream, give its name, then ''pipe'' something in or out of it, as shown:
<source lang=cpp>std::cout << "Hello, world!";</source>

The <tt><nowiki><<</nowiki></tt> operator feeds everything to the right of it into the stream. We have essentially fed a text object into the stream. That's as far as our work goes; the stream now decides what to do with that object. In the case of the output stream, it's printed on-screen.

We're not limited to only sending a single object type to the stream, nor indeed are we limited to one object a time. Consider the examples below:
<source lang=cpp>
 std::cout << "Hello, " << "Joe"<< std::endl;
 std::cout << "The answer to life, the universe and everything is " << 42 << std::endl;
</source>

As can be seen, we feed in various values, separated by a pipe character. The result comes out something like:

 Hello, Joe
 The answer to life, the universe and everything is 42

You will have noticed the use of <tt>std::endl</tt> throughout some of the examples so far. This is the newline constant. It is a member of the standard IO library, and comes "free" when we instantiate that in order to use the output stream. When the output stream receives this constant, it starts a new line in the console.

And of course, we're not limited to sending only ONE newline, either:
<source lang=cpp>
 std::cout << "Hello, " << "Joe" << std::endl << std::endl;
 std::cout << "How old are you?";
</source>

Which produces something like:

 Hello, Joe
 
 How old are you?

===== Input =====
What would be the use of an application that only ever outputted information, but didn't care about what its users wanted? Minimal to none. Fortunately, inputting is as easy as outputting when you're using the stream.

The ''standard input stream'' is called <tt>std::cin</tt> and is used very similarly to the output stream. Once again, we instantiate the standard IO library:

<source lang=cpp>
#include <iostream>
</source>
This gives us access to <tt>std::cin</tt> (and the rest of that class). Now, we give the name of the stream as usual, and pipe output from it into a variable. A number of things have to happen here, demonstrated in the example below:

<source lang=cpp>
#include <iostream>
int main(int argc, char argv[]) {
  int a;
  std::cout << "Hello! How old are you? ";
  std::cin >> a;
  std::cout << "You're really " << a << " years old?" << std::endl;
 
  return 0;
}
</source>

We instantiate the standard IO library as usual, and call our main function in the normal way. Now we need to consider where the user's input goes. This calls for a variable (discussed in a later chapter) which we declare as being called <tt>a</tt>.

Next, we send some output, asking the user for their age. The real input happens now; everything the user types until they hit Enter is going to be stored in the input stream. We pull this out of the input stream and save it in our variable.

Finally, we output the user's age, piping the contents of our variable into the output stream.

Note: You will notice that if anything other than a whole number is entered, the program will crash. This is due to the way in which we set up our variable. Don't worry about this for now; we will cover variables later on.

===== A Program Using User Input ===== 
The following program inputs two numbers from the user and prints their sum:
<source lang=cpp>
 #include <iostream>
 
 int main()
 {
    int num1, num2;
    std::cout << "Enter number 1: ";
    std::cin >> num1;
    std::cout << "Enter number 2: ";
    std::cin >> num2;
    std::cout << "The sum of " << num1 << " and " << num2 << " is "
               << num1 + num2 << ".\n";
    return 0;
 }
</source>

Just like <tt>std::cout</tt> which represents the standard output stream, the C++ library provides (and the <tt>iostream</tt> header declares) the object <tt>std::cin</tt> representing standard input, which usually gets input from the keyboard. The statement:

<source lang=cpp>
 std::cin >> num1;
</source>

uses the ''extraction operator'' (<tt>>></tt>) to get an integer input from the
user. When used to input integers, any leading whitespace is skipped, a sequence of valid digits optionally preceded by a <tt>+</tt> or <tt>-</tt> sign is read and the value stored in the variable. Any remaining characters in the user input are not ''consumed''. These would be considered next time some input operation is performed.

If you want the program to use a function from a specific namespace, normally you must specify which namespace the function is in.  The above example calls to <tt>cout</tt>, which is a member of the <tt>std</tt> namespace (hence <tt>std::cout</tt>).  If you want a program to specifically use the std namespace for an identifier, which essentially removes the need for all future scope resolution (e.g. <tt>std::</tt>), you could write the above program like this:

<source lang=cpp>
#include <iostream>
 
using namespace std;

int main()
{
    int num1, num2;
    cout << "Enter number 1: ";
    cin >> num1;
    cout << "Enter number 2: ";
    cin >> num2;
    cout << "The sum of " << num1 << " and " << num2 << " is "
               << num1 + num2 << ".\n";
    return 0;
}
</source>

Please note that 'std' namespace is the namespace defined by standard C++ library.

===== Manipulators =====
A manipulator is a function that can be passed as an argument to a stream in different circumstances. For example, the manipulator 'hex' will cause the stream object to format subsequent integer input to the stream in hexadecimal instead of decimal. Likewise, 'oct' results in integers displaying in octal, and 'dec' reverts back to decimal.

'''Example'''
<source lang="cpp">
#include <iostream>
using namespace std;

int main()
{
  cout << dec << 16 << ' ' << 10 << endl;
  cout << oct << 16 << ' ' << 10 << endl;
  cout << hex << 16 << ' ' << 10 << endl;

  return 0;
}
</source>

'''Execution'''
 16 10
 20 12
 10 a

There are many manipulators which can be used in conjunction with streams to simplify the formatting of input. For example, 'setw()' sets the field width of the data item next displayed. Used in conjunction with 'left' and 'right'(which set the justification of the data), 'setw' can easily be used to create columns of data.

'''Example'''
<source lang="cpp">
#include <iostream>
#include <iomanip>
using namespace std;

int main()
{
	cout << setw(10) << right << 90 << setw(8) << "Help!" << endl;
	cout << setw(10) << left << 45 << setw(8) << "Hi!" << endl;

	return 0;
}
</source>

'''Execution'''
         90   Help!
 45        Hi!

The data in the top row display at the right of the columns created by 'setw', while in the next row, the data is left justified in the column.
Please note the inclusion of a new library 'iomanip'. Most formatting manipulators require this library.

Here are some other manipulators and their uses:

{| class="wikitable" style="text-align: left"
|-
! Manipulator !! Function
|-
| boolalpha || displays boolean values as 'true' and 'false' instead of as integers.
|-
| noboolalpha || forces bools to display as integer values
|-
| showuppercase || converts strings to uppercase before displaying them
|-
| noshowuppercase || displays strings as they are received, instead of in uppercase
|-
| fixed || forces floating point numbers to display with a fixed number of decimal places
|-
| scientific || displays floating point numbers in scientific notation
|}

===== Buffers =====
Most stream objects, including 'cout' and 'cin', have an area in memory where the information they are transferring sits until it is asked for. This is called a 'buffer'. Understanding the function of buffers is essential to mastering streams and their use.

''' Example '''
<source lang="cpp">
#include <iostream>
using namespace std;

int main()
{
  int num1, num2;
  cin >> num1;
  cin >> num2;

  cout << "Number1: " << num1 << endl
       << "Number2: " << num2 << endl;

  return 0;
}
</source>

''' Execution 1 '''
 >74
 >27
 Number1: 74
 Number2: 27

The inputs are given separately, with a hard return between them. '>' denotes user input.

''' Execution 2 '''
 >74 27
 Number1: 74
 Number2: 27

The inputs are entered on the same line. They both go into the 'cin' stream buffer, where they are stored until needed. As 'cin' statements are executed, the contents of the buffer are read into the appropriate variables.

''' Execution 3 '''
 >74 27 56
 Number1: 74
 Number2: 27

In this example, 'cin' received more input than it asked for. The third number it read in, 56, was never inserted into a variable. It would have stayed in the buffer until 'cin' was called again. The use of buffers can explain many strange behaviors that streams can exhibit.

''' Example '''
<source lang="cpp">
#include <iostream>
using namespace std;

int main()
{
  int num1, num2, num3;
  cin >> num1 >> num2;

  cout << "Number1: " << num1 << endl
    << "Number2: " << num2 << endl;

  cin >> num3;

  cout << "Number3: " << num3 << endl;

  return 0;
}
</source>

''' Execution '''
 >45 89 37
 Number1: 45
 Number2: 89
 Number3: 37

Notice how all three numbers were entered at the same time in one line, but the stream only pulled them out of the buffer when they were asked for. This can cause unexpected output, since the user might accidentally put an extra space into his input. A well written program will test for this type of unexpected input and handle it gracefully.

==== ios ====
'''ios''' is a [[C++ Programming/Programming Languages/C++/Code/File Organization#.h|header file]] in the C++ standard library which defines several types and functions basic to the operation of iostreams. This header is typically included automatically by other iostream headers. Programmers rarely include it directly.

===== Typedefs =====
{|class="wikitable"
|-
! Name || description
|-
|<code>ios</code> || Supports the <code>ios</code> class from the old <code>iostream</code> library.
|-
|<code>streamoff</code> || Supports internal operations.
|-
|<code>streampos</code> || Holds the current position of the buffer pointer or file pointer.
|-
|<code>streamsize</code> || Specifies the size of the stream.
|-
|<code>wios</code> || Supports the <code>wios</code> class from the old <code>iostream</code> library.
|-
|<code>wstreampos</code> || Holds the current position of the buffer pointer or file pointer.
|}

=====Manipulators=====
{|class="wikitable"
|-
! Name || description
|-
|<code>boolalpha</code> || Specifies that variables of type <code>bool</code> appear as true or false in the stream.
|-
|<code>dec</code> || Specifies that integer variables appear in base 10 notation.
|-
|<code>fixed</code> || Specifies that a floating-point number is displayed in fixed-decimal notation.
|-
|<code>hex</code> || Specifies that integer variables appear in base 16 notation.
|-
|<code>internal</code> || Causes a number's sign to be left justified and the number to be right justified.
|-
|<code>left</code> || Causes text that is not as wide as the output width to appear in the stream flush with the left margin.
|-
|<code>noboolalpha</code> || Specifies that variables of type <code>bool</code> appear as 1 or 0 in the stream.
|-
|<code>noshowbase</code> || Turns off indicating the notational base in which a number is displayed.
|-
|<code>noshowpoint</code> || Displays only the whole-number part of floating-point numbers whose fractional part is zero.
|-
|<code>noshowpos</code> || Causes positive numbers to not be explicitly signed.
|-
|<code>noskipws</code> || Cause spaces to be read by the input stream.
|-
|<code>nounitbuf</code> || Causes output to be buffered and processed when the buffer is full.
|-
|<code>nouppercase</code> || Specifies that hexadecimal digits and the exponent in scientific notation appear in lowercase.
|-
|<code>oct</code> || Specifies that integer variables appear in base 8 notation.
|-
|<code>right</code> || Causes text that is not as wide as the output width to appear in the stream flush with the right margin.
|-
|<code>scientific</code> || Causes floating point numbers to be displayed using scientific notation.
|-
|<code>showbase</code> || Indicates the notational base in which a number is displayed.
|-
|<code>showpoint</code> || Displays the whole-number part of a floating-point number and digits to the right of the decimal point even when the fractional part is zero.
|-
|<code>showpos</code> || Causes positive numbers to be explicitly signed.
|-
|<code>skipws</code> || Cause spaces to not be read by the input stream.
|-
|<code>unitbuf</code> || Causes output to be processed when the buffer is not empty.
|-
|<code>uppercase</code> || Specifies that hexadecimal digits and the exponent in scientific notation appear in uppercase.
|}

=====Classes=====
{|class="wikitable"
|-
! Name || description
|-
|<code>basic_ios</code> || The template class describes the storage and member functions common to both input streams (of template class basic_istream) and output streams (of template class basic_ostream) that depend on the template parameters.
|-
|<code>fpos</code> || The template class describes an object that can store all the information needed to restore an arbitrary file-position indicator within any stream.
|-
|<code>ios_base</code> || The class describes the storage and member functions common to both input and output streams that do not depend on the template parameters.
|}

==== fstream ====
With <tt>cout</tt> and <tt>cin</tt>, we can do basic communication with the user. For more complex io, we would like to read to and write from files. This is done with a file stream classes, defined in the header <tt>'''<fstream>'''</tt>. '''<tt>ofstream</tt>''' is an output file stream, and '''<tt>ifstream</tt>''' is an input file stream.

;Files
To ''open'' a file, one can either call <tt>open</tt> on the file stream or, more commonly, use the constructor. One can also supply an open mode to further control the file stream. Open modes include
*ios::app Leaves the file's original contents and appends new data to the end.
*ios::out Outputs new data in the file, removing the old contents. (default for ofstream)
*ios::in  Reads data from the file. (default for ifstream)

'''Example'''
<source lang=cpp>
// open a file called Test.txt and write "HELLO, HOW ARE YOU?" to it
#include <fstream>

using namespace std;

int main()
{
  ofstream file1;

  file1.open("file1.txt", ios::app);
  file1 << "This data will be appended to the file file1.txt\n";
  file1.close();

  ofstream file2("file2.txt");
  file2 << "This data will replace the contents of file2.txt\n";

  return 0;
}
</source>

The call to close() can be omitted if you do not care about the return value (whether it succeeded); the destructors will call close when the object goes out of scope.

If an operation (e.g. opening a file) was unsuccessful, a flag is set in the stream object. You can check the flags' status using the bad() or fail() member functions, which return a boolean value. The stream object doesn't throw any exceptions in such a situation; hence manual status check is required. See reference for details on bad() and fail().

===== Text input until EOF/error/invalid input =====
Input from the stream <tt>infile</tt> to a variable <tt>data</tt> until one of the following:
*EOF reached on <tt>infile</tt>.
*An error occurs while reading from <tt>infile</tt> (e.g., connection closed while reading from a remote file).
*The input item is invalid, e.g. non-numeric characters, when <tt>data</tt> is of type <tt>'''int'''</tt>.

<source lang=cpp>
#include <iostream>

// ...

while (infile >> data)
{
    // manipulate data here
}
</source>

Note that the following is not correct:

<source lang=cpp>
#include <iostream>

// ...

while (!infile.eof())
{
    infile >> data; // wrong!
    // manipulate data here
}
</source>

This will cause the last item in the input file to be processed twice, because <tt>eof()</tt> does not return true until input ''fails'' due to EOF.

==== ostream ====
;Classes and output streams
It is often useful to have your own classes' instances compatible with the stream framework. For instance, if you defined the class Foo like this:
<source lang=cpp>
 class Foo
 {
 public:

	Foo() : x(1), y(2)
	{
	}

	int x, y;
 };
</source>

You will not be able to pass its instance to cout directly using the '<<' operator, because it is not defined for these two objects (Foo and ostream). What needs to be done is to define this operator and thus bind the user-defined class with the stream class.
<source lang=cpp>
 ostream& operator<<(ostream& output, Foo& arg)
 {
	output << arg.x << "," << arg.y;
	return output;
 }
</source>

Now this is possible:
<source lang=cpp>
 Foo my_object;
 cout << "my_object's values are: " << my_object << endl;
</source>

The operator function needs to have 'ostream&' as its return type, so chaining output works as usual between the stream and objects of type Foo:
<source lang=cpp>
 Foo my1, my2, my3;
 cout << my1 << my2 << my3;
</source>

This is because (cout << my1) is of type ostream&, so the next argument (my2) can be appended to it in the same expression, which again gives an ostream& so my3 can be appended and so on.

If you decided to restrict access to the member variables x and y (which is probably a good idea) within the class Foo, i.e.:

<source lang=cpp>
 class Foo
 {
 public:

	Foo() : x(1), y(2)
	{
	}

 private:
	int x, y;
 };
</source>

you will have trouble, because the global operator<< function doesn't have access to the private variables of its second argument. There are two possible solutions to this problem:

1. Within the class Foo, declare the operator<< function as the classes' friend which grants it access to private members, i.e. add the following line to the class declaration:

<source lang=cpp> 
 friend ostream& operator<<(ostream& output, Foo& arg);
</source>

Then define the operator<< function as you normally would (note that the declared function is not a member of Foo, just its friend, so don't try defining it as Foo::operator<<).

2. Add public-available functions for accessing the member variables and make the operator<< function use these instead:

<source lang=cpp>
 class Foo
 {
 public:

	Foo() : x(1), y(2)
	{
	}

	int get_x()
	{
		return x;
	}

	int get_y()
	{
		return y;
	}

 private:
	int x, y;
 };

 ostream& operator<<(ostream& output, Foo& arg)
 {
	output << arg.get_x() << "," << arg.get_y();
	return output;
 }
</source>


[[Category:{{FULLBOOKNAME}}|I]]
