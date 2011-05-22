>=== The string class ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== The string class ===
The string class is a part of the C++ standard library, used for convenient manipulation of sequences of characters, to replace the static, unsafe C method of handling strings. To use the string class in a program, the <string> header must be included. The standard library string class can be accessed through the '''''std namespace'''''.

The basic template class is <code>basic_string<></code> and its standard specializations are <code>string</code> and <code>wstring</code>.

==== Basic usage ====
Declaring a ''std'' string is done by using one of these two methods:

<source lang=cpp>
using namespace std;
string std_string;

or
 
std::string std_string;
</source>

==== Text I/O ====
This section will deal only keyboard, text input. There are many other inputs that can be read (mouse movements and button clicks, etc...), but will not be covered in this section, even reading the special keys of the keyboard will be excluded.  

Perhaps the most basic use of the string class is for reading text from the user and writing it to the screen. In the header file <tt>iostream</tt>, C++ defines an object named cin that handles input in much the same way that cout handles output.

<source lang=cpp>
// snipped designed to get an integer value from the user
int x; 
std::cin >> x; 
</source>

The >> operator will cause the execution to stop and will wait for the user to type something. If the user types a valid integer, it will be converted into an integer value and stored in x.

If the user types something other than an integer, the compiler will not report an error. Instead, it leaves the old content (a "random" meaningless value) in <tt>x</tt> and continues. 

This can then be extended into the following program:
{{:C++ Programming/Code/IO/Streams/string/Examples/Accepting and printing a String}}

Although a string may hold a sequence containing any character--including spaces and nulls--when reading into a string using cin and the extraction operator (>>) only the characters before the first space will be stored. Alternatively, if an entire line of text is desired, the ''getline'' function may be used:

<source lang=cpp>
    std::getline(std::cin, name);
</source>

===== Getting user input =====
Fortunately, there is a way to check and see if an input statement succeeds. We can invoke the good function on cin to check what is called the stream state. good returns a bool: if true, then the last input statement succeeded. If not, we know that some previous operation failed, and also that the next operation will fail.

Thus, getting input from the user might look like this:

<source lang=cpp>
#include <iostream>
int main () 
{ 
  using namespace std; // pull in the std namespace
  int x; 
 
  // prompt the user for input 
  cout << "Enter an integer: "; 
 
  // get input 
  cin >> x; 
 
  // check and see if the input statement succeeded 
  if (cin.good() == false) { 
    cout << "That was not an integer." << endl; 
    return -1; 
  } 
 
  // print the value we got from the user 
  cout << x << endl; 
  return 0; 
} 
</source>

cin can also be used to input a string:

<source lang=cpp>
string name; 

cout << "What is your name? "; 
cin >> name; 
cout << name << endl; 
</source>

As with the scanf() function from the Standard C Library, this statement only takes the first word of input, and leaves the rest for the next input statement. So, if you run this program and type your full name, it will only output your first name.

You may also notice the >> operator doesn't handle errors as expected (for example, if you accidentally typed your name in a prompt for a number.)  Because of these issues, it may be more suitable to read a line of text, and using the line for input — this is performed using the function called getline.

<source lang=cpp>
string name; 

cout << "What is your name? "; 
getline (cin, name); 
cout << name << endl; 
</source>

The first argument to getline is cin, which is where the input is coming from. The second argument is the name of the string variable where you want the result to be stored.

getline reads the entire line until the user hits Return or Enter. This is useful for inputting strings that contain spaces.

In fact, getline is generally useful for getting input of any kind. For example, if you wanted the user to type an integer, you could input a string and then check to see if it is a valid integer. If so, you can convert it to an integer value. If not, you can print an error message and ask the user to try again.

To convert a string to an integer you can use the strtol function defined in the header file cstdlib.  (Note that the older function atoi is less safe than strtol, as well as being less capable.)

If you still need the features of the >> operator, you will need to create a string stream as available from <tt>&lt;sstream&gt;</tt>.  The use of this stream will be discussed in a later chapter.

{{BookCat}}

==== More advanced string manipulation ====
{{TODO|Detail the commonly used std::string member functions(partially done)|C++ Programming}}

We will be using this dummy string for some of our examples.

<source lang=cpp>
string str("Hello World!");
</source>

This invokes the default constructor with a <code>const char*</code> argument. Default constructor creates a string which contains nothing, i.e. no characters, not even a <code>'\0'</code> (however std::string is not null terminated).

<source lang=cpp>
string str2(str);
</source>

Will trigger the copy constructor. <code>std::string</code> knows enough to make a deep copy of the characters it 
stores.

<source lang=cpp>
string str2 = str;
</source>

This will copy strings using assignment operator. Effect of this code is same as using copy constructor in example above.


===== Size =====
<source lang=cpp>
string::size_type string::size() const;
string::size_type string::length() const;
</source>

So for example one might do:

<source lang=cpp>
string::size_type strSize =  str.size();
string::size_type strSize2 = str2.length();
</source>

The methods <code>size()</code> and <code>length()</code> both return the size of the string object. There is no apparent difference. Remember that the last character in the string is <code>size() - 1</code> and not <code>size()</code>. Like in C-style strings, and arrays in general, <code>std::string</code> starts counting from 0.

===== I/O =====
<source lang=cpp>
ostream& operator<<(ostream &out, string &str);
istream& operator>>(istream &in, string &str);
</source>

The shift operators (<code>>></code> and <code><<</code>) have been overloaded so you can perform I/O operations on <code>istream</code> and <code>ostream</code> objects, most notably <code>cout</code>, <code>cin</code>, and filestreams. Thus you could just do console I/O like this:

<source lang=cpp>
std::cout << str << endl;
std::cin >> str;

istream& getline (istream& in, string& str, char delim = '\n');
</source>

Alternatively, if you want to read entire lines at a time, use <code>getline()</code>. Note that this is not a member function. <code>getline()</code> will retrieve characters from input stream <code>in</code> and assign them to <code>str</code> until <code>EOF</code> is reached or <code>delim</code> is encountered. <code>getline</code> will reset the input string before appending data to it. <code>delim</code> can be set to any <code>char</code> value and acts as a general delimiter. Here is some example usage:

<source lang=cpp>
#include <fstream>
//open a file
std::ifstream file("somefile.cpp");
std::string data, temp;

while( getline(file, temp, '#')) //while data left in file
{
    //append data
    data += temp;
}

std::cout << data;
</source>

Because of the way <code>getline</code> works (i.e. it returns the input stream), you can nest multiple <code>getline()</code> calls to get multiple strings; however this may significantly reduce readability.

===== Operators =====
<source lang=cpp>
char& string::operator[](string::size_type pos);
</source>

<code>Chars</code> in <code>string</code>s can be accessed directly using the overloaded subscript (<code>[]</code>) operator, like in <code>char</code> arrays:

<source lang=cpp>
std::cout << str[0] << str[2];
</source>

prints "Hl".

<code>std::string</code> supports casting from the older C string type <code>const char*</code>. You can also assign or append a simple <code>char</code> to a string. Assigning a <code>char*</code> to a <code>string</code> is as simple as

<source lang=cpp>
str = "Hello World!";
</source>

If you want to do it character by character, you can also use

<source lang=cpp>
str = 'H';
</source>

Not surprisingly, <code>operator+</code> and <code>operator+=</code> are also defined! You can append another <code>string</code>, a <code>const char*</code> or a <code>char</code> to any string.

The comparison operators <code>>, <, ==, >=, <=, !=</code> all perform comparison operations on strings, similar to the C strcmp() function. These return a true/false value.

<source lang=cpp>
if(str == "Hello World!")
{
 std::cout << "Strings are equal!";
}
</source>

===== Searching strings =====
<source lang=cpp>
string::size_type string::find(string needle, string::size_type pos = 0) const;
</source>
You can use the <code>find()</code> member function to find the first occurrence of a string inside another. <code>find()</code> will look for <code>needle</code> inside <code>this</code> starting from position <code>pos</code> and return the position of the first occurrence of the <code>needle</code>. For example:

<source lang=cpp>
std::string haystack = "Hello World!";
std::string needle = "o";
std::cout << haystack.find(needle);
</source>

Will simply print "4" which is the index of the first occurrence of "o" in <code>str</code>. If we want the "o" in "World", we need to modify <code>pos</code> to point past the first occurrence. <code>str.find(find, 4)</code> would return 4, while <code>str.find(find, 5)</code> would give 7. If the substring isn't found, <code>find()</code> returns <code>std::string::npos</code>.This simple code searches a string for all occurrences of "wiki" and prints their positions:

<source lang=cpp>
std::string wikistr = "wikipedia is full of wikis (wiki-wiki means fast)";
for(string::size_type i = 0, tfind; (tfind = wikistr.find("wiki", i)) != string::npos; i = tfind + 1)
{
 std::cout << "Found occurrence of 'wiki' at position " << tfind << std::endl;
}

string::size_type string::rfind(string needle, string::size_type pos = string::npos) const;
</source>

The function <code>rfind()</code> works similarly, except it returns the ''last'' occurrence of the passed string.

===== Inserting/erasing =====
<source lang=cpp>
string& string::insert(size_type pos, const string& str);
</source>
You can use the <code>insert()</cite> member function to insert another string into a string.
For example:
<source lang=cpp>
string newstr = " Human";
str.insert (5,newstr);
</source>

Would return ''Hello Human World!''

<source lang=cpp>
string& string::erase(size_type pos, size_type n);
</source>
You can use <code>erase()</code> to remove a substring from a string. For example:
<source lang=cpp>
str.erase (6,11);
</source>
Would return ''Hello!''

<source lang=cpp>
string& string::substr(size_type pos, size_type n);
</source>
You can use <code>substr()</code> to extract a substring from a string. For example:
<source lang=cpp>
string str = "Hello World!";
string part = str.substr(6,5);
</source>
Would return ''World''.

===== Backwards compatibility =====
<source lang=cpp>
const char* string::c_str() const;
const char* string::data() const;
</source>
For backwards compatibility with C/C++ functions which only accept <code>char*</code> parameters, you can use the member functions <code>string::c_str()</code> and <code>string::data()</code> to return a temporary <code>const char*</code> string you can pass to a function.  The difference between these two functions is that <code>c_str()</code> returns a null-terminated string while <code>data()</code> does not necessarily return a null-terminated string.  So, if your legacy function requires a null-terminated string, use <code>c_str()</code>, otherwise use <code>data()</code> (and presumably pass the length of the string in as well).

==== String Formatting ====

Strings can only be appended to other strings, but not to numbers or other datatypes, so something like <code>std::string("Foo") + 5</code> would not result in a string with the content <code>"Foo5"</code>. To convert other datatypes into string there exist the class <code>std::ostringstream</code>, found in the include file <code><sstream></code>. <code>std::ostringstream</code> acts exactly like <code>std::cout</code>, the only difference is that the output doesn't go to the current standard output as provided by the operating system, but into an internal buffer, that buffer can be converted into a <code>std::string</code> via the <code>std::ostringstream::str()</code> method.

===== Example =====
<source lang="cpp">
#include <iostream>
#include <sstream>

int main()
{ 
    std::ostringstream buffer;

    // Use the std::ostringstream just like std::cout or other iostreams
    buffer << "You have: " << 5 << " Helloworlds in your inbox";
 
    // Convert the std::ostringstream to a normal string
    std::string text = buffer.str();
 
    std::cout << text << std::endl;
 
    return 0;
}
</source>

==== Advanced use ====
{{TODO|Template parameters to basic_string etc.|C++ Programming}}

[[Category:C++ Programming|{{SUBPAGENAME}}]]
