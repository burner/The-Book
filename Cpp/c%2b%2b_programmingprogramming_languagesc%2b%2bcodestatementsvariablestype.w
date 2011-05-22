>=== Type ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== Type ===
Just as there are different types of values (integer, character, etc.), there are different types of variables. A variable can refer to simple values like integers called a ''primitive type'' or to a set of values called a ''composite type'' that are made up of [[w:primitive types|primitive types]] and other [[w:composite types|composite types]]. Types consist of a set of valid values and a set of valid operations which can be performed on these values. A variable must declare what type it is before it can be used in order to enforce value and operation safety and to know how much space is needed to store a value.

Major functions that type systems provide are:
* '''Safety''' - types make it impossible to code some operations which cannot be valid in a certain context. This mechanism effectively catches the majority of common mistakes made by programmers. For example, an expression <tt>"Hello, Wikipedia"/1</tt> is invalid because a [[w:string literal|string literal]] cannot be divided by an [[w:integer|integer]] in the usual sense. As discussed below, strong typing offers more safety, but it does not necessarily guarantee complete safety (see [[w:type-safety|type-safety]] for more information).
* '''Optimization''' - static type checking might provide useful information to a compiler. For example, if a type says a value is aligned at a multiple of 4, the memory access can be optimized.
* '''Documentation''' - using types in languages also improves [[w:documentation|documentation]] of code. For example, the declaration of a variable as being of a specific type documents how the variable is used. In fact, many languages allow programmers to define semantic types derived from [[w:primitive type|primitive type]]s; either composed of elements of one or more primitive types, or simply as aliases for names of primitive types.
* '''Abstraction''' - types allow programmers to think about programs in higher level, not bothering with low-level implementation. For example, programmers can think of strings as values instead of a mere array of bytes.
* '''Modularity''' - types allow programmers to express the interface between two subsystems. This localizes the definitions required for interoperability of the subsystems and prevents inconsistencies when those subsystems communicate.
{{:C++ Programming/Programming Languages/C++/Code/Statements/Variables/Type/Data Types}}

==== Standard types ====
There are five basic ''primitive types'' called '''standard types''', specified by particular keywords, that store a single value. These types stand isolated from the complexities of class type variables, even if the syntax of utilization at times brings them all in line, standard types do not share class properties (i.e.: don't have a constructor). 

The type of a variable determines what kind of values it can store:

*<tt>bool</tt> - a boolean value: true; false
*<tt>int</tt> - Integer: -5; 10; 100
*<tt>char</tt> - a character in some encoding, often something like ASCII, ISO-8859-1 ("Latin 1") or ISO-8859-15: 'a', '=', 'G', '2'.
*<tt>float</tt> - floating-point number: 1.25; -2.35*10^23
*<tt>double</tt> - double-precision floating-point number: like float but more decimals

{{NOTE|A <tt>char</tt> variable cannot store sequences of characters (strings), such as "C++" ({'C', '+', '+', '\0'}); it takes 4 <tt>char</tt> variables (including the null-terminator) to hold it. This is a common confusion for beginners. There are several types in C++ that store string values, but we will discuss them later.}}

The <tt>float</tt> and <tt>double</tt> primitive data types are called 'floating point' types and are used to represent real numbers (numbers with decimal places, like 1.435324 and 853.562).  Floating point numbers and floating point arithmetic can be very tricky, due to the nature of how a computer calculates floating point numbers.

{{NOTE|note=Don't use floating-point variables where discrete values are needed. Using a float for a loop counter is a great way to shoot yourself in the foot. Always test floating-point numbers as <tt><=</tt> or <tt>>=</tt>, never use an exact comparison (<tt>==</tt> or <tt>!=</tt>).}}

==== Declaration ====
C++ is a '''statically typed''' language. Hence, any variable cannot be used without specifying its type. This is why the type figures in the declaration. This way the compiler can protect you from trying to store a value of an incompatible type into a variable, e.g. storing a string in an integer variable. Declaring variables before use also allows spelling errors to be easily detected. Consider a variable used in many statements, but misspelled in one of them. Without declarations, the compiler would silently assume that the misspelled variable actually refers to some other variable. With declarations, an "Undeclared Variable" error would be flagged. Another reason for specifying the type of the variable is so the compiler knows how much space in memory must be ''allocated'' for this variable.

The simplest variable declarations look like this (the parts in []s are optional):

 ''[specifier(s)]'' ''type'' ''variable_name'' ''[ = initial_value]'';

To create an integer variable for example, the syntax is
<source lang=cpp>
int sum; 
</source>

where sum is the name you made up for the variable. This kind of statement is called a declaration. It ''declares'' <tt>sum</tt> as a variable of type <tt>'''int'''</tt>, so that <tt>sum</tt> can store an integer value. Every variable has to be declared before use and it is common practice to declare variables as close as possible to the moment where they are needed. This is unlike languages, such as C, where all declarations must precede all other statements and expressions.

In general, you will want to make up variable names that indicate what you plan to do with the variable. For example, if you saw these variable declarations:

<source lang=cpp>
char firstLetter; 
char lastLetter; 
int hour, minute; 
</source>

you could probably make a good guess at what values would be stored in them. This example also demonstrates the syntax for declaring multiple variables with the same type in the same statement: hour and minute are both integers (''int'' type). Notice how a comma separates the variable names.

<source lang=cpp>
int a = 123;
int b (456);
</source>

Those lines also declare variables, but this time the variables are ''initialized'' to some value. What this means is that not only is space allocated for the variables but the space is also filled with the given value. The two lines illustrate two different but equivalent ways to initialize a variable. The assignment operator '=' in a declaration has a subtle distinction in that it assigns an initial value instead of assigning a new value. The distinction becomes important especially when the values we are dealing with are not of simple types like integers but more complex objects like the input and output streams provided by the <tt>iostream</tt> class.

The expression used to initialize a variable need not be constant. So the lines:

<source lang=cpp>
int sum;
sum = a + b;
</source>

can be combined as:

<source lang=cpp>
int sum = a + b;
</source>

or:

<source lang=cpp>
int sum (a + b);
</source>

Declare a floating point variable 'f' with an initial value of 1.5:

<source lang=cpp>
float f = 1.5 ;
</source>

Floating point constants should always have a '.' (decimal point) somewhere in them.  Any number that does not have a decimal point is interpreted as an integer, which then must be converted to a floating point value before it is used.

For example:
<source lang=cpp>
double a = 5 / 2;
</source>

will not set a to 2.5 because 5 and 2 are integers and integer arithmetic will apply for the division, cutting off the fractional part.
A correct way to do this would be:

<source lang=cpp>
double a = 5.0 / 2.0;
</source>

You can also declare floating point values using scientific notation.  The constant .05 in scientific notation would be <math>5 \times 10^{-2}</math>.  The syntax for this is the base, followed by an e, followed by the exponent.  For example, to use .05 as a scientific notation constant:

<source lang=cpp>
double a = 5e-2;
</source><br>
{{NOTE|Single letters can sometimes be a bad choice for variable names when their purpose cannot be determined. However, some single-letter variable names are so commonly used that they're generally understood. For example <tt>i</tt>, <tt>j</tt>, and <tt>k</tt> are commonly used for loop variables and iterators; <tt>n</tt> is commonly used to represent the number of some elements or other counts; <tt>s</tt>, and <tt>t</tt> are commonly used for strings (that don't have any other meaning associated with them, as in utility routines); <tt>c</tt> and <tt>d</tt> are commonly used for characters; and <tt>x</tt> and <tt>y</tt> are commonly used for Cartesian co-ordinates.}}

Below is a program storing two values in integer variables, adding them and displaying the result:

{{:C++ Programming/Code/Variables/Examples/Adds two numbers and prints their sum}}

or, if you like to save some space, the same above statement can be written as:

{{:C++ Programming/Code/Variables/Examples/Adds two numbers and prints their sum1}}

===== {{C++ Programming/kw|register}} =====
{{:C++ Programming/Programming Languages/C++/Code/Keywords/register}}

==== Modifiers ====
There are several modifiers that can be applied to data types to change the range of numbers they can represent.

===== const =====
A variable declared with this specifier cannot be changed (as in read only). Either local or class-level variables (''scope'') may be declared <tt>const</tt> indicating that you don't intend to change their value after they're initialized. You declare a variable as being constant using the <tt>const</tt> keyword. Global <tt>const</tt> variables have static linkage. If you need to use a global constant across multiple files the best option is to use a special header file that can be included across the project.

<source lang=cpp>
const unsigned int DAYS_IN_WEEK = 7 ; 
</source>

declares a positive integer constant, called <tt>DAYS_IN_WEEK</tt>, with the value 7. Because this value cannot be changed, you must give it a value when you declare it. If you later try to assign another value to a constant variable, the compiler will print an error.

<source lang=cpp>
int main(){
  const int i = 10;

  i = 3;            // ERROR - we can't change "i"
 
  int &j = i;       // ERROR - we promised not to
                    // change "i" so we can't
                    // create a non-const reference
                    // to it

  const int &x = i; // fine - "x" is a const
                    // reference to "i"
 
  return 0;
}
</source>

The full meaning of <tt>const</tt> is more complicated than this; when working through pointers or references, <tt>const</tt> can be applied to mean that the object pointed (or  referred) to will not be changed ''via that pointer'' or ''reference''.  There may be other names for the object, and it may still be changed using one of those names so long as it was not originally defined as being truly <tt>const</tt>.

It has an advantage for programmers over <tt>#define</tt> command because it is understood by the compiler, not just substituted into the program text by the preprocessor, so any error messages can be much more helpful.

With pointer it can get messy...

<source lang=cpp>
T const *p;                     // p is a pointer to a const T
T *const p;                     // p is a const pointer to T
T const *const p;               // p is a const pointer to a const T
</source>                 
                                           
If the pointer is a local, having a <tt>const</tt> pointer is useless.
The order of T and const can be reversed:     

<source lang=cpp>
const T *p;
</source>

is the same as

<source lang=cpp>
T const *p;
</source>

{{NOTE|<tt>const</tt> can be used in the declaration of variables (arguments, return values and methods) - some of which we will mention later on.

Using <tt>const</tt> has several advantages: 

To users of the <tt>class</tt>, it is immediately obvious that the <tt>const</tt> methods will not modify the object. 
*Many accidental modifications of objects will be caught at compile time. 
*Compilers like <tt>const</tt> since it allows them to do better optimization.
}}

===== volatile =====
A hint to the compiler that a variable's value can be changed externally; therefore the compiler must avoid aggressive optimization on any code that uses the variable.

Unlike in Java, C++'s <tt>volatile</tt> specifier does not have any meaning in relation to multi-threading. Standard C++ does not include support for multi-threading (though it is a common extension) and so variables needing to be synchronized between threads need a synchronization mechanisms such as mutexes to be employed, keep in mind that <tt>volatile</tt> implies only safety in the presence of implicit or unpredictable actions by the same thread (or by a signal handler in the case of a '''volatile sigatomic_t''' object). Accesses to  <tt>mutable volatile</tt> variables and fields are viewed as synchronization operations by most compilers and can affect control flow and thus determine whether or not other shared variables are accessed, this implies that in general ordinary memory operations cannot be reordered with respect to a mutable volatile access. This also means that mutable volatile accesses are sequentially consistent. This is not (as yet) part of the standard, it is under discussion and should be avoided until it gets defined.

===== mutable =====
This specifier may only be applied to a non-static, non-const member variables.  It allows the variable to be modified within '''const''' member functions.

'''mutable''' is usually used when an object might be <em>logically constant</em>, i.e., no outside observable behavior changes, but not <em>bitwise</em> '''const''', i.e. some internal member might change state.

The canonical example is the proxy pattern. Suppose you have created an image catalog application that shows all images in a long, scrolling list. This list could be modeled as:

<source lang=cpp>
class image {
 public:
   // construct an image by loading from disk
   image(const char* const filename); 

   // get the image data
   char const * data() const;
 private:
   // The image data
   char* m_data;
}
 
class scrolling_images {
   image const* images[1000];
};
</source>

Note that for the image class, bitwise '''const''' and logically '''const''' is the same: If m_data changes, the public function data() returns different output.

At a given time, most of those images will not be shown, and might never be needed. To avoid having the user wait for a lot of data being loaded which might never be needed, the proxy pattern might be invoked:

<source lang=cpp>
class image_proxy {
  public:
   image_proxy( char const * const filename )
      : m_filename( filename ),
        m_image( 0 ) 
   {}
   ~image_proxy() { delete m_image; }
   char const * data() const {
      if ( !m_image ) {
         m_image = new image( m_filename );
      }
      return m_image->data();
   }
  private:
   char const* m_filename;
   mutable image* m_image;
};
 
class scrolling_images {
   image_proxy const* images[1000];
};
</source>

Note that the image_proxy does not change observable state when <tt>data()</tt> is invoked: it is logically constant. However, it is not bitwise constant since <tt>m_image</tt> changes the first time <tt>data()</tt> is invoked. This is made possible by declaring <tt>m_image</tt> mutable. If it had not been declared mutable, the <tt>image_proxy::data()</tt> would not compile, since <tt>m_image</tt> is assigned to within a constant function.

{{NOTE|Like exceptions to most rules, the <tt>mutable</tt> keyword exists for a reason, but should not be overused. If you find that you have marked a significant number of the member variables in your class as <tt>mutable</tt> you should probably consider whether or not the design really makes sense.}}

===== short =====
The <tt>short</tt> specifier can be applied to the <tt>int</tt> data type.  It can decrease the number of bytes used by the variable, which decreases the range of numbers that the variable can represent.  Typically, a <tt>short int</tt> is half the size of a regular <tt>int</tt> -- but this will be different depending on the compiler and the system that you use.  When you use the <tt>short</tt> specifier, the <tt>int</tt> type is implicit.  For example:

<source lang=cpp>
short a;
</source>

is equivalent to:

<source lang=cpp>
short int a;
</source>

{{NOTE|Although <tt>short</tt> variables may take up less memory, they can be slower than regular <tt>int</tt> types on some systems.  Because most machines have plenty of memory today, it is rare that using a <tt>short int</tt> is advantageous.}}

===== long =====
The <tt>long</tt> specifier can be applied to the <tt>int</tt> and <tt>double</tt> data types.  It can increase the number of bytes used by the variable, which increases the range of numbers that the variable can represent.  A <tt>long int</tt> is typically twice the size of an <tt>int</tt>, and a <tt>long double</tt> can represent larger numbers more precisely.  When you use <tt>long</tt> by itself, the <tt>int</tt> type is implied.  For example:

<source lang=cpp>
long a;
</source>

is equivalent to:

<source lang=cpp>
long int a;
</source>

The shorter form, with the <tt>int</tt> implied rather than stated, is more idiomatic (i.e., seems more natural to experienced C++ programmers).

Use the <tt>long</tt> specifier when you need to store larger numbers in your variables.  Be aware, however, that on some compilers and systems the long specifier may not increase the size of a variable.  Indeed, most common 32-bit platforms (and one 64-bit platform) use 32 bits for <tt>int</tt> and also 32 bits for <tt>long int</tt>.

{{NOTE|C++ does not yet allow <tt>long long int</tt> like modern C does, though it is likely to be added in a future C++ revision, and then would be guaranteed to be at least a 64-bit type.  Most C++ implementations today offer <tt>long long</tt> or an equivalent as an ''extension'' to standard C++.}}

===== {{C++ Programming/kw|unsigned}} =====
{{:C++ Programming/Programming Languages/C++/Code/Keywords/unsigned}}

===== signed =====
The <tt>signed</tt> specifier makes a variable represent both positive and negative numbers.  It can be applied only to the <tt>char</tt>, <tt>int</tt> and <tt>long</tt> data types.  The <tt>signed</tt> specifier is applied by default for <tt>int</tt> and <tt>long</tt>, so you typically will never use it in your code.

{{NOTE|Plain <tt>char</tt> is a distinct type from both <tt>signed char</tt> and {{C++ Programming/kw|unsigned}} <tt>char</tt> although it has the same range and representation as one or the other.  On some platforms plain <tt>char</tt> can hold negative values, on others it cannot.  <tt>char</tt> should be used to represent a character; for a small integral type, use signed char, or for a small type supporting [[w:modular arithmetic|modular arithmetic]] use {{C++ Programming/kw|unsigned}} <tt>char</tt>.}}

===== {{C++ Programming/kw|static}} =====
{{:C++ Programming/Programming Languages/C++/Code/Keywords/static}}
{{:C++ Programming/Programming Languages/C++/Code/Keywords/static/Permanent Storage}}

==== Enumerated data type ====
In programming it is often necessary to deal with data types that describe a fixed set of alternatives.  For example, when designing a program to play a card game it is necessary to keep track of the suit of an individual card.

One method for doing this may be to create unique constants to keep track of the suit.  For example one could define
<source lang=cpp>
const int Clubs=0;
const int Diamonds=1;
const int Hearts=2;
const int Spades=3;

int current_card_suit=Diamonds;
</source>

Unfortunately there are several problems with this method.  The most minor problem is that this can be a bit cumbersome to write.  A more serious problem is that this data is indistinguishable from integers.  It becomes very easy to start using the associated numbers instead of the suits themselves.  Such as:
<source lang=cpp>
int current_card_suit=1;
</source>

...and worse to make mistakes that may be very difficult to catch such as a typo...

<source lang=cpp>
current_card_suit=11;
</source>

...which produces a valid expression in C++, but would be meaningless in representing the card's suit.

One way around these difficulty is to create a ''new'' data type specifically designed to keep track of the suit of the card, and ''restricts'' you to only use valid possibilities.  We can accomplish this using an enumerated data type using the C++ {{C++ Programming/kw|enum}} keyword. 

{{:C++ Programming/Programming Languages/C++/Code/Keywords/enum}}

==== {{C++ Programming/kw|typedef}} ====
{{:C++ Programming/Programming Languages/C++/Code/Keywords/typedef}}

==== Derived types ====
{{TODO|Complete...  ref to:<br>[[C++ Programming/Operators/Pointers]]<br>[[C++ Programming/Operators/Arrays]]|C++ Programming}}

==== Type conversion ====
'''Type conversion''' or '''typecasting''' refers to changing an entity of one data type into another.

===== Implicit type conversion =====
'''Implicit type conversion''', also known as '''coercion''', is an automatic and temporary type conversion by the compiler. In a mixed-type expression, data of one or more subtypes can be converted to a supertype as needed at runtime so that the program will run correctly.

For example:
<source lang="cpp">
double  d;
long    l;
int     i; 
 
if (d > i)      d = i;
if (i > l)      l = i;
if (d == l)     d *= 2;
</source>

As you can see d, l and i belong to different data types, the compiler will then automatically and temporarily converted the original types to equal data types each time a comparison or assignment is executed.

{{NOTE|This behavior should be used with caution, and most modern compiler will provide a warning, as unintended consequences can arise.

Data can be lost when floating-point representations are converted to integral representations as the fractional components of the floating-point values will be truncated (rounded down). Conversely, converting from an integral representation to a floating-point one can also lose precision, since the floating-point type may be unable to represent the integer exactly (for example, float might be an IEEE 754 single precision type, which cannot represent the integer 16777217 exactly, while a 32-bit integer type can). This can lead to situations such as storing the same integer value into two variables of type int and type single which return false if compared for equality.}}

===== Explicit type conversion =====
'''Explicit type conversion''' manually converts one type into another, and is used in cases where automatic type casting doesn't occur.

<source lang="cpp">
double d = 1.0;

printf ("%d\n", (int)d);
</source>

In this example, ''d'' would normally be a double and would be passed to the [[C++ Programming/Code/Standard C Library/Functions/printf|printf]] function as such. This would result in unexpected behavior, since [[C++ Programming/Code/Standard C Library/Functions/printf|printf]] would try to look for an int.  The typecast in the example corrects this, and passes the integer to [[C++ Programming/Code/Standard C Library/Functions/printf|printf]] as expected.

{{NOTE|Explicit type casting should only be used as required.  It should not be used if implicit type conversion would satisfy the requirements.}}
{{BookCat}}
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
