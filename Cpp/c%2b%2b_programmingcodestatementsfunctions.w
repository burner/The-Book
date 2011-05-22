>==Functions==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">==Functions==
A '''[[w:subroutine|function]]''', which can also be referred to as [[w:subroutine|subroutine]], '''procedure''', '''subprogram''' or even [[w:Method_(computer_science)|method]], carries out tasks defined by a sequence of statements called a '''''[[C++ Programming/Programming Languages/C++/Code/Statements|statement block]]''''' that need only be written once and ''called'' by a program as many times as needed to carry out the same task.

Functions may depend on variables passed to them, called '''''[[#Parameters and arguments|arguments]]''''', and may pass results of a task on to the caller of the function, this is called the ''[[w:Return statement|return value]]''.

It is important to note that a '''function''' that exists in the '''''[[C++ Programming/Programming Languages/C++/Code/Statements/Scope|global scope]]''''' can also be called '''global function''' and a function that is defined inside a class is called a '''member function'''.  (The term '''method''' is commonly used in other programming languages to refer to things ''like'' member functions, but this can lead to confusion in dealing with C++ which supports both virtual and non-virtual dispatch of member functions.)

{{NOTE|note=
When talking or reading about programming, you must consider the language background and the topic of the source.
It is very rare to see a C++ programmer use the words '''procedure''' or '''subprogram''', this will vary from language to language.
In many programming languages the word '''function''' is reserved for subroutines that return a value, this is not the case with C++.
}}

=== Declarations ===
A function must be declared before being used, with a name to identify it, what type of value the function returns and the types of any arguments that are to be passed to it. Parameters must be named and declare what type of value it takes. Parameters should always be passed as ''const'' if their arguments are not modified. Usually functions performs actions, so the name should make clear what it does. By using verbs in function names and following other naming conventions programs can be read more naturally.

The next example we define a function named <code>main</code> that returns an integer value <code>int</code> and takes no parameters. The content of the function is called the ''body'' of the function. The word <code>int</code> is a ''keyword''. C++ keywords are ''reserved words'', i.e., cannot be used for any purpose other than what they are meant for. On the other hand ''main'' is not a keyword and you can use it in many places where a keyword cannot be used (though that is not recommended, as confusion could result).
<source lang=cpp>
int main()
{
  // code
  return 0;
}
</source>

==== {{C++ Programming/kw|inline}} ====
{{TODO|Merge and spread the info}}
{{:C++ Programming/Programming Languages/C++/Code/Keywords/inline}}

Normally when calling a function, a program will evaluate and store the arguments, and then call (or branch to) the function's code, and then the function will later return back to the caller.  While function calls are fast (typically taking much less than a microsecond on modern processors), the overhead can sometimes be significant, particularly if the function is simple and is called many times.

One approach which can be a performance optimization in some situations is to use so-called {{C++ Programming/kw|inline}} functions.  Marking a function as {{C++ Programming/kw|inline}} is a request (sometimes called a hint) to the compiler to consider replacing a ''call'' to the function by a copy of the code of that function.

The result is in some ways similar to the use of the #define macro, but as [[C++ Programming/Programming Languages/C++/Code/Compiler/Preprocessor#Macros|mentioned before]], macros can lead to problems since they are not evaluated by the [[C++ Programming/Programming Languages/C++/Code/Compiler/Preprocessor|preprocessor]].  {{C++ Programming/kw|inline}} functions do not suffer from the same problems.

If the inlined function is large, this replacement process (known for obvious reasons as "inlining") can lead to "code bloat", leading to bigger (and hence usually slower) code.  However, for small functions it can even reduce code size, particularly once a compiler's optimizer runs.

Note that the inlining process requires that the function's definition (including the code) must be available to the compiler.  In particular, inline headers that are used from more than one source file must be completely defined within a header file (whereas with regular functions that would be an error).

The most common way to designate that a function is inline is by the use of the {{C++ Programming/kw|inline}} keyword. One must keep in mind that compilers can be configured to ignore this keyword and use their own optimizations.

Further considerations are given when dealing with [[C++ Programming/Classes/Member Functions#Inline|inline member function]], this will be covered on the '''[[C++ Programming/Chapter Object Oriented Programming|Object-Oriented Programming Chapter]]''' . 

{{TODO|Complete and give examples|C++ Programming}}

=== Parameters and arguments ===
The function declaration defines its parameters.  A parameter is a variable which takes on the meaning of a corresponding argument passed in a call to a function.

An argument represents the value you supply to a function parameter when you call it. The calling code supplies the arguments when it calls the function.

The part of the function declaration that declares the expected parameters is called the '''parameter list''' and the part of function call that specifies the arguments is called the '''argument list'''.

<source lang=cpp>
//Global functions declaration
int subtraction_function( int parameter1, int parameter2 ) { return ( parameter1 - parameter2 ); }

//Call to the above function using 2 extra variables so the relation becomes more evident
int argument1 = 4;
int argument2 = 3; 
int result = subtraction_function( argument1, argument2 );
// will have the same result as
int result = subtraction_function( 4, 3 );
</source>

Many programmers use parameter and argument interchangeably, depending on context to distinguish the meaning. In practice, distinguishing between the two terms is usually unnecessary in order to use them correctly or communicate their use to other programmers. Alternatively, the equivalent terms formal parameter and actual parameter may be used instead of parameter and argument.

=== Parameters ===
You can define a function with no parameters, one parameter, or more than one, but to use a call to that function with arguments you must take into consideration what is defined. 

==== Empty parameter list ====

<source lang=cpp>
//Global functions with no parameters
void function() { /*...*/ }
//empty parameter declaration equivalent the use of void
void function( void ) ( /*...*/ }
</source>

{{NOTE|This is the only valid case were ''void'' can be used as a parameter type, you can only derived types from ''void'' (ie: ''void*'' ).}}

==== Multiple parameters ====
The syntax for declaring and invoking functions with multiple parameters can be a source of errors. When you write the function definition, you must declare the type of each and every parameter.

<source lang=cpp>
// Example - function using two int parameters by value
void printTime (int hour, int minute) { 
  std::cout << hour; 
  std::cout << ":"; 
  std::cout << minute; 
}
</source>

It might be tempting to write (int hour, minute), but that format is only legal for variable declarations, not for parameter declarations.

However, you do not have to declare the types of arguments when you call a function. (Indeed, it is an error to attempt to do so). 

'''Example'''
<source lang=cpp>
int main void(){
int hour = 11; 
    int minute = 59; 
    printTime( int hour, int minute ); // WRONG!
    printTime( hour, minute ); // Right!
}
</source> 

In this case, the compiler can tell the type of hour and minute by looking at their declarations. It is unnecessary and illegal to include the type when you pass them as arguments.</code>.

==== by pointer ====
A function may use pass by pointer when the object pointed to might not exist, that is, when you are giving either the address of a real object or NULL. Passing a pointer is not different to passing anything else. Its a parameter the same as any other. The characteristics of the pointer type is what makes it a worth distinguishing.

The passing a pointer to a function is very similar to passing it as a reference. It is used to avoid the overhead of copying, and the slicing problem (since child classes have a bigger memory footprint that the parent) that can  occur when passing base class objects by value. This is also the preferred method in C (for historical reasons), were passing by pointer signifies that wanted to modify the original variable. In C++ it is preferred to use references to pointers and guarantee that the function before dereferencing it, verifies the pointer for validity.

{{TODO|Reorder, simplify and clarify|C++ Programming}}

<source lang=cpp>
#include <iostream>

void MyFunc( int *x ) 
{ 
  std::cout << *x << std::endl; // See next section for explanation
} 
  
int main() 
{ 
  int i; 
  MyFunc( &i ); 

  return 0; 
} 
</source>

Since a reference is just an alias, it has exactly the same address as what it refers to, as in the following example:

<source lang=cpp>
#include <iostream>

void ComparePointers (int * a, int * b)
{
  if (a == b)
    std::cout<<"Pointers are the same!"<<std::endl;
  else
    std::cout<<"Pointers are different!"<<std::endl;
}

int main()
{
  int i, j;
  int& r = i;

  ComparePointers(&i, &i);
  ComparePointers(&i, &j);
  ComparePointers(&i, &r);
  ComparePointers(&j, &r);

  return 0;
}
</source>
This schizophrenic program will tell you that the pointers are the same, then that they are different, then the same, then different again.

;Arrays are similar to pointers, remember?
Now might be a good time to reread the section on arrays.  If you do not feel like flipping back that far, though, here's a brief recap:  Arrays are blocks of memory space.  

<source lang=cpp>
int my_array[5]; 
</source>

In the statement above, <tt>my_array</tt> is an area in memory big enough to hold five </code>int</code>s.  To use an element of the array, it must be ''dereferenced''.  The third element in the array (remember they're zero-indexed) is <tt>my_array[2]</tt>.  When you write <tt>my_array[2]</tt>, you're actually saying "give me the third integer in the array <tt>my_array</tt>".  Therefore, <tt>my_array</tt> is an array, but <tt>my_array[2]</tt> is an <code>int</code>.

; Passing a single array element
So let's say you want to pass one of the integers in your array into a function.  How do you do it?  Simply pass in the dereferenced element, and you'll be fine.  

'''Example'''
<source lang=cpp>
#include <iostream>

void printInt(int printable){
  std::cout << "The int you passed in has value " << printable << std::endl;
}
int main(){
  int my_array[5];
  
  // Reminder: always initialize your array values!
  for(int i = 0; i < 5; i++)
    my_array[i] = i * 2;
  
  for(int i = 0; i < 5; i++)
    printInt(my_array[i]); // <-- We pass in a dereferenced array element
}
</source>

This program outputs the following:

 The int you passed in has value 0
 The int you passed in has value 2
 The int you passed in has value 4
 The int you passed in has value 6
 The int you passed in has value 8

This passes array elements just like normal integers, because array elements like <tt>my_array[2]</tt> are integers.

;Passing a whole array

Well, we can pass single array elements into a function.  But what if we want to pass a whole array?  We can not do that directly, but you can treat the array as a pointer.

'''Example'''
<source lang=cpp>
#include <iostream>

void printIntArr(int *array_arg, int array_len){
  std::cout << "The length of the array is " << array_len << std::endl;
  for(int i = 0; i < array_len; i++)
    std::cout << "Array[" << i << "] = " << array_arg[i] << std::endl;
}
 
int main(){
  int my_array[5];

  // Reminder: always initialize your array values!
  for(int i = 0; i < 5; i++)
    my_array[i] = i * 2;
  
  printIntArr(my_array, 5);
}
</source>

{{NOTE|note=
Due to array-pointer interchangeability ''in the context of parameter declarations only'', we can also declare pointers as arrays in function parameter lists.  It is treated identically.  For example, the first line of the function above can also be written as

<source lang=cpp>void printIntArr(int array_arg[], int array_len)</source>

It is important to note that even if it is written as <tt>int array_arg[]</tt>, the parameter is still a pointer of type '''int *'''.  It is not an array; an array passed to the function will still be automatically converted to a pointer to its first element.
}}

This will output the following:

 The length of the array is 5
 Array[0] = 0
 Array[1] = 2
 Array[2] = 4
 Array[3] = 6
 Array[4] = 8

As you can see, the array in main is accessed by a pointer.  Now here's some important points to realize:
*Once you pass an array to a function, it is converted to a pointer so that function has no idea how to guess the length of the array.  Unless you always use arrays that are the same size, you should always pass in the array length along with the array. 
*You've passed in a POINTER.   <tt>my_array</tt> is an array, not a pointer.  If you change <tt>array_arg</tt> within the function, <tt>my_array</tt> does not change (i.e., if you set <tt>array_arg</tt> to point to a new array).  But if you change any element of <tt>array_arg</tt>, you're changing the memory space pointed to by <tt>array_arg</tt>, which is the array <tt>my_array</tt>.  

{{TODO|Passing a single element (by value vs. by reference), passing the whole array (always by reference), passing as '''const'''|C++ Programming}}

==== by reference ====
The same concept of references is used when passing variables. 

'''Example'''
<source lang=cpp>
void foo( int &i )
{
  ++i;
}
 
int main()
{
  int bar = 5;   // bar == 5
  foo( bar );    // bar == 6
  foo( bar );    // bar == 7
 
  return 0;
}
</source>
        
Here we display one of the two common uses of references in function arguments -- they allow us to use the conventional syntax of passing an argument by value but manipulate the value in the caller.

{{NOTE|note=If the parameter is a non-const reference, the caller expects it to be modified. If the function does not want to modify the parameter, a const reference should be used instead.}}

However there is a more common use of references in function arguments -- they can also be used to pass a handle to a large data structure without making multiple copies of it in the process. Consider the following:

<source lang=cpp>
void foo( const std::string & s ) // const reference, explained below
{
  std::cout << s << std::endl;
}

void bar( std::string s )
{
  std::cout << s << std::endl;
}

int main()
{
  std::string const text = "This is a test.";

  foo( text ); // doesn't make a copy of "text"
  bar( text ); // makes a copy of "text"

  return 0;
}
</source>
        
In this simple example we're able to see the differences in pass by value and pass by reference. In this case pass by value just expends a few additional bytes, but imagine for instance if <tt>text</tt> contained the text of an entire book.

The reason why we use a constant reference instead of a reference is the user of this function can assure that the value of the variable passed does not change within the function. We technically call this "const-to-reference".

The ability to pass it by reference keeps us from needing to make a copy of the string and avoids the ugliness of using a pointer.

{{NOTE|note=
It should also be noted that "const-to-reference" only makes sense for complex types -- classes and structs. In the case of ordinal types -- i.e. '''int''', '''float''', '''bool''', etc. -- there is no savings in using a reference instead of simply using pass by value, and indeed the extra costs associated with indirection may make code using a reference slower than code that copies small objects.
}}

;Passing an array of fixed-length by using reference
In some case, a function requires an array of a specific length to work:
<source lang=cpp>
void func(int(&para)[4]);
</source>
Unlike the case of array changed into pointer above, the parameter is not a PLAIN array that can be changed into a pointer, but rather a reference to array with 4 <code>int</code>s. Therefore, only array of 4 <code>int</code>s, not array of any other length, not pointer to int, can be passed into this function. This helps you prevent buffer overflow errors because the array object is ALWAYS allocated unless you circumvent the type system by casting.

It can be used to pass an array without specifying the number of elements manually:
<source lang=cpp>
template<int n>void func(int(&para)[n]);
</source>
The compiler generates the value of length at compile time, inside the function, n stores the number of elements. However, the use of template generates code bloat.

In C++, a multi-dimensional array cannot be converted to a multi-level pointer, therefore, the code below is invalid:
<source lang=cpp>
// WRONG
void foo(int**matrix,int n,int m);
int main(){
	int data[10][5];
	// do something on data
	foo(data,10,5);
}
</source>
Although an int[10][5] can be converted to an (*int)[5], it cannot be converted to int**. Therefore you may need to hard-code the array bound in the function declaration:
<source lang=cpp>
// BAD
void foo(int(*matrix)[5],int n,int m);
int main(){
	int data[10][5];
	// do something on data
	foo(data,10,5);
}
</source>
To make the function more generic, templates and function overloading should be used:
<source lang=cpp>
// GOOD
template<int junk,int rubbish>void foo(int(&matrix)[junk][rubbish],int n,int m);
void foo(int**matrix,int n,int m);
int main(){
	int data[10][5];
	// do something on data
	foo(data,10,5);
}
</source>
The reason for having n and m in the first version is mainly for consistency, and also deal with the case that the array allocated is not used completely. It may also be used for checking buffer overflows by comparing n/m with junk/rubbish.

==== by value ====
When we want to write a function which the value of the argument is independent to the passed variable, we use pass-by-value approach.

<source lang=cpp>
int add(int num1, int num2)
{
 num1 += num2; // change of value of "num1"
 return num1;
}

int main()
{
 int a = 10, b = 20, ans;
 ans = add(a, b);
 std::cout << a << " + " << b << " = " << ans << std::endl;
 return 0;
}
</source>

Output:
 10 + 20 = 30

The above example shows a property of pass-by-value, the arguments are copies of the passed variable and only in the [[C++ Programming/Programming Languages/C++/Code/Statements/Scope|scope]] of the corresponding function. This means that we have to afford the cost of copying. However, this cost is usually considered only for larger and more complex variables.<br>
In this case, the values of "a" and "b" are copied to "num1" and "num2" on the function "add()". We can see that the value of "num1" is changed in line 3. However, we can also observe that the value of "a" is kept after passed to this function.

==== Constant Parameters ====
The keyword <tt>const</tt> can also be used as a guarantee that a function will not modify a value that is passed in. This is really only useful for references and pointers (and not things passed by value), though there's nothing syntactically to prevent the use of <tt>const</tt> for arguments passed by value.

Take for example the following functions:

 '''void''' foo( '''const''' std::string &s )
 {
    s.append("blah"); // ERROR -- we can't modify the string
 
    std::cout << s.length() << std::endl; // fine
 }
 
 '''void''' bar( '''const''' Widget *w )
 {
     w->rotate(); // ERROR - rotate wouldn't be const
 
     std::cout << w->name() << std::endl; // fine
 }
        
In the first example we tried to call a non-const method -- <tt>append()</tt> -- on an argument passed as a <tt>const</tt> reference, thus breaking our agreement with the caller not to modify it and the compiler will give us an error.

The same is true with <tt>rotate()</tt>, but with a <tt>const</tt> pointer in the second example.

==== Default values ====
Parameters in C++ functions (including member functions and constructors) can be declared with default values, like this
 int foo (int a, int b = 5, int c = 3);

Then if the function is called with fewer arguments (but enough to specify the arguments without default values), the compiler will assume the default values for the missing arguments at the end. For example, if I call
 foo(6, 1)
that will be equivalent to calling
 foo(6, 1, 3)

In many situations, this saves you from having to define two separate functions that take different numbers of parameters, which are almost identical except for a default value.

The "value" that is given as the default value is often a constant, but may be any valid expression, including a function call that performs arbitrary computation.

Default values can only be given for the last arguments; i.e. you cannot give a default value for a parameter that is followed by a parameter that does not have a default value, since it will never be used.

Once you define the default value for a parameter in a function declaration, you cannot re-define a default value for the same parameter in a later declaration, even if it is the same value.

==== Ellipsis (...) as a parameter ====
If the parameter list ends with an ellipsis, it means that the arguments number must be equal or greater than the number of parameters specified. It will in fact create a variadic function, a function of variable arity; that is, one which can take different numbers of arguments.

{{TODO|Mention printf, <cstdarg> and check declaration|C++ Programming}}


{{NOTE|
The variadic function feature is going to be readdressed in the upcoming C++ language standard, C++0x; with the possible inclusion of variatic macros and the ability to create variadic template classes and variadic template functions. Variadic templates will finally allow the creation of true tuple classes in C++.}}

{{:C++ Programming/Code/Statements/Functions/Returning Values}}

=== Composition ===
Just as with mathematical functions, C++ functions can be composed, meaning that you use one expression as part of another. For example, you can use any expression as an argument to a function:
<source lang=cpp>double x = cos (angle + pi/2);</source> 

This statement takes the value of pi, divides it by two and adds the result to the value of angle. The sum is then passed as an argument to the cos function.

You can also take the result of one function and pass it as an argument to another:
<source lang=cpp>double x = exp (log (10.0));</source>  

This statement finds the log base e of 10 and then raises e to that power. The result gets assigned to x; I hope you know what it is.

=== Recursion ===
In programming languages, [[w:Recursion|recursion]] was first implemented in [[Programming:Lisp|Lisp]] on the basis of a mathematical concept that existed earlier on, it is a concept that allows us to break down a problem into one or more subproblems that are similar in form to the original problem, in this case, of having a function call itself in some circumstances. It is generally distinguished from [[C++ Programming/Programming Languages/C++/Code/Statements/Flow Control#Loops (iterations)|iterators or loops]].

A simple example of a recursive function is:

  '''void''' func(){
     func();
  }

It should be noted that non-terminating recursive functions as shown above are almost never used in programs (indeed, some definitions of recursion would exclude such non-terminating definitions). A terminating condition is used to prevent infinite recursion.

; Example:

  '''double''' power('''double''' x, '''int''' n)
  {
   '''if'''(n < 0)
   {
      std::cout << std::endl
                << "Negative index, program terminated.";
      exit(1);
   }
   '''if'''(n)
      '''return''' x * power(x, n-1);
   '''else'''
      '''return''' 1.0;
  }

The above function can be called like this:
  
  x = power(x, static_cast<int>(power(2.0, 2)));

Why is recursion useful? Although, theoretically, anything possible by recursion is also possible by iteration (that is, <code>while</code>), it is sometimes much more convenient to use recursion. Recursive code happens to be much easier to follow as in the example below. The problem with recursive code is that it takes too much memory. Since the function is called many times, without the data from the calling function removed, memory requirements increase significantly. But often the simplicity and elegance of recursive code overrules the memory requirements.

The classic example of recursion is the factorial:
<math>n!=(n-1)!n</math>, where <math>0!=1</math> by convention. In recursion, this function can be succinctly defined as
 '''unsigned''' factorial('''unsigned''' n)
 {
   '''if'''(n != 0) 
   {
     '''return''' n * factorial(n-1);
   } 
   '''else''' 
   {
     '''return''' 1;
   }
 }

With iteration, the logic is harder to see:
 '''unsigned''' factorial2('''unsigned''' n)
 {
   '''int''' a = 1;
   '''while'''(n > 0)
   {
     a = a*n;
     n = n-1;
   }
   '''return''' a;
 }

Although recursion tends to be slightly slower than iteration, it should be used where using iteration would yield long, difficult-to-understand code. Also, keep in mind that recursive functions take up additional memory (on the stack) for each level. Thus they can run out of memory where an iterative approach may just use constant memory.

Each recursive function needs to have a '''Base Case'''. A base case is where the recursive function stops calling itself and returns a value. The value returned is (hopefully) the  desired value.

For the previous example, 
 '''unsigned''' factorial('''unsigned''' n)
 {
   '''if'''(n != 0) 
   {
     '''return''' n * factorial(n-1);
   } 
   '''else''' 
   {
     '''return''' 1;
   }
 }

the base case is reached when <math>n=0</math>. In this example, the base case is everything contained in the else statement (which happens to return the number 1). The overall value that is returned is every value from <math>n</math> to <math>0</math> multiplied together. So, suppose we call the function and pass it the value <math>3</math>. The function then does the math <math>3*2*1=6</math> and returns 6 as the result of calling factorial(3).

Another classic example of recursion is the sequence of Fibonacci numbers:

 0 1 1 2 3 5 8 13 21 34 ...

The zeroth element of the sequence is 0. The next element is 1. Any other number of this series is the sum of the two elements coming before it. As an exercise, write a function that returns the ''n''th Fibonacci number using recursion.

=== main ===
The function <tt>main</tt> also happens to be the ''entry point'' of any (standard-compliant) C++ program and must be defined. The compiler arranges for the <tt>main</tt> function to be called when the program begins execution.  <tt>main</tt> may ''call'' other functions which may call yet other functions.

{{NOTE|note=<tt>main</tt> is special in C++ in that user code is not allowed to call it; in particular, it cannot be directly or indirectly recursive.  This is one of the many small ways in which C++ differs from C. }}

The <tt>main</tt> function returns an integer value. In certain systems, this value is interpreted as a success/failure code.  The return value of zero signifies a successful completion of the program.  Any non-zero value is considered a failure. Unlike other functions, if control reaches the end of <tt>main()</tt>, an implicit <code>return 0;</code> for success is automatically added.  To make return values from <tt>main</tt> more readable, the header file <tt>cstdlib</tt> defines the constants <tt>EXIT_SUCCESS</tt> and <tt>EXIT_FAILURE</tt> (to indicate successful/unsuccessful completion respectively).

{{NOTE|The ISO C++ Standard (ISO/IEC 14882:1998) specifically requires main to have a return type of <tt>int</tt>. But the ISO C Standard (ISO/IEC 9899:1999) actually does not, though most compilers treat this as a minor warning-level error.

The explicit use of <code>return 0;</code> (or <code>return EXIT_SUCCESS;</code>) to exit the main function is left to the '''''[[C++ Programming/Programming Languages/C++/Code/Style Conventions#Explicitness or Implicitness|coding style]]''''' used. }}

The main function can also be declared like this:

<source lang=cpp>
int main(int argc, char **argv){
  // code
}
</source>

which defines the <tt>main</tt> function as returning an integer value '''int''' and taking two parameters. The first parameter of the <tt>main</tt> function, '''argc''', is an integer value '''int''' that specifies the number of arguments passed to the program, while the second, '''argv''', is an array of strings containing the actual arguments. There is almost always at least one argument passed to a program; the name of the program itself is the first argument, <tt>argv[0]</tt>. Other arguments may be passed from the system.  

'''Example'''
<source lang=cpp>
#include <iostream>

int main(int argc, char **argv){
  std::cout << "Number of arguments: " << argc << std::endl;
  for(size_t i = 0; i < argc; i++)
    std::cout << "  Argument " << i << " = '" << argv[i] << "'" << std::endl;
}
</source>

{{NOTE|'''size_t''' is the return type of {{C++ Programming/kw|sizeof}} function. '''size_t''' is a '''typedef''' for some {{C++ Programming/kw|unsigned}} type and is often defined as {{C++ Programming/kw|unsigned}} '''int''' or {{C++ Programming/kw|unsigned}} '''long''' but not always.}}

If the program above is compiled into the executable <tt>arguments</tt> and executed from the command line like this in *nix:

 $ ./arguments I love chocolate cake

Or in Command Prompt in Windows or MS-DOS:

 C:\>arguments I love chocolate cake

It will output the following (but note that argument 0 may not be quite the same as this -- it might include a full path, or it might include the program name only, or it might include a relative path, or it might even be empty):

 Number of arguments: 5
   Argument 0 = './arguments'
   Argument 1 = 'I'
   Argument 2 = 'love'
   Argument 3 = 'chocolate'
   Argument 4 = 'cake'

You can see that the command line arguments of the program are stored into the <tt>argv</tt> array, and that <tt>argc</tt> contains the length of that array.  This allows you to change the behavior of a program based on the command line arguments passed to it.

{{NOTE|note='''argv''' is a (pointer to the first element of an) array of strings.  As such, it can be written as <tt>char **argv</tt> or as <tt>char *argv[]</tt>.  However, <tt>char argv[][]</tt> is not allowed.  Read up on C++ arrays for the exact reasons for this.

Also, '''argc''' and '''argv''' are the two most common names for the two arguments given to the <tt>main</tt> function.  You can think them to stand for "arguments count" and "arguments variables" respectively. They can, however, be changed if you'd like.  The following code is just as legal:

<source lang=cpp>
int main(int foo, char **bar){
  // code
}
</source>

However, any other programmer that sees your code might get mad at you if you code like that.

From the example above, we can also see that C++ do not really care about what the variables' names are (of course, you cannot use reserved words as names) but their types.
}}

{{:C++ Programming/Operators/Pointers/To Functions}}

=== Callback ===
In [[w:computer programming|computer programming]], a '''callback''' is [[w:executable code|executable code]] that is passed as an [[w:argument (computer science)|argument]] to other code. It allows a lower-level [[w:abstraction layer|abstaraction layer]] to call a [[w:subroutine|function]] defined in a higher-level layer. A callback is often back on the level of the original caller.
<center>[[Image:Callback-notitle.svg||400px|A callback is often back on the level of the original caller.]]</center> 

Usually, the higher-level code starts by calling a function within the lower-level code, passing to it a [[w:pointer|pointer]] or [[w:smart pointer|handle]] to another function.  While the lower-level function executes, it may call the passed-in function any number of times to perform some subtask. In another scenario, the lower-level function registers the passed-in function as a ''handler'' that is to be called asynchronously by the lower-level at a later time in reaction to something.

A callback can be used as a simpler alternative to [[w:polymorphism (computer science)|polymorphism]] and [[w:generic programming|generic programming]], in that the exact behavior of a function can be dynamically determined by passing different (yet compatible) function pointers or handles to the lower-level function. This can be a very powerful technique for [[w:code reuse|code reuse]]. In another common scenario, the callback is first registered and later called asynchronously.
<center>[[Image:Callback-async-notitle.svg||In another common scenario, the callback is first registered and later called asynchronously.]]</center>
{{TODO| Add missing, redirect links info and add examples...|C++ Programming}}

=== Overloading ===
Function overloading is the use of a single name for several different functions in the same scope. Multiple functions who share the same name must be differentiated by using another set of parameters for every such function. The functions can be different in the number of parameters they expect, or their parameters can differ in type. This way, the compiler can figure out the exact function to call by looking at the arguments the caller supplied. This is called overload resolution, and is quite complex.

<source lang=cpp>
// Overloading Example

// (1)
double geometric_mean( int, int );
 
// (2)
double geometric_mean( double, double );
 
// (3)
double geometric_mean( double, double, double );
 
// ...
 
// Will call (1):
geometric_mean( 10, 25 );
// Will call (2):
geometric_mean( 22.1, 421.77 );
// Will call (3):
geometric_mean( 11.1, 0.4, 2.224 );
</source>

Under some circumstances, a call can be ambiguous, because two or more functions
match with the supplied arguments equally well.

Example, supposing the declaration of geometric_mean above:
 // This is an error, because (1) could be called and the second
 // argument casted to an int, and (2) could be called with the first
 // argument casted to a double. None of the two functions is
 // unambiguously a better match.
 geometric_mean(7, 13.21);
 // This will call (3) too, despite its last argument being an int,
 // Because (3) is the only function which can be called with 3
 // arguments
 geometric_mean(1.1, 2.2, 3);

Templates and non-templates can be overloaded. A non-template function takes
precedence over a template, if both forms of the function match the supplied
arguments equally well.

Note that you can overload many operators in C++ too.

==== Overloading resolution ====
Please beware that overload resolution in C++ is one of the most complicated parts of the language. This is probably unavoidable in any case with automatic template instantiation, user defined implicit conversions, built-in implicit conversation and more as language features. So do not despair if you do not understand this at first go. It is really quite natural, once you have the ideas, but written down it seems extremely complicated.

{{TODO|*This section does not cover the selection of constructors because, well, that's even worse. Namespaces are also not considered below.
*Feel free to add the missing information, possibly as another chapter.
|C++ Programming}}

The easiest way to understand overloading is to imagine that the compiler first finds every function which might possibly be called, using any legal conversions and template instantiations. The compiler then selects the best match, if any, from this set. Specifically, the set is constructed like this:
* All functions with matching name, including function templates, are put into the set. Return types and visibility are not considered. Templates are added with as closely matching parameters as possible. Member functions are considered functions with the first parameter being a pointer-to-class-type.
* Conversion functions are added as so-called surrogate functions, with two parameters, the first being the class type and the second the return type.
* All functions that do not match the number of parameters, even after considering defaulted parameters and ellipses, are removed from the set.
* For each function, each argument is considered to see if a legal conversion sequence exists to convert the caller's argument to the function's parameters. If no such conversion sequence can be found, the function is removed from the set.

The legal conversions are detailed below, but in short a legal conversion is any number of built-in (like int to float) conversions combined with <em>at most one user defined conversion</em>. The last part is critical to understand if you are writing replacements to built-in types, such as smart pointers. User defined conversions are described above, but to summarize it is 
# implicit conversion operators like <tt>operator short toShort();</tt>
# One argument constructors (If a constructor has all but one parameter defaulted, it is considered one-argument)

The overloading resolution works by attempting to establish the best matching function. 

;'''Easy conversions are preferred''':
Looking at one parameter, the preferred conversion is roughly based on scope of the conversion. Specifically, the conversions are preferred in this order, with most-preferred highest:
# No conversion, adding one or more '''const''', adding reference, convert array to pointer to first member
## '''const''' are preferred for rvalues (roughly constants) while non-const are preferred for lvalues (roughly assignables)
# Conversion from short integral types ('''bool''', '''char''', '''short''') to '''int''', and '''float''' to '''double'''.
# Built-in conversions, such as between int and double and pointer type conversion. Pointer conversion are ranked as
## Base to derived (pointers) or derived to base (for pointers-to-members), with most-derived preferred
## Conversion to <tt>void*</tt>
## Conversion to '''bool'''
# User-defined conversions, see above. 
# Match with ellipses. (As an aside, this is rather useful knowledge for template meta programming)

The best match is now determined according to the following rules:

;*'''A function is only a better match if all parameters match at least as well'''
In short, the function must be better in every respect --- if one parameter matches better and another worse, neither function is considered a better match. If no function in the set is a better match than both, the call is ambiguous (i.e., it fails)
Example:
 '''void''' foo('''void*''', '''bool''');
 '''void''' foo('''int*''', '''int''');
 
 '''int''' main() {
    '''int''' a;
    foo(&a, true); // ambiguous 
 }

;*'''Non-templates are preferred over templates'''
If all else is equal between two functions, but one is a template and the other not, the non-template is preferred. This seldom causes surprises.

;*'''Most-specialized template is preferred'''
When all else is equal between two template function, but one is more specialized than the other, the most specialized version is preferred. 
Example:
 '''template'''<'''typename''' T> '''void''' foo(T);  //1
 '''template'''<'''typename''' T> '''void''' foo(T*); //2
 
 '''int''' main() {
    '''int''' a;
    foo(&a); // Calls 2, since 2 is more specialized.
 }

Which template is more specialized is an entire chapter unto itself.

;*'''Return types are ignored'''
This rule is mentioned above, but it bears repeating: Return types are <em>never</em> part of overload resolutions, even if the function selected has a return type that will cause the compilation to fail.
Example:
 '''void''' foo('''int''');
 '''int''' foo('''float''');
 
 '''int''' main() { 
    // This will fail since foo(int) is best match, and void cannot be converted to int.
    '''return''' foo(5); 
 }

;*'''The selected function may not be accessible'''
If the selected best function is not accessible (e.g., it is a private function and the call it not from a member or friend of its class), the call fails.

[[Category:C++ Programming|{{SUBPAGENAME}}]]
