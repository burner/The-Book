>== Operators ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== Operators ==
Now that we have covered the '''''[[C++ Programming/Programming Languages/C++/Code/Statements/Variables|variables]]''''' and '''''[[C++ Programming/Programming Languages/C++/Code/Statements/Variables/Type|data types]]''''' it becomes possible to introduce '''operators'''. '''Operators''' are special symbols that are used to represent and direct simple computations, this is significative importance in programming, since they serve to define, in a very direct, non-abtractive way and simple way, actions and simple interaction with data.

Since computers are mathematical devices, [[C++ Programming/Programming Languages/C++/Code/Compiler|compilers]] and [[w:interpreter|interpreters]] require a full syntactic theory of all operations in order to parse formulas involving any combinations correctly. In particular they depend on [[w:operator precedence|operator precedence]] rules, on [[w:order of operations|order of operations]], that are tacitly assumed in mathematical writing and the same applies to programming languages.
Conventionally, the computing usage of ''operator'' also goes beyond the [[w:operator|mathematical usage]] (for functions).

C++ like all [[C++ Programming/Programming Languages|programming languages]] uses a set of operators, they are subdivided into several groups:
* arithmetic operators (like addition and multiplication).
* boolean operators.
* string operators (used to manipulate [[w:literal string|strings of text]]).
* pointer operators.
* named operators (operators such as {{C++ Programming/kw|sizeof}}, <tt>new</tt>, and <tt>delete</tt> defined by alphanumeric  names rather than a punctuation character). 

Most of the operators in C++ do exactly what you would expect them to do, because most are common mathematical symbols. For example, the operator for adding two integers is +. C++ does allows the re-definition of some operators ([[C++ Programming/Operators/Operator Overloading|operator overloading]]) on more complex types, this be covered later on.

Expressions can contain both variables names and integer values. In each case the name of the variable is replaced with its value before the computation is performed.

=== Order of operations ===
When more than one operator appears in an expression the order of evaluation depends on the rules of precedence. A complete explanation of precedence can get complicated, but just to get you started:

Multiplication and division happen before addition and subtraction. So 2*3-1 yields 5, not 4, and 2/3-1 yields -1, not 1 (remember that in integer division 2/3 is 0). 
If the operators have the same precedence they are evaluated from left to right. So in the expression minute*100/60, the multiplication happens first, yielding 5900/60, which in turn yields 98. If the operations had gone from right to left, the result would be 59*1 which is 59, which is wrong. 
Any time you want to override the rules of precedence (or you are not sure what they are) you can use parentheses. Expressions in parentheses are evaluated first, so 2 * (3-1) is 4. You can also use parentheses to make an expression easier to read, as in (minute * 100) / 60, even though it doesn't change the result.

=== [[w:Operator precedence|Precedence]] (Composition) ===
At this point we have looked at some of the elements of a programming language like variables, expressions, and statements in isolation, without talking about how to combine them.

One of the most useful features of programming languages is their ability to take small building blocks and compose them (solving big problems by taking small steps at a  time). For example, we know how to multiply integers and we know how to output values; it turns out we can do both at the same time:

<source lang=cpp>
std::cout << 17 * 3; 
</source>

Actually, I shouldn't say "at the same time," since in reality the multiplication has to happen before the output, but the point is that any expression, involving numbers, characters, and variables, can be used inside an output statement. We've already seen one example:

<source lang=cpp>
std::cout << hour * 60 + minute << std::endl; 
</source>

You can also put arbitrary expressions on the right-hand side of an assignment statement:

<source lang=cpp>
int percentage; 
percentage = ( minute * 100 ) / 60; 
</source>

This ability may not seem so impressive now, but we will see other examples where composition makes it possible to express complex computations neatly and concisely.

{{NOTE|There are limits on where you can use certain expressions; most notably, the left-hand side of an assignment statement (normally) has to be a variable name, not an expression. That's because the left side indicates the storage location where the result will go. Expressions do not represent storage locations, only values.}}

The following is illegal:<source lang=cpp> minute+1 = hour;</source>

The exact rule for what can go on the left-hand side of an assignment expression is not so simple as it was in C; as  [[w:operator overloading|operator overloading]] and reference types can complicate the picture.
{{:C++ Programming/Operators/Chaining}}
{{:C++ Programming/Operators Table}}

=== Assignment ===
The most basic assignment operator is the "=" operator. It assigns one variable to have the value of another. For instance, the statement <tt>x = 3</tt> assigns <tt>x</tt> the value of 3, and <tt>y = x</tt> assigns whatever was in <tt>x</tt> to be in <tt>y</tt>. When the "=" operator is used to assign a class or struct, it acts like using the "=" operator on every single element. For instance:

<source lang=cpp>
//Example to demonstrate default "=" operator behavior.

struct A
 {
  int i;
  float f;
  A * next_a;
 };

//Inside some function
 {
  A a1, a2;              // Create two A objects.

  a1.i = 3;              // Assign 3 to i of a1.
  a1.f = 4.5;            // Assign the value of 4.5 to f in a1
  a1.next_a = &a2;       // a1.next_a now points to a2

  a2.next_a = NULL;      // a2.next_a is guaranteed to point at nothing now.
  a2.i = a1.i;           // Copy over a1.i, so that a2.i is now 3.
  a1.next_a = a2.next_a; // Now a1.next_a is NULL

  a2 = a1;               // Copy a2 to a1, so that now a2.f is 4.5. The other two are unchanged, since they were the same.
 }
</source>

Assignments can also be chained since the assignment operator returns the value it assigns. But this time the chaining is from right to left. For example, to assign the value of <tt>z</tt> to <tt>y</tt> and assign the same value (which is returned by the <tt>=</tt> operator) to <tt>x</tt> you use:

<source lang=cpp>
x = y = z;
</source>

When the "=" operator is used in a declaration, it has special meaning. It tells the [[C++ Programming/Programming Languages/C++/Code/Compiler|compiler]] to directly initialize the variable from whatever is on the right-hand side of the operator. This is called defining a variable, in the same way that you define a class or a function. With classes, this can make a difference, especially when assigning to a function call:

<source lang=cpp>
class A { /* ... */ };
A foo () { /* ... */ };

// In some function
 {
  A a;
  a = foo();

  A a2 = foo();
 }
</source>

In the first case, <tt>a</tt> is constructed, then is changed by the "=" operator. In the second statement, <tt>a2</tt> is constructed directly from the return value of <tt>foo()</tt>. In many cases, the [[C++ Programming/Programming Languages/C++/Code/Compiler|compiler]] can save a lot of time by constructing <tt>foo()</tt>'s return value directly into <tt>a2</tt>'s memory, which makes the program run faster.

Whether or not you define can also matter in a few cases where a definition can result in different linkage, making the variable more or less available to other source files.

=== Arithmetic operators ===
Arithmetic operations that can be performed on integers (also common in many other languages) include:

* Addition, using the <code>+</code> operator
* Subtraction, using the <code>-</code> operator
* Multiplication, using the <code>*</code> operator
* Division, using the <code>/</code> operator
* Remainder, using the <code>%</code> operator

Consider the next example, it will perform an addition and show the result:
{{:C++ Programming/Programming Languages/C++/Code/Statements/Variables/Operators/Examples/Adding Integer Numbers}}

The line relevant for the operatio is where the <tt>+</tt> operator adds the values stored in the locations <tt>a</tt> and <tt>b</tt>. <tt>a</tt> and <tt>b</tt> are said to be the ''operands'' of <tt>+</tt>. The combination <tt>a + b</tt> is called an ''expression'', specifically an ''arithmetic expression'' since <tt>+</tt> is an ''arithmetic operator''. 

Addition, subtraction and multiplication all do what you expect, but you might be surprised by division. For example, the following program:

<source lang=cpp>
int hour, minute; 
hour = 11; 
minute = 59; 
std::cout << "Number of minutes since midnight: "; 
std::cout << hour*60 + minute << std::endl; 
std::cout << "Fraction of the hour that has passed: "; 
std::cout << minute/60 << std::endl; 
</source>

would generate the following output:

<tt>Number of minutes since midnight: 719</tt><br> 
<tt>Fraction of the hour that has passed: 0</tt>

The first line is what we expected, but the second line is odd. The value of the variable minute is 59, and 59 divided by 60 is 0.98333, not 0. The reason for the discrepancy is that C++ is performing integer division.

When both of the operands are integers (operands are the things operators operate on), the result must also be an integer, and by definition integer division always rounds down, even in cases like this where the next integer is so close.

A possible alternative in this case is to calculate a percentage rather than a fraction:

<source lang=cpp>
std::cout << "Percentage of the hour that has passed: "; 
std::cout << minute*100/60 << std::endl;
</source>

The result is:

<tt>Percentage of the hour that has passed: 98</tt> 

Again the result is rounded down, but at least now the answer is approximately correct. In order to get an even more accurate answer, we could use a different type of variable, called floating-point, that is capable of storing fractional values.

This next example:
{{:C++ Programming/Programming Languages/C++/Code/Statements/Variables/Operators/Examples/Quotient and Remainder}}
will return:
 Quotient = 6
 Remainder = 3

The ''multiplicative'' operators <tt>*</tt>, <tt>/</tt> and <tt>%</tt> are always evaluated before the ''additive'' operators <tt>+</tt> and <tt>-</tt>. Among operators of the same class, evaluation proceeds from left to right. This order can be overridden using grouping by parentheses, <tt>(</tt> and <tt>)</tt>; the expression contained within parentheses is evaluated before any other neighboring operator is evaluated. But note that some [[C++ Programming/Programming Languages/C++/Code/Compiler|compilers]] may not strictly follow these rules when they try to optimize the code being generated, unless violating the rules would give a different answer.

For example the following statements convert a temperature expressed in degrees Celsius to degrees Fahrenheit and vice versa:

<source lang=cpp>
deg_f = deg_c * 9 / 5 + 32;
deg_c = ( deg_f - 32 ) * 5 / 9;
</source>

=== Compound assignment ===
One of the most common patterns in software with regards to operators is to update a value:

<source lang=cpp>
a = a + 1;
b = b * 2;
c = c / 4;
</source>

Since this pattern is used many times, there is a shorthand for it called compound assignment operators.  They are a combination of an existing arithmetic operator and assignment operator:

* +=
* -=
* *= 
* /= 
* %=
* <<= 
* >>= 
* |= 
* &=
* ^= 

Thus the example given in the beginning of the section could be rewritten as

<source lang=cpp>
a += 1;  // Equivalent to (a = a + 1)
b *= 2;  // Equivalent to (b = b * 2)
c /= 4;  // Equivalent to (c = c / 4)
</source>

{{TODO|Parent topic may need a re-writing. About optimization and distinction on the steps in a <nowiki>+= 1, a = a + 1, ++a or a++</nowiki>.}}

=== Character operators ===
Interestingly, the same mathematical operations that work on integers also work on characters. 

<source lang=cpp>
char letter; 
letter = 'a' + 1; 
std::cout << letter << std::endl; 
</source>

For the above example, outputs the letter b (on most systems -- note that C++ doesn't assume use of ASCII, EBCDIC, Unicode etc. but rather allows for all of these and other [[w:charset|charsets]]). Although it is syntactically legal to multiply characters, it is almost never useful to do it.

Earlier I said that you can only assign integer values to integer variables and character values to character variables, but that is not completely true. In some cases, C++ converts automatically between types. For example, the following is legal.

<source lang=cpp>
int number; 
number = 'a'; 
std::cout << number << std::endl; 
</source>

On most mainstream desktop computers the result is 97, which is the number that is used internally by C++ on that system to represent the letter 'a'. However, it is generally a good idea to treat characters as characters, and integers as integers, and only convert from one to the other if there is a good reason.  Unlike some other languages, C++ does not make strong assumptions about how the underlying platform represents characters; ASCII, EBCDIC and others are possible, and portable code will not make assumptions (except that '0', '1', ..., '9' are sequential, so that e.g. '9'-'0' == 9).

Automatic type conversion is an example of a common problem in designing a programming language, which is that there is a conflict between formalism, which is the requirement that formal languages should have simple rules with few exceptions, and convenience, which is the requirement that programming languages be easy to use in practice.

More often than not, convenience wins, which is usually good for expert programmers, who are spared from rigorous but unwieldy formalism, but bad for beginning programmers, who are often baffled by the complexity of the rules and the number of exceptions. In this book I have tried to simplify things by emphasizing the rules and omitting many of the exceptions.

=== Bitwise operators ===
These operators deal with a bitwise operations. Bit operations needs the understanding of binary numeration since it will deal with on one or two bit patterns or binary numerals at the level of their individual bits. On most microprocessors, bitwise operations are sometimes slightly faster than addition and subtraction operations and usually significantly faster than multiplication and division operations.

Bitwise operations especially important for much low-level programming from optimizations to writing device drivers, low-level graphics, communications protocol packet assembly and decoding.

Although machines often have efficient built-in instructions for performing arithmetic and logical operations, in fact all these operations can be performed just by combining the bitwise operators and zero-testing in various ways.

The bitwise operators work bit by bit on the operands. The operands must be of integral type (one of the types used for integers). 

For this section, recall that a number starting with '''0x''' is hexadecimal (hexa, or hex for short or referred also as base-16). Unlike the normal decimal system using powers of 10 and the digits 0123456789, hex uses powers of 16 and the symbols 0123456789abcdef. In the examples remember that Oxc equals 1100 in binary and 12 in decimal. C++ does not directly support binary notation, which would hamper readability of the code.

;NOT
; ~a    :  bitwise complement of '''a'''.
: ~0xc produces the value -1-0xc (in binary, ~1100 produces ...11110011 where "..." may be many more 1 bits)

The negation operator is a unary operator which precedes the operand, This operator must not be confused with the "logical not" operator, "<code>!</code>" (exclamation point), which treats the entire value as a single [[w:Boolean datatype|Boolean]]—changing a true value to false, and vice versa. The "logical not" is not a bitwise operation.

These others are binary operators which lie between the two operands. The precedence of these operators is lower than that of the relational and equivalence operators; it is often required to parenthesize expressions involving bitwise operators.

;AND
; a & b :  bitwise boolean and of '''a''' and '''b'''
: 0xc & 0xa produces the value 0x8 (in binary, 1100 & 1010 produces 1000)

The [[w:truth table|truth table]] of '''a AND b''':
{| border="1" cellpadding="1" cellspacing="0" style="text-align:center;"
|+
! style="width:35px;background:#aaaaaa;" | a
! style="width:35px;background:#aaaaaa;" | b
! style="width:35px" | ∧
|-
| 1  || 1  || 1
|-
| 1  || 0  || 0
|-
| 0  || 1  || 0
|-
| 0  || 0  || 0
|}

;OR
; a | b :  bitwise boolean or of '''a''' and '''b'''
: 0xc | 0xa produces the value 0xe (in binary, 1100 | 1010 produces 1110)

The [[w:truth table|truth table]] of '''a OR b''' is:
{| border="1" cellpadding="1" cellspacing="0" style="text-align:center;"
|+
! style="width:35px;background:#aaaaaa;" | a
! style="width:35px;background:#aaaaaa;" | b
! style="width:35px" | ∨
|-
| 1  || 1  || 1
|-
| 1  || 0  || 1
|-
| 0  || 1  || 1
|-
| 0  || 0  || 0
|}


;XOR
; a ^ b :  bitwise xor of '''a''' and '''b'''
: 0xc ^ 0xa produces the value 0x6 (in binary, 1100 ^ 1010 produces 0110)

The [[w:truth table|truth table]] of '''a XOR b''':
{| border="1" cellpadding="1" cellspacing="0" style="text-align:center;"
|+
! style="width:35px;background:#aaaaaa;" | a
! style="width:35px;background:#aaaaaa;" | b
! style="width:35px" | <big>⊕</big>
|-
| 1  || 1  || 0
|-
| 1  || 0  || 1
|-
| 0  || 1  || 1
|-
| 0  || 0  || 0
|}

;Bit shifts
; a << b :  shift '''a''' left by '''b''' (multiply a by <math>2^b</math>)
: 0xc << 1 produces the value 0x18 (in binary, 1100 << 1 produces the value 11000)

; a >> b :  shift '''a''' right by '''b''' (divide a by <math>2^b</math>)
: 0xc >> 1 produces the value 0x6 (in binary, 1100 >> 1 produces the value 110)

=== Derived types operators ===
There are three data types known as pointers, references, and arrays, that have their own operators for dealing with them. Those are <tt>*</tt>, <tt>&</tt>, <tt>[]</tt>, <tt>-></tt>, <tt>.*</tt>, and <tt>->*</tt>. 

Pointers, references, and arrays are fundamental data types that deal with accessing other variables. Pointers are used to pass around a variables ''address'' (where it is in memory), which can be used to have multiple ways to access a single variable. References are aliases to other objects, and are similar in use to pointers, but still very different. Arrays are large blocks of contiguous memory that can be used to store multiple objects of the same type, like a sequence of characters to make a string.

==== Subscript operator [ ] ====
This operator is used to access an object of an array. It is also used when declaring array types, allocating them, or deallocating them.
{{:C++ Programming/Operators/Arrays}}

==== address-of operator & ====
To get the address of a variable so that you can assign a pointer, you use the "address of" operator, which is denoted by the ampersand <tt>&</tt> symbol. The "address of" operator does exactly what it says, it returns the "address of" a variable, a symbolic constant, or a element in an array, in the form of a pointer of the corresponding type. To use the "address of" operator, you tack it on in front of the variable that you wish to have the address of returned. It is also used when declaring reference types.

Now, do not confuse the "address of" operator with the declaration of a reference. Because use of operators is restricted to expression, the [[C++ Programming/Programming Languages/C++/Code/Compiler|compiler]] knows that <tt>&sometype</tt> is the "address of" operator being used to denote the return of the address of <tt>sometype</tt> as a [[C++ Programming/Operators/Pointers|pointer]].

===== References =====
References are a way of assigning a "handle" to a variable.  References can also be thought of as "aliases"; they're not real objects, they're just alternative names for other objects.

;''Assigning References'': This is the less often used variety of references, but still worth noting as an introduction to the use of references in function arguments. Here we create a reference that looks and acts like a standard variable except that it operates on the same data as the variable that it references.

<source lang="cpp">
int tZoo = 3;       // tZoo == 3
int &refZoo = tZoo; // tZoo == 3
refZoo = 5;         // tZoo == 5
</source>

<tt>refZoo</tt> is a reference to <tt>tZoo</tt>. Changing the value of <tt>refZoo</tt> also changes the value of <tt>tZoo</tt>.

{{NOTE|One use of variable references is to pass function arguments using references. This allows the function to update / change the data in the variable being referenced }}

For example say we want to have a function to swap 2 integers

<source lang="cpp">
void swap(int &a, int &b){
  int temp = a; 
  a = b; 
  b = temp;
}
</source>

<source lang="cpp">
int main(){
   int x = 5; 
   int y = 6; 
   int &refx = x; 
   int &refy = y; 
   swap(refx, refy); // now x = 6 and y = 5
   swap(x, y); // and now x = 5 and y = 6 again
}
</source>

References cannot be null as they refer to instantiated objects, while pointers can be null. References cannot be reassigned, while pointers can be. 

<source lang="cpp">
int main(){
   int x = 5;
   int y = 6;
   int &refx = x;
   &refx = y; // won't compile
}
</source>

As references provide strong guarantees when compared with pointers, using references makes the code simpler. Therefore using references should usually be preferred over using pointers. Of course, pointers have to be used at the time of dynamic memory allocation (new) and deallocation (delete).

{{:C++ Programming/Operators/Pointers}}
=== {{C++ Programming/kw|sizeof}} ===
{{:C++ Programming/Programming Languages/C++/Code/Keywords/sizeof}}

=== Dynamic memory allocation ===
'''Dynamic memory allocation''' is the allocation of [[w:computer storage|memory]] storage for use in a [[w:computer program|computer program]] during the [[w:runtime|runtime]] of that program.  It is a way of distributing ownership of limited memory resources among many pieces of data and code.  Importantly, the amount of memory allocated is determined by the program at the time of allocation and need not be known in advance.  A dynamic allocation exists until it is explicitly released, either by the programmer or by a [[w:garbage collection (computer science)|garbage collector]] implementation; this is notably different from [[w:automatic memory allocation|automatic]] and [[w:static memory allocation|static memory allocation]], which require advance knowledge of the required amount of memory and have a fixed duration.  It is said that an object so allocated has ''dynamic lifetime''.

The task of fulfilling an allocation request, which involves finding a block of unused memory of sufficient size, is complicated by the need to avoid both internal and external [[w:fragmentation (computer)|fragmentation]] while keeping both allocation and deallocation [[w:Algorithmic_efficiency|efficient]]. Also, the allocator's [[w:metadata (computing)|metadata]] can inflate the size of (individually) small allocations; [[w:chunking (computing)|chunking]] attempts to reduce this effect.

Usually, memory is allocated from a large pool of unused memory area called '''the heap''' (also called the '''free store''').  Since the precise location of the allocation is not known in advance, the memory is accessed indirectly, usually via a [[w:reference (computer science)|reference]]. The precise algorithm used to organize the memory area and allocate and deallocate chunks is hidden behind an abstract interface and may use any of the methods described below.

You have probably wondered how programmers allocate memory efficiently without knowing, prior to running the program, how much memory will be necessary. Here is when the fun starts with dynamic memory allocation.

===== new and delete =====
For dynamic memory allocation we use the '''new''' and '''delete''' keywords, the old malloc from C functions can now be avoided but are still accessible for compatibility and low level control reasons.

{{TODO|add info on malloc|C++ Programming}}

As covered before, we assign values to pointers using the "address of" operator because it returns the address in memory of the variable or constant in the form of a pointer. Now, the "address of" operator is NOT the only operator that you can use to assign a pointer. You have yet another operator that returns a pointer, which is the new operator. The new operator allows the programmer to allocate memory for a specific data type, struct, class, etc., and gives the programmer the address of that allocated sect of memory in the form of a pointer. The new operator is used as an rvalue, similar to the "address of" operator. Take a look at the code below to see how the new operator works.

By assigning the pointers to an allocated sector of memory, rather than having to use a variable declaration, you basically override the "middleman" (the variable declaration). Now, you can allocate memory dynamically without having to know the number of variables you should declare.

<source lang=cpp>
int n = 10; 
SOMETYPE *parray, *pS; 
int *pint; 

parray = new SOMETYPE[n]; 
pS = new SOMETYPE; 
pint = new int; 
</source>

If you looked at the above piece of code, you can use the new operator to allocate memory for arrays too, which comes quite in handy when we need to manipulate the sizes of large arrays and or classes efficiently. The memory that your pointer points to because of the new operator can also be "deallocated," not destroyed but rather, freed up from your pointer. The delete operator is used in front of a pointer and frees up the address in memory to which the pointer is pointing.

<source lang=cpp>
delete [] parray;// note the use of [] when destroying an array allocated with new
delete pint; 
</source>

The memory pointed to by <code>parray</code> and <code>pint</code> have been freed up, which is a very good thing because when you're manipulating multiple large arrays, you try to avoid losing the memory someplace by leaking it. Any allocation of memory needs to be properly deallocated or a leak will occur and your program won't run efficiently. Essentially, every time you use the new operator on something, you should use the delete operator to free that memory before exiting. The delete operator, however, not only can be used to delete a pointer allocated with the new operator, but can also be used to "delete" a null pointer, which prevents attempts to delete non-allocated memory (this action compiles and does nothing).

You must keep in mind that <tt>new T</tt> and <tt>new T()</tt> are not equivalent. This will be more understandable after you are introduced to more complex types like classes, but keep in mind that when using <code>new T()</code> it will initialize the <tt>T</tt> memory location ("zero out") before calling the constructor (if you have non-initialized members variables, they will be initialized by default).

The '''new''' and '''delete''' operators do not have to be used in conjunction with each other within the same function or block of code. It is proper and often advised to write functions that allocate memory and other functions that deallocate memory.  Indeed, the currently favored style is to release resources in object's destructors, using the so-called [[w:RAII|resource acquisition is initialization]] (RAII) idiom.

{{TODO|Move or split some of the information or add references, classes, destructor and constructors have yet to be introduced and bellow we are using a vector for the example|C++ Programming}}

As we will see when we get to the Classes, a '''class''' destructor is the ideal location for its deallocator, it is often advisable to leave memory allocators out of classes' constructors. Specifically, using '''new''' to create an array of objects, each of which also uses '''new''' to allocate memory during its construction, often results in runtime errors. If a '''class''' or structure contains members which must be pointed at dynamically-created objects, it is best to sequentially initialize arrays of the parent object, rather than leaving the task to their constructors.

{{NOTE|If possible you should use <tt>new</tt> and <tt>delete</tt> instead of <tt>malloc</tt> and <tt>free</tt>.}}

<source lang=cpp>
// Example of a dynamic array

const int b = 5;
int *a = new int[b];

//to delete
delete[] a;
</source>

The ideal way is to not use arrays at all, but rather the STL's vector type (a container similar to an array).  To achieve the above functionality, you should do:

<source lang=cpp>
const int b = 5;
std::vector<int> a;
a.resize(b);

//to delete
a.clear();
</source>

Vectors allow for easy insertions even when "full."  If, for example, you filled up a, you could easily make room for a 6th element like so:

<source lang=cpp>
int new_number = 99;
a.push_back( new_number );//expands the vector to fit the 6th element
</source>

You can similarly dynamically allocate a rectangular multidimensional array (be careful about the type syntax for the pointers):

<source lang=cpp>
const int d = 5;
int (*two_d_array)[4] = new int[d][4];

//to delete
delete[] two_d_array;
</source>

You can also emulate a ragged multidimensional array (sub-arrays not the same size) by allocating an array of pointers, and then allocating an array for each of the pointers. This involves a loop. 

<source lang=cpp>
const int d1 = 5, d2 = 4;
int **two_d_array = new int*[d1];
for( int i = 0; i < d1; ++i)
  two_d_array[i] = new int[d2];

//to delete
for( int i = 0; i < d1; ++i)
  delete[] two_d_array[i];

delete[] two_d_array;
</source>
{{BookCat}}
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
