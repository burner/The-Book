>== Type Conversion ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== Type Conversion ==
'''Type conversion''' (often a result of ''type casting'') refers to changing an entity of one '''''[[C++ Programming/Programming Languages/C++/Code/Statements/Variables/Type/Data Types|data type]]''''', expression, function argument, or return value into another.  This is done to take advantage of certain features of type hierarchies.  For instance, values from a more limited set, such as integers, can be stored in a more compact format and later converted to a different format enabling operations not previously possible, such as division with several decimal places' worth of accuracy.  In the [[w:object-oriented|object-oriented]] programming paradigm, type conversion allows programs also to treat objects of one type as one of another. One must do it carefully as type casting can lead to loss of data.

{{NOTE|The Wikipedia article about [[w:Strongly-typed_programming_language|strongly typed]] suggests that there is not enough consensus on the term "strongly typed" to use it safely. So you should re-check the intended meaning carefully, the above statement is what C++ programmers refer as  ''strongly typed'' in the language scope.}}

=== Automatic type conversion ===
'''Automatic type conversion''' (or standard conversion) happens whenever the compiler expects data of a particular type, but the data is given as a different type, leading to an automatic conversion by the compiler without an explicit indication by the programmer.

{{NOTE|This is not "casting" or explicit type conversions.  There is no such thing as an "automatic cast".}}

When an expression requires a given type that cannot be obtained through an implicit conversion or if more than one standard conversion creates an ambiguous situation, the programmer must explicitly specify the target type of the conversion. If the conversion is impossible it will result in an error or warning at compile time. Warnings may vary depending on the compiler used or compiler options.

This type of conversion is useful and relied upon to perform integral promotions, integral conversions, floating point conversions, floating-integral conversions, arithmetic conversions, pointer conversions.

<source lang=cpp>
int a = 5.6;
float b = 7;
</source>

In the example above, in the first case an expression of type float is given and automatically interpreted as an integer. In the second case (more subtle), an integer is given and automatically interpreted as a float.

There are two types of automatic type conversions between numeric types: promotion and conversion. Numeric promotion causes a simple type conversion whenever a value is used, while more complex numeric conversions can take place if the context of the expression requires it.

;Any automatic type conversion is an implicit conversion if not done explicitly in the source code.

Automatic type conversions (implicit conversions) can also occur in the implicit "decay" from an array to a corresponding pointer type based or as an [[C++ Programming/Classes#User defined automatic type conversion|user defined behavior]], we will cover that after we introduce classes (user defined types) as the automatic type conversions of references (derived class reference to base class reference) and pointer-to-member (from pointing to member of a base class to pointing to member of a derived class).

==== Promotion ====
A numeric promotion is the conversion of a value to a type with a wider range that happens whenever a value of a narrower type is used. Values of integral types narrower than <code>int</code> (<code>char</code>, <code>signed char</code>, <code>unsigned char</code>, <code>short int</code> and <code>unsigned short</code>) will be promoted to <code>int</code> if possible, or <code>unsigned int</code> if <code>int</code> can't represent all the values of the source type. Values of <code>bool</code> type will also be converted to <code>int</code>, and in particular <code>true</code> will get promoted to 1 and <code>false</code> to 0.

<source lang=cpp>
// promoting short to int
short left = 12;
short right = 23;

short total = left + right;
</source>

In the code above, the values of <code>left</code> and <code>right</code> are both of type <code>short</code> and could be added and assigned as such. However, in C++ they will each be promoted to <code>int</code> before being added, and the result converted back to <code>short</code> afterwards. The reason for this is that the <code>int</code> type is designed to be the most natural integer representation on the machine architecture, so requiring that the compiler do its calculations with smaller types may cause an unnecessary performance hit.

Since the C++ standard guarantees only the minimum sizes of the data types, the sizes of the types commonly vary between one architecture and another (and may even vary between one compiler and another). This is the reason why the compiler is allowed the flexibility to promote to <code>int</code> or <code>unsigned int</code> as necessary.

Promotion works in a similar way on floating-point values: a <code>float</code> value will be promoted to a <code>double</code> value, leaving the value unchanged.

Since promotion happens in cases where the expression does not require type conversion in order to be compiled, it can cause unexpected effects, for example in overload resolution:

<source lang="cpp">
void do_something(short arg)
{
    cout << "Doing something with a short" << endl;
}

void do_something(int arg)
{
    cout << "Doing something with an int" << endl;
}

int main(int argc, char **argv)
{
    short val = 12;

    do_something(val); // Prints "Doing something with a short"
    do_something(val * val); // Prints "Doing something with an int"
}
</source>

Since <code>val</code> is a <code>short</code>, you might expect that the expression <code>val * val</code> would also be a <code>short</code>, but in fact <code>val</code> is promoted to <code>int</code>, and the <code>int</code> overload is selected.

==== Numeric conversion ====
After any numeric promotion has been applied, the value can then be converted to another numeric type if required, subject to various constraints.

{{NOTE|The standard guarantees that some conversions are possible without specifying what the exact result will be. This means that certain conversions that are legal can unexpectedly give different results using different compilers.}}

A value of any integer type can be converted to any other integer type, and a value of an enumeration type can be converted to an integer type. This only gets complicated when overflow is possible, as in the case where you convert from a larger type to a smaller type. In the case of conversion to an unsigned type, overflow works in a nice predictable way: the result is the smallest unsigned integer congruent to the value being converted (modulo <math>2^n</math>, where <math>n</math> is the number of bits in the destination type).

When converting to a signed integer type where overflow is possible, the result of the conversion depends on the compiler. Most modern compilers will generate a warning if a conversion occurs where overflow could happen. Should the loss of information be intended, the programmer may do explicit type casting to suppress the warning; bit masking may be a superior alternative.

Floating-point types can be converted between each other, but are even more prone to platform-dependence. If the value being converted can be represented exactly in the new type then the exact conversion will happen. Otherwise, if there are two values possible in the destination type and the source value lies between them, then one of the two values will be chosen. In all other cases the result is implementation-defined.

Floating-point types can be converted to integer types, with the fractional part being discarded.
<source lang="cpp">
double a = 12.5;
int b = a;

cout << b; // Prints "12"
</source>

{{NOTE|If a floating-point value is converted to an integer and the result can't be expressed in the destination type, behavior is undefined by the C++ standard, meaning that your program may crash.}}

A value of an integer type can be converted to a floating point type. The result is exact if possible, otherwise it is the next lowest or next highest representable value (depending on the compiler).

=== Explicit type conversion (casting) ===
'''Explicit type conversion''' (casting) is the use of direct and specific notation in the source code to request a conversion or to specify a member from an overloaded class. There are cases where no automatic type conversion can occur or where the compiler is unsure about what type to convert to, those cases require explicit instructions from the programmer or will result in error.

==== Specific type casts ====
A set of casting operators have been introduced into the C++ language to address the shortcomings of the old C-style casts, maintained for compatibility purposes. Bringing with them an clearer syntax, improved semantics and type-safe conversions.

All of the casting operators share a similar syntax and as we will see are used in a manner similar to [[C++ Programming/Templates|templates]], with these new keywords casting becomes easier to understand, find, and maintain.

;The basic form of type cast
The basic explicit form of typecasting is the static cast. 

A static cast looks like this:
<source lang=cpp>
static_cast<target type>(expression)
</source>

The compiler will try its best to interpret the ''expression'' as if it would be of type ''type''. This type of cast will not produce a warning, even if the type is demoted.

<source lang=cpp>
int a = static_cast<int>(7.5);
</source>

The cast can be used to suppress the warning as shown above.  {{C++ Programming/kw|static_cast}} cannot do all conversions; for example, it cannot remove const qualifiers, and it cannot perform "cross-casts" within a class hierarchy.  It can be used to perform most numeric conversions, including conversion from a integral value to an enumerated type.

===== {{C++ Programming/kw|static_cast}} =====
{{:C++ Programming/Programming Languages/C++/Code/Keywords/static cast}}

===== {{C++ Programming/kw|const_cast}} =====
{{:C++ Programming/Programming Languages/C++/Code/Keywords/const cast}}

===== {{C++ Programming/kw|dynamic_cast}} =====
{{:C++ Programming/Programming Languages/C++/Code/Keywords/dynamic cast}}

===== {{C++ Programming/kw|reinterpret_cast}} =====
{{:C++ Programming/Programming Languages/C++/Code/Keywords/reinterpret cast}}

==== Old C-style casts ====
Other common type casts exist, they are of the form <tt>type(expression)</tt> (a functional, or function-style, cast) or <tt>(type)expression</tt> (often known simply as a C-style cast). The format of <tt>(type)expression</tt> is more common in C (where it is the only cast notation). It has the basic form:
<source lang=cpp>
int i = 10;
long l;
 
l = (long)i; //C programming style cast
l = long(i); //C programming style cast in functional form (preferred by some C++ programmers) 
             //note: initializes a new long to i, this is not an explicit cast as in the example above
             //however an implicit cast does occur. i = long((long)i);
</source>

A C-style cast can, in a single line of source code, make two conversions. For instance remove a variable consteness and alter it's type. In C++, the old C-style casts are retained for backwards compatibility.
<source lang=cpp>
const char string[]="1234";
function( (unsigned char*) string ); //remove const, add unsigned
</source>

There are several shortcomings in the old C-style casts:
#They allows casting practically any type to any other type. Leading to lots of unnecessary trouble, even to creating source code that will compile but not to the intended result. 
#The syntax is the same for every casting operation. Making it impossible for the compiler and users to tell the intended purpose of the cast.
#Hard to identify in the source code.

The C++ specific cast keyword are more controlled. Some will make the code safer since they will enable to catch more errors at compile-time, and all are easier to search, identify and maintain in the source code. Performance wise they are the same with the exception of {{C++ Programming/kw|dynamic_cast}}, for which there is no C equivalent. This makes them generally preferred.
[[Category:C++ Programming|{{SUBPAGENAME}}]]
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
