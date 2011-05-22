>==Structures==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">==Structures==
A simple implementation of the object paradigm from (OOP) that holds collections of data records (also known as ''compound values'' or ''set''). A <tt>struct</tt> is like a class '''except for the default access''' (class has default access of private, struct has default access of public). C++ also guarantees that a struct that only contains C types is equivalent to the same C struct thus allowing access to legacy C functions, it can (but may not) also have constructors (and must have them, if a templated class is used inside a <tt>struct</tt>), as with Classes the compiler implicitly-declares a destructor if the struct doesn’t have a user-declared destructor. Structures will also allow [[C++ Programming/Operators/Operator Overloading|Operator Overloading]].

A struct is ''defined'' by:
<source lang=cpp>
struct myStructType /*: inheritances */ {
public: 
 // public members
protected:
 // protected members
private:
 // private members
} myStructName;
</source>

Because it is not supported in C, it is uncommon to have structs in C++ using inheritances even though they are supported just like in classes. The more distinctive aspect is that structs can have two identities one is in reference to the type and another to the specific object.
The public access label can sometimes be ignored since the default state of struct for member functions and fields is public.

An object of type ''myStructType'' (case-sensitive) is ''declared'' using:
<source lang=cpp>
 myStructType obj1;
</source>

<br>{{NOTE|From a technical viewpoint, a struct and a class are practically the same thing. A struct can be used anywhere a class can be and vice-versa, the only technical difference is that class members default to ''private'' and struct members default to ''public''. Structs can be made to behave like classes simply by putting in the keyword private at the beginning of the struct. Other than that it is mostly a difference in convention.}}

; Why should you Use Structs, Not Classes?
Older programmer languages used a similar type called Record (i.e.: COBOL, FORTRAN) this was implemented in C as the struct keyword. And so C++ uses structs to comply with this C's heritage (the code and the programmers). Structs are simpler to be managed by the programmer and the compiler. One should use a <tt>struct</tt> for POD ([[wiki:PlainOldData|PlainOldData]]) types that have no methods and whose data members are all <tt>public</tt>.
<tt>struct</tt> may be used more efficiently in situations that default to public inheritance (which is the most common kind) and where <tt>public</tt> access (which is what you want if you list the public interface first) is the intended effect. Using a <tt>class</tt>, you typically have to insert the keyword public in two places, for no real advantage. In the end it's just a matter of convention, which programmers should be able to get used to.

; Point objects :
As a simple example of a compound structure, consider the concept of a mathematical point. At one level, a point is two numbers (coordinates) that we treat collectively as a single object. In mathematical notation, points are often written in parentheses, with a comma separating the coordinates. For example, (0, 0) indicates the origin, and (x, y) indicates the point x units to the right and y units up from the origin.

The natural way to represent a point is using two doubles. The '''structure''' or <tt>struct</tt> is one of the solutions to group these two values into a compound object.

<source lang=cpp>
// A struct definition:
 struct Point { 
   double x, y; }; 
</source>

This definition indicates that this structure contains two members, named <tt>x</tt> and <tt>y</tt>. These members are also called instance variables, for reasons I will explain a little later.

It is a common error to leave off the semi-colon at the end of a structure definition. It might seem odd to put a semi-colon after a squiggly-brace, but you'll get used to it. This syntax is in place to allow the programmer the facility to create an instance[s] of the struct when it is defined.

Once you have defined the new structure, you can create variables with that type:

<source lang=cpp>
struct Point blank; 
blank.x = 3.0; 
blank.y = 4.0; 
</source>

The first line is a conventional variable declaration: blank has type Point. The next two lines initialize the instance variables of the structure. The "dot notation" used here is similar to the syntax for invoking a function on an object, as in <tt>fruit.length()</tt>. Of course, one difference is that function names are always followed by an argument list, even if it is empty.

As usual, the name of the variable blank appears outside the box and its value appears inside the box. In this case, that value is a compound object with two named instance variables.

; Accessing instance variables :
You can read the values of an instance variable using the same syntax we used to write them:

<source lang=cpp>
int x = blank.x; 
</source>

The expression <tt>blank.x</tt> means "go to the object named blank and get the value of the member named x." In this case we assign that value to a local variable named <tt>x</tt>. Notice that there is no conflict between the local variable named <tt>x</tt> and the instance variable named <tt>x</tt>. The purpose of dot notation is to identify which variable you are referring to unambiguously.

You can use dot notation as part of any expression, so the following are legal.

<source lang=cpp>
cout << blank.x << ", " << blank.y << endl; 
double distance = sqrt(blank.x * blank.x + blank.y * blank.y); 
</source>

The first line outputs 3, 4; the second line calculates the value 5.

; Operations on structures :
Most of the operators we have been using on other types, like mathematical operators ( <tt>+</tt>, <tt>%</tt>, etc.) and comparison operators (<tt>==</tt>, <tt>></tt>, etc.), do not work on structures. Actually, it is possible to define the meaning of these operators for the new type, but we won't do that in this book.

On the other hand, the assignment operator does work for structures. It can be used in two ways: to initialize the instance variables of a structure or to copy the instance variables from one structure to another. An initialization looks like this:

<source lang=cpp>
Point blank = { 3.0, 4.0 }; 
</source>

The values in curly brackets get assigned to the instance variables of the structure one by one, in order. So in this case, <tt>x</tt> gets the first value and y gets the second.

Unfortunately, this syntax can be used only in an initialization, not in an assignment statement. Therefore, the following is illegal.

<source lang=cpp>
Point blank; 
blank = { 3.0, 4.0 }; // WRONG !! 
</source>

You might wonder why this perfectly reasonable statement should be illegal, and there is no good answer.  (Note, however, that a ''similar'' syntax is legal in C since 1999, and is under consideration for possible inclusion in C++ in the future.)

On the other hand, it is legal to assign one structure to another. For example:

<source lang=cpp>
Point p1 = { 3.0, 4.0 }; 
Point p2 = p1; 
cout << p2.x << ", " <<  p2.y << endl; 
</source>

The output of this program is <code>3, 4</code>.

; Structures as return types :
You can write functions that return structures. For example, findCenter takes a Rectangle as an argument and returns a Point that contains the coordinates of the center of the Rectangle:

<source lang=cpp>
Point findCenter (Rectangle& box) 
{ 
  double x = box.corner.x + box.width/2; 
  double y = box.corner.y + box.height/2; 
  Point result = {x, y}; 
  return result; 
} 
</source>

To call this function, we have to pass a box as an argument (notice that it is being passed by reference), and assign the return value to a Point variable:

<source lang=cpp>
Rectangle box = { {0.0, 0.0}, 100, 200 }; 
Point center = findCenter (box); 
printPoint (center); 
</source>

The output of this program is (50, 100).

; Passing other types by reference :
It's not just structures that can be passed by reference. All the other types we've seen can, too. For example, to swap two integers, we could write something like:

<source lang=cpp>
void swap (int& x, int& y) 
{ 
  int temp = x; 
  x = y; 
  y = temp; 
} 
</source>

We would call this function in the usual way:

<source lang=cpp>
int i = 7; 
int j = 9; 
swap (i, j); 
cout << i << j << endl; 
</source>

The output of this program is 97. Draw a stack diagram for this program to convince yourself this is true. If the parameters x and y were declared as regular parameters (without the &s), swap would not work. It would modify x and y and have no effect on i and j.

When people start passing things like integers by reference, they often try to use an expression as a reference argument. For example:

<source lang=cpp>
int i = 7; 
int j = 9; 
swap (i, j+1); // WRONG!! 
</source>

This is not legal because the expression <tt>j+1</tt> is not a variable — it does not occupy a location that the reference can refer to. It is a little tricky to figure out exactly what kinds of expressions can be passed by reference. For now, a good rule of thumb is that reference arguments have to be variables.

;Pointers and structures:
Structures can also be pointed by pointers and store pointers. The rules are the same as for any fundamental data type. The pointer must be declared as a pointer to the structure.

=== Nesting structures ===
Structures can also be nested so that a valid element of a structure can also be another structure.

<source lang=cpp>
//of course you have to define the Point struct first!

struct Rectangle {
 Point upper_left; 
 Point upper_right;
 Point lower_left;
 Point lower_right;
};
</source>

=== this ===
The '''this''' keyword is a implicitly created pointer that is only accessible within nonstatic member functions of a struct (or a union or class) and points to the object for which the member function is called. The this pointer is not available in static member functions. This will be restated again on when introducing unions a more in depth analysis is provided in the [[C++ Programming/Classes/this|Section about classes]].

{{BookCat}}
