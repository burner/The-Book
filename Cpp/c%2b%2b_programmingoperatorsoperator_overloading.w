>== Operator overloading ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== Operator overloading ==
'''Operator overloading''' (less commonly known as [[w:ad-hoc|ad-hoc]] [[w:type polymorphism|polymorphism]]) is a specific case of [[w:polymorphism (computer science)|polymorphism]] (part of the OO nature of the language) in which some or all operators like <tt>+</tt>, <tt>=</tt> or <tt>==</tt> are treated as polymorphic functions and as such have different behaviors depending on the types of its arguments.
Operator overloading is usually only [[w:syntactic sugar|syntactic sugar]]. It can easily be emulated using function calls.

Consider this operation:
<source lang=cpp>
add (a, multiply (b,c))
</source>


Using operator overloading permits a more concise way of writing it, like this:
 a + b &times; c

(Assuming the <tt>&times;</tt> operator has higher [[w:precedence|precedence]] than <tt>+</tt>.)

Operator overloading provides more than an aesthetic benefit since the language allows operators to be invoked implicitly in some circumstances. 

Problems and critics to the use of operator overloading arise because it allows programmers to give operators completely free functionality, without an imposition of coherency that permits to consistently satisfy user/reader expectations, usage of the <code>&lt;&lt;</code> operator is an example of this problem.

<source lang=cpp>
// The expression
a << 1;
</source>

Will return twice the value of <tt>a</tt> if <tt>a</tt> is an integer variable, but if <tt>a</tt> is an output stream instead this will write "1" to it. Because operator overloading allows the programmer to change the usual semantics of an operator, it is usually considered good practice to use operator overloading with care.

To overload an operator is to provide it with a new meaning for user-defined types. This is done in the same fashion as defining a function. The basic syntax follows (where @ represents a valid operator):

<source lang=cpp>
return_type operator@(parameter_list)
{
    // ... definition
}
</source>

Not all operators may be overloaded, new operators cannot be created, and the precedence, associativity or arity of operators cannot be changed (for example ! cannot be overloaded as a binary operator). Most operators may be overloaded as either a member function or non-member function, some, however, must be defined as member functions. Operators should only be overloaded where their use would be natural and unambiguous, and they should perform as expected. For example, overloading + to add two complex numbers is a good use, whereas overloading * to push an object onto a vector would not be considered good style.

{{NOTE|Operator overloading should only be utilized when the meaning of the overloaded operator's operation is unambiguous and practical for the underlying type and where it would offer a significant notational brevity over appropriately named function calls.}}

; A simple Message Header
<source lang=cpp>
// sample of Operator Overloading

#include <string>

class PlMessageHeader
{
    std::string m_ThreadSender;
    std::string m_ThreadReceiver;

    //return true if the messages are equal, false otherwise
    inline bool operator == (const PlMessageHeader &b) const
    {
        return ( (b.m_ThreadSender==m_ThreadSender) &&
                (b.m_ThreadReceiver==m_ThreadReceiver) );
    }

    //return true if the message is for name
    inline bool isFor (const std::string &name) const
    {
        return (m_ThreadReceiver==name);
    }

    //return true if the message is for name
    inline bool isFor (const char *name) const
    {
        return (m_ThreadReceiver==name);// since name type is std::string, it becomes unsafe if name == NULL
    }
};
</source>

{{NOTE|note=
    The use of the <tt>inline</tt> keyword in the example above is technically redundant, as functions defined within a class definition like this are implicitly inline.}}

=== Operators as member functions ===
Aside from the operators which must be members, operators may be overloaded as member or non-member functions. The choice of whether or not to overload as a member is up to the programmer. Operators are generally overloaded as members when they:

# change the left-hand operand, or
# require direct access to the non-public parts of an object.

When an operator is defined as a member, the number of explicit parameters is reduced by one, as the calling object is implicitly supplied as an operand. Thus, binary operators take one explicit parameter and unary operators none. In the case of binary operators, the left hand operand is the calling object, and no type [[C++ Programming/Programming Languages/C++/Code/Statements/Variables#Implicit type conversion|coercion]] will be done upon it. This is in contrast to non-member operators, where the left hand operand may be coerced.
    <source lang=cpp>
    // binary operator as member function
    Vector2D Vector2D::operator+(const Vector2D& right)const {...}

    // binary operator as non-member function
    Vector2D operator+(const Vector2D& left, const Vector2D& right) {...}

    // binary operator as member function with 2 arguments 
    friend Vector2D operator+(const Vector2D& left, const Vector2D& right) {...}

    // unary operator as member function
    Vector2D Vector2D::operator-()const {...}

    // unary operator as non-member function
    Vector2D operator-(const Vector2D& vec) {...}
    </source>

=== Overloadable operators ===

===== Arithmetic operators =====
*'''+''' (addition)
*'''-''' (subtraction)
*'''*''' (multiplication)
*'''/''' (division)
*'''%''' (modulus)


As binary operators, these involve two arguments which do not have to be the same type. These operators may be defined as member or non-member functions. An example illustrating overloading for the addition of a 2D mathematical vector type follows.
<source lang=cpp>
Vector2D operator+(const Vector2D& left, const Vector2D& right)
{
    Vector2D result;
    result.set_x(left.x() + right.x());
    result.set_y(left.y() + right.y());
    return result;
}
</source>
It is good style to only overload these operators to perform their customary arithmetic operation.

===== Bitwise operators =====
*'''^''' (XOR)
*'''|''' (OR)
*'''&''' (AND)
*'''~''' (complement)
*'''<<''' (shift left, insertion to stream)
*'''>>''' (shift right, extraction from stream)

All of the bitwise operators are binary, excepting complement, which is unary. It should be noted that these operators have a lower precedence than the arithmetic operators, so if ^ were to be overloaded for exponentiation, x ^ y + z may not work as intended. Of special mention are the shift operators, << and >>. These have been overloaded in the standard library for interaction with streams.  When overloading these operators to work with streams the rules below should be followed:

#overload << and >> as friends (so that it can access the private variables with the stream be passed in by references 
#(input/output modifies the stream, and copying is not allowed)
#the operator should return a reference to the stream it receives (to allow chaining, cout << 3 << 4 << 5)

; An example using a 2D vector:
<source lang=cpp>
friend ostream& operator<<(ostream& out, const Vector2D& vec) // output
{
    out << "(" << vec.x() << ", " << vec.y() << ")";
    return out;
}

friend istream& operator>>(istream& in, Vector2D& vec) // input
{
    double x, y;
    in >> x >> y;
    vec.set_x(x);
    vec.set_y(y);
    return in;
}

</source>

===== Assignment operator =====
The assignment operator, '''=''', '''must be a member function''', and is given default behavior for user-defined classes by the compiler, performing an assignment of every member using its assignment operator. This behavior is generally acceptable for simple classes which only contain variables. However, where a class contains references or pointers to outside resources, the assignment operator should be overloaded (as general rule, whenever a destructor and copy constructor are needed so is the assignment operator), otherwise, for example, two strings would share the same buffer and changing one would change the other.

In this case, an assignment operator should perform two duties:

# clean up the old contents of the object
# copy the resources of the other object

For classes which contain raw pointers, before doing the assignment, the assignment operator should check for self-assignment, which generally will not work (as when  the old contents of the object are erased, they cannot be copied to refill the object). Self assignment is generally a sign of a coding error, and thus for classes without raw pointers, this check is often omitted, as while the action is wasteful of cpu cycles, it has no other effect on the code.

; Example:
<source lang=cpp>
class BuggyRawPointer { // example of super-common mistake
    T *m_ptr;
    public:
    BuggyRawPointer(T *ptr) : m_ptr(ptr) {}
    BuggyRawPointer& operator=(BuggyRawPointer const &rhs) {
        delete m_ptr; // free resource; // Problem here!
        m_ptr = 0;
        m_ptr = rhs.m_ptr;
        return *this;
    };
};

BuggyRawPointer x(new T);
x = x; // We might expect this to keep x the same. This sets x.m_ptr == 0. Oops!

// The above problem can be fixed like so:
class WithRawPointer2 {
    T *m_ptr;
    public:
    WithRawPointer2(T *ptr) : m_ptr(ptr) {}
    WithRawPointer2& operator=(WithRawPointer2 const &rhs) {
        if (this != &rhs) {
            delete m_ptr; // free resource;
            m_ptr = 0;
            m_ptr = rhs.m_ptr;
        }
        return *this;
    };
};

WithRawPointer2 x2(new T);
x2 = x2; // x2.m_ptr unchanged.
</source>

Another common use of overloading the assignment operator is to declare the overload in the private part of the class and not define it. Thus any code which attempts to do an assignment will fail on two accounts, first by referencing a private member function and second fail to link by not having a valid definition. This is done for classes where copying is to be prevented, and generally done with the addition of a privately declared copy constructor

; Example:
<source lang=cpp>
class DoNotCopyOrAssign {
    public:
        DoNotCopyOrAssign() {};
    private:
        DoNotCopyOrAssign(DoNotCopyOrAssign const&);
        DoNotCopyOrAssign &operator=(DoNotCopyOrAssign const &);
};

class MyClass : public DoNotCopyOrAssign {
    public:
        MyClass();
};

MyClass x, y;
x = y; // Fails to compile due to private assignment operator;
MyClass z(x); // Fails to compile due to private copy constructor.
</source>

===== Relational operators =====
*'''==''' (equality)
*'''!=''' (inequality)
*'''>''' (greater-than)
*'''<''' (less-than)
*'''>=''' (greater-than-or-equal-to)
*'''<=''' (less-than-or-equal-to)

All relational operators are binary, and should return either true or false. Generally, all six operators can be based off a comparison function, or each other, although this is never done automatically (e.g. overloading > will not automatically overload < to give the opposite). There are, however, some templates defined in the header <utility>; if this header is included, then it suffices to just overload operator== and operator<, and the other operators will be provided by the STL.

===== Logical operators =====
*'''!''' (NOT)
*'''&&''' (AND)
*'''||''' (OR)

The ! operator is unary, && and || are binary. It should be noted that in normal use, && and || have "short-circuit" behavior, where the right operand may not be evaluated, depending on the left operand. When overloaded, these operators get function call precedence, and this short circuit behavior is lost. It is best to leave these operators alone.

; Example:
<source lang=cpp>
bool Function1();
bool Function2();

Function1() && Function2();
</source>
If the result of Function1() is false, then Function2() is not called.
<source lang=cpp>
MyBool Function3();
MyBool Function4();

bool operator&&(MyBool const &, MyBool const &);

Function3() && Function4()
</source>
Both Function3() and Function4() will be called no matter what the result of the call is to Function3()
This is a waste of CPU processing, and worse, it could have surprising unintended consequences compared to the expected "short-circuit" behavior of the default operators.  Consider:
<source lang=cpp>
extern MyObject * ObjectPointer;
bool Function1() { return ObjectPointer != null; }
bool Function2() { return ObjectPointer->MyMethod(); }
MyBool Function3() { return ObjectPointer != null; }
MyBool Function4() { return ObjectPointer->MyMethod(); }

bool operator&&(MyBool const &, MyBool const &);

Function1() && Function2(); // Does not execute Function2() when pointer is null
Function3() && Function4(); // Executes Function4() when pointer is null
</source>

===== Compound assignment operators =====
*'''+=''' (addition-assignment)
*'''-=''' (subtraction-assignment)
*'''*=''' (multiplication-assignment)
*'''/=''' (division-assignment)
*'''%=''' (modulus-assignment)
*'''&=''' (AND-assignment)
*'''|=''' (OR-assignment)
*'''^=''' (XOR-assignment)
*'''>>=''' (shift-right-assignment)
*'''<<=''' (shift-left-assignment)


Compound assignment operators should be overloaded as member functions, as they change the left-hand operand. Like all other operators (except basic assignment), compound assignment operators must be explicitly defined, they will not be automatically (e.g. overloading = and + will not automatically overload +=). A compound assignment operator should work as expected: A @= B should be equivalent to A = A @ B. An example of += for a two-dimensional mathematical vector type follows.
<source lang=cpp>
Vector2D& Vector2D::operator+=(const Vector2D& right)
{
    this->x += right.x;
    this->y += right.y;
    return *this;
}
</source>

===== Increment and decrement operators =====
*'''++''' (increment)
*'''--''' (decrement)

Increment and decrement have two forms, prefix (++i) and postfix (i++). To differentiate, the postfix version takes a dummy integer. Increment and decrement operators are most often member functions, as they generally need access to the private member data in the class. The prefix version in general should return a reference to the changed object. The postfix version should just return a copy of the original value. In a perfect world, A += 1, A = A + 1, A++, ++A should all leave A with the same value.

; Example:
    <source lang=cpp>
SomeValue& SomeValue::operator++() // prefix
{
    ++data;
    return *this;
}

SomeValue SomeValue::operator++(int unused) // postfix
{
    SomeValue result = *this;
    ++data;
    return result;
}
</source>
Often one operator is defined in terms of the other for ease in maintenance, especially if the function call is complex.
<source lang=cpp>
SomeValue SomeValue::operator++(int unused) // postfix
{
    SomeValue result = *this;
    ++(*this); // call SomeValue::operator++()
    return result;
}
</source>
===== Subscript operator =====
The subscript operator, '''[ ]''', is a binary operator which '''must be a member function''' (hence it takes only one explicit parameter, the index). The subscript operator is not limited to taking an integral index. For instance, the index for the subscript operator for the std::map template is the same as the type of the key, so it may be a string etc. The subscript operator is generally overloaded twice; as a non-constant function (for when elements are altered), and as a constant function (for when elements are only accessed).

===== Function call operator =====
The function call operator, '''( )''', is generally overloaded to create objects which behave like functions, or for classes that have a primary operation. The function call operator must be a member function, but has no other restrictions - it may be overloaded with any number of parameters of any type, and may return any type. A class may also have several definitions for the function call operator.

===== Address of, Reference, and Pointer operators =====
These three operators, operator&(), operator*() and operator->() can be overloaded. In general these operators are only overloaded for smart pointers, or classes which attempt to mimic the behavior of a raw pointer. The pointer operator, operator->() has the additional requirement that the result of the call to that operator, must return a pointer, or a class with an overloaded operator->(). In general A == *&A should be true.

; Example:
<source lang=cpp>
class T {
    public:
        const memberFunction() const;
};

// forward declaration
class DullSmartReference;

class DullSmartPointer {
    private:
        T *m_ptr;
    public:
        DullSmartPointer(T *rhs) : m_ptr(rhs) {};
        DullSmartReference operator*() const {
            return DullSmartReference(*m_ptr);
        }
        T *operator->() const {
            return m_ptr;
        }
};

class DullSmartReference {
    private:
        T *m_ptr;
    public:
        DullSmartReference (T &rhs) : m_ptr(&rhs) {}
        DullSmartPointer operator&() const {
            return DullSmartPointer(m_ptr);
        }
        // conversion operator
        operator T { return *m_ptr; }
};

DullSmartPointer dsp(new T);
dsp->memberFunction(); // calls T::memberFunction

T t;
DullSmartReference dsr(t);
dsp = &dsr;
t = dsr; // calls the conversion operator
</source>
These are extremely simplified examples designed to show how the operators can be overloaded and not the full details of a SmartPointer or SmartReference class. In general you won't want to overload all three of these operators in the same class.

===== Comma operator =====
The comma operator,() ''',''' can be overloaded. The language comma operator has left to right precedence, the operator,() has function call precedence, so be aware that overloading the comma operator has many pitfalls.

; Example:
<source lang=cpp>
MyClass operator,(MyClass const &, MyClass const &);

MyClass Function1();
MyClass Function2();

MyClass x = Function1(), Function2();
</source>
For non overloaded comma operator, the order of execution will be Function1(), Function2(); With the overloaded comma operator, the compiler can call either Function1(), or Function2() first.

===== Member access operators =====
The two member access operators, operator->() and operator->*() can be overloaded. The most common use of overloading these operators is with defining expression template classes, which is not a common programming technique. Clearly by overloading these operators you can create some very unmaintainable code so overload these operators only with great care.

When the -> operator is applied to a pointer value of type (T *), the language dereferences the pointer and applies the . member access operator (so x->m is equivalent to (*x).m).  However, when the -> operator is applied to a class instance, it is called as a unary postfix operator; it is expected to return a value to which the -> operator can again be applied.  Typically, this will be a value of type (T *), as in the example under [[#Address of.2C Reference.2C and Pointer operators|Address of, Reference, and Pointer operators]] above, but can also be a class instance with operator->() defined; the language will call operator->() as many times as necessary until it arrives at a value of type (T *).

===== Memory management operators =====
* '''new''' (allocate memory for object)
* '''new[ ]''' (allocate memory for array)
* '''delete''' (deallocate memory for object)
* '''delete[ ]''' (deallocate memory for array)

The memory management operators can be overloaded to customize allocation and deallocation (e.g. to insert pertinent memory headers). They should behave as expected, '''new''' should return a pointer to a newly allocated object on the heap, '''delete''' should deallocate memory, ignoring a NULL argument. To overload '''new''', several rules must be followed:

* '''new''' must be a member function 
* the return type must be ''void*''
* the first explicit parameter must be a ''size_t'' value

To overload '''delete''' there are also conditions:

* '''delete''' must be a member function (and cannot be virtual)
* the return type must be ''void''
* there are only two forms available for the parameter list, and only one of the forms may appear in a class:
** ''void*''
** ''void*, size_t''

===== Conversion operators =====
Conversion operators enable objects of a class to be either implicitly (coercion) or explicitly (casting) converted to another type. Conversion operators must be member functions, and should not change the object which is being converted, so should be flagged as constant functions. The basic syntax of a conversion operator declaration, and declaration for an int-conversion operator follows.
<source lang=cpp>
operator ''type''() const; // const is not necessary, but is good style
operator int() const;
</source>
Notice that the function is declared without a return-type, which can easily be inferred from the type of conversion. Including the return type in the function header for a conversion operator is a syntax error.
<source lang=cpp>
double operator double() const; // error - return type included
</source>
=== Operators which cannot be overloaded ===
*'''?:''' (conditional)
*'''.''' (member selection)
*'''.*''' (member selection with pointer-to-member)
*'''::''' (scope resolution)
*{{C++ Programming/kw|sizeof}} (object size information)
*'''typeid''' (object type information)

To understand the reasons why the language doesn't permit these operators to be overloaded, read "Why can't I overload dot, ::, {{C++ Programming/kw|sizeof}}, etc.?" at the Bjarne Stroustrup's C++ Style and Technique FAQ ( http://www.research.att.com/~bs/bs_faq2.html#overload-dot ).

[[Category:C++ Programming|{{SUBPAGENAME}}]]
