>== Templates ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== Templates ==
Templates are a way to make code more reusable. Trivial examples include creating generic data structures which can store arbitrary data types. Templates are of great utility to programmers, especially when combined with multiple [[w:Inheritance in object-oriented programming|inheritance]] and [[w:operator overloading|operator overloading]]. The [[C++ Programming/STL|Standard Template Library]] (STL) provides many useful functions within a framework of connected templates.

As the templates are very expressive they may be used for things other than generic programming.  One such use is called [[w:template metaprogramming|template metaprogramming]], which is a way of pre-evaluating some of the code at compile-time rather than run-time.  Further discussion here only relates to templates as a method of generic programming.

By now you should have noticed that functions that perform the same tasks tend to look similar. For example, if you wrote a function that prints an int, you would have to have the int declared first. This way, the possibility of error in your code is reduced, however, it gets somewhat annoying to have to create different versions of functions just to handle all the different data types you use. For example, you may want the function to simply print the input variable, regardless of what type that variable is. Writing a different function for every possible input type (<tt>double</tt>,<tt>char *</tt>, etc. ...) would be extremely cumbersome. That is where templates come in.

Templates solve some of the same problems as macros, generate "optimized" code at compile time, but are subject to C++'s strict type checking.

Parameterized types, better known as templates, allow the programmer to create one function that can handle many different types. Instead of having to take into account every data type, you have one arbitrary parameter name that the compiler then replaces with the different data types that you wish the function to use, manipulate, etc.

*Templates are instantiated at compile-time with the source code. 
*Templates are type safe. 
*Templates allow user-defined specialization. 
*Templates allow non-type parameters. 
*Templates use “lazy structural constraints”. 
*Templates support mix-ins. 

; Syntax for Templates :
Templates are pretty easy to use, just look at the syntax:

<source lang=cpp>
 template <class TYPEPARAMETER> 
</source>

(or, equivalently, and preferred by some)

<source lang=cpp>
 template <typename TYPEPARAMETER> 
</source>

=== Function template ===
There are two kinds of templates.  A ''function template'' behaves like a function that can accept arguments of many different types.  For example, the Standard Template Library contains the function template <tt>max(x, y)</tt> which returns either <tt>x</tt> or <tt>y</tt>, whichever is larger.  <tt>max()</tt> could be defined like this:

<source lang=cpp>
    template <typename TYPEPARAMETER>
    TYPEPARAMETER max(TYPEPARAMETER x, TYPEPARAMETER y)
    {
        if (x < y)
            return y;
        else
            return x;
    }
</source>

This template can be called just like a function:

<source lang=cpp>
    std::cout << max(3, 7);   // outputs 7
</source>

The compiler determines by examining the arguments that this is a call to <tt>max(int, int)</tt> and ''instantiates'' a version of the function where the type <tt>TYPEPARAMETER</tt> is <tt>int</tt>.

This works whether the arguments <tt>x</tt> and <tt>y</tt> are integers, strings, or any other type for which it makes sense to say <tt>x</tt> &lt; <tt>y</tt>". If you have defined your own data type, you can use operator overloading to define the meaning of <code>&lt;</code> for your type, thus allowing you to use the <tt>max()</tt> function. While this may seem a minor benefit in this isolated example, in the context of a comprehensive library like the STL it allows the programmer to get extensive functionality for a new data type, just by defining a few operators for it.  Merely defining <tt>&lt;</tt> allows a type to be used with the standard <tt>sort()</tt>, <tt>stable_sort()</tt>, and <tt>binary_search()</tt> algorithms; data structures such as <code>set</code>s, heaps, and associative arrays; and more.

As a counterexample, the standard type <tt>complex</tt> does not define the <tt>&lt;</tt> operator, because there is no strict order on [[w:complex number|complex number]]s.  Therefore <code>max(x, y)</code> will fail with a compile error if ''x'' and ''y'' are <tt>complex</tt> values.  Likewise, other templates that rely on <tt>&lt;</tt> cannot be applied to <tt>complex</tt> data.  Unfortunately, compilers historically generate somewhat esoteric and unhelpful error messages for this sort of error. Ensuring that a certain object adheres to a [[w:protocol (computer science)|method protocol]] can alleviate this issue.

<tt>{TYPEPARAMETER}</tt> is just the arbitrary '''TYPEPARAMETER''' name that you want to use in your function. Some programmers prefer using just <tt>T</tt> in place of <tt>TYPEPARAMETER</tt>.

Let us say you want to create a swap function that can handle more than one data type... something that looks like this:

<source lang=cpp>
 template <class SOMETYPE> 
 void swap (SOMETYPE &x, SOMETYPE &y) 
 { 
   SOMETYPE temp = x; 
   x = y; 
   y = temp; 
 } 
</source>

The function you see above looks really similar to any other swap function, with the differences being the template <class SOMETYPE> line before the function definition and the instances of SOMETYPE in the code. Everywhere you would normally need to have the name or class of the datatype that you're using, you now replace with the arbitrary name that you used in the template <class SOMETYPE>. For example, if you had '''SUPERDUPERTYPE''' instead of '''SOMETYPE''', the code would look something like this:

<source lang=cpp>
 template <class SUPERDUPERTYPE> 
 void swap (SUPERDUPERTYPE &x, SUPERDUPERTYPE &y) 
 { 
   SUPERDUPERTYPE temp = x; 
   x = y; 
   y = temp; 
 } 
</source>

As you can see, you can use whatever label you wish for the template '''TYPEPARAMETER''', as long as it is not a reserved word.

=== Class template ===
A ''class template'' extends the same concept to classes.  Class templates are often used to make generic containers. For example, the STL has a [[w:linked list|linked list]] container. To make a linked list of integers, one writes <tt>list&lt;int&gt;</tt>. A list of strings is denoted <tt>list&lt;string&gt;</tt>. A <tt>list</tt> has a set of standard functions associated with it, which work no matter what you put between the brackets.

If you want to have more than one template '''TYPEPARAMETER''', then the syntax would be:
<source lang=cpp>
 template <class SOMETYPE1, class SOMETYPE2, ...> 
</source>

;Templates and Classes
Let us say that rather than create a simple templated function, you would like to use templates for a class, so that the class may handle more than one datatype. You may have noticed that some classes from are able to accept a type as a parameter and create variations of an object based on that type (for example the classes of the STL container class hierarchy). This is because they are declared as templates using syntax not unlike the one presented below:

<source lang=cpp>
 template <class T> class Foo
 {
 public:
   Foo();
   void some_function();
   T some_other_function();

 private:
   int member_variable;
   T parametrized_variable;
 };
</source>

Defining member functions of a template class is somewhat like defining a function template, except for the fact, that you use the scope resolution operator to indicate that this is the template classes' member function. The one important and non-obvious detail is the requirement of using the template operator containing the parametrized type name after the class name.

The following example describes the required syntax by defining functions from the example class above.

<source lang=cpp>
 template <class T> Foo<T>::Foo()
 {
   member_variable = 0;
 }

 template <class T> void Foo<T>::some_function()
 {
   cout << "member_variable = " << member_variable << endl;
 }

 template <class T> T Foo<T>::some_other_function()
 {
   return parametrized_variable;
 }
</source>

As you may have noticed, if you want to declare a function that will return an object of the parametrized type, you just have to use the name of that parameter as the function's return type.

{{NOTE|A class template can be used to avoid the overhead of virtual member functions in inheritance. Since the type of class is known at compile-time, the class template will no for the virtual pointer table that is required by a class with virtual member functions. This distinction also permits the inlining of the function members of a class template.}}

=== Advantages and disadvantages ===
Some uses of templates, such as the <tt>max()</tt> function, were previously filled by function-like [[w:preprocessor|preprocessor]] [[w:macro|macro]]s.

<source lang=cpp>
// a max() macro
#define max(a,b)   ((a) < (b) ? (b) : (a))
</source>

Both macros and templates are expanded at compile time.  Macros are always expanded inline; templates can also be expanded as inline functions when the compiler deems it appropriate.  Thus both function-like macros and function templates have no run-time overhead.

However, templates are generally considered an improvement over macros for these purposes.  Templates are type-safe.  Templates avoid some of the common errors found in code that makes heavy use of function-like macros.  Perhaps most importantly, templates were designed to be applicable to much larger problems than macros.  The definition of a function-like macro must fit on a single logical line of code.

There are three primary drawbacks to the use of templates.  First, many compilers historically have very poor support for templates, so the use of templates can make code somewhat less portable.  Second, almost all compilers produce confusing, unhelpful error messages when errors are detected in template code.  This can make templates difficult to develop.  Third, each use of a template may cause the compiler to generate extra code (an ''instantiation'' of the template), so the indiscriminate use of templates can lead to [[w:code bloat|code bloat]], resulting in excessively large executables.

The other big disadvantage of templates is that to replace a #define like max which acts identically with dissimilar types or function calls is impossible. Templates have replaced using #defines for complex functions but not for simple stuff like max(a,b). For a full discussion on trying to create a template for the #define max, see the paper [http://www.aristeia.com/Papers/C%2B%2BReportColumns/jan95.pdf "Min, Max and More"] that Scott Meyer wrote for ''C++ Report'' in January 1995.

The biggest advantage of using templates, is that a complex algorithm can have a simple interface that the compiler then uses to choose the correct implementation based on the type of the arguments. For instance, a searching algorithm can take advantage of the properties of the container being searched. This technique is used throughout the C++ standard library.

=== Linkage problems ===
While linking a template-based program consisting over several modules spread over a couple files, it is a frequent and mystifying situation to find that the object code of the modules won't link due to 'unresolved reference to (insert template member function name here) in (...)'. The offending function's implementation is there, so why is it missing from the object code? Let us stop a moment and consider how can this be possible.

Assume you have created a template based class called Foo and put its declaration in the file Util.hpp along with some other regular class called Bar:

<source lang=cpp>
 template <class T> Foo
 {
 public: 
   Foo();
   T some_function();
   T some_other_function();
   T some_yet_other_function();
   T member;
 };

 class Bar
 {
   Bar();
   void do_something();
 };
</source>

Now, to adhere to all the rules of the art, you create a file called Util.cc, where you put all
the function definitions, template or otherwise:

<source lang=cpp>
 #include "Util.hpp"

 template <class T> T Foo<T>::some_function()
 {
  ...
 }

 template <class T> T Foo<T>::some_other_function()
 {
  ...
 }

 template <class T> T Foo<T>::some_yet_other_function()
 {
  ...
 }
</source>

and, finally:

<source lang=cpp>
 void Bar::do_something()
 {
   Foo<int> my_foo;
   int x = my_foo.some_function();
   int y = my_foo.some_other_function();
 }
</source>

Next, you compile the module, there are no errors, you are happy. But suppose there is an another 
(main) module in the program, which resides in MyProg.cc:

<source lang=cpp>
 #include "Util.hpp"	// imports our utility classes' declarations, including the template

 int main()
 {
   Foo<int> main_foo;
   int z = main_foo.some_yet_other_function();
   return 0;
 }
</source>

This also compiles clean to the object code. Yet when you try to link the two modules together, you get an error saying there is an undefined reference to Foo<int>::some_yet_other function() in
MyProg.cc. You defined the template member function correctly, so what is the problem?

As you remember, templates are instantiated at compile-time. This helps avoid code bloat,
which would be the result of generating all the template class and function variants for all
possible types as its parameters. So, when the compiler processed the Util.cc code, it saw that
the only variant of the Foo class was Foo<int>, and the only needed functions were: 

<source lang=cpp>
 int Foo<int>::some_function();
 int Foo<int>::some_other_function();
</source>

No code in Util.cc required any other variants of Foo or its methods to exist, so the compiler
generated no code other than that. There is no implementation of some_yet_other_function() in the object code, just as there is no implementation for

<source lang=cpp>
 double Foo<double>::some_function();
</source>

or

<source lang=cpp>
 string Foo<string>::some_function();
</source>

The MyProg.cc code compiled without errors, because the member function of Foo it uses is correctly declared in the Util.hpp header, and it is expected that it will be available upon linking. But it is not and hence the error, and a lot of nuisance if you are new to templates
and start looking for errors in your code, which ironically is perfectly correct.

The solution is somewhat compiler dependent. For the GNU compiler, try experimenting with the -frepo flag, and also reading the template-related section of 'info gcc' (node "Template Instantiation": "Where is the Template?") may prove enlightening. In Borland, supposedly, there is a selection in the linker options, which activates 'smart' templates just for this kind of problem.

The other thing you may try is called explicit instantiation. What you do is create some dummy code
in the module with the templates, which creates all variants of the template class and calls all
variants of its member functions, which you know are needed elsewhere. Obviously, this requires you to know a lot about what variants you need throughout your code. In our simple example this would go like this:

1. Add the following class declaration to Util.hpp:

<source lang=cpp>
 class Instantiations
 {
 private:
   void Instantiate();
 };
</source>

2. Add the following member function definition to Util.cc:

<source lang=cpp>
 void Instantiations::Instantiate()
 {
   Foo<int> my_foo;
   my_foo.some_yet_other_function();
   // other explicit instantiations may follow
 }
</source>

Of course, you never need to actual instantiate the Instantiations class, or call any of its methods. The fact that they just exist in the code makes the compiler generate all the template variations which are required. Now the object code will link without problems.

There is still one, if not elegant, solution. Just move all the template functions' definition code to the Util.hpp header file. This is not pretty, because header files are for declarations, and the implementation is supposed to be defined elsewhere, but it does the trick in this situation. 
While compiling the MyProg.cc (and any other modules which include Util.hpp) code, the compiler will generate all the template variants which are needed, because the definitions are readily available.

[[Category:C++ Programming|{{SUBPAGENAME}}]]
