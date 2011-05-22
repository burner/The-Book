>== Programming paradigms ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== Programming paradigms ==
A '''[[w:programming paradigm|programming paradigm]]''' is a model of programming based on distinct concepts that shapes the way programmers design, organize and write programs.  A [[w:multiparadigm programming language|multi-paradigm programming language]] allows programmers to choose a specific single approach or mix parts of different programming paradigms. C++ as a multi-paradigm programming language supports single or mixed approaches using Procedural or Object-oriented programming and mixing in utilizations of Generic and even Functional programming concepts.

=== Procedural programming ===
'''[[w:Procedural programming|Procedural programming]]''' can be defined as a subtype of [[w:Imperative programming|imperative programming]] as a programming paradigm based upon the concept of procedure calls, in which '''''[[C++ Programming/Programming Languages/C++/Code/Statements|statements]]''''' are structured into procedures (also known as subroutines or [[C++ Programming/Code/Statements/Functions|functions]]). Procedure calls are modular and are bound by scope. A procedural program is composed of one or more [[w:module (programming)|modules]]. Each module is composed of one or more [[w:subprogram (programming)|subprograms]]. Modules may consist of procedures, functions, subroutines or methods, depending on the programming language. Procedural programs may possibly have multiple levels or scopes, with subprograms defined inside other subprograms. Each scope can contain names which cannot be seen in outer scopes.

Procedural programming offers many benefits over simple sequential programming since procedural code:
* is easier to read and more maintainable
* is more flexible
* facilitates the practice of good program design
* allows modules to be reused in the form of '''''[[C++ Programming/Compiler/Linker/Libraries|code libraries]]'''''.

{{NOTE|Nowadays it is very rare to see C++ strictly using the Procedural Programming paradigm, mostly it is used only on small demonstration or test programs.}}

=== Statically typed ===
'''Typing''' refers to how a computer language handles its variables, how they are differentiated by '''''[[C++ Programming/Programming Languages/C++/Code/Statements/Variables/Type|type]]'''''. Variables are values that the program uses during execution. These values can change; they are variable, hence their name. ''Static typing'' usually results in compiled code that executes more quickly. When the compiler knows the exact types that are in use, it can produce machine code that does the right thing easier. In C++, variables need to be defined before they are used so that compilers know what type they are, and hence is statically typed.  Languages that are not statically typed are called ''dynamically typed''.

Static typing usually finds type errors more reliably at compile time, increasing the reliability of compiled programs. Simply put, it means that "A round peg won't fit in a square hole", so the compiler will report it when a type leads to ambiguity or incompatible usage. However, programmers disagree over how common type errors are and what proportion of bugs that are written would be caught by static typing. Static typing advocates believe programs are more reliable when they have been type checked, while dynamic typing advocates point to dynamic code that has proved reliable and to small bug databases. The value of static typing, then, presumably increases as the strength of the type system is increased.

A statically typed system constrains the use of powerful language constructs more than it constrains less powerful ones. This makes powerful constructs harder to use, and thus places the burden of choosing the "right tool for the problem" on the shoulders of the programmer, who might otherwise be inclined to use the most powerful tool available. Choosing overly powerful tools may cause additional performance, reliability or correctness problems, because there are [[w:Computational complexity theory|theoretical limits]] on the properties that can be expected from powerful language constructs. For example, indiscriminate use of [[w:recursion|recursion]] or [[w:global variable|global variable]]s may cause well-documented adverse effects.

Static typing allows construction of libraries which are less likely to be accidentally misused by their users. This can be used as an additional mechanism for communicating the intentions of the library developer.

=== Type checking ===
'''Type checking''' is the process of verifying and enforcing the constraints of types, which can occur at either compile-time or run-time. Compile time checking, also called ''static type'' checking, is carried out by the compiler when a program is compiled. Run time checking, also called ''dynamic type checking'', is carried out by the program as it is running. A programming language is said to be ''strongly typed'' if the type system ensures that conversions between types must be either valid or result in an error. A ''weakly typed'' language on the other hand makes no such guarantees and generally allows automatic conversions between types which may have no useful purpose. C++ falls somewhere in the middle, allowing a mix of automatic type conversion and programmer defined conversions, allowing for almost complete flexibility in interpreting one type as being of another type. Converting variables or expression of one type into another type is called '''''[[C++ Programming/Programming Languages/C++/Code/Statements/Variables/Type Casting|type casting]]'''''.

=== Object-oriented programming ===
'''[[w:Object-oriented programming|Object-oriented programming]]''' can be seen as an extension of procedural programming in which programs are made up of collection of individual units called '''objects''' that have a distinct purpose and function with limited or no dependencies on [[w:implementation|implementation]]. For example, a car is like an object; it gets you from point A to point B with no need to know what type of engine the car uses or how the engine works. Object-oriented languages usually provide a means of [[w:documentation|documenting]] what an object can and cannot do, like instructions for driving a car.

==== Objects and Classes ====
An '''object''' is composed of '''members''' and '''methods'''. The members (also called ''data members'', ''characteristics'', ''attributes'', or ''properties'') describe the object. The methods generally describe the actions associated with a particular object. Think of an object as a noun, its members as adjectives describing that noun, and its methods as the verbs that can be performed by or on that noun.

For example, a sports car is an object. Some of its members might be its height, weight, acceleration, and speed. An object's members just hold data about that object. Some of the methods of the sports car could be "drive", "park", "race", etc. The methods really do not mean much unless associated with the sports car, and the same goes for the members.

The blueprint that lets us build our sports car object is called a '''class'''. A class does not tell us how fast our sports car goes, or what color it is, but it does tell us that our sports car will have a member representing speed and color, and that they will be say, a number and a word, respectively. The class also lays out the methods for us, telling the car how to park and drive, but these methods can not take any action with just the blueprint - they need an object to have an effect.

===== Encapsulation =====
{{SAMPLE|sampletext=''«No component in a complex system should depend on the internal details of any other component.»''|caption=<small>--Dan Ingalls (Smalltalk Architect)
</small>}}

Encapsulation, the principle of [[w:Information hiding|information hiding]] (from the user), 
is the process of hiding the data structures of the class and allowing changes in the data through a public interface where the incoming values are checked for validity, and so not only it permits the hiding of data in an object but also of behavior. This prevents clients of an interface from depending on those parts of the implementation that are likely to change in future, thereby allowing those changes to be made more easily, that is, without changes to clients. In modern programming languages, the principle of information hiding manifests itself in a number of ways, including encapsulation and polymorphism.

{{:C++ Programming/Programming Languages/Paradigms/Inheritance}}
{{:C++ Programming/Programming Languages/Paradigms/Inheritance/Multiple Inheritance}}
{{:C++ Programming/Programming Languages/Paradigms/Polymorphism}}

=== Generic programming ===
'''[[w:Generic programming|Generic programming]]''' or '''[[w:polymorphism (computer science)|polymorphism]]''' is a programming style that emphasizes techniques that allow one value to take on different types as long as certain contracts such as [[w:subtype|subtypes]] and [[w:signature (computer science)|signature]] are kept. In simpler terms generic programming is based in finding the most abstract representations of efficient algorithms. [[w:template (programming)|Templates]] popularized the notion of generics. Templates allow code to be written without consideration of the [[w:datatype|type]] with which it will eventually be used. Templates are defined in the [[C++ Programming/STL|Standard Template Library (STL)]], where generic programming was introduced into C++.

=== Free-form ===
'''Free-form''' refers to how the programmer crafts the code. Basically, there are no rules on how you choose to write your program, save for the semantic rules of C++. Any C++ program should compile as long as it is legal C++.

The free-form nature of C++ is used (or abused, depending on your point of view) by some programmers in crafting obfuscated C++ (C++ that is purposefully written to be difficult to understand). The use of obfuscation is regarded by some as a security mechanism, ensuring that the source code is more difficult to analyze by the average user or to prevent the functionality from being duplicated.

{{BookCat}}
