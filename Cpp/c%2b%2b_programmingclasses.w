>== Classes ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== Classes ==
Classes are used to create ''user defined types''.  An instance of a class is called an ''object'' and programs can contain any number of classes.  As with other types, object types are case-sensitive. 

Classes provide ''encapsulation'' as defined in the Object Oriented Programming (OOP) paradigm.  A class can have both data members and functions members associated with it.  Unlike the built-in types, the class can contain several variables and functions, those are called members.   

Classes also provide flexibility in the "''[[w:divide and conquer|divide and conquer]]''" scheme in program writing.  In other words, one programmer can write a class and guarantee an interface. Another programmer can write the main program with that expected interface. The two pieces are put together and compiled for usage. 

{{NOTE|From a technical viewpoint, a struct and a class are practically the same thing. A struct can be used anywhere a class can be and vice-versa, the only technical difference is that class members default to ''private'' and struct members default to ''public''. Structs can be made to behave like classes simply by putting in the keyword private at the beginning of the struct. Other than that it is mostly a difference in convention.

In C++  the standard there isn't a definition for method, when discussing with users of other languages, the use of method as representing a member function can at times become confusing or raise problems to interpretation, like referring to a static member function as a static method, it is even common for some C++ programmers to use the term method to refer specifically to a virtual member functions in an informal context.   
}}

=== Declaration ===
A class is ''defined'' by:
<source lang=cpp>
class myClass {
 /* public, protected and private
 variables, constants, and functions */
};  
</source>

An object of type ''myClass'' (case-sensitive) is ''declared'' using:

<source lang=cpp>
myClass Object;
</source>

*by default, all class members are initially ''private''.
*keywords ''public'' and ''protected'' allow access to class members.
*classes contain not only data members, but also functions to manipulate that data.
*a class is used as the basic building block of OOP (this is a distinction of convention, not of language-enforced semantics).

;A class can be created
* before main() is called.
* when a function is called in which the object is declared.
* when the "new" operator is used.

;Class Names
*Name the class after what it is. If you can't determine a name, then you have not designed the system well enough.
*Compound names of over three words are a clue your design may be confusing various entities in your system. Revisit your design. Try a CRC card session to see if your objects have more responsibilities than they should.
*Avoid the temptation of naming a class something similar to the class it is derived from.  A class should stand on its own.  Declaring an object with a class type doesn't depend on where that class is derived from.
*Suffixes or prefixes are sometimes helpful. For example, if your system uses agents then naming something DownloadAgent conveys real information.

;''Data Abstraction''
A fundamental concept of Object Oriented (OO) recommends an object should not expose any of its implementation details. This way, you can change the implementation without changing the code that uses the object. The class, by design, allows its programmer to hide (and also prevents changes as to) how the class is implemented.  This powerful tool allows the programmer to build in a 'preventive' measure.  Variables within the class often have a very significant role in what the class does, therefore variables can be secured within the ''private'' section of the class.

=== Access labels ===
The access labels '''Public''', '''Protected''' and '''Private''' are used within classes to set access permissions for the members in that section of the ''class''.  All class members are initially ''private'' by default.  The labels can be in any order.  These labels can be used multiple times in a class declaration for cases where it is logical to have multiple groups of these types.  An access label will remain active until another access label is used to change the permissions.

We have already mentioned that a class can have member functions "inside" it; we will see more about them later. Those member functions can access and modify all the data and member function that are inside the class. Therefore, permission labels are to restrict access to member function that reside outside the class and for other classes.

For example, a class "Bottle" could have a private variable ''fill'', indicating a liquid level 0-3 dl. ''fill'' cannot be modified directly (compiler error), but instead ''Bottle'' provides the member function sip() to reduce the liquid level by 1. Mywaterbottle could be an instance of that class, an object.

<source lang=cpp>
/* Bottle - Class and Object Example */
#include <iostream>
#include <iomanip>
 
using namespace std;
 
class Bottle
{
  private:  // variables are modified by member functions of class
  int fill; // dl of liquid
     
  public:
    Bottle()  // Default Constructor
    : fill(3) // They start with 3 dl of liquid
      {
        // More constructor code would go here if needed.
      }
     
     bool sip() // return true if liquid was available
     {

       if (fill>0)
       {
         --fill;
         return true;
       }
       else
       {
         return false;
       }

     }
 
     int level() const  // return level of liquid dl
     {
         return fill;
     }
 };  // Class declaration has a trailing semicolon
 
int main()
{
  // terosbottle object is an instance of class Bottle
  Bottle terosbottle;
  cout << "In the beginning, mybottle has "
       << terosbottle.level()
       << "  dl of liquid"
       << endl;
   
  while (terosbottle.sip())
  {
     cout << "Mybottle has "
          << terosbottle.level()
          << " dl of liquid"
          << endl;
  }
   
  return 0;
}
</source>

These keywords, ''private, public, and protected,'' affect the permissions of the members -- whether functions or variables.

==== '''public''' ====
This label indicates any members within the 'public' section can accessed freely anywhere a declared object is in scope.

{{NOTE|Avoid declaring public data members, since doing so would contribute to create unforeseen disasters.}}

==== '''private''' ====
Members defined as private are only accessible within the class defining them, or friend classes. Usually the domain of member variables and helper functions. It's often useful to begin putting functions here and then moving them to the higher access levels as needed so to reduce complexity.

{{NOTE|It's often overlooked that different instances of the same class may access each others' private or protected variables. A common case for this is in copy constructors.}}

(This is an example where the default copy constructor will do the same thing.)

<source lang=cpp>
class Foo
{
 public:
   Foo( const Foo &f )
   {
     m_value = f.m_value; // perfectly legal
   }
 
 private:
   int m_value;
};
</source>

==== '''protected''' ====
The protected label has a special meaning to inheritance, protected members are accessible in the class that defines them and in classes that inherit from that base class, or friends of it. In the section on inheritance we will see more about it.

{{NOTE|Other instances of the same class can access a protected field - provided the two classes are of the same type.  However, an instance of a child class cannot access a protected field or method of an instance of a parent class.}}

{{:C++ Programming/Classes/Inheritance}}

=== Data Members ===
'''Data members''' are used for storing information of a class, therefore they may include members of any type, even other user-defined types. Data members are declared in the same way as a global or function variable, but explicit initializers are not allowed inside the class definition, except if they are <code>const static int<code> or enumeration types, these may have an explicit initializer.

Data members are usually hidden from users of a class and are accessed through special functions.

{{TODO|Add more info|C++ Programming}}

{{:C++ Programming/Classes/this}}

{{:C++ Programming/Programming Languages/C++/Code/Keywords/static/Data Member}}

{{:C++ Programming/Classes/Member Functions}}

=== Subsumption property ===
Subsumption is a property that all objects that reside in a class hierarchy must fulfill: an object of the base class can be substituted by an object that derives from it (directly or indirectly). All mammals are animals (they derive from them), and all cats are mammals. Therefore, because of the subsumption property we can "treat" any mammal as an animal and any cat as a mammal. This implies abstraction, because when we are "treating" a mammal as an animal, the only we should know about it is that it lives, it grows, etc, but nothing related to mammals.

This property is applied in C++, whenever we are using pointers or references to objects that reside in a class hierarchy. In other words, a pointer of class animal can point to an object of class animal, mammal or cat.

Let's continue with our example:
<source lang=cpp>

 //needs to be corrected
  enum animalType {
       Herbivore,
       Carnivore,
       Omnivore,
  }
 
  class animal {
        public:
               bool isAlive;
               animalType Type;
               int numberOfChildren;
  }
 
  class mammal : public animal{
        public:
               int numberOfTeats;
  }
 
  class cat: public mammal{
        public:
              bool likesFish; //probably true
  }
 
  int main() {
      animal* a1= new animal;
      animal* a2= new mammal;
      animal* a3= new cat;
      mammal* m= new cat;
      
      a2->isAlive= True; //Correct
      a2->Type= Herbivore; //Correct
      m->numberOfTeats=2; //Correct
 
      a2->numberOfTeats=6; //Incorrect
      a3->likesFish=True; //Incorrect
      
      cat* c= (cat*)a3; //Downcast, correct (but very poor practice, see later)
      c->likesFish=False; //Correct (although it is a very awkward cat)
   }
</source>

In the last lines of the example there is cast of a pointer to animal, to a pointer to cat. This is called "Downcast". Downcasts are useful and should be used, but first we must ensure that the object we are casting is really of the type we are casting to it. Downcasting a base class to an unrelated class is an error. To resolve this issue, the casting operators {{C++ Programming/kw|dynamic_cast}}, or {{C++ Programming/kw|static_cast}}<> should be used. 
These correctly cast an object from one class to another, and will throw an exception if the class types are not related. eg. If you try:

<source lang=cpp>
cat* c = new cat;
motorbike* m;
m = dynamic_cast<motorbike*>(c);
</source>

then the app will throw an exception as a cat is not a motorbike. Static_cast is very similar, only it will perform the type checking at compile time. If you have an object where you are not sure of its type then you should use {{C++ Programming/kw|dynamic_cast}}, and be prepared to handle errors when casting. If you are downcasting objects where you know the types, then you should use {{C++ Programming/kw|static_cast}}. Do not use old-style C casts as these will simply give you an access violation if the types cast are unrelated.

=== Local Classes ===
A '''local class''' is a class defined inside a function, this is done the like any other class, but they can not however access non-static local variables or to to define static member variables. These type of classes are useful especially for use in the template functions, we will cover it in that section. 

<source lang=cpp>
void MyFunction()
{
   class LocalClass
   {
   // ... members definitions ...
   };
   
   // ... any code that needs the class ...

}
</source>

=== User defined automatic type conversion ===
We already covered [[C++ Programming/Programming Languages/C++/Code/Statements/Variables/Type Casting#Automatic type conversion|automatic type conversions]] (implicit conversion) and mentioned that some can be user-defined.

A user-defined conversion from a class to another class can be done by providing a constructor in the target class that takes the source class as an argument, <code>Target(const Source& a_Class)</code> or by providing the target class with a conversion operator, as <code>operator Source()</code>.

=== Ensuring objects of a class are never copied ===
This is required e.g. to prevent memory-related problems that would result in case the default copy-constructor or the default assignment operator is unintentionally applied to a class <tt>C</tt> which uses dynamically allocated memory, where a copy-constructor and an assignment operator are probably an overkill as they won't be used frequently.

Some style guidelines suggest making all classes non-copyable by default, and only enabling copying if it makes sense.  Other (bad) guidelines say that you should always explicitly write the copy constructor and copy assignment operators; that's actually a bad idea, as it adds to the maintenance effort, adds to the work to read a class, is more likely to introduce errors than using the implicitly declared ones, and doesn't make sense for most object types. A sensible guideline is to ''think'' about whether copying makes sense for a type; if it does, then first prefer to arrange that the compiler-generated copy operations will do the right thing (e.g., by holding all resources via resource management classes rather than via raw pointers or handles), and if that's not reasonable then obey the [[C++ Programming/Classes/Member Functions#Law of three|law of three]]. If copying doesn't make sense, you can disallow it in either of two idiomatic ways as shown below.

Just declare the copy-constructor and assignment operator, and make them <tt>private</tt>. Do not define them. As they are not <tt>protected</tt> or <tt>public</tt>, they are inaccessible outside the class. Using them within the class would give a linker error since they are not defined.

<source lang=cpp>
class C
{
  ...
 
  private:
    // Not defined anywhere
    C (const C&);
    C& operator= (const C&);
};
</source>

Remember that if the class uses dynamically allocated memory for data members, you ''must'' define the memory release procedures in destructor <tt>~C ()</tt> to release the allocated memory.

A class which only declares these two functions can be used as a private base class, so that all classes which privately inherits such a class will disallow copying.

{{NOTE|A part of the [[C++ Programming/Libraries/Boost|Boost]] library, the utility class <tt>boost:noncopyable</tt>  performs a similar function, easier to use but with added costs due to the required derivation.}}

=== Container class ===
A class that is used to hold objects in memory or external storage is often called a ''container class''. A container class acts as a generic holder and has a predefined behavior and a well-known interface. It is also a supporting class whose purpose is to hide the topology used for maintaining the list of objects in memory. When it contains a group of mixed objects, the container is called a heterogeneous container; when the container is holding a group of objects that are all the same, the container is called a homogeneous container.

=== Interface class ===
{{TODO|Complete|C++ Programming}}

=== Singleton class ===
A [[C++ Programming/Code/Design_Patterns#Singleton|Singleton]] class is a class that can only be instantiated once (similar to the use of static variables or functions). It is one of the possible implementations of a [[C++ Programming/Code/Design_Patterns#Creational_Patterns|creational pattern]], which is fully covered in the [[C++ Programming/Code/Design_Patterns|Design Patterns Section]] of the book. 

[[Category:C++ Programming|{{SUBPAGENAME}}]]
