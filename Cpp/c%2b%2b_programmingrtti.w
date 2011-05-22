>==Run-Time Type Information (RTTI)==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">==Run-Time Type Information (RTTI)==
RTTI refers to the ability of the system to report on the dynamic type of an object and to provide information about that type at runtime (as opposed to at compile time), when utilized consistently can be a powerful tool to ease the work of the programmer in managing resources.

=== {{C++ Programming/kw|dynamic_cast}} ===
Consider what you have already learned about the {{C++ Programming/kw|dynamic_cast}} keyword and let's say that we have the following class hierarchy:

<source lang="cpp">
class Interface
{
public:
        virtual void GenericOp() = 0;// pure virtual function
};

class SpecificClass : public Interface
{
public:
        virtual void GenericOp();
        virtual void SpecificOp();
};
</source>

Let's say that we also have a pointer of type <code>Interface*</code>, like so:

<source lang="cpp">
Interface* ptr_interface;
</source>

Supposing that a situation emerges that we are forced to presume but have no guarantee that the pointer points to an object of type <code>SpecificClass</code> and we would like to call the member <code>SpecificOp()</code> of that class. To dynamically convert to a derived type we can use {{C++ Programming/kw|dynamic_cast}}, like so:

<source lang="cpp">
SpecificClass* ptr_specific = dynamic_cast<SpecificClass*>(ptr_interface);
if( ptr_specific ){
    // our suspicions are confirmed -- it really was a SpecificClass
    ptr_specific->SpecificOp();
}else{
    // our suspicions were incorrect -- it is definitely not a SpecificClass.
    // The ptr_interface points to an instance of some other child class of the base InterfaceClass.
};
ptr_interface->GenericOp();
</source>

With {{C++ Programming/kw|dynamic_cast}}, the program converts the base class pointer to a derived class pointer and allows the derived class members to be called. Be very careful, however: if the pointer that you are trying to cast is not of the correct type, then {{C++ Programming/kw|dynamic_cast}} will return a null pointer.

We can also use {{C++ Programming/kw|dynamic_cast}} with references.

<source lang="cpp">
SpecificClass& ref_specific = dynamic_cast<SpecificClass&>(ref_interface);
</source>

This works almost in the same way as pointers. However, if the real type of the object being cast is not correct then {{C++ Programming/kw|dynamic_cast}} will not return null (there's no such thing as a null reference). Instead, it will throw a <code>std::bad_cast</code> exception.

=== {{C++ Programming/kw|typeid}} ===
{{:C++ Programming/Programming Languages/C++/Code/Keywords/typeid}}

In RTTI it is used in this setup:

<source lang="cpp">
const std::type_info& info = typeid(object_expression);
</source>

Sometimes we need to know the exact type of an object. The <code>typeid</code> operator returns a reference to a standard class <code>std::type_info</code> that contains information about the type. This class provides some useful members including the <code>==</code> and <code>!=</code> operators. The most interesting method is probably:

<source lang="cpp">
const char* std::type_info::name() const;
</source>

This member function returns a pointer to a C-style string with the name of the object type. For example, using the classes from our earlier example:

<source lang="cpp">
const std::type_info &info = typeid(*ptr_interface);
std::cout << info.name() << std::endl;
</source>

This program would print something like<ref>
(The exact string returned by std::type_info::name() is compiler-dependent).
</ref>
<code>SpecificClass</code> because that is the dynamic type of the pointer <code>ptr_interface</code>.

<code>typeid</code> is actually an operator rather than a function, as it can also act on types:

<source lang="cpp">
const std::type_info& info = typeid(type);
</source>

for example (and somewhat circularly)

<source lang="cpp">
const std::type_info& info = typeid(std::type_info);
</source>

will give a <code>type_info</code> object which describes <code>type_info</code> objects.  This latter use is not RTTI, but rather CTTI (compile-time type identification).

===Limitations===
There are some limitations to RTTI. First, RTTI can only be used with ''polymorphic types''. That means that your classes must have at least one virtual function, either directly or through inheritance. Second, because of the additional information required to store types some compilers require a special switch to enable RTTI.

Note that references to pointers will not work under RTTI:

<source lang="cpp">
void example( int*& refptrTest )
{
        std::cout << "What type is *&refptrTest : " << typeid( refptrTest ).name() << std::endl;
}
</source>

Will report <code>int*</code>, as <code>typeid()</code> does not support reference types.

===Misuses of RTTI===
RTTI should only be used sparingly in C++ programs. There are several reasons for this. Most importantly, other language mechanisms such as polymorphism and templates are almost always superior to RTTI. As with everything, there are exceptions, but the usual rule concerning RTTI is more or less the same as with <code>goto</code> statements. Do not use it as a shortcut around proper, more robust design.  Only use RTTI if you have a very good reason to do so and only use it if you know what you are doing.

----
{{reflist}}

[[Category:C++ Programming|{{SUBPAGENAME}}]]
