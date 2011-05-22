>== Smart Pointers ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== Smart Pointers ==
Using raw pointers to store allocated data and then cleaning them up in the destructor can generally be considered a very bad idea since it is error-prone.  Even temporarily storing allocated data in a raw pointer and then deleting it when done with it should be avoided for this reason.  For example, if your code throws an exception, it can be cumbersome to properly catch the exception and delete all allocated objects.  

Smart pointers can alleviate this headache by using the compiler and language semantics to ensure the pointer content is automatically released when the pointer itself goes out of scope.

<source lang="cpp">
#include <memory>
class A
{
public:
        virtual ~A() {}
	virtual char val() = 0;
};

class B : public A
{
public:
	virtual char val() { return 'B'; }
};

A* get_a_new_b()
{
	return new B();
}

bool some_func()
{
	bool rval = true;
	std::auto_ptr<A> a( get_a_new_b() );
	try {
		std::cout << a->val();
	} catch(...) {
		if( !a.get() ) {
			throw "Memory allocation failure!";
		}
		rval = false;
	}
	return rval;
}
</source>

{{TODO|Could note that the '''rebind''' pattern used by allocators is an alternative to using a template template parameter.  Historically the STL was largely developed before C++ compilers offered support for template template parameters.  Interestingly, modern template metaprogramming style has promoted a rebind-like approach instead of using template template parameters.|C++ Programming}}

==Semantics==
<!-- Section content copied from http://en.wikipedia.org/w/index.php?title=Auto_ptr&oldid=267300087 --> 
The auto_ptr has semantics of strict ownership, meaning that the auto_ptr instance is the sole entity responsible for the object's lifetime. If an auto_ptr is copied, the source loses the reference. For example:
<source lang="cpp">
#include <iostream>
#include <memory>
using namespace std;
 
int main(int argc, char **arv)
{
    int *i = new int;
    auto_ptr<int> x(i);
    auto_ptr<int> y;
    
    y = x;
    
    cout << x.get() << endl;
    cout << y.get() << endl;
}
</source>

This code will print a NULL address for the first auto_ptr object and some non-NULL address for the second, showing that the source object lost the reference during the assignment (''=''). The raw pointer <code>i</code> in the example should not be deleted, as it will be deleted by the auto_ptr that owns the reference.  In fact, <code>new int</code> could be passed directly into x, eliminating the need for <code>i</code>.

Notice that the object pointed by an auto_ptr is destructed using <code>operator delete</code>; this means that you should only use auto_ptr for pointers obtained with <code>operator new</code>. This excludes pointers returned by <code>malloc(), calloc() or realloc()</code> and <code>operator new[]</code>.

[[Category:C++ Programming|{{SUBPAGENAME}}]]
