>== Boost Library ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== Boost Library ==
The [[w:Boost (programming)|'''Boost library''']] (http://www.boost.org/) provides free [[w:peer-review|peer-reviewed]] , [[w:open source|open source]] [[w:Library (computer science)|libraries]] that extend the functionality of C++. Most of the libraries are licensed under the [http://www.boost.org/more/license_info.html Boost Software License], designed to allow Boost to be used with both open and [[w:closed source|closed source]] projects.

Many of Boost's founders are on the [[w:ISO/IEC 14882|C++ standard]] committee and several Boost libraries have been accepted for incorporation into the [[w:Technical Report 1|Technical Report 1]] of [[w:C++0x|C++0x]]. Although Boost was begun by members of the C++ Standards Committee Library Working Group, participation has expanded to include thousands of programmers from the C++ community at large.

The emphasis is on libraries which work well with the C++ Standard Library. The libraries are aimed at a wide range of C++ users and application domains, and are in regular use by thousands of programmers. They range from general-purpose libraries like [http://www.boost.org/libs/smart_ptr SmartPtr], to OS Abstractions like [http://www.boost.org/libs/filesystem FileSystem], to libraries primarily aimed at other library developers and advanced C++ users, like [http://www.boost.org/libs/mpl MPL].

A further goal is to establish "existing practice" and provide reference implementations so that Boost libraries are suitable for eventual standardization. Ten Boost libraries will be included in the  [http://www.open-std.org/jtc1/sc22/wg21/ C++ Standards Committee's] upcoming  [http://std.dkuug.dk/jtc1/sc22/wg21/docs/library_technical_report.html C++ Standard Library Technical Report] as a step toward becoming part of a future C++ Standard.

In order to ensure efficiency and flexibility, Boost makes extensive use of [[w:template (programming)|template]]s.  Boost has been a source of extensive work and research into [[w:generic programming|generic programming]] and [[w:metaprogramming|metaprogramming]] in C++.

=== extension libraries ===

* Algorithms
* Concurrent programming ([[w:thread (computer science)|threads]])
* [[w:Data structure|Containers]]
**[http://boost.org/doc/html/array.html array] - Management of fixed-size arrays with STL container semantics
**[http://boost.org/libs/graph/doc/table_of_contents.html Boost Graph Library (BGL)] - Generic graph containers, components and algorithms
**[http://boost.org/libs/multi_array/doc/index.html multi-array] - Simplifies creation of N-dimensional arrays
**[http://boost.org/libs/multi_index/doc/index.html multi-index containers] - Containers with built in indexes that allow different sorting and access semantics
**[http://boost.org/libs/ptr_container/doc/ptr_container.html pointer containers] - Containers modeled after most standard STL containers that allow for transparent management of pointers to values
**[http://boost.org/libs/property_map/property_map.html property map] - Interface specifications in the form of concepts and a general purpose interface for mapping key values to objects
**[http://boost.org/doc/html/variant.html variant] - A safe and generic stack-based object container that allows for the efficient storage of and access to an object of a type that can be chosen from among a set of types that must be specified at compile time.
* Correctness and [[w:Software testing|testing]]
**[http://boost.org/libs/concept_check/concept_check.htm concept check] - Allows for the enforcement of actual template parameter requirements (concepts)
**[http://boost.org/doc/html/boost_staticassert.html static assert] - Compile time assertion support
**[http://boost.org/libs/test/doc/index.html Boost Test Library] - A matched set of components for writing test programs, organizing tests into test cases and test suites, and controlling their runtime execution
* Data structures
**[http://boost.org/libs/dynamic_bitset/ dynamic_bitset] - Dynamic <code>std::bitset-</code>like data structure
* Function objects and [[w:higher-order programming|higher-order programming]]
**[http://boost.org/libs/bind/bind.html bind] and [http://www.boost.org/libs/bind/mem_fn.html mem_fn] - General binders for functions, function objects, function pointers and member functions
**[http://boost.org/doc/html/function.html function] - Function object wrappers for deferred calls. Also, provides a generalized mechanism for callbacks
**[http://boost.org/libs/functional/index.html functional] - Enhancements to the function object adapters specified in the C++ Standard Library, including:
***[http://boost.org/libs/functional/function_traits.html function object traits]
***[http://boost.org/libs/functional/negators.html negators]
***[http://boost.org/libs/functional/binders.html binders]
***[http://boost.org/libs/functional/ptr_fun.html adapters for pointers to functions]
***[http://boost.org/libs/functional/mem_fun.html adapters for pointers to member functions]
**[http://boost.org/doc/html/hash.html hash] - An implementation of the hash function object specified by the C++ Technical Report 1 (TR1). Can be used as the default hash function for unordered associative containers
**[http://boost.org/doc/html/lambda.html lambda] - In the spirit of [[w:Lambda calculus|lambda abstractions]], allows for the definition of small anonymous function objects and operations on those objects at a call site, using placeholders, especially for use with deferred callbacks from algorithms. 
**[http://boost.org/doc/html/ref.html ref] - Provides utility class templates for enhancing the capabilities of standard C++ references, especially for use with generic functions
**[http://boost.org/libs/utility/utility.htm#result_of result_of] - Helps in the determination of the type of a call expression
**[http://boost.org/doc/html/signals.html signals and slots] - Managed signals and slots callback implementation
* [[w:Generic programming|Generic programming]]
* [[w:Graph (data structure)|Graphs]]
* Input/output
* Interlanguage support (for [[w:Python (programming language)|Python]])
* [[w:Iterator#C.2B.2B|Iterators]]
**[http://boost.org/libs/iterator/doc/index.html iterators]
**[http://boost.org/libs/utility/operators.htm operators] - Class templates that help with overloaded operator definitions for user defined iterators and classes that can participate in arithmetic computation.
**[http://boost.org/libs/tokenizer/index.html tokenizer] - Provides a view of a set of tokens contained in a sequence that makes them appear as a container with iterator access
* Math and Numerics
* [[w:Main memory|Memory]]
**[http://boost.org/libs/pool/doc/index.html pool] - Provides a simple segregated storage based memory management scheme
**[http://boost.org/libs/smart_ptr/smart_ptr.htm smart_ptr] - A collection of smart pointer class templates with different pointee management semantics
***[http://boost.org/libs/smart_ptr/scoped_ptr.htm scoped_ptr] - Owns the pointee (single object)
***[http://boost.org/libs/smart_ptr/scoped_array.htm scoped_array] - Like scoped_ptr, but for arrays
***[http://boost.org/libs/smart_ptr/shared_ptr.htm shared_ptr] - Potentially shares the pointer with other shared_ptrs. Pointee is destroyed when last shared_ptr to it is destroyed
***[http://boost.org/libs/smart_ptr/shared_array.htm shared_array] - Like shared_ptr, but for arrays
***[http://boost.org/libs/smart_ptr/weak_ptr.htm weak_ptr] - Provides a "weak" reference to an object that is already managed by a shared_ptr
***[http://boost.org/libs/smart_ptr/intrusive_ptr.html intrusive_ptr] - Similared to shared_ptr, but uses a reference count provided by the pointee
**[http://boost.org/libs/utility/utility.htm utility] - Miscellaneous support classes, including:
***[http://boost.org/libs/utility/base_from_member.html base from member idiom] - Provides a workaround for a class that needs to initialize a member of a base class inside its own (i.e., the derived class') constructor's initializer list
***[http://boost.org/libs/utility/checked_delete.html checked delete] - Check if an attempt is made to destroy an object or array of objects using a pointer to an incomplete type
***[http://boost.org/libs/utility/utility.htm#functions_next_prior next and prior functions] - Allow for easier motion of a forward or bidirectional iterator, especially when the results of such a motion need to be stored in a separate iterator (i.e., should not change the original iterator)
***[http://boost.org/libs/utility/utility.htm#Class_noncopyable noncopyable] - Allows for the prohibition of copy construction and copy assignment
***[http://boost.org/libs/utility/utility.htm#addressof addressof] - Allows for the acquisition of an object's real address, bypassing any overloads of operator&(), in the process
***[http://boost.org/libs/utility/utility.htm#result_of result_of] - Helps in the determination of the type of a call expression
* Miscellaneous
* [[w:Spirit parser framework|Parsers]]
* Preprocessor metaprogramming
* [[w:String (computer science)|String]] and text processing
**[http://boost.org/libs/conversion/lexical_cast.htm lexical_cast] - Type conversions to/from text
**[http://boost.org/libs/format/index.html format] - Type safe argument formatting according to a format string
**[http://boost.org/libs/iostreams/doc/index.html iostreams] - C++ streams and stream buffer assistance for new sources/sinks, filters framework
**[http://www.boost.org/doc/libs/1_44_0/libs/regex/doc/html/index.html regex] - Support for regular expressions
**[[w:Spirit Parser Framework|Spirit]] - An object-oriented recursive-descent parser generator framework
**[http://boost.org/doc/html/string_algo.html string algorithms] - A collection of various algorithms related to strings
**[http://boost.org/libs/tokenizer/index.html tokenizer] - Allows for the partitioning of a string or other character sequence into [[w:token (parser)|token]]s
**[http://boost.org/libs/wave/index.html wave] - Standards conformant implementation of the mandated [[w:C99|C99]] / C++ pre-processor functionality packed behind an easy to use interface
* [[w:Template metaprogramming|Template metaprogramming]]
**[http://boost.org/libs/mpl/doc/index.html mpl] - A general purpose high-level metaprogramming framework of compile-time algorithms, sequences and metafunctions
**[http://boost.org/doc/html/boost_staticassert.html static assert] - Compile time assertion support
**[http://boost.org/doc/html/boost_typetraits.html type traits] - Templates that define the fundamental properties of types
* Workarounds for broken compilers

The current Boost release contains 87 individual libraries, including the following three:

=== noncopyable ===
The <tt>boost::noncopyable</tt> utility class that [[C++ Programming/Classes#Ensuring_objects_of_a_class_are_never_copied|ensures that objects of a class are never copied]].

<source lang=cpp>
class C : boost::noncopyable
{
  ...
};
</source>

=== Linear algebra – uBLAS ===
Boost includes the '''uBLAS''' [[w:linear algebra|linear algebra]] library, with [[w:Basic Linear Algebra Subprograms|BLAS]] support for vectors and matrices. uBlas supports a wide range of linear algebra operations, and has bindings to some widely used numerics libraries, such as ATLAS, BLAS and LAPACK.

* Example showing how to multiply a vector with a matrix:
<source lang="cpp">#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/io.hpp>
#include <iostream>

using namespace boost::numeric::ublas;

/* "y = Ax" example */
int main () 
{
      vector<double> x(2);
      x(0) = 1; x(1) = 2;
 
      matrix<double> A(2,2);
      A(0,0) = 0; A(0,1) = 1;
      A(1,0) = 2; A(1,1) = 3;

      vector<double> y = prod(A, x);

      std::cout << y << std::endl;
      return 0;
}
</source>

=== Generating random numbers – Boost.Random ===
Boost provides distribution-independent [[w:pseudorandom number generator|pseudorandom number generator]]s and PRNG-independent probability distributions, which are combined to build a concrete generator.
* Example showing how to sample from a [[w:normal distribution|normal distribution]] using the [[w:Mersenne Twister|Mersenne Twister]] generator:
<source lang="cpp">
#include <boost/random.hpp>
#include <ctime>

using namespace boost;

double SampleNormal (double mean, double sigma)
{
    // Create a Mersenne twister random number generator
    // that is seeded once with #seconds since 1970
    static mt19937 rng(static_cast<unsigned> (std::time(0)));

    // select Gaussian probability distribution
    normal_distribution<double> norm_dist(mean, sigma);

    // bind random number generator to distribution, forming a function
    variate_generator<mt19937&, normal_distribution<double> >  normal_sampler(rng, norm_dist);

    // sample from the distribution
    return normal_sampler();
}
</source>
See [http://boost.org/libs/random/ Boost Random Number Library] for more details.

=== Multi-threading – Boost.Thread ===
Example code that demonstrates creation of threads:
<source lang="cpp">
#include <boost/thread/thread.hpp>
#include <iostream>

using namespace std; 

void hello_world() 
{
  cout << "Hello world, I'm a thread!" << endl;
}

int main(int argc, char* argv[]) 
{
  // start two new threads that calls the "hello_world" function
  boost::thread my_thread1(&hello_world);
  boost::thread my_thread2(&hello_world);

  // wait for both threads to finish
  my_thread1.join();
  my_thread2.join();
  
  return 0;
}
</source>

See also [http://antonym.org/2009/05/threading-with-boost---part-i-creating-threads.html Threading with Boost - Part I: Creating Threads]

=== Thread locking ===
Example usage of a mutex to enforce exclusive access to a function:
<source lang="cpp">
#include <iostream>
#include <boost/thread.hpp>

void locked_function ()
{
    // function access mutex
    static boost::mutex m;
    // wait for mutex lock
    boost::mutex::scoped_lock lock(m);

    // critical section
    // TODO: Do something

    // auto-unlock on return
}

int main (int argc, char* argv[]) 
{
    locked_function();
    return 0;
}
</source>

Example of read/write locking of a property:
<source lang="cpp">
#include <iostream>
#include <boost/thread.hpp>

/** General class for thread-safe properties of any type. */
template <class T>
class lock_prop : boost::noncopyable {
public:
    lock_prop () {}

    /** Set property value. */
    void operator = (const T & v) {
        // wait for exclusive write access
        boost::unique_lock<boost::shared_mutex> lock(mutex);

        value = v;
    }

    /** Get property value. */
    T operator () () const {
        // wait for shared read access
        boost::shared_lock<boost::shared_mutex> lock(mutex);

        return value;
    }

private:
    /// Property value.
    T                           value;
    /// Mutex to restrict access
    mutable boost::shared_mutex mutex;
};

int main () {
    // read/write locking property
    lock_prop<int> p1;
    p1 = 10;
    int a = p1();

    return 0;
}
</source>


* [http://www.ddj.com/dept/cpp/184401518 Introduction to Boost.Threads] in [[w:Dr. Dobb's Journal|Dr. Dobb's Journal]]. (2002)
* [http://www.ddj.com/cpp/211600441 What's New in Boost Threads?] in [[w:Dr. Dobb's Journal|Dr. Dobb's Journal]]. (2008)
* Boost.Threads [http://www.boost.org/doc/html/thread.html API reference].
* [http://threadpool.sourceforge.net threadpool library] based on Boost.Thread

[[Category:C++ Programming|{{SUBPAGENAME}}]]
