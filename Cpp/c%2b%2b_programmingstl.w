>==Standard Template Library (STL)==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">==Standard Template Library (STL)==
The '''Standard Template Library''' ('''STL''') offers collections of algorithms, containers, iterators, and other fundamental components, implemented as templates, classes, and functions essential to extend functionality and standardization to C++. STL main focus is to provide improvements implementation standardization  with  emphasis in performance and correctness.

Instead of wondering if your array would ever need to hold 257 records or having nightmares of string buffer overflows, you can enjoy '''vector''' and '''string''' that automatically extend to contain more records or characters. For example, vector is just like an array, except that vector's size can expand to hold more cells or shrink when fewer will suffice. One must keep in mind that the STL does not conflict with OOP but in itself is not object oriented; In particular it makes no use of runtime polymorphism (i.e., has no virtual functions).

The true power of the STL lies not in its [[#Containers|container]] classes, but in the fact that it is a framework, combining algorithms with data structures using indirection through iterators to allow generic implementations of higher order algorithms to work efficiently on varied forms of data. To give a simple example, the same '''std::copy''' function can be used to copy elements from one array to another, or to copy the bytes of a file, or to copy the whitespace-separated words in "text like this" into a container such as '''std::vector<std::string>'''.

<source lang="cpp">
 // std::copy from array a to array b
 int a[10] = { 3,1,4,1,5,9,2,6,5,4 };
 int b[10];
 std::copy(&a[0], &a[10], b);

 // std::copy from input stream a to an arbitrary OutputIterator
 template <typename OutputIterator>
 void f(std::istream &a, OutputIterator destination) {
   std::copy(std::istreambuf_iterator<char>(a),
             std::istreambuf_iterator<char>(),
             destination);
 }

 // std::copy from a buffer containing text, inserting items in
 // order at the back of the container called words.
 std::istringstream buffer("text like this");
 std::vector<std::string> words;
 std::copy(std::istream_iterator<std::string>(buffer),
           std::istream_iterator<std::string>(),
           std::back_inserter(words));
 assert(words[0] == "text");
 assert(words[1] == "like");
 assert(words[2] == "this");
</source>

=== History ===
[[Image:Alexander Stepanov.jpg|120px|right|Alexander Stepanov]]
The C++ Standard Library incorporated part of the STL (published as a software library by [[w:Silicon Graphics|SGI]]/Hewlett-Packard Company). The primary implementer of the C++ Standard Template Library was [[w:Alexander Stepanov|Alexander Stepanov]].

Today we call STL to what was adopted into the C++ Standard. The ISO C++ does not specify header content, and allows implementation of the STL either in the headers, or in a true library.

{{NOTE|In an interview Alexander Stepanov, stated that he originally, wanted all auxiliary functions in STL to be visible but it was not politically possible, especially the heap functions. That Bjarne did reduce the number of components in STL by a factor of two as to permit the adoption into the standard.}}

Compilers will already have one implementation included as part of the C++ Standard (i.e., MS Visual Studio uses the Dinkum STL). All implementations will have to comply to the standard's requirements regarding functionality and behavior, but consistency of programs across all major hardware implementations, operating systems, and compilers will also depends on the portability of the STL implementation. They may also offer extended features or be optimized to distinct setups. 

;List of STL implementations.   
* libstdc++ from gnu (was part of libg++)
* SGI STL library (http://www.sgi.com/tech/stl/) free STL implementation.
* Rogue Wave standard library (HP, SGI, SunSoft, Siemens-Nixdorf) / [[w:Apache C++ Standard Library|Apache C++ Standard Library (STDCXX)]]
* Dinkum STL library by P.J. Plauger (http://www.dinkumware.com/) commercial STL implementation widely used, since it was licensed in is co-maintained by Microsoft and it is the STL implementation that ships with Visual Studio.

There are many different implementations of the STL, all based on the language standard but  nevertheless differing from each other, making it transparent for the programmer, enabling specialization and rapid evolution of the code base.  

;Open Source versions of the STL are available <small>(can be useful to consult)</small>:
* Apache C++ Standard Library (STDCXX) ( http://stdcxx.apache.org/ ).
* STLport STL library (http://www.stlport.com/) free and highly cross-platform implementation based on the SGI implementation.

{{NOTE|There are advantages on having compartmentalized functionalities, some developers active avoid using some of the language features, for a multitude of reasons. C++ permits the programmer to chose how to express himself, have control over the development paradigms and not be constricted by an higher level of abstraction.}}

===Containers===
The containers we will discuss in this section of the book are part of the standard namespace (std::).  They all originated in the original SGI implementation of the STL.
 
{{NOTE|note=When choosing a container, you should have in mind what makes them different, this will help you produce more efficient code. See also the [[C++ Programming/Optimization|Optimization Section]] of the book, about [[C++ Programming/Optimization#The right data in the right container|using the right data in the right container]]. }}

====Sequence Containers====
; Sequences - easier than arrays :
Sequences are similar to C arrays, but they are easier to use. Vector is usually the first sequence to be learned. Other sequences, list and double-ended queues, are similar to vector but more efficient in some special cases.  (Their behavior is also different in important ways concerning validity of iterators when the container is changed; iterator validity is an important, though somewhat advanced, concept when using containers in C++.)

* vector - "an easy-to-use array"
* list - in effect, a doubly-linked list
* deque - double-ended queue (properly pronounced "deck", often mispronounced as "dee-queue")

=====vector=====
The '''vector''' is a template class in itself, it is a ''Sequence Container'' and allows you to easily create a [[w:dynamic array|dynamic array]] of elements (one type per instance) of almost any data-type or object within a programs when using it. The '''vector''' class handles most of the memory management for you.

Since a '''vector''' contain contiguous elements it is an ideal choice to replace the old C style array, in a situation where you need to store data, and ideal in a situation where you need to store dynamic data as an array that changes in size during the program's execution (old C style arrays can't do it). However, vectors do incur a very small overhead compared to static arrays (depending on the quality of your compiler), and cannot be initialized through an initialization list.

{{NOTE|
Vector is known to be slow when using the MSVC compiler due to the SECURE_SCL flag, that, by default, forces bounds checking even in optimized builds.}}

Accessing members of a vector or appending elements takes a fixed amount of time, no matter how large the vector is, whereas locating a specific value in a vector element or inserting elements into the vector takes an amount of time directly proportional to its location in it (size dependent).

{{NOTE|
If you create a vector you can access its data using consecutive pointers:

<source lang="cpp">
  std::vector<type> myvector(8);
  type * ptr = &myvector[0];
  ptr[0], ptr[7]; // access the first and last objects in myvector
</source>

this information is present in INCITS/ISO/IEC 14882-2003 but was not properly documented in the 1998 version of the C++ standard.<br>
Be aware that ptr[i] is faster than myvector.at(i) because no error checking is performed. Watch out for how long that pointer is valid. The contiguous nature of vectors is most often important when interfacing to C code.

You should also keep in mind that std::vector<T>::iterator may not be a pointer; using an iterator is the safest mode to access a container but safety has always a cost in performance.
}}

;'''Example'''
{{TODO|Should this be split into 2 examples, a "old C style array" example and a "new C++ STL vector" example?|C++ Programming}}

<source lang=cpp>
/*
David Cary 2009-03-04
quick demo for wikibooks
*/

#include <iostream>
#include <vector>
using namespace std;

vector<int> pick_vector_with_biggest_fifth_element(vector<int> left,vector<int> right)
{
    if(left[5] < right[5])
    {
        return( right );
    }
    // else
    return left ;
}

int* pick_array_with_biggest_fifth_element(int * left,int * right)
{
    if(left[5] < right[5])
    {
        return( right );
    }
    // else 
    return left ;
}

int vector_demo(void)
{
    cout << "vector demo" << endl;
    vector<int> left(7);
    vector<int> right(7);

    left[5] = 7;
    right[5] = 8;
    cout << left[5] << endl;
    cout << right[5] << endl;
    vector<int> biggest(pick_vector_with_biggest_fifth_element( left, right ) );
    cout << biggest[5] << endl;

    return 0;
}

int array_demo(void)
{
    cout << "array demo" << endl;
    int left[7];
    int right[7];

    left[5] = 7;
    right[5] = 8;
    cout << left[5] << endl;
    cout << right[5] << endl;
    int * biggest =
        pick_array_with_biggest_fifth_element( left, right );
    cout << biggest[5] << endl;

    return 0;
}

int main(void)
{
    vector_demo();
    array_demo();
}
</source>

;Member Functions
The <code>vector</code> class models the [http://www.sgi.com/tech/stl/Container.html Container] [[w:concept (generic programming)|concept]], which means it has <code>begin()</code>, <code>end()</code>, <code>size()</code>, <code>max_size()</code>, <code>empty()</code>, and <code>swap()</code> methods.

{{NOTE|Since most vector (or deque) implementations typically reserves some extra internal storage for future growth. Prefer the <code>swap()</code> method when altering a standard vector size (or freeing the memory used) when memory resources becomes a factor.}}

* informative
**<code>vector::front</code> - Returns reference to first element of vector.
**<code>vector::back</code> - Returns reference to last element of vector.
**<code>vector::size</code> - Returns number of elements in the vector.
**<code>vector::empty</code> - Returns true if vector has no elements.
*standard operations
**<code>vector::insert</code> - Inserts elements into a vector (single & range), shifts later elements up. Inefficient.
**<code>vector::push_back</code> - Appends (inserts) an element to the end of a vector, allocating memory for it if necessary. [[w:Amortized analysis|Amortized]] O(1) time.
**<code>vector::erase</code> - Deletes elements from a vector (single & range), shifts later elements down. Inefficient.
**<code>vector::pop_back</code> -  Erases the last element of the vector, (possibly reducing capacity - usually it isn't reduced, but this depends on particular STL implementation). [[w:Amortized analysis|Amortized]] O(1) time.
**<code>vector::clear</code> - Erases all of the elements.  Note however that if the data elements are pointers to memory that was created dynamically (e.g., the '''new''' operator was used), the memory will not be freed.
*allocation/size modification
**<code>vector::assign</code> - Used to delete a ''origin'' '''vector''' and copies the specified elements to an empty ''target'' '''vector'''.
**<code>vector::reserve</code> - Changes capacity (allocates more memory) of vector, if needed. In many STL implementations capacity can only grow, and is never reduced.
**<code>vector::capacity</code> - Returns current capacity (allocated memory) of vector.
**<code>vector::resize</code> - Changes the vector size.
* iteration
**<code>vector::begin</code> - Returns an iterator to start traversal of the vector.
**<code>vector::end</code> - Returns an iterator that points just beyond the end of the vector.
**<code>vector::at</code> - Returns a reference to the data element at the specified location in the '''vector''', with bounds checking.

{{NOTE|
It is important to remember the distinctions of capacity(), size() and empty() when dealing with containers.
}}

<source lang="cpp">
vector<int> v;
for (vector<int>::iterator it = v.begin(); it!=v.end(); ++it/* increment operand is used to move to next element*/) {
    cout << *it << endl;
}
</source>

======vector::Iterators======
std::vector<T> provides Random Access Iterators; as with all containers, the primary access to iterators is via begin() and end() member functions.  These are overloaded for const- and non-const containers, returning iterators of types std::vector<T>::const_iterator and std::vector<T>::iterator respectively.

{{TODO|Add missing data|C++ Programming}}

======vector examples======
<source lang="cpp">
 /* Vector sort example */
 #include <iostream>
 #include <vector>
 
 int main()
 {
         using namespace std;
  
         cout << "Sorting STL vector, \"the easier array\"... " << endl;
         cout << "Enter numbers, one per line.  Press ctrl-D to quit." << endl;
 
         vector<int> vec; 
         int tmp;
         while (cin>>tmp) {
                 vec.push_back(tmp);
         }
 
         cout << "Sorted: " << endl;
         sort(vec.begin(), vec.end());   
         int i = 0;
         for (i=0; i<vec.size(); i++) {
                 cout << vec[i] << endl;;
         }
 
         return 0;
 }
</source>

The call to <tt>sort</tt> above actually calls an instantiation of the function template <tt>std::sort</tt>, which will work on any half-open range specified by two random access iterators.

If you like to make the code above more "STLish" you can write this program in the following way:

<source lang="cpp">
 #include <iostream>
 #include <vector>
 #include <algorithm>
 #include <iterator>
 
 int main()
 {
        using namespace std;
 
        cout << "Sorting STL vector, \"the easier array\"... " << endl;
        cout << "Enter numbers, one per line.  Press ctrl-D to quit." << endl;
 
        vector<int> vec(istream_iterator<int>(cin), istream_iterator<int>());
 
        sort(vec.begin(), vec.end());
 
        cout << "Sorted: " << endl;
 
        copy(vec.begin(), vec.end(), ostream_iterator<int>(cout, "\n"));
 
        return 0;
 }
</source>

{{TODO|Rewriting to use the range constructor of <tt>vector<int></tt> rather than using the copy algorithm introduces C++'s ''[[w:most vexing parse|most vexing parse]]''.|C++ Programming}}

=====Linked lists=====
The STL provides a class template called '''list''' (part of the standard namespace (std::)) which implements a non-intrusive doubly-[[w:linked list|linked list]].  Linked lists can insert or remove elements in the middle in constant time, but do not have random access.  One useful feature of '''std::list''' is that references, pointers and iterators to items inserted into a list remain valid so long as that item remains in the list.

{{NOTE|Consider using vector instead of list for better cache coherency and avoid "death by swapping", see the [[C++ Programming/Optimization|Optimization Section]], about using the [[C++ Programming/Optimization#The right data in the right container|right data in the right container]].
}}

======list examples======
{{TODO|Add missing data|C++ Programming}}

====Associative Containers (key and value)====
This type of container point to each element in the container with a key value, thus simplifying searching containers for the programmer. Instead of iterating through an array or vector element by element to find a specific one, you can simply ask for people["tero"]. Just like vectors and other containers, associative containers can expand to hold any number of elements.

=====Maps and Multimaps=====
''map'' and ''multimap'' are associative containers that manage key/value pairs as elements as seen above.  The elements of each container will sort automatically using the actual key for sorting criterion.  The difference between the two is that maps do not allow duplicates, whereas, multimaps does. 

* map - unique keys
* multimap - same key can be used many times
* set - unique key is the value
* multiset - key is the value, same key can be used many times

<source lang="cpp">
  /* Map example - character distribution  */
  #include <iostream>
  #include <map>
  #include <string>
  #include <cctype>
 
  using namespace std;
 
  int main()
  {
          /* Character counts are stored in a map, so that 
           * character is the key.
           * Count of char a is chars['a']. */
          map<char, long> chars;
 
          cout << "chardist - Count character distributions" << endl;
          cout << "Type some text. Press ctrl-D to quit." << endl;
          char c;
          while (cin.get(c)) {
                  // Upper A and lower a are considered the same 
                  c=tolower(static_cast<unsigned char>(c));
                  chars[c]=chars[c]+1; // Could be written as ++chars[c];
          }
 
          cout << "Character distribution: " << endl;
            
          string alphabet("abcdefghijklmnopqrstuvwxyz");
          for (string::iterator letter_index=alphabet.begin(); letter_index != alphabet.end(); letter_index++) {
                  if (chars[*letter_index] != 0) {
                          cout << char(toupper(*letter_index))
                               << ":" << chars[*letter_index]
                               << "\t" << endl; 
                  }
          }
          return 0;
  }
</source>

==== Container Adapters ====
* stack - last in, first out (LIFO)
* queue - first in, first out (FIFO)
* priority queue

===Iterators===
C++'s iterators are one of the foundation of the STL. Iterators exist in languages other than C++, but C++ uses an unusual form of iterators, with pros and cons.

In C++, an iterator is a ''concept'' rather than a specific type, they are a generalization of the pointers as an abstraction for the use of containers. Iterators are further divided based on properties such as traversal properties.

The basic idea of an iterator is to provide a way to navigate over some collection of objects concept.

Some (overlapping) categories of iterators are:
* Singular iterators
* Invalid iterators
* Random access iterators
* Bidirectional iterators
* Forward iterators
* Input iterators
* Output iterators
* Mutable iterators

A pair of iterators '''[begin, end)''' is used to define a [[Algebra/Interval Notation|half open range]], which includes the element identified from '''begin''' to '''end''', except for the element identified by '''end'''.  As a special case, the half open range '''[x, x)''' is empty, for any valid iterator x.

{{NOTE|The range notation may vary, the meaning is to express the inclusion or exclusion of the range limits. An also common notation is [begin, end[ (meaning begin is part of the range and end is not).}}

The most primitive examples of iterators in C++ (and likely the inspiration for their syntax) are the built-in pointers, which are commonly used to iterate over elements within arrays.

====Iteration over a Container ====
Accessing (but not modifying) each element of a container <tt>group</tt> of type <tt>''C''<''T''></tt> using an iterator.

<source lang="cpp">
 for (
      typename C<T>::const_iterator iter = group.begin();
      iter != group.end();
      ++iter
     )
 {
     T const &element = *iter;
 
     // access element here
 }
</source>

Note the usage of typename. It informs the compiler that 'const_iterator' is a type as opposed to a static member variable.  (It is only necessary inside templated code, and indeed in C++98 is invalid in regular, non-template, code.  This may change in the next revision of the C++ standard so that the '''typename''' above is always permitted.)

Modifying each element of a container <tt>group</tt> of type <tt>''C''<''T''></tt> using an iterator.

<source lang="cpp">
 for (
      typename C<T>::iterator iter = group.begin();
      iter != group.end();
      ++iter
     )
 {
     T &element = *iter;
 
     // modify element here
 }
</source>

When modifying the container itself while iterating over it, some containers (such as vector) require care that the iterator doesn't become invalidated, and end up pointing to an invalid element.  For example, instead of:

<source lang="cpp">
  for (i = v.begin(); i != v.end(); ++i) {
    ...
    if (erase_required) {
      v.erase(i);
    }
  }
</source>

Do:

<source lang="cpp">
  for (i = v.begin(); i != v.end(); ) {
    ...
    if (erase_required) {
        i = v.erase(i);
    } else {
        ++i;
    }
  }
</source>


The <tt>erase()</tt> member function returns the next valid iterator, or <tt>end()</tt>, thus ending the loop.  Note that <tt>++i</tt> is ''not'' executed when <tt>erase()</tt> has been called on an element.

===Functors===
A functor or function object, is an object that has an <code>operator ()</code>.  The importance of functors is that they can be used in many contexts in which C++ functions can be used, whilst also having the ability to maintain state information.  Next to iterators, functors are one of the most fundamental ideas exploited by the STL.

The STL provides a number of pre-built functor classes; std::less, for example, is often used to specify a default comparison function for algorithms that need to determine which of two objects comes "before" the other.

<source lang="cpp">

 #include <vector>
 #include <algorithm>
 #include <iostream>

 // Define the Functor for AccumulateSquareValues
 template<typename T>
 struct AccumulateSquareValues
 {
     AccumulateSquareValues() : sumOfSquares()
     {
     }
     void operator()(const T& value)
     {
         sumOfSquares += value*value;
     }
     T Result() const
     {
         return sumOfSquares;
     }
     T sumOfSquares;
 };

 std::vector<int> intVec;
 intVec.reserve(10);
 for( int idx = 0; idx < 10; ++idx )
 {
     intVec.push_back(idx);
 }
 AccumulateSquareValues<int> sumOfSquare = std::for_each(intVec.begin(), 
                                                         intVec.end(), 
                                                         AccumulateSquareValues<int>() );
 std::cout << "The sum of squares for 1-10 is " << sumOfSquare.Result() << std::endl;

 // note: this problem can be solved in another, more clear way:
 // int sum_of_squares = std::inner_product(intVec.begin(), intVec.end(), intVec.begin(), 0);
</source>

{{:C++ Programming/STL/Algorithms}}

===Allocators===
Allocators are used by the Standard C++ Library (and particularly by the STL) to allow parameterization of memory allocation strategies.  

The subject of allocators is somewhat obscure, and can safely be ignored by most application software developers.  All standard library constructs that allow for specification of an allocator have a default allocator which is used if none is given by the user.

Custom allocators can be useful if the memory use of a piece of code is unusual in a way that leads to performance problems if used with the general-purpose default allocator.  There are also other cases in which the default allocator is inappropriate, such as when using standard containers within an implementation of replacements for global operators new and delete.

[[Category:C++ Programming|{{SUBPAGENAME}}]]
