>==== Java ====
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">==== Java ====
This is a comparison of the [[Java Programming|Java programming language]] with the C++ programming language. C++ and Java share many common traits. You can get a better understanding of Java in the [[Programming:Java|Java Programming WikiBook]].

'''Java''' was created initially to support [[w:network computing|network computing]] on [[w:embedded system|embedded system]]s. Java was designed to be extremely [[w:porting|portable]], [[w:computer security|secure]], [[w:thread (computer science)|multi-threaded]] and [[w:distributed computing|distributed]], none of which were design goals for C++. The syntax of Java was chosen to be familiar to C programmers, but direct compatibility with C was not maintained. Java also was specifically designed to be simpler than C++ but it keeps evolving above that simplification.

:{| class="wikitable"
!                              !! '''C++'''                            !! '''Java'''
|-
|Compatibility                 || backwards compatible, including C          ||| backwards compatibility with previous versions
|-
|Focus                         || execution efficiency                 ||| developer productivity
|-
|Freedom                       || trusts the programmer                ||| imposes some constrains to the programmer
|-
|Memory Management             || [[w:pointer|arbitrary memory access possible]]     ||| memory access only through objects
|-
|Code                          || concise expression                   ||| explicit operation
|-
|[[w:type safety|Type Safety]] || type casting is restricted greatly       ||| only compatible types can be cast
|-
|[[w:programming paradigm|Programming Paradigm]]          || [[w:procedural|procedural]] or [[w:object-oriented|object-oriented]]||| object-oriented
|-
|Operators                     || [[w:operator overloading|operator overloading]]             ||| meaning of operators immutable
|-
|Main Advantage                || powerful capabilities of language    ||| feature-rich, easy to use standard library
|}

Differences between C++ and Java are:
* C++ parsing is somewhat more complicated than with Java; for example, <code>Foo&lt;1&gt;(3);</code> is a sequence of comparisons if Foo is a variable, but it creates an object if Foo is the name of a class template.
* C++ allows {{C++ Programming/kw|namespace}} level constants, variables, and functions. All such Java declarations must be inside a class or [[w:interface (Java)|interface]].
* <code>[[w:const|const]]</code> in C++ indicates data to be 'read-only,' and is applied to types.  <code>final</code> in java indicates that the variable is not to be reassigned.  For basic types such as <code>const int</code> vs <code>final int</code> these are identical, but for complex classes, they are different.
* C++ doesn't support constructor delegation.
* C++ runs on the hardware, Java runs on a virtual machine so with C++ you have greater power at the cost of portability.
* C++, <tt>int main()</tt> is a function by itself, without a class.
* C++ access specification ('''public''', '''private''') is done with labels and in groups.
* C++ access to class members default to <tt>private</tt>, in Java it is package access.
* C++ classes declarations end in a semicolon.
* C++ lacks language level support for garbage collection while Java has built-in garbage collection to handle memory deallocation.
* C++ supports {{C++ Programming/kw|goto}} statements; Java does not, but its [[w:labelled break|labeled break]] and [[w:labelled continue|labeled continue]] statements provide some structured {{C++ Programming/kw|goto}}-like functionality. In fact, Java enforces [[w:structured control flow|structured control flow]], with the goal of code being easier to understand.
* C++ provides some low-level features which Java lacks. In C++, pointers can be used to manipulate specific memory locations, a task necessary for writing low-level [[w:operating system|operating system]] components. Similarly, many C++ compilers support [[w:inline assembler|inline assembler]]. In Java, assembly code can still be accessed as libraries, through the [[w:Java Native Interface|Java Native Interface]].  However, there is significant overhead for each call.
* C++ allows a range of implicit conversions between native types, and also allows the programmer to define implicit conversions involving compound types. However, Java only permits widening conversions between native types to be implicit; any other conversions require explicit cast syntax.
** A consequence of this is that although loop conditions (<code>if</code>, <code>while</code> and the exit condition in {{C++ Programming/kw|for}}) in Java and C++ both expect a boolean expression, code such as <code>if(a = 5)</code> will cause a compile error in Java because there is no implicit narrowing conversion from int to boolean. This is handy if the code were a typo for <code>if(a == 5)</code>, but the need for an explicit cast can add verbosity when statements such as <code>if (x)</code> are translated from Java to C++.
* For passing parameters to functions, C++ supports both true [[w:pass-by-reference|pass-by-reference]] and [[w:pass-by-value|pass-by-value]].  As in C, the programmer can simulate by-reference parameters with by-value parameters and [[w:indirection|indirection]].  In Java, all parameters are passed by value, but object (non-primitive) parameters are [[w:reference (computer science)|reference]] values, meaning [[w:indirection|indirection]] is built-in.
* Generally, Java built-in types are of a specified size and range; whereas C++ types have a variety of possible sizes, ranges and representations, which may even change between different versions of the same compiler, or be configurable via compiler switches.
** In particular, Java characters are 16-bit [[w:Unicode|Unicode]] characters, and strings are composed of a sequence of such characters. C++ offers both narrow and wide characters, but the actual size of each is platform dependent, as is the character set used. Strings can be formed from either type.
* The rounding and precision of floating point values and operations in C++ is platform dependent. Java provides a [[w:strictfp|strict floating-point model]] that guarantees consistent results across platforms, though normally a more lenient mode of operation is used to allow optimal floating-point performance.
* In C++, [[w:pointers|pointers]] can be manipulated directly as memory address values.  Java does not have pointers&mdash;it only has object references and array references, neither of which allow direct access to memory addresses.  In C++ one can construct pointers to pointers, while Java references only access objects.
* In C++ pointers can point to functions or member functions ([[w:function pointer|function pointer]]s or [[w:functor|functor]]s).  The equivalent mechanism in Java uses object or interface references.
* C++ features programmer-defined [[w:operator overloading|operator overloading]].  The only overloaded operators in Java are the "<code>+</code>" and "<code>+=</code>" operators, which concatenate strings as well as performing addition.
* Java features standard [[w:Application programming interface|API]] support for [[w:Reflection (computer science)|reflection]] and [[w:dynamic loading|dynamic loading]] of arbitrary new code.
* Java has generics. C++ has templates.
* Both Java and C++ distinguish between native types (these are also known as "fundamental" or "built-in" types) and user-defined types (these are also known as "compound" types). In Java, native types have value semantics only, and compound types have reference semantics only. In C++ all types  have value semantics, but a reference can be created to any object, which will allow the object to be manipulated via reference semantics.
* C++ supports [[w:multiple inheritance|multiple inheritance]] of arbitrary classes. Java supports multiple inheritance of types, but only single inheritance of implementation. In Java, a class can derive from only one class, but a class can implement multiple [[w:Interface (Java)|interface]]s.
* Java explicitly distinguishes between interfaces and classes. In C++ multiple inheritance and pure virtual functions makes it possible to define classes that function just as Java interfaces do.
* Java has both language and standard library support for [[w:multi-threading|multi-threading]].  The <code>synchronized</code> [[w:Java keywords|keyword in Java]] provides simple and secure [[w:Mutual exclusion|mutex lock]]s to support multi-threaded applications.  While mutex lock mechanisms are available through libraries in C++, the lack of language semantics makes writing [[w:thread safe|thread safe]] code more difficult and error prone.

===== Memory management ===== 
* Java requires automatic [[w:Garbage collection (computer science)|garbage collection]]. Memory management in C++ is usually done by hand, or through [[w:smart pointer|smart pointer]]s.  The C++ standard permits garbage collection, but does not require it; garbage collection is rarely used in practice.  When permitted to relocate objects, modern garbage collectors can improve overall application space and time efficiency over using explicit deallocation.
* C++ can allocate arbitrary blocks of memory. Java only allocates memory through object instantiation. (Note that in Java, the programmer can simulate allocation of arbitrary memory blocks by creating an array of bytes. Still, Java [[w:array|array]]s are objects.)
* Java and C++ use different idioms for resource management. Java relies mainly on garbage collection, while C++ relies mainly on the [[w:Resource Acquisition Is Initialization|RAII (Resource Acquisition Is Initialization)]] idiom. This is reflected in several differences between the two languages:
** In C++ it is common to allocate objects of compound types as local stack-bound variables which are destructed when they go '''''[[C++ Programming/Programming Languages/C++/Code/Statements/Scope|out of scope]]'''''. In Java compound types are always allocated on the heap and collected by the garbage collector (except in virtual machines that use [[w:escape analysis|escape analysis]] to convert heap allocations to stack allocations).
** C++ has destructors, while Java has [[w:finalizer|finalizer]]s. Both are invoked prior to an object's deallocation, but they differ significantly. A C++ object's destructor must be implicitly (in the case of stack-bound variables) or explicitly invoked to deallocate the object. The destructor executes [[w:Synchronization|synchronously]] at the point in the program at which the object is deallocated. Synchronous, coordinated uninitialization and deallocation in C++ thus satisfy the RAII idiom. In Java, object deallocation is implicitly handled by the garbage collector. A Java object's finalizer is invoked [[w:Asynchrony|asynchronously]] some time after it has been accessed for the last time and before it is actually deallocated, which may never happen. Very few objects require finalizers; a finalizer is only required by objects that must guarantee some clean up of the object state prior to deallocation&mdash;typically releasing resources external to the JVM. In Java safe synchronous deallocation of resources is performed using the try/finally construct.
** In C++ it is possible to have a [[w:dangling pointer|dangling pointer]] &ndash; a [[w:reference (computer science)|reference]] to an object that has been destructed; attempting to use a dangling pointer typically results in program failure. In Java, the garbage collector won't destruct a referenced object.
** In C++ it is possible to have an object that is allocated, but unreachable. An [[w:unreachable object|unreachable object]] is one that has no reachable references to it. An unreachable object cannot be destructed (deallocated), and results in a [[w:memory leak|memory leak]].  By contrast, in Java an object will not be deallocated by the garbage collector ''until'' it becomes unreachable (by the user program). (Note: ''[[w:weak reference|weak reference]]s'' are supported, which work with the Java garbage collector to allow for different ''strengths'' of reachability.) Garbage collection in Java prevents many memory leaks, but leaks are still possible under some circumstances.

===== Libraries ===== 
* [[C++ Programming/STL|C++ standard library]] provides a limited set of basic and relatively general purpose components. Java has a considerably larger standard library. This additional functionality is available for C++ by (often free) third party libraries, but third party libraries do not provide the same ubiquitous cross-platform functionality as standard libraries.
* C++ is mostly [[w:backward compatible|backward compatible]] with C, and C libraries (such as the [[w:Application programming interface|API]]s of most [[w:operating system|operating system]]s) are directly accessible from C++. In Java, the richer functionality its standard library is that it provides [[w:cross-platform|cross-platform]] access to many features typically only available in platform-specific libraries.  Direct access from Java to native operating system and hardware functions requires the use of the [[w:Java Native Interface|Java Native Interface]].

===== Runtime ===== 
* C++ is normally compiled directly to [[w:machine code|machine code]] which is then executed directly by the [[w:operating system|operating system]]. Java is normally compiled to [[w:byte-code|byte-code]] which the [[w:Java virtual machine|Java virtual machine]] (JVM) then either [[w:Interpreter (computing)|interprets]] or [[w:Just-in-time compilation|JIT]] compiles to machine code and then executes.
* Due to the lack of constraints in the use of some C++ language features (e.g. unchecked array access, raw pointers), programming errors can lead to low-level [[w:buffer overflow|buffer overflow]]s, [[w:page fault|page fault]]s, and [[w:segmentation fault|segmentation fault]]s. The [[C++ Programming/STL|Standard Template Library]], however, provides higher-level abstractions (like vector, list and map) to help avoid such errors. In Java, such errors either simply cannot occur or are detected by the [[w:Java virtual machine|JVM]] and reported to the application in the form of an [[w:exception handling|exception]].
* In Java, [[w:bounds checking|bounds checking]] is implicitly performed for all array access operations. In C++, array access operations on native arrays are not bounds-checked, and bounds checking for random-access element access on standard library collections like std::vector and std::deque is optional.

===== Miscellaneous ===== 
* Java and C++ use different techniques for splitting up code in multiple source files. Java uses a package system that dictates the file name and path for all program definitions. In Java, the compiler imports the executable [[w:class (file format)|class files]]. C++ uses a [[w:header file|header file]] [[w:source code|source code]] inclusion system for sharing declarations between source files.  (See [[w:Comparison of imports and includes|Comparison of imports and includes]].)
* Templates and macros in C++, including those in the standard library, can result in duplication of similar code after compilation.  Second, [[w:library (computer science)#Dynamic linking|dynamic linking]] with standard libraries eliminates binding the libraries at compile time.
* C++ compilation features a textual [[C++ Programming/Programming Languages/C++/Code/Compiler/Preprocessor|preprocessing]] phase, while Java does not.  Java supports many optimizations that mitigate the need for a preprocessor, but some users add a preprocessing phase to their build process for better support of conditional compilation.
* In Java, arrays are container objects which you can inspect the length of at any time. In both languages, arrays have a fixed size. Further, C++ programmers often refer to an array only by a pointer to its first element, from which they cannot retrieve the array size. However, C++ and Java both provide container classes ('''std::vector''' and '''java.util.ArrayList''' respectively) which are re-sizable and store their size.
* Java's division and modulus operators are well defined to truncate to zero.  C++ does not specify whether or not these operators truncate to zero or "truncate to -infinity".  -3/2 will always be -1 in Java, but a C++ compiler may {{C++ Programming/kw|return}} either -1 or -2, depending on the platform. [[w:C99|C99]] defines division in the same fashion as Java.  Both languages guarantee that <code>(a/b)*b + (a%b) == a</code> for all a and b (b != 0).  The C++ version will sometimes be faster, as it is allowed to pick whichever truncation mode is native to the processor.
* The sizes of integer types is defined in Java (int is 32-bit, long is 64-bit), while in C++ the size of integers and pointers is compiler-dependent.  Thus, carefully-written  C++ code can take advantage of the 64-bit processor's capabilities while still functioning properly on 32-bit processors. However, C++ programs written without concern for a processor's word size may fail to function properly with some compilers. In contrast, Java's fixed integer sizes mean that programmers need not concern themselves with varying integer sizes, and programs will run exactly the same. This may incur a performance penalty since Java code cannot run using an arbitrary processor's word size.

===== Performance ===== 
Computing performance is a measure of resource consumption when a system of hardware and software performs a piece of computing work such as an algorithm or a transaction. Higher performance is defined to be 'using fewer resources'. Resources of interest include memory, bandwidth, persistent storage and CPU cycles. Because of the high availability of all but the latter on modern desktop and server systems, performance is colloquially taken to mean the least CPU cycles; which often converts directly into the least wall clock time. Comparing the performance of two software languages requires a fixed hardware platform and (often relative) measurements of two or more software subsystems. This section compares the relative computing performance of C++ and Java on common operating systems such as Windows and Linux.

Early versions of Java were significantly outperformed by statically compiled languages such as C++. This is because the program statements of these two closely related languages may compile to a few machine instructions with C++, while compiling into several byte codes involving several machine instructions each when interpreted by a Java [[w:JVM|JVM]]. For example:
{| class="wikitable"
! Java/C++ statement
! C++ generated code
! Java generated byte code
|- 
| vector[i]++;
| mov edx,[ebp+4h]<br/>
mov eax,[ebp+1Ch]<br/>
inc dword ptr [edx+eax*4]
| aload_1<br/>
iload_2<br/>
dup2<br/>
iaload<br/>
iconst_1<br/>
iadd<br/>
iastore
|-
|}

While this may still be the case for [[w:embedded systems|embedded systems]] because of the requirement for a small footprint, advances in [[w:Just-in-time compilation|just in time (JIT)]] compiler technology for long-running server and desktop Java processes has closed the performance gap and in some cases given the performance advantage to Java. In effect, Java byte code is compiled into machine instructions at run time, in a similar manner to C++ static compilation, resulting in similar instruction sequences.

C++ is still faster in most operations than Java at the moment, even at low-level and numeric computation. For in-depth information you could check [http://www.idiom.com/~zilla/Computer/javaCbenchmark.html Performance of Java versus C++]. It's a bit pro-Java but very detailed.

{{BookCat}}
