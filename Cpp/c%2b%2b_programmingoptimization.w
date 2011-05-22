>== Optimizations ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== Optimizations ==
Optimization can be regarded as a directed effort to increase the performance of something, an important concept in engineering, in the particular in this case we will cover Software engineering. We will deal with specific computational tasks and best practices to reduce resources utilizations, not only of system resources but also of programmers and users, all based in optimal solutions evolved from the empirical validating of hypothesis and logical steps.

All optimization steps taken should have as a goal the reduction of requirements and the promotion of the program objectives, in doubt all claims can only be substantiated by doing a [[#Profiling|profiling]] the given problem and the applied solution. '''Without profiling any optimization is moot'''.

Optimization is often a topic of discussion among programmers and not all conclusion may be consensual, since they are very closely related to the goals, the programmer experience and dependent of specific setups. The level of optimization will mostly depend directly from actions and decisions the programmer makes. Those can be simple things, from simple coding practices to the selection of the tools one choses to use to create the program.  Even selecting the right compiler will have an impact.  A good optimizing compilers permits the programmer to define his aspirations for the optimized outcome, how good the compiler is at optimizing depends on the level of satisfaction the programmer gets from the resulting compilation. 

=== Code ===
One of the safest ways of optimization is to reduce complexity, ease organization and structure and at the same time evading code bloat. This requires the capacity to plan without losing track of future needs, in fact it is a compromise the programmer makes between a multitude of factors. 

Code optimization techniques, fall into the categories of:
* High Level Optimization
** Algorithmic Optimization (Mathematical Analysis)
** Simplification

* Low Level Optimization
** Loop Unrolling
** Strength Reduction
** Duff's Device
** Clean Loops

==== KISS ====
The "keep it simple, stupid" ([[w::KISS principle|KISS]]) principle, calls for giving simplicity a high priority in development. It is very similar to a maxim from Albert Einstein's that states, "everything should be made as simple as possible, but no simpler.", the difficulty for many adopters have is to determine what level of simplicity should be maintained. In any case, analysis of basic and simpler system is always easier, removing complexity will also open the door for code reutilization and a more generic approach to tasks and problems.

{{SAMPLE|sampletext=
"Always code as if the guy who ends up maintaining your code will be a violent psychopath who knows where you live."|caption=—Martin Golding}}

==== Code cleanup ====
Most of the benefits of a code cleanup should be evident to the experienced programmer, they become a second nature due to the adoption of good programming style guidelines. But as in any human activity, errors will occur and exceptions made, so, in this section we will try to remember the small changes that can have an impact on the optimization of your code. 

;virtual member functions
Remember the cost on performance of virtual members functions (covered when introducing the [[C++ Programming/Classes#virtual|virtual keyword]]). At the time optimization becomes an issue most project design change regarding optimization will not be possible, but artifacts may remain to be cleaned up. Guaranteeing that no superfluous use of virtual (like in the leaf nodes of your class/structure inheritance trees), will permit other optimizations to occur (i.e.: compiler [[#auto inline|optimized inline]]).

==== The right data in the right container ====
One of the top bottleneck on today's systems is dealing with memory [[w:Cache|caches]], be it [[w:CPU cache|CPU cache]] or the physical memory resources, even if [[w:Paging|paging]] problems are becoming increasingly rare. Since the data (and the load level) a program will handle is highly predictable at the design level, the better optimizations still fall to the programmer.

One should store the appropriate data structure in the appropriate container, prefer storing pointers to objects rather than the objects themselves, use "smart" pointers (see the Boost library) and don't attempt to store auto_ptr<> in STL containers, it is not allowed by the Standard, but some implementations are known to incorrectly allow it.

Avoid removing and inserting elements in the middle of a container, doing it at the end of the container has less overhead. Use STL containers when the number of objects is unknown; use static array or buffer when it is known. This requires the 
understanding of not only each container, but its O(x) guarantees.

Take as an example the STL containers on the issue of using (<code>myContainer.empty()</code>) versus (<code>myContainer.size() == 0</code>), it is important to understand that depending on the container type or its implementation, the size member function might have to count the number of objects before comparing it to zero. This is very common with list type containers. 

While the STL attempts to provides optimal solutions to general cases, if performance does not match your requirements think about writing your own optimal solution for your case, maybe a custom container (probably based on vector) that does not call individual object destructors and uses custom allocators that avoid the delete time overhead. 

Using pre-allocation of memory can provide some speed gains and be as simple remember to use the STL vector<T>::reserve() if permitted.  Optimize the use system's memory and the target hardware.  In today's systems, with virtual memory, threads and multiple-cores (each with its own cache) where I/O operations on the main memory and the amount of time spent moving it around, can slow things down.  This can become a performance bottleneck.  Instead opt for array-based data structures (cache-coherent data structures), like the STL vector, because data is stored contiguously in memory, over pointer-linked data structures as in linked lists. This will avoid "death by swapping", as the program needs to access highly fragmented data, and will even help the memory pre-fetch that most modern processors do today.

Whenever possible avoid returning containers by value, pass containers by reference.

==== Consider security costs ====
Security always has a cost, even in programming. For any algorithm, adding checks, will result in increase the number of steps it takes to finish. As languages get more complex and abstract, understanding all the finer details (and remember them) increases the time needed to obtain the required experience. Sadly most of the steps taken by some of the implementors of the C++ language lack visibility to the programmer and since they are outside of the standard language, aren't often learned. Remember to familiarized yourself with any extensions or particularities of the C++ implementation you are using.

As a language that puts the power of decision into the programmer's hands, C++ provides several instances where the a similar result can be archived by similar but distinct means. Understanding the sometimes subtle differences is important. For instance, when deciding the needed requirements in [[C++ Programming/STL#vector|accessing members of a std::vector]], you can chose [], at() and the an iterator, all have similar results but with distinct performance costs and security considerations.

==== Code reutilization ====
Optimization is also reflected on the effectiveness of a code. If you can use an already existing code base/framework that a considerable number of programmers have access to, you can expect it to be less buggy and optimized to solve your particular need.

Some of these code repositories are available to programmers as libraries. Be careful to consider dependencies and check how implementation is done: if used without considerations this can also lead to code bloat and increased memory footprint, as well as decrease the portability of the code. We will take a close look at them in the [[../Compiler/Linker/Libraries|Libraries Section]] of the book.

To increase code reutilization you will probably fragment the code in smaller sections, files or code, remember to equate that more files and overall complexity also increases compile time.

==== Function and algorithmic optimizations ====
When creating a function or a algorithm to address a specific problem sometimes we are dealing with mathematical structures that are specifically indicated to be optimized by established methods of mathematical minimization, this falls into the specific field of [[Engineering Analysis/Optimization|Engineering analysis for optimization]].

{{TODO|Extend with small examples}}

===== Use of {{C++ Programming/kw|inline}} =====
As seen before when examining the {{C++ Programming/kw|inline}} keyword, it allows the definition of an inline type of function, that works similarly to [[X86 Disassembly/Code Optimization#Loop_Unwinding|loop unwinding]] for increasing code performance. A non-inline function requires a call instruction, several instructions to create a stack frame, and then several more instructions to destroy the stack frame and return from the function. By copying the body of the function instead of making a call, the size of the machine code increases, but the execution time ''decreases''. 

In addition to using the {{C++ Programming/kw|inline}} keyword to declare an inline function, optimizing compilers may decide to make other functions inline as well (see [[C++ Programming/Optimization#Compiler optimizations|Compiler optimizations]] section).

==== ASM ====
If portability is not a problem and you are proficient with assembler you can use it to optimize computational bottlenecks, even looking at the output of a disassembler will often help looking for ways to improve it. Using ASM in your code brings to the table some other problems (maintainability for instance) so use it at a last resort in you optimization process, and if you use it be sure to document what you have done well.

The [[x86 Disassembly]] Wikibook provides some [[x86 Disassembly/Optimization Examples|optimization examples]] using x86 ASM code.

{{NOTE|If using the gcc compiler, the -S option will output the compilation generated assembly.}}

=== Reduction of compile time ===
Some projects may take a long time to compile. To reduce the time it takes to finish compiling the first step is to check if you have the any Hardware deficiencies. You may be low in resources like memory or just have a slow CPU, even having your HD with a high level of fragmentation can increase compile time.

On the other side, problems may not be due to hardware limitations but in the tools you use, check if you are using the right tools for the job at hand, see if you have the latest version, or if do, if that is what is causing trouble, some incompatibilities may result from updates. In compilers new is always better, but you should check first what has changed and if it serves your purposes.

Experience tells that most likely if you are suffering from slow compile times, the program you are trying to compile was probably poorly designed, check the structure of object dependencies, the includes and take some the time to structure your own code to minimize re-compilation after changes if the compile time justifies it.

Use pre-compiled headers and external header guards this will reduce the work done by the compiler.

=== Compiler optimizations ===
[[w:Compiler optimization|Compiler optimization]] is the process of tuning, mostly automatically, the output of a compiler in an attempt to improve the operations the programmer has requested, so to minimize or maximize some attribute of an compiled program '''while ensuring the result is identical'''. By rilling in the compiler optimization programmers can write more intuitive code, and still have them execute in a reasonably fast way, for instance skipping the use of [[C++ Programming/Operators Table|pre-increment/decrement operators]].

Generally speaking, optimizations are not, and can not be, defined on the C++ standard. The standard sets rules and best practices that dictate a normalization of inputs and outputs. The C++ standard itself permits some latitude on how compilers perform their task since some sections are marked as implementation dependent but generally a base line is established, even so some vendors/implementors do creep in some singular characteristic apparently for security and optimization benefits.

One notion that is good to keep in mind is that there is not a perfect C++ compiler, but most recent compilers will do several simple optimizations by default, that attempt to abstract and take advantage of existing deeper hardware optimizations or specific characteristics of the target platform, most of these optimizations are almost always welcomed but it is up to the programmer still to have and idea of what is going on and if indeed they are beneficial. As a result it is highly recommended to examine your compiler documentation on how it operates and what optimizations are under the programmer's control, just because a compiler can make some optimization in theory does not mean that it will or even that it will result in an optimization.   

The most common compiler optimizations options available to the programmer fall into three categories:
*'''Speed'''; improving the runtime performance of the generated object code. This is the most common optimization
*'''Space'''; reducing the size of the generated object code
*'''Safety'''; reducing the possibility of data structures becoming corrupted (for example, ensuring that an illegal array element is not written to)

Unfortunately, many "speed" optimizations make the code larger, and many "space" optimizations make the code slower -- this is known as the [[w:space-time tradeoff|space-time tradeoff]].

==== auto-{{C++ Programming/kw|inline}} ====
Auto-inlining is similar to implicit inline. Inlining can be an optimization, or a pessimization depending on the code and optimization options selected.

==== Making use of extended instructions sets ====
{{TODO|[[w:3DNow!|3DNow!]], [[w:MMX (instruction set)|MMX]], etc...}}

==== GPU ====
{{TODO|Add missing information}}

=== Run time ===
As we have seen before runtime is the duration of a program execution, from beginning to termination. This is were all resources needed to run the compiled code are allocated and hopefully released, this is the final objective of any program to be executed, as such it should be the target for ultimate optimizations.   

=== Memory footprint ===
In the past computer memory has been expensive and technologically limited in size, and scarce resource for programmers.  Large amounts of ingenuity was spent in implement complex programs and process huge amounts of data using as little as possible of this resource.  Today, modern systems contain enough memory for most usages but capacity demands and expectations have increased as well; as such, techniques to minimize memory usage may still be essential.

;Remember to use <code>swap()</code> on <code>std::vector</code> (or deque).
When attempting to reduce reduce (or zero) the size of a vector or deque using the <code>swap()</code>, on a standard container of that type, will guarantee that the memory is released and no overhead buffer for growth is used. It will also avoid the fallacy of using <code>erase()</code> or <code>reserve()</code> that will not reduce the memory footprint. 

==== Lazy initialization ====
It is always needed to maintain the balance between the performance of the system and the resource consumption. 
Lazy instantiation is one memory conservation mechanism, by which the object initialization is deferred until it is required.

Look at the following example:

<source lang=cpp>
#include <iostream> 

class Wheel {
        int speed;
    public:
        int getSpeed(){
            return speed;
        }
        void setSpeed(int speed){
            this->speed = speed;
        }
};

class Car{
    private:
        Wheel wheel;
    public:
        int getCarSpeed(){
            return wheel.getSpeed();
        }
        char *getName(){
            return "My Car is a Super fast car";
        }
};

int main(){
    Car myCar;
    std::cout << myCar.getName();
}
</source>

Instantiation of class Car by default instantiates the class Wheel. The purpose of the whole class is to just print the name of the car.  Since the instance wheel doesn't serve any purpose, initializing it is a complete resource waste.

It is better to defer the instantiation of the un-required class until it is needed. Modify the above class Car as follows:

<source lang=cpp>
class Car{
    private:
        Wheel *wheel;
    public:
        Car() {
            wheel=NULL; // a better place would be in the class constructor initialization list
        }
        ~Car() {
             if (wheel) {
                 delete wheel;
             }
        }
        int getCarSpeed(){
            if(wheel == NULL){
                wheel = new Wheel(); 
            }
            return wheel->getSpeed();
        }
        char *getName(){
            return "My Car is a Super fast car";
        }
};
</source>
Now the Wheel will be instantiated only when the member function getCarSpeed() is called.

=== Parallelization ===
As seen when examining [[C++ Programming/Threading|threads]], they can be a "simple" form of taking advantage of hardware resources and optimize the speed performance of a program. When dealing with thread you should remember that it has a cost in complexity, memory and if done wrong when synchronization is required it can even reduce the speed performance, if the design permits it is best to allow threads to run as unencumbered as possible.

=== I/O reads and writes ===
[[Image: Queue_System.PNG|thumb|450 px|A Schematic of a Queue System]]
{{TODO|Delayed writes, read ahead, how the OS processes I/O requests...|C++ Programming}}

=== Profiling ===
Profiling is a form of [[w:dynamic program analysis|dynamic program analysis]] (as opposed to [[w:static code analysis|static code analysis]]), consists in the study of program's behavior using information gathered as the program executes. its purpose is usually to determine which sections of a program to optimize. Mostly by determining which parts of a program are taking most of the execution time, causing bottleneck on accessing resources or the level of access to those resources.

Global clock execution time should be the bottom line when comparing applications performance. Select your algorithms by examining the asymptotic order of executions, as in a parallel setup they will continue to give the best performance. In the case you find an hotspot that can not be parallelized, even after examining higher levels on the call stack, then you should attempt to find a slower but parallelizable algorithm.

{{TODO|small examples}}

;branch-prediction profiler

;call-graph generating cache profiler

;line-by-line profiling

;heap profiler

==== Profiler ====
;Free Profiling tools:
*Valgrind ( http://valgrind.org/ ) an instrumentation framework for building dynamic analysis tools. Includes a cache and branch-prediction profiler, a call-graph generating cache profiler, and a heap profiler. It runs on the following platforms: X86/Linux, AMD64/Linux, PPC32/Linux, PPC64/Linux, and X86/Darwin (Mac OS X). Open Source under the GNU General Public License, version 2.
*GNU gprof ( http://www.gnu.org/software/binutils/ ) a profiler tool. The program was first was introduced on the SIGPLAN Symposium on Compiler Construction in 1982, and is now part of the binutils that are available in mostly all flavors of UNIX. It is capable of monitoring time spent in functions (or even source code lines) and calls to/from them. Open Source  under the GNU General Public License.

== Further reading ==

* [[Optimizing C++]]

[[Category:C++ Programming|{{SUBPAGENAME}}]]
