>=== Multitasking ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== Multitasking ===
'''Multitasking''' is a process by which multiple tasks (also known as [[#Processes|processes]]), share common processing resources such as a [[w:Central processing unit|CPU]].

A computer with a single CPU, will only run one process at a time. By ''running'' it means that in a specific point in time, the CPU is actively executing instructions for that process. With a single CPU systems using [[w:Scheduling (computing)|scheduling]] can achieve multitasking by which the time of the processor is time-shared by several processes permitting each to advance seemingly in parallel their computations. A process runs for some time and another waiting gets a turn.

The act of reassigning a CPU from one task to another one is called a [[w:context switch|context switch]]. When context switches occur frequently enough the illusion of [[w:Parallel computing|parallelism]] is achieved.

{{NOTE|Context switching has a cost, when deciding to use multi-tasks a programmer must be aware of trade-offs in performance.}}

Even on computers with more than one CPU, [[w:multiprocessor|multiprocessor]] machines, multitasking allows many more tasks to be run than there are CPUs. 

Operating systems may adopt one of many different [[w:Scheduling (computing)|scheduling strategies]], which generally fall into the following categories:
* In ''multiprogramming'' systems, the running task keeps running until it performs an operation that requires waiting for an external event (e.g. reading from a tape) or until the computer's scheduler forcibly swaps the running task out of the CPU. Multiprogramming systems are designed to maximize CPU usage.
* In ''[[w:time-sharing|time-sharing]]'' systems, the running task is required to relinquish the CPU, either voluntarily or by an external event such as a [[w:hardware interrupt|hardware interrupt]]. Time sharing systems are designed to allow several programs to execute apparently simultaneously.  The term ''time-sharing'' used to define this behavior is no longer in use, having been replaced by the term ''multitasking''.
* In ''[[w:Real-time computing|real-time]]'' systems, some waiting tasks are guaranteed to be given the CPU when an external event occurs. Real time systems are designed to control mechanical devices such as industrial robots, which require timely processing.

Multitasking has already been successfully integrated into current Operating Systems. Most computers in use today supports running several processes at a time this is required for systems using [[w:Symmetric multiprocessing|symmetric multiprocessor (SMP)]] in ''distributed computing'' and [[w:Multi-core processor|multi-core or chip multiprocessors (CMPs)]] computing, where processors have gone from dual-core to quad-core and core number will continue to increase. Each technology with has their specific limitations and applicability but share the common objective of performing concurrent processing.

{{NOTE|Due to the general adoption of the new paradigm it becomes extremely important to prepare your code for it (plan for scalability), understand guarantees regarding parallelization, and select external libraries that provide the required support.}}

=== Processes ===
[[w:process (computing)|Process]]es are independent execution units that contain their own state information, use their own address spaces, and only interact with each other via interprocess communication mechanisms (IPC). A process can be said to at least contain one thread of execution (not to be confused to a complete thread construct). Processes are managed by the hosting OS in a process data structure. The number of maximum number of processes depend on the OS and on the available resources of that system.

==== Child Process ====
A '''child process''' is a ''spawn process'', a process that was created by another process (the [[w:parent process|parent process]]), inheriting most of the parent attributes, such as opened files. Each process may create many child processes but will have at most one parent process; if a process does not have a parent this usually indicates that it was created directly by the [[w:kernel (computer science)|kernel]].

In [[w:Unix|UNIX]], a child process is in fact created (using [[w:fork (operating system)|fork]]) as a copy of the parent. The child process can then [[w:overlay (operating system)|overlay]] itself with a different program (using <tt>[[w:Exec (operating system)|exec]]</tt>) as required. The very first process, called [[w:init|init]], is started by the kernel at booting time and never terminates; other parentless processes may be launched to carry out various [[w:daemon (computing)|daemon]] tasks in [[w:userspace|userspace]]. Another way for a process to end up without a parent is if its parent dies, leaving an [[w:orphan process|orphan process]]; but in this case it will shortly be adopted by init.

==== Inter-Process Communication (IPC) ====
IPC is generally managed by the operating system.

===== Shared Memory =====
Most of more recent OSs provide some sort of memory protection. In a Unix system, each process is given its own virtual address space, and the system, in turn, guarantees that no process can access the memory area of another. If an error occurs on a process only that process memory's contents can be corrupted. 

With shared memory the need of enabling random-access to shared data between different processes is addressed. But declaring a given section of memory as simultaneously accessible by several processes raises the need for control and synchronization, since several processes might try to alter this memory area at the same time.

=== Multi-Threading ===
For now the C++ standard does not include specifications or built in support for multi-threading multi-threading support. [[w:Thread (computer science)|Threading]] must therefore be implemented using special threading libraries, which are often platform dependent, as an extension to the C++ standard.

{{NOTE|The new C++0x standard will support multi-threading. Reducing the need to know multiple APis and enabling increasing the portability of code.}} 

Some popular C++ threads libraries include:<br>
<small>(This list is not intended to be complete.)</small>

* [[C++ Programming/Libraries/Boost|Boost]] - This package includes several libraries, one of which is threads (concurrent programming). the boost threads library is not very full featured, but is complete, portable, robust and in the flavor of the C++ standard. Uses the boost license that is similar to the BSD license.
* Intel® Threading Building Blocks (TBB) (http://www.threadingbuildingblocks.org/) offers a rich approach to expressing parallelism in a C++ program. The library helps you take advantage of multi-core processor performance without having to be a threading expert. Threading Building Blocks is not just a threads-replacement library. It represents a higher-level, task-based parallelism that abstracts platform details and threading mechanism for performance and scalability and performance. It is an open source project under the GNU General Public License version two (GPLv2) with the runtime exception.
* [[w:Adaptive Communication Environment|Adaptive Communication Environment (often referred to as ACE)]] - Another toolkit which includes a portable threads abstraction along with many many other facilities, all rolled into one library. Open source released under a nonstandard but nonrestrictive license.
* [[w:ZThreads|Zthreads]] - A portable thread abstraction library. This library is feature rich, deals only with concurrency and is open source licensed under the MIT license. 

Of course, you can access the full POSIX and the C language threads interface from C++ and on Windows the API. So why bother with a library on top of that?

The reason is that things like locks are resources that are allocated, and C++ provides abstractions to make managing these things easier. For instance, <code>boost::scoped_lock<></code> uses object construction/destruction to insure that a mutex is unlocked when leaving the lexical scope of the object. Classes like this can be very helpful in preventing deadlock, race conditions, and other problems unique to threaded programs. Also, these libraries enable you to write cross-platform multi-threading code, while using platform-specific function cannot.

In any case when using ''threading methodology'', dictates that you must identify '''hotspots''', the segments of code that take the most execution time. To determine the best chance at achieving the maximum performance possible, the task can be approached from ''bottom-up'' and ''top-down'' to determine those code segments that can run in parallel.

In the ''bottom-up'' approach, one focus solely on the hotspots in the code. This requires a deep analysis of the call stack of the application to determine the sections of code that can be run in parallel and reduce hotspots. In hotspot sections that employ concurrency, it is still required to move that concurrency at a point higher up in the call stack as to increase the [[#Computation_granularity|granularity]] of each thread execution.

Using the ''top-down'' approach, the focus is on all the parts of the application, in determining what computations can be coded to run in parallel, at a higher level of abstraction. Reducing the level of abstraction until the overall performance gains are sufficient to reach the necessary goals, the benefit being speed of implementation and code re-usability. This is also the best method for archiving a optimal level of [[#Computation_granularity|granularity]] for all computations.

;Threads vs. Processes
Both threads and processes are methods of parallelizing an application, its implementation may differ from one [[w:operating system|operating system]] to another. A process has always one thread of execution, also known as the '''primary thread'''. In general, a thread is contained inside a process (in the address space of the process) and different threads of the same process share some resources while different processes do not.

==== Atomicity ====
Atomicity refers to '''atomic''' operations that are '''indivisible''' and/or '''uninterruptible'''. Even on a single core, you cannot assume that an operation will be atomic. In that regard only when using assembler can one guarantee the atomicity of an operation. Therefore, the C++ standard provides some guarantees as do operating systems and external libraries.

An '''atomic operation''' can also be seen as any given set of [[w:Instruction (computer science)|operation]]s that can be combined so that they appear to the rest of the system to be a single operation with only two possible outcomes: success or failure. This all depends on the level of abstraction and underling guarantees.

All modern processors provide basic '''atomic primitives''' which are then used to build more complex atomic objects. In addition to atomic read and write operations, most platforms provide an atomic read-and-update operation like [[w:test-and-set|test-and-set]] or [[w:compare-and-swap|compare-and-swap]], or a pair of operations like [[w:Load-Link/Store-Conditional|load-link/store-conditional]] that only have an effect if they occur atomically (that is, with no intervening, conflicting update). These can be used to implement [[w:Lock (software engineering)|locks]], a vital mechanism for multi-threaded programming, allowing invariants and atomicity to be enforced across groups of operations.

Many [[w:central processing unit|processors]], especially [[w:32-bit|32-bit]] ones with [[w:64-bit|64-bit]] [[w:floating point|floating point]] support, provide some read and write operations that are not atomic: one [[w:Thread (computer science)|thread]] reading a 64-bit register while another thread is writing to it may see a combination of both "before" and "after" values, a combination that may never actually have been written to the register. Further, only single operations are guaranteed to be atomic; threads arbitrarily performing groups of reads and writes will also observe a mixture of "before" and "after" values. Clearly, invariants cannot be relied on when such effects are possible.

If not dealing with known guaranteed atomic operations, one should rely on the synchronization primitives at the level of abstraction that you're coding to.

;Example - One process
For example, imagine a single process is running on a computer incrementing a value in a given [[w:memory location|memory location]]. To increment the value in that memory location:
:# the process reads the value in the memory location;
:# the process adds one to the value;
:# the process writes the new value back into the memory location.

;Example - Two processes
Now, imagine two processes are running incrementing a single, shared memory location:
# the first process reads the value in memory location;
# the first process adds one to the value;
but before it can write the new value back to the memory location it is suspended, and the second process is allowed to run:
# the second process reads the value in memory location, the ''same'' value that the first process read;
# the second process adds one to the value;
# the second process writes the new value into the memory location.
The second process is suspended and the first process allowed to run again:
# the first process writes a now-wrong value into the memory location, unaware that the other process has already updated the value in the memory location.

This is a trivial example. In a real system, the operations can be more complex and the errors introduced extremely subtle. For example, reading a 64-bit value from memory may actually be implemented as two [[w:sequence|sequential]] reads of two 32-bit memory locations. If a process has only read the first 32-bits, and before it reads the second 32-bits the value in memory gets changed, it will have neither the original value nor the new value but a mixed-up [[w:garbage (computer science)|garbage]] value.

Furthermore, the specific order in which the processes run can change the results, making such an error difficult to detect and debug.

;OS and portability
Considerations are not only necessary with regard to the underling hardware but also in dealing with the different OS APIs. When porting code across different OSs one should consider what guarantees are provided. Similar considerations are necessary when dealing with external libraries. 

{{NOTE|For instance on the Macintosh, the set file position call is atomic, whereas on Windows, it's a pair of calls.}}

==== Race condition ====
A '''race condition''' (''data race'', or simply ''race''), occurs when data is accessed concurrently from multiple execution paths. It happens for instance when multiple threads have shared access to the same resource such as a file or a block of memory, and at least one of the accesses is a write. This can lead to interference with one another.

Threaded programming is built around predicates and shared data. It is necessary to identify all possible execution paths and identify truly independent computations. To avoid problems it is best to implement concurrency at the highest level possible.

Most ''race conditions'' occur due to an erroneous assumption about the order in which threads will run. When dealing with shared variables, never assume that a threaded write operation will precede a threaded read operation. If you need guarantees you should see if synchronization primitives are available, and if not, you should implement your own. 

===== Locking =====
Locking temporarily prevents un-shareable resources from being used simultaneously. Locking can be achieved by using a synchronization object.

One of the biggest problems with threading is that locking requires analysis and understanding of the data and code relationships. This complicates software development--especially when targeting multiple operating systems. This makes multi-threaded programming more like art than science.

The number of locks (depending on the synchronization object) may be limited by the OS. A lock can be set to protect more than one resource, if always accessed in the same critical region.

===== Critical section =====
A '''critical section''' is a region defined as critical to the parallelization of code execution. The term is used to define code  sections that need to be executed in isolation with respect to other code in the program.

This is a common fundamental concept. These sections of code need to be protected by a synchronization technique as they can create  ''race conditions''.

===== Deadlock =====
A deadlock is said to happen whenever there is a lock operation that results in a never-ending waiting cycle among concurrent threads.

==== Synchronization ====
Except when used to guarantee the correct execution of a parallel computation, synchronization is an overhead. Attempt to keep it to a minimum by taking advantage of the [[#Thread local storage (TLS)|thread's local storage]] or by using exclusive memory locations.

===== Computation granularity =====
'''Computation granularity''' is loosely defined as the amount of computation performed before any synchronization is needed. The longer the time between synchronizations, the less granularity the computation will have. When dealing with the requirements for parallelism, it will mean being easier to scale to an increased number of threads and having lower overhead costs. A high level of granularity can mean that any benefit from using threads will be lost due to the requirements of synchronization and general thread overhead.

===== [[w:Mutual exclusion|Mutex]] =====
Mutex is an abbreviation for ''mutual exclusion''. It relies on a synchronization facility supplied by the operating system (not the CPU). Since this system objects can only be owned by a single thread at any given time, the mutex object facilitates protection against data races and allows for thread-safe synchronization of data between threads. By calling one of the lock functions, the thread obtains ownership of a mutex object, it then relinquishes ownership by calling the corresponding unlock function. Mutexes can be either recursive or non-recursive, and may grant simultaneous ownership to one or many threads.

===== Semaphore =====
A semaphore is a ''yielding'' synchronization object that can be used to synchronize several threads. This is the most commonly used method for synchronization

===== Spinlock =====
Spinlocks are ''busy-wait'' synchronization objects, used as a substitute for Mutexes. They are an implementation of inter-thread locking using machine dependent assembly instructions (such as test-and-set) where a thread simply waits (''spins'') in a loop that repeatedly checks if the lock becomes available (''busy wait''). This is why spinlocks perform better if locked for a short period of time. They are never used on single-CPU machines.

==== Threads ====
Threads are by definition a coding construct and part of a [[w:computer program|program]] that enable it to [[w:Fork_(operating_system)|fork]] (or split) itself into two or more simultaneously (or pseudo-simultaneously) running [[w:task (computers)|task]]s. Threads use [[w:pre-emptive multitasking|pre-emptive multitasking]]. 

The thread is the basic unit (the smallest piece of code) to which the operating system can allocate a distinct processor time (schedule) for execution, this means that threads in reality don't run concurrently but in sequence on any single core system.  Threads often depend on the OS thread scheduler to preempt a busy thread and resume another thread.

The thread today is not only a key concurrency model supported by most if not all modern computers, programming languages, and operating systems but is it self at the core of hardware evolution, such as symmetric multiprocessors, understanding threads is now a necessity to all programmers.

The order of execution of the threads is controlled by the process scheduler of the OS, it is non-deterministic. The only control available to the programmer is in attributing a priority to the thread but never assume a particular order of execution.

;Thread quantum

===== User Interface Thread =====
This type of distinction is reserved to indicate that the particular thread implements a message map to respond to events and messages generated by user inputs as he interacts with the application. This is especially common when working with the Windows platform (Win32 API) because of the way it implements message pumps.

===== Worker Thread =====
This distinction serves to specify threads that do not directly depend or are part of the graphical user interface of the application, and run concurrently with the main execution thread.

===== Thread local storage (TLS) =====
The residence of thread local variables, a thread dedicated section of the global [[w:Computer storage|memory]]. Each thread (or fiber) will receive its own stack space, residing in a different memory location. This will consist of both reserved and initially committed memory. That is freed when the thread exits but will not be freed if the thread is terminated by other means.

Since all threads in a [[w:Process (computing)|process]] share the same [[w:address space|address space]], it makes data in a static or [[w:global variable|global variable]] to be normally located at the same memory location, when referred to by threads from the same process. It is important for software to take in consideration hardware cache coherence. For instance in multiprocessor environments, each processor has a local cache. If threads on different processors modify variables residing on the same cache line, this will invalidate that cache line, forcing a cache update, hurting performance. This is referred to as '''false sharing'''.

This type of storage is indicated for variables that store temporary or even partial results, since condensing the needed synchronization of the partial results in as fewer and infrequent instances possible will contribute to the reduction of synchronization overhead.

===== Thread Synchronization =====
The synchronization can be defined in several steps the first is the process lock, were a process is made to halt execution due to find a protected resource locked, there is a cost for locking especially if the lock last for too long.

Obviously there is a performance hit if any synchronization mechanism is heavily used. Because they are an expensive operation, in certain cases, increasing the use of TLSs instead of relying only on shared data structures will reduce the need for synchronization.

;Critical Section

{{TODO|Guard or monitor sections ?|C++ Programming}}

;Suspend and Resume

;Synchronizing on Objects

;Cooperative vs. Preemptive Threading

;Thread pool
[[Image:Thread pool.svg|390px|right|thumb|A simple thread pool. The task queue has many waiting tasks (blue circles). When a thread opens up in the queue (green box with dotted circle) a task comes off the queue and the open thread executes it (red circles in green boxes). The completed task then "leaves" the thread pool and joins the completed tasks list (yellow circles)..]]
{{clear}}

==== Fibers ====
A '''fiber''' is a particularly lightweight [[w:thread of execution|thread of execution]]. Like threads, fibers share [[w:address space|address space]]. However, fibers use [[w:Computer multitasking#Cooperative multitasking/time-sharing|co-operative multitasking]], fibers yield themselves to run another fiber while executing.

;Operating system support
Less support from the [[w:operating system|operating system]] is needed for fibers than for threads. They can be implemented in modern [[w:Unix|Unix]] systems using the library functions [[w:setcontext|getcontext, setcontext and swapcontext]] in ucontext.h, as in [[w:GNU Portable Threads|GNU Portable Threads]].

On [[w:Microsoft Windows|Microsoft Windows]], fibers are created using the ConvertThreadToFiber and CreateFiber calls; a fiber that is currently suspended may be resumed in any thread. Fiber-local storage, analogous to [[w:thread-local storage|thread-local storage]], may be used to create unique copies of variables.

[[w:Symbian OS|Symbian OS]] uses a similar concept to fibers in its Active Scheduler. An [[w:Active object (Symbian OS)|Active object (Symbian OS)]] contains one fiber to be executed by the Active Scheduler when one of several outstanding asynchronous calls complete. Several Active objects can be waiting to execute (based on priority) and each one must restrict is own execution time.

=== Exploiting parallelism ===
Most of the parallel architecture research was done in the 1960s and 1970s, providing solutions for problems that only today are reaching general awareness. As the need of concurrent programming increases, mostly due to today's hardware evolution, we as programmers are pressed to implement programming models that ease the complicated process of dealing with the old thread model in a way it preserves development time by abstracting the problem. 

{{TODO|To extend|C++ Programming}}

==== OpenMP ====
[[Image:OpenMP language extensions.svg|center|670px|thumb|Chart of OpenMP constructs.]]

{{TODO|To extend|C++ Programming}}

[[Category:C++ Programming|{{SUBPAGENAME}}]]
