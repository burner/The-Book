>== Resource Acquisition Is Initialization (RAII) ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== Resource Acquisition Is Initialization (RAII) ==
The RAII technique is often used for controlling thread locks in multi-threaded applications.
Another typical example of RAII is file operations, e.g. the C++ standard library's file-streams. An input file stream is opened in the object's constructor, and it is closed upon destruction of the object. Since C++ allows objects to be allocated on the [[w:call stack|stack]], C++'s scoping mechanism can be used to control file access.

With RAII we can use for instance Class destructors to guarantee to clean up, similarly to functioning as ''finally'' keyword on other languages, doing automates the task and so avoids errors but gives the freedom not to use it.

RAII is also used (as shown in the example below) to ensure exception safety. RAII makes it possible to avoid resource leaks without extensive use of <code>try</code>/<code>catch</code> blocks and is widely used in the software industry.

The ownership of dynamically allocated memory (memory allocated with <tt>new</tt>) can be controlled with RAII. For this purpose, the C++ Standard Library defines [[w:auto ptr|auto ptr]]. Furthermore, lifetime of shared objects can be managed by a smart pointer with shared-ownership semantics such as <code>boost::shared_ptr</code> defined in C++ by the [[../Libraries/Boost|Boost library]] or policy based <code>Loki::SmartPtr</code> from [[w:Loki (C++)|Loki library]].

The following RAII class is a lightweight wrapper to the C standard library file system calls.
<source lang=cpp>
 #include <cstdio>
 
 // exceptions
 class file_error { } ;
 class open_error : public file_error { } ;
 class close_error : public file_error { } ;
 class write_error : public file_error { } ;
   
 class file
 {
 public:
     file( const char* filename )
         :
         m_file_handle(std::fopen(filename, "w+"))
     {
         if( m_file_handle == NULL )
         {
             throw open_error() ;
         }
     }
   
     ~file()
     {
         std::fclose(m_file_handle) ;
     }
 
     void write( const char* str )
     {
         if( std::fputs(str, m_file_handle) == EOF )
         {
             throw write_error() ;
         }
     }
  
     void write( const char* buffer, std::size_t num_chars )
     {
         if( num_chars != 0
             &&
             std::fwrite(buffer, num_chars, 1, m_file_handle) == 0 )
         {
             throw write_error() ;
         }
     }
 
 private:
     std::FILE* m_file_handle ;
       
     // copy and assignment not implemented; prevent their use by
     // declaring private.
     file( const file & ) ;
     file & operator=( const file & ) ;
 } ;
</source>

This RAII class can be used as follows :

<source lang=cpp>
void example_with_RAII()
{
  // open file (acquire resource)
  file logfile("logfile.txt") ;
   
  logfile.write("hello logfile!") ;
  // continue writing to logfile.txt ...
   
  // logfile.txt will automatically be closed because logfile's
  // destructor is always called when example_with_RAII() returns or
  // throws an exception.
}
</source>

Without using RAII, each function using an output log would have to manage the file explicitly.
For example, an equivalent implementation without using RAII is this:

<source lang=cpp>
void example_without_RAII()
{
  // open file
  std::FILE* file_handle = std::fopen("logfile.txt", "w+") ;
 
  if( file_handle == NULL )
  {
    throw open_error() ;
  }
  
  try
  {

    if( std::fputs("hello logfile!", file_handle) == EOF )
    {
      throw write_error() ;
    }
  
    // continue writing to logfile.txt ... do not return
    // prematurely, as cleanup happens at the end of this function
  }
  catch(...)
  {
    // manually close logfile.txt
    std::fclose(file_handle) ;
 
    // re-throw the exception we just caught
    throw ;
  }
  
  // manually close logfile.txt 
  std::fclose(file_handle) ;
}
</source>

The implementation of <code>file</code> and <code>example_without_RAII()</code> becomes more complex if <code>fopen()</code> and <code>fclose()</code> could potentially throw exceptions; <code>example_with_RAII()</code> would be unaffected, however.

The essence of the RAII idiom is that the class <code>file</code> encapsulates the management of any finite resource, like the <code>FILE*</code> file handle. It guarantees that the resource will properly be disposed of at function exit. Furthermore, <code>file</code> instances guarantee that a valid log file is available (by throwing an exception if the file could not be opened).

There's also a big problem in the presence of exceptions: in <code>example_without_RAII()</code>, if more than one resource were allocated, but an exception was to be thrown between their allocations, there's no general way to know which resources need to be released in the final <code>catch</code> block - and releasing a not-allocated resource is usually a bad thing. RAII takes care of this problem; the automatic variables are destructed in the reverse order of their construction, and an object is only destructed if it was fully constructed (no exception was thrown inside its constructor). So <code>example_without_RAII()</code> can never be as safe as <code>example_with_RAII()</code> without special coding for each situation, such as checking for invalid default values or nesting try-catch blocks.  Indeed, it should be noted that <code>example_without_RAII()</code> contained resource bugs in previous versions of this article.

This frees <code>example_with_RAII()</code> from explicitly managing the resource as would otherwise be required. When several functions use <code>file</code>, this simplifies and reduces overall code size and helps ensure program correctness.

<code>example_without_RAII()</code> resembles the idiom used for resource management in non-RAII languages such as Java. While Java's ''try-finally'' blocks allow for the correct release of resources, the burden nonetheless falls on the programmer to ensure correct behavior, as each and every function using <code>file</code> may explicitly demand the destruction of the log file with a ''try-finally'' block.

[[Category:C++ Programming|{{SUBPAGENAME}}]]
