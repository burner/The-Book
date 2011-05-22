>== Libraries ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== Libraries ==
Libraries allow existing code to be reused in a program. Libraries are like programs except that instead of relying on '''main()''' to do the work you call the specific functions provided by the library to do the work. Functions provide the interface between the program being written and the library being used. This interface is called [[w:Application programming interface|''Application Programming Interface'']] or API.

Libraries should and tend to be domain specific as to permit greater mobility across applications, and provide extended specialization.
Libraries that are not, are often header only distribution, intended for static linking as to permit the compiler and the application, only to use the needed bits of code.

{{:C++ Programming/Code/API}}

As seen in the [[C++ Programming/Programming Languages/C++/Code/File Organization|File organization Section]], compiled libraries consists in C++ headers files that are included by the preprocessor and binary library files which are used by the linker to generate the resulting compilation. For a dynamically linked library, only the loading code is added to the compilation that uses them, the actual loading of the library is done in the memory at run-time.

Programs can make use of libraries in two forms, as static or dynamic depending on how the programmer decides to distribute its code or even due to the licensing used by third party libraries, the [[C++ Programming/Compiler/Linker/Libraries/Static and Dynamic Libraries|static and dynamic libraries]] section of this book will cover in depth this subject.

{{NOTE|As we will see when covering [[C++ Programming/Threading|multi-threading]] when selecting libraries. Remember to verify if they conform to the your requirements on that area.}} 

=== Third party libraries ===
Additional functionality that goes beyond the standard libraries (like [[C++ Programming/Compiler/Linker/Libraries/Garbage Collection|garbage collection]]) are available (often free) by third party libraries, but remember that third party libraries do not necessarily provide the same ubiquitous cross-platform functionality or an API style conformant with as standard libraries. The main motivation for their existence is for preventing one tho reinvent the wheel and to make efforts converge; too much energy has been spent by generations of programmers to write safe and "portable" code.

There are several libraries a the programmer is expected to know about or have at least a passing idea of what they are. Time, consistency and extended references will make a few libraries pop-out from the rest. One notable example is the highly respected collection of [[C++ Programming/Libraries/Boost|Boost libraries]] that we will examine ahead.

;Licensing on third party libraries
The programmer may also be limited by the requirements of the license used on external libraries that he has no direct control, for instance the use of the [[w:GNU General Public License|GNU General Public License]] (GNU GPL) code in closed source applications isn't permitted to address this issue the FSF provides an alternative in the form of the GNU LGPL license that permits such uses but only in the  dynamically linked form, this is mirrored by several other legal requirements a programmer must attend and comply to.

{{:C++ Programming/Compiler/Linker/Libraries/Static and Dynamic Libraries}}

[[Category:C++ Programming|{{SUBPAGENAME}}]]
