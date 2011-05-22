>==== C# ====
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">==== C# ====
'''[[Subject:C Sharp programming language|C#]]''' (pronounced "See Sharp") is a multi-purpose computer [[C++ Programming/Programming Languages|programming language]] catering to all development needs using [[w:.NET Framework|Microsoft .NET Framework]]. 

C#'s chief designer was Anders Hejlsberg. Before joining Microsoft in 1996, he worked at Borland developing Turbo Pascal and Delphi. At Microsoft he worked as an architect for J++ and he is still a key participant of the development of the .NET framework. 

C# is very similar to Java in that it takes the basic operators and style of C++ but forces programs to be type safe, in that it executes the code in a controlled sandbox called the virtual machine. As such, all code must be encapsulated inside an object, among other things. C# provides many additions to facilitate interaction with [[w:Microsoft|Microsoft]]'s Windows, COM, and Visual Basic. C# is a ECMA and ISO standard.

;Issues C# vs C++
*Limitation: With C#, features like multiple inheritance from classes (C# implements a different approach called Multiple Implementation, where a class can implement more than one interface), declaring objects on the stack, deterministic  destruction (allowing RAII) and allowing default arguments as function parameters will not be available. 
*Performance:  Applications build in C# may not perform as well when compared with native C++. C# has an intrusive garbage collector, reference tracking and other overheads with some of the framework services. The .NET framework has a big runtime footprint (as large as 30 Mb).
*Flexibility:  Due to the dependency on the .NET framework, operating system level functionality (system level APIs) are buffered by a generic set of functions that will reduce some freedoms.
*Runtime Redistribution:  Programs need to be distributed with the .NET framework (pre-Windows XP or non Windows Machines), similar to the issue with the Java language, with all the normal upgrade requirements attached.

There are several shortcomings to C++ which are resolved in C#.  One of the more subtle ones is the use of reference variables as function arguments.  When a code maintainer is looking at C++ source code, if a called function is declared in a header somewhere, the immediate code does not provide any indication that an argument to a function is passed as a reference. An argument passed by reference could be changed after calling the function whereas an argument passed by value cannot be changed. A maintainer not be familiar with the function looking for the location of an unexpected value change of a variable would additionally need to examine the header file for the function in order to determine whether or not that function could have changed the value of the variable.  C# insists that the '''ref''' keyword be placed in the function call (in addition to the function declaration), thereby cluing the maintainer in that the value could be changed by the function.

{{TODO|Should refer MS & MONO and Portable.NET [http://getdotgnu.com/pnet http://getdotgnu.com/pnet], as well as the ECMA and ISO standards for C# and CLI|C++ Programming}}

{{BookCat}}
