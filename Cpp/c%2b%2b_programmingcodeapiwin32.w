>=== The Windows 32 API ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== The Windows 32 API ===
''Win32 API'' is a set of functions defined in the ''Windows OS'', in other words it is the '''Windows API''', this is the name given by ''Microsoft'' to the core set of [[w:application programming interface|application programming interface]]s available in the [[w:Microsoft Windows|Microsoft Windows]] [[w:operating systems|operating systems]]. It is designed for usage by [[w:C|C]]/[[w:C Plus Plus|C++]] programs and is the most direct way to interact with a ''Windows'' system for [[w:Application software|software applications]]. Lower level access to a ''Windows'' system, mostly required for [[w:device drivers|device drivers]], is provided by the [[w:Windows Driver Model|Windows Driver Model]] in current versions of ''Windows''.

One can get more information about the ''API'' and support from ''Microsoft'' itself, using the MSDN Library ( http://msdn.microsoft.com/ ) essentially a resource for developers using Microsoft tools, products, and technologies. It contains a bounty of technical programming information, including sample code, documentation, technical articles, and reference guides. You can also check out Wikibooks [[Windows Programming]] book for some more detailed information that goes beyond the scope of this book.

A [[w:software development kit|software development kit]] (SDK) is available for ''Windows'', which provides documentation and tools to enable developers to create software using the ''Windows API'' and associated ''Windows'' technologies.
( http://www.microsoft.com/downloads/ )

; History 
The Windows API has always exposed a large part of the underlying structure of the various Windows systems for which it has been built to the programmer. This has had the advantage of giving Windows programmers a great deal of flexibility and power over their applications. However, it also has given Windows applications a great deal of responsibility in handling various low-level, sometimes tedious, operations that are associated with a [[w:Graphical user interface|Graphical user interface]].

[[w:Charles Petzold|Charles Petzold]], writer of various well read Windows API books, has said: ''"The original hello-world program in the Windows 1.0 SDK was a bit of a scandal. HELLO.C was about 150 lines long, and the HELLO.RC resource script had another 20 or so more lines. (...) Veteran C programmers often curled up in horror or laughter when encountering the Windows hello-world program."''. A [[w:hello world program|hello world program]] is a often used programming example, usually designed to show the easiest possible application on a system that can actually do something (i.e. print a line that says "Hello World").

Over the years, various changes and additions were made to the Windows Operating System, and the Windows API changed and grew to reflect this. The windows API for [[w:Windows 1.0|Windows 1.0]] supported less than 450 [[w:Subroutine|function calls]], where in modern versions of the Windows API there are thousands. In general, the interface has remained fairly consistent however, and a old Windows 1.0 application will still look familiar to a programmer who is used to the modern Windows API.

A large emphasis has been put by [[w:Microsoft|Microsoft]] on maintaining software [[w:backwards compatibility|backwards compatibility]]. To achieve this, Microsoft sometimes went as far as supporting software that was using the API in a undocumented or even (programmatically) illegal way. [[w:Raymond Chen|Raymond Chen]], a Microsoft developer who works on the Windows API, has said that he ''"could probably write for months solely about bad things apps do and what we had to do to get them to work again (often in spite of themselves). Which is why I get particularly furious when people accuse Microsoft of maliciously breaking applications during OS upgrades. If any application failed to run on Windows 95, I took it as a personal failure."''

==== Variables and <u>Win32</u>====
'''Win32''' uses an extended set of data types, using C's typedef mechanism These include:
*BYTE - {{C++ Programming/kw|unsigned}} 8 bit integer.
*DWORD -	32 bit {{C++ Programming/kw|unsigned}} integer.
*LONG - 32 bit signed integer.
*LPDWORD - 32 bit pointer to DWORD.
*LPCSTR - 32 bit pointer to constant character string.
*LPSTR -	32 bit pointer to character string.
*UINT - 32 bit {{C++ Programming/kw|unsigned}} int.
*WORD - 16 bit {{C++ Programming/kw|unsigned}} int.
*HANDLE - opaque pointer to system data.

Of course standard data types are also available when programming with '''Win32 API'''.

==== Windows Libraries (DLLs) ====
In Windows, library code exists in  a number of forms, and can be accessed in various ways.

Normally, the only thing that is needed is to include in the appropriate header file on the source code the information to the compiler, and linking  to the .lib file will occur during the linking phase.

This .lib file either contains code which is to be statically linked into compiled object code
or contains code to allow access to a dynamically link to a binary library(.DLL) on the system.

It is also possible to generate a binary library .DLL within C++ by including appropriate
information such as an import/export table when compiling and linking.

DLLs stand for Dynamic Link Libraries, the basic file of functions that are used in some programs. Many newer C++ IDEs such as Dev-CPP support such libraries.

Common libraries on Windows include those provided by the Platform Software Development Kit,
Microsoft Foundation Class and a C++ interface to .Net Framework assemblies.

Although not strictly use as library code, the Platform SDK and other libraries provide a set of standardized interfaces to objects accessible via the Common Object Model implemented as part of 
Windows. 

{{TODO| Add '''Registry''', '''IO (Input/Output)''', '''Security''', '''Processes and Threads'''|C++ Programming}}

==== API conventions and Win32 API Functions (by focus) ====

===== Time =====
----
Time measurement has to come from the OS in relation to the hardware it is run, unfortunately  most computers don't have a standard high-accuracy, high-precision time clock that is also quick to access.

MSDN Time Functions ( http://msdn.microsoft.com/library/default.asp?url=/library/en-us/sysinfo/base/time_functions.asp )

Timer Function Performance ( http://developer.nvidia.com/object/timer_function_performance.html )

'''GetTickCount''' has a precision (dependent on your timer tick rate) of one millisecond, its accuracy typically within a 10-55ms expected error, the best thing is that it increments at a constant rate. (''WaitForSingleObject'' uses the same timer).

'''GetSystemTimeAsFileTime''' has a precision of 100-nanoseconds, its accuracy is similar to ''GetTickCount''.

'''QueryPerformanceCounter''' can be slower to obtain but has higher accuracy, uses the HAL (with some help from ACPI) a problem with it is that it can travel back in time on over-clocked PCs due to garbage on the LSBs, note that  the functions fail unless the supplied LARGE_INTEGER is DWORD aligned.

Performance counter value may unexpectedly leap forward ( http://support.microsoft.com/default.aspx?scid=KB;EN-US;Q274323& )

'''timeGetTime''' (via winmm.dll) has a precision of ~5ms.

===== File System =====
----
'''MakeSureDirectoryPathExists''' (via Image Help Library - IMAGHLP.DLL, <tt>#pragma comment( lib, "imagehlp.lib" )</tt>, <tt>#include <imagehlp.h></tt> ) creates directories, only useful to create/force the existence of a given dir tree or multiple directories, or if the linking is already present, note that it is single threaded.

{{TODO| Add '''Basics in building "windows"''', '''Window eventhandling''', '''Resources'''|C++ Programming}}

==== Resources ====
----
Resources are perhaps one of the most useful elements of the WIN32 API, they are how we program menu's, add icons, backgrounds, music and many more aesthetically pleasing elements to our programs. 

They are defined in a .rc file (resource c) and are included at the linking phase of compile. Resource files work hand in hand with a header file (usually called resource.h) which carries the definitions of each ID.

For example a simple RC file might contain a menu:

<table>
//////////////
IDR_MYMENU MENU<br />
BEGIN<br />
    POPUP "&File"<br />
    BEGIN<br />
         MENUITEM "&About", ID_FILE_ABOUT<br />
        MENUITEM "E&xit", ID_FILE_EXIT<br />
    END<br />
<br />
    POPUP "&Edit"<br />
    BEGIN<br />
    // Insert menu here :p<br />
    END<br />
    <br />
    POPUP "&Links"<br />
    BEGIN<br />
        MENUITEM "&Visit Lukem_95's Website", ID_LINK_WEBSITE<br />
        MENUITEM "G&oogle.com", ID_LINK_GOOGLE<br />
END<br />
END<br />
//////////////
</table>

And the corresponding H file:

<nowiki>#define</nowiki> IDR_MYMENU 9000<br />
<nowiki>#define</nowiki> ID_FILE_EXIT 9001<br />
<nowiki>#define</nowiki> ID_LINK_WEBSITE 9002<br />
<nowiki>#define</nowiki> ID_LINK_GOOGLE 9003<br /><nowiki>#define</nowiki> ID_FILE_ABOUT 9004<br />

==== Network ====
Network applications are often built in C++ on windows utilizing the WinSock API functions.

[[Category:C++ Programming|{{SUBPAGENAME}}]]
