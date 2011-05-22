>=== Generic wrappers ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== Generic wrappers ===
Generic GUI/API wrappers are programming libraries that provide a uniform platform neutral interface (API) to the operating system regardless of underlying platform. Such libraries greatly simplify development of cross-platform software.

Using a wrapper as a portability layer will offer applications some or all following benefits:

* Independence from the hardware.
* Independence from the Operating System.
** Independence from changes made to specific releases.
** Independence from API styles and error codes.

Cross-platform programming is more than only GUI programming. Cross-platform programming deals with the minimum requirements for the sections of code that aren't specified by the C++ Standard Language, so as programs can be compiled and run across different hardware platforms. 

Here is some cross-platform GUI toolkit:
* '''[[w:Gtkmm|Gtkmm]]''' - an interface for the C GUI library GTK+. It is not cross-platform by design, but rather mutli-platform i.e. can be used on many platform.
* '''[[w:Qt (toolkit)|Qt]]''' (http://qt.nokia.com) - a cross-platform (Qt is the basis for the Linux KDE desktop environment and supports the X Window System (Unix/X11), Apple Mac OS X, Microsoft Windows NT/9x/2000/XP/Vista/7 and the Symbian OS), it is an object-oriented '''application development''' framework, widely used for the development of GUI programs (in which case it is known as a widget toolkit), and for developing non-GUI programs such as console tools and servers. Used in numerous commercial applications such as Google Earth, Skype for Linux and Adobe Photoshop Elements. Released under the LGPL or a commercial license.
* '''[[w:WxWidgets|WxWidgets]]''' (http://www.wxwindows.org/) - a widget toolkit for creating graphical user interfaces (GUIs) for cross-platform applications on Win32, Mac OS X, GTK+, X11, Motif, WinCE, and more using one codebase. It can be used from languages such as C++, Python, Perl, and C#/.NET. Unlike other cross-platform toolkits, wxWidgets applications look and feel native. This is because wxWidgets uses the platform's own native controls rather than emulating them. It's also extensive, free, open-source, and mature. wxWidgets is more than a GUI development toolkit it provides classes for files and streams, application settings, multiple threads, interprocess communication, database access and more.
* '''[[w:Fltk|FLTK]]''' The "Fast, Light Toolkit"

[[Category:C++ Programming|{{SUBPAGENAME}}]]
