>=== Scope ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== Scope ===
In any language, '''scope''' (the context; what is the background) has a high impact on  a given action or statement validity. The same is true in a programming language.

In a program we may have various constructs, may they be objects, variables or any other such. They come into existence from the point where you declare them (before they are declared they are unknown) and then, at some point, they are destroyed (as we will see there are many reasons to be so) and all are destroyed when your program terminates.

We will see [[C++ Programming/Programming Languages/C++/Code/Statements/Variables#Scope|that variables have a finite life-time when your program executes]], that the scope of an object or variable is simply that part of a program in which the variable name exists or is visible to the compiler.

==== Global scope ====
The default scope is defined as '''global scope''', this is commonly used to defines and use global variables or other global constructs (classes, structure, functions, etc...), this makes them valid and visible to the compiler at all times.

{{NOTE|
It is considered a good practice, if possible and as a way to reduce complexity and name collisions, to use a {{C++ Programming/kw|namespace}} scope for hiding the otherwise global elements, without removing their validity.
}}

==== Local scope ====
A '''local scope''' relates to the scope created inside a '''''[[C++ Programming/Programming Languages/C++/Code/Statements#Compound statement|compound statement]]'''''. 

{{NOTE|The only exceptional case is the {{C++ Programming/kw|for}} keyword. In that case the variables declared on the  {{C++ Programming/kw|for}} ''initialization'' section will be part of the local scope.}}

{{:C++ Programming/Programming Languages/C++/Code/Statements/Scope/Namespaces}}
{{BookCat}}
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
