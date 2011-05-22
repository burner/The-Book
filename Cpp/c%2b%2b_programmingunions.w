>== {{C++ Programming/kw|union}} ==
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">== {{C++ Programming/kw|union}} ==
{{:C++ Programming/Programming Languages/C++/Code/Keywords/union}}

=== Writing to Different Bytes ===

Unions are very useful for low-level programming tasks that involve writing to the same memory area but at different portions of the allocated memory space, for instance:

<source lang=cpp>
union item {
  // The item is 16-bits
  short theItem;
  // In little-endian lo accesses the low 8-bits -
  // hi, the upper 8-bits
  struct { char lo; char hi; } portions;
};
</source>

{{NOTE|A name for the struct declared in item can be omitted because it is not used. All that needs to be explicitly named is the parts that we intend to access, namely the instance itself, portions.}}

<source lang=cpp>
  item tItem;
  tItem.theItem = 0xBEAD;
  tItem.portions.lo = 0xEF; // The item now equals 0xBEEF
</source>

Using this union we can modify the low-order or high-order bytes of theItem without disturbing any other bytes.

=== Example in Practice: SDL Events ===
One real-life example of unions is the event system of SDL, a graphics library in C. In graphical programming, an event is an action triggered by the user, such as a mouse move or keyboard press. One of the SDL's responsibilities is to handle events and provide a mechanism for the programmer to listen for and react to them.

{{NOTE|The following section deals with a library in C rather than C++, so some features, such as methods of objects, are not used here. However C++ is more-or-less a superset of C, so you can understand the code with the knowledge you have gained so far.}}

<source lang=cpp>
// primary event structure in SDL

typedef union {
  Uint8 type;
  SDL_ActiveEvent active;
  SDL_KeyboardEvent key;
  SDL_MouseMotionEvent motion;
  SDL_MouseButtonEvent button;
  SDL_JoyAxisEvent jaxis;
  SDL_JoyBallEvent jball;
  SDL_JoyHatEvent jhat;
  SDL_JoyButtonEvent jbutton;
  SDL_ResizeEvent resize;
  SDL_ExposeEvent expose;
  SDL_QuitEvent quit;
  SDL_UserEvent user;
  SDL_SysWMEvent syswm;
} SDL_Event;
</source>

Each of the types other than <tt>Uint8</tt> (an 8-bit {{C++ Programming/kw|unsigned}} integer) is a struct with details for that particular event.  

<source lang=cpp>
// SDL_MouseButtonEvent

typedef struct{
  Uint8 type;
  Uint8 button;
  Uint8 state;
  Uint16 x, y;
} SDL_MouseButtonEvent;
</source>

When the programmer receives an event from SDL, he first checks the type value. This tells him what kind of an event it is. Based on this value, he either ignores the event or gets more information by getting the appropriate part of the union.

For example, if the programmer received an event in <tt>SDL_Event ev</tt>, he could react to mouse clicks with the following code.

<source lang=cpp>
if (ev.type == SDL_MOUSEBUTTONUP && ev.button.button == SDL_BUTTON_RIGHT) {
  cout << "You have right-clicked at coordinates (" << ev.button.x << ", "
       << ev.button.y << ")." << endl;
}
</source>

{{NOTE|As each of the SDL_SomethingEvent structs contain a <tt>Uint8 type</tt> entry, it is safe to access both <tt>Uint8 type</tt> and the corresponding sub-struct together.}}

While identical functionality can be provided with a struct rather than a union, the union is far more space efficient; the struct would use memory for each of the different event types, whereas the union only uses memory for one. As only one entry has meaning per instance, it is reasonable to use a union in this case.

This scheme could also be constructed with polymorphism and inheritance features of object-oriented C++, however the setup would be involved and less efficient than this one. Use of unions loses type safety, however it gains in performance.

=== this ===
The this keyword is a implicitly created pointer that is only accessible within nonstatic member functions of a union (or a struct or class ) and points to the object for which the member function is called. The this pointer is not available in static member functions. This will be restated again on when introducing unions a more in depth analysis is provided in the [[C++ Programming/Classes/this|Section about classes]]. 

[[Category:C++ Programming|{{SUBPAGENAME}}]]
