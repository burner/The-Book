>{{Haskell minitoc}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc}}

Haskell has at least four toolkits for programming a graphical interface:
* [[:w:WxHaskell|wxHaskell]] - provides a Haskell interface to the wxWidgets toolkit 
* [http://www.haskell.org/haskellwiki/Gtk2Hs Gtk2Hs] - provides a Haskell interface to the GTK+ library
* [http://code.google.com/p/hoc/ hoc] (documentation at [http://hoc.sourceforge.net/ sourceforge]) - provides a Haskell to Objective-C binding which allows users to access to the Cocoa library on MacOS X
* [http://qthaskell.berlios.de/ qtHaskell] - provides a set of Haskell bindings for the Qt Widget Library from Nokia 

In this tutorial, we will focus on the wxHaskell toolkit, as it allows you to produce a native graphical interface on all platforms that wxWidgets is available on, including Windows, Linux and MacOS X.

==Getting and running wxHaskell==

To install wxHaskell, you'll need to use [http://haskell.org/ghc/ GHC]. Then, download the wxHaskell package from [http://hackage.haskell.org/cgi-bin/hackage-scripts/package/wx Hackage] using 
 sudo cabal install wxcore --global
 cabal install wx

or the [http://wxhaskell.sourceforge.net/download.html wxHaskell download page] and follow the installation instructions provided on the wxHaskell download page. Don't forget to register wxHaskell with GHC, or else it won't run. To compile source.hs (which happens to use wxHaskell code), open a command line and type:

 ghc -package wx source.hs -o bin

Code for GHCi is similar:

 ghci -package wx

You can then load the files from within the GHCi interface. To test if everything works, go to $wxHaskellDir/samples/wx ($wxHaskellDir is the directory you installed it in) and load (or compile) HelloWorld.hs. It should show a window with title "Hello World!", a menu bar with File and About, and a status bar at the bottom, that says "Welcome to wxHaskell".

If it doesn't work, you might try to copy the contents of the $wxHaskellDir/lib directory to the ghc install directory.

==Hello World==

Here's the basic Haskell "Hello World" program:

 module Main where
 
 main :: IO ()
 main = putStr "Hello World!"

It will compile just fine, but it isn't really fancy. We want a nice GUI! So how to do this? First, you must import <tt>Graphics.UI.WX</tt>. This is the wxHaskell library. <tt>Graphics.UI.WXCore</tt> has some more stuff, but we won't be needing that now.

To start a GUI, use (guess what) <tt>start gui</tt>. In this case, <tt>gui</tt> is the name of a function which we'll use to build the interface. It must have an IO type. Let's see what we have:

 module Main where
 
 import Graphics.UI.WX
 
 main :: IO ()
 main = start gui
 
 gui :: IO ()
 gui = do
   --GUI stuff

To make a frame, we use <tt>frame</tt>. Check the type of <tt>frame</tt>. It's <tt>[Prop (Frame ())] -> IO (Frame ())</tt>. It takes a list of "frame properties" and returns the corresponding frame. We'll look deeper into properties later, but a property is typically a combination of an attribute and a value. What we're interested in now is the title. This is in the <tt>text</tt> attribute and has type <tt>(Textual w) => Attr w String</tt>. The most important thing here, is that it's a <tt>String</tt> attribute. Here's how we code it:

 gui :: IO ()
 gui = do
   frame [text := "Hello World!"]

The operator <tt>(:=)</tt> takes an attribute and a value, and combines both into a property. Note that <tt>frame</tt> returns an <tt>IO (Frame ())</tt>. You can change the type of <tt>gui</tt> to <tt>IO (Frame ())</tt>, but it might be better just to add <tt>return ()</tt>. Now we have our own GUI consisting of a frame with title "Hello World!". Its source:



 module Main where
 
 import Graphics.UI.WX
 
 main :: IO ()
 main = start gui
 
 gui :: IO ()
 gui = do
   frame [text := "Hello World!"]
   return ()

The result should look like the screenshot.  (It might look slightly different on Linux or MacOS X, on which wxhaskell also runs)

==Controls==
{{Info|From here on, its good practice to keep a browser window or tab open with the [http://wxhaskell.sourceforge.net/doc/ wxHaskell documentation]. It's also available in $wxHaskellDir/doc/index.html.''}}

===A text label===

Simply a frame doesn't do much. In this chapter, we're going to add some more elements. Let's start with something simple: a label. wxHaskell has a <tt>label</tt>, but that's a layout thing. We won't be doing layout until next chapter. What we're looking for is a <tt>staticText</tt>. It's in <tt>Graphics.UI.WX.Controls</tt>. As you can see, the <tt>staticText</tt> function takes a <tt>Window</tt> as argument, and a list of properties. Do we have a window? Yup! Look at <tt>Graphics.UI.WX.Frame</tt>. There we see that a <tt>Frame</tt> is merely a type-synonym of a special sort of window. We'll change the code in <tt>gui</tt> so it looks like this:

[[Image:WxHaskell2_winxp.png|thumb|Hello StaticText! (winXP)]]

 gui :: IO ()
 gui = do
   f <- frame [text := "Hello World!"]
   staticText f [text := "Hello StaticText!"]
   return ()

Again, <tt>text</tt> is an attribute of a <tt>staticText</tt> object, so this works. Try it!

===A button===

Now for a little more interaction. A button. We're not going to add functionality to it until the chapter about events, but at least something visible will happen when you click on it.

A <tt>button</tt> is a control, just like <tt>staticText</tt>. Look it up in <tt>Graphics.UI.WX.Controls</tt>.

Again, we need a window and a list of properties. We'll use the frame again. <tt>text</tt> is also an attribute of a button:

[[Image:WxHaskell3_winxp.png|thumb|Overlapping button and StaticText (winXP)]]

 gui :: IO ()
 gui = do
   f <- frame [text := "Hello World!"]
   staticText f [text := "Hello StaticText!"]
   button f [text := "Hello Button!"]
   return ()

Load it into GHCi (or compile it with GHC) and... hey!? What's that? The button's been covered up by the label! We're going to fix that next, in the layout chapter.

==Layout==

The reason that the label and the button overlap, is that we haven't set a ''layout'' for our frame yet. Layouts are created using the functions found in the documentation of <tt>Graphics.UI.WXCore.Layout</tt>. Note that you don't have to import <tt>Graphics.UI.WXCore</tt> to use layouts.

The documentation says we can turn a member of the widget class into a layout by using the <tt>widget</tt> function. Also, windows are a member of the widget class. But, wait a minute... we only have one window, and that's the frame! Nope... we have more, look at <tt>Graphics.UI.WX.Controls</tt> and click on any occurrence of the word ''Control''. You'll be taken to <tt>Graphics.UI.WXCore.WxcClassTypes</tt> and it is here we see that a Control is also a type synonym of a special type of window. We'll need to change the code a bit, but here it is.

 gui :: IO ()
 gui = do
   f <- frame [text := "Hello World!"]
   st <- staticText f [text := "Hello StaticText!"]
   b <- button f [text := "Hello Button!"]
   return ()

Now we can use <tt>widget st</tt> and <tt>widget b</tt> to create a layout of the staticText and the button. <tt>layout</tt> is an attribute of the frame, so we'll set it here:

[[Image:WxHaskell4_winxp.png|thumb|StaticText with layout (winXP)]]

 gui :: IO ()
 gui = do
   f <- frame [text := "Hello World!"]
   st <- staticText f [text := "Hello StaticText!"]
   b <- button f [text := "Hello Button!"]
   set f [layout := widget st]
   return ()

The <tt>set</tt> function will be covered in the chapter about attributes. Try the code, what's wrong? This only displays the staticText, not the button. We need a way to combine the two. We will use ''layout combinators'' for this. <tt>row</tt> and <tt>column</tt> look nice. They take an integer and a list of layouts. We can easily make a list of layouts of the button and the staticText. The integer is the spacing between the elements of the list. Let's try something:

[[Image:WxHaskell5_row_winxp.png|thumb|A row layout (winXP)]]
[[Image:WxHaskell5_col_winxp.PNG|thumb|Column layout with a spacing of 25 (winXP)]]

 gui :: IO ()
 gui = do
   f <- frame [text := "Hello World!"]
   st <- staticText f [text := "Hello StaticText!"]
   b <- button f [text := "Hello Button!"]
   set f [layout := 
           row 0 [widget st, widget b]
         ]
   return ()

Play around with the integer and see what happens, also change <tt>row</tt> into <tt>column</tt>. Try to change the order of the elements in the list to get a feeling of how it works. For fun, try to add <tt>widget b</tt> several more times in the list. What happens?

Here are a few exercises to spark your imagination. Remember to use the documentation!


{{Exercises|1=
# Add a checkbox control. It doesn't have to do anything yet, just make sure it appears next to the staticText and the button when using row-layout, or below them when using column layout. <tt>text</tt> is also an attribute of the checkbox.
# Notice that <tt>row</tt> and <tt>column</tt> take a list of ''layouts'', and also generates a layout itself. Use this fact to make your checkbox appear on the left of the staticText and the button, with the staticText and the button in a column.
# Can you figure out how the radiobox control works? Take the layout of the previous exercise and add a radiobox with two (or more) options below the checkbox, staticText and button. Use the documentation!
# Use the <tt>boxed</tt> combinator to create a nice looking border around the four controls, and another one around the staticText and the button. (''Note: the <tt>boxed</tt> combinator might not be working on MacOS X - you might get widgets that can't be interacted with.  This is likely just a bug in wxhaskell.'')}}


After having completed the exercises, the end result should look like this:
[[Image:WxHaskell6_winxp.png|thumb|Answer to exercises]]
You could have used different spacing for <tt>row</tt> and <tt>column</tt>, or the options of the radiobox are displayed horizontally.

==Attributes==

After all this, you might be wondering things like: "Where did that <tt>set</tt> function suddenly come from?", or "How would ''I'' know if <tt>text</tt> is an attribute of something?". Both answers lie in the attribute system of wxHaskell.

===Setting and modifying attributes===

In a wxHaskell program, you can set the properties of the widgets in two ways:
# during creation: <tt>f <- frame '''[ text := "Hello World!" ]'''</tt>
# using the <tt>set</tt> function: <tt>set f '''[ layout := widget st ]'''</tt>

The <tt>set</tt> function takes two arguments: one of any type <tt>w</tt>, and the other is a list of properties of <tt>w</tt>.  In wxHaskell, these will be the widgets and the properties of these widgets. Some properties can only be set during creation, like the <tt>alignment</tt> of a <tt>textEntry</tt>, but you can set most others in any IO-function in your program, as long as you have a reference to it (the <tt>f</tt> in <tt>set '''f''' [''stuff'']</tt>).

Apart from setting properties, you can also get them. This is done with the <tt>get</tt> function. Here's a silly example:

 gui :: IO ()
 gui = do
   f <- frame [ text := "Hello World!" ]
   st <- staticText f []
   ftext <- get f text
   set st [ text := ftext]
   set f [ text := ftext ++ " And hello again!" ]

Look at the type signature of <tt>get</tt>. It's <tt>w -> Attr w a -> IO a</tt>. <tt>text</tt> is a <tt>String</tt> attribute, so we have an <tt>IO String</tt> which we can bind to <tt>ftext</tt>. The last line edits the text of the frame. Yep, destructive updates are possible in wxHaskell. We can overwrite the properties using <tt>(:=)</tt> anytime with <tt>set</tt>. This inspires us to write a modify function:

 modify :: w -> Attr w a -> (a -> a) -> IO ()
 modify w attr f = do
   val <- get w attr
   set w [ attr := f val ]

First it gets the value, then it sets it again after applying the function. Surely we're not the first one to think of that...

And nope, we aren't. Look at this operator: <tt>(:~)</tt>. You can use it in <tt>set</tt>, because it takes an attribute and a function. The result is a property, in which the original value is modified by the function. This means we can write:

 gui :: IO ()
 gui = do
   f <- frame [ text := "Hello World!" ]
   st <- staticText f []
   ftext <- get f text
   set st [ text := ftext ]
   set f [ text :~ ++ " And hello again!" ]

This is a great place to use anonymous functions with the lambda-notation.

There are two more operators we can use to set or modify properties: <tt>(::=)</tt> and <tt>(::~)</tt>. They do the same as <tt>(:=)</tt> and <tt>(:~)</tt>, except a function of type <tt>w -> orig</tt> is expected, where <tt>w</tt> is the widget type, and <tt>orig</tt> is the original "value" type (<tt>a</tt> in case of <tt>(:=)</tt>, and <tt>a -> a</tt> in case of <tt>(:~)</tt>). We won't be using them now, though, as we've only encountered attributes of non-IO types, and the widget needed in the function is generally only useful in IO-blocks.

===How to find attributes===

Now the second question. Where did I read that <tt>text</tt> is an attribute of all those things? The easy answer is: in the documentation. Now where in the documentation to look for it?

Let's see what attributes a button has, so go to [http://hackage.haskell.org/packages/archive/wx/0.10.2/doc/html/Graphics-UI-WX-Controls.html <tt>Graphics.UI.WX.Controls</tt>], and click the link that says [http://hackage.haskell.org/packages/archive/wx/0.10.2/doc/html/Graphics-UI-WX-Controls.html#4 "Button"]. You'll see that a <tt>Button</tt> is a type synonym of a special kind of <tt>Control</tt>, and a list of functions that can be used to create a button. After each function is a list of "Instances". For the normal <tt>button</tt> function, this is ''Commanding -- Textual, Literate, Dimensions, Colored, Visible, Child, Able, Tipped, Identity, Styled, Reactive, Paint''. This is the list of classes of which a button is an instance. Read through the [[../Class declarations/]] chapter. It means that there are some class-specific functions available for the button. <tt>Textual</tt>, for example, adds the <tt>text</tt> and <tt>appendText</tt> functions. If a widget is an instance of the <tt>Textual</tt> class, it means that it has a <tt>text</tt> attribute!

Note that while <tt>StaticText</tt> hasn't got a list of instances, it's still a <tt>Control</tt>, which is a synonym for some kind of <tt>Window</tt>, and when looking at the <tt>Textual</tt> class, it says that <tt>Window</tt> is an instance of it. This is an error on the side of the documentation.

Let's take a look at the attributes of a frame. They can be found in <tt>Graphics.UI.WX.Frame</tt>. Another error in the documentation here: It says <tt>Frame</tt> instantiates <tt>HasImage</tt>. This was true in an older version of wxHaskell. It should say <tt>Pictured</tt>. Apart from that, we have <tt>Form</tt>, <tt>Textual</tt>, <tt>Dimensions</tt>, <tt>Colored</tt>, <tt>Able</tt> and a few more. We're already seen <tt>Textual</tt> and <tt>Form</tt>. Anything that is an instance of <tt>Form</tt> has a <tt>layout</tt> attribute.

<tt>Dimensions</tt> adds (among others) the <tt>clientSize</tt> attribute. It's an attribute of the <tt>Size</tt> type, which can be made with <tt>sz</tt>. Please note that the <tt>layout</tt> attribute can also change the size. If you want to use <tt>clientSize</tt> you should set it after the <tt>layout</tt>.

<tt>Colored</tt> adds the <tt>color</tt> and <tt>bgcolor</tt> attributes.

<tt>Able</tt> adds the Boolean <tt>enabled</tt> attribute. This can be used to enable or disable certain form elements, which is often displayed as a greyed-out option.

There are lots of other attributes, read through the documentation for each class.

==Events==

There are a few classes that deserve special attention. They are the <tt>Reactive</tt> class and the <tt>Commanding</tt> class. As you can see in the documentation of these classes, they don't add attributes (of the form <tt>Attr w a</tt>), but ''events''. The <tt>Commanding</tt> class adds the <tt>command</tt> event. We'll use a button to demonstrate event handling.

Here's a simple GUI with a button and a staticText:

[[Image:WxHaskell7_before_winxp.png|thumb|Before (winXP)]]

 gui :: IO ()
 gui = do
   f <- frame [ text := "Event Handling" ]
   st <- staticText f [ text := "You haven\'t clicked the button yet." ]
   b <- button f [ text := "Click me!" ]
   set f [ layout := column 25 [ widget st, widget b ] ]

We want to change the staticText when you press the button. We'll need the <tt>on</tt> function:

   b <- button f [ text := "Click me!"
                 , on command := --stuff
                 ]

The type of <tt>on</tt>: <tt>Event w a -> Attr w a</tt>. <tt>command</tt> is of type <tt>Event w (IO ())</tt>, so we need an IO-function. This function is called the ''Event handler''. Here's what we get:

<noinclude>
<!--
[[Image:WxHaskell7_after_winxp.png|thumb|After (winXP)]]
-->
</noinclude>

 gui :: IO ()
 gui = do
   f <- frame [ text := "Event Handling" ]
   st <- staticText f [ text := "You haven\'t clicked the button yet." ]
   b <- button f [ text := "Click me!"
                 , on command := set st [ text := "You have clicked the button!" ]
                 ]
   set f [ layout := column 25 [ widget st, widget b ] ]

''Insert text about event filters here''

{{Haskell navigation}}
{{Auto category}}
