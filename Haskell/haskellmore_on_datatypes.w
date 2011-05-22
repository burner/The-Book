>{{Haskell minitoc|chapter=Intermediate Haskell}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Intermediate Haskell}}

== Enumerations ==

One special case of the <tt>data</tt> declaration is the ''enumeration''.  This is simply a data type where none of the constructor functions have any arguments:

 data Month = January | February | March | April | May | June | July
              | August | September | October | November | December

You can mix constructors that do and do not have arguments, but it's only an enumeration if none of the constructors have
arguments. For instance,

 data Colour = Black | Red | Green | Blue | Cyan
             | Yellow | Magenta | White | RGB Int Int Int

The last constructor takes three arguments, so <tt>Colour</tt> is not an enumeration. As you will see further on when we discuss classes and derivation, this distinction is not only conceptual.

Incidentally, the definition of the <tt>Bool</tt> datatype is:

 data Bool = False | True
     deriving (Eq, Ord, Enum, Read, Show, Bounded)

== Named Fields (Record Syntax) ==

Consider a datatype whose purpose is to hold configuration settings.
Usually when you extract members from this type, you really only care
about one or possibly two of the many settings.  Moreover, if many of
the settings have the same type, you might often find yourself
wondering "wait, was this the fourth or ''fifth'' element?"  One
thing you could do would be to write accessor functions.  Consider the
following made-up configuration type for a terminal program:

<pre>
data Configuration =
    Configuration String          -- user name
                  String          -- local host
                  String          -- remote host
                  Bool            -- is guest?
                  Bool            -- is super user?
                  String          -- current directory
                  String          -- home directory
                  Integer         -- time connected
              deriving (Eq, Show)
</pre>
You could then write accessor functions, like (I've only listed a
few):

<pre>
getUserName (Configuration un _ _ _ _ _ _ _) = un
getLocalHost (Configuration _ lh _ _ _ _ _ _) = lh
getRemoteHost (Configuration _ _ rh _ _ _ _ _) = rh
getIsGuest (Configuration _ _ _ ig _ _ _ _) = ig
...
</pre>
You could also write update functions to update a single element.  Of
course, now if you add an element to the configuration, or remove one,
all of these functions now have to take a different number of
arguments.  This is highly annoying and is an easy place for bugs to
slip in.  However, there's a solution.  We simply give names to the
fields in the datatype declaration, as follows:

<pre>
data Configuration =
    Configuration { username      :: String,
                    localhost     :: String,
                    remotehost    :: String,
                    isguest       :: Bool,
                    issuperuser   :: Bool,
                    currentdir    :: String,
                    homedir       :: String,
                    timeconnected :: Integer
                  }
</pre>
This will automatically generate the following accessor functions for
us:

<pre>
username :: Configuration -> String
localhost :: Configuration -> String
...
</pre>
Moreover, it gives us very convenient update methods.  Here is a short
example for a "post working directory" and "change directory" like
functions that work on <code>Configuration</code>s:

<pre>
changeDir :: Configuration -> String -> Configuration
changeDir cfg newDir =
    -- make sure the directory exists
    if directoryExists newDir
      then -- change our current directory
           cfg{currentdir = newDir}
      else error "directory does not exist"

postWorkingDir :: Configuration -> String
  -- retrieve our current directory
postWorkingDir cfg = currentdir cfg
</pre>
So, in general, to update the field <code>x</code> in a datatype <code>y</code> to
<code>z</code>, you write <code>y{x=z}</code>.  You can change more than one; each
should be separated by commas, for instance, <code>y{x=z, a=b, c=d}</code>.

{{body note|1=

Those of you familiar with object-oriented languages might be tempted to, after all of this talk about "accessor functions" and "update methods", think of the <code>y{x=z}</code> construct as a setter method, which modifies the value of x in a pre-existing y. It is '''not''' like that - remember that in Haskell [[Haskell/Variables and functions#Variables|variables are immutable]]. Therefore, if, using the example above, you do something like 
<code>conf2 = changeDir conf1 "/opt/foo/bar"</code> 
conf2 will be defined as a Configuration which is just like conf1 except for having "/opt/foo/bar" as its currentdir, but conf1 will remain unchanged.
}}

=== It's only sugar ===

You can of course continue to pattern match against
<code>Configuration</code>s as you did before.  The named fields are simply
syntactic sugar; you can still write something like:

<pre>
getUserName (Configuration un _ _ _ _ _ _ _) = un
</pre>
But there is little reason to.  Finally, you can pattern match against
named fields as in:

<pre>
getHostData (Configuration {localhost=lh,remotehost=rh})
  = (lh,rh)
</pre>
This matches the variable <code>lh</code> against the <code>localhost</code> field on
the <code>Configuration</code> and the variable <code>rh</code> against the
<code>remotehost</code> field on the <code>Configuration</code>.  These matches of
course succeed.  You could also constrain the matches by putting
values instead of variable names in these positions, as you would for
standard datatypes.

You can create values of <code>Configuration</code> in the old way as shown in
the first definition below, or in the named-field's type, as shown in
the second definition below:
<pre>
initCFG =
    Configuration "nobody" "nowhere" "nowhere"
                  False False "/" "/" 0
initCFG' =
    Configuration
       { username="nobody",
         localhost="nowhere",
         remotehost="nowhere",
         isguest=False,
         issuperuser=False,
         currentdir="/",
         homedir="/",
         timeconnected=0 }
</pre>
The first way is much shorter, although the second is much more understandable (unless you
litter your code with comments).

== Parameterised Types ==

Parameterised types are similar to "generic" or "template" types in other languages.  A parameterised type takes one or more type parameters.  For example, the Standard Prelude type <tt>Maybe</tt> is defined as follows:

 data Maybe a = Nothing | Just a

This says that the type <tt>Maybe</tt> takes a type parameter <tt>a</tt>.  You can use this to declare, for example:

 lookupBirthday :: [Anniversary] -> String -> Maybe Anniversary

The <tt>lookupBirthday</tt> function takes a list of birthday records and a string and returns a <code>Maybe Anniversary</code>.  Typically, our interpretation is that if it finds the name then it will return <tt>Just</tt> the corresponding record, and otherwise, it will return <code>Nothing</code>.

You can parameterise <tt>type</tt> and <tt>newtype</tt> declarations in exactly the same way.  Furthermore you can combine parameterised types in arbitrary ways to construct new types.

=== More than one type parameter ===

We can also have more than one type parameter. An example of this is the <tt>Either</tt> type:

 data Either a b = Left a | Right b

For example:

 eitherExample :: Int -> Either Int String
 eitherExample a | even a = Left (a `div` 2)
                 | a `mod` 3 == 0 = Right "three"
                 | otherwise = Right "neither two nor three"
 
 otherFunction :: Int -> String
 otherFunction a = case eitherExample a of
   Left c -> "Even: " ++ show a ++ " = 2*" ++ show c ++ "."
   Right s -> show a ++ " is divisible by " ++ s ++ "."

In this example, when you call <tt>otherFunction</tt>, it'll return a <tt>String</tt>. If you give it an even number as argument, it'll say so, and give half of it. If you give it anything else, <tt>eitherExample</tt> will determine if it's divisible by three and pass it through to <tt>otherFunction</tt>.

=== Kind Errors ===

The flexibility of Haskell parameterised types can lead to errors in type declarations that are somewhat like type errors, except that they occur in the type declarations rather than in the program proper.  Errors in these "types of types" are known as "kind" errors.  You don't program with kinds: the compiler infers them for itself.  But if you get parameterised types wrong then the compiler will report a kind error.


{{Haskell navigation|chapter=Intermediate Haskell}}

[[Category:Haskell|More on datatypes]]
