>{{clear}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{clear}}
{{Haskell minitoc|chapter=Fun with Types}}

Phantom types are a way to embed a language with a stronger type system than Haskell's. <!--''FIXME: that's about all I know, and it's probably wrong. :) I'm yet to be convinced of PT's usefulness, I'm not sure they should have such a prominent position.'' [[User:DavidHouse|DavidHouse]] 17:42, 1 July 2006 (UTC)-->

== Phantom types ==
An ordinary type

 data T = TI Int | TS String
 
 plus :: T -> T -> T
 concat :: T -> T -> T

its phantom type version

 data T a = TI Int | TS String

Nothing's changed - just a new argument <code>a</code> that we don't touch.  But magic!
 
 plus :: T Int -> T Int -> T Int
 concat :: T String -> T String -> T String

Now we can enforce a little bit more!

This is useful if you want to increase the type-safety of your code, but not impose additional runtime overhead:

 -- Peano numbers at the type level.
 data Zero = Zero
 data Succ a = Succ a
 -- Example: 3 can be modeled as the type
 -- Succ (Succ (Succ Zero)))
 
 data Vector n a = Vector [a] deriving (Eq, Show)
 
 vector2d :: Vector (Succ (Succ Zero)) Int
 vector2d = Vector [1,2]
 
 vector3d :: Vector (Succ (Succ (Succ Zero))) Int
 vector3d = Vector [1,2,3]
 
 -- vector2d == vector3d raises a type error
 -- at compile-time:
 
 --   Couldn't match expected type `Zero'
 --          against inferred type `Succ Zero'
 --     Expected type: Vector (Succ (Succ Zero)) Int
 --     Inferred type: Vector (Succ (Succ (Succ Zero))) Int
 --   In the second argument of `(==)', namely `vector3d'
 --   In the expression: vector2d == vector3d
 
 -- while vector2d == Vector [1,2,3] works

{{Haskell stub}}
{{Haskell navigation|chapter=Fun with Types}}
{{Auto category}}
