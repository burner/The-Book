>{{Haskell minitoc|chapter=Fun with Types}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{Haskell minitoc|chapter=Fun with Types}}

== Kinds for C++ users ==

* * is any concrete type, including functions. These all have kind *:
 type MyType = Int
 type MyFuncType = Int -> Int
 myFunc :: Int -> Int

 typedef int MyType;
 typedef int (*MyFuncType)(int);
 int MyFunc(int a);

* * -> * is a template that takes one type argument. It is like a function from types to types: you plug a type in and the result is a type. Confusion can arise from the two uses of MyData (although you can give them different names if you wish) - the first is a type constructor, the second is a data constructor. These are equivalent to a class template and a constructor respectively in C++. Context resolves the ambiguity - where Haskell expects a type (e.g. in a type signature) MyData is a type constructor, where a value, it is a data constructor.
 data MyData t -- type constructor with kind * -> *
               = MyData t -- data constructor with type a -> MyData a
 *Main> :k MyData
 MyData :: * -> *
 *Main> :t MyData
 MyData :: a -> MyData a
 
 template <typename t> class MyData
 {
    t member;
 };

* * -> * -> * is a template that takes two type arguments
 data MyData t1 t2 = MyData t1 t2

 template <typename t1, typename t2> class MyData
 {
    t1 member1;
    t2 member2;
    MyData(t1 m1, t2 m2) : member1(m1), member2(m2) { }
 };

* (* -> *) -> * is a template that takes one template argument of kind (* -> *)
 data MyData tmpl = MyData (tmpl Int)

 template <template <typename t> class tmpl> class MyData
 {
    tmpl<int> member1;
    MyData(tmpl<int> m) : member1(m) { }
 };

<div style="clear:both">
{{Haskell navigation|chapter=Fun with Types}}
</div>

{{BookCat}}
