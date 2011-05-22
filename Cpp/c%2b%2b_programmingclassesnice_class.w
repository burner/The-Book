>=== What is a "nice" class? ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== What is a "nice" class? ===

A "nice" class takes into consideration the use of the following functions:

1.  The copy constructor.<br>
2.  The assignment operator.<br>
3.  The equality operator.<br>
4.  The inequality operator.<br>

==== Class Declaration ====
<source lang=cpp>
class Nice
{
public:
    Nice(const Nice &Copy);
    Nice &operator= (const Nice &Copy);
    bool operator== (const Nice &param) const;
    bool operator!= (const Nice &param) const;
};
</source>

==== Description ====
A "nice" class could also be called a container safe class.  Many containers such as those in the [[C++ Programming/STL|Standard Template Library]] (STL), that we'll see later, use copy construction and the assignment operator when interacting with the objects of your class.  The assignment operator and copy constructor only need to be declared and defined if the default behavior, which is a member-wise (not binary) copy, is undesirable or insufficient to properly copy/construct your object.

A general rule of thumb is that if the default, member-wise copy operations do not work for your objects then you should define a suitable copy constructor and assignment operator.  They are both needed if either is defined.

==Copy Constructor==

The purpose of the copy constructor is to allow the programmer to perform the same instructions as the assignment operator with the special case of knowing that the caller is initializing/constructing rather than an copying.

It is also good practice to use the explicit keyword when using a copy constructor to prevent unintended implicit type conversion.

'''Example'''
<source lang=cpp>
class Nice
{
public:
  explicit Nice(int _a) : a(_a)
  {
    return;
  }
private:
  int a;
};

class NotNice
{
public:
  NotNice(int _a) :  a(_a)
  {
    return;
  }
private:
  int a;
};

int main()
{
  Nice proper = Nice(10); //this is ok
  Nice notproper = 10; //this will result in an error
  NotNice eg = 10; //this WILL compile, you may not have intended this conversion
  return 0;
}
</source>

==Equality Operator==
The equality operator says, "Is this object equal to that object?".  What constitutes equal is up to the programmer.  This is a requirement if you ever want to use the equality operator with objects of your class.

However, in most applications (e.g. mathematics), it is usually the case that coding the inequality is easier than coding the equality.  In which case the following code can be written for the equality.

<source lang=cpp>
inline bool Nice::operator== (const Nice& param) const
{
    return !(*this != param);
}
</source>

==Inequality Operator==
The inequality operator says, "Is this object not equal to that object?".  What constitutes not equal is up to the programmer.  This is a requirement if you ever want to use the inequality operator with objects of your class.

However, in some applications, coding the equality is easier than coding the inequality.  In which case the following code can be written for the inequality.

<source lang=cpp>
inline bool Nice::operator!= (const Nice& param) const
{
    return !(*this == param);
}
</source>

If the statement about the (in)equality operators having different efficiency (whatever kind) seems complete nonsense to you, consider that ''typically'', all object attributes must match for two objects to be considered equal.<br>  ''Typically'', only one object attribute must differ for two objects to be considered unequal.  For equality and inequality operators, that doesn't mean one is faster than the other.

Note, however, that using both the above equality and inequality functions as defined will result in an infinite recursive loop and care must be taken to use only one or the other.  Also, there are some situations where neither applies and therefore neither of the above can be used.

Given two objects A and B (with class attributes x and y), an equality operator could be written as

<source lang=cpp>
  if (A.x != B.x) return false;
  if (A.y != B.y) return false;
  return true;
</source>

while an inequality operator could be written as

<source lang=cpp>
  if (A.x != B.x) return true;
  if (A.y != B.y) return true;
  return false;
</source>

So yes, the equality operator can certainly be written ''...!(a!=b)...'', but it isn't any faster.  In fact, there's the additional overhead of a method call and a negation operation.

So the question becomes, is a little execution overhead worth the smaller code and improved maintainability?  There is no simple answer to this it all depend on how the programmer is using them. If your class is composed of, say, an array of 1 billion elements, the overhead is negligible.

[[Category:C++ Programming|{{SUBPAGENAME}}]]
