>__TOC__
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">__TOC__
{{TODO|
*Add missing information '''Relational''' operators.
|C++ Programming}}

=== Logical operators ===
The operators <tt>'''and'''</tt> (can also be written as '''&&''') and <tt>'''or'''</tt> (can also be written as '''||''') allow two or more conditions to be chained together. The <tt>'''and'''</tt> operator checks whether all conditions are true and the <tt>'''or'''</tt> operator checks whether at least one of the conditions is true. Both operators can also be mixed together in which case the order in which they appear from left to right, determines how the checks are performed.  Older versions of the C++ standard used the keywords '''&&''' and '''||''' in place of <tt>'''and'''</tt> and <tt>'''or'''</tt>. Both operators are said to ''short circuit''. If a previous <tt>'''and'''</tt> condition is false, later conditions are not checked. If a previous <tt>'''or'''</tt> condition is true later conditions are not checked.

{{NOTE|The iso646.h header file is part of the C standard library, since 1995, as an amendment to the C90 standard. It defines a number of macros which allow programmers to use C language bitwise and logical operators in textual form, which, without the header file, cannot be quickly or easily typed on some international and non-QWERTY keyboards.
These symbols are keywords in the ISO C++ programming language and do not require the inclusion of a header file. For consistency, however, the C++98 standard provides the header <ciso646>. On MS Visual Studio that historically implements nonstandard language extensions this is the only way to enable these keywords (via macros) without disabling the extensions.}} 

The <tt>'''not'''</tt> (can also be written as '''!''') operator is used to return the inverse of one or more conditions.

* '''Syntax''':
 ''condition1 ''<b>and</b>'' condition2''
 ''condition1 ''<b>or</b>'' condition2''
 <b>not</b> ''condition''
 
* '''Examples''':


When something should not be true. It is often combined with other conditions. If x>5 but not x = 10, it would be written:

<source lang=cpp>
if ((x > 5) and not (x == 10)) // if (x greater than 5) and ( not (x equal to 10) ) 
{
  //...code...
}
</source>

When all conditions must be true. If x must be between 10 and 20:

<source lang=cpp>
if (x > 10 and x < 20) // if x greater than 10 and x less than 20
{
  //....code...
}
</source>

When at least one of the conditions must be true. If x must be equal to 5 or equal to 10 or less than 2:

<source lang=cpp>
if (x == 5 or x == 10 or x < 2) // if x equal to 5 or x equal to 10 or x less than 2
{
  //...code...
}
</source>

When at least one of a group of conditions must be true. If x must be between 10 and 20 or between 30 and 40.

<source lang=cpp>
if ((x >= 10 and x <= 20) or (x >= 30 and x <= 40)) // >= -> greater or equal etc...
{
  //...code...
}
</source>

Things get a bit more tricky with more conditions. The trick is to make sure the parenthesis are in the right places to establish the order of thinking intended. However, when things get this complex, it can often be easier to split up the logic into nested if statements, or put them into bool variables, but it is still useful to be able to do things in complex boolean logic.

Parenthesis around <tt>x > 10</tt> and around <tt>x < 20</tt> are implied, as the <tt><</tt> operator has a higher precedence than <tt>'''and'''</tt>. First <tt>x</tt> is compared to 10. If <tt>x</tt> is greater than 10, <tt>x</tt> is compared to 20, and if <tt>x</tt> is also less than 20, the code is executed.


==== and (&&) ====
{| border="1" cellpadding="1" cellspacing="0" style="text-align:center;"
|+
! style="width:35px;background:#aaaaaa;" | ''statement1''
! style="width:35px;background:#aaaaaa;" | ''statement2''
! style="width:35px" | and
|-
| T  || T  || T
|-
| T  || F  || F
|-
| F  || T  || F
|-
| F  || F  || F
|}

The logical AND operator, '''and''', compares the left value and the right value. If both ''statement1'' and ''statement2'' are true, then the expression returns TRUE. Otherwise, it returns FALSE.

<source lang=cpp>
if ((var1 > var2) and (var2 > var3))
{
  std::cout << var1 " is bigger than " << var2 << " and " << var3 << std::endl;
}
</source>

In this snippet, the '''if''' statement checks to see if ''var1'' is greater than ''var2''. Then, it checks if ''var2'' is greater than ''var3''. If it is, it proceeds by telling us that ''var1'' is bigger than both ''var2'' and ''var3''.

{{NOTE|note=The logical AND operator '''and''' is sometimes written as '''&&''', which is not the same as the address operator and the bitwise AND operator, both of which are represented with '''&'''}}

==== or (||)====
{| border="1" cellpadding="1" cellspacing="0" style="text-align:center;"
|+
! style="width:35px;background:#aaaaaa;" | ''statement1''
! style="width:35px;background:#aaaaaa;" | ''statement2''
! style="width:35px" | or
|-
| T  || T  || T
|-
| T  || F  || T
|-
| F  || T  || T
|-
| F  || F  || F
|}

The logical OR operator is represented with '''or'''. Like the logical AND operator, it compares ''statement1'' and ''statement2''. If either ''statement1'' or ''statement2'' are true, then the expression is true. The expression is also true if both of the statements are true.

<source lang=cpp>
if ((var1 > var2) or (var1 > var3))
{
  std::cout << var1 " is either bigger than " << var2 << " or " << var3 << std::endl;
}
</source>

Let's take a look at the previous expression with an OR operator. If ''var1'' is bigger than either ''var2'' or ''var3'' or both of them, the statements in the '''if''' expression are executed. Otherwise, the program proceeds with the rest of the code.

==== not (!)  ====
The logical NOT operator, '''not''', returns TRUE if the statement being compared is not true. Be careful when you're using the NOT operator, as well as any logical operator. 

<source lang=cpp>
not x > 10
</source>

The logical expressions have a higher precedence than normal operators. Therefore, it compares whether "not x" is greater than 10. However, this statement always returns false, no matter what "x" is. That's because the logical expressions only return boolean values(1 and 0).
{{BookCat}}
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
