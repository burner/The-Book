>=== Conditional Operator ===
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">=== Conditional Operator ===
'''Conditional operators''' (also known as ternary operators) allow a programmer to check: if (x is more than 10 and eggs is less than 20 and x is not equal to a...).

Most operators compare two variables; the one to the left, and the one to the right. However, C++ also has a ternary operator (sometimes known as the conditional operator), '''?:''' which chooses from two expressions based on the value of a condition expression. The basic syntax is:

  ''condition-expression'' ? ''expression-if-true'' : ''expression-if-false''

If ''condition-expression'' is true, the expression returns the value of ''expression-if-true''. Otherwise, it returns the value of ''expression-if-false''. Because of this, the ternary operator can often be used in place of the '''if''' expression. 

{{NOTE|The use of the ternary operator versus the '''if''' expression often depends on the level of complexity and overall impact of the logical decision tree, using the '''if''' expression in convoluted or less than obvious situations should be preferred as it can not only be more clearly written but easier to understand, thus avoiding simple logical errors that would otherwise be hard to perceive.}}

*'''For example:'''
 '''int''' foo = 8;
 std::cout << "foo is " << (foo < 10 ? "smaller than" : "greater than or equal to") << " 10." << std::endl;
The output will be "foo is smaller than 10.".

{{TODO|Note the short-cut semantics of evaluation.  Note the conditions on the types of the expressions, and the conversions that will be applied if they have different types.|C++ Programming}}
{{BookCat}}
<noinclude>{{displaytitle|title=C++ Programming}}</noinclude>
