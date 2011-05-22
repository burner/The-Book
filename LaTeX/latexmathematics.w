><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

One of the greatest motivating forces for Donald Knuth when he began developing the original TeX system was to create something that allowed simple construction of mathematical formulas, whilst looking professional when printed. The fact that he succeeded was most probably why TeX (and later on, LaTeX) became so popular within the scientific community. Typesetting mathematics is one of LaTeX's greatest strengths. It is also a large topic due to the existence of so much mathematical notation.

If your document requires only a few simple mathematical formulas, plain LaTeX has most of the tools that you will need. If you are writing a scientific document that contains numerous complicated formulas, the {{LaTeX/Package|amsmath}} package<ref>http://www.ams.org/publications/authors/tex/amslatex</ref> introduces several new commands that are more powerful and flexible than the ones provided by LaTeX. The {{LaTeX/Package|mathtools}} package fixes some {{LaTeX/Package|amsmath}} quirks and adds some useful settings, symbols, and environments to amsmath<ref>http://www.tex.ac.uk/ctan/macros/latex/contrib/mh/mathtools.pdf</ref>.  To use either package, include:
<source lang="latex">
\usepackage{amsmath}
</source>
or
<source lang="latex">
\usepackage{mathtools}
</source>
in the preamble of the document.

== Mathematics environments ==

LaTeX needs to know beforehand that the subsequent text does in fact contain mathematical elements. This is because LaTeX typesets maths notation differently than normal text. Therefore, special environments have been declared for this purpose. They can be distinguished into two categories depending on how they are presented:

* ''text'' - text formulas are displayed in-line, that is, within the body of text where it is declared. e.g., I can say that ''a'' + ''a'' = 2''a'' within this sentence.
* ''displayed'' - displayed formulas are separate from the main text.

As maths require special environments, there are naturally the appropriate environment names you can use in the standard way. Unlike most other environments, however, there are some handy shorthands to declaring your formulas. The following table summarizes them:

{| class="wikitable"
! Type
! Environment
! LaTeX shorthand
! TeX shorthand
|-
| Text
| {{LaTeX/LaTeX|code=\begin{math}...\end{math} }}
| {{LaTeX/LaTeX|code=\(...\) }}
| {{LaTeX/LaTeX|code=$...$}}
|-
| Displayed
| {{LaTeX/LaTeX|code=\begin{displaymath}...\end{displaymath} }} or 
{{LaTeX/LaTeX|code=\begin{equation*}...\end{equation*} }}<ref name="amsmath"/>
| {{LaTeX/LaTeX|code=\[...\]}}
| {{LaTeX/LaTeX|code=$$...$$}}
|}
'''Note:''' Using the {{LaTeX/LaTeX|code=$$...$$}} should be avoided, as it may cause problems, particularly with the AMS-LaTeX macros. Furthermore, should a problem occur, the error messages may not be helpful.

Additionally, there is a second possible environment for the ''displayed'' type of formulas: {{LaTeX/Environment|equation}}. The difference between this and {{LaTeX/Environment|displaymath}} is that {{LaTeX/Environment|equation}} also adds sequential equation numbers by the side.

If you are typing text normally, you are said to be in ''text mode'', while you are typing within one of those mathematical environments, you are said to be in ''math mode'', that has some differences compared to the ''text mode'':
# Most spaces and line breaks do not have any significance, as all spaces are either derived logically from the mathematical expressions, or have to be specified with special commands such as {{LaTeX/LaTeX|code=\quad}}
# Empty lines are not allowed. Only one paragraph per formula.
# Each letter is considered to be the name of a variable and will be typeset as such. If you want to typeset normal text within a formula (normal upright font and normal spacing) then you have to enter the text using [[#Adding text to equations|dedicated commands.]]

=== Inserting "Displayed" maths inside blocks of text === 
In order for some operators, such as {{LaTeX/LaTeX|code=\lim}} or {{LaTeX/LaTeX|code=\sum}} to be displayed correctly inside some math environments (read {{LaTeX/LaTeX|code=$......$}}), it might be convenient to write the {{LaTeX/LaTeX|code=\displaystyle}} class inside the environment. Doing so might cause the line to be taller, but will cause exponents and indices to be displayed correctly for some math operators

== Symbols ==

Mathematics has lots and lots of symbols! If there is one aspect of maths that is difficult in LaTeX it is trying to remember how to produce them. There are of course a set of symbols that can be accessed directly from the keyboard:
 + - = ! / ( ) [ ] < > | ' :

Beyond those listed above, distinct commands must be issued in order to display the desired symbols. And there are ''a lot!'' of Greek letters, set and relations symbols, arrows, binary operators, etc. For example: 

{{LaTeX/Example|code=\[
 \forall x \in X, \quad \exists y \leq \epsilon
\]
|render=<math>
\forall x \in X, \quad \exists y \leq \epsilon
 \,</math>}}

Fortunately, there's a tool that can greatly simplify the search for the command for a specific symbol. Look for Detexify in the [[#External links|external links]] section below.

==Greek letters==

Greek letters are commonly used in mathematics, and they are very easy to type in ''math mode''. You just have to type the name of the letter after a backslash: if the first letter is lowercase, you will get a lowercase Greek letter, if the first letter is uppercase (and only the first letter), then you will get an uppercase letter. Note that some uppercase Greek letters look like Latin ones, so they are not provided by LaTeX (e.g. uppercase ''Alpha'' and ''Beta'' are just "A" and "B" respectively).
Lowercase epsilon, theta, phi, pi, rho, and sigma are provided in two different versions.  The alternate, or ''var''iant, version is created by adding "var" before the name of the letter:

{{LaTeX/Example|code=\[
  \alpha, \Alpha, \beta, \Beta, \gamma, \Gamma, 
  \pi, \Pi, \phi, \varphi, \Phi
\]
|render=<math>\alpha, \Alpha, \beta, \Beta, \gamma, \Gamma, \pi, \Pi, \phi, \varphi, \Phi</math>}}

Scroll down to [[#List_of_Mathematical_Symbols]] for a complete list of Greek symbols.

== Operators ==
An operator is a function that is written as a word: e.g. trigonometric functions (sin, cos, tan), logarithms and exponentials (log, exp). LaTeX has many of these defined as commands:
{{LaTeX/Example|code=\[
 \cos (2\theta) = \cos^2 \theta - \sin^2 \theta
\]
|render=<math>\cos (2\theta) = \cos^2 \theta - \sin^2 \theta \,</math>
}}

For certain operators such as [[w:Limit (mathematics)|limits]], the subscript is placed underneath the operator:
{{LaTeX/Example|code=\[
 \lim_{x \to \infty} \exp(-x) = 0
\]
|render=<math>\lim_{x \to \infty} \exp(-x) = 0</math>}}

For the [[w:Modular arithmetic|modular operator]] there are two commands: {{LaTeX/LaTeX|code=\bmod}} and {{LaTeX/LaTeX|code=\pmod}}:
{{LaTeX/Example|code=\[
 a \bmod b
\]
|render=<math>
 a \, \bmod \, b
\,</math>}}
{{LaTeX/Example|code=\[
 x \equiv a \pmod b
\]
|render=<math>
 x \equiv a \pmod b
\,</math>}}

To use operators which are not pre-defined, such as [[w:argmax|argmax]], see [[../Advanced Mathematics#Custom operators|custom operators]]

== Powers and indices ==
Powers and indices are equivalent to superscripts and subscripts in normal text mode. The caret (<code>^</code>) character is used to raise something, and the underscore (<code>_</code>) is for lowering. If more than one expression is raised or lowered, they should be grouped using curly braces (<code>{</code> and <code>}</code>).
{{LaTeX/Example|code=\[
 k_{n+1} = n^2 + k_n^2 - k_{n-1}
\]
|render=<math>
k_{n+1} = n^2 + k_n^2 - k_{n-1}
 \,</math>}}

An underscore (<code>_</code>) can be used with a vertical bar (<math>|</math>) to denote evaluation using subscript notation in mathematics:

{{LaTeX/Example|code=\[
 f(n) = n^5 + 4n^2 + 2 {{!}}_{n=17}
\]
|render=<math>
 f(n) = n^5 + 4n^2 + 2 |_{n=17}
 \,</math>}}

== Fractions and Binomials ==
A fraction is created using the {{LaTeX/LaTeX|code=\frac{numerator}{denominator}<!---->}} command. (for those who need their memories refreshed, that's the ''top'' and ''bottom'' respectively!). Likewise, the [[w:Binomial coefficient|binomial coefficient]] (aka the Choose function) may be written using the {{LaTeX/LaTeX|code=\binom}} command<ref name="amsmath"/>:
{{LaTeX/Example|code=
\[
 \frac{n!}{k!(n-k)!} = \binom{n}{k}
\]
|render=
<math>
\frac{n!}{k!(n-k)!} = \binom{n}{k}
</math>
}}

It is also possible to use the {{LaTeX/LaTeX|code=\choose}} command without the {{LaTeX/Package|amsmath}} package:
{{LaTeX/Example|code=
\[
 \frac{n!}{k!(n-k)!} = {n \choose k}
\]
|render=
<math>
 \frac{n!}{k!(n-k)!} = {n \choose k}
</math>
}}

You can embed fractions within fractions:
{{LaTeX/Example|code=
\[
 \frac{\frac{1}{x}+\frac{1}{y}<!---->}{y-z}
\]
|render=
<math>
\frac{\frac{1}{x}+\frac{1}{y}}{y-z}
</math>
}}

Note that when appearing inside another fraction, or in inline text <math>\tfrac{a}{b}</math>, a fraction is noticeably smaller than in displayed mathematics. The {{LaTeX/LaTeX|code=\tfrac}} and {{LaTeX/LaTeX|code=\dfrac}} commands<ref name="amsmath"/> force the use of the respective styles, {{LaTeX/LaTeX|code=\textstyle}} and {{LaTeX/LaTeX|code=\displaystyle}}. Similarly, the {{LaTeX/LaTeX|code=\tbinom}} and {{LaTeX/LaTeX|code=\dbinom}} commands typeset the binomial coefficient.

Another way to write fractions is to use the {{LaTeX/LaTeX|code=\over}} command without the {{LaTeX/Package|amsmath}} package:
{{LaTeX/Example|code=
\[
 {n! \over k!(n-k)!} = {n \choose k}
\]
| render=
<math>
 {n! \over k!(n-k)!} = {n \choose k}
</math>
}}

For relatively simple fractions, it may be more aesthetically pleasing to use [[#Powers and indices|powers and indices]]:
{{LaTeX/Example|code=
\[
 ^3/_7
\]
|render=<math>
^3/_7
 \,</math>
}}

If you use them throughout the document, usage of {{LaTeX/Package|xfrac}} package is recommended.
This package provides {{LaTeX/LaTeX|code=\sfrac}} command to create slanted fractions. Usage:
{{LaTeX/Example|code=
Take \sfrac{1}{2} cup of sugar, \dots
 \[
  3\times\sfrac{1}{2}=1\sfrac{1}{2}
 \]

Take ${}^1/_2$ cup of sugar, \dots
 \[
  3\times{}^1/_2=1{}^1/_2
 \]
|render=[[Image:LaTeX-xfrac-example.png|400px]]}}

Alternatively, the {{LaTeX/Package|nicefrac}} package provides the {{LaTeX/LaTeX|code=\nicefrac}} command, whose usage is similar to {{LaTeX/LaTeX|code=\sfrac}}.

=== Continued fractions ===
Continued fractions should be written using {{LaTeX/LaTeX|code=\cfrac}} command<ref name=amsmath/>:
{{LaTeX/Example|code=
\begin{equation}
  x = a_0 + \cfrac{1}{a_1 
          + \cfrac{1}{a_2 
          + \cfrac{1}{a_3 + a_4}<!---->}<!---->}
\end{equation}
|render=
<math>   x = a_0 + \cfrac{1}{a_1 
           + \cfrac{1}{a_2 
           + \cfrac{1}{a_3 + a_4}}}
</math>
}}

== Roots ==
The {{LaTeX/LaTeX|code=\sqrt}} command creates a square root surrounding an expression. It accepts an optional argument specified in square brackets (<code>[</code> and <code>]</code>) to change magnitude:
{{LaTeX/Example|code=
\[
\sqrt{\frac{a}{b}<!---->}
\]
|render=
<math>
\sqrt{\frac{a}{b}}
</math>
}}
{{LaTeX/Example|code=
\[
\sqrt[n]{1+x+x^2+x^3+\ldots}
\]
|render=<math>
\sqrt[n]{1+x+x^2+x^3+\ldots}
</math>
}}


Some people prefer writing the square root "closing" it over its content. This method arguably makes it more clear just what is in the scope of the root sign. This habit is not normally used while writing with the computer, but if you still want to change the output of the square root, LaTeX gives you this possibility. Just add the following code in the preamble of your document:
{{LaTeX/Example|code=
% New definition of square root:
% it renames \sqrt as \oldsqrt
\let\oldsqrt\sqrt
% it defines the new \sqrt in terms of the old one
\def\sqrt{\mathpalette\DHLhksqrt}
\def\DHLhksqrt#1#2{%
\setbox0=\hbox{$#1\oldsqrt{#2\,}$}\dimen0=\ht0
\advance\dimen0-0.2\ht0
\setbox2=\hbox{\vrule height\ht0 depth -\dimen0}%
{\box0\lower0.4pt\box2}<!---->}
|render=[[Image:Latex_new_squareroot.png|thumb|right|250px|The new style is on left, the old one on right]]
}}
This TeX code first renames the {{LaTeX/LaTeX|code=\sqrt}} command as {{LaTeX/LaTeX|code=\oldsqrt}}, then redefines {{LaTeX/LaTeX|code=\sqrt}} in terms of the old one, adding something more. The new square root can be seen in the picture on the right, compared to the old one. Unfortunately this code won't work if you want to use multiple roots: if you try to write <math>\sqrt[b]{a}</math> as {{LaTeX/LaTeX|code=\sqrt[b]{a}<!---->}} after you used the code above, you'll just get a wrong output. In other words, you can redefine the square root this way only if you are not going to use multiple roots in the whole document.

== Sums and integrals ==
The {{LaTeX/LaTeX|code=\sum}} and {{LaTeX/LaTeX|code=\int}} commands insert the sum and integral symbols respectively, with limits specified using the caret (<code>^</code>) and underscore (<code>_</code>).  The typical notation for sums is:
{{LaTeX/Example|code=
\[
 \sum_{i=1}^{10} t_i 
\]
|render=<math>
\sum_{i=1}^{10} t_i 
 \,</math>
}}
The limits for the integrals follow the same notation.  It's also important to represent the integration variables with an upright d, which in math mode is obtained through the \mathrm{} command, and with a small space separating it from the integrand, which is attained with the \, command.
{{LaTeX/Example|code=
\[
 \int_0^\infty e^{-x}\,\mathrm{d}x
\]
|render=<math>
\int_0^\infty e^{-x}\,\mathrm{d}x
 \,</math>
}}

There are many other "big" commands which operate in a similar manner:
{|
| <code>\sum</code> || <math>\sum \,</math> 
|style="padding-left:20px"|
| <code>\prod</code> || <math>\prod</math> 
|style="padding-left:20px"|
| <code>\coprod</code> || <math>\coprod</math> 
|-
| <code>\bigoplus</code> || <math>\bigoplus</math> 
|style="padding-left:20px"|
| <code>\bigotimes</code> || <math>\bigotimes</math> 
|style="padding-left:20px"|
| <code>\bigodot</code> || <math>\bigodot</math> 
|-
| <code>\bigcup</code> || <math>\bigcup</math> 
|style="padding-left:20px"|
| <code>\bigcap</code> || <math>\bigcap</math> 
|style="padding-left:20px"|
| <code>\biguplus</code> || <math>\biguplus</math> 
|-
| <code>\bigsqcup</code> || <math>\bigsqcup</math> 
|style="padding-left:20px"|
| <code>\bigvee</code> || <math>\bigvee</math> 
|style="padding-left:20px"|
| <code>\bigwedge</code> || <math>\bigwedge</math> 
|-
| <code>\int</code> || <math>\int</math> 
|style="padding-left:20px"|
| <code>\oint</code> || <math>\oint</math> 
|style="padding-left:20px"|
| <code>\iint</code><ref name="amsmath"/> || <math>\iint</math> 
|-
| <code>\iiint</code><ref name="amsmath"/> || <math>\iiint</math> 
|style="padding-left:20px"|
| <code>\iiiint</code><ref name="amsmath"/> || <math>\iiiint</math> 
|style="padding-left:20px"|
| <code>\idotsint</code><ref name="amsmath"/> || <math>\int \! \cdots \! \int</math> 
|}

For more integral symbols, including those not included by default in the Computer Modern font, try the {{LaTeX/Package|esint}} package.

The {{LaTeX/LaTeX|code=\substack}} command<ref name="amsmath"/> allows the use of {{LaTeX/LaTeX|code=\\}} to write the limits over multiple lines:
{{LaTeX/Example|code=
\[
 \sum_{\substack{
   0<i<m \\
   0<j<n
  }<!---->} 
 P(i,j)
\]
|render=<math>
\sum_{\overset{\scriptstyle 0<i<m} {\scriptstyle 0<j<n}} P(i,j)
 \,</math>
}}

If you want the limits of an integral to be specified above and below the symbol (like the sum), use the {{LaTeX/LaTeX|code=\limits}} command:
{{LaTeX/Example|code=
\[
 \int\limits_a^b
\]
|render=<math>
\int\limits_a^b
 \,</math>
}}
However if you want this to apply to ALL integrals, it is preferable to specify the {{LaTeX/Parameter|intlimits}} option when loading the {{LaTeX/Package|amsmath}} package:
{{LaTeX/Usage|code=
\usepackage[intlimits]{amsmath}
}}

Subscripts and superscripts in other contexts as well as other parameters to {{LaTeX/Package|amsmath}} package related to them are described in [[LaTeX/Advanced_Mathematics#Advanced_formatting|Advanced Mathematics]] chapter.

For bigger integrals, you may use personal declarations, or the {{LaTeX/Package|bigints}} package <ref name=LM> http://hdl.handle.net/2268/6219</ref>.

== Brackets, braces and delimiters ==
<small>''How to use braces in multi line equations is described in the [[LaTeX/Advanced_Mathematics#Braces_spanning_multiple_lines|Advanced Mathematics]] chapter.''</small>

The use of delimiters such as brackets soon becomes important when dealing with anything but the most trivial equations. Without them, formulas can become ambiguous. Also, special types of mathematical structures, such as matrices, typically rely on delimiters to enclose them.

There are a variety of delimiters available for use in LaTeX:
{{LaTeX/Example|code=
\[
 () \, [] \, \{\} \, {{!}}{{!}} \, \{{!}}\{{!}} \, 
 \langle\rangle \, \lfloor\rfloor \, \lceil\rceil
\]
|render=<math>
() \, [] \, \{\} \, || \, \|\| \, \langle\rangle \, \lfloor\rfloor \, \lceil\rceil
 \,</math>
}}

=== Automatic sizing ===
Very often mathematical features will differ in size, in which case the delimiters surrounding the expression should vary accordingly. This can be done automatically using the {{LaTeX/LaTeX|code=\left}} and {{LaTeX/LaTeX|code=\right}} commands. Any of the previous delimiters may be used in combination with these:
{{LaTeX/Example|code=
\[
 \left(\frac{x^2}{y^3}\right)
\]
|render=<math>
\left(\frac{x^2}{y^3}\right)
 \,</math>
}}
If a delimiter on only one side of an expression is required, then an invisible delimiter on the other side may be denoted using a period (<code>.</code>).

=== Manual sizing ===
In certain cases, the sizing produced by the {{LaTeX/LaTeX|code=\left}} and {{LaTeX/LaTeX|code=\right}} commands may not be desirable, or you may simply want finer control over the delimiter sizes. In this case, the {{LaTeX/LaTeX|code=\big}}, {{LaTeX/LaTeX|code=\Big}}, {{LaTeX/LaTeX|code=\bigg}} and {{LaTeX/LaTeX|code=\Bigg}} modifier commands may be used:
{{LaTeX/Example|code=
\[
 ( \big( \Big( \bigg( \Bigg( 
\]
|render=<math>
( \big( \Big( \bigg( \Bigg( 
 \,</math>
}}

== Matrices and arrays ==
A basic matrix may be created using the {{LaTeX/Environment|matrix}} environment<ref name="amsmath">requires the {{LaTeX/Package|amsmath}} package</ref>: in common with other table-like structures, entries are specified by row, with columns separated using an ampersand ({{LaTeX/LaTeX|code=&}}) and a new rows separated with a double backslash ({{LaTeX/LaTeX|code=\\}})
{{LaTeX/Example|code=
\[
 \begin{matrix}
  a & b & c \\
  d & e & f \\
  g & h & i
 \end{matrix}
\]
|render=<math>
\begin{matrix}
a & b & c \\
d & e & f \\
g & h & i
\end{matrix}
</math>
}}

To specify alignment of columns in the table, use starred version<ref name="mathtools">requires the {{LaTeX/Package|mathtools}} package</ref>:
{{LaTeX/Example|code=
\[
 \begin{matrix}
  -1 & 3 \\
  2 & -4
 \end{matrix}
 =
 \begin{matrix*}[r]
  -1 & 3 \\
  2 & -4
 \end{matrix*}
\]
|render=
<math>
 \begin{matrix}
  -1 & 3 \\
  2 & -4
 \end{matrix}
 =
 \begin{matrix}
  -1 & \,\;\;3 \\
  \,\;\;2 & -4
 \end{matrix}
</math>
}}

The alignment by default is {{LaTeX/Parameter|c}} but it can be any column type valid in {{LaTeX/Environment|array}} environment.

However matrices are usually enclosed in delimiters of some kind, and while it is possible to use the [[#Automatic sizing|<tt>\left</tt> and <tt>\right</tt> commands]], there are various other predefined environments which automatically include delimiters:
{|class="wikitable"
! Environment name
! Surrounding delimiter
! Notes
|-
| {{LaTeX/Environment|pmatrix}}<ref name="amsmath"/>
| <math>( \, ) </math>
| centers columns by default
|-
| {{LaTeX/Environment|pmatrix*}}<ref name="mathtools"/>
| <math>( \, ) </math>
| allows to specify alignment of columns in optional parameter
|-
| {{LaTeX/Environment|bmatrix}}<ref name="amsmath"/>
| <math>[ \, ] </math>
| centers columns by default
|-
| {{LaTeX/Environment|bmatrix*}}<ref name="mathtools"/>
| <math>[ \, ] </math>
| allows to specify alignment of columns in optional parameter
|-
| {{LaTeX/Environment|Bmatrix}}<ref name="amsmath"/>
| <math>\{ \, \} </math>
| centers columns by default
|-
| {{LaTeX/Environment|Bmatrix*}}<ref name="mathtools"/>
| <math>\{ \, \} </math>
| allows to specify alignment of columns in optional parameter
|-
| {{LaTeX/Environment|vmatrix}}<ref name="amsmath"/>
| <math>| \, | </math>
| centers columns by default
|-
| {{LaTeX/Environment|vmatrix*}}<ref name="mathtools"/>
| <math>| \, | </math>
| allows to specify alignment of columns in optional parameter
|-
| {{LaTeX/Environment|Vmatrix}}<ref name="amsmath"/>
| <math>\| \, \| </math>
| centers columns by default
|-
| {{LaTeX/Environment|Vmatrix*}}<ref name="mathtools"/>
| <math>\| \, \| </math>
| allows to specify alignment of colums in optional parameter
|}

When writing down arbitrary sized matrices, it is common to use horizontal, vertical and diagonal triplets of dots (known as [[w:ellipsis|ellipses]]) to fill in certain columns and rows. These can be specified using the {{LaTeX/LaTeX|code=\cdots}}, {{LaTeX/LaTeX|code=\vdots}} and {{LaTeX/LaTeX|code=\ddots}} respectively:
{{LaTeX/Example|code=
\[
 A_{m,n} = 
 \begin{pmatrix}
  a_{1,1} & a_{1,2} & \cdots & a_{1,n} \\
  a_{2,1} & a_{2,2} & \cdots & a_{2,n} \\
  \vdots  & \vdots  & \ddots & \vdots  \\
  a_{m,1} & a_{m,2} & \cdots & a_{m,n} 
 \end{pmatrix}
\]
|render=<math>
A_{m,n} = 
\begin{pmatrix}
a_{1,1} & a_{1,2} & \cdots & a_{1,n} \\
a_{2,1} & a_{2,2} & \cdots & a_{2,n} \\
\vdots  & \vdots  & \ddots & \vdots  \\
a_{m,1} & a_{m,2} & \cdots & a_{m,n} 
\end{pmatrix}
</math>
}}
In some cases you may want to have finer control of the alignment within each column, or want to insert lines between columns or rows. This can be achieved using the {{LaTeX/Environment|array}} environment, which is essentially a math-mode version of the [[../Tables#The tabular environment|<tt>tabular</tt> environment]], which requires that the columns be pre-specified:
{{LaTeX/Example|code=
\[
 \begin{array}{c{{!}}c}
  1 & 2 \\ 
  \hline
  3 & 4
 \end{array}
\]
|render=<math>
\begin{array}{c|c}
1 & 2 \\ 
\hline
3 & 4
\end{array}
</math>
}}

You may see that the AMS matrix class of environments doesn't leave enough space when used together with fractions resulting in output similar to this:

<math>
 M = \begin{bmatrix}
       \frac{5}{6} & \frac{1}{6} & 0\\
       \frac{5}{6} & 0           & \frac{1}{6}\\
       0           & \frac{5}{6} & \frac{1}{6}
     \end{bmatrix}
</math>

To counteract this problem, add additional leading space with the optional parameter to the {{LaTeX/LaTeX|code=\\}} command:

{{LaTeX/Example|code=
\[
 M = \begin{bmatrix}
       \frac{5}{6} & \frac{1}{6} & 0           \\[0.3em]
       \frac{5}{6} & 0           & \frac{1}{6} \\[0.3em]
       0           & \frac{5}{6} & \frac{1}{6}
     \end{bmatrix}
\]
|render=<math>
 M = \begin{bmatrix}
       \frac{5}{6} & \frac{1}{6} & 0           \\[0.3em]
       \frac{5}{6} & 0           & \frac{1}{6} \\[0.3em]
       0           & \frac{5}{6} & \frac{1}{6}
     \end{bmatrix}</math>
}}

If you need "border" or "indexes" on your matrix, plain TeX provides the macro {{LaTeX/LaTeX|code=\bordermatrix}}
{{LaTeX/Example|code=
\[
M = \bordermatrix{~ & x & y \cr
                  A & 1 & 0 \cr
                  B & 0 & 1 \cr}
\]
|render=[[Image:bordermatrix.png|150px]]
}}


=== Matrices in running text ===
To insert a small matrix, and not increase leading in the line containing it, use {{LaTeX/Environment|smallmatrix}} environment:

{{LaTeX/Example|code=
A matrix in text must be set smaller
$\bigl(\begin{smallmatrix}
a&b\\ c&d
\end{smallmatrix} \bigr)$
to not increase leading in a portion of text.
|render=[[Image:LaTeX-smallmatrix.png|370px]]
}}

== Adding text to equations ==

The math environment differs from the text environment in the representation of text.  Here is an example of trying to represent text within the math environment:
{{LaTeX/Example|code=
\[
 50 apples \times 100 apples = lots of apples^2
\]
|render=<math>
 50 apples \times 100 apples = lots of apples^2
\,</math>
}}

There are two noticeable problems: there are no spaces between words or numbers, and the letters are italicized and more spaced out than normal. Both issues are simply artifacts of the maths mode, in that it treats it as a mathematical expression: spaces are ignored (LaTeX spaces mathematics according to its own rules), and each character is a separate element (so are not positioned as closely as normal text).

There are a number of ways that text can be added properly. The typical way is to wrap the text with the {{LaTeX/LaTeX|code=\text{...}<!---->}} command <ref name="amsmath"/> (a similar command is {{LaTeX/LaTeX|code=\mbox{...}<!---->}}, though this causes problems with subscripts, and has a less descriptive name). Let's see what happens when the above equation code is adapted:
{{LaTeX/Example|code=
\[
 50 \text{apples} \times 100 \text{apples} 
 = \text{lots of apples}^2
\]
|render=
<math>
 50 \text{apples} \times 100 \text{apples} = \text{lots of apples}^2
\,</math>
}}

The text looks better. However, there are no gaps between the numbers and the words. Unfortunately, you are required to explicitly add these. There are many ways to add spaces between maths elements, but for the sake of simplicity you may literally add the space character in the affected {{LaTeX/LaTeX|code=\text}}(s) itself (just before the text.)
{{LaTeX/Example|code=
\[
 50 \text{ apples} \times 100 \text{ apples}
 = \text{lots of apples}^2
\]
|render=
<math>
 50 \text{ apples} \times 100 \text{ apples} = \text{lots of apples}^2
\,</math>
}}

=== Formatted text ===
Using the {{LaTeX/LaTeX|code=\text}} is fine and gets the basic result. Yet, there is an alternative that offers a little more flexibility. You may recall the introduction of [[LaTeX/Formatting#Font Styles and size|font formatting commands]], such as {{LaTeX/LaTeX|code=\textrm}}, {{LaTeX/LaTeX|code=\textit}}, {{LaTeX/LaTeX|code=\textbf}}, etc. These commands format the argument accordingly, e.g., {{LaTeX/LaTeX|code=\textbf{bold text}<!---->}} gives '''bold text'''. These commands are equally valid within a maths environment to include text. The added benefit here is that you can have better control over the font formatting, rather than the standard text achieved with {{LaTeX/LaTeX|code=\text}}.

{{LaTeX/Example|code=
\[
 50 \textrm{ apples} \times 100
 \textbf{ apples} = \textit{lots of apples}^2
\]
|render=<math>
 50 \;\textrm{ apples} \times 100 \;\textbf{ apples} = \textit{lots}\;of\;apples^2
\,</math>
}}

== Formatting mathematics symbols ==
So we can format text, what about formatting mathematics? There are a set of formatting commands very similar to the font formatting ones just used, except they are aimed specifically for text in maths mode (requires {{LaTeX/Package|amsfonts}})
{|class="wikitable"
! LaTeX command
! Sample
! Description
! Common use
|-
| {{LaTeX/LaTeX|code=\mathnormal{…}<!---->}}
| <math>ABCDEF abcdef 123456\,</math>
| the default math font
| most mathematical notation
|-
| {{LaTeX/LaTeX|code=\mathrm{…}<!---->}}
| <math>\mathrm{ABCDEF abcdef 123456}\,</math>
| this is the default or normal font, unitalicised
| units of measurement, one word functions
|-
| {{LaTeX/LaTeX|code=\mathit{…}<!---->}}
| <math>\mathit{ABCDEF abcdef 123456}\,</math>
| italicised font
| 
|-
| {{LaTeX/LaTeX|code=\mathbf{…}<!---->}}
| <math>\mathbf{ABCDEF abcdef 123456}\,</math>
| bold font
| vectors
|-
| {{LaTeX/LaTeX|code=\mathsf{…}<!---->}}
| <math>\mathsf{ABCDEF abcdef 123456}\,</math>
| [[w:sans-serif|Sans-serif]]
|
|-
| {{LaTeX/LaTeX|code=\mathtt{…}<!---->}}
| <font style="font-family: monospace;">ABCDEFabcdef123456</font>
| [[w:Monospace font|Monospace (fixed-width) font]]
|
|-
| {{LaTeX/LaTeX|code=\mathcal{…}<!---->}}
| <math>\mathcal{ABCDEF abcdef 123456}\,</math>
| Calligraphy (uppercase only)
| often used for sheaves/schemes and categories, used to denote [[w:en:Cryptography|cryptological]] concepts like an ''alphabet of deinition'' (<math>\mathcal{A}</math>), ''message space'' (<math>\mathcal{M}</math>), ''ciphertext space'' (<math>\mathcal{C}</math>) and ''[[w:key space|key space]]'' (<math>\mathcal{K}</math>); [[w:Kleene's O|Kleene's <math>\mathcal{O}</math>]]; [[w:Description logic#Naming Convention|naming convention in description logic]]
|-
| {{LaTeX/LaTeX|code=\mathfrak{…}<!---->}}<ref name="amsfonts">requires <tt>amsfonts</tt> or <tt>amssymb</tt> packages</ref>
| <math>\mathfrak{ABCDEF abcdef 123456}\,</math>
| [[w:Fraktur (script)|Fraktur]]
| Almost canonical font for Lie algebras, with superscript used to denote [[w:List of New Testament papyri|New Testament papyri]], [[w:Ideal (ring theory)|ideals]] in ring theory
|-
| {{LaTeX/LaTeX|code=\mathbb{…}<!---->}}<ref name="amsfonts"/>
| <math>\mathbb{ABCDEF abcdef 123456}\,</math>
| [[w:Blackboard bold|Blackboard bold]]
| Used to denote special sets (e.g. real numbers)
|-
| {{LaTeX/LaTeX|code=\mathscr{…}<!---->}}<ref>require <tt>mathrsfs</tt> package</ref>
| 
| [[w:Script (typefaces)|Script]]
| 
|}
The maths formatting commands can be wrapped around the entire equation, and not just on the textual elements: they only format letters, numbers, and uppercase Greek, and the rest of the maths syntax is ignored. 

To bold lowercase Greek or other symbols use the {{LaTeX/LaTeX|code=\boldsymbol}} command<ref name="amsmath"/>; this will only work if there exists a bold version of the symbol in the current font. As a last resort there is the {{LaTeX/LaTeX|code=\pmb}} command<ref name="amsmath"/> (poor mans bold): this prints multiple versions of the character slightly offset against each other
{{LaTeX/Example|code=
\[
 \boldsymbol{\beta} = (\beta_1,\beta_2,\ldots,\beta_n)
\]
|render=
<math>
 \boldsymbol{\beta} = (\beta_1,\beta_2,\ldots,\beta_n)
\,</math>
}}
To change the size of the fonts in math mode, see [[../Advanced Mathematics#Changing font size|Changing font size]].

=== Accents ===
So what to do when you run out of symbols and fonts? Well the next step is to use accents:

{|
| <code>a'</code> || <math>a'\,</math> 
| style="padding-left:20px" |
| <code><nowiki>a''</nowiki></code> || <math>a''\,</math>
| style="padding-left:20px" |
| <code><nowiki>a'''</nowiki></code> || <math>a'''\,</math>
| style="padding-left:20px" |
| <code><nowiki>a''''</nowiki></code> || <math>a''''\,</math>
|-
| <code>\hat{a}</code> || <math>\hat{a} \,</math> 
| style="padding-left:20px" |
| <code>\bar{a}</code> || <math>\bar{a} \,</math> 
| style="padding-left:20px" |
| <code>\overline{aaa}</code> || <math>\overline{aaa} \,</math> 
| style="padding-left:20px" |
| <code>\check{a}</code> || <math>\check{a} \,</math> 
| style="padding-left:20px" |
| <code>\tilde{a}</code> || <math>\tilde{a} \,</math> 
|-
| <code>\grave{a}</code> || <math>\grave{a} \,</math> 
| style="padding-left:20px" |
| <code>\acute{a}</code> || <math>\acute{a} \,</math> 
| style="padding-left:20px" |
| <code>\breve{a}</code> || <math>\breve{a} \,</math> 
| style="padding-left:20px" |
| <code>\vec{a}</code> || <math>\vec{a} \,</math> 
|-
| <code>\dot{a}</code> || <math>\dot{a} \,</math> 
| style="padding-left:20px" |
| <code>\ddot{a}</code> || <math>\ddot{a} \,</math> 
| style="padding-left:20px" |
| <code>\dddot{a}</code><ref name="amsmath"/> || 
| style="padding-left:20px" |
| <code>\ddddot{a}</code><ref name="amsmath"/> || 
|-
| <code>\not{a}</code> || <math>\not{a} \,</math> 
| style="padding-left:20px" |
| <code>\mathring{a}</code> ||
| style="padding-left:20px" |
| <code>\widehat{AAA}</code> || <math>\widehat{AAA} \,</math> 
| style="padding-left:20px" |
| <code>\widetilde{AAA}</code> ||
|}

==Plus and minus signs==

Latex deals with the + and − signs in two possible ways. The most common is as a binary operator. When two maths elements appear either side of the sign, it is assumed to be a binary operator, and as such, allocates some space either side of the sign. The alternative way is a sign designation. This is when you state whether a mathematical quantity is either positive or negative. This is common for the latter, as in maths, such elements are assumed to be positive unless a − is prefixed to it. In this instance, you want the sign to appear close to the appropriate element to show their association. If you put a + or a − with nothing before it but you want it to be handled like a binary operator you can add an ''invisible'' character before the operator using {{LaTeX/LaTeX|code={}<!---->}}. This can be useful if you are writing multiple-line formulas, and a new line could start with a = or a +, for example, then you can fix some strange alignments adding the invisible character where necessary.

== Controlling horizontal spacing ==

LaTeX is obviously pretty good at typesetting maths—it was one of the chief aims of the core Tex system that LaTeX extends. However, it can't always be relied upon to accurately interpret formulas in the way you did. It has to make certain assumptions when there are ambiguous expressions. The result tends to be slightly incorrect horizontal spacing. In these events, the output is still satisfactory, yet, any perfectionists will no doubt wish to ''fine-tune'' their formulas to ensure spacing is correct. These are generally very subtle adjustments.

There are other occasions where LaTeX has done its job correctly, but you just want to add some space, maybe to add a comment of some kind. For example, in the following equation, it is preferable to ensure there is a decent amount of space between the maths and the text.

{{LaTeX/Example|code=
\[
  f(n) = \left\{ 
  \begin{array}{l l}
    n/2 & \quad \text{if $n$ is even}\\
    -(n+1)/2 & \quad \text{if $n$ is odd}\\
  \end{array} \right.
\]

|render=
<math>
f(n) =
\begin{cases}
n/2 & \quad \text{if } n \text{ is even} \\
-(n+1)/2 & \quad \text{if } n \text{ is odd}\\ 
\end{cases} 
</math>
}}

This code produces errors with Miktex 2.9 and does not yield the results seen on the right.
Use \textrm instead of just \text.


(Note that this particular example can be expressed in more elegant code by the {{LaTeX/Environment|cases}} construct provided by the {{LaTeX/Package|amsmath}} package described in [[LaTeX/Advanced_Mathematics#The_cases_environment|Advanced Mathematics]] chapter.)

LaTeX has defined two commands that can be used anywhere in documents (not just maths) to insert some horizontal space. They are {{LaTeX/LaTeX|code=\quad}} and {{LaTeX/LaTeX|code=\qquad}}

A {{LaTeX/LaTeX|code=\quad}} is a space equal to the current font size. So, if you are using an 11pt font, then the space provided by {{LaTeX/LaTeX|code=\quad}} will also be 11pt (horizontally, of course.) The {{LaTeX/LaTeX|code=\qquad}} gives twice that amount. As you can see from the code from the above example, {{LaTeX/LaTeX|code=\quad}}s were used to add some separation between the maths and the text.

OK, so back to the fine tuning as mentioned at the beginning of the document. A good example would be displaying the simple equation for the indefinite integral of ''y'' with respect to ''x'':

<math>\int y\, \mathrm{d}x</math>

If you were to try this, you may write:

{{LaTeX/Example|code=
\[ \int y \mathrm{d}x \]
|render=
<math>\int y \mathrm{d}x</math>
}}

However, this doesn't give the correct result. LaTeX doesn't respect the white-space left in the code to signify that the ''y'' and the d''x'' are independent entities. Instead, it lumps them altogether. A {{LaTeX/LaTeX|code=\quad}} would clearly be overkill is this situation—what is needed are some small spaces to be utilized in this type of instance, and that's what LaTeX provides:

{| class="wikitable"
! Command
! Description
! Size
|-
| {{LaTeX/LaTeX|code=\,}}
| small space
| 3/18 of a quad
|-
| {{LaTeX/LaTeX|code=\:}}
| medium space
| 4/18 of a quad
|-
| {{LaTeX/LaTeX|code=\;}}
| large space
| 5/18 of a quad
|-
| {{LaTeX/LaTeX|code=\!}}
| negative space
| -3/18 of a quad
|}

NB you can use more than one command in a sequence to achieve a greater space if necessary.

So, to rectify the current problem:

{{LaTeX/Example|code=
\[ \int y\, \mathrm{d}x \]
|render=
<math>\int y\, \mathrm{d}x</math>
}}
{{LaTeX/Example|code=
\[ \int y\: \mathrm{d}x \]
|render=
<math>\int y\;\;\!\! \mathrm{d}x</math>
}}
{{LaTeX/Example|code=
\[ \int y\; \mathrm{d}x \]
|render=
<math>\int y\; \mathrm{d}x</math>
}}

The negative space may seem like an odd thing to use, however, it wouldn't be there if it didn't have ''some'' use! Take the following example:

{{LaTeX/Example|code=
\[
  \left(
    \begin{array}{c}
      n \\
      r
    \end{array}
  \right) = \frac{n!}{r!(n-r)!}
\]
|render=
<math>\left(
   \begin{matrix}
     n \\
     r
   \end{matrix}
   \right) = \frac{n!}{r!(n-r)!}</math>
}}


The matrix-like expression for representing binomial coefficients is too padded. There is too much space between the brackets and the actual contents within. This can easily be corrected by adding a few negative spaces after the left bracket and before the right bracket.

{{LaTeX/Example|code=
\[
  \left(\!
    \begin{array}{c}
      n \\
      r
    \end{array}
  \!\right) = \frac{n!}{r!(n-r)!}
\]
|render=
<math>\left(\!
   \begin{matrix}
     n \\
     r
   \end{matrix}
   \!\right) = \frac{n!}{r!(n-r)!}
</math>
}}

In any case, adding some spaces manually should be avoided whenever possible: it makes the source code more complex and it's against the basic principles of a What You See is What You Mean approach. The best thing to do is to define some commands using all the spaces you want and then, when you use your command, you don't have to add any other space. Later, if you change your mind about the length of the horizontal space, you can easily change it modifying only the command you defined before. Let us use an example: you want the ''d'' of a ''dx'' in an integral to be in roman font and a small space away from the rest. If you want to type an integral like {{LaTeX/LaTeX|code=\int x \; \mathrm{d} x}}, you can define a command like this:
{{LaTeX/Usage|code=
\newcommand{\dd}{\; \mathrm{d}<!---->}
}}
in the preamble of your document. We have chosen {{LaTeX/LaTeX|code=\dd}} just because it reminds the "d" it replaces and it is fast to type. Doing so, the code for your integral becomes {{LaTeX/LaTeX|code=\int x \dd x}}. Now, whenever you write an integral, you just have to use the {{LaTeX/LaTeX|code=\dd}} instead of the "d", and all your integrals will have the same style. If you change your mind, you just have to change the definition in the preamble, and all your integrals will be changed accordingly.

==Advanced Mathematics: AMS Math package==

The AMS ([[Wikipedia:American Mathematical Society|American Mathematical Society]]) mathematics package is a powerful package that creates a higher layer of abstraction over mathematical LaTeX language; if you use it it will make your life easier. Some commands {{LaTeX/Package|amsmath}} introduces will make other plain LaTeX commands obsolete: in order to keep consistency in the final output you'd better use {{LaTeX/Package|amsmath}} commands whenever possible. If you do so, you will get an elegant output without worrying about alignment and other details, keeping your source code readable. If you want to use it, you have to add this in the preamble:
{{LaTeX/Usage|code=
\usepackage{amsmath}
}}
 
===Introducing text and dots in formulas===
{{LaTeX/Package|amsmath}} defines also the {{LaTeX/LaTeX|code=\dots}} command, that is a generalization of the existing {{LaTeX/LaTeX|code=\ldots}}. You can use {{LaTeX/LaTeX|code=\dots}} in both text and math mode and LaTeX will replace it with three dots "…" but it will decide according to the context whether to put it on the bottom (like {{LaTeX/LaTeX|code=\ldots}}) or centered (like {{LaTeX/LaTeX|code=\cdots}}).

===Dots===

LaTeX gives you several commands to insert dots in your formulas. This can be particularly useful if you have to type big matrices omitting elements. First of all, here are the main dots-related commands LaTeX provides:

{| class="wikitable"
! Code !! Output !! Comment
|-
| {{LaTeX/LaTeX|code=\dots}} || <math>\dots</math> || generic dots, to be used in text (outside formulas as well). It automatically manages whitespaces before and after itself according to the context, it's a higher level command.
|-
| {{LaTeX/LaTeX|code=\ldots}}|| <math>\ldots</math> || the output is similar to the previous one, but there is no automatic whitespace management; it works at a lower level.
|-
| {{LaTeX/LaTeX|code=\cdots}} || <math>\cdots</math> || These dots are centered relative to the height of a letter. There is also the binary multiplication operator, <tt>\cdot</tt>, mentioned below.
|-
| {{LaTeX/LaTeX|code=\vdots}} || <math>\vdots</math> || vertical dots
|-
| {{LaTeX/LaTeX|code=\ddots}} || <math>\ddots</math> || diagonal dots
|-
| {{LaTeX/LaTeX|code=\iddots}} ||  || inverse diagonal dots (requires the mathdots package)
|-
| {{LaTeX/LaTeX|code=\hdotsfor{n}<!---->}} || <math>\ldots \ldots</math>|| to be used in matrices, it creates a row of dots spanning ''n'' columns. 
|}

Instead of using {{LaTeX/LaTeX|code=\ldots}} and {{LaTeX/LaTeX|code=\cdots}}, you should use the semantically oriented commands. It makes it possible to adapt your document to different conventions on the fly, in case (for example) you have to submit it to a publisher who insists on following house tradition in this respect. The default treatment for the various kinds follows American Mathematical Society conventions.

{| class="wikitable"
! Code !! Output !! Comment
|-
| {{LaTeX/LaTeX|code=A_1,A_2,\dotsc,}} || [[File:LaTeX Dotsc.png]] || for "dots with commas"
|-
| {{LaTeX/LaTeX|code=A_1+\dotsb+A_N}} || [[File:LaTeX Dotsb.png]] || for "dots with binary operators/relations"
|-
| {{LaTeX/LaTeX|code=A_1 \dotsm A_N}} || [[File:LaTeX Dotsm.png]] || for "multiplication dots"
|-
| {{LaTeX/LaTeX|code=\int_a^b \dotsi}} || [[File:LaTeX Dotsi.png]] || for "dots with integrals"
|-
| {{LaTeX/LaTeX|code=A_1\dotso A_N}} || [[File:LaTeX Dotso.png]] || for "other dots" (none of the above)
|}

==List of Mathematical Symbols==
All the pre-defined mathematical symbols from the \TeX\ package are listed below. More symbols are available from extra packages.

{| class="wikitable" valign="top"
|+Relation Symbols
! Symbol !! Script !! Symbol !! Script !! Symbol !! Script !! Symbol !! Script !! Symbol !! Script 
|-
| <math>\leq</math>||<tt>\leq</tt>|| <math>\geq</math>||<tt>\geq</tt>|| <math>\equiv\,</math>||<tt>\equiv</tt>|| <math>\models</math>||<tt>\models</tt>|| <math>\prec</math>||<tt>\prec</tt>
|-
| <math>\succ</math>||<tt>\succ</tt>|| <math>\sim</math>||<tt>\sim</tt>|| <math>\perp</math>||<tt>\perp</tt>|| <math>\preceq</math>||<tt>\preceq</tt>|| <math>\succeq</math>||<tt>\succeq</tt>
|-
| <math>\simeq</math>||<tt>\simeq</tt>|| <math>\mid</math>||<tt>\mid</tt>|| <math>\ll</math>||<tt>\ll</tt>|| <math>\gg</math>||<tt>\gg</tt>|| <math>\asymp</math>||<tt>\asymp</tt>
|-
| <math>\parallel</math>||<tt>\parallel</tt>|| <math>\subset</math>||<tt>\subset</tt>|| <math>\supset</math>||<tt>\supset</tt>|| <math>\approx</math>||<tt>\approx</tt>|| <math>\bowtie</math>||<tt>\bowtie</tt>
|-
| <math>\subseteq</math>||<tt>\subseteq</tt>|| <math>\supseteq</math>||<tt>\supseteq</tt>|| <math>\cong</math>||<tt>\cong</tt>|| <math>\sqsubset</math>||<tt>\sqsubset</tt>|| <math>\sqsupset</math>||<tt>\sqsupset</tt>
|-
| <math>\neq\,</math>||<tt>\neq</tt>|| <math>\smile</math>||<tt>\smile</tt>|| <math>\sqsubseteq</math>||<tt>\sqsubseteq</tt>|| <math>\sqsupseteq</math>||<tt>\sqsupseteq</tt>|| <math>\doteq</math>||<tt>\doteq</tt>
|-
| <math>\frown</math>||<tt>\frown</tt>|| <math>\in</math>||<tt>\in</tt>|| <math>\ni</math>||<tt>\ni</tt>|| <math>\propto</math>||<tt>\propto</tt>|| <math>=\,</math>||<tt>=</tt>
|-
| <math>\vdash</math>||<tt>\vdash</tt>|| <math>\dashv</math>||<tt>\dashv</tt>|| <math><\,</math>||<tt><</tt>|| <math>>\,</math>||<tt>></tt>
|}

{| class="wikitable" align="left"
|+Greek Letters
! Symbol !! Script
|-
| <math>\Alpha\,</math> and <math>\alpha\,</math>|| <tt>\Alpha</tt> and <tt>\alpha</tt>
|-
| <math>\Beta\,</math> and <math>\beta\,</math>|| <tt>\Beta</tt> and <tt>\beta</tt>
|-
| <math>\Gamma\,</math> and <math>\gamma\,</math>|| <tt>\Gamma</tt> and <tt>\gamma</tt>
|-
| <math>\Delta\,</math> and <math>\delta\,</math>|| <tt>\Delta</tt> and <tt>\delta</tt>
|-
| <math>\Epsilon\,</math>, <math>\epsilon\,</math> and <math>\varepsilon</math>|| <tt>\Epsilon</tt>, <tt>\epsilon</tt> and <tt>\varepsilon</tt>
|-
| <math>\Zeta\,</math> and <math>\zeta\,</math>|| <tt>\Zeta</tt> and <tt>\zeta</tt>
|-
| <math>\Eta\,</math> and <math>\eta\,</math>|| <tt>\Eta</tt> and <tt>\eta</tt>
|-
| <math>\Theta\,</math>, <math>\theta\,</math> and <math>\vartheta</math>|| <tt>\Theta</tt>, <tt>\theta</tt> and <tt>\vartheta</tt>
|-
| <math>\Iota\,</math> and <math>\iota\,</math>|| <tt>\Iota</tt> and <tt>\iota</tt>
|-
| <math>\Kappa\,</math> and <math>\kappa\,</math>|| <tt>\Kappa</tt> and <tt>\kappa</tt>
|-
| <math>\Lambda\,</math> and <math>\lambda\,</math>|| <tt>\Lambda</tt> and <tt>\lambda</tt>
|-
| <math>\Mu\,</math> and <math>\mu\,</math>|| <tt>\Mu</tt> and <tt>\mu</tt>
|-
| <math>\Nu\,</math> and <math>\nu\,</math>|| <tt>\Nu</tt> and <tt>\nu</tt>
|-
| <math>\Xi\,</math> and <math>\xi\,</math>|| <tt>\Xi</tt> and <tt>\xi</tt>
|-
| <math>\Pi\,</math>, <math>\pi\,</math> and <math>\varpi</math>|| <tt>\Pi</tt>, <tt>\pi</tt> and <tt>\varpi</tt>
|-
| <math>\Rho\,</math>, <math>\rho\,</math> and <math>\varrho</math>|| <tt>\Rho</tt>, <tt>\rho</tt> and <tt>\varrho</tt>
|-
| <math>\Sigma\,</math>, <math>\sigma\,</math> and <math>\varsigma</math>|| <tt>\Sigma</tt>, <tt>\sigma</tt> and <tt>\varsigma</tt>
|-
| <math>\Tau\,</math> and <math>\tau\,</math>|| <tt>\Tau</tt> and <tt>\tau</tt>
|-
| <math>\Upsilon\,</math> and <math>\upsilon\,</math>|| <tt>\Upsilon</tt> and <tt>\upsilon</tt>
|-
| <math>\Phi\,</math>, <math>\phi\,</math>, and <math>\varphi</math>|| <tt>\Phi</tt>, <tt>\phi</tt> and <tt>\varphi</tt>
|-
| <math>\Chi\,</math> and <math>\chi\,</math>|| <tt>\Chi</tt> and <tt>\chi</tt>
|-
| <math>\Psi\,</math> and <math>\psi\,</math>|| <tt>\Psi</tt> and <tt>\psi</tt>
|-
| <math>\Omega\,</math> and <math>\omega\,</math>|| <tt>\Omega</tt> and <tt>\omega</tt>
|}

{| class="wikitable" align="center"
|+Binary Operations
! Symbol !! Script !! Symbol !! Script !! Symbol !! Script !! Symbol !! Script 
|-
| <math>\pm\,</math> || <tt>\pm</tt> || <math>\cap\,</math> || <tt>\cap</tt> || <math>\diamond</math> || <tt>\diamond</tt> || <math>\oplus</math> || <tt>\oplus</tt> 
|-
| <math>\mp</math> || <tt>\mp</tt> || <math>\cup</math> || <tt>\cup</tt> || <math>\bigtriangleup</math> || <tt>\bigtriangleup</tt> || <math>\ominus</math> || <tt>\ominus</tt> 
|-
| <math>\times\,</math> || <tt>\times</tt> || <math>\uplus</math> || <tt>\uplus</tt> || <math>\bigtriangledown</math> || <tt>\bigtriangledown</tt> || <math>\otimes</math> || <tt>\otimes</tt> 
|-
| <math>\div</math> || <tt>\div</tt> || <math>\sqcap</math> || <tt>\sqcap</tt> || <math>\triangleleft</math> || <tt>\triangleleft</tt> || <math>\oslash</math> || <tt>\oslash</tt> 
|-
| <math>\ast</math> || <tt>\ast</tt> || <math>\sqcup</math> || <tt>\sqcup</tt> || <math>\triangleright</math> || <tt>\triangleright</tt> || <math>\odot</math> || <tt>\odot</tt> 
|-
| <math>\star</math> || <tt>\star</tt> || <math>\vee</math> || <tt>\vee</tt> || <math>\bigcirc</math> || <tt>\bigcirc</tt> || <math>\circ</math> || <tt>\circ</tt> 
|-
| <math>\wedge</math> || <tt>\wedge</tt> || <math>\dagger\,</math> || <tt>\dagger</tt> || <math>\bullet</math> || <tt>\bullet</tt> || <math>\setminus\,</math> || <tt>\setminus</tt> 
|-
| <math>\ddagger\,</math> || <tt>\ddagger</tt> || <math>\cdot</math> || <tt>\cdot</tt> || <math>\wr</math> || <tt>\wr</tt> || <math>\amalg</math> || <tt>\amalg</tt> 
|}

{| class="wikitable" 
|+Set and/or Logic Notation
! Symbol !! Script
|-
| <math>\exists\,</math> || <tt>\exists</tt>
|-
| <math>\forall\,</math> || <tt>\forall</tt>
|-
| <math>\neg\,</math> || <tt>\neg</tt>
|-
| <math>\in\,</math> and <math>\notin\,</math> || <tt>\in</tt> and <tt>\notin</tt>
|-
| <math>\ni\,</math> || <tt>\ni</tt>
|-
| <math>\land\,</math> || <tt>\land</tt>
|-
| <math>\lor\,</math> || <tt>\lor</tt>
|-
| <math>\rightarrow\,</math> || <tt>\rightarrow</tt>
|-
| <math>\implies\,</math> || <tt>\implies</tt>
|-
| <math>\iff\,</math> || <tt>\iff</tt>
|-
| <math>\top\,</math> || <tt>\top</tt>
|-
| <math>\bot\,</math> || <tt>\bot</tt>
|-
| <math>\emptyset\,</math> and <math>\varnothing\,</math> || <tt>\emptyset</tt> and <tt>\varnothing</tt>
|}

{| class="wikitable" align="left"
|+Delimiters
! Symbol !! Script
|-
| <math>\uparrow\,</math> || <tt>\uparrow</tt>
|-
| <math>\Uparrow\,</math> || <tt>\Uparrow</tt>
|-
| <math>\downarrow\,</math> || <tt>\downarrow</tt>
|-
| <math>\Downarrow\,</math> || <tt>\Downarrow</tt>
|-
| <math>\{\,</math> || <tt>\{</tt>
|-
| <math>\}\,</math> || <tt>\}</tt>
|-
| <math>\lceil\,</math> || <tt>\lceil</tt>
|-
| <math>\rceil\,</math> || <tt>\rceil</tt>
|-
| <math>\langle\,</math> || <tt>\langle</tt>
|-
| <math>\rangle\,</math> || <tt>\rangle</tt>
|-
| <math>/\,</math> || <tt>/</tt>
|-
| <math>\backslash\,</math> || <tt>\backslash</tt>
|-
| <math>|\,</math> || <tt><nowiki>|</nowiki></tt>
|-
| <math>\|\,</math> || <tt>\<nowiki>|</nowiki></tt>
|}

{| class="wikitable" align="center" valign="top"
|+Other symbols
! Symbol !! Script
|-
| <math>\partial</math>||<tt>\partial</tt>
|-
| <math>\infty</math>||<tt>\infty</tt>
|-
| <math>\nabla</math>||<tt>\nabla</tt>
|-
| <math>\hbar</math>||<tt>\hbar</tt>
|-
| <math>\Box</math>||<tt>\Box</tt>
|-
| <math>\aleph</math>||<tt>\aleph</tt>
|-
| <math>\ell</math>||<tt>\ell</tt>
|-
| <math>\imath</math>||<tt>\imath</tt>
|-
| <math>\jmath</math>||<tt>\jmath</tt>
|-
| <math>\Re</math>||<tt>\Re</tt>
|-
| <math>\Im</math>||<tt>\Im</tt>
|-
| <math>\wp</math>||<tt>\wp</tt>
|}


{| class="wikitable" align="left"
|+Trigonometric Functions
! Symbol !! Script !! Symbol !! Script !! Symbol !! Script !! Symbol !! Script
|-
| <math>\sin</math>||<tt>\sin</tt>|| <math>\cos</math>||<tt>\cos</tt>|| <math>\tan</math>||<tt>\tan</tt>|| <math>\cot</math>||<tt>\cot</tt>
|-
| <math>\arcsin</math>||<tt>\arcsin</tt>|| <math>\arccos</math>||<tt>\arccos</tt>|| <math>\arctan</math>||<tt>\arctan</tt>|| <math>\arccot</math>||<tt>\arccot</tt>
|-
| <math>\sinh</math>||<tt>\sinh</tt>|| <math>\cosh</math>||<tt>\cosh</tt>|| <math>\tanh</math>||<tt>\tanh</tt>|| <math>\coth</math>||<tt>\coth</tt>
|-
| <math>\sec</math>||<tt>\sec</tt>|| <math>\csc</math>||<tt>\csc</tt>|| || || ||
|}


<!-- Sections remaining: Table 3 onwards from symbols.pdf -->

{{clear}}
=== Summary ===

As you begin to see, typesetting math can be tricky at times. However, because Latex provides so much control, you can get professional quality mathematics typesetting with relatively little effort (once you've had a bit of practice, of course!). It would be possible to keep going and going with math topics because it seems potentially limitless. However, with this tutorial, you should be able to get along sufficiently.

{{TODO|
* introduce symbols from [http://www.andy-roberts.net/misc/latex/tutorial9/symbols.pdf]
* add symbols from [http://www.ctan.org/tex-archive/macros/latex/contrib/wasysym/wasysym.pdf]
* consider adding symbols from [http://www.ctan.org/tex-archive/info/symbols/comprehensive/symbols-letter.pdf] -- the list of nearly all symbols available for LaTeX
* Consider, instead of using the symbols from the above mentioned, using what has already been introduced in [http://en.wikipedia.org/wiki/Math_markup] instead of retyping the tables
* How to box an equation within an align environment
* Color in equations
}}

== Notes ==
<references/>

==Further reading==
* [[meta:Help:Displaying a formula]]: Wikimedia uses a subset of LaTeX commands.

==External links==

* [http://www.artofproblemsolving.com/Wiki/index.php/LaTeX:Symbols LaTeX maths symbols]
* [http://detexify.kirelabs.org detexify]: applet for looking up LaTeX symbols by handwriting them
* [ftp://ftp.ams.org/pub/tex/doc/amsmath/amsldoc.pdf <tt>amsmath</tt> documentation]
* [http://www.thestudentroom.co.uk/wiki/LaTeX LaTeX - The Student Room]
* [http://www.ctan.org/tex-archive/info/symbols/comprehensive/symbols-letter.pdf The Comprehensive LaTeX Symbol List]

<noinclude>
{{LaTeX/Bottom|Page Layout|Advanced Mathematics}}
</noinclude>
[[pl:LaTeX/Matematyka]]
