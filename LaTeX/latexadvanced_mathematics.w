><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>
This page outlines some more advanced uses of mathematics markup using LaTeX. In particular it makes heavy use of the AMS-LaTeX packages supplied by the [[w:American Mathematical Society|American Mathematical Society]].

== Equation numbering ==
The <code>equation</code> environment automatically numbers your equation.
{{LaTeX/Example|code=\begin{equation} 
 f(x)=(x+a)(x+b)
\end{equation}
|render=<math> {f(x)}=(x+a)(x+b) {\color{White} ww} (1) \, </math>}}

You can also use the {{LaTeX/LaTeX|code=\label}} and {{LaTeX/LaTeX|code=\ref}} commands to label and reference equations respectively. Or {{LaTeX/LaTeX|code=\eqref}} but this requires {{LaTeX/Package|amsmath}} package, here is an example showing how to reference formulas using {{LaTeX/LaTeX|code=\ref}} (results in <math>1\,</math> for equation 1) and then using {{LaTeX/LaTeX|code=\eqref}} using the {{LaTeX/Package|amsmath}} package (results in <math>(1)\,</math> for equation 1):
{{LaTeX/Example|code=
\begin{equation} \label{eq:someequation}
5^2 - 5 = 20
\end{equation}

this references the equation \ref{eq:someequation}.
|render=<math>5^2 - 5 = 20\qquad(1)</math><br/>
<math>\text{this references the equation 1.}\,</math>
}}

{{LaTeX/Example|code=
\begin{equation} \label{eq:erl}
a = bq + r
\end{equation}

where \eqref{eq:erl} is true if $a$ and $b$ are integers with $b \neq c$.
|render=<math>a = bq + r\qquad(1)</math><br/>
<math>\text{where (1) is true if }a\text{ and }b\,</math><br/><math>\text{are integers with }b \neq c.</math>
}}

Further information is provided in the [[LaTeX/Labels and Cross-referencing|labels and cross-referencing]] chapter.

To have the enumeration follow from your section or subsection heading, you must use the {{LaTeX/Package|amsmath}} package or use AMS class documents. 
Then enter 
{{LaTeX/Usage|code=
\numberwithin{equation}{section}
}}
to the preamble to get enumeration at the section level or
{{LaTeX/Usage|code=
\numberwithin{equation}{subsection}
}}
to have the enumeration go to the subsection level.

{{LaTeX/Example|code=\documentclass[12pt]{article}
\usepackage{amsmath}
 \numberwithin{equation}{subsection}
 \begin{document}
 \section{First Section}

 \subsection{A subsection}
 \begin{equation}
  L' = {L}{\sqrt{1-\frac{v^2}{c^2}<!-- -->}<!-- -->}
 \end{equation}
\end{document}
|render=
<math> L' = {L}{\sqrt{1-\frac{v^2}{c^2} } } {\color{White} ww} (1.1.1) \, </math>}}

If the style you follow requires putting dots after ordinals (as it is required at least in Polish typography) the {{LaTeX/LaTeX|code=\numberwithin{equation}{subsection} }} command in preamble will result in the equation number in the above example to be rendered in this way: (1.1..1).

To remove the duplicate dot, add following command immediately after
{{LaTeX/LaTeX|code=\numberwithin{equation}{subsection} }}:
{{LaTeX/Usage|code=\renewcommand{\theequation}{\thesection\arabic{equation}<!-- -->} }}

For numbering scheme using {{LaTeX/LaTeX|code=\numberwithin{equation}{subsection} }} use:
{{LaTeX/Usage|code=\renewcommand{\theequation}{\thesubsection\arabic{equation}<!-- -->} }}
in the preamble of the document.

Note: Though it may look like the {{LaTeX/LaTeX|code=\renewcommand}} works by itself, it won't reset the equation number with each new section. It must be used together with manual equation number resetting after each new section beginning or with the much cleaner {{LaTeX/LaTeX|code=\numberwithin}}.

=== Subordinate equation numbering ===
To number subordinate equations in a numbered equation environment, place the part of document containing them in a {{LaTeX/Environment|subequations}} environment:
{{LaTeX/Example|code=\begin{subequations}
Maxwell's equations:
\begin{align}
        B'&=-\partial \times E,\\
        E'&=\partial \times B - 4\pi j,
\end{align}
\end{subequations}
|render=
<math>\text{Maxwell}'\text{s equations:}\,</math>

<math>
\begin{align}
        B'&=-\partial \times E, &\quad&\mathrm{(1.1a)}\\
        E'&=\partial \times B - 4\pi j, &&\mathrm{(1.1b)}
\end{align}
</math>
}}

== Vertically aligning displayed mathematics ==
An often encountered problem with displayed environments ({{LaTeX/Environment|displaymath}} and {{LaTeX/Environment|equation}}) is the lack of any ability to span multiple lines. While it is possible to define lines individually, these will not be aligned.

=== Above and below ===
The {{LaTeX/LaTeX|code=\overset}} and {{LaTeX/LaTeX|code=\underset}} commands<ref name="amsmath"/>  typeset symbols above and below expressions. 
Without AmsTex the same result of  {{LaTeX/LaTeX|code=\overset}}  can be obtained with  {{LaTeX/LaTeX|code=\stackrel}}.
This can be particularly useful for creating new binary relations:
{{LaTeX/Example|code=
\[
 A \overset{!}{=} B; A \stackrel{!}{=} B
\]
|render=
<math>
 A \overset{!}{=} B;~~
 A \stackrel{!}{=} B
\,</math>
}}
or to show usage of [[Wikipedia:L'Hôpital's_rule|L'Hôpital's rule]]:
{{LaTeX/Example|code=
\[
 \lim_{x\to 0}{\frac{e^x-1}{2x}<!-- -->}
 \overset{\left[\frac{0}{0}\right]}{\underset{\mathrm{H}<!-- -->}{=}<!-- -->}
 \lim_{x\to 0}{\frac{e^x}{2}<!-- -->}={\frac{1}{2}<!-- -->}
\]
|render=
<math>
 \lim_{x\to 0}{\frac{e^x-1}{2x} }
 \overset{\left[\frac{0}{0}\right]}{\underset{\mathrm{H} }{=} }
 \lim_{x\to 0}{\frac{e^x}{2} }={\frac{1}{2} }
</math>
}}

It's convenient to define a new operator that will set set the equal sign with H and provided fraction:
{{LaTeX/Usage|code=
\newcommand{\Heq}[1]{\overset{\left[#1\right]}{\underset{\mathrm{H}<!-- -->}{=}<!-- -->}<!-- -->}
}}
to reduce above example to:
{{LaTeX/Usage|code=
\[
 \lim_{x\to 0}{\frac{e^x-1}{2x}<!-- -->}
 \Heq{\frac{0}{0}<!-- -->}
 \lim_{x\to 0}{\frac{e^x}{2}<!-- -->}={\frac{1}{2}<!-- -->}
\]
}}

If the purpose is to make comments on particular parts of an equation, the {{LaTeX/LaTeX|code=\overbrace}} and {{LaTeX/LaTeX|code=\underbrace}} commands may be more useful, however they have a different syntax:
{{LaTeX/Example|code=
\[
 z = \overbrace{
   \underbrace{x}_\text{real} + 
   \underbrace{iy}_\text{imaginary}
  }^\text{complex number}
\]
|render=<math>
 z = \overbrace{
   \underbrace{x}_\text{real} + 
   \underbrace{iy}_\text{imaginary}
  }^\text{complex number}
</math>}}

Sometimes the comments are longer than the formula being commented on, which can cause spacing problems. These can be removed using the {{LaTeX/LaTeX|code=\mathclap}} command<ref name="mathtools"/>:
{{LaTeX/Example|code=
\[
 y = a + f(\underbrace{b x}_{
                    \ge 0 \text{ by assumption}<!-- -->}) 
   = a + f(\underbrace{b x}_{
          \mathclap{\ge 0 \text{ by assumption}<!-- -->}<!-- -->})
\]
|render=
[[Image:LaTeX-mathclap-example.png|350px]]
}}

Alternatively, to use brackets instead of braces use {{LaTeX/LaTeX|code=\underbracket}} and {{LaTeX/LaTeX|code=\overbracket}} commands<ref name="mathtools">requires the {{LaTeX/Package|mathtools}} package</ref>:
{{LaTeX/Example|code=
\[
 z = \overbracket[3pt]{
     \underbracket{x}_{\text{real}<!---->} +
     \underbracket[0.5pt][7pt]{iy}_{\text{imaginary}<!---->}
     }^{\text{complex number}<!---->} 
\]
|render=[[Image:LaTeX-mathtools-brackets.png|170px]]}}

The optional arguments set the rule thickness and bracket height respectively:
{{LaTeX/Usage|code=
\underbracket[rule thickness][bracket height]{argument}_{text below}
}}

The {{LaTeX/LaTeX|code=\xleftarrow}} and {{LaTeX/LaTeX|code=\xrightarrow}} commands<ref name="amsmath"/> produce arrows which extend to the length of the text. Yet again, the syntax is different: the optional argument (using <code>[</code> & <code>]</code>) specifies the subscript, and the mandatory argument (using <code>{</code> & <code>}</code>) specifies the superscript (this can be left empty).
{{LaTeX/Example|code=
\[
 A \xleftarrow{\text{this way}<!-- -->} B 
  \xrightarrow[\text{or that way}]{} C
\]
|render=
<math>
A \xleftarrow{\text{this way}} B \xrightarrow[\text{or that way}]{} C
\,</math>
}}

For more extensible arrows, you must use {{LaTeX/Package|mathtools}} package:
{{LaTeX/Example|code=
\[
 a \xleftrightarrow[under]{over} b\\
%
 A \xLeftarrow[under]{over} B\\
%
 B \xRightarrow[under]{over} C\\
%
 C \xLeftrightarrow[under]{over} D\\
%
 D \xhookleftarrow[under]{over} E\\
%
 E \xhookrightarrow[under]{over} F\\
%
 F \xmapsto[under]{over} G\\
\]
|render=[[Image:LaTeX-mathtools-arrows.png|90px]]}}

and for harpoons:
{{LaTeX/Example|code=
\[
 H \xrightharpoondown[under]{over} I\\
%
 I \xrightharpoonup[under]{over} J\\
%
 J \xleftharpoondown[under]{over} K\\
%
 K \xleftharpoonup[under]{over} L\\
%
 L \xrightleftharpoons[under]{over} M\\
%
 M \xleftrightharpoons[under]{over} N
\]
|render=[[Image:LaTeX-mathtools-harpoons.png|90px]]}}

=== {{LaTeX/Environment|align}} and {{LaTeX/Environment|align*}} ===
The {{LaTeX/Environment|align}} and {{LaTeX/Environment|align*}} environments<ref name="amsmath">Requires the {{LaTeX/Package|amsmath}} package</ref> are used for arranging equations of multiple lines. As with matrices and tables, {{LaTeX/LaTeX|code=\\}} specifies a line break, and {{LaTeX/LaTeX|code=&}} is used to indicate the point at which the lines should be aligned.

The {{LaTeX/Environment|align*}} environment is used like the {{LaTeX/Environment|displaymath}} or {{LaTeX/Environment|equation*}} environment:
{{LaTeX/Example|code=
\begin{align*}
 f(x) &= (x+a)(x+b) \\
 &= x^2 + (a+b)x + ab
\end{align*}
|render=
<math>\begin{align}
f(x) &= (x+a)(x+b) \\
&= x^2 + (a+b)x + ab
\end{align}\,</math>
}}
To force numbering on a specific line, use the {{LaTeX/LaTeX|code=\tag{...}<!-- -->}} command before the linebreak.

The {{LaTeX/Environment|align}} is similar, but automatically numbers each line like the {{LaTeX/Environment|equation}} environment.  Individual lines may be referred to by placing a {{LaTeX/LaTeX|code=\label{...}<!-- -->}} before the linebreak. The {{LaTeX/LaTeX|code=\nonumber}} or {{LaTeX/LaTeX|code=\notag}} command can be used to suppress the number for a given line:
{{LaTeX/Example|code=
\begin{align}
 f(x) &= x^4 + 7x^3 + 2x^2 \nonumber \\
 &\qquad {} + 10x + 12
\end{align}
|render=
<math>\begin{align}
f(x) &= x^4 + 7x^3 + 2x^2 \\
&\qquad {} + 10x + 12 \qquad \qquad (3)
\end{align}</math>
}}
Notice that we've added some indenting on the second line.  Also, we need to insert the double braces {} before the + sign because otherwise latex won't create the correct spacing after the + sign.  The reason for this is that without the braces, latex interprets the + sign as a unary operator, instead of the binary operator that it really is.

=== Braces spanning multiple lines ===
Additional {{LaTeX/LaTeX|code=&}}'s on a single line will specify multiple "equation columns", each of which is aligned. If you want a brace to continue across a new line, do the following:
{{LaTeX/Example|code=
\begin{align}
 f(x) &= \pi \left\{ x^4 + 7x^3 + 2x^2 \right.\nonumber\\
 &\qquad \left. {} + 10x + 12 \right\}
\end{align}
|render=
<math>\begin{align}
f(x) &= \pi \left\{ x^4 + 7x^3 + 2x^2 \right.\\
&\qquad \left. {} + 10x + 12 \right\}  \qquad \qquad (4)
\end{align}</math>
}}

The sizes of the left and right braces are made equal and matching the typical size of the symbols between them by using {{LaTeX/LaTeX|code=\left\{}} and {{LaTeX/LaTeX|code=\right\}<!-- -->}}.  But because these two commands occur on different lines, we need to balance them with the {{LaTeX/LaTeX|code=\right.}} and {{LaTeX/LaTeX|code=\left.}} commands.  (Normally these aren't needed if the formula is on one line.)  Alternatively, you can control the size of the braces manually with the {{LaTeX/LaTeX|code=\big}}, {{LaTeX/LaTeX|code=\Big}}, {{LaTeX/LaTeX|code=\bigg}}, {{LaTeX/LaTeX|code=\Bigg}} commands.

To automatically match sizes of opening and closing braces in a tall equation use {{LaTeX/LaTeX|code=\vphantom}} command:
{{LaTeX/Example|code=
\begin{align}
 A &=     \left(\int_t XXX       \right.\nonumber\\
   &\qquad \left.\vphantom{\int_t} YYY \dots \right)
\end{align}
|render=
<math>
\begin{align}
 A &=     \left(\int_t XXX\right.\\
   &\qquad YYY \dots \biggr)\qquad\qquad \mathrm{(5)}
\end{align}
</math>
}}

=== The {{LaTeX/Environment|cases}} environment ===
The {{LaTeX/Environment|cases}} environment<ref name="amsmath"/> allows the writing of piecewise functions:
{{LaTeX/Example|code=
\[
 u(x) = 
  \begin{cases} 
   \exp{x} & \text{if } x \geq 0 \\
   1       & \text{if } x < 0
  \end{cases}
\]
|render=
<math>
u(x) = 
\begin{cases}
\exp{x} & \text{if } x \geq 0 \\
1       & \text{if } x < 0
\end{cases}
</math>
}}
Just like before, you don't have to take care of definition or alignment of columns, LaTeX will do it for you.

Unfortunately, it sets the internal math environment to text style, leading to such result:

<math>
a =
 \begin{cases}
  \int x\, dx\\
  b^2
 \end{cases}
</math>

To force display style for equations inside this construct, use {{LaTeX/Environment|dcases}} environment<ref name=mathtools/>:
{{LaTeX/Example|code=
\[
 a =
   \begin{dcases}
     \int x\, dx\\
     b^2
   \end{dcases}
\]
|render=
<math>
a =
 \begin{cases}
  \displaystyle\int x\, dx\\
  \displaystyle b^2
 \end{cases}
</math>}}

Often the second column consists mostly of normal text, to set it in normal roman font of the document use {{LaTeX/Environment|dcases*}} environment<ref name="mathtools"/>:
{{LaTeX/Example|code=
\[
 f(x) = \begin{dcases*}
        x  & when $x$ is even\\
        -x & when $x$ is odd
        \end{dcases*}
\]
|render=
<math>
 f(x) = \begin{cases}
 x  & \text{when }x\text{ is even}\\
 -x & \text{when }x\text{ is odd}
 \end{cases}
</math>}}

=== Other environments ===
Although {{LaTeX/Environment|align}} and {{LaTeX/Environment|align*}} are the most useful, there are several other environments which may also be of interest:
{|class="wikitable"
! Environment name
! Description
! Notes
|-
| {{LaTeX/Environment|eqnarray}} and {{LaTeX/Environment|eqnarray*}}
| Similar to {{LaTeX/Environment|align}} and {{LaTeX/Environment|align*}}
| Not recommended since spacing is inconsistent
|-
| {{LaTeX/Environment|multline}} and {{LaTeX/Environment|multline*}}<ref name="amsmath"/>
| First line left aligned, last line right aligned
| Equation number aligned vertically with first line and not centered as with other other environments.
|-
| {{LaTeX/Environment|gather}} and {{LaTeX/Environment|gather*}}<ref name="amsmath"/>
| Consecutive equations without alignment
| 
|-
| {{LaTeX/Environment|flalign}} and {{LaTeX/Environment|flalign*}}<ref name="amsmath"/>
| Similar to {{LaTeX/Environment|align}}, but left aligns first equation column, and right aligns last column
| 
|-
| {{LaTeX/Environment|alignat}} and {{LaTeX/Environment|alignat*}}<ref name="amsmath"/>
| Takes an argument specifying number of columns. Allows to control explicitly the horizontal space between equations
| You can calculate the number of columns by counting {{LaTeX/LaTeX|code=&}} characters in  a line, adding 1 and dividing the result by 2
|}

There are also few environments that don't form a math environment by themselves and can be used as building blocks for more elaborate structures:
{|class="wikitable"
! Math environment name
! Description
! Notes
|-
| {{LaTeX/Environment|gathered}}<ref name="amsmath"/>
| Allows to gather few equations to be set under each other and assigned a single equation number
|-
| {{LaTeX/Environment|split}}<ref name="amsmath"/>
| Similar to {{LaTeX/Environment|align*}}, but used inside another displayed mathematics environment
| 
|-
| {{LaTeX/Environment|aligned}}<ref name="amsmath"/>
| Similar to {{LaTeX/Environment|align}}, to be used inside another mathematics environment.
|
|-
| {{LaTeX/Environment|alignedat}}<ref name="amsmath"/>
| Similar to {{LaTeX/Environment|alignat}}, and just as it, takes an additional argument specifying number of columns of equations to set.
|
|}

For example:
{{LaTeX/Example|code=
\begin{equation}
 \left.\begin{aligned}
        B'&=-\partial \times E,\\
        E'&=\partial \times B - 4\pi j,
       \end{aligned}
 \right\}
 \qquad \text{Maxwell's equations}
\end{equation}
|render=
<math>
\left.\begin{align}
        B'&=-\partial \times E,\\
        E'&=\partial \times B - 4\pi j,
\end{align}\right\}\quad\text{Maxwell}'\text{s equations}\qquad\mathrm{(1.1)}
</math>
}}
{{LaTeX/Example|code=
\begin{alignat}{2}
 \sigma_1 &= x + y  &\quad \sigma_2 &= \frac{x}{y} \\	
 \sigma_1' &= \frac{\partial x + y}{\partial x} & \sigma_2' 
    &= \frac{\partial \frac{x}{y}<!---->}{\partial x}
\end{alignat}
|render=
<math>\begin{align}
 \sigma_1 &= x + y  &\sigma_2 &= \frac{x}{y}  &\qquad&\qquad&(1)   \\	
 \sigma_1' &= \frac{\partial x + y}{\partial x} & \sigma_2' &= \frac{\partial \frac{x}{y}}{\partial x} &&&(2)
\end{align}</math>
}}

== Indented Equations ==

In order to indent an equation, you can set {{LaTeX/Parameter|fleqn}} in the document class and then specify a certain value for {{LaTeX/LaTeX|code=\mathindent}} variable:
{{LaTeX/Example|code=
\documentclass[a4paper,fleqn]{report}
\usepackage{amsmath}
\setlength{\mathindent}{1cm}
\begin{document}
\noindent Euler's formula is given below:
\begin{equation*}
 e^{ix} = \cos{x} + i \sin{x}
\end{equation*}
\noindent This is a very important formula.
\end{document}
|render=
[[File:LaTeX - Indented Equations.png]]
}}

== Page breaks in math environments ==
To suggest LaTeX a page break inside one of {{LaTeX/Package|amsmath}} environments you may use the {{LaTeX/LaTeX|code=\displaybreak}} command before line break. Just like with {{LaTeX/LaTeX|code=\pagebreak}}, {{LaTeX/LaTeX|code=\displaybreak}} can take an optional argument between 0 and 4 denoting the level of desirability of a page break in specific break. While 0 means "it is permissible to break here", 4 forces a break. No argument means the same as 4.

Alternatively, you may enable automatic page breaks in math environments with {{LaTeX/LaTeX|code=\allowdisplaybreaks}}. It can too have an optional argument denoting permissiveness of page breaks in equations. Similarly, 1 means "allow page breaks but avoid them" and 4 means "break whenever you want". You can prohibit a page break after a given line using {{LaTeX/LaTeX|code=\\*}}.

LaTeX will insert a page break into a long equation if it has additional text added using {{LaTeX/LaTeX|code=\intertext{}<!-- -->}} without any additional commands though.

Specific usage may look like this:
{{LaTeX/Example|code=
\begin{align*}
 &\vdots\\ 
 &=12+7 \int_0^2
  \left(
    -\frac{1}{4}\left(e^{-4t_1}+e^{4t_1-8}\right)
  \right)\,dt_1\displaybreak[3]\\
 &= 12-\frac{7}{4}\int_0^2 e^{-4t_1}+e^{4t_1-8}\,dt_1\\
 &\vdots % 
\end{align*}
|render=[[Image:LaTeX-displaybreak-in-math.png|400px]]
}}

== Boxed Equations ==
For a single equation, with the tag outside the box, use {{LaTeX/LaTeX|code=\boxed{}<!-- -->}}:
{{LaTeX/Example|code=
\begin{equation}
 \boxed{x^2+y^2 = z^2}
\end{equation}
|render=
[[Image:LaTeX-boxed-equation.png|250px]]
}}

If you want the entire line or several equations to be boxed, use a {{LaTeX/Environment|minipage}} inside an {{LaTeX/LaTeX|code=\fbox{}<!---->}}:
{{LaTeX/Example|code=
\fbox{
 \addtolength{\linewidth}{-2\fboxsep}%
 \addtolength{\linewidth}{-2\fboxrule}%
 \begin{minipage}{\linewidth}
  \begin{equation}
   x^2+y^2=z^2
  \end{equation}
 \end{minipage}
}
|render=
[[Image:LaTeX-boxed-formula-minipage.png|400px]]
}}

There is also the mathtools {{LaTeX/LaTeX|code=\Aboxed{}<!-- -->}} which is able to box across alignment marks
{{LaTeX/Example|code=
\begin{align*}
\Aboxed{ f(x) & = \int h(x)\, dx} \\
              & = g(x)
\end{align*}
}}

== Custom operators ==
Although many common [[../Mathematics#Operators|operators]] are available in LaTeX, sometimes you will need to write your own, for example to typeset the [[w:Arg max|argmax]] operator. The {{LaTeX/LaTeX|code=\operatorname}} and {{LaTeX/LaTeX|code=\operatorname*}} commands<ref name="amsmath">Requires {{LaTeX/Package|amsmath}} package</ref> display a custom operators, the <code>*</code> version sets the underscored option underneath like the {{LaTeX/LaTeX|code=\lim}} operator:
{{LaTeX/Example|code=
\[
 \operatorname{arg\,max}_a f(a) 
 = \operatorname*{arg\,max}_b f(b)
\]
|render=
<math>\operatorname{arg\,max}_a f(a) = \underset{b}\operatorname{arg\,max} f(b)</math>
}}

However if the operator is frequently used, it is preferable to keep within the LaTeX ideal of markup to define a new operator. The {{LaTeX/LaTeX|code=\DeclareMathOperator}} and {{LaTeX/LaTeX|code=\DeclareMathOperator*}} commands<ref name="amsmath"/> are specified in the header of the document:
{{LaTeX/Usage|code=
\DeclareMathOperator*{\argmax}{arg\,max}<!---->
}}
This defines a new command which may be referred to in the body:
{{LaTeX/Example|code=
\[
 \argmax_c f(c)
\]
|render=
<math>\underset{c}{\operatorname{arg\,max}} f(c)</math>
}}

== Advanced formatting ==
=== Limits ===
There are defaults for placement of subscripts and superscripts. For example, limits for the <tt>lim</tt> operator are usually placed below the symbol, like this:
{{LaTeX/Example|code=
\begin{equation}
  \lim_{a\to \infty} \tfrac{1}{a}
\end{equation}
|render=
<math>\lim_{a\to \infty} \tfrac{1}{a}</math>
}}

To override this behavior use the {{LaTeX/LaTeX|code=\nolimits}} operator:
{{LaTeX/Example|code=
\begin{equation}
  \lim\nolimits_{a\to \infty} \tfrac{1}{a}
\end{equation}
|render=
<math>\lim\nolimits_{a\to \infty} \tfrac{1}{a}</math>
}}

A <tt>lim</tt> in running text (inside {{LaTeX/LaTeX|code=$...$}}) will have its limits placed on the side, so that additional leading won't be required. To override this behavior use {{LaTeX/LaTeX|code=\limits}} command.

Similarly one can put subscripts under a symbol that usually have them on the side:
{{LaTeX/Example|code=
\begin{equation}
  \int_a^b x^2
\end{equation}
|render=
<math>\int_a^b x^2</math>
}}

Limits below and under:
{{LaTeX/Example|code=
\begin{equation}
  \int\limits_a^b x^2
\end{equation}
|render=
<math>\int\limits_a^b x^2</math>
}}

To change default placement in all instances of summation-type symbol to the side add {{LaTeX/Parameter|nosumlimits}} option to {{LaTeX/Package|amsmath}} package. To change placement for integral symbols add {{LaTeX/Parameter|intlimits}} to options and {{LaTeX/Parameter|nonamelimits}} to change the default for named operators like <tt>det</tt>, <tt>min</tt>, <tt>lim</tt>...

=== Subscripts and superscripts ===

While you can place symbols in sub- or superscript in summation style symbols with the above introduced {{LaTeX/LaTeX|code=\nolimits}}:
{{LaTeX/Example|code=
\begin{equation}
  \sum\nolimits' C_n
\end{equation}
|render=
<math>\sum\nolimits' C_n</math>
}}

It's impossible to mix them with typical usage of such symbols:
{{LaTeX/Example|code=
\begin{equation}
  \sum_{n=1}\nolimits' C_n
\end{equation}
|render=
<math>\sum_{n=1}\nolimits' C_n</math>
}}

To add both prime and a limit to a symbol, one have to use {{LaTeX/LaTeX|code=\sideset}} command:
{{LaTeX/Example|code=
\begin{equation}
  \sideset{}{'}\sum_{n=1}C_n
\end{equation}
|render=
<math>\sideset{}{'}\sum_{n=1}C_n</math>
}}

It's very flexible, for example, to put letters in each corner of the symbol use this command:
{{LaTeX/Example|code=
\begin{equation}
  \sideset{_a^b}{_c^d}\sum
\end{equation}
|render=
<math>\sideset{_a^b}{_c^d}\sum</math>
}}

If you wish to place them on the corners of an arbitrary symbol, you should use {{LaTeX/LaTeX|code=\fourIdx}} from the {{LaTeX/Package|fouridx}} package.

=== Multiline subscripts ===
To produce multiline subscript use {{LaTeX/LaTeX|code=\substack}} command:
{{LaTeX/Example|code=
\begin{equation}
  \prod_{\substack{
            1\le i \le n\\
            1\le j \le m}<!---->}
     M_{i,j}
\end{equation}
|render=
<math>\prod_{1 \le i \le n \atop 1 \le j \le m \ } M_{i,j}</math>
}}

== Text in aligned math display ==
To add small interjections in math environments use {{LaTeX/LaTeX|code=\intertext}} command:
{{LaTeX/Example|code=
\begin{minipage}{3in}
\begin{align*}
\intertext{If}
   A &= \sigma_1+\sigma_2\\
   B &= \rho_1+\rho_2\\
\intertext{then}
C(x) &= e^{Ax^2+\pi}+B
\end{align*} 
\end{minipage}
|render=
[[Image:Latex-intertext.png|250px]]
}}
Note that usage of this command doesn't change alignment, as would stopping and restarting the {{LaTeX/Environment|align}} environment.

Also in this example, the mathtools {{LaTeX/LaTeX|code=\shortintertext{} }} could have been used instead of intertext to reduce the amount of vertical whitespace added between the lines.

== Changing font size ==

Probably a rare event, but there may be a time when you would prefer to have some control of the size. For example, using text-mode maths, by default a simple fraction will look like this: <math>\textstyle \frac{a}{b}</math> where as you may prefer to have it displayed larger, like when in display mode, but still keeping it in-line, like this: <math>\displaystyle \frac{a}{b} </math>.

A simple approach is to utilize the predefined sizes for maths elements:

{| class="wikitable"
! Size command
! Description
|-
| {{LaTeX/LaTeX|code=\displaystyle}}
| Size for equations in display mode
|-
| {{LaTeX/LaTeX|code=\textstyle}}
| Size for equations in text mode
|-
| {{LaTeX/LaTeX|code=\scriptstyle}}
| Size for first sub/superscripts
|-
| {{LaTeX/LaTeX|code=\scriptscriptstyle}}
| Size for subsequent sub/superscripts
|}

A classic example to see this in use is typesetting continued fractions (though it's better to use the {{LaTeX/LaTeX|code=\cfrac}} command<ref name="amsmath"/> described in the [[LaTeX/Mathematics#Continued_fractions|Mathematics]] chapter over the method provided below). The following code provides an example.
{{LaTeX/Example|code=
\begin{equation}
  x = a_0 + \frac{1}{a_1 + \frac{1}{a_2 + \frac{1}{a_3 + a_4}<!---->}<!---->}
\end{equation}
|render=
<math>x = a_0 + \frac{1}{a_1 + \frac{1}{a_2 + \frac{1}{a_3 + a_4}}}</math>
}}

As you can see, as the fractions continue, they get smaller (although they will not get any smaller as in this example, they have reached the {{LaTeX/LaTeX|code=\scriptstyle}} limit). If you wanted to keep the size consistent, you could declare each fraction to use the display style instead, e.g.:
{{LaTeX/Example|code=
\begin{equation}
  x = a_0 + \frac{1}{\displaystyle a_1 
          + \frac{1}{\displaystyle a_2 
          + \frac{1}{\displaystyle a_3 + a_4}<!---->}<!---->}
\end{equation}
|render=
<math>   x = a_0 + \frac{1}{\displaystyle a_1 
           + \frac{1}{\displaystyle a_2 
           + \frac{1}{\displaystyle a_3 + a_4}}}
</math>
}}

Another approach is to use the {{LaTeX/LaTeX|code=\DeclareMathSizes}} command to select your preferred sizes. You can only define sizes for {{LaTeX/LaTeX|code=\displaystyle}}, {{LaTeX/LaTeX|code=\textstyle}}, etc. One potential downside is that this command sets the global maths sizes, as it can only be used in the document preamble.

However, it's fairly easy to use: {{LaTeX/LaTeX|code=\DeclareMathSizes{ds}{ts}{ss}{sss}<!---->}}, where ''ds'' is the ''display size'', ''ts'' is the ''text size'', etc. The values you input are assumed to be point (pt) size.

NB the changes only take place if the value in the first argument matches the current document text size. It is therefore common to see a set of declarations in the preamble, in the event of the main font being changed. E.g.,
{{LaTeX/Usage|code=
\DeclareMathSizes{10}{18}{12}{8}   % For size 10 text
\DeclareMathSizes{11}{19}{13}{9}   % For size 11 text
\DeclareMathSizes{12}{20}{14}{10}  % For size 12 text
}}

== Forcing \displaystyle for all math in a document ==
Put
{{LaTeX/Usage|code=
\everymath{\displaystyle}
}}
before \begin{document} to force all math to \displaystyle.

{{TODO|
* Consider to add stuff from mathtools
}}

== Notes ==
<references/>

<noinclude>
{{LaTeX/Bottom|Mathematics|Theorems}}
</noinclude>
