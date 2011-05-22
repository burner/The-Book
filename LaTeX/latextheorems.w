><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

With "[[w:Theorem|theorem]]" we can mean any kind of labelled enunciation that we want to look separated from the rest of the text and with sequential numbers next to it. This approach is commonly used for theorems in mathematics, but can be used for anything. LaTeX provides a command that will let you easily define any theorem-like enunciation.

== Basic theorems ==
First of all, make sure you have the amsthm package enabled:

<source lang="latex">
\usepackage{amsthm}
</source>

The easiest is the following:
<source lang="latex">
\newtheorem{name}{Printed output}
</source>
put it in the preamble. The first argument is the name you will use to reference it, the second argument is the output LaTeX will print whenever you use it. For example:
<source lang="latex">
\newtheorem{mydef}{Definition}
</source>

will define the <code>mydef</code> environment; if you use it like this:
<source lang="latex">
\begin{mydef}
Here is a new definition
\end{mydef}
</source>

It will look like this:
:'''Definition 3''' ''Here is a new definition''
with line breaks separating it from the rest of the text. 

== Theorem counters ==
Often the counters are determined by section, for example "Theorem 2.3" refers to the 3rd theorem in the 2nd section of a document. In this case, specify the theorem as follows:
<source lang="latex">
\newtheorem{name}{Printed output}[numberby]
</source>

where ''numberby'' specifies the [[LaTeX/Document Structure#Sectioning Commands|section level]] (section/subsection/etc.) at which the numbering is to take place.

By default, each theorem uses its own counter. However it is common for similar types of theorems (e.g. Theorems, Lemmas and Corollaries) to share a counter. In this case, define subsequent theorems as:
<source lang="latex">
\newtheorem{name}[counter]{Printed output}
</source>

where ''counter'' is the name of the counter to be used. Usually this will be the name of the master theorem.

You can also create a theorem environment that is not numbered by using the <code>newtheorem*</code> command<ref name="amsthm">Requires the <code>amsthm</code> package</ref>.  For instance,
<source lang="latex">
\newtheorem*{mydef}{Definition}
</source>
defines the <code>mydef</code> environment, which will generate definitions without numbering. This requires <code>amsthm</code> package.

== Proofs ==
The <code>proof</code> environment<ref name="amsthm"/> can be used for adding the proof of a theorem. The basic usage is:
<source lang="latex">
\begin{proof}
Here is my proof
\end{proof}
</source>

It just adds ''Proof'' in italics at the beginning of the text given as argument and a white square ([[w:Q.E.D|Q.E.D]] symbol, also known as a [[w:Tombstone (typography)|tombstone]]) at the end of it. If you are writing in another language than English, just use [[../Internationalization/|babel]] with the right argument and the word ''Proof'' printed in the output will be translated accordingly; anyway, in the source the name of the environment remains <code>proof</code>.

If you would like to manually name the proof, include the name in square brackets:
<source lang="latex">
\begin{proof}[Proof of important theorem]
Here is my important proof
\end{proof}
</source>

If the last line of the proof is displayed math then the Q.E.D. symbol will appear on a subsequent empty line. To put the Q.E.D. symbol at the end of the last line, use the <code>\qedhere</code> command:
<source lang="latex">
\begin{proof}
Here is my proof:
\[
a^2 + b^2 = c^2 \qedhere
\]
\end{proof}
</source>
The method above does not work with the deprecated environment <code>eqnarray*</code>. Here is a workaround:
<source lang="latex">
\begin{proof}
Here is my proof:
\begin{eqnarray*}
a^2 + b^2 = c^2
\end{eqnarray*}
\vspace{-1.3cm}\[\qedhere\]
\end{proof}
</source>

To use a custom Q.E.D. symbol, redefine the <tt>\qedsymbol</tt> command. To hide the Q.E.D. symbol altogether, redefine it to be blank:
<source lang="latex">
\renewcommand{\qedsymbol}{}
</source>

== Theorem styles ==
It adds the possibility to change the output of the environments defined by <code>\newtheorem</code> using the <code>\theoremstyle</code> command<ref name="amsthm"/> command in the header:
<source lang="latex">
\theoremstyle{stylename}
</source>
the argument is the style you want to use. All subsequently defined theorems will use this style. Here is a list of the possible pre-defined styles:

{|class="wikitable"
!<tt>stylename</tt>!!Description
|-
|<tt>plain</tt>|| Used for theorems, lemmas, propositions, etc. (default)
|-
|<tt>definition</tt>||Used for definitions and examples
|-
|<tt>remark</tt>||Used for remarks and notes
|}

=== Custom styles ===
To define your own style, use the <code>\newtheoremstyle</code> command<ref name="amsthm"/>:
<source lang="latex">
\newtheoremstyle{stylename}% name of the style to be used
  {spaceabove}% measure of space to leave above the theorem. E.g.: 3pt
  {spacebelow}% measure of space to leave below the theorem. E.g.: 3pt
  {bodyfont}% name of font to use in the body of the theorem
  {indent}% measure of space to indent
  {headfont}% name of head font
  {headpunctuation}% punctuation between head and body
  {headspace}% space after theorem head; " " = normal interword space
  {headspec}% Manually specify head
</source>
(Any arguments that are left blank will assume their default value).  Here is an example ''headspec'':
<source lang="latex">
\thmname{#1}\thmnumber{ #2}:\thmnote{ #3}
</source>
which would look something like:<br/>
'''Definition 2''': Topology<br/>
for the following:
<source lang="latex">
\begin{definition}[Topology]...
</source>
(The note argument, which in this case is Topology, is always optional, but will not appear by default unless you specify it as above in the head spec).<br/>

==Conflicts==

The theorem environment conflicts with other environments,  for example ''wrapfigure''.
A work around is to redefine theorem, for example the following way:
<source lang="latex">
% Fix latex
\def\smallskip{\vskip\smallskipamount}
\def\medskip{\vskip\medskipamount}
\def\bigskip{\vskip\bigskipamount}

% Hand made theorem
\newcounter{thm}[section]
\renewcommand{\thethm}{\thesection.\arabic{thm}}
\def\claim#1{\par\medskip\noindent\refstepcounter{thm}\hbox{\bf \arabic{chapter}.\arabic{section}.\arabic{thm}. #1.}
\it\ %\ignorespaces
}
\def\endclaim{
\par\medskip}
\newenvironment{thm}{\claim}{\endclaim}
</source>

In this case theorem looks like:

<source lang="latex">
\begin{thm}{Claim}\label{lyt-prob} 
Let it be.
Then you know.
\end{thm}
</source>

== Notes ==
<references/>

== External links ==
* [ftp://ftp.ams.org/pub/tex/doc/amscls/amsthdoc.pdf <tt>amsthm</tt> documentation]

<noinclude>
{{LaTeX/Bottom|Mathematics|Labels and Cross-referencing}}
</noinclude>
