><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>
Here are some topics that are not really necessary to write a proper document, but could help you making your life easier and giving you some details to modify.

== Adding your own counters ==
In LaTeX it is fairly easy to create new counters and even counters that reset automatically when another counter is increased (think subsection in a section for example). With the command
<source lang="latex">
\newcounter{NameOfTheNewCounter}
</source>
you create a new counter that is automatically set to zero.
If you want the counter to be reset to zero every time another counter is increased, use:
<source lang="latex">
\newcounter{NameOfTheNewCounter}[NameOfTheOtherCounter]
</source>

To increase the counter, either use 
<source lang="latex">
\stepcounter{NameOfTheNewCounter}
</source>
or
<source lang="latex">
\refstepcounter{NameOfTheNewCounter} % used for labels and cross referencing
</source>
or
<source lang="latex">
\addtocounter{NameOfTheNewCounter}{number}
</source>
here the number can also be negative. For automatic reseting you need to use <code>\stepcounter</code>.

The values of the counters can be easily found by, for example:
<source lang="latex">
\arabic{NameOfTheNewCounter}
</source>
Instead of <code>\arabic</code> you could also use <code>\alph</code>, <code>\Alph</code>, <code>\roman</code>, or <code>\Roman</code>.

Here is an example for recreating something similar to a section and subsection counter that already exist in LaTeX:
<source lang="latex">
\newcounter{mysection}
\newcounter{mysubsection}[mysection]
\addtocounter{mysection}{2} % set them to some other numbers than 0
\addtocounter{mysection}{10} % same
%
\arabic{mysection}.\arabic{mysubsection}
bla bla

\stepcounter{mysection}
\arabic{mysection}.\arabic{mysubsection}
bla bla

\stepcounter{mysubsection}
\arabic{mysection}.\arabic{mysubsection}
bla bla

\addtocounter{mysubsection}{25}
\arabic{mysection}.\arabic{mysubsection}
bla bla and more bla bla
</source>

== Boxes ==

LaTeX builds up its pages by pushing around boxes. At first, each letter is a little box, which is then glued to other letters to form words. These are again glued to other words, but with special glue, which is elastic so that a series of words can be squeezed or stretched as to exactly fill a line on the page.

Admittedly, this is a very simplistic description of what really happens, but the point is that TeX operates with glue and boxes. Letters are not the only things that can be boxes. One can put virtually everything into a box, including other boxes. Each box will then be handled by LaTeX as if it were a single letter.

The past chapters have already dealt with some boxes, although they weren't described as such. The tabular environment and the <code>\includegraphics</code>, for example, both produce a box. This means that one can easily arrange two tables or images side by side. You just have to make sure that their combined width is not larger than the <code>\textwidth</code>.

You can also pack a paragraph of your choice into a box with either the <source lang="latex">\parbox[pos]{width}{text}</source>
command or the <source lang="latex">\begin{minipage}[pos]{width} text \end{minipage}</source>
environment. The ''pos'' parameter can take one of the letters <tt>c</tt>, <tt>t</tt> or <tt>b</tt> to control the vertical alignment of the box, relative to the baseline of the surrounding text. width takes a length argument specifying the width of the box. The main difference between a <tt>minipage</tt> and a <tt>\parbox</tt> is that you cannot use all commands and environments inside a parbox, while almost anything is possible in a minipage.

While <tt>\parbox</tt> packs up a whole paragraph doing line breaking and everything, there is also a class of boxing commands that operates only on horizontally aligned material. We already know one of them; it’s called <tt>\mbox</tt>. It simply packs up a series of boxes into another one, and can be used to prevent LaTeX from breaking two words. As you can put boxes inside boxes, these horizontal box packers give you ultimate flexibility.

<source lang="latex">
\makebox[width][pos]{text}
</source>

''width'' defines the width of the resulting box as seen from the outside (This means it can be smaller than the material inside the box. You can even set the width to 0pt so that the text inside the box will be typeset without influencing the surrounding boxes). Besides the ''length'' expressions, you can also use <tt>\width, \height, \depth,</tt> and <tt>\totalheight</tt> in the width parameter. They are set from values obtained by measuring the typeset text. The ''pos'' parameter takes a one letter value: '''c'''enter, flush'''l'''eft, flush'''r'''ight, or '''s'''pread the text to fill the box.

The command <tt>\framebox</tt> works exactly the same as <tt>\makebox</tt>, but it draws a box around the text.

The following example shows you some things you could do with the <tt>\makebox</tt> and <tt>\framebox</tt> commands:
{|
|<source lang="latex">
\makebox[\textwidth]{%
c e n t r a l}\par
\makebox[\textwidth][s]{%
s p r e a d}\par
\framebox[1.1\width]{Guess I’m
framed now!} \par
\framebox[0.8\width][r]{Bummer,
I am too wide} \par
\framebox[1cm][l]{never
mind, so am I}
Can you read this?
</source>
|
[[Image:Latex example box test.png|300px]]
|}

Now that we control the horizontal, the obvious next step is to go for the vertical. No problem for LaTeX. The
<source lang="latex">
\raisebox{lift}[extend-above-baseline][extend-below-baseline]{text}
</source>
command lets you define the vertical properties of a box. You can use <tt>\width, \height, \depth</tt>, and <tt>\totalheight</tt> in the first three parameters, in order to act upon the size of the box inside the text argument:

{|
|<source lang="latex">
\raisebox{0pt}[0pt][0pt]{\Large%
\textbf{Aaaa\raisebox{-0.3ex}{a}%
\raisebox{-0.7ex}{aa}%
\raisebox{-1.2ex}{r}%
\raisebox{-2.2ex}{g}%
\raisebox{-4.5ex}{h}}}
he shouted but not even the next
one in line noticed that something
terrible had happened to him.
</source>
|
[[Image:Latex example box test 2.png|300px]]
|}

An alternative to these approaches is the usage of the '''framed''' environment (you will need to include the "framed" package to use it). This provides an easy way to box a paragraph within a document:
<source lang="latex">
\begin{framed}
This is an easy way to box text within a document!
\end{framed}
</source>

== Rules and Struts ==

The <tt>\rule</tt> command in normal use produces a simple black box:
<source lang="latex">
\rule[lift]{width}{height}
</source>

Here is an example:
{|
|<source lang="latex">
\rule{3mm}{.1pt}%
\rule[-1mm]{5mm}{1cm}%
\rule{3mm}{.1pt}%
\rule[1mm]{1cm}{5mm}%
\rule{3mm}{.1pt}
</source>
|
[[Image:Latex example rule.png|300px]]
|}

This is useful for drawing vertical and horizontal lines.

A special case is a rule with no width but a certain height. In professional typesetting, this is called a ''strut''. It is used to guarantee that an element on a page has a certain minimal height. You could use it in a tabular environment to make sure a row has a certain minimum height.

{{-}}
{{TODO|Describe <tt>microtype</tt> package -- hanging punctuation, font expansion, additional kerning. Link from Formatting chapter}}
The microtype package can be used to eliminate all hyphenation.
<noinclude>
{{LaTeX/Bottom|Packages|Customizing LaTeX}}
</noinclude>
