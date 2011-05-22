>  <noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">  <noinclude>{{LaTeX/Top}}</noinclude>
In academic writing, tables are a common feature, often for summarising results from research. It is therefore a skill that needs mastering in order to produce quality papers.

However, if there is one area about LaTeX that is the least intuitive, then this is it. Basic tables are not too taxing, but you will quickly notice that anything more advanced can take a fair bit of construction. So, we start slowly and build up from there.

'''Workaround''': You might save lots of time by converting tables from OpenOffice spreadsheets with the help of open source plugins, see e.g. [http://calc2latex.sourceforge.net/ http://calc2latex.sourceforge.net/]. 

== The <tt>tabular</tt> environment ==


The <code>tabular</code> environment can be used to typeset tables with optional horizontal and vertical lines. LaTeX determines the width of the columns automatically.

The first line of the environment has the form:
<source lang="latex" enclose="none">
\begin{tabular}[pos]{table spec}
</source>

the ''table spec'' argument tells LaTeX the alignment to be used in each column and the vertical lines to insert.

The number of columns does not need to be specified as it is inferred by looking at the number of arguments provided. It is also possible to add vertical lines between the columns here. The following symbols are available to describe the table columns (some of them require that the package ''array'' has been loaded):

{| class="wikitable"
| <tt>l</tt>
| left-justified column
|-
| <tt>c</tt>
| centered column
|-
| <tt>r</tt>
| right-justified column
|-
| <tt>p{''width''}</tt>
| paragraph column with text vertically aligned at the top
|-
| <tt>m{''width''}</tt>
| paragraph column with text vertically aligned in the middle (requires array package)
|-
| <tt>b{''width''}</tt>
| paragraph column with text vertically aligned at the bottom (requires array package)
|-
| <tt><nowiki>|</nowiki></tt>
| vertical line
|-
| <tt><nowiki>||</nowiki></tt>
| double vertical line
|}

By default, if the text in a column is too wide for the page, LaTeX won’t automatically wrap it. Using <code>p{''width''}</code> you can define a special type of column which will wrap-around the text as in a normal paragraph. You can pass the width using any unit supported by LaTeX, such as pt and cm, or ''command lengths'', such as <code>\textwidth</code>.You can find a complete list in appendix [[LaTeX/Useful Measurement Macros|Useful Measurement Macros]].



The optional parameter ''pos'' can be used to specify the vertical position of the table relative to
the baseline of the surrounding text. In most cases, you will not need this option. It becomes relevant only if your table is not in a paragraph of its own. You can use the following letters:

{| class="wikitable"
|-
| <tt>b</tt>
|bottom
|-
| <tt>c</tt>
|center (default)
|-
| <tt>t</tt>
|top
|-
|}

In the first line you have pointed out how many columns you want, their alignment and the vertical lines to separate them. Once in the environment, you have to introduce the text you want, separating between cells and introducing new lines. The commands you have to use are the following:

{| class="wikitable"
| <tt>&</tt>
| column separator
|-
| <tt>\\</tt>
| start new row (additional space may be specified after <code>\\</code> using square brackets, such as <code>\\[6pt]</code>)
|-
| <tt>\hline</tt>
| horizontal line
|-
| <tt>\newline</tt>
| start a new line within a cell
|-
| <tt>\cline{''i''-''j''}</tt>
| partial horizontal line beginning in column ''i'' and ending in column ''j''
|}

Note, any white space inserted between these commands is purely down to ones' preferences. I personally add spaces between to make it easier to read.

=== Basic examples ===

This example shows how to create a simple table in LaTeX. It is a three-by-three table, but without any lines.

{|
| style="vertical-align: middle;" |
<source lang="latex" enclose="none">
\begin{tabular}{ l c r }
  1 & 2 & 3 \\
  4 & 5 & 6 \\
  7 & 8 & 9 \\
\end{tabular}
</source>
| style="vertical-align: middle;" |
[[Image:basic_table1.png]]
|}

Expanding upon that by including some vertical lines:

{|
| style="vertical-align: middle;" |
<source lang="latex" enclose="none">
\begin{tabular}{ l | c || r | }
  1 & 2 & 3 \\
  4 & 5 & 6 \\
  7 & 8 & 9 \\
\end{tabular}</source>
| style="vertical-align: middle;" |
[[Image:basic_table2.png]]
|}

To add horizontal lines to the very top and bottom edges of the table:

{|
| style="vertical-align: middle;" |
<source lang="latex" enclose="none">
\begin{tabular}{ l | c || r | }
  \hline			
  1 & 2 & 3 \\
  4 & 5 & 6 \\
  7 & 8 & 9 \\
  \hline  
\end{tabular}</source>
| style="vertical-align: middle;" |
[[Image:basic_table3.png]]
|}

And finally, to add lines between all rows, as well as centering (notice the use of the center environment - of course, the result of this is not obvious from the preview on this web page):

{|
| style="vertical-align: middle;" |
<source lang="latex" enclose="none">
\begin{center}
  \begin{tabular}{ l | c || r | }
    \hline
    1 & 2 & 3 \\ \hline
    4 & 5 & 6 \\ \hline
    7 & 8 & 9 \\
    \hline
  \end{tabular}
\end{center}</source>
| style="vertical-align: middle;" |
[[Image:basic_table4.png]]
|}

{|
|
<source lang="latex" enclose="none">
\begin{tabular}{|r|l|}
  \hline
  7C0 & hexadecimal \\
  3700 & octal \\ \cline{2-2}
  11111000000 & binary \\
  \hline \hline
  1984 & decimal \\
  \hline
\end{tabular}
</source>
|[[Image:Latex example tabular cline.png|250px]]
|}

=== Column specification using <code>>{\cmd}</code> and <code><{\cmd}</code> ===
Using the array package, the column specification can be altered. This is done in the 
argument of the tabular environment using <code>>{\command}</code> for commands executed right 
''before'' each column element and <code><{\command}</code> for commands to be executed right
''after'' each column element. 
As an example: to get a column in math mode enter: <code>\begin{tabular}{>{$}c<{$}}</code>. 
Another example is changing the font: <code>\begin{tabular}{>{\small}c}</code> to print the column in a small font.

The argument of the <code>></code> and <code><</code> specifications must be correctly balanced when it comes to <code>{</code> and <code>}</code> characters. This means that <code>>{\bfseries}</code> is valid, while <code>>{\textbf}</code> will not work and <code>>{\textbf{}</code> is not valid. If there is the need to use the text of the table as an argument (for instance, using the <code>\textbf</code> to produce bold text), one should use the <code>\bgroup</code> and <code>\egroup</code> commands: <code>>{\textbf\bgroup}c<{\egroup}</code> produces the intended effect. This works only for some basic LaTeX commands. For other commands, such as <code>\underline</code> to underline text, it is necessary to temporarily store the column text in a box using <code>lrbox</code>. First, you must define such a box with <code>\newsavebox{\boxname}</code> and then you can define:

<source lang="latex" enclose="none">
>{\begin{lrbox}{\boxname}}%
l%
<{\end{lrbox}%
  \underline{\unhbox\boxname}}%
 }
</source>

This stores the text in a box and afterwards, takes the text out of the box with <code>\unhbox</code> (this destroys the box, if the box is needed again one should use <code>\unhcopy</code> instead) and passing it to <code>\underline</code>. (For LaTeX2e, you may want to use <code>\usebox{\boxname}</code> instead of <code>\unhbox\boxname</code>.)

This same trick done with <code>\raisebox</code> instead of <code>\underline</code> can force all lines in a table to have equal height, instead of the natural varying height that can occur when e.g. math terms or superscripts occur in the text.

Here is an example showing the use of both <code>p{...}</code> and <code>>{\centering}</code> :

<source lang="latex" enclose="none">
\begin{tabular}{>{\centering}p{3.5cm}>{\centering}p{3.5cm}}
Geometry  & Algebra
\tabularnewline
\hline
 Points & Addition 
\tabularnewline
 Spheres & Multiplication 
\end{tabular}
</source>

Note the use of <code>\tabularnewline</code> instead of <code>\\</code> to avoid a <code>Misplaced \noalign</code> error.

=== Text wrapping in tables ===

LaTeX's algorithms for formatting tables have a few shortcomings. One is that it will not automatically wrap text in cells, even if it overruns the width of the page. For columns that you know will contain a certain amount of text, then it is recommended that you use the ''p'' attribute and specify the desired width of the column (although it may take some trial-and-error to get the result you want). Use the ''m'' attribute to have the lines aligned toward the middle of the box and the ''b'' attribute to align along the bottom of the box.

Here is a practical example. The following code creates two tables with the same code; the only difference is that the last column of the second one has a defined width of 5 centimeters, while in the first one we didn't specify any width. Compiling this code:

<source lang="latex" enclose="none">
\documentclass{article} 

\usepackage[english]{babel}

\begin{document}

Without specifying width for last column:

\begin{center}
    \begin{tabular}{ | l | l | l | l |}
    \hline
    Day & Min Temp & Max Temp & Summary \\ \hline
    Monday & 11C & 22C & A clear day with lots of sunshine.
    However, the strong breeze will bring down the temperatures. \\ \hline
    Tuesday & 9C & 19C & Cloudy with rain, across many northern regions. Clear spells 
    across most of Scotland and Northern Ireland, 
    but rain reaching the far northwest. \\ \hline
    Wednesday & 10C & 21C & Rain will still linger for the morning. 
    Conditions will improve by early afternoon and continue 
    throughout the evening. \\
    \hline
    \end{tabular}
\end{center}

With width specified:

\begin{center}
    \begin{tabular}{ | l | l | l | p{5cm} |}
    \hline
    Day & Min Temp & Max Temp & Summary \\ \hline
    Monday & 11C & 22C & A clear day with lots of sunshine.  
    However, the strong breeze will bring down the temperatures. \\ \hline
    Tuesday & 9C & 19C & Cloudy with rain, across many northern regions. Clear spells 
    across most of Scotland and Northern Ireland, 
    but rain reaching the far northwest. \\ \hline
    Wednesday & 10C & 21C & Rain will still linger for the morning. 
    Conditions will improve by early afternoon and continue 
    throughout the evening. \\
    \hline
    \end{tabular}
\end{center}

\end{document}
</source>

You get the following output:

[[Image:Latex example wrapped table.png|800px|center]]

Note that the first table is cropped: The output is wider than the page width.

=== Text justification in tables ===

On rare occasions, it might be necessary to stretch every row in a table to the natural width of its longest line, for instance when one has the same text in two languages and wishes to present these next to each other with lines synching up. A tabular environment helps control where lines should break, but cannot justify the text, which leads to ragged right edges. The <code>eqparbox</code> package provides the command <code>\eqmakebox</code> which is like <code>\makebox</code> but instead of a ''width'' argument, it takes a tag. During compilation it bookkeeps which <code>\eqmakebox</code> with a certain tag contains the widest text and can stretch all <code>\eqmakebox</code>es with the same tag to that width. Combined with the <code>array</code> package, one can define a column specifier that justifies the text in all lines: (See the documentation of the <code>eqparbox</code> package for more details.)

<source lang="latex" enclose="none">
\newsavebox{\tstretchbox}
\newcolumntype{S}[1]{%
 >{\begin{lrbox}{\tstretchbox}}%
 l%
 <{\end{lrbox}%
   \eqmakebox[#1][s]{\unhcopy\tstretchbox}}%
  }
</source>

=== Other environments inside tables ===

If you use some LaTeX environments inside table cells, like <tt>verbatim</tt> or <tt>[[LaTeX/Formatting#Enumerate|enumerate]]</tt>

<source lang="latex" enclose="none">
\begin{tabular}{| c | c |}
	\hline
	\begin{verbatim}
	code
	\end{verbatim}
	& description
 	\\ \hline
\end{tabular}
</source>

you might encounter errors similar to
 ! LaTeX Error: Something's wrong--perhaps a missing \item.

To solve this problem, change [[#The tabular environment|column specifier]] to "paragraph" (<tt>p</tt>, <tt>m</tt> or <tt>b</tt>).

<source lang="latex" enclose="none">
\begin{tabular}{| m{5cm} | c |}
</source>

=== Defining multiple columns ===

It is possible to define many identical columns at once using the <code>*{''num''}{''str''}</code> syntax. 

This is particularly useful when your table has many columns. 

Here is a table with six centered columns flanked by a single column on each side:

{|
| style="vertical-align: middle;" |
<source lang="latex" enclose="none">
\begin{tabular}{l*{6}{c}r}
Team              & P & W & D & L & F  & A & Pts \\
\hline
Manchester United & 6 & 4 & 0 & 2 & 10 & 5 & 12  \\
Celtic            & 6 & 3 & 0 & 3 &  8 & 9 &  9  \\
Benfica           & 6 & 2 & 1 & 3 &  7 & 8 &  7  \\
FC Copenhagen     & 6 & 2 & 1 & 2 &  5 & 8 &  7  \\
\end{tabular}</source>
|
|}

[[Image:Latex example defining multiple columns.png|450px]]

=== @-expressions ===

The column separator can be specified with the <code>@{...}</code> construct. 

It typically takes some text as its argument, and when appended to a column, it will automatically insert that text into each cell in that column before the actual data for that cell. This command kills the inter-column space and replaces it with whatever is between the curly braces. To add space, use <code>@{\hspace{''width''}}</code>.

Admittedly, this is not that clear, and so will require a few examples to clarify. Sometimes, it is desirable in scientific tables to have the numbers aligned on the decimal point. This can be achieved by doing the following:

{|
| style="vertical-align: middle;" |
<source lang="latex" enclose="none">
\begin{tabular}{r@{.}l}
  3   & 14159 \\
  16  & 2     \\
  123 & 456   \\
\end{tabular}</source>
| style="vertical-align: middle;" |
[[Image:align.png]]
|}


Note that the approach outlined above won't work well if the column header is longer than any of the numbers. To center the column on the decimal separator, use the <tt>dcolumn</tt> package, which provides a new column specifier for floating point data.

The space suppressing qualities of the @-expression actually make it quite useful for manipulating the horizontal spacing between columns. Given a basic table, and varying the column descriptions:

<source lang="latex" enclose="none">
\begin{tabular}{|l|l|}
  \hline
  stuff & stuff \\ \hline
  stuff & stuff \\
  \hline
\end{tabular}
</source>

{|
| style="vertical-align: middle;" | <tt><nowiki>{|l|l|}</nowiki></tt>
| style="vertical-align: middle;" |
[[Image:specifier1.png]]
|-
| style="vertical-align: middle;" | <tt><nowiki>{|@{}l|l@{}|}</nowiki></tt>
| style="vertical-align: middle;" |
[[Image:specifier2.png]]
|-
| style="vertical-align: middle;" | <tt><nowiki>{|@{}l@{}|l@{}|}</nowiki></tt>
| style="vertical-align: middle;" |
[[Image:specifier3.png]]
|-
| style="vertical-align: middle;" | <tt><nowiki>{|@{}l@{}|@{}l@{}|}</nowiki></tt>
| style="vertical-align: middle;" |
[[Image:specifier4.png]]
|}

=== Spanning ===

To complete this tutorial, we take a quick look at how to generate slightly more complex tables. Unsurprisingly, the commands necessary have to be embedded within the table data itself.

==== Rows spanning multiple columns ====

The command for this looks like this: <code>\multicolumn{''num_cols''}{''alignment''}{''contents''}</code>. ''num_cols'' is the number of subsequent columns to merge; ''alignment'' is, either l, c, r or to have text wrapping specifiy a width <code>p{5.0cm}</code> . And ''contents'' is simply the actual data you want to be contained within that cell. A simple example:

{|
| style="vertical-align: middle;" |
<source lang="latex" enclose="none">
\begin{tabular}{|l|l|}
  \hline
  \multicolumn{2}{|c|}{Team sheet} \\
  \hline
  GK & Paul Robinson \\
  LB & Lucus Radebe \\
  DC & Michael Duberry \\
  DC & Dominic Matteo \\
  RB & Didier Domi \\
  MC & David Batty \\
  MC & Eirik Bakke \\
  MC & Jody Morris \\
  FW & Jamie McMaster \\
  ST & Alan Smith \\
  ST & Mark Viduka \\
  \hline
\end{tabular}</source>
| style="vertical-align: middle;" |
[[Image:multicolumn.png]]
|}

==== Columns spanning multiple rows ====

The first thing you need to do is add <code>\usepackage{multirow}</code> [http://www.ctan.org/tex-archive/macros/latex/contrib/multirow/] to the preamble. This then provides the command needed for spanning rows: <code>\multirow{''num_rows''}{''width''}{''contents''}</code>. The arguments are pretty simple to deduce (<code>*</code> for the ''width'' means the content's natural width).

{|
| style="vertical-align: middle;" |
<source lang="latex" enclose="none">
...
\usepackage{multirow}
...

\begin{tabular}{|l|l|l|}
\hline
\multicolumn{3}{|c|}{Team sheet} \\
\hline
Goalkeeper & GK & Paul Robinson \\ \hline
\multirow{4}{*}{Defenders} & LB & Lucus Radebe \\
 & DC & Michael Duberry \\
 & DC & Dominic Matteo \\
 & RB & Didier Domi \\ \hline
\multirow{3}{*}{Midfielders} & MC & David Batty \\
 & MC & Eirik Bakke \\
 & MC & Jody Morris \\ \hline
Forward & FW & Jamie McMaster \\ \hline
\multirow{2}{*}{Strikers} & ST & Alan Smith \\
 & ST & Mark Viduka \\
\hline
\end{tabular}</source>
| style="vertical-align: middle;" |
[[Image:multirow.png]]
|}

The main thing to note when using <code>\multirow</code> is that a blank entry must be inserted for each appropriate cell in each subsequent row to be spanned.

If there is no data for a cell, just don't type anything, but you still need the "&" separating it from the next column's data. The astute reader will already have deduced that for a table of <math>n</math> columns, there must always be <math>n-1</math> ampersands in each row. The exception to this is when <tt>\multicolumn</tt> and <tt>\multirow</tt> are used to create cells which span multiple columns or rows.

==== Spanning in both directions simultaneously ====

Here is a nontrivial example how to use spanning in both directions simultaneously and have the borders of the cells drawn correctly:

{|
| style="vertical-align: middle;" |
<source lang="latex" enclose="none">
\usepackage{multirow}

\begin{tabular}{cc|c|c|c|c|l}
\cline{3-6}
& & \multicolumn{4}{|c|}{Primes} \\ \cline{3-6}
& & 2 & 3 & 5 & 7 \\ \cline{1-6}
\multicolumn{1}{|c|}{\multirow{2}{*}{Powers}} &
\multicolumn{1}{|c|}{504} & 3 & 2 & 0 & 1 &     \\ \cline{2-6}
\multicolumn{1}{|c|}{}                        &
\multicolumn{1}{|c|}{540} & 2 & 3 & 1 & 0 &     \\ \cline{1-6}
\multicolumn{1}{|c|}{\multirow{2}{*}{Powers}} &
\multicolumn{1}{|c|}{gcd} & 2 & 2 & 0 & 0 & min \\ \cline{2-6}
\multicolumn{1}{|c|}{}                        &
\multicolumn{1}{|c|}{lcm} & 3 & 3 & 1 & 1 & max \\ \cline{1-6}
\end{tabular}
</source>
| style="vertical-align: middle;" |
[[Image:multirowandcolumnexample.png|300px]]
|}

The command <code>\multicolumn{1}{|c|}{...}</code> is just used to draw vertical borders both on the left and on the right of the cell. Even when combined with <code>\multirow{2}{*}{...}</code>, it still draws vertical borders that only span the first row. To compensate for that, we add <code>\multicolumn{1}{|c|}{...}</code> in the following rows spanned by the multirow. Note that we cannot just use <code>\hline</code> to draw horizontal lines, since we do not want the line to be drawn over the text that spans several rows. Instead we use the command <code>\cline{2-6}</code> and opt out the first column that contains the text "Powers".

Here is another example exploiting the same ideas to make
the familiar and popular "2x2" or double dichotomy:

{|
| style="vertical-align: middle;" |
<source lang="latex" enclose="none">
\begin{tabular}{r|c|c|}
\multicolumn{1}{r}{}
 &  \multicolumn{1}{c}{noninteractive}
 & \multicolumn{1}{c}{interactive} \\
\cline{2-3}
massively multiple & Library & University \\
\cline{2-3}
one-to-one & Book & Tutor \\
\cline{2-3}
\end{tabular}
</source>
| style="vertical-align: middle;" |
[[Image:Latex-tables-double-dichotomy-example.png|400px]]
|}

===Resize tables ===

The command <code>\resizebox{width}{height}{object}</code> can be used with <code>tabular</code> to specify the height and width of a table. The following example shows how to resize a table to 8cm width while maintaining the original width/height ratio.

<source lang="latex" enclose="none">
\resizebox{8cm}{!} {
  \begin{tabular}...
  \end{tabular}
}
</source>

Alternatively you can use <code>\scalebox{ratio}{object}</code> in the same way but with ratios rather than fixed sizes:

<source lang="latex" enclose="none">
\scalebox{0.7}{
  \begin{tabular}...
  \end{tabular}
}
</source>

Both <code>\resizebox</code> and <code>\scalebox</code> require the graphicx package.

To tweak the space between columns (LaTeX will by default choose very tight columns), one can alter the column separation: <code>\setlength{\tabcolsep}{5pt}</code>.
The default value is 6pt.

===Sideways tables ===

Tables can also be put on their side within a document using the <code>rotating</code> package and the <code>sidewaystable</code> environments in place of the table environment. (NOTE: most DVI viewers do not support displaying rotated text. Convert your document to a PDF to see the result. Most, if not all, PDF viewers do support rotated text.)

<source lang="latex" enclose="none">
\usepackage{rotating}

\begin{sidewaystable}
  \begin{tabular}...
  \end{tabular}
\end{sidewaystable}
</source>

When it is desirable to place the rotated table at the exact location where it appears in the source (.tex) file, <code>rotfloat</code> package may be used. Then one can use <code>\begin{sidewaystable}[H]</code> just like for normal tables. The 'H' option can not be used without this package.

=== Alternate Row Colors in Tables ===

The <code>xcolor</code> package provides the necessary commands to produce tables with alternate row colors, when loaded with the <code>table</code> option.
The command <code>\rowcolors{<''starting row''>}{<''odd color''>}{<''even color''>}</code> has to be specified right before the <code>tabular</code> environment starts.
{|
| style="vertical-align: middle;" |
<source lang="latex" enclose="none">
\documentclass{article}

\usepackage[table]{xcolor}

\begin{document}

\begin{center}
\rowcolors{1}{green}{pink}

\begin{tabular}{lll}
odd 	& odd 	& odd \\
even 	& even 	& even\\
odd 	& odd 	& odd \\
even 	& even 	& even\\
\end{tabular}
\end{center}

\end{document}
</source>
| style="vertical-align: middle;" | [[File:LaTeXAlternateRowTable.png|right]]
|}
The command <code>\hiderowcolors</code> is available to deactivate highlighting of a specified row.
Highlighting can be reactivated within the table via the <code>\showrowcolors</code> command.

=== Colors of individual Cells ===

As above this uses the <code>xcolor</code> package.

<source lang="latex" enclose="none">
% Include this somewhere in your document
\usepackage[table]{xcolor}

% Enter this in the cell you wish to color a light grey.
% NB: the word 'gray' here denotes the grayscale color scheme, not the color grey. `0.9' denotes how dark the grey is.
\cellcolor[gray]{0.9}
% The following will color the cell red.
\cellcolor{red}
</source>

=== Partial Vertical Lines ===

Adding a partial vertical line to an individual cell:

{|
| style="vertical-align: middle;" |
<source lang="latex" enclose="none">
\begin{tabular}{ l c r }
  1 & 2 & 3 \\ \hline
  4 & 5 & \multicolumn{1}{r|}{6}  \\ \hline
  7 & 8 & 9 \\ \hline
\end{tabular}
</source>
| style="vertical-align: middle;" |
[[Image:Partial-vertical-line-add.png]]
|}

Removing part of a vertical line in a particular cell:

{|
| style="vertical-align: middle;" |
<source lang="latex" enclose="none">
\begin{tabular}{ | l | c | r | }
  1 & 2 & 3 \\ \hline
  4 & 5 & \multicolumn{1}{r}{6} \\ \hline
  7 & 8 & 9 \\ \hline
\end{tabular}
</source>
| style="vertical-align: middle;" |
[[Image:Partial-vertical-line-remove.png]]
|}

== The <tt>table</tt> environment - captioning etc ==

The <code>tabular</code> environment doesn't cover all that you need to do with tables. For example, you might want a caption for your table. For this and other reasons, you should typically place your <code>tabular</code> environment inside a <code>table</code> environment:

<source lang="latex" enclose="none">
\begin{table}
  \caption{Performance at peak F-measure}
  \begin{tabular}{| r | r || c | c | c |}

      ...

  \end{tabular}
\end{table}
</source>

Why do the two different environments exist? Think of it this way: The <code>tabular</code> environment is concerned with arranging elements in a tabular grid, while the <code>table</code> environment represents the table more conceptually. This explains why it isn't <code>tabular</code> but <code>table</code> that provides for captioning (because the caption isn't displayed in the grid-like layout).

A <code>table</code> environment has a lot of similarities with a <code>figure</code> environment, in the way the "floating" is handled etc.
For instance you can specify its placement in the page with the option <code>[placement]</code>, the valid values are any combination of (order is not important):

{| class="wikitable"
|-
| <tt>h</tt>
|where the table is declared ('''h'''ere)
|-
| <tt>t</tt>
|at the '''t'''op of the page
|-
| <tt>b</tt>
|at the '''b'''ottom of the page
|-
| <tt>p</tt>
|on a dedicated '''p'''age of floats
|-
| <tt>!</tt>
|override the default float restrictions. E.g., the maximum size allowed of a <tt>b</tt> float is normally quite small; if you want a large one, you need this <tt>!</tt> parameter as well.
|-
|}

The default is <tt>[tbp]</tt>. If you want to place a table in the place where it's declared, do not just write <tt>[h]</tt>; if the table cannot fit (because the text is near the bottom of the page, say) it will float to a dedicated page of floats (as if it were a <tt>p</tt> float) which can be some distance away in the document. A good rule of thumb is to always use <tt>htbp</tt> until the document is finished, at which stage the final float parameters can be fine-tuned.

The <code>table</code> environment is also useful when you want to have a
list of tables at the beginning or end of your document with the command
<code>\listoftables</code>;
it enables making cross-references to the table with:

<source lang="latex" enclose="none">
You may refer to table~\ref{my_table} for an example.

...

\begin{table}
  \begin{tabular}
     ...
  \end{tabular}
  \caption{An example of table}
  \label{my_table}
\end{table}
</source>

== The <tt>tabular*</tt> environment - controlling table width ==

This is basically a slight extension on the original tabular version, although it requires an extra argument (before the column descriptions) to specify the preferred width of the table.

{|
|-
|
<source lang="latex" enclose="none">
\begin{tabular*}{0.75\textwidth}{ | c | c | c | r | }
  \hline
  label 1 & label 2 & label 3 & label 4 \\
  \hline 
  item 1  & item 2  & item 3  & item 4  \\
  \hline
\end{tabular*}
</source>
|[[Image:LaTeX TabWidth1.png|right]]
|}

However, that may not look quite as intended. The columns are still at their natural width (just wide enough to fit their contents) while the rows are as wide as the table width specified. If you do not like this default, you must also explicitly insert extra column space. LaTeX has ''rubber lengths'', which, unlike others, are not fixed. LaTeX can dynamically decide how long the lengths should be. So, an example of this is the following.

<source lang="latex" enclose="none">
\begin{tabular*}{0.75\textwidth}{@{\extracolsep{\fill}} | c | c | c | r | }
  \hline
  label 1 & label 2 & label 3 & label 4 \\
  \hline 
  item 1  & item 2  & item 3  & item 4  \\
  \hline
\end{tabular*}
</source>

[[Image:LaTeX TabWidth2.png|right]]

You will notice the <code>@{...}</code> construct added at the beginning of the column description. Within it is the <code>\extracolsep</code> command, which requires a width. A fixed width could have been used. However, by using a rubber length, such as <code>\fill</code>, the columns are automatically spaced evenly.

== The <tt>tabularx</tt> package - simple column stretching ==

This package provides a table environment called <tt>tabularx</tt> which is similar to the <tt>tabular*</tt> environment, except that it has a new column specifier <tt>X</tt> (in uppercase). The column(s) specified with this specifier will be stretched to make the table as wide as specified, greatly simplifying the creation of tables.

{|
|-
|
<source lang="latex" enclose="none">
\usepackage{tabularx}

...

\begin{tabularx}{\textwidth}{ |X|X|X|X| }
  \hline
  label 1 & label 2 & label 3 & label 4 \\
  \hline 
  item 1  & item 2  & item 3  & item 4  \\
  \hline
\end{tabularx}
</source>
|[[Image:LaTeX TabXWidth1.png|450px|right]]
|}



The content provided for the boxes is treated as for a <tt>p</tt> column, except that the width is calculated automatically. If you use the package <tt>array</tt>, you may also apply any <tt>&gt;{\cmd}</tt> or <tt>&lt;{\cmd}</tt> command to achieve specific behavior (like <tt>\centering</tt>, or <tt>\raggedright\arraybackslash</tt>) as described previously.

Another option is the use of <tt>\newcolumntype</tt> in order to get selected columns formatted in a different way. It defines a new column specifier, e.g. <tt>R</tt> (in uppercase). In this example, the second and fourth column is adjusted in a different way (<tt>\raggedleft</tt>):

{|
|-
|
<source lang="latex" enclose="none">
\usepackage{tabularx}

...
\newcolumntype{R}{>{\raggedleft\arraybackslash}X}%
\begin{tabularx}{\textwidth}{ |l|R|l|R| }
  \hline
  label 1 & label 2 & label 3 & label 4 \\
  \hline 
  item 1  & item 2  & item 3  & item 4  \\
  \hline
\end{tabularx}
</source>
|[[Image:LaTeX TabXWidth2.png|450px|right]]
|}
Tabularx with rows spanning multiple columns using <tt>\multicolumn</tt>. The two central columns are posing as one by using the <tt>X@{}</tt> option. Note that the <tt>\multicolumn</tt> width (which in this example is 2) should equal the (in this example 1+1) width of the spanned columns:

{|
|-
|
<source lang="latex" enclose="none">
\usepackage{tabularx}

...
\begin{tabularx}{1\textwidth}{|>{\setlength\hsize{1\hsize}\centering}X|>{\setlength\hsize{1\hsize}\raggedleft}X@{} >{\setlength\hsize{1\hsize}\raggedright}X|>{\setlength\hsize{1\hsize}\centering}X|} 
  \hline
Label 1 & \multicolumn{2}{>{\centering\setlength\hsize{2\hsize}}X|}{Label 2} & Label 3\tabularnewline
\hline 
  123  & 123  & 456  & 123  \tabularnewline
  \hline
  123  & 123  & 456  & 123  \tabularnewline
  \hline
\end{tabularx}
</source>
|[[Image:LaTeX tabularx_multi.png|450px|right]]
|}

== Vertically centered images ==

Inserting images into a table row will align it at the top of the cell.
By using the <tt>array</tt> package this problem can be solved.
Defining a new columntype will keep the image vertically centered.

{|
|-
|<source lang="latex" enclose="none">
\newcolumntype{V}{>{\centering\arraybackslash} m{.4\linewidth} }
</source>
|}

Or use a parbox to center the image.

{|
|-
|<source lang="latex" enclose="none">
\parbox[c]{1em}{\includegraphics{image.png}}
</source>
|}

== Professional tables ==

Many tables in professionally typeset books and journals feature simple tables, which have appropriate spacing above and below lines, and almost ''never'' use vertical rules. Many examples of LaTeX tables (including this Wikibook) showcase the use of vertical rules (using "<code>|</code>"), and double-rules (using <code>\hline\hline</code>" or "<code>||</code>"), which are regarded as unnecessary and distracting in a professionally published form.  The [http://www.ctan.org/tex-archive/macros/latex/contrib/booktabs/ booktabs] package is useful for easily providing this professionalism in LaTeX tables, and the [http://mirrors.ctan.org/macros/latex/contrib/booktabs/booktabs.pdf documentation] also provides guidelines on what constitutes a "good" table.

In brief, the package uses <code>\toprule</code> for the uppermost rule (or line), <code>\midrule</code> for the rules appearing in the middle of the table (such as under the header), and <code>\bottomrule</code> for the lowermost rule. This ensures that the rule weight and spacing are acceptable. In addition, <code>\cmidrule</code> can be used for mid-rules that span specified columns. The following example contrasts the use of booktabs and normal LaTeX implementations (the later example requires <code>\usepackage{booktabs}</code> in the preamble).
{|
! Normal LaTeX
! Using <tt>array</tt>
! Using <tt>booktabs</tt>
|- valign=bottom
|
<source lang="latex" enclose="none">
\begin{tabular}{llr}
\hline
\multicolumn{2}{c}{Item} \\
\cline{1-2}
Animal & Description & Price (\$) \\
\hline
Gnat  & per gram & 13.65 \\
      & each     &  0.01 \\
Gnu   & stuffed  & 92.50 \\
Emu   & stuffed  & 33.33 \\
Armadillo & frozen & 8.99 \\
\hline
\end{tabular}
</source>
|
<source lang="latex" enclose="none">
\begin{tabular}{llr}
\firsthline
\multicolumn{2}{c}{Item} \\
\cline{1-2}
Animal & Description & Price (\$) \\
\hline
Gnat  & per gram & 13.65 \\
      & each     &  0.01 \\
Gnu   & stuffed  & 92.50 \\
Emu   & stuffed  & 33.33 \\
Armadillo & frozen & 8.99 \\
\lasthline
\end{tabular}
</source>
|
<source lang="latex" enclose="none">
\usepackage{booktabs}

...

\begin{tabular}{llr}
\toprule
\multicolumn{2}{c}{Item} \\
\cmidrule(r){1-2}
Animal & Description & Price (\$) \\
\midrule
Gnat  & per gram & 13.65 \\
      & each     &  0.01 \\
Gnu   & stuffed  & 92.50 \\
Emu   & stuffed  & 33.33 \\
Armadillo & frozen & 8.99 \\
\bottomrule
\end{tabular}
</source>
|-valign=top
| [[Image:LaTeX animal table.png]]
|
| [[Image:LaTeX animal table with booktabs.png]]
|}

Usually the need arises for footnotes under a table (and not at the bottom of the page), with a caption properly spaced above the table. These are addressed by the [http://www.ctan.org/tex-archive/macros/latex/contrib/ctable/ ctable] package. It provides the option of a short caption given to be inserted in the list of tables, instead of the actual caption (which may be quite long and inappropriate for the list of tables). The ctable package uses the booktabs package.

== Adding rule spacing above or below <tt>\hline</tt> and <tt>\cline</tt> commands ==

An alternative way to adjust the rule spacing is to add <code>\noalign{\smallskip}</code> before or after the <code>\hline</code> and <code>\cline{i-j}</code> commands:

'''Normal LaTeX'''

<source lang="latex" enclose="none">
\begin{tabular}{llr}
\hline\noalign{\smallskip}
\multicolumn{2}{c}{Item} \\
\cline{1-2}\noalign{\smallskip}
Animal & Description & Price (\$) \\
\noalign{\smallskip}\hline\noalign{\smallskip}
Gnat  & per gram & 13.65 \\
      & each     &  0.01 \\
Gnu   & stuffed  & 92.50 \\
Emu   & stuffed  & 33.33 \\
Armadillo & frozen & 8.99 \\
\noalign{\smallskip}\hline
\end{tabular}
</source>

You may also specify the skip after a line explicitly using glue after the line terminator

<source lang="latex" enclose="none">
\begin{tabular}{|l|l|}
\hline
Mineral & Color \\[1cm]
Ruby & red \\
Sapphire & blue \\
\hline
\end{tabular}
</source>

== Tables with different font size ==

A table can be globally switched to a different font size by simply adding the desired size command (here: <code>\footnotesize</code>) after the <code>\begin{table}...</code> statement:

<source lang="latex" enclose="none">
\begin{table}[h]\footnotesize
  \caption{Performance at peak F-measure}
  \begin{tabular}{| r | r || c | c | c |}

      ...

  \end{tabular}
\end{table}
</source>

The table caption font size is not affected.

To control the caption font size, see [[LaTeX/Floats,_Figures_and_Captions#Caption_Styles|Caption Styles]].

== Table with legend ==
To add a legend to a table the [http://www.ctan.org/tex-archive/macros/latex/contrib/caption/ caption] package can be used. With the caption package a <code>\caption*{...}</code> statement can be added besides the 
normal <code>\caption{...}</code>.

Example

<source lang="latex" enclose="none">
\begin{table}
  \begin{tabular}{| r | r || c | c | c |}

      ...

  \end{tabular}
  \caption{A normal caption}
  \caption*{
    A legend, even a table can be used
    \begin{tabular}{l l}
      item 1 & explanation 1 \\
    \end{tabular}
  }
\end{table}
</source>

The normal caption is needed for labels en references.

== Need more complicated features? ==

Have a look at one of the following packages:

* [http://tug.ctan.org/pkg/hhline <tt>hhline</tt>]:   do whatever you want with horizontal lines
* [http://tug.ctan.org/pkg/array <tt>array</tt>]:    gives you more freedom on how to define columns
* [http://tug.ctan.org/pkg/colortbl <tt>colortbl</tt>]: make your table more colorful
* [http://tug.ctan.org/pkg/supertabular <tt>supertabular</tt>]: for tables that need to stretch over several pages
* [http://tug.ctan.org/pkg/longtable <tt>longtable</tt>]:  similar to <tt>supertab</tt>.
**Note: footnotes do not work properly in a normal tabular environment. If you replace it with a longtable environment, footnotes work properly
* [http://tug.ctan.org/pkg/xtab <tt>xtab</tt>]: Yet another package for tables that need to span many pages
* [http://tug.ctan.org/pkg/tabulary <tt>tabulary</tt>]: modified <tt>tabular*</tt> allowing width of columns set for equal heights
* [http://tug.ctan.org/pkg/arydshln <tt>arydshln</tt>]: creates dashed horizontal and vertical lines
* [http://tug.ctan.org/pkg/ctable <tt>ctable</tt>]: allows for footnotes under table and properly spaced caption above (incorporates booktabs package)
* [http://tug.ctan.org/pkg/slashbox <tt>slashbox</tt>]: create 2D tables with the first cell containing a description for both axes
* [http://tug.ctan.org/pkg/dcolumn <tt>dcolumn</tt>]: decimal point alignment of numeric cells
* [http://tug.ctan.org/pkg/rccol <tt>rccol</tt>]: advanced decimal point alignment of numeric cells with rounding

== Summary ==

This concludes discussion of basic tables. Experimentation quickly leads to mastery. The table syntax in LaTeX can look rather messy, and seeing new examples can look confusing. But hopefully, enough has been covered here so that a user can create any table needed for your papers. Unsurprisingly, LaTeX has plenty more up its sleeve, so expect a follow up tutorial covering more advanced features in the near future.

<noinclude>
{{LaTeX/Bottom|Bibliography Management|Formatting}}
</noinclude>
