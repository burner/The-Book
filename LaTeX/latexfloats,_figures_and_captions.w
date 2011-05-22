><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

In the [[LaTeX/Importing Graphics|previous]] chapter, the importing of graphics was introduced. However, just having a picture stuck in-between paragraphs does not look professional. For starters, we want a way of adding captions, and to be able to cross-reference. What we need is a way of defining ''figures''. It would also be good if LaTeX could apply similar principles to when it arranges text to look its best, to arranging pictures too. This is where ''floats'' come into play.

== Floats ==

Floats are containers for things in a document that cannot be broken over a page. LaTeX by default recognizes "table" and "figure" floats, but you can define new ones of your own (see [[#Custom Floats|Custom Floats]] below).  Floats are there to deal with the problem of the object that won't fit on the present page, and to help when you really don't want the object here just now.

Floats are not part of the normal stream of text, but separate entities, positioned in a part of the page to themselves (top, middle, bottom, left, right, or wherever the designer specifies). They always have a caption describing them and they are always numbered so they can be referred to from elsewhere in the text. LaTeX automatically floats Tables and Figures, depending on how much space is left on the page at the point that they are processed. If there is not enough room on the current page, the float is moved to the top of the next page. This can be changed by moving the Table or Figure definition to an earlier or later point in the text, or by adjusting some of the parameters which control automatic floating.

Authors sometimes have many floats occurring in rapid succession, which raises the problem of how they are supposed to fit on the page and still leave room for text. In this case, LaTeX stacks them all up and prints them together if possible, or leaves them to the end of the chapter in protest. The skill is to space them out within your text so that they intrude neither on the thread of your argument or discussion, nor on the visual balance of the typeset pages. 


=== Figures ===

To create a figure that floats, use the <code>figure</code> environment.

<source lang="latex">
\begin{figure}[placement specifier]
... figure contents ...
\end{figure}
</source>

The previous section mentioned how floats are used to allow Latex to handle figures, while maintaining the best possible presentation. However, there may be times when you disagree, and a typical example is with its positioning of figures. The ''placement specifier'' parameter exists as a compromise, and its purpose is to give the author a greater degree of control over where certain floats are placed.

{{Anchor|placement}}
{| class="wikitable"
! Specifier
! Permission
|-
| <tt>h</tt>
| Place the float ''here'', i.e., ''approximately'' at the same point it occurs in the source text (however, not ''exactly'' at the spot)
|-
| <tt>t</tt>
| Position at the ''top'' of the page.
|-
| <tt>b</tt>
| Position at the ''bottom'' of the page.
|-
| <tt>p</tt>
| Put on a special ''page'' for floats only.
|-
| <tt>!</tt>
| Override internal parameters Latex uses for determining "good" float positions.
|-
| <tt>H</tt>
| Places the float at precisely the location in the LaTeX code. Requires the <code>float</code> package,<ref>http://www.ctan.org/tex-archive/macros/latex/contrib/float/</ref> e.g., <code>\usepackage{float}</code>. This is somewhat equivalent to <code>h!</code>.
|}

What you do with these ''placement permissions'' is to list which of the options that you wish to make available to LaTeX. These are simply possibilities, and Latex will decide when typesetting your document which of your supplied specifiers it thinks is best.

Use <code>\listoffigures</code> to add a list of the figures in the beginning of the document.
To change the name used in the caption from '''Figure''' to '''Example''',
use <code>\renewcommand{\figurename}{Example}</code> in the figure contents.

=== Figures with borders ===

It's possible to get a thin border around all figures. You have to write the following once at the beginning of
the document:
<source lang="latex">
\usepackage{float}
\floatstyle{boxed} 
\restylefloat{figure}
</source>
The border will not include the caption.

=== Tables ===

Although [[LaTeX/Tables|tables have already been covered]], it was only the internal syntax that was discussed. The <code>tabular</code> environment that was used to construct the tables is not a float by default. Therefore, for tables you wish to float, wrap the <code>tabular</code> environment within a <code>table</code> environment, like this:

<source lang="latex">
\begin{table}
  \begin{tabular}{...}
  ... table data ...
  \end{tabular}
\end{table}
</source>

You may feel that it is a bit long winded, but such distinctions are necessary, because you may not want all tables to be treated as a float.

Use <code>\listoftables</code> to add a list of the tables in the beginning of the document.

== Captions ==

It is always good practice to add a caption to any figure or table. Fortunately, this is very simple in LaTeX. All you need to do is use the <code>\caption{''text''}</code> command within the float environment. Because of how LaTeX deals sensibly with logical structure, it will automatically keep track of the numbering of figures, so you do not need to include this within the caption text.

The location of the caption is traditionally underneath the float. However, it is up to you to therefore insert the caption command after the actual contents of the float (but still within the environment). If you place it before, then the caption will appear above the float. Try out the following example to demonstrate this effect:

<table>
<tr>
<td>
<source lang="latex">
\documentclass[a4paper,12pt]{article}

\usepackage[english]{babel}
\usepackage{graphicx}

\begin{document}

\begin{figure}[h!]
  \caption{A picture of a gull.}
  \centering
    \includegraphics[width=0.5\textwidth]{gull}
\end{figure}

\begin{figure}[h!]
  \centering
    \reflectbox{%
      \includegraphics[width=0.5\textwidth]{gull}}
  \caption{A picture of the same gull
           looking the other way!}
\end{figure}

\begin{table}[h!]
  \begin{center}
    \begin{tabular}{| l c r |}
    \hline
    1 & 2 & 3 \\
    4 & 5 & 6 \\
    7 & 8 & 9 \\
    \hline
    \end{tabular}
  \end{center}
  \caption{A simple table}
\end{table}

Notice how the tables and figures
have independent counters.

\end{document}
</source>
</td>
<td>
[[Image:Latex caption example.png|center|347px]]
</td>
</tr>
</table>

note that the command <code>\reflectbox{...}</code> flips its content horizontally.

===Lists of figures and tables===

Captions can be listed at the beginning of a paper or report in a "List of Tables" or a "List of Figures" section by using the <code>\listoftables</code> or <code>\listoffigures</code> commands, respectively. The caption used for each figure will appear in these lists, along with the figure numbers, and page numbers that they appear on.

The <code>\caption</code> command also has an optional parameter, <code>\caption[''short'']{''long''}</code> which is used for the ''List of Tables'' or ''List of Figures''. Typically the <code>short</code> description is for the caption listing, and the <code>long</code> description will be placed beside the figure or table. This is particularly useful if the caption is long, and only a "one-liner" is desired in the figure/table listing. Here is an example of this usage:

{|
|<source lang="latex">
\documentclass[12pt]{article}
\usepackage{graphics}

\begin{document}

\listoffigures

\section{Introduction}

\begin{figure}[hb]
  \centering
  \includegraphics[width=4in]{gecko}
  \caption[Close up of \textit{Hemidactylus} sp.]%
  {Close up of \textit{Hemidactylus} sp., which is
   part the genus of the gecko family. It is the
   second most speciose genus in the family.}
\end{figure}

\end{document}
</source>
| [[Image:LaTeX figure caption with lof entry.png|400px]]
|}

=== Side captions ===

It is sometimes desirable to have a caption appear on the side of a float, rather than above or below. The <code>sidecap</code> package can be used to place a caption beside a figure or table. The following example demonstrates this for a figure by using a <code>SCfigure</code> environment in place of the <code>figure</code> environment.

{|
|<source lang="latex">
\documentclass{article}

\usepackage[pdftex]{graphicx}
\usepackage{sidecap}

\begin{document}

\begin{SCfigure}
  \centering
  \includegraphics[width=5\textwidth]%
    {Giraff_picture}% picture filename
  \caption{ ... caption text ... }
\end{SCfigure}

\end{document}
</source>
|
[[Image:Latex example sidecap.png]]
|}

=== Labels and Cross-referencing ===

Labels and cross-references work fairly similarly to the general case - see the [[LaTeX/Labels and Cross-referencing|Labels and Cross-referencing]] section for more information.

:''Warning:'' If you want to label a figure so that you can reference it later, you have to add the label '''after the caption''' (inside seems to work in LaTeX 2e) but '''inside the floating environment'''. If it is declared outside, it will give the section number. If the label picks up the section or list number instead of the figure number, put the label inside the caption to ensure correct numbering.

=== Wrapping text around figures ===

Although not normally the case in academic writing, an author may prefer that some floats do not break the flow of text, but instead allow text to wrap around it. (Obviously, this effect only looks decent when the figure in question is significantly narrower than the text width.) 

A word of warning: Wrapping figures in LaTex will require a lot of manual adjustment of your document. There are several packages available for the task, but none of them work perfectly. Before you make the choice of including figures with text wrapping in your document, make sure you have considered all the options. For example, you could use a layout with two columns for your documents and have no text-wrapping at all.

Anyway, we will look at the package <code>wrapfig</code>. (Note: wrapfig may not come with the default installation of LaTeX; you might need to install additional packages manually.)

To use <code>wrapfig</code>, you must first add this to the preamble:<source lang="latex">\usepackage{wrapfig}</source> This then gives you access to:

<source lang="latex">
\begin{wrapfigure}[lineheight]{alignment}{width}
</source>

''Alignment'' can normally be either ''l'' for left, or ''r'' for right.  Lowercase ''l'' or ''r'' forces the figure to start precisely where specified (and may cause it to run over page breaks), while capital ''L'' or ''R'' allows the figure to float.  If you defined your document as twosided, the alignment can also be ''i'' for inside or ''o'' for outside, as well as ''I'' or ''O''. The ''width'' is, of course, the width of the figure. An example:

{|
|<source lang="latex">
\begin{wrapfigure}{r}{0.5\textwidth}
  \begin{center}
    \includegraphics[width=0.48\textwidth]{gull}
  \end{center}
  \caption{A gull}
\end{wrapfigure}
</source>
|
[[Image:Latex example wrapfig.png|center|337px]]
|}

Note that we have specified a size for both the <code>wrapfigure</code> environment and the image we have included. We did it in terms of the text width: it is always better to use relative sizes in LaTeX, let LaTeX do the work for you! The "wrap" is slightly bigger than the picture, so the compiler will not return any strange warning and you will have a small white frame between the image and the surrounding text. You can change it to get a better result, but if you don't keep the image smaller than the "wrap", you will see the image ''over'' the text, and this shouldn't be the effect you want to get!

The wrapfig package can also be used with user-defined floats with float package. See below in the [[#Custom Floats|section on custom floats]].

===Tip for figures with too much white space===

It happens that you'll generate figures with too much (or too little) white space on the top or bottom. In such a case, you can simply make use of the optional argument <code>[lineheight]</code>. It specifies the height of the figure in number of lines of text.

Another possibility is adding space within the float using the <code>\vspace{...}</code> command. The argument is the size of the space you want to add, you can use any unit you want, including pt, mm, in, etc. If you provide a negative argument, it will add a ''negative'' space, thus removing some white space. Using <code>\vspace</code> tends to move the caption relative to the float while the <code>[lineheight]</code> argument does not. Here is an example using the <code>\vspace</code> command, the code is exactly the one of the previous case, we just added some negative vertical spaces to shrink everything up:

{|
|<source lang="latex">
\begin{wrapfigure}{r}{0.5\textwidth}
  \vspace{-20pt}
  \begin{center}
    \includegraphics[width=0.48\textwidth]{gull}
  \end{center}
  \vspace{-20pt}
  \caption{A gull}
  \vspace{-10pt}
\end{wrapfigure}
</source>
|
[[Image:Latex example wrapfig vspace.png|center|336]]
|}

In this case it may look too shrunk, but you can manage spaces the way you like. In general, it is best not to add any space at all: let LaTeX do the formatting work!

Alternatively you might use the <code>picins</code> package instead of the wrapfigure package which produces a correct version without the excess white space out of the box without any hand tuning.

There is also an alternative to <code>wrapfig</code>: the package <code>floatflt</code> [http://tug.ctan.org/tex-archive/macros/latex/contrib/floatflt/] - for documentation see [http://www.ctan.org/tex-archive/macros/latex/contrib/floatflt/floatflt.pdf].

=== Subfloats ===

A useful extension is the <code>subfig</code> package [http://www.ctan.org/tex-archive/macros/latex/contrib/subfig/subfig.pdf], which uses subfloats within a single float. This gives the author the ability to have subfigures within figures, or subtables within table floats. Subfloats have their own caption, and an optional global caption.  An example will best illustrate the usage of this package:

<source lang="latex">
\usepackage{subfig}

\begin{figure}
  \centering
  \subfloat[A gull]{\label{fig:gull}\includegraphics[width=0.3\textwidth]{gull}}                
  \subfloat[A tiger]{\label{fig:tiger}\includegraphics[width=0.3\textwidth]{tiger}}
  \subfloat[A mouse]{\label{fig:mouse}\includegraphics[width=0.3\textwidth]{mouse}}
  \caption{Pictures of animals}
  \label{fig:animals}
\end{figure}
</source>

[[Image:Latex example subfig.png|center|500px]]

You will notice that the figure environment is set up as usual. You may also use a table environment for subtables. For each subfloat, you need to use: 

<source lang="latex">
\subfloat[sub caption]{ ... figure or table ... }
</source>

If you intend to cross-reference any of the subfloats, see where the label is inserted; <code>\caption</code> will provide the global caption. 

<code>subfig</code> will arrange the figures or tables side-by-side providing they can fit, otherwise, it will automatically shift subfloats below. This effect can be added manually, by putting the newline command (<code>\\</code>) before the figure you wish to move to a newline.

Horizontal spaces between figures is controlled by one of several commands, which are placed in between each <code>\subfloat{}</code> command:
*Any whitespace (such as spaces, returns, and tabs) will result in one regular space
*Math spaces: <code>\qquad</code>, <code>\quad</code>, <code>\;</code>, and <code>\,</code>
*Generic space: <code>\hspace{''length''}</code>

===Wide figures in two column documents===

If you are writing a document using two columns (i.e. you started your document with something like <code>\documentclass[twocolumn]{article}</code>), you might have noticed that you can't use floating elements that are wider than the width of a column (using a LaTeX notation, wider than <code>0.5\textwidth</code>), otherwise you will see the image overlapping with text. If you really have to use such wide elements, the only solution is to use the "starred" variants of the floating environments, that are <code>{figure*}</code> and <code>{table*}</code>. Those "starred" versions work exactly like the standard ones, but they will be as wide as the page, so you will get no overlapping.

A bad point of those environments is that they can be placed only at the top of the page or on their own page. If you try to specify their position using modifiers like ''b'' or ''h'' they will be ignored. Add <code>\usepackage{stfloats}</code> to the preamble in order to alleviate this problem with regard to placing these floats at the bottom of a page, using the optional specifier <code>[b]</code>. Default is <code>[tbp]</code>. However, ''h'' still does not work.

To prevent the figures from being placed out-of-order with respect to their "non-starred" counterparts, the package <code>fixltx2e</code>  <ref>http://www.tex.ac.uk/cgi-bin/texfaq2html?label=2colfltorder</ref> should be used (e.g. <code>\usepackage{fixltx2e}</code>).

=== Custom Floats ===

If tables and figures are not adequate for your needs, then you always have the option to create your own! Examples of such instances could be source code examples, or maps. For a program float example, one might therefore wish to create a float named <code>program</code>. The package <code>float</code> is your friend for this task. ''All commands to set up the new float must be placed in the preamble, and not within the document.''

# Add <code>\usepackage{float}</code> to the preamble of your document
# Declare your new float using: <code>\newfloat{type}{placement}{ext}[outer counter]</code>, where:
#* ''type'' - the new name you wish to call your float, in this instance, 'program'.
#* ''placement'' - t, b, p, or h (as previously described in [[#placement|Placement]]), where letters enumerate permitted placements.
#* ''ext'' - the file name extension of an auxiliary file for the list of figures (or whatever). Latex writes the captions to this file. 
#* ''outer counter'' - the presence of this parameter indicates that the counter associated with this new float should depend on outer counter, for example 'chapter'.
# The default name that appears at the start of the caption is the type. If you wish to alter this, use <code>\floatname{type}{floatname}</code>
# Changing float style can be issued with <code>\floatstyle{style}</code> (Works on all subsequent <code>\newfloat</code> commands, therefore, must be inserted before <code>\newfloat</code> to be effective). 
#* <tt>plain</tt> - the normal style for Latex floats, i.e., nothing!
#* <tt>boxed</tt> - a box is drawn that surrounds the float, and the caption is printed below.
#* <tt>ruled</tt> - the caption appears above the float, with rules immediately above and below. Then the float contents, followed by a final horizontal rule.

Float styles can also be customized as the second example below illustrates.

An example document using a new <code>program</code> float type:

<source lang="latex">
\documentclass{article}

\usepackage{float}

\floatstyle{ruled}
\newfloat{program}{thp}{lop}
\floatname{program}{Program}

\begin{document}

\begin{program}
  \begin{verbatim}

class HelloWorldApp {
  public static void main(String[] args) {
    //Display the string
    System.out.println("Hello World!");
  }
}
\end{verbatim}
  \caption{The Hello World! program in Java.}
\end{program}

\end{document}
</source>

The <code>verbatim</code> environment is an environment that is already part of Latex. Although not introduced so far, its name is fairly intuitive! Latex will reproduce everything you give it, including new lines, spaces, etc. It is good for source code, but if you want to introduce a lot of code you might consider using the <code>[[LaTeX/Packages/Listings|listings]]</code> package, that was made just for it.

While this is useful, one should be careful when embedding the float within another float. In particular, the error <code>not in outer par mode</code> may occur. One solution might be to use the [H] option (not any other) on the inner float, as this option "pins" the inner float to the outer one.

Newly created floats with ''newfloat'' can also be used in combination with the ''wrapfig'' package from above. E.g. the following code creates a floating text box, which floats in the text on the right side of the page and is complete with caption, numbering, an index file with the extension .lob and a customization of the float's visual layout:

<source lang="latex">
\documentclass{article}

% have hyperref package before float in order to get strange errors with .\theHfloatbox
\usepackage[pdftex]{hyperref}

\usepackage{float}

%allows use of "@" before \begin{document}
\makeatletter

% this creates a custom and simpler ruled box style
\newcommand\floatc@simplerule[2]{{\@fs@cfont #1 #2}\par}
\newcommand\fs@simplerule{\def\@fs@cfont{\bfseries}\let\@fs@capt\floatc@simplerule
  \def\@fs@pre{\hrule height.8pt depth0pt \kern4pt}%
  \def\@fs@post{\kern4pt\hrule height.8pt depth0pt \kern4pt \relax}%
  \def\@fs@mid{\kern8pt}%
  \let\@fs@iftopcapt\iftrue}

% this code block defines the new and custom floatbox float environment
\floatstyle{simplerule}
\newfloat{floatbox}{thp}{lob}[section]
\floatname{floatbox}{Text Box}

\begin{document}

\begin{floatbox}{r}{}
  \textit{Bootstrapping} is a resampling technique used 
  for robustly estimating statistical quantities, such as 
  the model fit $R^2$. It offers some protection against 
  the sampling bias.
  \caption{Bootstrapping}
\end{floatbox}

\end{document}
</source>

=== Caption Styles ===

To change the appearance of captions, use the <code>caption</code> [http://mirror.ctan.org/macros/latex/contrib/caption/caption-eng.pdf] package. For example, to make all caption labels small and bold:

<source lang="latex">
\usepackage[small,bf]{caption}
</source>

The KOMA script packages [http://www.komascript.de/] have their own caption customizing features with e.g. <code>\captionabove</code>, <code>\captionformat</code> and <code>\setcapwidth</code>. However these definitions have limited effect on newly created float environments with the <code>wrapfig</code> package.

==Labels in the figures==

There is a LaTeX package [http://www.math.uni-leipzig.de/~matveyev/lpic/ lpic] to put LaTeX on top of included graphics, thus allowing to add TeX annotations to imported graphics. 
It defines a convenient interface to put TeX over included graphics, and allows for drawing a white background under the typeset material to overshadow the graphics.
It is a better alternative for labels inside of graphics; you do not have to change text size when rescaling pictures, and all LaTeX power is available for labels.

A very similar package, with somewhat different syntax, is [http://www.ctan.org/tex-archive/help/Catalogue/entries/pinlabel.html pinlabel].  The link given also points to the packages psfrag and overpic.

=== Summary ===

That concludes all the fundamentals of floats. You will hopefully see how much easier it is to let Latex do all the hard work and tweak the page layouts in order to get your figures in the best place. As always, the fact that LaTeX takes care of all caption and reference numbering is a great time saver.

<noinclude>
{{LaTeX/Bottom|Importing Graphics|Presentations}}

{{A-Roberts}}
</noinclude>

== References == 
{{reflist|2}}
