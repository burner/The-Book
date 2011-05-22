>{{todo|
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{todo|
* Add other packages for creating presentations. 
* Bonus: Add screenshots of the results. 
* Working with columns}}

<noinclude>{{LaTeX/Top}}</noinclude>

LaTeX can be used for creating presentations. There are several packages for the task, including the Beamer package.

== The Beamer package ==

The beamer package is provided with most LaTeX distributions, but is also available from [http://www.ctan.org/tex-archive/macros/latex/contrib/beamer/ CTAN]. If you use MikTeX, all you have to do is to include the beamer package and let LaTeX download all wanted packages automatically. The [http://www.ctan.org/tex-archive/macros/latex/contrib/beamer/doc/beameruserguide.pdf documentation] explains the features in great detail. You can also have a look at the PracTex article '''Beamer by example'''<ref>Andrew Mertz and William Slough ''Beamer by Example''</ref>

=== Introductory example ===

The beamer package is loaded by calling the <tt>beamer</tt> class:
<source lang="latex">
\documentclass{beamer}
</source>
The usual header information may then be specified. Note that if you are compiling with XeTeX then you should use
<source lang="latex">
\documentclass[xetex,mathserif,serif]{beamer}
</source>

Inside the usual <tt>document</tt> environment, multiple <tt>frame</tt> environments specify the content to be put on each slide. The <tt>frametitle</tt> command specifies the title for each slide (See image):
<source lang="latex">
\begin{document}
  \begin{frame}
    \frametitle{This is the first slide}
    %Content goes here
  \end{frame}
  \begin{frame}
    \frametitle{This is the second slide}
    \framesubtitle{A bit more information about this}
    %More content goes here
  \end{frame}
% etc
\end{document}
</source>

[[Image:Frametitle_keyword_example.png]]

Usual environments (<tt>itemize</tt>,<tt>enumerate</tt>, <tt>equation</tt>, etc.) may be used as usual.

Inside of frames, you can use environments like <tt>block</tt>, <tt>theorem</tt>, <tt>proof</tt>, ... Also, <tt>\maketitle</tt> is possible to create the frontpage, if <tt>title</tt> and <tt>author</tt> is set.

Trick : Instead of using ''\begin{frame}…\end{frame}'', you can also use ''\frame{…}''.

For the actual talk, if you can compile it with pdfLaTeX then you could use Adobe Reader with its fullscreen mode.  If you want to navigate in your presentation, you can use the almost invisible links in the bottom right corner without leaving the fullscreen mode.

=== Document Structure ===

==== Title page and information ====

You give information about authors, titles and dates in the preamble
<source lang="latex">
\title[Crisis]%(optional, only for long titles)
{The Economics of Financial Crisis}
\subtitle{Evidence from India}
\author[Author, Anders] % (optional, for multiple authors)
{F.~Author\inst{1} \and S.~Anders\inst{2}}
\institute[Universitäten Hier und Dort] % (optional)
{
  \inst{1}%
  Institut für Informatik\\
  Universität Hier
  \and
  \inst{2}%
  Institut für theoretische Philosophie\\
  Universität Dort
}
\date[KPT 2003] % (optional)
{Konferenz über Präsentationstechniken, 2004}
\subject{Informatik}
</source>

In the document, you add the title page : 
<source lang="latex">
\frame{\titlepage}
</source>

==== Table of Contents ====

You can print the table of contents and highlight the current section/subsection by typing : 

<source lang="latex">
\begin{frame}
\frametitle{Table of Contents}
\tableofcontents[currentsection]
\end{frame}
</source>

You can automatically print the table of contents at the beginning of each section by adding in the preamble the following line. 

<source lang="latex">
\AtBeginSection[]{\begin{frame}\frametitle{Table of Contents}\tableofcontents[currentsection]\end{frame}} 
</source>

You can do the same for subsections : 

<source lang="latex">
\AtBeginSubsection[]
{
  \begin{frame}<beamer>
    \frametitle{Gliederung}
    \tableofcontents[currentsection,currentsubsection]
  \end{frame}
}
</source>


==== References (Beamer)====

<source lang="latex">
\begin{frame}[allowframebreaks]
  \frametitle<presentation>{Weiterf¸hrende Literatur}    
  \begin{thebibliography}{10}    
  \beamertemplatebookbibitems
  \bibitem{Autor1990}
    A.~Autor.
    \newblock {\em Einf¸hrung in das Pr‰sentationswesen}.
    \newblock Klein-Verlag, 1990.
  \beamertemplatearticlebibitems
  \bibitem{Jemand2000}
    S.~Jemand.
    \newblock On this and that.
    \newblock {\em Journal of This and That}, 2(1):50--100, 2000.
  \end{thebibliography}
\end{frame}
</source>



=== Style ===


==== Themes ====

The first solution is to use a built-in theme such as Warsaw, Berlin, etc. The second solution is to specify colors, inner themes and outer themes.

===== The Built-in solution =====

To the preamble you can add the following line:

<source lang="latex">
\usetheme{Warsaw}
</source>
to use the "Warsaw" theme. <tt>Beamer</tt> has several themes, many of which are named after cities (e.g. Barcelona, Madrid, Berlin, etc.). Color themes, typically with animal names, can be specified with
<source lang="latex">
\usecolortheme{beaver}
</source>

This [http://www.hartwork.org/beamer-theme-matrix/ Theme Matrix] contains the various theme and color combinations included with <tt>beamer</tt>. For more customizing options, have a look to the official documentation included in your distribution of <tt>beamer</tt>, particularly the part ''Change the way it looks''.

The list of all themes : Antibes, Bergen, Berkeley, Berlin, Boadilla, Copenhagen, Darmstadt, Dresden, Frankfurt, Goettingen, Hannover, Ilmenau, JuanLesPins, Luebeck, Madrid, Malmoe, Marburg, Montpellier, PaloAlto, Pittsburgh, Rochester, Singapore, Szeged, Warsaw, boxes, default

===== The ''do it yourself'' solution =====

First you can specify the ''outertheme''. The outertheme defines the head and the footline of each slide. 

<source lang="latex">
\useoutertheme{infolines} 
</source>

Here is a list of all available outer themes
* infolines 
* miniframes
* shadow
* sidebar
* smoothbars
* smoothtree
* split
* tree

Then you can add the innertheme : 

<source lang="latex">
\useinnertheme{rectangles} 
</source>

Here is a list of all available inner themes : 

* rectangles
* circles
* inmargin
* rounded

You can define the color of every element : 

<source lang="latex">
\setbeamercolor{alerted text}{fg=orange}
\setbeamercolor{background canvas}{bg=white}
\setbeamercolor{block body alerted}{bg=normal text.bg!90!black}
\setbeamercolor{block body}{bg=normal text.bg!90!black}
\setbeamercolor{block body example}{bg=normal text.bg!90!black}
\setbeamercolor{block title alerted}{use={normal text,alerted text},fg=alerted text.fg!75!normal text.fg,bg=normal text.bg!75!black}
\setbeamercolor{block title}{bg=blue}
\setbeamercolor{block title example}{use={normal text,example text},fg=example text.fg!75!normal text.fg,bg=normal text.bg!75!black}
\setbeamercolor{fine separation line}{}
\setbeamercolor{frametitle}{fg=brown}
\setbeamercolor{item projected}{fg=black}
\setbeamercolor{normal text}{bg=black,fg=yellow}
\setbeamercolor{palette sidebar primary}{use=normal text,fg=normal text.fg}
\setbeamercolor{palette sidebar quaternary}{use=structure,fg=structure.fg}
\setbeamercolor{palette sidebar secondary}{use=structure,fg=structure.fg}
\setbeamercolor{palette sidebar tertiary}{use=normal text,fg=normal text.fg}
\setbeamercolor{section in sidebar}{fg=brown}
\setbeamercolor{section in sidebar shaded}{fg= grey}
\setbeamercolor{separation line}{}
\setbeamercolor{sidebar}{bg=red}
\setbeamercolor{sidebar}{parent=palette primary}
\setbeamercolor{structure}{bg=black, fg=green}
\setbeamercolor{subsection in sidebar}{fg=brown}
\setbeamercolor{subsection in sidebar shaded}{fg= grey}
\setbeamercolor{title}{fg=brown}
\setbeamercolor{titlelike}{fg=brown}




</source>

Remember that you can define your own colors : 

<source lang="latex">
\definecolor{chocolate}{RGB}{33,33,33}
</source>

You can also define the style of blocks : 

<source lang="latex">
\setbeamertemplate{blocks}[shadow=false]
</source>

<source lang="latex">
\setbeamertemplate{background canvas}[vertical shading][bottom=white,top=structure.fg!25]
</source>

You can also suppress the navigation bar, which is not very useful: 
<source lang="latex">
\beamertemplatenavigationsymbolsempty 
</source>

==== Fonts ====

You may also change the fonts for particular elements. If you wanted the title of the presentation as rendered by <tt>\frame{\titlepage}</tt> to occur in a serif font instead of the default sanserif, you would use:

<source lang="latex">
\setbeamerfont{title}{family=\rm}
</source>

You could take this a step further if you are using OpenType fonts with Xe(La)TeX and specify a serif font with increased size and oldstyle proportional alternate number glyphs:

<source lang="latex">
\setbeamerfont{title}{family=\rm\addfontfeatures{Scale=1.18, Numbers={Lining, Proportional}}}
</source>

===== Math Fonts =====
The default settings for <tt>beamer</tt> use a different set of math fonts than one would expect from creating a simple math article. One quick fix for this is to use at the beginning of the file the option <tt>mathserif</tt>

<source lang="latex">
\documentclass[mathserif]{beamer}
</source>

Others have proposed to use the command

<source lang="latex">
\usefonttheme[onlymath]{serif}
</source>

but it is not clear if this works for absolutely every math character.

==== Frames Options ====

The ''plain'' option. Sometimes you need to include a large figure or a large table and you don't want to have the bottom and the top of the slides. In that case, use the plain option : 
 
<source lang="latex">
\frame[plain]{
…
}
</source>

If you want to include lots of text on a slide, use the ''shrink'' option.
<source lang="latex">
\frame[shrink]{
…
}
</source>

==== Text animations ====
You can simply use the ''\pause'' statement :

<source lang="latex">
\begin{frame}
\frametitle{Some background}
We start our discussion with some concepts.
\pause
The first concept we introduce originates with Erd\H os.
\end{frame}
</source>


For text animations, for example in the itemize environment, you can write:
<source lang="latex">
\begin{itemize}
  \item This one is always shown
  \item<1-> The first time
  \item<2-> The second time
  \item<1-> Also the first time
  \only<1-> This one is shown at the first time, but it will hide soon.
\end{itemize}
</source>


<source lang="latex">
\begin{frame}
	\frametitle{`Hidden higher-order concepts?'}
	\begin{itemize}[<+->]
	\item The truths of arithmetic which are independent of PA in some 
	sense themselves `{contain} essentially {\color{blue}{hidden higher-order}},
	 or infinitary, concepts'???
	\item `Truths in the language of arithmetic which \ldots
	\item	That suggests stronger version of Isaacson's thesis. 
	\end{itemize}
\end{frame}
</source>

==== Handout mode====
In beamer class, the default mode is ''presentation'' which makes the slides. However, you can work in a different mode that is called ''handout'' by setting this option when calling the class:
<source lang="latex">
\documentclass[12pt,handout]{beamer}
</source>
This mode is useful to see each slide only one time with all its stuff on it, making the itemize<+-> to be there all at once (for instance, printable version). Nevertheless, this makes an issue when working with the only command, because its purpose is to have ''only'' some text or figures at a time and not all of them together.

If you want to solve this, you can add a statement to precise the behavior it must have when dealing with only commands in handout mode. Suppose you have a code like this
<source lang="latex">
\only<1>{\includegraphics{pic1.eps}}
\only<2>{\includegraphics{pic2.eps}}
</source>
These pictures being completely different, you want them both in the handout, but they cannot be both on the same slide since they are large. The solution is to add the handout statement to have the following:
<source lang="latex">
\only<1| handout:1>{\includegraphics{pic1.eps}}
\only<2| handout:2>{\includegraphics{pic2.eps}}
</source>
This will ensure the handout will make a slide for each picture.

Now imagine you still have your two pictures with the only statements, but the second one show the first one plus some other graphs and you don't need the first one to appear in the handout. You can thus precise the handout mode not to include some only commands by:
<source lang="latex">
\only<1| handout:0>{\includegraphics{pic1.eps}}
\only<2>{\includegraphics{pic2.eps}}
</source>

The command can also be used to hide frames, e.g.
<source lang="latex">
\begin{frame}<handout:0>
</source>
or even, if you have written a frame that you don't want anymore but maybe you will need it later, you can write
<source lang="latex">
\begin{frame}<0| handout:0>
</source>
and this will hide your slide in both modes. (The order matters. Don't put handout:0|beamer:0 or it won't work.)

A last word about the handout mode is about the notes. Actually, the full syntax for a frame is 
<source lang="latex">
\begin{frame}
...
\end{frame}
\note{...}
\note{...}
...
</source>
and you can write your notes about a frame in the field ''note'' (many of them if needed). Using this, you can add an option to the class calling, either
<source lang="latex">
\documentclass[12pt,handout,notes=only]{beamer}
</source>
or
<source lang="latex">
\documentclass[12pt,handout,notes=show]{beamer}
</source>
The first one is useful when you make a presentation to have only the notes you need, while the second one could be given to those who have followed your presentation or those who missed it, for them to have both the slides with what you said.

Note that the 'handout' option in the \documentclass line suppress all the animations.

'''Important:''' the ''notes=only'' mode is '''literally''' doing only the notes. This means there will be no output file but the DVI. Thus it requires you to have run the compilation in another mode before. If you use separate files for a better distinction between the modes, you may ned to copy the .aux file from the handout compilation with the slides (w/o the notes).

==== Columns and Blocks ====

There are two handy environments for structuring a slide: "blocks", which divide the slide (horizontally) into headed sections, and "columns" which divides a slide (vertically) into columns.

===== Columns =====

Example
<source lang="latex">
\begin{frame}
    \begin{columns}[c] % the "c" option specifies center vertical alignment
    \column{.5\textwidth} % column designated by a command
     Contents of the first column
    \column{.5\textwidth}
     Contents split \\ into two lines
    \end{columns}
\end{frame}

\begin{frame}
     \begin{columns}[t] % contents are top vertically aligned
     \begin{column}[T]{5cm} % each column can also be its own environment
     Contents of first column \\ split into two lines
     \end{column}
     \begin{column}[T]{5cm} % alternative top-align that's better for graphics
          \includegraphics[height=3cm]{graphic.png}
     \end{column}
     \end{columns}
\end{frame} 
</source>

===== Blocks =====

Enlosing text in the ``block'' environment creates a distinct, headed block of text. This allows to visually distinguish parts of a slide easily. There are three basic types of block. Their formating depends on the theme being used.

Simple
<source lang="latex">
\begin{frame}

   \begin{block}{This is a Block}
      This is important information
   \end{block}

   \begin{alertblock}{This is an Alert block}
   This is an important alert
   \end{alertblock}

   \begin{exampleblock}{This is an Example block}
   This is an example 
   \end{exampleblock}

\end{frame}
</source>

=== PDF options ===

You can specify the default options of your PDF. 

<source lang="latex">
\hypersetup{pdfstartview={FitH}} % By default the pdf fit the width of the screen.
</source>

== The powerdot package ==

The powerdot package is available from [http://www.ctan.org/tex-archive/macros/latex/contrib/powerdot/ CTAN].  The [http://www.ctan.org/get/macros/latex/contrib/powerdot/doc/powerdot.pdf documentation] explains the features in great detail.

The powerdot package is loaded by calling the <tt>powerdot</tt> class:
<source lang="latex">
\documentclass{powerdot}
</source>
The usual header information may then be specified.

Inside the usual <tt>document</tt> environment, multiple <tt>slide</tt> environments specify the content to be put on each slide.
<source lang="latex">
\begin{document}
  \begin{slide}{This is the first slide}
    %Content goes here
  \end{slide}
  \begin{slide}{This is the second slide}
    %More content goes here
  \end{slide}
% etc
\end{document}
</source>


== References ==

{{reflist}}

==Links==
* [[Wikipedia:Beamer (LaTeX)]]
* [http://www.ctan.org/tex-archive/macros/latex/contrib/beamer/doc/beameruserguide.pdf beamer user guide] (pdf) from CTAN
* [http://www.math-linux.com/spip.php?article77 A tutorial for creating a presentation using beamer package]
* [http://www.ctan.org/get/macros/latex/contrib/powerdot/doc/powerdot.pdf The powerdot class] (pdf) from CTAN
* [http://happymutant.com/latex/misce/beamer.php Making LaTeX Beamer Presentations ]
<noinclude>
{{LaTeX/Bottom|Floats,_Figures_and_Captions|Hyperlinks}}
</noinclude>
----
