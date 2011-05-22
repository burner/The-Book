><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

Documents produced with the commands you have learned up to this point will look acceptable to a large audience. While they are not fancy-looking, they obey all the established rules of good typesetting, which will make them easy to read and pleasant to look at. However, there are situations where LaTeX does not provide a command or environment that matches your needs, or the output produced by some existing command may not meet your requirements.

In this chapter, I will try to give some hints on how to teach LaTeX new tricks and how to make it produce output that looks different from what is provided by default.

==New commands==

To add your own commands, use the
<source lang="latex">
\newcommand{name}[num]{definition}
</source>
command. Basically, the command requires two arguments: the ''name'' of the command you want to create, and the ''definition'' of the command. The ''num'' argument in square brackets is optional and specifies the number of arguments the new command takes (up to 9 are possible). If missing it defaults to 0, i.e. no argument allowed.

The following two examples should help you to get the idea. The first example defines a new command called <tt>\wbal</tt> that will print "The Wikibook about LaTeX". Such a command could come in handy if you had to write the title of this book over and over again.

{|
|<source lang="latex">
\newcommand{\wbal}{The Wikibook about \LaTeX}
This is ‘‘\wbal'' \ldots{} ‘‘\wbal''
</source>
|This is “The Wikibook about LaTeX” … “The Wikibook about LaTeX”
|}

The next example illustrates how to define a new command that takes one argument. The <tt>#1</tt> tag gets replaced by the argument you specify. If you wanted to use more than one argument, use <tt>#2</tt> and so on, these arguments are added in an extra set of brackets.

{|
|<source lang="latex">
\newcommand{\wbalsup}[1] {This is the Wikibook about LaTeX supported by #1}
\newcommand{\wbalTwo}[2] {This is the Wikibook about LaTeX supported by #1 #2}
% in the document body:
\begin{itemize}
\item \wbalsup{Wikimedia}
\item \wbalsup{lots of users!}
\item \wbalTwo{John}{Doe}
\end{itemize}
</source>
|
*This is the Wikibook about LaTeX supported by Wikimedia
*This is the Wikibook about LaTeX supported by lots of users!
*This is the Wikibook about LaTeX supported by John Doe
|}

Note: use <tt>\wbalTwo</tt>, not <tt>\wbal2</tt> (error on compiling)

LaTeX will not allow you to create a new command that would overwrite an existing one. But there is a special command in case you explicitly want this: <tt>\renewcommand</tt>. It uses the same syntax as the <tt>\newcommand</tt> command.

In certain cases you might also want to use the <tt>\providecommand</tt> command. It works like <tt>\newcommand</tt>, but if the command is already defined, LaTeX will silently ignore it.


With LaTex2e, it is also possible to add a default parameter to a command with the following syntax:
<source lang="latex">
\newcommand{name}[num][default]{definition}
</source>
If the default parameter of <tt>\newcommand</tt> is present, then the first of the number of arguments specified by <tt>num</tt> is optional with a default value of <tt>default</tt>; if absent, then all of the arguments are required.
{|
|<source lang="latex">
\newcommand{\wbalTwo}[2][Wikimedia]{This is the Wikibook about LaTeX supported by {#1} and {#2}!}
% in the document body:
\begin{itemize}
\item \wbalTwo{John Doe}
\item \wbalTwo[lots of users]{John Doe}
\end{itemize}
</source>
|
*This is the Wikibook about LaTeX supported by Wikimedia and John Doe!
*This is the Wikibook about LaTeX supported by lots of users and John Doe!
|}

NOTE: when the command is used with an explicit first parameter it is given enclosed with brackets ( "<tt>[lots of users]</tt>" ).

==New Environments==

Just as with the <tt>\newcommand</tt> command, there is a command to create your own environments. The <tt>\newenvironment</tt> command uses the following syntax:
<source lang="latex">
\newenvironment{name}[num]{before}{after}
</source>

Again <tt>\newenvironment</tt> can have an optional argument. The material specified in the ''before'' argument is processed before the text in the environment gets processed. The material in the ''after'' argument gets processed when the <tt>\end{''name''}</tt> command is encountered.

The ''num'' argument is used the same way as in the <tt>\newcommand</tt> command. LaTeX makes sure that you do not define an environment that already exists. If you ever want to change an existing command, you can use the <tt>\renewenvironment</tt> command. It uses the same syntax as the <tt>\newenvironment</tt> command.

The example below illustrates the usage of the <tt>\newenvironment</tt> command:

{|
|<source lang="latex">
\newenvironment{king}
{\rule{1ex}{1ex}\hspace{\stretch{1}}}
{\hspace{\stretch{1}}\rule{1ex}{1ex}}

\begin{king}
My humble subjects \ldots
\end{king}
</source>
|[[Image:Latex example newenvironment.png|300px]]
|}

==Extra space==

When creating a new environment you may easily get bitten by extra spaces
creeping in, which can potentially have fatal effects. For example when you
want to create a title environment which suppresses its own indentation as well
as the one on the following paragraph. The <tt>\ignorespaces</tt> command in the
begin block of the environment will make it ignore any space after executing
the begin block. The end block is a bit more tricky as special processing
occurs at the end of an environment. With the <tt>\ignorespacesafterend</tt>
LaTeX will issue an <tt>\ignorespaces</tt> after the special ‘end’ processing has
occurred.


{|
|<source lang="latex">
\newenvironment{simple}%
{\noindent}%
{\par\noindent}

\begin{simple}
See the space\\to the left.
\end{simple}
Same\\here.
</source>
|<pre>
  See the space
to the left.

  Same
here.
</pre>
|}

{|
|<source lang="latex">
\newenvironment{correct}%
{\noindent\ignorespaces}%
{\par\noindent%
\ignorespacesafterend}

\begin{correct}
No space\\to the left.
\end{correct}
Same\\here.
</source>
|<pre>
No space
to the left.

Same
here.
</pre>
|}

Also, if you're still having problems with extra space being appended at the end of your environment when using the <tt>\input</tt> for external source, make sure there is no space between the beginning, sourcing, and end of the environment, such as:

<source lang="latex">\begin{correct}\input{somefile.tex}\end{correct}</source>

==Command-line LaTeX==

If you work on a Unix-like OS, you might be using Makefiles or any kind of script to build your LaTeX projects. In that connection it might be interesting to produce different versions of the same document by calling LaTeX with command-line parameters. If you add the following structure to your document:
<source lang="latex">
\usepackage{ifthen}
\ifthenelse{\equal{\blackandwhite}{true}}{
% "black and white" mode; do something..
}{
% "color" mode; do something different..
}
</source>

Now you can call LaTeX like this:
 latex '\newcommand{\blackandwhite}{true}\input{test.tex}'

First the command <tt>\blackandwhite</tt> gets defined and then the actual file is read with input. By setting <tt>\blackandwhite</tt> to false the color version of the document would be produced.

==Creating your own package==

If you define a lot of new environments and commands, the preamble of your document will get quite long. In this situation, it is a good idea to create a LaTeX package containing all your command and environment definitions. You can then use the <tt>\usepackage</tt> command to make the package available in your document. Writing a package basically consists of copying the contents of your document preamble into a separate file with a name ending in <tt>.sty</tt>.

It is very simple, just follow the steps:
# create a simple text file called ''mypack.sty'' (or any other name you like) and open it with any text editor
# at the very beginning of the text document just write<source lang="latex">
\ProvidesPackage{mypack}</source>
#:note: it has to have the same name of the file without the extension. It tells LaTeX the name of the package and will allow it to issue a sensible error message when you try to include a package twice.
# write whatever you want in it using all the LaTeX commands you know. Normally you should define new commands or import other packages.
# import your new package with the known command<source lang="latex">
\usepackage{mypack}</source>
#or<source lang="latex">
\RequirePackage{mypack}</source>
the file ''mypack.sty'' and the LaTeX source you are compiling must be in the same directory. It will be as though all you have written within your package is within the document itself.

Alternatively, it is possible to place the package within ''~/texmf/tex/latex/mypack/mypack.sty'' where '~' is your home directory (On Windows this is often ''C:\Users\username'') and where ''mypack'' is the name of your package.  Running <tt>texhash</tt> or equivalent will allow you to use your package as detailed above, but without it needing to be in the same directory as your document.

==Creating your own style==

It is also possible to create your own style file. The process is similar to the creation of your own package, you can call your own style file in the preamble of any document by  the command:
<source lang="latex">
\documentclass{mystyle}
</source>
The name of the style file is then ''mystyle.cls'' and can be opened with any text editor. At the beginning of this file, the following line has to be provided:
<source lang="latex">
\ProvidesClass{mystyle}
</source>
again, within the style files other files or packages are imported by the requirepackage command.

==Spacing==

===Line Spacing===

If you want to use larger inter-line spacing in a document, you can change its value by putting the
<source lang="latex">
\linespread{factor}
</source>

command into the preamble of your document. Use <tt>\linespread{1.3}</tt> for "one and a half" line spacing, and <tt>\linespread{1.6}</tt> for "double" line spacing. Normally the lines are not spread, so the default line spread factor is 1. 

The <tt>setspace</tt> package allows more fine-grained control over line spacing. To set "one and a half" line spacing document-wide, but not where it is usually unnecessary (e.g. footnotes, captions):

<source lang="latex">
\usepackage{setspace}
%\singlespacing
\onehalfspacing
%\doublespacing
%\setstretch{1.1}
</source>

To change line spacing within the document, the <tt>setspace</tt> package provides the environments <tt>singlespace</tt>, <tt>onehalfspace</tt>, <tt>doublespace</tt> and <tt>spacing</tt>:

<source lang="latex">
This paragraph has \\ default \\ line spacing.
 
\begin{doublespace}
  This paragraph has \\ double \\ line spacing.
\end{doublespace}
 
\begin{spacing}{2.5}
  This paragraph has \\ huge gaps \\ between lines.
\end{spacing}
</source>

===Paragraph formatting===

In LaTeX, there are two parameters influencing paragraph layout. By placing a definition like:

<source lang="latex">
\setlength{\parindent}{0pt}
\setlength{\parskip}{1ex plus 0.5ex minus 0.2ex}
</source>

in the preamble of the input file, you can change the layout of paragraphs. These two commands increase the space between two paragraphs while setting the paragraph indent to zero.

The <tt>plus</tt> and <tt>minus</tt> parts of the length above tell TeX that it can compress and expand the inter paragraph skip by the amount specified, if this is necessary to properly fit the paragraphs onto the page. In continental Europe, paragraphs are often separated by some space and not indented. But beware, this also has its effect on the table of contents. Its lines get spaced more loosely now as well. To avoid this, you might want to move the two commands from the preamble into your document to some place below the command <tt>\tableofcontents</tt>. You may want to consider whether or not you want to use paragraph spacing. Most professional books use indenting and not spacing to separate paragraphs.

If you want to indent a paragraph that is not indented, you can use
<source lang="latex">
\indent
</source>
at the beginning of the paragraph. Obviously, this will only have an effect when <tt>\parindent</tt> is not set to zero. If you want to indent the beginning of every section, you can use the <tt>indentfirst</tt> package, see the chapter about [[LaTeX/Packages]] for more information. 

To create a non-indented paragraph, you can use
<source lang="latex">
\noindent
</source>

as the first command of the paragraph. This might come in handy when you start a document with body text and not with a sectioning command.

===Horizontal Space===
LaTeX determines the spaces between words and sentences automatically. To add horizontal space, use:
<source lang="latex">
\hspace{length}
</source>

If such a space should be kept even if it falls at the end or the start of a line, use <tt>\hspace*</tt> instead of <tt>\hspace</tt>. The length in the simplest case is just a number plus a unit, e.g. <tt>\hspace{1.5 cm}</tt>. For a list of the possible units, see the [[LaTeX/Useful Measurement Macros|Useful Measurement Macros]] appendix.

The command:
<source lang="latex">
\stretch{n}
</source>
generates a special rubber space. It stretches until all the remaining space on a line is filled up. If two <tt>\hspace{\stretch{n}}</tt> commands are issued
on the same line, they grow according to the stretch factor.
{|
|<source lang="latex">
x\hspace{\stretch{1}}
x\hspace{\stretch{3}} x
</source>
|<pre>
x      x                  x
</pre>
|}

===Vertical Space===

The space between paragraphs, sections, subsections, etc. is determined automatically by LaTeX. If you want to customize the default paragraph spacing, it can be achieved with the following command in the preamble of your document:
<source lang="latex">
\parskip 7.2pt
</source>

If necessary, additional vertical space ''between two paragraphs'' can be added with the command:
<source lang="latex">
\vspace{length}
</source>
This command should normally be used between two empty lines. If the space should be preserved at the top or at the bottom of a page, use the starred version of the command, <tt>\vspace*</tt>, instead of <tt>\vspace</tt>. The <tt>\stretch</tt> command, in connection with <tt>\pagebreak</tt>, can be used to typeset text on the last line of a page, or to center text vertically on a page.

Additional space between two lines of the same paragraph or within a table is specified with the
<source lang="latex">
\\[length]
</source>
command.

If you want to add space at the beginning of the document, without anything else written before, then you may use
<source lang="latex">
{ \vspace*{length} }
</source>
It's important you use the <tt>\vspace*</tt> command instead of <tt>\vspace</tt>, otherwise latex can silently ignore the extra space.

<noinclude>
{{LaTeX/Bottom|Advanced Topics|Multiple files}}
</noinclude>
