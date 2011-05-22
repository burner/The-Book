><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

Add-on features for LaTeX are known as packages. Dozens of these are pre-installed with LaTeX and can be used in your documents immediately. They should all be stored in subdirectories of <tt>texmf/tex/latex</tt> named after each package. To find out what other packages are available and what they do, you should use the [http://tug.ctan.org/search.html CTAN search page] which includes a link to Graham Williams' comprehensive package catalogue.
A package is a file or collection of files containing extra LaTeX commands and programming which add new styling features or modify those already existing. Installed package files all end with <tt>.sty</tt> (there may be ancillary files as well). When you try to typeset a document which requires a package which is not installed on your system, LaTeX will warn you with an error message that it is missing, and you can then download the package and install it using the instructions in the [[LaTeX/Packages/Installing Extra Packages|installing extra packages]] section. You can also download updates to packages you already have (both the ones that were installed along with your version of LaTeX as well as ones you added). There is no limit to the number of packages you can have installed on your computer (apart from disk space!), but there is probably a physical limit to the number that can be used inside any one LaTeX document at the same time, although it depends on how big each package is. In practice there is no problem in having even a couple of dozen packages active.

==Using an existing package==

To use a package already installed on your system, insert a <tt>\usepackage</tt> command in your document preamble with the package name in curly braces:
<source lang="latex">
\usepackage{package_name}
</source>

For example, to use the <tt>color</tt> package, which lets you typeset in colors, you would type:

<source lang="latex">
\documentclass[11pt,a4paper,oneside]{report}

\usepackage{color}

\begin{document}
...
\end{document}
</source>

You can include several package names in one \usepackage command by separating the names with commas, like this:

<source lang="latex">
\usepackage{package1,package2,package3}
</source>

and you can have more than one \usepackage command. Some packages allow optional settings in square brackets. If you use these, you must give the package its own separate \usepackage command, like geometry shown below:

<source lang="latex">
\documentclass[11pt,a4paper,oneside]{report}

\usepackage{pslatex,palatino,avant,graphicx,color}
\usepackage[margin=2cm]{geometry}

\begin{document}
\title{\color{red}Practical Typesetting}
\author{\color{blue}Name\\ Work}
\date{\color{green}December 2005}
\maketitle

\end{document}
</source>

Many packages can have additional formatting specifications in optional arguments in square brackets, in the same way as <tt>geometry</tt> does. Read the documentation for the package concerned to find out what can be done. You can pass several options together separated by a comma:

<source lang="latex">
\usepackage[option1,option2,option3]{''package_name''}
</source>

==Package documentation==

To find out what commands a package provides (and thus how to use it), you need to read the documentation. In the <tt>texmf/doc</tt> subdirectory of your installation there should be directories full of .dvi files, one for every package installed. This location is distribution-specific, but is ''typically'' found in:
{|class="wikitable"
!Distribution
!Path
|-
| MiKTeX || <tt>C:\Program Files\MiKTeX 2.7\doc\<font color="#744">latex</font></tt>
|-
| teTeX || <tt>/usr/share/texmf-tetex/doc/<font color="#744">latex</font></tt>
|}
Generally, ''most'' of the packages are in the <tt><font color="#744">latex</font></tt> subdirectory, although other packages (such as BibTeX and font packages) are found in other subdirectories in <tt>doc</tt>. The documentation directories have the same name of the package (e.g. <tt>amsmath</tt>), which generally have one or more relevant documents in a variety of formats (<tt>dvi</tt>, <tt>txt</tt>, <tt>pdf</tt>, etc.). The documents generally have the same name as the package, but there are exceptions (for example, the documentation for <tt>amsmath</tt> is found at <tt>latex/amsmath/amsdoc.dvi</tt>). If your installation procedure has not installed the documentation, the DVI files can all be downloaded from CTAN. Before using a package, you should read the documentation carefully, especially the subsection usually called "User Interface", which describes the commands the package makes available. You cannot just guess and hope it will work: you have to read it and find out.

==Packages list==

Here is a (not complete) list of useful packages that can be used for a wide range of different kind of documents. Each package has a short description next to it and, when available, there is a link to a section describing such package in detail. All of them (unless stated) should be included in your LaTeX distribution as ''package_name.sty''. For more information, refer to the documentation of the single packages, as described in the previous section.
The list is in alphabetical order.

{|class="wikitable"
|-
|'''amsmath'''|| it contains the advanced math extensions for LaTeX. The complete documentation should be in your LaTeX distribution; the file is called ''amsdoc'', and can be ''dvi'' or ''pdf''. For more information, see the chapter about [[LaTeX/Mathematics|Mathematics]]
|-
|'''amssymb'''||it adds new symbols in to be used in math mode.
|-
|'''amsthm'''||it introduces the <tt>proof</tt> environment and the <tt>theoremstyle</tt> command. For more information see the [[LaTeX/Theorems|Theorems]] section.
|-
|'''array'''||it extends the possibility of LaTeX to handle tables, fixing some bugs and adding new features. Using it, you can create very complicated and customized tables. For more information, see the [[LaTeX/Tables|Tables]] section.
|-
|'''babel'''|| it provides the internationalization of LaTeX. It has to be loaded in any document, and you have to give as an option the main language you are going to use in the document. For more information see the [[LaTeX/Internationalization|Internationalization]] section.
|-
|'''bm'''|| allows use of bold greek letters in math mode using the <tt>\bm{...}</tt> command. This supersedes the <tt>amsbsy</tt> package.
|-
|'''boxedminipage'''||it introduces the <tt>boxedminipage</tt> environment, that works exactly like <tt>minipage</tt> but adds a frame around it
|-
|'''caption'''||allows customization of appearance and placement of captions for figures, tables, etc.
|-
|'''cancel'''||provides commands for striking out mathematical expressions. The syntax is
<source lang="latex">
\cancel{x}
</source>
or
<source lang="latex">
\cancelto{0}{x}
</source>
|-
|'''changepage'''||to easily change the margins of pages. The syntax is
<source lang="latex">
\changepage{textheight}{textwidth}%
  {evensidemargin}{oddsidemargin}%
  {columnsep}{topmargin}%
  {headheight}{headsep}%
  {footskip}
</source>
All the arguments can be both positive and negative numbers; they will be added (keeping the sign) to the relative variable.
|-
|'''cite'''||assists in citation management
|-
|'''color'''||it adds support for colored text. For more information, see the [[LaTeX/Packages/Color|relevant section]]
|-
|'''easylist'''||adds support for arbitrarily-deep nested lists (useful for outlines)
|-
|'''esint'''||adds additional integral symbols, for integrals over squares, clockwise integrals over sets, etc.
|-
|'''eucal'''||other mathematical symbols
|-
|'''fancyhdr'''||to change header and footer of any page of the document. It is described in the [[LaTeX/Page Layout|Page Layout]] section
|-
|'''fontenc'''||to choose the font encoding of the output text. You might need it if you are writing documents in a language other than English. Check in the [[LaTeX/Internationalization|Internationalization]] section.
|-
|'''geometry'''||for easy management of document margins and the document page size
|-
|'''glossaries'''||for creation of glossaries and list of acronyms. For more information, see [[LaTeX/Glossary|relevant chapter]].
|-
|'''graphicx'''||to manage external pictures
|-
|'''hyperref'''||it gives LaTeX the possibility to manage links within the document or to any URL when you compile in PDF. For more information, see the [[LaTeX/Packages/Hyperref|relevant section]]
|-
|'''indentfirst'''|| once loaded, the beginning of any chapter/section is indented by the usual paragraph indentation.
|-
|'''inputenc'''||to choose the encoding of the input text. You might need it if you are writing documents in a language other than English. Check in the [[LaTeX/Internationalization|Internationalization]] section.
|-
|'''latexsym'''||other mathematical symbols
|-
|'''listings'''||to insert programming code within the document. Many languages are supported and the output can be customized. For more information, see the [[LaTeX/Packages/Listings|relevant section]]
|-
|'''mathrsfs'''||other mathematical symbols
|-
|'''natbib'''||gives additional citation options and styles
|-
|'''pdfpages'''||This package simplifies the insertion of external multi-page PDF or PS documents.
|-
|'''rotating'''||It lets you rotate any kind of object. It is particularly useful for rotating tables. For more information, see the [[LaTeX/Packages/Rotating|relevant section]]
|-
|'''setspace'''||Lets you change line spacing, e.g. provides the <tt>\doublespacing</tt> command for making double spaced documents. For more information, see the [[LaTeX/Customizing_LaTeX#Line_Spacing|relevant section]]
|-
|'''showkeys'''||it is very useful while writing any document. If you want to reference an image or a formula, you have to give it a name using <tt>\label{...}</tt> and then you can recall it using <tt>\ref{...}</tt>. When you compile the document these will be replaced only with numbers, and you can't know which label you had used unless you take a look at the source. If you have loaded the <tt>showkeys</tt> package, you will see the label just next or above the relevant number in the compiled version. An example of a reference to a section is [[Image:Latex_showkeys_example.png|250px]]. This way you can easily keep track of the labels you add or use, simply looking at the preview (both ''dvi'' or ''pdf''). Just before the final version, remove it
|-
|'''showidx''' 
|it prints out all index entries in the left margin of the text. This is quite useful for proofreading a document and verifying the index. For more information, see the [[LaTeX/Indexing|Indexing]] section.
|-
|'''subfiles'''
|the "root" and "child" document can be compiled at the same time without making changes to the "child" document. For more information, see the [[LaTeX/Multiple_files#Subfiles_package|Subfile package]] section.
|-
|'''subfig'''
|it allows to define multiple floats (figures, tables) within one environment giving individual captions and labels in the form 1a, 1b.
|-
|'''syntonly'''||if you add the following code in your preamble:
<source lang="latex">
\usepackage{syntonly}
\syntaxonly
</source>
LaTeX skims through your document only checking for proper syntax and usage of the commands, but doesn’t produce any (DVI or PDF) output. As LaTeX runs faster in this mode you may save yourself valuable time. If you want to get the output, you can simply comment out the second line.
|-
|'''textcomp'''|| provides extra symbols, e.g. arrows like <tt>\textrightarrow</tt>, various currencies (<tt>\texteuro</tt>,...), things like <tt>\textcelsius</tt> and many other
|-
|'''theorem'''|| you can change the style of newly defined theorems. For more information see the [[LaTeX/Theorems|Theorems]] section.
|-
|'''todonotes'''|| lets you insert notes of stuff to do with the syntax <tt>\todo{Add details.}</tt>
|-
|'''siunitx'''|| helps you typeset of SI-units correctly. For example <tt>\SI{12}{\mega\herz}</tt>. Automatically handles the correct spacing between the number and the unit. Note that even non-SI-units are set, like dB, rad, ...
|-
|'''ulem'''|| it allows to underline text (either with straight or wavy line). Few examples of usage are added to the [[LaTeX/Formatting|Formatting]] chapter.
|-
|'''url'''||it defines the <tt>\url{...}</tt> command. URLs often contain special character such as ''_'' and ''&'', in order to write them you should ''escape'' them inserting a backslash, but if you write them as an argument of <tt>\url{...}</tt>, you don't need to escape any special character and it will take care of proper formatting for you. If you are using the <tt>hyperref</tt>, you don't need to load <tt>url</tt> because it already provides the <tt>\url{...}</tt> command.
|-
|'''verbatim'''||it improves the <tt>verbatim</tt> environment, fixing some bugs. Moreover, it provides the <tt>comment</tt> environment, that lets you add multiple-line comments or comment out easily big parts of the code.

|-
|'''wrapfig'''|| to insert images surrounded by text. It was discussed in section [[LaTeX/Floats, Figures and Captions|Floats, Figures and Captions]]
|-
|'''xypic'''||is used to create trees, graphs, (commutative) diagrams, and similar things.
|}

==Creating packages==
See
* [[LaTeX/Customizing LaTeX#Creating your own package]]
* [[LaTeX/Advanced Topics#Creating your own package]]

== External resources ==
The best way to look for LaTeX packages is the already mentioned [http://tug.ctan.org/search.html CTAN: Search].
Additional resources form [http://www.ctan.org/tex-archive/help/Catalogue/catalogue.html The TeX Catalogue Online]:
* [http://www.ctan.org/tex-archive/help/Catalogue/alpha.html Alphabetic catalogue]
* [http://www.ctan.org/tex-archive/help/Catalogue/brief.html With brief descriptions]
* [http://www.ctan.org/tex-archive/help/Catalogue/bytopic.html Topical catalogue] with packages sorted systematically
* [http://www.ctan.org/tex-archive/help/Catalogue/hier.html Hierarchical] mirroring the CTAN folder hierarchy

<noinclude>
{{LaTeX/Bottom|Letters|Producing Mathematical Graphics}}
</noinclude>
