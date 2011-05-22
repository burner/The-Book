><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>
LaTeX enables typesetting of hyperlinks, useful when the resulting format is PDF, and the hyperlinks can be followed. It does so using the package ''hyperref''.

==Hyperref==

The package <tt>hyperref</tt> provides LaTeX the ability to create hyperlinks within the document. It works with ''pdflatex'' and also with standard "latex" used with dvips and ghostscript or dvipdfm to build a PDF file. If you load it, you will have the possibility to include interactive external links and all your internal references will be turned to hyperlinks. The compiler ''pdflatex'' makes it possible to create PDF files directly from the LaTeX source, and PDF supports more features than DVI. In particular PDF supports hyperlinks, and the only way to introduce them in LaTeX is using <tt>hyperref</tt>. Moreover, PDF can contain other information about a document such as the title, the author, etc., and you can edit those using this same package.

==Usage==

The basic usage with the standard settings is straightforward. Just load the package in the preamble, at the end of '''all''' the other packages but prior to other settings:

<source lang="latex">
\usepackage{hyperref}
</source>

This will automatically turn all your internal references into hyperlinks. It won't affect the way to write your documents: just keep on using the standard <source lang="latex" enclose="none">\label</source>/<source lang="latex" enclose="none">\ref</source> system; with <tt>hyperref</tt> those "connections" will become links and you will be able to click on them to be redirected to the right page. Moreover the table of contents, list of figures/tables and index will be made of hyperlinks, too.

The package provides three useful commands for inserting links pointing outside the document:
* <source lang="latex" enclose="none">\hyperref[label_name]{''link text''}</source>: this will have the same effect as <source lang="latex" enclose="none">\ref{label_name}</source> but will make the text ''link text'' a full link, instead. The two can be combined, for example in

<source lang="latex">we use \hyperref[mainlemma]{lemma \ref*{mainlemma}}</source>

Note the <tt>*</tt> after <source lang="latex" enclose="none">\ref</source> for avoiding nested hyperlinks.

If the lemma labelled as "mainlemma" was number 4.1.1, then the outputted text would be "we use lemma 4.1.1" with the hyperlink as expected.
* <source lang="latex" enclose="none">\url{''my_url''}</source>: it will show the URL using a mono-spaced font and, if you click on it, your browser will be opened pointing at it.
* <source lang="latex" enclose="none">\href{''my_url''}{''description''}</source>: it will show the string "description" using standard document font but, if you click on it, your browser will be opened pointing at "my_url". Here is an example:

<source lang="latex">
\url{http://www.wikibooks.org}
\href{http://www.wikibooks.org}{wikibooks home}
</source>

both point at the same page, but in the first case the URL will be shown, while in the second case the URL will be hidden. Note that, if you print your document, the link stored using <source lang="latex" enclose="none">\href</source> will not be shown anywhere in the document. You can use relative paths to link documents near the location of your current document; in order to do so, use the standard Unix-like notation (<tt>./</tt> is the current directory, <tt>../</tt> is the previous directory, etc.)

A possible way to insert emails is by

<source lang="latex">\href{mailto:my_address@wikibooks.org}{my_address@wikibooks.org}</source>

it just shows your email address (so people can know it even if the document is printed on paper) but, if the reader clicks on it, (s)he can easily send you an email. Or, to incorporate the <tt>url</tt> package's formatting and line breaking abilities into the displayed text, use<ref>{{cite web | title=Email link with hyperref, url packages | work=comp.text.tex User Group | url=http://groups.google.com/group/comp.text.tex/browse_thread/thread/ae160fd2fc5680a5/71a5a7c7bfceb3cb?lnk=gst&q=email+url+hyperref#71a5a7c7bfceb3cb | accessmonthday=January 22 | accessyear=2008}}</ref>

<source lang="latex">\href{mailto:my_address@wikibooks.org}{\nolinkurl{my_address@wikibooks.org}}</source>

When using this form, note that the <source lang="latex" enclose="none">\nolinkurl</source> command is fragile and if the hyperlink is inside of a moving argument, it must be preceeded by a <source lang="latex" enclose="none">\protect</source> command.

==Hyperlink and Hypertarget==

It is also possible to create an anchor anywhere in the document (with or without caption) and to link to it, with:

<source lang="latex">\hyperlink{label}{anchor caption}</source>

and

<source lang="latex">\hypertarget{label}{link caption}</source>

==Customization==

The standard settings should be fine for most users, but if you want to change something, you can easily do it. There are several variables you can change and there are two methods to pass those to the package. You can pass the options as an argument of the package when you load it (that's the standard way packages work), or you can use the <tt>\hypersetup</tt> command:

<source lang="latex">\hypersetup{''option1'' [, ...]}</source>

you can pass as many options as you want; separate them with a comma. Options have to be in the form:

<source lang="latex">variable_name=new_value</source>

exactly the same format has to be used if you pass those options to the package while loading it, like this:

<source lang="latex">\usepackage[''option1, option2'']{hyperref}</source>

Here is a list of the possible variables you can change (for the complete list, see the official documentation). The default values are written in an upright font:

{| class="wikitable"
!variable
!values
!comment
|-
|<tt>bookmarks</tt>
|<tt>=true'',false''</tt>
|show or hide the bookmarks bar when displaying the document
|-
|<tt>unicode</tt>
|<tt>=false'',true''</tt>
|allows to use characters of non-Latin based languages in Acrobat’s bookmarks
|-
|<tt>pdfborder</tt>
|<tt>={''RadiusH'' ''RadiusV'' ''Width'' [''Dash-Pattern'']}</tt>
|set the style of the border around a link. The first two parameters (RadiusH, RadiusV) have no effect in most pdf viewers. ''Width'' defines the thickness of the border. ''Dash-Pattern'' is a series of numbers separated by space and enclosed by box-brackets. It is an optional parameter to specify the length of each line & gap in the dash pattern. For example, {0 0 0.5 [3 3]} is supposed to draw a square box (no rounded corners) of width 0.5 and a dash pattern with a dash of length 3 followed by a gap of length 3. There is no uniformity in whether/how different pdf viewers render the dash pattern.
|-
|<tt>pdftoolbar</tt>
|<tt>=true'',false''</tt>
|show or hide Acrobat’s toolbar
|-
|<tt>pdfmenubar</tt>
|<tt>=true'',false''</tt>
|show or hide Acrobat’s menu
|-
|<tt>pdffitwindow</tt>
|<tt>=true'',false''</tt>
|resize document window to fit document size
|-
|<tt>pdfstartview </tt>
|<tt>={FitH},''{FitV}'',etc<ref>Other possible values are defined in the [http://mirror.switch.ch/ftp/mirror/tex/macros/latex/contrib/hyperref/doc/manual.html#TBL-7-40-1 hyperref manual]</ref>.</tt>
|fit the width of the page to the window
|-
|<tt>pdftitle</tt>
|<tt>={text}</tt>
|define the title that gets displayed in the "Document Info" window of Acrobat
|-
|<tt>pdfauthor</tt>
|<tt>={text}</tt>
|the name of the PDF’s author, it works like the one above
|-
|<tt>pdfsubject</tt>
|<tt>={text}</tt>
|subject of the document, it works like the one above
|-
|<tt>pdfcreator</tt>
|<tt>={text}</tt>
|creator of the document, it works like the one above
|-
|<tt>pdfproducer</tt>
|<tt>={text}</tt>
|producer of the document, it works like the one above
|-
|<tt>pdfkeywords</tt>
|<tt>={text}</tt>
|list of keywords, separated by brackets, example below
|-
|<tt>pdfnewwindow</tt>
|<tt>(=true'',false)''</tt>
|define if a new window should get opened when a link leads out of the current document
|-
|<tt>pagebackref</tt>
|<tt>(=false'',true)''</tt>
|activate back references inside bibliography. Must be specified as part of the ''\usepackage{}'' statement.
|-
|<tt>colorlinks</tt>
|<tt>(=false'',true)''</tt>
|surround the links by color frames (<tt>false</tt>) or colors the text of the links (<tt>true</tt>). The color of these links can be configured using the following options (default colors are shown):
|-
|<tt>linkcolor</tt>
|<tt>=red</tt>
|color of internal links (sections, pages, etc.)
|-
|<tt>linktoc</tt>
|<tt>=''none'',section,''page'',''all''</tt>
|defines which part of an entry in the table of contents is made into a link
|-
|<tt>citecolor</tt>
|<tt>=green</tt>
|color of citation links (bibliography)
|-
|<tt>filecolor</tt>
|<tt>=magenta</tt>
|color of file links
|-
|<tt>urlcolor</tt>
|<tt>=cyan</tt>
|color of URL links (mail, web)
|-
|<tt>linkbordercolor</tt>
|<tt>={1 0 0}</tt>
|color of frame around internal links (if <tt>colorlinks=false</tt>)
|-
|<tt>citebordercolor</tt>
|<tt>={0 1 0}</tt>
|color of frame around citations
|-
|<tt>urlbordercolor</tt>
|<tt>={0 1 1}</tt>
|color of frame around URL links
|}

Please note, that explicite RGB specification is only allowed for the border colors (like linkbordercolor etc.), while the others may only assigned to named colors (which you can define your own, see [[LaTeX/Colors|Colors]]). In order to speed up your customization process, here is a list with the variables with their default value. Copy it in your document and make the changes you want. Next to the variables, there is a short explanations of their meaning:

<source lang="latex">
\hypersetup{
    bookmarks=true,         % show bookmarks bar?
    unicode=false,          % non-Latin characters in Acrobat’s bookmarks
    pdftoolbar=true,        % show Acrobat’s toolbar?
    pdfmenubar=true,        % show Acrobat’s menu?
    pdffitwindow=false,     % window fit to page when opened
    pdfstartview={FitH},    % fits the width of the page to the window
    pdftitle={My title},    % title
    pdfauthor={Author},     % author
    pdfsubject={Subject},   % subject of the document
    pdfcreator={Creator},   % creator of the document
    pdfproducer={Producer}, % producer of the document
    pdfkeywords={keyword1} {key2} {key3}, % list of keywords
    pdfnewwindow=true,      % links in new window
    colorlinks=false,       % false: boxed links; true: colored links
    linkcolor=red,          % color of internal links
    citecolor=green,        % color of links to bibliography
    filecolor=magenta,      % color of file links
    urlcolor=cyan           % color of external links
}
</source>

If you don't need such a high customization, here are some smaller but useful examples. When creating PDFs destined for printing, colored links are not a good thing as they end up in gray in the final output, making it difficult to read. You can use color frames, which are not printed:
<source lang="latex">
 \usepackage{hyperref}
 \hypersetup{colorlinks=false}
</source>
or make links black:
<source lang="latex">
\usepackage{hyperref}
\hypersetup{
    colorlinks,%
    citecolor=black,%
    filecolor=black,%
    linkcolor=black,%
    urlcolor=black
}
</source>
When you just want to provide information for the Document Info section of the PDF file, as well as enabling back references inside bibliography:
<source lang="latex">
\usepackage[pdfauthor={Author's name},%
pdftitle={Document Title},%
pagebackref=true,%
pdftex]{hyperref}
</source>

By default, URLs are printed using mono-spaced fonts. If you don't like it and you want them to be printed with the same style of the rest of the text, you can use this:
<source lang="latex">
 \urlstyle{same}
</source>

==Problems with Links and Equations==
Messages like the following
 ! pdfTeX warning (ext4): destination with the same identifier (name{
 equation.1.7.7.30}) has been already used, duplicate ignored

appear, when you have made something like

<source lang="latex">
\begin{eqnarray}a=b\nonumber\end{eqnarray}
</source>

The error disappears, if you use instead this form:

<source lang="latex">
\begin{eqnarray*}a=b\end{eqnarray*}
</source>

Beware that the shown line number is often completely different from the erroneous line.

==Problems with Links and Pages==

Messages like the following:
 ! pdfTeX warning (ext4): destination with the same
 identifier (name{page.1}) has been already used,
 duplicate ignored

appear when a counter gets reinitialized, for example by using the command <source lang="latex" enclose="none">\mainmatter</source> provided by the book document class. It resets the page number counter to 1 prior to the first chapter of the book. But as the preface of the book also has a page number 1 all links to "page 1" would not be unique anymore, hence the notice that "duplicate has been ignored." The counter measure consists of putting <tt>plainpages=false</tt> into the <tt>hyperref</tt> options. This unfortunately only helps with the page counter. An even more radical solution is to use the option <tt>hypertexnames=false</tt>, but this will cause the page links in the index to stop working.

The best solution is to give each page a unique name by using the <source lang="latex" enclose="none">\pagenumbering</source> command:

<source lang="latex">
\pagenumbering{alph}    % a, b, c, ...
... titlepage, other front matter ...
\pagenumbering{roman}   % i, ii, iii, iv, ...
... table of contents, table of figures, ...
\pagenumbering{arabic}  % 1, 2, 3, 4, ...
... beginning of the main matter (chapter 1) ...
</source>

Another solution is to use <source lang="latex" enclose="none">\pagenumbering{alph}</source> before the command <source lang="latex" enclose="none">\maketitle</source>, which will give the title page the label page.a. Since the page number is suppressed, it won't make a difference to the output.

By changing the page numbering every time before the counter is reset, each page gets a unique name. In this case, the pages would be numbered a, b, c, i, ii, iii, iv, v, 1, 2, 3, 4, 5, ...

If you don't want the page numbers to be visible (for example, during the front matter part), use <source lang="latex" enclose="none">\pagestyle{empty} ... \pagestyle{plain}</source>. The important point is that although the numbers are not visible, each page will have a unique name.

Another more flexible approach is to set the counter to something negative:

<source lang="latex">
\setcounter{page}{-100}
... titlepage, other front matter ...
\pagenumbering{roman}   % i, ii, iii, iv, ...
... table of contents, table of figures, ...
\pagenumbering{arabic}  % 1, 2, 3, 4, ...
... beginning of the main matter (chapter 1) ...
</source>

which will give the first pages a unique negative number.

The problem can also occur with the <tt>algorithms</tt> package: because each algorithm uses the same line-numbering scheme, the line identifiers for the second and follow-on algorithms will be duplicates of the first.<!-- And the solution is...? -->

The problem occurs with equation identifiers if you use <source lang="latex" enclose="none">\nonumber</source> on every line of an eqnarray environment.  In this case, use the *'ed form instead, e.g. <source lang="latex" enclose="none" enclose="none">\begin{eqnarray*} ... \end{eqnarray*}</source> (which is an unnumbered equation array), and remove the now unnecessary <source lang="latex" enclose="none">\nonumber</source> commands.

If your url's are too long and running off of the page, try using the breakurl package to split the url over multiple lines.  This is especially importiant in a multicolumn environment where the line with is greatly shortened.

==Problems with Bookmarks==
The text displayed by bookmarks does not always look like you expect it
to look. Because bookmarks are "just text", much fewer characters are
available for bookmarks than for normal LaTeX text. Hyperref will normally
notice such problems and put up a warning:

 Package hyperref Warning:
 Token not allowed in a PDFDocEncoded string:

You can now work around this problem by providing a text string for the bookmarks, which replaces the offending text:

<source lang="latex">
\texorpdfstring{''TEX text''}{''Bookmark Text''}
</source>

Math expressions are a prime candidate for this kind of problem:

<source lang="latex">
\section{\texorpdfstring{$E=mc^2$}{E=mc2}}
</source>

which turns <source lang="latex" enclose="none">\section{$E=mc^2$}</source> to <tt>E=mc2</tt> in the bookmark area. Color changes also do not travel well into bookmarks:

<source lang="latex">
\section{\textcolor{red}{Red !}}
</source>

produces the string "redRed!". The command <source lang="latex" enclose="none">\textcolor</source> gets ignored but its argument (red) gets printed.
If you use:

<source lang="latex">
\section{\texorpdfstring{\textcolor{red}{Red !}}{Red\ !}}
</source>

the result will be much more legible.

If you write your document in unicode and use the <tt>unicode</tt> option for the <tt>hyperref</tt> package you can use unicode characters in bookmarks. This will give you a much larger selection of characters to pick from when using <source lang="latex" enclose="none">\texorpdfstring</source>.

==Problems with tables and figures==

The links created by <code>hyperref</code> point to the label created within the float environment, which, as [[LaTeX/Floats, Figures and Captions#Labels and Cross-referencing|previously described]], must always be set after the caption. Since the caption is usually below a figure or table, the figure or table itself will not be visible upon clicking the link<ref>http://www.ctan.org/tex-archive/macros/latex/contrib/hyperref/README</ref>. A workaround exists by using the package <code>hypcap</code> [http://www.ctan.org/tex-archive/macros/latex/contrib/oberdiek/hypcap.pdf] with:
<source lang="latex">
\usepackage[all]{hypcap}
</source>
Be sure to call this package ''after'' loading <code>hyperref</code>, which should otherwise be loaded last.

If you use the <code>wrapfig</code> package mentioned in the "[[LaTeX/Floats, Figures and Captions#Wrapping text around figures|Wrapping text around figures]]" section of the "Floats, Figures and Captions" chapter, or other similar packages that define their own environments, you will need to manually include <source lang="latex" enclose="none">\capstart</source> in those environments, e.g.:
<source lang="latex">
\begin{wrapfigure}{R}{0.5\textwidth}
  \capstart
  \begin{center}
    \includegraphics[width=0.48\textwidth]{filename}
  \end{center}  
  \caption{\label{labelname}a figure}
\end{wrapfigure}
</source>

==Problems with long caption and \listoffigures or long title ==
There is an issue when using <source lang="latex" enclose="none">\listoffigures</source> with <code>hyperref</code> for long captions or long titles. This happens when the captions (or the titles) are longer than the page width (about 7-9 words depending on your settings). To fix this, you need to use the option breaklinks when first declaring:
<source lang="latex">
\usepackage[breaklinks]{hyperref}
</source>

This will then cause the links in the <source lang="latex" enclose="none">\listoffigures</source> to word wrap properly.

==Problems with already existing .toc, .lof and similar files ==
The format of some of the auxilliary files generated by latex changes when you include the <code>hyperref</code> package. One can therefore encounter errors like <code>! Argument of \Hy@setref@link has an extra }.</code> when the document is typeset with <code>hyperref</code> for the first time and these files already exist. The solution to the problem is to delete all the files that latex uses to get references right and typeset again.

==References==
<references/>

<noinclude>
{{LaTeX/Bottom|Presentations|Colors}}
</noinclude>
