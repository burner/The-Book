><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

Latex and the document class will normally take care of page layout issues for you. For submission to an academic publication, this entire topic will be out of your hands, as the publishers want to control the presentation. However, for your own documents, there are some obvious settings that you may wish to change: margins, page orientation and columns, to name but three. The purpose of this tutorial is to show you how to configure your pages.

== Page dimensions ==

A page in Latex is defined by a myriad of internal parameters. Each parameter corresponds to the length of an element of the page, for example, <code>\paperheight</code> is the physical height of the page. Here you can see a diagram showing all the variables defining the page:

[[Image:Latex_layout.svg|500px]]
{{col-begin}}
{{col-2}}
# one inch + <tt>\hoffset</tt>
# one inch + <tt>\voffset</tt>
# <tt>\oddsidemargin</tt> = 31pt
# <tt>\topmargin</tt> = 20pt
# <tt>\headheight</tt> = 12pt
# <tt>\headsep</tt> = 25pt
# <tt>\textheight</tt> = 592pt
# <tt>\textwidth</tt> = 390pt
{{col-break}}
<ol start="9">
<li> <tt>\marginparsep</tt> = 10pt</li>
<li> <tt>\marginparwidth</tt> = 35pt</li>
<li> <tt>\footskip</tt> = 30pt</li>
</ol>
* <tt>\marginparpush</tt> = 7pt (not shown)
* <tt>\hoffset</tt> = 0pt
* <tt>\voffset</tt> = 0pt
* <tt>\paperwidth</tt> = 597pt
* <tt>\paperheight</tt> = 845pt
{{col-end}}

It will not have been immediately obvious - because it doesn't really cause any serious problems - that the default page size for all standard document classes is ''US letter''. This is shorter by 18 mm (about 3/4 inch), and slightly wider by 8 mm (about 1/4 inch), compared to A4 (which is the standard in almost all the rest of the world). As I said, it's not a great problem, and most printers will print the page without a hiccup. However, it is possible to specify alternative sizes.

<source lang="latex">
\documentclass[a4paper]{article}
</source>

The above example illustrates how to pass the optional argument to the <tt>\documentclass</tt>, which will then modify the page dimensions accordingly. The standard document classes that are a part of Latex are built to be fairly generic, which is why you have the flexibility of specifying the page size. Other classes may have different options (or none at all). Normally, 3rd party classes come with some documentation to let you know.

Readers from a word processing background are probably thinking why there is so much white space surrounding the text. There is a good reason, and it's all down to readability. Have a look in a few books, and pick a few lines at random. Count the number of characters per line. I bet the average is about 66. Studies have shown that it's easier to read text when there are 60-70 characters per line - and it would seem that 66 is the optimal number. Therefore, the page margins are set to ensure that readability remains as good as possible. Also, white space is often left in the inner margin for the assumption that the document will be bound.

If you wish to change the margins of your document, there are many ways to do so:

* Simply use the <code>fullpage</code> package for somewhat standardized smaller margins (around an inch):

<source lang="latex">
\usepackage{fullpage}
</source>

For an even greater effect give it the <code>cm</code> option (around 1.5cm):

<source lang="latex">
\usepackage[cm]{fullpage}
</source>

* Use the <code>a4wide</code> package for a page with A4 document size with smaller margins.

* Use the <code>geometry</code> package.  This package allows you to specify the 4 margins without needing to remember the particular page dimensions commands.  You can enter the measures in centimeters and inches as well. Use <code>cm<code> for centimeters and <code>in<code> for inches after each value (e.g: 1.0in or 2.54cm). These values are relative to the edge of paper (0in) and go inward it. It may be implemented as follows:

<source lang="latex">
\usepackage[top=tlength, bottom=blength, left=llength, right=rlength]{geometry}
</source>

* Edit individual page dimension variables described above, using the <code>\addtolength</code> and <code>\setlength</code> commands.  For instance, 

<source lang="latex">
\oddsidemargin=-1cm
\setlength{\textwidth}{6.5in}
\addtolength{\voffset}{-5pt}
</source>

Additionally, there are several packages designed to solve the problem of varying pages sizes, which override any defaults setup by the document class.  One of the most versatile packages for page layout is the <code>geometry</code> package. For instance, to set the page size, add the following to your preamble:

<source lang="latex">
\usepackage[a4paper]{geometry}
</source>

The <code>geometry</code> package has many pre-defined page sizes, like <code>a4paper</code>, built in.  Others include:
<code>a0paper</code>, <code>a1paper</code>, ..., <code>a6paper</code>, <code>b0paper</code>, <code>b1paper</code>, ..., <code>b6paper</code>, <code>letterpaper</code>, <code>legalpaper</code>, <code>executivepaper</code>.

To explicitly change the paper dimensions using the <code>geometry</code> package, the paperwidth and paperheight options can be used. For example:

<source lang="latex">
\usepackage[margin=1in, paperwidth=5.5in, paperheight=8.5in]{geometry}
</source>

=== Top margin above Chapter ===
The top margin above a chapter can be changed using the <code>titlesec</code> package. Example: [http://www.ctex.org/documents/packages/layout/titlesec.pdf]

<source lang="latex">
\usepackage{titlesec}
\titlespacing*{\chapter}{0pt}{-50pt}{20pt}
\titleformat{\chapter}[display]{\normalfont\huge\bfseries}{\chaptertitlename\ \thechapter}{20pt}{\Huge}
</source>

The command <code>\titleformat</code> must be used when the spacing of a chapter is changed. In case of a section this command can be omitted.

=== Page size issues ===

If you intend to get a pdf in the end, there are basically three ways:
<source lang="bash">
TeX => PDF
TeX => DVI => PDF
TeX => DVI => PS => PDF
</source>

Which are in general obtained with
<source lang="bash">
pdflatex myfile               # TeX => PDF
latex myfile                  # TeX => DVI
dvipdf myfile                 # DVI => PDF
dvips myfile -o myfile.ps     # DVI => PS
ps2pdf myfile.ps myfile.pdf   # PS => PDF
</source>

With all the available Ghostscript versions, the safest way to always get the right paper size in the end is to add
<source lang="latex">
\documentclass[...,a4paper,...]{...}
\special{papersize=210mm,297mm}
</source>
to the tex file, <code> -t a4 </code> after dvips and <code> -sPAPERSIZE=a4 </code> after the ps2pdf.
For pdflatex to work fine, using the package <code> geometry </code> usually works.

If you want US Letter instead, replace "210mm,297mm" by "8.5in,11in" and "a4" by "letter".

== Page orientation ==

When you talk about changing page orientation, it usually means changing to landscape mode, since portrait is the default. I shall introduce two slightly different styles of changing orientation.

The first is for when you want all of your document to be in landscape from the very beginning. There are various packages available to achieve this, but the one I prefer is the <code>geometry</code> package. All you need to do is call the package, with ''landscape'' as an option:

<source lang="latex">
\usepackage[landscape]{geometry}
</source>

Although, if you intend to use <code>geometry</code> to set your paper size, don't add the <code>\usepackage</code> commands twice, simply string all the options together, separating with a comma:

<source lang="latex">
\usepackage[a4paper,landscape]{geometry}
</source>

The second method is for when you are writing a document in portrait, but you have some contents, like a large diagram or table that would be displayed better on a landscape page. However, you still want the consistency of your headers and footers appearing the same place as the other pages.

The <code>lscape</code> package is for this very purpose. It supplies a <code>landscape</code> environment, and anything inside is basically rotated. No actual page dimensions are changed. This approach is more applicable to books or reports than to typical academic publications.  Using <code>pdflscape</code> instead of <code>lscape</code> when generating a PDF document will make the page appear right side up when viewed: the single page that is in landscape format will be rotated, while the rest will be left in portrait orientation.

Also, to get a table to appear correctly on a landscaped page, one must place the <code>tabular</code> environment inside a <code>table</code> environment, which is itself inside the <code>landscape</code> environment.  e.g., it should look like this:

<source lang="latex">
\begin{landscape}
\begin{table}
\centering     % optional, probably makes it look better to have it centered on the page
\begin{tabular}{....}
.......
\end{tabular}
\end{table}
\end{landscape}
</source>

== Page styles ==

Page styles in Latex terms refers not to page dimensions, but to the running headers and footers of a document. These headers typically contain document titles, chapter or section numbers/names, and page numbers.

=== Standard page styles ===

The possibilities of changing the headers in plain Latex are actually quite limited. There are two commands available: <code>\pagestyle{''style''}</code> will apply the specified style to the current and all subsequent pages, and <code>\thispagestyle{''style''}</code> will only affect the current page. The possible styles are:

{|
| '''empty'''
| Both header and footer are clear
|-
| '''plain'''
| Header is clear, but the footer contains the page number in the center.
|-
| '''headings'''
| Footer is blank, header displays information according to document class (e.g., section name) and page number top right.
|-
| '''myheadings'''
| Page number is top right, and it is possible to control the rest of the header.
|}

With myheadings, the commands <code>\markright</code> (in the standard document classes, book, report and article) and <code>\markboth</code> (only in the book class) are used to control the headings. The following commands placed at the beginning of an article document will set the header of all pages to contain "John Smith" top left, "On page styles" centered and the page number top right:

<source lang="latex">
\pagestyle{myheadings}
\markright{John Smith\hfill On page styles\hfill}
</source>

An issue to look out for is that the major sectioning commands (<code>\part</code>, <code>\chapter</code> or <code>\maketitle</code>) specify a <code>\thispagestyle{plain}</code>. So, if you wish to suppress all styles by inserting a <code>\pagestyle{empty}</code> at the beginning of your document, then the style command at each section will override your initial rule, for those pages only. To achieve the intended result one can follow the new section commands with <code>\thispagestyle{empty}</code>. The <code>\part</code> command, however, cannot be fixed this way, because it sets the page style, but also advances to the next page, so that \thispagestyle{} cannot be applied to that page. Another approach is to simply write <code>\usepackage{nopageno}</code> in the preamble. This package will make <code>\pagestyle{plain}</code> have the same effect as <code>\pagestyle{empty}</code>, effectively suppressing page numbering when it it used.

=== Customising with <tt>fancyhdr</tt> ===

To get better control over the headers, one can use the package <code>fancyhdr</code> written by
Piet van Oostrum. It provides several commands that allow you to customize the header and footer lines of
your document. For a more complete guide, the author of the package produced this [http://www.ctan.org/tex-archive/macros/latex/contrib/fancyhdr/fancyhdr.pdf documentation].

The tricky problem when customizing headers and footers is to get things like running section and chapter names in there. LaTeX accomplishes this with a two-stage approach. In the header and footer definition, you use the commands <code>\rightmark</code> and <code>\leftmark</code> to represent the current section and chapter heading, respectively. The values of these two commands are overwritten whenever a chapter or section command is processed. For ultimate flexibility, the <code>\chapter</code> command and its friends do not redefine <code>\rightmark</code> and <code>\leftmark</code> themselves. They call yet another command (<code>\chaptermark</code>, <code>\sectionmark</code>, or <code>\subsectionmark</code>) that is responsible for redefining <code>\rightmark</code> and <code>\leftmark</code>.

If you want to change the look of the chapter name in the header line, you need only "renew" the <code>\chaptermark</code> command.

To begin, add the following lines to your preamble:

<source lang="latex">
\usepackage{fancyhdr}
\setlength{\headheight}{15.2pt}
\pagestyle{fancy}
</source>

The second line will prevent LaTeX from giving a warning. Both the header and footer comprise three elements each according to its horizontal position (left, centre or right). To set their values, the following commands are available:

{|
| <tt>\lhead[</tt>''lh-even''<tt>]{</tt>''lh-odd''<tt>}</tt>
| <tt>\lfoot[</tt>''lf-even''<tt>]{</tt>''lf-odd''<tt>}</tt>
|-
| <tt>\chead[</tt>''ch-even''<tt>]{</tt>''ch-odd''<tt>}</tt>
| <tt>\cfoot[</tt>''cf-even''<tt>]{</tt>''cf-odd''<tt>}</tt>
|-
| <tt>\rhead[</tt>''rh-even''<tt>]{</tt>''rh-odd''<tt>}</tt>
| <tt>\rfoot[</tt>''rf-even''<tt>]{</tt>''rf-odd''<tt>}</tt>
|}

Hopefully, the behaviour of the above commands is fairly intuitive: if it has ''head'' in it, it affects the head etc, and obviously, ''l'', ''c'' and ''r'' means left, centre and right respectively. Documents can be either one- or two-sided. Articles are by default one-sided, books are two-sided. Two-sided documents differentiate the left (even) and right (odd) pages, whereas one-sided do not. 

Watch out: if you give long text in two different "parts" only in the footer or only in the header, you might see overlapping text, be careful. There are special commands you can use as arguments:

{|
|-
|<tt>\thepage</tt> || number of the current page
|-
|<tt>\leftmark</tt> || current chapter name printed like "CHAPTER 3. THIS IS THE CHAPTER TITLE"
|-
| <tt>\rightmark</tt> || current section name printed like "1.6. THIS IS THE SECTION TITLE"
|-
|<tt>\chaptername</tt> || the name ''chapter'' in the current language. If this is English, it will display "Chapter"
|-
|<tt>\thechapter</tt> || current chapter number
|-
|<tt>\thesection</tt> || current section number
|-
|}

Note that <code>\leftmark</code> and <code>\rightmark</code> convert the names to uppercase, whichever was the formatting of the text. If you want them to print the actual name of the chapter without converting it to uppercase use the following command:
<source lang="latex">
\renewcommand{\chaptermark}[1]{\markboth{#1}{}}
\renewcommand{\sectionmark}[1]{\markright{#1}{}}
</source>
now <code>\leftmark</code> and <code>\rightmark</code> will just print the name of the chapter and section, without number and without affecting the formatting. Note that these redefinitions must be inserted ''after'' the first call of <code>\pagestyle{fancy}</code>.  The standard book formatting of the <code>\chaptermark</code> is:
<source lang="latex">
\renewcommand{\chaptermark}[1]{\markboth{\MakeUppercase{\chaptername\ \thechapter.\ #1}}{}}
</source>
Moreover, with the following commands you can define the thickness of the decorative lines on both the header and the footer:
<source lang="latex">
\renewcommand{\headrulewidth}{0.5pt}
\renewcommand{\footrulewidth}{0pt}
</source>
The first line for the header, the second for the footer. Setting it to zero means that there will be no line.

An example:

<source lang="latex">
\fancyhf{}

\lhead{Andrew Roberts}
\rhead{\today}
\rfoot{\thepage}
</source>

It is often necessary to clear any defaults or a previous style definition, and the first line of the above example will do this. The commands are an alternative interface to customising the headers/footers that fancyhdr offers, and so by not passing anything to them, it assumes that you want it all blank.

The result of these commands will put my name at the top left, todays date at the top right, and the current page number at the bottom right of the page. Even if the document class was two-sided, because no optional text has been supplied for the even pages, the style will be used for all pages. 

This approach has a serious bad point: some pages like the title or the beginning of each chapter have no header or footer, but with the code we have shown ''every'' page will get the same output. There is a way to solve this problem: you can use the ''fancyplain'' style. If you do so, you can use the command <code>\fancyplain{...}{...}</code> inside <code>\lhead{...}</code> etc.

When LaTeX wants to create a page with an empty style, it will insert the first argument of <code>fancyplain</code>, in all the other cases it will use the second argument. So, an improved version of the previous code would be:
<source lang="latex">
\pagestyle{fancyplain}

\fancyhf{}

\lhead{\fancyplain{}{Andrew Roberts}}
\rhead{\fancyplain{}{\today}}
\rfoot{\fancyplain{}{\thepage}}
</source>

It has the same behavior of the previous code, but you will get empty header and footer in the title and at the beginning of chapters.

For two-sided, it's common to mirror the style of opposite pages, you tend to think in terms of ''inner'' and ''outer''. So, the same example as above for two-sided is:

<source lang="latex">
\lhead[Andrew Roberts]{}
\rhead[]{Andrew Roberts}
\lhead[]{\today}
\rhead[\today]{}
\lfoot[\thepage]{}
\rfoot[]{\thepage}
</source>

This is effectively saying my name is top outer, todays date is top inner, and current page number is bottom outer. You can use the <code>fancyplain</code> command within them for two-sided documents, too.

As an example, here is the complete code of a basic configuration you could use for a real document:
<source lang="latex">
\usepackage{fancyhdr}
\setlength{\headheight}{15pt}

\pagestyle{fancyplain}
\renewcommand{\chaptermark}[1]{\markboth{#1}{}}

\lhead{\fancyplain{}{\thepage}}
\chead{}
\rhead{\fancyplain{}{\textit{\leftmark}}}
\lfoot{}
\cfoot{}
\rfoot{}
</source>

'' NB. If you want to make the '''article class two-sided''', use '' '''<code>\documentclass[twoside]{article}</code>'''.

=== Another approach with <tt>fancyhdr</tt> ===

If you want to get different style for even and odd pages, there is another possible way, still using <code>fancyhdr</code>. Start again with:
<source lang="latex">
\fancyhf{}
</source>
it will just delete the current heading/footer configuration, so you can make your own. Now you can create what you want using the command <code>\fancyhead</code> for header and <tt>\fancyfoot</tt> for footer. They work in the same way, so we'll explain only the first one. The syntax is:
<source lang="latex">
\fancyhead[selectors]{output you want}
</source>

The selectors are the following:
{|
|-
| '''E''' || even page
|-
| '''O''' || odd page
|-
| '''L''' || left side
|-
| '''C''' || centered
|-
| '''R''' || right side
|-
|}
so '''CE''' will refer to the center of the even pages and '''RO''' to the right side of the odd pages. Whether it is header or footer, depends if you are using <code>\fancyhead</code> or <code>\fancyfoot</code>. You can use multiple selectors separated by a comma. Here is an example:
<source lang="latex">
\fancyhead[CE]{Author's Name}
\fancyhead[CO]{\today}
\fancyfoot[LE,RO]{\thepage}
</source>
it will print author's name on the center of the header of the even pages, the date of the current day on the center of the odd pages and the current page number on the left side of even pages ''and'' on the right size of the odd pages.
Finally, in order to have the pages at the beginning of any chapter really plain, you could redefine the ''plain'' style, for example to have a really plain page when you want. The command to use is <code>\fancypagestyle{plain}{...}</code> and the argument can contain all the commands explained before. An example is the following:
<source lang="latex">
\fancypagestyle{plain}{ %
\fancyhf{} % remove everything
\renewcommand{\headrulewidth}{0pt} % remove lines as well
\renewcommand{\footrulewidth}{0pt}}
</source>

Finally, here is the complete code of a possible style you could use for a two-sided document:
<source lang="latex">
\usepackage{fancyhdr}
\setlength{\headheight}{15pt}

\pagestyle{fancy}
\renewcommand{\chaptermark}[1]{\markboth{#1}{}}
\renewcommand{\sectionmark}[1]{\markright{#1}{}}

\fancyhf{}
\fancyhead[LE,RO]{\thepage}
\fancyhead[RE]{\textit{\nouppercase{\leftmark}}}
\fancyhead[LO]{\textit{\nouppercase{\rightmark}}}

\fancypagestyle{plain}{ %
\fancyhf{} % remove everything
\renewcommand{\headrulewidth}{0pt} % remove lines as well
\renewcommand{\footrulewidth}{0pt}}
</source>

=== Page ''n'' of ''m'' ===

Some people like to put the current page number in context with the whole document. LaTeX only provides access to the current page number.  However, you can use the <code>lastpage</code> package to find the total number of pages, like this:

<source lang="latex">
\usepackage{lastpage}
...
\cfoot{\thepage\ of \pageref{LastPage}}
</source>

''Note the capital letters''. Also, add a backslash after <code>\thepage</code> to ensure adequate space between the page number and 'of'. And recall, when using references, that you have to run LaTeX an extra time to resolve the cross-references.

== Multi-column pages ==

It is common to see articles and conference proceedings formatted with two columns of text. However, such publishers will usually provide you with their own document class, which automatically implements this format, without you having to do anything. It is very easy to format your page in this way. If you are using a standard Latex document class, then you can simply pass the optional argument ''twocolumn'' to the document class: <code>\documentclass[twocolumn]{article}</code> which will give the desired effect.

While this simple addition will do the job 9 out of 10 times, it is widely acknowledged that there are many limitations of this approach, and that the <code>multicol</code> package is much more useful for handling multiple columns. It has several advantages:

* Can support up to ten columns.
* Implements a ''multicols'' environment, therefore, it is possible to mix the number of columns within a document.
* Additionally, the environment can be nested inside other environments, such as <code>figure</code>.
* <code>Multicol</code> outputs ''balanced'' columns, whereby the columns on the final page will be of roughly equal length.
* Vertical rules between columns can be customised.
* Column environments can be easily customised locally or globally.

Floats are not fully supported by this environment. It can only cope if you use the starred forms of the float commands (e.g., <code>\begin{figure*}</code> ) which makes the float span all columns. This is not hugely problematic, since floats of the same width as a column may be too small, and you would probably want to span them anyway.

To create a typical two-column layout:

<source lang="latex">
\begin{multicols}{2}
  lots of text
\end{multicols}
</source>

The parameter <code>\columnseprule</code> holds the width of the vertical rules. By default, the lines are omitted as this parameter is set to a length of 0pt. Do the following before the beginning of the environment:

<source lang="latex">
\setlength{\columnseprule}{1pt}
</source>

This will draw a thin line of 1pt in width. A thick line would not look very pleasing, however, you are free to put in any length of your choosing. Also, to change the horizontal space in between columns (the default is set at 10pt, which is quite narrow) then you need to change the <code>\columnsep</code> parameter:

<source lang="latex">
\setlength{\columnsep}{20pt}
</source>

== Manual page formatting ==

There may be instances, especially in very long documents, such as books, that Latex will not get all page breaks looking as good as it could. It may, therefore, be necessary to manually tweak the page formatting. Of course, you should only do this at the very final stage of producing your document, once all the content is complete. Latex offers the following:

{| class="wikitable"
| <tt>\newline</tt>
| Breaks the line at the point of the command.
|-
| <tt>\\</tt>
| Breaks the line at the point of the command, it's a shorter version of the previous command but it does exactly the same
|-
| <tt>\\*</tt>
| Breaks the line at the point of the command and additionally prohibits a page break after the forced line break
|-
| <tt>\linebreak[number]</tt>
| Breaks the line at the point of the command. The ''number'' you provide as an argument represents the priority of the command in a range from 0 (it will be easily ignored) to 4 (do it anyway). LaTeX will try to produce the best line breaks possible, meeting its high standards. If it cannot, it will decide whether including the linebreak or not according to the priority you have provided.
|-
| <tt>\newpage</tt>
| Ends the current page and starts a new one.
|-
| <tt>\pagebreak[number]</tt>
| Breaks the current page at the point of the command. The optional ''number'' argument sets the priority in a scale from 0 to 4.
|-
| <tt>\nopagebreak[number]</tt>
| Stops the page being broken at the point of the command. The optional ''number'' argument sets the priority in a scale from 0 to 4.
|-
| <tt>\clearpage</tt>
| Ends the current page and causes any floats encountered in the input, but yet to appear, to be printed.
|}

== Widows and orphans ==
{{wikipedia|Widows and orphans}}
In professional books, it's not desirable to have single lines at the beginning or end of a page. In typesetting such situations are called 'widows' and 'orphans'. Normally it is possible that widows and orphans appear in LaTeX documents. You can try to deal with them using manual page formatting, but there's also an automatic solution.

LaTeX has a parameter for 'penalty' for widows and orphans ('club lines' in LaTeX terminology). With the greater penalty LaTeX will try more to avoid widows and orphans. You can try to increase these penalties by putting following commands in your document preamble:

<source lang="latex">
\widowpenalty=300
\clubpenalty=300
</source>

If this does not help, you can try increasing these values even more, to a maximum of 10000. However, it is not recommended to set this value too high, as setting it to 10000 forbids LaTeX from doing this altogether, which might result in strange behavior.

It also helps to have rubber band values for the space between paragraphs:

<source lang="latex">
\setlength{\parskip}{3ex plus 2ex minus 2ex}
</source>

== Summary ==

This tutorial is relatively short, largely due to the fact that the whole LaTeX ethos is to concentrate on the content, and let LaTeX (and/or other typographers who have developed suitable document classes) decide on the best presentation. The next step to achieve greater control of page layout is to set about designing your own class. Unfortunately, that is not a straightforward task, and is often best left to the professionals!

<noinclude>
{{LaTeX/Bottom|Formatting|Mathematics}}
{{A-Roberts}}
</noinclude>
