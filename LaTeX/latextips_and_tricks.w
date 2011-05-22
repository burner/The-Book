><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

== ''id est'' and ''exempli gratia'' (i.e. and e.g.) ==
If you simply use the forms "<code>i.e.</code>" or "<code>e.g.</code>", LaTeX will treat the periods as end of sentence periods (i.e. [[w:Full stop|full stop]]) since they are followed by a space, and add more space before the next "sentence". To prevent LaTeX from adding space after the last period, the correct syntax is either "<code>i.e.\ </code>" or "<code>e.g.\ </code>".

Depending on style (e.g., ''[[w:The Chicago Manual of Style|The Chicago Manual of Style]]''), a comma can be used afterwards, which is interpreted by LaTeX as part of a sentence, since the period is not followed by any space. In this case, "<code>i.e.,</code>" and "<code>e.g.,</code>" do not need any special attention.

If the command <code>\frenchspacing</code> is used in the preamble, the space between sentences is always consistent.

==Grouping Figure/Equation Numbering by Section==

For long documents the numbering can become cumbersome as the numbers reach into double and triple digits. To reset the counters at the start of each section and prefix the numbers by the section number, include the following in the preamble.

<source lang="latex">
\usepackage{amsmath}
\numberwithin{equation}{section}
\numberwithin{figure}{section}
</source>

The same can be done with similar counter types and document units such as "subsection".

==Generic header==

As explained in the previous sections, a LaTeX source can be used to generate both a DVI and a PDF file. For very basic documents the source is the same but, if the documents gets more complicated, it could be necessary to make some changes in the source so that it will work for a format but it will not for the other. For example, all that is related to graphics has to be adapted according to the final format. As discussed in the section about floating objects, even if you should use different pictures according to the final format, you can override this limit putting in the same folder pictures in different formats (e.g., EPS and PNG) with the same name and link them without writing the extension. There is a simple way to solve this problem: 
<source lang="latex">
\usepackage{ifpdf}
</source>
or, if you don't have this package, you can add the following text just after <tt>\documentclass[...]{...} </tt>: 
<source lang="latex">
\newif\ifpdf
\ifx\pdfoutput\undefined
  \pdffalse
\else
  \ifnum\pdfoutput=1
    \pdftrue
  \else
    \pdffalse
  \fi
\fi
</source>

this is plain TeX code. The ''ifpdf'' package and this code, both define a new ''if-else'' you can use to change your code according to the compiler you are using. After you have used this code, you can use whenever you want in your document the following syntax:

<source lang="latex">
\ifpdf
    % we are running pdflatex
\else
    % we are running latex
\fi
</source>

place after <tt>\ifpdf</tt> the code you want to insert if you are compiling with ''pdflatex'', place after <tt>\else</tt> the code you want to insert if you are compiling with ''latex''. For example, you can use this syntax to load different packages according to the compiler.

==Graphics and Graph editors==

===Vector image editors with LaTeX support===
It is often preferable to use the same font and font size in your images as in the document. Moreover, for scientific images, you may need mathematical formulae or special characters (such as Greek letters). Both things can be achieved easily if the image editor allows you to use LaTeX code in your image. Most vector image editors do not offer this option. There are, however, a few exceptions.

In early days, LaTeX users used [[w:Xfig|Xfig]] for their drawings. The editor is still used by quite a few people nowadays because it has special 'export to LaTeX' features. It also gives you some very basic ways of encapsulating LaTeX text and math in the image (setting the text's 'special flag' to 'special' instead of 'normal'). When exporting, all LaTeX text will be put in a .tex-file, separately from the rest of the image (which is put in a .ps file). 

A newer and easier-to-use vector image editor specially tailored to LaTeX use is [[w:Ipe_(program)|IPE]]. It allows any LaTeX command, including but not limited to mathematical formulae in the image. The program saves its files as editable .eps or .pdf files, which eliminates the need of exporting your image each time you have edited it. 

A very versatile vector image editor is [[w:Inkscape|Inkscape]]. It does not support LaTeX text by itself, but you can use the plugin [http://pav.iki.fi/software/textext/ Textext] for that. This allows you to put any block of LaTeX code in your image. Additionally since version 0.48 you can export to vectorgraphics with texts separated in a .tex file. Using this way text is rendered by the latex compiler itself.

Another way to generate vectorgraphics is using the [[w:Asymptote_(vector_graphics_language)|Asymptote]] language. It is a programming language which produces vector images in encapsulated postscript format and supports LaTeX syntax in any textlabels.

===Graphs with gnuplot===
A simple method to include graphs and charts in LaTeX documents is to create it within a common spreadsheet software (OpenOffice Calc or MS Office Excel etc.) and include it in the document as a cropped screenshot. However, this produces poor quality rasterized images. Calc also allows you to copy-paste the charts into OpenOffice Draw and save them as PDF files.

Using Microsoft Excel 2010, charts can be copied directly to Microsoft Expression Design 4, where they can be saved as PDF files. These PDF files can be included in LaTeX. This method produces high quality vectorized images.

An excellent method to render graphs is through '''[[w:gnuplot|gnuplot]]''', a free and versatile plotting software, that has a special output filter directly for exporting files to LaTeX. We assume, that the data is in a CSV file (comma separated text) in the first and third column. A simple gnuplot script to plot the data can look like this:
[[Image:Soliton_2nd_order.svg|thumb|right|gnuplot can plot various numerical data, functions, error distribution as well as 3D graphs and surfaces]]
 set format "$%g$"
 set title "Graph 3: Dependence of $V_p$ on $R_0$"
 set xlabel "Resistance $R_0$ [$\Omega$]"
 set ylabel "Voltage $V_p$ [V]"
 set border 3
 set xtics nomirror
 set ytics nomirror
 set terminal epslatex
 set output "graph1.eps"
 plot "graph1.csv" using 1:3   #Plot the data

Now gnuplot produces two files: the graph drawing in <tt>graph.eps</tt> and the text in <tt>graph.tex</tt>. The second includes the EPS image, so that we only need to include the file graph.tex in our document:

 \input{graph1.tex}

The above steps can be automated by the package gnuplottex. By placing gnuplot commands inside \begin{gnuplot}\end{gnuplot}, and compiling with latex -shell-escape, the graphs are created and added into your document.

When using pdfLaTeX instead of simple LaTeX, we must convert the EPS image to PDF and to substitute the name in the <tt>graph1.tex</tt> file.  If we are working with a Unix-like shell, it is simply done using:

 eps2pdf graph1.eps
 sed -i s/".eps"/".pdf"/g graph1.tex

With the included tex file we can work as with an ordinary image.

Instead of calling <tt>eps2pdf</tt> directly, we can also include the <tt>epstopdf</tt> package that automates the process. If we include a graphics now and leave out the file extension, <tt>epstopdf</tt> will automatically transform the .eps-file to PDF and insert it in the text.

 \includegraphics{graph1}

This way, if we choose to output to PS or DVI, the EPS version is used and if we output to PDF directly, the converted PDF graphics is used. Please note that usage of <tt>epstopdf</tt> requires compiling with latex -shell-escape.

Note: Emacs AucTex users might want to check out [http://cars9.uchicago.edu/~ravel/software/gnuplot-mode.html Gnuplot-mode].
===Generate png screenshots===
This section describes how to generate a png screenshot of a LaTeX page using the *nix tool <tt>dvipng</tt> and the LaTeX package <tt>preview</tt>. This screenshots are useful, for example, if you want to include a LaTeX generated formula on a presentation using you favorite slideware like Powerpoint, Keynote or OpenOffice Impress. First, start by making sure you have the two required tools <tt>dvipng</tt> and <tt>preview</tt>. To check for the LaTeX package type
<source lang="bash">
locate preview.sty
</source>
on a console and if locate returns a directory, then you are good. Next, type
<source lang="bash">
which dvipng
</source>
to see if you have <tt>dvipng</tt> installed on your machine and ready to use. Again, if you get a directory, you are ready to start doing screenshot of LaTeX pages.
Say you want to take a screenshot of 
:<math>
\pi = \sqrt{12}\sum^\infty_{k=0} \frac{(-3)^{-k}}{2k+1}.
</math>
Write this formula on a TeX file name <tt> foo.tex</tt> in this way: 
<source lang="latex">
\documentclass{article}
\usepackage[active]{preview}
\begin{document}
\begin{preview}
\[
\pi = \sqrt{12}\sum^\infty_{k=0} \frac{(-3)^{-k}}{2k+1}
\]
\end{preview}
\end{document}
</source>
Run LaTeX as usual to generate the dvi file <tt>foo.dvi</tt>. Note the <tt>active</tt> option in the package declaration and the preview environment around the equation's code. Without any of these two, you won't get an <tt>dvi</tt> output.
Now, we want an X font size formula, where X is measure in pixels. You need to convert this, to dots per inch (dpi). The formula is: <tt><dpi> = <font_px>*72.27/10</tt>. If you want, for instance, X = 32, then the size in dpi corresponds to 231.26. This value will be passed to <tt>dvipng</tt> using the flag <tt>-D</tt>. To generate the desired png file run the command as follows: 
<source lang="bash">
dvipng -T tight -D 231.26 -o foo.png foo.dvi
</source> 
The flag <tt>-T</tt> sets the size of the image. The option <tt>tight</tt> will only include all ink put on the page. The option <tt>-o</tt> sends the output to the file name <tt>foo.png</tt>.  
==Spell-checking and Word Counting==
If you want to spell-check your document, you can use command-line <tt>aspell</tt> (preferably) or <tt>ispell</tt> programs.
 ispell yourfile.tex
 aspell -c yourfile.tex

Both understand LaTeX and will skip LaTeX commands. You can also use [[w:LyX|LyX]] or [[W:Kile|Kile]] — graphical LaTeX editors with built-in spell checking. Last another option is to [[../Export To Other Formats#Convert to plain text|convert LaTeX source to plain text]] and open resulting file in a word processor like OpenOffice.org or KOffice.

If you want to count words you can, again, use LyX or convert your LaTeX source to plain text and use, for example, UNIX <tt>wc</tt> command:
 detex yourfile | wc

An alternative to the <tt>detex</tt> command is the <tt>pdftotext</tt> command which extracts an ASCII text file from PDF:

 1. pdflatex yourfile.tex
 2. pdftotext yourfile.pdf
 3. wc yourfile.txt

== New even page ==

In the twoside-mode you have the ability to get a new odd-side page by: 
<source lang="latex">
\cleardoublepage
</source>

However, LaTeX doesn't give you the ability to get a new even-side page. The following method opens up this;

The following must be put in your document preamble:

<source lang="latex">
\usepackage{ifthen}

\newcommand{\newevenside}{
	\ifthenelse{\isodd{\thepage}}{\newpage}{
	\newpage
        \phantom{placeholder} % doesn't appear on page
	\thispagestyle{empty} % if want no header/footer
	\newpage
	}
}
</source>

To active the new even-side page, type the following where you want the new even-side:

<source lang="latex">
\newevenside
</source>

If the given page is an odd-side page, the next new page is subsequently an even-side page, and LaTeX will do nothing more than a regular \newpage. However, if the given page is an even page, LaTeX will make a new (odd) page, put in a placeholder, and make another new (even) page. A crude but effective method.



<noinclude>
{{LaTeX/Bottom|Collaborative Writing of LaTeX Documents|General Guidelines}}
</noinclude>
