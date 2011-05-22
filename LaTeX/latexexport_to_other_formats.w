><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

Strictly speaking, LaTeX source can be used to directly generate two formats:
* DVI using ''latex'', the first one to be supported
* PDF using ''pdflatex'', more recent
Using other software freely available on Internet, you can easily convert DVI and PDF to other document formats. In particular, you can obtain the PostScript version using software coming within your LaTeX distribution. Some LaTeX IDE will give you the possibility to generate the PostScript version directly (even if it uses internally a DVI mid-step, e.g. LaTeX → DVI → PS). It is also possible to create PDF from DVI and vice versa. It doesn't seem logical to create a file with two steps when you can create it straight away, but some users might need it because, as you remember from the first chapters, the format you can generate depends upon the formats of the images you want to include (EPS for DVI, PNG and JPG for PDF). Here you will find sections about different formats with description about how to get it.

Other formats can be produced, such as RTF (which can be used in Microsoft Word) and HTML. However, these documents are produced from software that parses and interprets the LaTeX files, and do not implement all the features available for the primary DVI and PDF outputs. Nonetheless, they do work, and can be crucial tools for collaboration with colleagues that do not edit documents with LaTeX.

==Convert to [[w:PDF|PDF]]==
=== Directly ===
 pdflatex my_file

=== DVI to PDF===
<pre>
dvipdfm my_file.dvi
</pre>
will create <tt>my_file.pdf</tt>. Another way is to pass through PS generation:
<pre>
dvi2ps myfile.dvi
ps2pdf myfile.ps
</pre>
you will get also a file called ''my_file.ps'' that you can delete.

===Merging PDF===

If you have created different PDF documents and you want to merge them into one single PDF file you can use the following command-line command. You need to have Ghostscript installed:

For Windows:

 gswin32 -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE=Merged.pdf -dBATCH 1.pdf 2.pdf 3.pdf

For Linux:

 gs -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE=Merged.pdf -dBATCH 1.pdf 2.pdf 3.pdf

Alternatively, [http://pdfshuffler.sourceforge.net/ PDF-Shuffler] is a small python-gtk application, which helps the user to merge or split pdf documents and rotate, crop and rearrange their pages using an interactive and intuitive graphical interface. This program may be avaliable in your Linux distribution's repository.

Another option to check out is [http://www.accesspdf.com/ pdftk] (or PDF toolkit), which is a command-line tool that can manipulate PDFs in many ways. To merge one or more files, use:

 pdftk 1.pdf 2.pdf 3.pdf cat output 123.pdf

''Note:'' If you are merging external PDF documents into a Latex document which is compiled with <code>pdflatex</code>, a much simpler option is to use the <code>pdfpages</code> package, e.g.:

<source lang="latex">
\usepackage{pdfpages}
...
\includepdf[pages=-]{Document1.pdf}
\includepdf[pages=-]{Document2.pdf}
...
</source>

Three simple [[wikipedia:Shell (computing)|shell]] scripts using the pdfpages package are provided in the [http://www2.warwick.ac.uk/fac/sci/statistics/staff/academic/firth/software/pdfjam pdfjam bundle] by D. Firth. They include options for merge several pdf (pdfjoin), put several pages in one physical sheet (pdfnup) and rotate pages (pdf90).

===XeTeX===
You can also use XeTeX (or, more precisely, XeLaTeX), which works in the same way as pdflatex: it creates a PDF file directly from LaTeX source. One advantage of XeTeX over standard LaTeX is support for Unicode and modern typography. See [[w:XeTeX|its Wikipedia entry]] for more details.

Customization of PDF output in XeTeX (setting document title, author, keywords etc.) is done using the configuration of [[LaTeX/Hyperlinks#Customization|hyperref]] package.

== Convert to [[w:PostScript|PostScript]] ==

;from PDF:
<pre>
pdf2ps my_file.pdf
</pre>

;from DVI:
<pre>
dvi2ps my_file.dvi
</pre>

== Convert to [[w:Rich Text Format|RTF]] ==

LaTeX can be converted into an RTF file, which in turn can be opened by a word processor such as [[w:OpenOffice.org Writer|OpenOffice.org Writer]] or [[w:Microsoft Word|Microsoft Word]]. This conversion is done through [http://latex2rtf.sourceforge.net/ latex2rtf], which can run on any computer platform. The program operates by reading the LaTeX source, and mimicking the behaviour of the LaTeX program. <code>latex2rtf</code> supports most of the standard implementations of LaTeX, such as standard formatting, some math typesetting, inclusion of EPS, PNG or JPG graphics, and tables. As well, it has some limited support for packages, such as varioref, and natbib. However, many other packages are not supported.

latex2rtf is simple to use. The Windows version has a GUI, which is straightforward to use. The command-line version is offered for all platforms, and can be used on an example <tt>mypaper.tex</tt> file:
 latex mypaper
 bibtex mypaper # if you use bibtex
 latex2rtf mypaper
Both <code>latex</code> and (if needed) <code>bibtex</code> commands need to be run ''before'' <code>latex2rtf</code>, because the <tt>.aux</tt> and <tt>.bbl</tt> files are needed to produce the proper output. The result of this conversion will create <tt>myfile.rtf</tt>, which you may open in many modern word processors such as Microsoft word or Open Office.

== Convert to HTML ==
There are many converters to HTML. One option is the [http://hevea.inria.fr HEVEA] program:
;LaTeX:
 hevea mylatexfile
;BibTeX:
 bibtex2html mybibtexfile
;TeX4ht
[http://www.cse.ohio-state.edu/~gurari/TeX4ht/ TeX4ht] is a very powerful conversion program, but its configuration is not straightforward. Basically a configuration file has to be prepared, and then the program is called.

==Convert to image formats==
=== [[w:PNG|PNG]] ===

To convert from PDF, open your file with [[GIMP]]. It will ask you which page you want to convert, whether you want to use anti-aliasing (choose ''strong'' if you want to get something similar to what you see on the screen). Try different resolutions to fit your needs, but 100 dpi should be enough. Once you have the image within GIMP, you can post-process it as you like and save it to any format supported by GIMP, as PNG for example. A method for DVI  files is [http://savannah.nongnu.org/projects/dvipng/ dvipng] (usage is the same as dvipdfm). Also, the "convert" command from the [http://www.imagemagick.org/ ImageMagick] suite can convert both DVI and PDF files to PNG.

You can optimize the resulting image using [http://optipng.sourceforge.net/ optipng] so that it will take up less space.

=== [[w:Scalable Vector Graphics|SVG]] ===

Convert it to PS as described before, then use the bash script [http://en.wikipedia.org/wiki/Wikipedia:WikiProject_Electronics/Ps2svg.sh ps2svg.sh] (it could be possible to write a step-by-step guide for Windows as well; all the software it uses is multiplatform).

One can also use [http://dvisvgm.sourceforge.net/ dvisvgm], an open source utility that converts from DVI to SVG.

==Convert to plain text==
If you are thinking of converting to plain text to perform a spell-checking or count words, read [[LaTeX/Tips and Tricks#Spell-checking and Word Counting|Tips and Tricks]] first.

Most LaTeX distributions come with <tt>detex</tt> program, which strips LaTeX commands. It can handle multi-file projects, so all you need is to give one command:
 detex yourfile
(note the omission of .tex extension). This will output result to standard output. If you want the plain text go to a file, use
 detex yourfile > yourfile.txt

If the output from <tt>detex</tt> does not satisfy you, you can try a newer version available on [http://code.google.com/p/opendetex/ Google Code], or use HTML conversion first and then copy text from your browser.

<noinclude>
{{LaTeX/Bottom|General Guidelines|Internationalization}}
</noinclude>
