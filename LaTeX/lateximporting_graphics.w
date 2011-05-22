><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>
Strictly speaking, LaTeX cannot manage pictures directly: in order to introduce graphics within documents, LaTeX just creates a box with the same size as the image you want to include and embeds the picture, without any other processing. This means you will have to take care that the images you want to include are in the right format to be included. This is not such a hard task because LaTeX supports the most common picture formats around.

==The <tt>graphicx</tt> package==

As stated before, LaTeX can't manage pictures directly, so we will need some extra help: we have to load the <code>graphicx</code> package in the preamble of our document:

<source lang="latex">
\usepackage{graphicx}
</source>

This package accepts as an argument the external driver to be used to manage pictures; however, the latest version of this package takes care of everything by itself, changing the driver according to the compiler you are using, so you don't have to worry about this. Still, just in case you want to understand better how it works, here are the possible options you can pass to the package:
* <tt>dvips</tt> (default if compiling with ''latex''), if you are compiling with ''latex'' to get a DVI and you want to see your document with a DVI or PS viewer.
* <tt>dvipdfm</tt>, if you are compiling with ''latex'' to get a DVI that you want to convert to PDF using ''dvipdfm'', to see your document with any PDF viewer.
* <tt>pdftex</tt> (default if compiling with ''pdflatex''), if you are compiling with ''pdftex'' to get a PDF that you will see with any PDF viewer.
but, again, you don't need to pass any option to the package because the default settings are fine in most of the cases.

In many respects, importing your images into your document using LaTeX is fairly simple... ''once'' you have your images in the right format that is! Therefore, I fear for many people the biggest effort will be the process of converting their graphics files. Now we will see which formats we can include and then we will see how to do it.

===Document Options===

The graphics and graphicx packages recognize the "draft" and "final" [[LaTeX/Basics#Document Class Options|options]] given in the <code>[[LaTeX/Basics#Document_Classes|\documentclass]][...]{...}</code> command at the start of the file. Using "draft" as the option will suppress the inclusion of the image in the output file and will replace the contents with the name of the image file that would have been seen. Using "final" will result in the image being placed in the output file. The default is "draft".

===Supported image formats===

As explained before, the image formats you can use depend on the driver that <tt>graphicx</tt> is using but, since the driver is automatically chosen according to the compiler, then the allowed image formats will depend on the compiler you are using.

====Compiling with ''latex''====

The only format you can include while compiling with ''latex'' is [[w:Encapsulated PostScript|Encapsulated PostScript]] ('''EPS''').

The EPS format was defined by Adobe Systems for making it easy for applications to import postscript-based graphics into documents. Because an EPS file declares the size of the image, it makes it easy for systems like LaTeX to arrange the text and the graphics in the best way. EPS is a [[w:Vector graphics|vector format]]—this means that it can have very high quality if it is created properly, with programs that are able to manage vector graphics. It is also possible to store bit-map pictures within EPS, but they will need ''a lot'' of disk space.

Many graphics software packages have the ability to save images in the EPS format (extension is normally <tt>.eps</tt>). Here are some examples of software that can output EPS formats:
* Printing in an EPS file:
** Under Windows, [http://sourceforge.net/projects/pdfcreator/ PDFCreator] is an open source software that can create PDF as well as EPS files. It installs a virtual printer that can be accessed from other software having a "print..." entry in their menu (virtually any program).
* Creating and converting vector graphics:
** Commercial vector graphics software, such as Adobe Illustrator, CorelDRAW, and FreeHand are commonly used and can ''read'' and ''write'' EPS figures. However, these products are limited to Windows and Mac OS platforms.
** [http://www.inkscape.org Inkscape] can save in vector EPS format, and it can run on multiple platforms. Inkscape cannot open EPS figures directly; however, with the [http://www.ctan.org/tex-archive/support/epstopdf/ epstopdf utility] one can convert EPS into PDF and Inkscape can import PDF. From version 0.48, Inkscape has a special PDF+LaTeX output option (and for EPS/PS too). See [http://wiki.inkscape.org/wiki/index.php/LaTeX Inkscape website].
** [http://live.gnome.org/Dia Dia] is a cross platform diagramming utility which can export eps.
* Creating and converting raster-only graphics to EPS:
** [http://www.gimp.org GIMP], has a graphical user interface, and it is multi-platform.
** For command-line:
***[http://pts.szit.bme.hu/sam2p/ Sam2p] (<code>convert</code>) or 
***[http://www.imagemagick.org/ ImageMagick] (<code>convert</code>) or
***[http://www.graphicsmagick.org/ GraphicsMagick] (<code>gm convert</code>).                
**These three programs operate much the same way, and can convert between most graphics formats. Sam2p however is the most recent of the three and seems to offer both the best quality and to result in the smallest files.
***[http://imgtops.sourceforge.net/ imgtops].  A lightweight graphics utility.
* Creating publication-quality vector-based plots and charts:
** [http://www.gnuplot.info Gnuplot], producing scientific graphics since 1986.
** [http://www.r-project.org/ R], statistical and scientific figures.
** [http://gmt.soest.hawaii.edu/ Generic Mapping Tools (GMT)], maps and a wide range of highly customisable plots.
** [http://projects.gnome.org/gnumeric/ Gnumeric], spreadsheets has SVG, EPS, PDF export
** [http://matplotlib.sourceforge.net/ matplotlib], plotting library written in python, with PDF and EPS export.
There are some tricks to be able to import formats other than EPS into your DVI document, but they're very complicated. On the other hand, converting any image to EPS is very simple, so it's not worth considering them.

====Compiling with <tt>pdflatex</tt>====

If you are compiling with <code>pdflatex</code> to produce a PDF, you have a wider choice. You can insert:
* '''JPG''', widely used on Internet, digital cameras, etc. They are the best choice if you want to insert photos
* '''PNG''', a very common format (even if not as much as JPG); it's a [[w:lossless|lossless]] format and it's the best choice for diagrams (if you were not able to generate a [[w:Vector graphics|vector]] version) and screenshots
* '''PDF''', it is widely used for documents but can be used to store images as well. It supports both vector and [[w:Raster graphics|bit-map]] images, but it's not recommended for the latter, as JPG or PNG will provide the same result using less disk space.
* '''Vector formats''' can be used with the help of Inkscape. [http://mirrors.ctan.org/info/svg-inkscape/InkscapePDFLaTeX.pdf There are instructions] on how to save your vector images in a PDF format understood by LaTeX and have LaTeX manage the text styles and sizes in the image automatically.
* '''EPS''' can be used with the help of  the epstopdf package. Please see [http://dirkraffel.com/2007/11/19/include-eps-files-in-latex  these instructions].

JPG and PNG are supported by any image processing program, so you just have to use the one you prefer. If you want to create high quality vector PDF to embed within your PDF document, you can use [http://www.inkscape.org Inkscape]: it supports many vector formats and so you can use it to convert from one to other. You could also create your graphics directly with Inkscape. If you want to make mathematical plots, then [http://www.gnuplot.info Gnuplot] can save in any format. 

Note, that EPS files cannot be used with pdflatex, however they can be converted to PDF using the [http://www.ctan.org/tex-archive/support/epstopdf/ epstopdf utility], included in most LaTeX distributions. This can be called automatically by LaTeX using the [http://www.ctan.org/tex-archive/help/Catalogue/entries/epstopdf-pkg.html epstopdf package]. In Windows, multiple files can be converted by placing the following line in a [[w:Batch file|batch file]] (a text file with a .BAT extension) in the same directory as the images:
<source lang="dos">
for %%f in (*.eps) do epstopdf %%f
</source>
which can then be run from the command line.
If [http://www.ctan.org/tex-archive/support/epstopdf/ epstopdf] produces whole page with your small graphics somewhere on it, use 
<source lang="bash">
$ epstopdf --gsopt=-dEPSCrop foo.eps
</source> 
or try using [http://svn.ghostscript.com/ghostscript/trunk/gs/doc/Ps2pdf.htm ps2pdf] utility 
<source lang="bash">
$ ps2pdf -dEPSCrop foo.eps
</source> 
to crop final PDF.

Images can be saved in multiple formats for different purposes. For example, a directory can have "<tt>diagram.pdf</tt>" for high-resolution printing, while "<tt>diagram.png</tt>" can be used for previewing on the monitor. You can specify which image file is to be used by <code>pdflatex</code> through the preamble command:
<source lang="latex">
\DeclareGraphicsExtensions{.pdf,.png,.jpg}
</source>
which specifies the files to include in the document, if files with the same basename exist, but with different extensions.

Acrobat Reader sometimes has problems with displaying colors correctly if you include graphics in PNG format with alpha channel. You can solve this problem by dropping the alpha channel. On Linux it can be achieved with <code>convert</code> from the ImageMagick program:
<source lang="bash">
convert -alpha off input.png output.png
</source>

=== Including graphics ===

Now that we have seen which formats we can include and how we could manage those formats, it's time to learn how to include them in our document.
After you have loaded the <tt>graphicx</tt> package in your preamble, you can include images with <tt>\includegraphics</tt>, whose syntax is the following:

<source lang="latex">
\includegraphics[attr1=val1, attr2=val2, ..., attrn=valn]{imagename}
</source>

As you should hopefully be aware by now, arguments in square brackets are optional, whereas arguments in curly braces are compulsory. The argument in the curly braces is the name of the image. Write it ''without'' the extension. This way the LaTeX compiler will look for any supported image format in that directory and will take the best one (EPS if the output is DVI; JPEG, PNG or PDF if the output is PDF). The variety of possible attributes that can be set is fairly large, so only the most common are covered below:

{| class="wikitable"
| <tt>width=xx</tt>
| width="50%" | Specify the preferred width of the imported image to ''xx''.
| rowspan="2" | ''NB. Only specifying either width or height will scale the image whilst maintaining the aspect ratio.''
|-
| <tt>height=xx</tt>
| Specify the preferred height of the imported image to ''xx''.
|-
| <tt>keepaspectratio</tt>
| colspan="2" | This can be set to either ''true'' or ''false''. When true, it will scale the image according to both height and width, but will not distort the image, so that neither width nor height are exceeded.
|-
| <tt>scale=xx</tt>
| colspan="2" | Scales the image by the desired scale factor. e.g, 0.5 to reduce by half, or 2 to double.
|-
| <tt>angle=xx</tt>
| colspan="2" | This option can rotate the image by ''xx'' degrees (anti-clockwise)
|-
| <tt>trim=l b r t</tt>
| colspan="2" | This option will crop the imported image by ''l'' from the left, ''b'' from the bottom, ''r'' from the right, and ''t'' from the top. Where l, b, r and t are lengths.
|-
| <tt>clip</tt>
| colspan="2" | For the <tt>trim</tt> option to work, you must set <tt>clip=true</tt>.
|-
| <tt>page=x</tt>
| colspan="2" | If the image file is a pdf file with multiple pages, this parameter allows you to use a different page than the first.
|}

In order to use more than one option at a time, simply separate each with a comma. The order you give the options matters. E.g you should first rotate your graphic (with angle) and then specify its width. 

Included graphics will be inserted just ''there'', where you placed the code, and the compiler will handle them as "big boxes". As we will see in the [[LaTeX/Floats, Figures and Captions|next section]], this may lead to a bad output so you'd better place graphics inside floating objects.

Also note that the trim option is not working with XeLaTex.

=== Examples ===

OK, it's time to see graphicx in action. Here are some examples:

<source lang="latex">
\includegraphics{chick}
</source>

This simply imports the image, without any other processing. However, it is very large (so I won't display it here!). So, let's scale it down:

{| class="wikitable"
|<source lang="latex">
\includegraphics[scale=0.5]{chick}
</source>
|[[Image:chick1.png]]
|}

This has now reduced by half. If you wish to be more specific and give actual lengths of the image dimensions, this is how to go about it:

{| class="wikitable"
|<source lang="latex">
\includegraphics[width=2.5cm]{chick}
</source>
|[[Image:chick2.png]]
|}

One can also specify the scale with respect to the width of a line in the local environment (<code>\linewidth</code>), the width of the text on a page (<code>\textwidth</code>) or the height of the text on a page (<code>\textheight</code>) (pictures not shown):

<source lang="latex">
\includegraphics[width=0.5\linewidth]{chick}

\includegraphics[width=0.75\textwidth]{chick}

\includegraphics[height=0.75\textheight]{chick}
</source>

To rotate (I also scaled the image down):

{| class="wikitable"
|<source lang="latex">
\includegraphics[scale=0.5, angle=180]{chick}
</source>
|[[Image:chick3.png]]
|}

And finally, an example of how to crop an image should you wish to focus in one particular area of interest:

{| class="wikitable"
|<source lang="latex">
%trim option's parameter order: left bottom right top
\includegraphics[trim = 10mm 80mm 20mm 5mm, clip, width=3cm]{chick}
</source>
|[[Image:chick4.png]]
|}

Note the presence of <code>clip</code>, as the trim operation will not work without it.

As you may have noticed, the file name of the picture is always without the extensions: LaTeX will take care of getting the right version for us. Consider the following situation: you have added some pictures to your document in JPG and you have successfully compiled it in PDF. Now you want to compile it in DVI, you run ''latex'' and you get a lot of errors... because you forgot to provide the EPS versions of the pictures you want to insert. At the beginning of this book, we had stated that the same LaTeX source can be compiled in both DVI and PDF without any change. This is true, as long as you don't use particular packages, and <code>graphicx</code> is one of those. In any case, you can still use both compilers with documents with pictures as well, as long as you always remember to provide the pictures in two formats (EPS and one of JPG, PNG and PDF).

=== Borders ===
It is possible to have LaTeX create a border around your image by using <code>fbox</code>:

<source lang="latex">
\setlength\fboxsep{0pt}
\setlength\fboxrule{0.5pt}
\fbox{\includegraphics{chick}}
</source>

You can control the border padding with the <code>\setlength\fboxsep{0pt}</code> command, in this case I set it to 0pt to avoid any padding, so the border will be placed tightly around the image. You can control the thickness of the border by adjusting the <code>\setlength\fboxrule{0.5pt}</code> command.

=== Graphics storage ===

There is a way to tell LaTeX where to look for images: for example, it can be useful if you store images centrally for use in many different documents. The answer is in the command <tt>\graphicspath</tt> which you supply with an argument giving the name of an additional directory path you want searched when a file uses the <tt>\includegraphics</tt> command, here are some examples (trailing / is required):

<source lang="latex">
\graphicspath{{c:\mypict~1\camera}}
\graphicspath{{c:/mypict~1/camera/}} *
\graphicspath{{/var/lib/images/}}
\graphicspath{{./images/}}
\graphicspath{{images_folder/}{other_folder/}{third_folder/}}
\graphicspath{{images//}}
</source>

<nowiki> * </nowiki> goes well in win XP

please see http://www.ctan.org/tex-archive/macros/latex/required/graphics/grfguide.pdf. The last command searches the files recursively because of the double slash "//". In the example shown you would have a directory named "images" in the same directory as your man tex file, i.e. this is RELATIVE addressing. In this manner you can create a "human sensible" directory tree below this that could e.g. make it easier for you to sort/find/edit large numbers of image files. There is a caveat that using this recursive search may hog memory, please keep this in mind.

As you may have noticed, in the first example I've used the "safe" (MS-DOS) form of the Windows ''MyPictures'' folder because it's a bad idea to use directory names containing spaces. Using absolute paths, <tt>\graphicspath</tt> does make your file less portable, while using relative paths (like the last example), you shouldn't have any problem with portability, but remember not to use spaces in file-names.  Alternatively, if you are using PDFLaTeX, you can use the package <tt>grffile</tt> which will then allow you to use spaces in file names.

=== Images as Figures ===

There are many scenarios where you might want to accompany an image with a caption and possibly a cross-reference. This is done using the figure environment. The following code sample shows the bare minimum required to use an image as a figure.

<source lang="latex">
\begin{figure}[htb]
\includegraphics{image.png}
\end{figure}
</source>

The above code extract is relatively trivial, and doesn't offer much functionality. The following code sample shows an extended use of the figure environment which is almost universally useful, offering a caption and label, centering the image and scaling it to 80% of the width of the text.

<source lang="latex">
\begin{figure}[htb]
\begin{center}
\leavevmode
\includegraphics[width=0.8\textwidth]{image.png}
\end{center}
\caption{Awesome Image}
\label{fig:awesome_image}
\end{figure}
</source>

The figure environment is not exclusively used for images. More information on the figure environment and how to use it can be found in [[LaTeX/Floats, Figures and Captions|Floats, Figures and Captions]].


=== Text Wrapping around Images ===

Text can also be wrapped around images. (This is especially useful if you include tall pictures.)

<source lang="latex">
%import section
\usepackage{wrapfig}

% content section
\begin{wrapfigure}{r}{8cm} % "l" or "r" for the side on the page. And the width parameter for the width of the image space.
\centering
\includegraphics[height=80mm]{Abb/bluesniper.jpg}
\caption{Selbstgebaute „Bluesniper“ um Bluetooth-Geräte aus über 1 km Entfernung anzugreifen. (Stand: 2004)}
\label{bluesniper}
\end{wrapfigure}
</source>

=== Including full PDF pages ===

There is a great package for including full pages of PDF files: [http://www.ctan.org/tex-archive/macros/latex/contrib/pdfpages pdfpages]. It is capable of inserting full pages as is and more pages per one page in any layout (e.g. 2x3). See more information in its [http://www.ctan.org/tex-archive/macros/latex/contrib/pdfpages/pdfpages.pdf documentation].

==Creating Vector Graphics==

===TikZ/PGF===
<small>''More thorough introduction to TikZ is available at the [[LaTeX/Creating Graphics#TikZ.2FPGF|Creating Graphics]] chapter''</small>

You can draw graphics directly with TeX commands using the tikz package: http://ftp.dante.de/tex-archive/help/Catalogue/entries/pgf.html
It comes with very good documentation with many examples.

<source lang="latex">
% This needs \usepackage{tikz} in the preamble
\begin{figure}
  \centering
  \begin{tikzpicture}
    \draw[thick,rounded corners=8pt] 
(0,0) -- (0,2) -- (1,3.25) -- (2,2) -- (2,0) -- (0,2) -- (2,2) -- (0,0) -- (2,0);
  \end{tikzpicture}
  \caption{This is the caption of my figure}
  \label{fig:test}
\end{figure}
</source>

An extensive collection of examples can be found here: http://www.texample.net/tikz/

Other packages building on top of TikZ (e.g. for drawing electrical circuits) can be found here: http://ftp.dante.de/tex-archive/help/Catalogue/bytopic.html#pgftikzsection

===Xfig===

Vector graphics can be created using the vector painting program Xfig (see [[LaTeX/Installation#Xfig|Installation]]), and exported for LaTeX. In Xfig, once your graphic is saved as a file <tt>test.fig</tt>, you need to export it using the '''File > Export''' drop down menu from the main Xfig window and then select the "Combined PS/Latex (both parts)" in the language drop down list. If you don't change any other settings, two files will be created in the same directory as the <tt>test.fig</tt> file, such as: <tt>test.pstex_t</tt> and <tt>test.pstex</tt>. The figure can then be placed in a LaTeX document:
<source lang="latex">
\begin{figure}
  \centering
  \input{./xfig/test.pstex_t}
  \caption{This is the caption of my figure}
  \label{fig:test}
\end{figure}
</source>

===ipe===

The Ipe extensible drawing editor is a free vector graphics editor for creating figures in PDF or EPS format.
Unlike Xfig, ipe represents [[LaTeX]] fonts in their correct size on the screen which makes it easier to place text labels at the right spot.
ipe also has various snapping modes (for example, snapping to points, lines, or intersections) that can be used to geometrically construct.

===Inkscape===

Another program for creating vector graphics is [http://www.inkscape.org/ Inkscape]. It works with [http://www.w3.org/Graphics/SVG/ Scalable Vector Graphics (SVG)] files, although it can export to many formats that can be included in [[LaTeX]] files, such as EPS and PDF.
From version 0.48, there is a combined PDF/EPS/PS+LaTeX output option, like XFig has.

===Editing EPS graphics===

As described above, graphics content can be imported into [[LaTeX]] from outside programs as EPS files. But sometimes you want to edit or retouch these graphics files. An EPS file can be edited with any text editor since it is formatted as ASCII. In a text editor, you can achieve simple operations like replacing strings or moving items slightly, but anything further becomes cumbersome.

To properly edit an EPS file, you can convert it to an ''editable'' format using [http://www.pstoedit.net/ pstoedit]. For instance, to get an Xfig-editable file, do:
<source lang="bash">
$ pstoedit -f fig input.eps output.fig
</source>And to get an SVG file for Inkscape you can do:
<source lang="bash">
$ pstoedit -f plot-svg input.eps output.svg
</source>

Sometimes pstoedit fails to create the target format (for example when the EPS file contains clipping information). A more robust way to edit EPS files is achieved by converting it first to PDF and then importing the resulting PDF in Inkscape. Inkscape uses the Cairo library that achieves a high-quality transformation of the original EPS figure:
<source lang="bash">
$ epstopdf input.eps
$ inkscape input.pdf
</source>

When all of the above fails, one can simplify the EPS file before attempting other conversions, by using the [http://linuxcommand.org/man_pages/eps2eps1.html eps2eps] tool (also see next section):
<source lang="bash">
$ eps2eps input.eps input-e2.eps
</source>
This will convert all the fonts to pre-drawn images, which is sometimes desirable when submitting manuscripts for publication. However, on the downside, the fonts are NOT converted to lines, but instead to bitmaps, which reduces the quality of the fonts.

===Converting a color EPS to grayscale===

Sometimes color EPS figures need to be converted to black-and-white or grayscale to meet publication requirements. This can be achieved with the [http://linuxcommand.org/man_pages/eps2eps1.html eps2eps] of the  [http://ghostscript.com/ Ghostscript] package and [http://www.mpch-mainz.mpg.de/~joeckel/pscol/index.html pscol] programs:
<source lang="bash">
$ eps2eps input.eps input-e2.eps
$ pscol -0gray input-e2.eps input-gray.eps
</source>

<noinclude>
{{LaTeX/Bottom|Tables|Floats, Figures and Captions}}
</noinclude>
