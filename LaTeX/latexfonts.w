><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

In order to select another than the default typeface in Latex environment, it is necessary to include some commands in the preamble of the document.

For example:

<source lang="latex">
 \usepackage[T1]{fontenc}
 \usepackage[light,math]{iwona}
</source>

'''Important:''' note that this only works for fonts that are already prepared for use with LaTeX. If what you have is a ttf font or similar, you will have to convert it and make it available to LaTeX. See the external links section below for some useful resources.

=== Example ===
Below is an example found at the Google discussion group latexlovers. The example demonstrates how to select different fonts in a simple document.

<source lang="latex">
\documentclass{book}

\begin{document}

 % using default font  (\familydefault = \rmdefault = Computer Modern Roman)
 Lorem ipsum dolor sit amet, consectitur adipiscing elit.

 \renewcommand*\rmdefault{ppl}\normalfont\upshape
 Lorem ipsum dolor sit amet, consectitur adipiscing elit. % using Palatino font

 \renewcommand*\rmdefault{iwona}\normalfont\upshape
 Lorem ipsum dolor sit amet, consectitur adipiscing elit. % using Iwona font

\end{document}
</source>

== XeTeX ==

If you use the [[w:XeTeX|XeTeX]] or [[w:LuaTeX|LuaTeX]] engine and the [http://www.ctan.org/tex-archive/help/Catalogue/entries/fontspec.html fontspec] package, you'll be able to use any font installed in the system effortlessly. XeTeX also allows using [[w:Opentype|OpenType]] technology of modern fonts like specifying alternate glyphs and optical size variants. XeTeX also uses [[w:Unicode|Unicode]] by default, which might be helpful for font issues.

To use the fonts, simply load the fontspec package and set the font:

<source lang="latex">
\documentclass{article}

\usepackage{fontspec}
\setmainfont{Arial}

\begin{document}
Lorem ipsum...
\end{document}
</source>

Then compile the document with XeLateX or LuaLaTeX. Note that you can only generate .pdf files, and that you need a sufficiently new TeX distribution (TeX Live 2009 should work for XeTeX and Tex Live 2010 for LuaTeX). Also you should NOT load the inputenc or fontenc package. To make support both pdflatex and XeTeX you can use the <code>\ifxetex</code> macro from the [http://www.ctan.org/tex-archive/macros/generic/ifxetex/ ifxetex] package.

<source lang="latex">
\documentclass{article}
\usepackage{ifxetex}

\ifxetex
  \usepackage{fontspec}
  \usepackage{xunicode}
  \defaultfontfeatures{Mapping=tex-text} % To support LaTeX quoting style
  \setromanfont{Hoefler Text}
\else
  \usepackage[utf8]{inputenc}
  \usepackage[T1]{fontenc}
\fi

\begin{document}
Lorem ipsum...
\end{document}
</source>

== Useful websites ==

* [http://www.tug.dk/FontCatalogue/ The Latex Font Catalogue]
* [http://www.cl.cam.ac.uk/~rf10/pstex/latexcommands.htm LaTeX font commands]
* [http://www.ee.iitb.ac.in/~trivedi/LatexHelp/latexfont.htm How to change fonts in Latex]
* [ftp://tug.ctan.org/tex-archive/fonts/utilities/fontinst/doc/talks/et99-font-tutorial.pdf Understanding the world of TEX fonts and mastering the basics of fontinst]
* [http://www.tug.org/TUGboat/Articles/tb27-1/tb86kroonenberg-fonts.pdf Font installation the shallow way] ''"For one-off projects, you can cut corners with font installation'' (i.e. fontinst) ''and end up with a more manageable set of files and a cleaner TEX installation. This article shows how and why"''

=== TrueType (ttf) fonts ===
* [http://c.caignaert.free.fr/Install-ttf-Font.pdf Step-by-step guide to manually install a ttf-font for PdfTeX]
* [http://www.tex.ac.uk/ctan/support/installfont/installfont.pdf A bash script for installing a LaTeX font family] ([http://latex.josef-kleber.de/download/installfont-tl MikTeX] / [http://latex.josef-kleber.de/download/installfont-tl TeXLive])
* [http://xpt.sourceforge.net/techdocs/language/latex/latex33-LaTeXAndTrueTypeFont LaTeX And TrueType Font]
* [http://fachschaft.physik.uni-greifswald.de/~stitch/ttf.html True Type Fonts with LaTeX under Linux + MiKTeX 2.5]
* [http://william.famille-blum.org/software/latexttf/index.html Unicode Truetype font installer for LaTeX under Windows + MikTeX]
* [http://www.radamir.com/tex/ttf-tex.htm Using TrueType fonts with TeX (LaTeX) and pdfTeX (pdfLaTeX)] (for MikTeX)

<noinclude>
{{LaTeX/Bottom||}}
</noinclude>
