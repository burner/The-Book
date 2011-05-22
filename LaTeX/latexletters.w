><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

Sometimes the mundane things are the most painful. However, it doesn't have to be that way because of evolved, user-friendly templates. Thankfully, LaTeX allows for very quick letter writing, with little hassle.

==The letter class==
To write letters use the standard document class ''letter''.

You can write multiple letters in one LaTeX file - start each one with \begin{letter}{''recipient''} and end with \end{letter}. You can leave ''recipient'' blank. Each letter consists of four parts:
# opening (like \opening{Dear Sir or Madam,} or \opening{Dear Kate,}
# main body - written as usual in LaTeX
# closing (like \closing{Yours sincerely,}
#: LaTeX will leave some space after closing for your hand-written signature; then it will put your name and surname, if you have declared them.
# additional elements: post scripta, carbon copy  and list of enclosures 

If you want your name, address and telephone number to appear in the letter, you have to declare them first signature, address and telephone.

The output letter will look like this:

[[Image:LaTeX-letter.png|none|150px|frame|A sample letter.]]

Here is the example's code:

<source lang="latex">
\documentclass{letter}
\signature{Joe Bloggs}
\address{21 Bridge Street \\ Smallville \\ Dunwich DU3 4WE}
\begin{document}

\begin{letter}{Director \\ Doe \& Co \\ 35 Anthony Road
\\ Newport \\ Ipswich IP3 5RT}
\opening{Dear Sir or Madam:}

I am writing to you on behalf of the Wikipedia project (http://www.wikipedia.org/),
an endeavour to build a fully-fledged multilingual encyclopaedia in an entirely
open manner, to ask for permission to use your copyrighted material.

% The \ldots command produces dots in a way that will not upset
% the typesetting of the document.
\ldots 

That said, allow me to reiterate that your material will be used to the noble end of
providing a free collection of knowledge for everyone; naturally enough, only if you
agree. If that is the case, could you kindly fill in the attached form and post it
back to me? We shall greatly appreciate it.

Thank you for your time and consideration.

I look forward to your reply.

\closing{Yours Faithfully,}
\end{letter}

\end{document}

</source>
To move the closing and signature parts to the left, insert the following before \begin{document}:

<source lang="latex">\longindentation=0pt</source>

The amount of space to the left can be adjusted by increasing the 0pt.

==Envelopes==
Here is a relatively simple envelope which uses the <tt>geometry</tt> package which is used because it vastly simplifies the task of rearranging things on the page (and the page itself).
<source lang="latex">
% envelope.tex
\documentclass{letter}
\usepackage[left=1in,top=0.15in,papersize={4.125in,9.5in},landscape,twoside=false]{geometry}
\setlength\parskip{0pt}
\pagestyle{empty}

\begin{document}

FROM-NAME

FROM-STREET ADDRESS

FROM-CITY, STATE, \ ZIP

\vspace{1.0in}\large
\setlength\parindent{3.6in}

TO-NAME

TO-STREET ADDRESS

TO-CITY, STATE, \ ZIP

\end{document}
</source>

[[Image:Envelope.jpg|none|100px|frame|A sample envelope to be printed in landscape mode.]]

This will certainly take care of the spacing but the actual printing is between you and your printer. After all, different printers have different feeding mechanisms for envelopes. You may find the following commands useful for printing the envelope.

<pre>
$ pdflatex envelope.tex
$ pdf2ps envelope.pdf
$ lpr -o landscape envelope.ps
</pre>

Alternatively, you can use the latex dvi output driver.

In the first line, dvips command converts the .dvi file produced by latex into a .ps (PostScript) file. In the second line, the PostScript file is sent to the printer. 
<pre>
$ latex envelope.tex && dvips -t unknown -T 9.5in,4.125in envelope.dvi
$ lpr -o landscape envelope.ps
</pre>

I have found that <tt>pdflatex</tt> creates the right page size but not <tt>dvips</tt> despite what it says in the <tt>geometry</tt> manual. It will never work though unless your printer settings are adjusted to the correct page style. These settings depend on the printer filter you are using and in CUPS might be available on the <tt>lpr</tt> command line if you are masochistic.

==Windowed envelopes==
An alternative to separately printing addresses on envelopes is to use the letter class from the KOMA package.  It supports additional features like folding marks and the correct address placement for windowed envelopes.  Using the scrlttr2 document class from the KOMA package the example letter code is:

<source lang="latex">
% koma_env.tex
\documentclass[a4paper]{scrlttr2}
\usepackage{lmodern}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[english]{babel}
\usepackage{url}


\setkomavar{fromname}{Joe Bloggs}
\setkomavar{fromaddress}{21 Bridge Street \\ Smallville \\ Dunwich DU3 4WE}
\setkomavar{fromphone}{0123 45679}

\begin{document}

\begin{letter}{Director \\ Doe \& Co \\ 35 Anthony Road
\\ Newport \\ Ipswich IP3 5RT}

\KOMAoptions{fromphone=true,fromfax=false}
\setkomavar{subject}{Wikipedia}
\setkomavar{customer}{2342}
\opening{Dear Sir or Madam,}

I am writing to you on behalf of the Wikipedia project
(\url{http://www.wikipedia.org/}), an endeavour to build a
fully-fledged multilingual encyclopaedia in an entirely open
manner, to ask for permission to use your copyrighted material.

\ldots 
 
That said, allow me to reiterate that your material will be used
to the noble end of providing a free collection of knowledge for
everyone; naturally enough, only if you agree. If that is the
case, could you kindly fill in the attached form and post it back
to me? We shall greatly appreciate it.
 
Thank you for your time and consideration.

I look forward to your reply.

\closing{Yours Faithfully,}
\ps{P.S. You can find the full text of GFDL license at
\url{http://www.gnu.org/copyleft/fdl.html}.}
\encl{Copyright permission form}

\end{letter}

\end{document}
</source>

The output is generated via
<pre>
$ pdflatex koma_env
</pre>

[[Image:Koma_env.png|none|150px|frame|A sample letter with folding marks ready for standardized windowed envelopes.]]

Folding the print of the resulting file koma_env.pdf according the folding marks it can be placed into standardized windowed envelopes DIN C6/5, DL, C4, C5 or C6.

In addition to the default, the KOMA-package includes predefined format definitions for different standardized Swiss and Japanese letter formats.

==Reference: letter.cls commands==

{| cellspacing="0" border="1"
! command !! description
|-
| \name{} ||
|-
| \signature{} ||
|-
| \address{} ||
|-
| \location{} ||
|-
| \telephone{} ||
|-
| \makelabels ||
|-
| \stopbreaks ||
|-
| \startbreaks ||
|-
| \opening{} ||
|-
| \closing{} ||
|-
| \cc{} || Start a parbox introduced with \ccname:
|-
| \encl{} || Start a parbox introduced with \enclname:
|-
| \ps || Begins a new paragraph, normally at the close of the letter
|-
| \stopletter || (empty)
|-
| \returnaddress || (empty)
|-
| \startlabels ||
|-
| \mlabel{}{} ||
|-
| \descriptionlabel{} ||
|-
| \ccname || "cc"
|-
| \enclname || "encl"
|-
| \pagename || "Page"
|-
| \headtoname || "To"
|-
| \today || Long form date
|-
|}

<br/>

{| cellspacing="0" border="1"
! environment !! Description
|-
| letter{} || See main article
|-
| description ||
|-
| verse ||
|-
| quotation ||
|-
| quote ||
|-
|}

==Sources==
* [http://www.ctan.org/tex-archive/macros/latex/contrib/koma-script/scrguien.pdf KOMA-Script - The Guide]

{{-}}
{{TODO|
* mention/introduce <code>cdpbundl</code> package (package for typesetting italian  styled business letters)
}}

<noinclude>
{{LaTeX/Bottom|Algorithms and Pseudocode|Teacher's Corner}}
</noinclude>

[[pl:LaTeX/Pisanie listów]]
