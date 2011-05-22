>{{merge|LaTeX/Numbering}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">{{merge|LaTeX/Numbering}}
{{subpages}}
<noinclude>{{LaTeX/Top}}</noinclude>

This section will guide you through the various text, paragraph, and page formatting techniques. ''Formatting'' tends to refer to most things to do with appearance, so it makes the list of possible topics quite eclectic: text style, font, size; paragraph alignment, interline spacing, indents; special paragraph types; footnotes, margin notes, etc.

A lot of the formatting techniques are required to differentiate certain elements from the rest of the text. It is often necessary to add emphasis to key words or phrases. Footnotes are useful for providing extra information or clarification without interrupting the main flow of text. So, for these reasons, formatting is very important. However, it is also very easy to abuse, and a document that has been over-done can look and read worse than one with none at all.

== Text formatting ==

===Hyphenation===

LaTeX hyphenates words whenever necessary. If the hyphenation algorithm does not find the correct hyphenation points, you can remedy the situation
by using the following commands to tell TeX about the exception. The command
{{LaTeX/Usage|code=\hyphenation{word list} }}
causes the words listed in the argument to be hyphenated only at the points marked by “-”. The argument of the command should only contain words built from normal letters, or rather characters that are considered to be normal letters by LaTeX. It is known that the hyphenation algorithm does not find all correct American English hyphenation points for several words. A log of known exceptions is published periodically in the ''TUGboat'' journal. (See a 2008 list: http://www.tug.org/TUGboat/Articles/tb29-2/tb92hyf.pdf)

The hyphenation hints are stored for the language that is active when the hyphenation command occurs. This means that if you place a hyphenation command into the preamble of your document it will influence the English language hyphenation. If you place the command after the {{LaTeX/LaTeX|code=\begin{document}}} and you are using some package for national language support like babel, then the hyphenation hints will be active in the language activated through babel. The example below will allow “hyphenation” to be hyphenated as well as “Hyphenation”, and it prevents “FORTRAN”, “Fortran” and “fortran” from being hyphenated at all. No special characters or symbols are allowed in the argument. Example:
{{LaTeX/Usage|code=
\hyphenation{FORTRAN Hy-phen-a-tion}
}}

The command {{LaTeX/LaTeX|code=\-}} inserts a discretionary hyphen into a word.  This also becomes the only point where hyphenation is allowed in this word.  This command is especially useful for words containing special characters (e.g., accented characters), because LaTeX does not automatically hyphenate words containing special characters.
{{LaTeX/Example|code=
\begin{minipage}{2in}
I think this is: su\-per\-cal\-%
i\-frag\-i\-lis\-tic\-ex\-pi\-%
al\-i\-do\-cious
\end{minipage}
|render=<math>\begin{array}{l}\mbox{I think this is: supercalifragi-}\\
\mbox{listicexpialidocious}\end{array}</math>
}}

This can be quite cumbersome if one has many words that contain a dash like electromagnetic-endioscopy. One alternative to this is using the {{LaTeX/LaTeX|code=\hyp}} command of the {{LaTeX/Package|hyphenat}} package. This command typesets a hyphen and allows full automatic hyphenation of the other words forming the compound word. One would thus write
{{LaTeX/Usage|code=
electromagnetic\hyp{}endioscopy
}}

Several words can be kept together on one line with the command
{{LaTeX/Usage|code=
\mbox{text}
}}

It causes its argument to be kept together under all circumstances. Example:
{{LaTeX/Usage|code=
My phone number will change soon. It will be \mbox{0116 291 2319}.
}}

{{LaTeX/LaTeX|code=\fbox}} is similar to {{LaTeX/LaTeX|code=\mbox}}, but in addition there will be a visible box drawn around the content.

To avoid hyphenation altogether, the penalty for hyphenation can be set to an extreme value:
{{LaTeX/Usage|code=
\hyphenpenalty=100000
}}

Sometimes it can be useful to change the degree of which latex will hyphenate by also changing the value of {{LaTeX/LaTeX|code=\tolerance=1000}} along with {{LaTeX/LaTeX|code=\hyphenpenalty=1000}}.
One will have to experiment with the values to achieve the desired effect. A low tolerance, will not tolerate uneven spacing between words, thus hyphenation will be used more frequently than with higher tolerance.

=== Quote-marks ===

Latex treats left and right quotes as different entities. For single quotes, <tt>`</tt> (on American keyboards, this symbol is found on the tilde key (adjacent to the number 1 key on most) gives a left quote mark, and <tt>'</tt> is the right. For double quotes, simply double the symbols, and Latex will interpret them accordingly. (Although, you can use the <tt>"</tt> for right double quotes if you wish). On British keyboards, ' <tt>`</tt> ' is left of the ' 1 ' key and shares the key with ' ¬ ', and sometimes ' ¦ ' or ' | '. The apostrophe (' <tt>'</tt> ') key is to the right of the colon/semicolon key and shares it with the ' @ ' symbol.

{|cellspacing=0
|-
|style="border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">To `quote' in Latex</source>
|style="padding-left:3em;"|
[[Image:quote1.png]]
|-
|style="border-right:2px black solid;padding-right:3em;"| 
<source lang="latex" enclose="none">To ``quote'' in Latex</source>
|style="padding-left:3em;"|
[[Image:quote2.png]]
|-
|style="border-right:2px black solid;padding-right:3em;"| 
<source lang="latex" enclose="none">To ``quote" in Latex</source>
|style="padding-left:3em;"|
[[Image:quote2.png]]
|-
|style="border-right:2px black solid;padding-right:3em;"| 
<source lang="latex" enclose="none">To ,,quote'' in Latex</source>
|style="padding-left:3em;"|
[[Image:quote4.png]]
|-
|style="border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">``Please press the `x' key.'' </source>
|style="padding-left:3em;"|
[[Image:Latex_quote_3.png|x17px]]
|-
|style="border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">,,Proszę, naciśnij klawisz <<x>>''.</source>
|style="padding-left:3em;"|
[[Image:Latex_quote_4.png]]
|}

The right quote is also used for apostrophe in Latex without trouble.

For left bottom quote and European quoting style you need to use <code>T1</code> font encoding enabled by:
{{LaTeX/Usage|code=\usepackage[T1]{fontenc} }}

The package csquotes offers a multi-lingual solution to quotations, with integration to citation mechanisms offered by BibTeX. This packages allows for example one to switch languages and quotation styles according to babel language selections.

=== Diacritics ===

Diacritics may be added to letters placing an escaped diacritic metacharacter before the letter that requires the diacritic. Below is a list of diacritic metacharacters.

[[File:Latex Accents.png]]

To place a diacritic on top of an i or a j, its dot has to be removed. This is accomplished by typing <source lang="latex" enclose="none">\i</source> and <source lang="latex" enclose="none">\j</source>. If you have to write all your document in a foreign language and you have to use particular diacritics several times, then using the right configuration you can write those characters directly in your document. 

Recently, XeTeX has made it possible to encode Unicode characters directly in your .tex file. If using XeLaTeX, any Unicode combining diacritic can be used. Also LuaTeX assumes UTF-8 input, but is still in development, the latest version being 0.6 of April 2010.

For more information, see the section [[LaTeX/Internationalization|Internationalization]].

===Space between Words and Sentences ===

To get a straight right margin in the output, LaTeX inserts varying amounts of space between the words. By default, It also inserts slightly more space at the end of a sentence. However, the extra space added at the end of sentences is generally considered typographically old-fashioned in English language printing. (The practice is found in nineteenth century design and in twentieth century typewriter styles.) Most modern typesetters treat the end of sentence space the same as the interword space. (See for example, Bringhurst's ''Elements of Typographic Style''.) The additional space after periods can be disabled with the command

<source lang="latex">
\frenchspacing
</source>

which tells LaTeX not to insert more space after a period than after ordinary character. Frenchspacing can be turned off later in your document via the <source lang="latex" enclose="none">\nonfrenchspacing</source> command.

If an author wishes to use the wider end-of-sentence spacing, care must be exercised so that punctuation marks are not misinterpreted as ends of sentences. TeX assumes that sentences end with periods, question marks or exclamation marks. Although if a period follows an uppercase letter, this is not taken as a sentence ending, since periods after uppercase letters normally occur in abbreviations. Any exception from these assumptions has to be specified by the author. A backslash in front of a space generates a space that will not be enlarged. A tilde ‘<source lang="latex" enclose="none">~</source>’ character generates a space that cannot be enlarged and additionally prohibits a line break. The command <source lang="latex" enclose="none">\@</source> in front of a period specifies that this period terminates a sentence even when it follows an uppercase letter. (If you are using <source lang="latex" enclose="none">\frenchspacing</source>, then none of these exceptions need be specified.)

====Margin misalignment and interword spacing====

Some very long words, numbers or URLs may not be hyphenated properly and move far beyond side margin. One solution for this problem is to use <tt>sloppypar</tt> environment, which tells LaTeX to adjust word spacing less strictly. As a result, some spaces between words may be a bit too large, but long words will be placed properly.

{|
|style="border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">
This is a paragraph with
a very long word ABCDEFGHIJKLMNOPRST;
then we have an another bad thing
--- a long number 1234567890123456789.

\begin{sloppypar}
This is a paragraph with
a very long word ABCDEFGHIJKLMNOPRST;
then we have an another bad thing
--- a long number 1234567890123456789.
\end{sloppypar}
</source>
|style="padding-left:3em;"|[[Image:LaTeX sloppypar.png|450px|border]]
|}

===Ligatures===
Some letter combinations are typeset not just by setting the different letters one after the other, but by actually using special symbols (like "ﬀ"), called [[wikipedia:Typographical ligature|ligatures]].
Ligatures can be prohibited by inserting <source lang="latex" enclose="none">{}</source> or, if this does not work, <source lang="latex" enclose="none">{\kern0pt}</source> between the two letters in question. This might be necessary with words built from two words. Here is an example:

{|
|-
|style="border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">
\Large Not shelfful\\
but shelf{}ful
</source>
|style="padding-left:3em;"|[[Image:Latex example ligatures.png|200px]]
|}

Some tools are unable to perform search in documents that contain ligatures (a search for <code>"'''fi'''nally"</code> wouldn't find the string <code>"'''ﬁ'''nally"</code>). If one desires, for greater accessibility, to disable ligatures altogether in the whole document, the <source lang="latex" enclose="none">\DisableLigatures</source> from the [http://www.ctan.org/tex-archive/macros/latex/contrib/microtype/ microtype package] can be used:

<source lang="latex">
\usepackage{microtype}
\DisableLigatures{encoding = *, family = *}
</source>

Note that this will also disable ligatures such as <tt>--</tt> → –, <tt>---</tt> → —, etc.

If you are using XeLaTeX and OpenType fonts, the fontspec package allows for standard ligatures to be turned off as well as fancy swash ligatures to be turned on.

===Slash marks===
The normal typesetting of the <source lang="latex" enclose="none">/</source> character in LaTeX does not allow following characters to be "broken" on to new lines, which often create "overfull" errors in output (where letters push off the margin). Words that use slash marks, such as "input/output" should be typeset as "<source lang="latex" enclose="none">input\slash output</source>", which allow the line to "break" after the slash mark (if needed). The use of the <source lang="latex" enclose="none">/</source> character in LaTeX should be restricted to units, such as "<source lang="latex" enclose="none">mm/year</source>", which should not be broken over multiple lines.

=== Emphasizing Text ===

In order to add some emphasis to a word or phrase, the simplest way is to use the <source lang="latex" enclose="none">\emph{text}</source> command.

{|
|style="border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">
I want to \emph{emphasize} a word.
</source>
|style="padding-left:3em;"|
[[Image:emph.png]]
|}

=== Fonts ===

:''See also: [[LaTeX/Fonts]].''

In LaTeX, there are many ways to specify and control font, and this section is only intended to serve as a brief overview of the topic. 

==== Font Styles ====

There are three main font families: roman (e.g., Times), sans serif (e.g., Arial) and monospace (e.g., Courier). You can also specify styles such as italic and bold.

The following table lists the commands you will need to access the typical font styles:

{|class="wikitable"
! LaTeX command
! Equivalent to
! Output style
! Remarks
|-
| <code>\textnormal{…}</code>
| <code>{\normalfont …}</code>
| <span style="font-family: serif; font-size: larger;">document font family</span>
| this is the default or normal font
|-
| <code>\emph{…}</code>
| <code>{\em …}</code>
| <span style="font-family: serif; font-size: larger;"><em>emphasis</em></span>
| typically italics
|-
| <code>\textrm{…}</code>
| <code>{\rmfamily …}</code>
| <span style="font-family: serif; font-size: larger;">roman font family</span>
|
|-
| <code>\textsf{…}</code>
| <code>{\sffamily …}</code>
| <span style="font-family: sans-serif; font-size: larger;">sans serif font family</span>
|
|-
| <code>\texttt{…}</code>
| <code>{\ttfamily …}</code>
| <span style="font-family: monospace; font-size: larger;">teletypefont family</span>
| this is a fixed-width or monospace font
|-
| <code>\textup{…}</code>
| <code>{\upshape …}</code>
| <span style="font-family: serif; font-size: larger; font-style: normal;">upright shape</span>
| the same as the normal typeface
|-
| <code>\textit{…}</code>
| <code>{\itshape …}</code>
| <span style="font-family: serif; font-size: larger; font-style: italic;">italic shape</span>
| 
|-
| <code>\textsl{…}</code>
| <code>{\slshape …}</code>
| <span style="font-family: serif; font-size: larger; font-style: oblique;">slanted shape</span>
| a skewed version of the normal typeface (similar to, but slightly different from, italics)
|-
| <code>\textsc{…}</code>
| <code>{\scshape …}</code>
| <span style="font-family: serif; font-size: larger; font-variant:small-caps;">Small Capitals</span>
|
|-
| <code>\uppercase{…}</code>
| 
| <span style="font-family: serif; font-size: larger; text-transform:uppercase;">uppercase (all caps)</span>
| Also <code>\lowercase</code>. There are some caveats, though; see [http://www.tex.ac.uk/cgi-bin/texfaq2html?label=casechange here].
|-
| <code>\textbf{…}</code>
| <code>{\bfseries …}</code>
| <span style="font-family: serif; font-size: larger; font-weight: bold;">bold</span>
|
|-
| <code>\textmd{…}</code>
| <code>{\mdseries …}</code>
| <span style="font-family: serif; font-size: larger; font-weight: medium;">medium weight</span>
| a font weight in between normal and bold
|}

The commands in column two are not entirely equivalent to the commands in column one: They do not correct spacing after the selected font style has ended. The commands in column one are therefore in general recommended.

You may have noticed the absence of underline. Although this is available via the <source lang="latex" enclose="none">\underline{...}</source> command, text underlined in this way will not break properly. This functionality has to be added with the <tt>ulem</tt> (underline emphasis) package. Stick <source lang="latex" enclose="none">\usepackage{ulem}</source> in your preamble. By default, this overrides the <source lang="latex" enclose="none">\emph</source> command with the underline rather than the italic style. It is unlikely that you wish this to be the desired effect, so it is better to stop <tt>ulem</tt> taking over <source lang="latex" enclose="none">\emph</source> and simply call the underline command as and when it is needed.

* To restore the usual <code>em</code> formatting, add <source lang="latex" enclose="none">\normalem</source> straight after the document environment begins. Alternatively, use <source lang="latex" enclose="none">\usepackage[normalem]{ulem}</source>.
* To underline, use <source lang="latex" enclose="none">\uline{...}</source>.
* To add a wavy underline, use <source lang="latex" enclose="none">\uwave{...}</source>.
* And for a strike-out <source lang="latex" enclose="none">\sout{...}</source>.

==== Sizing text ====

To apply different font sizes, simply follow the commands on this table:

[[File:Latex sizes table.png]]

Note that the font size definitions are set by the document class.  Depending on the document style the actual font size may differ from that listed above.  And not every document class has unique sizes for all 10 size commands.

{| class="wikitable"
|+ Absolute Point Sizes, [10pt] being default
!rowspan=2| size 
!colspan=3| standard classes, ''proc''
!colspan=3| AMS classes, ''memoir''
!rowspan=2| ''slides''
!colspan=3| ''beamer''
|-
! [10pt] !! [11pt] !! [12pt]
! [10pt] !! [11pt] !! [12pt]
! [10pt] !! [11pt] !! [12pt]
|-
|<tt>\tiny</tt>
|  6.80565 ||  7.33325    ||    7.33325
|  7.33325 ||  7.97224   ||  8.50012
| 17.27505 
|  5.31258 ||  6.37509   ||  6.37509
|-
|<tt>\scriptsize</tt>
|  7.97224 ||  8.50012   ||    8.50012
|  7.97224 ||  8.50012   ||   9.24994
| 20.73755 
|  7.43760 ||  8.50012   ||   8.50012
|-
|<tt>\footnotesize</tt>
|  8.50012 ||   9.24994  || 10.00002
|  8.50012 ||   9.24994  || 10.00002
| 20.73755  
|  8.50012  ||   9.24994  || 10.00002
|-
|<tt>\small</tt>
|  9.24994  || 10.00002 || 10.95003
|  9.24994  || 10.00002 || 10.95003
| 20.73755 
|  9.24994  || 10.00002 || 10.95003
|-
|<tt>\normalsize</tt>
| 10.00002 || 10.95003 || 11.74988
| 10.00002 || 10.95003 || 11.74988
| 24.88382 
| 10.00002 || 10.95003 || 11.74988
|-
|<tt>\large</tt>
| 11.74988 || 11.74988 || 14.09984
| 10.95003 || 11.74988 || 14.09984
| 29.86258 
| 11.74988 || 11.74988 || 14.09984
|-
|<tt>\Large</tt>
| 14.09984 || 14.09984 || 15.84985
| 11.74988 || 14.09984 || 15.84985
| 35.82510 
| 14.09984 || 14.09984 || 16.24988
|-
|<tt>\LARGE</tt>
| 15.84985 || 15.84985 || 19.02350
| 14.09984 || 15.84985 || 19.02350
| 43.00012 
| 16.24988 || 16.24988 || 19.50362
|-
|<tt>\huge</tt>
| 19.02350 || 19.02350  || 22.82086
| 15.84985 || 19.02350  || 22.82086
| 51.60014 
| 19.50362 || 19.50362  || 23.39682
|-
|<tt>\Huge</tt>
| 22.82086 || 22.82086 || 22.82086
| 19.02350 || 22.82086 || 22.82086
| 51.60014 
| 23.39682 || 23.39682 || 23.39682
|}

As a technical note, points in TeX follow the standard American point size in which 1 pt is approximately 0.3513_6 mm. The standard point size used in most modern computer programs (known as the ''desktop publishing point'' or ''PostScript point'') has 1 pt equal to approximately 0.352_7 mm while the standard European point size (known as the ''Didot point'') had 1 pt equal to approximately 0.37597151 mm. (See: [[w:Point_(typography)]].)

Even if you can easily change the output of your fonts using those commands, you're better off not using explicit commands like this, because they work in opposition to the basic idea of LaTeX, which is to separate the logical and visual markup of your document. This means that if you use the same font changing command in several places in order to typeset a special kind of information, you should use <source lang="latex" enclose="none">\newcommand</source> to define a "logical wrapper command" for the font changing command.

{|
|style="border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">
\newcommand{\oops}[1]{\textbf{#1}}

Do not \oops{enter} this room,
it’s occupied by \oops{machines}
of unknown origin and purpose.
</source>
|style="padding-left:3em;"|Do not '''enter''' this room, it’s occupied by '''machines''' of unknown origin and purpose.
|}

This approach has the advantage that you can decide at some later stage that you want to use some visual representation of danger other than <source lang="latex" enclose="none">\textbf</source>, without having to wade through your document, identifying all the occurrences of <source lang="latex" enclose="none">\textbf</source> and then figuring out for each one whether it was used for pointing out danger or for some other reason.

====Text mode superscript and subscript====

To superscript text in text-mode, you can use the <source lang="latex" enclose="none">\textsuperscript{}</source> command. This allows you to, for instance, typeset 6th as 6<sup>th</sup>:

<source lang="latex">
Michelangelo was born on March 6\textsuperscript{th}, 1475.
</source>

The primary use of subscripts within the text environment is to typeset chemical formulae. For this purposes, a highly recommended package is [http://www.ctan.org/tex-archive/macros/latex/contrib/mhchem/ mhchem]. This package is easy to use, and works with your text fonts (rather than math fonts). To insert a chemical formula, use <source lang="latex" enclose="none">\ce{}</source> with the text-equivalent formula, for example:
{|
|style="border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">
% In your preamble, add:
\usepackage[version=3]{mhchem}
...

% In your document:
Ammonium sulphate is \ce{(NH4)2SO4}.

</source>
|style="padding-left:3em;"|[[Image:Ammonium sulphate mhchem.png]]
|}


Subscripting in text-mode is not supported by LaTeX alone; however, several packages allow the use of the <source lang="latex" enclose="none">\textsubscript{}</source> command. For instance, [http://www.ctan.org/tex-archive/macros/latex/contrib/bpchem/ bpchem], [http://www.ctan.org/tex-archive/macros/latex/contrib/koma-script/ KOMA-Script2], and [http://tug.ctan.org/pkg/fixltx2e fixltx2e] all support this command.  Of these, [http://tug.ctan.org/pkg/fixltx2e fixltx2e] is perhaps the most universal option since it is distributed with LaTeX and requires no additional packages to be implemented. 

{|
|style="border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">
% In your preamble, add:
\usepackage{fixltx2e}
...

% In your document:
It is found that height\textsubscript{apple tree} is
different than height\textsubscript{orange tree}.
</source>
|style="padding-left:3em;"|<math>\text{It is found that height}_\text{apple tree}\text{ is different than height}_\text{orange tree}\text{.}\,</math>
|}

If you do not load a package that supports <source lang="latex" enclose="none">\textsubscript{}</source>, the math mode must be used. This is easily accomplished in running text by bracketing your text with the <source lang="latex" enclose="none">$</source> symbol. In math mode subscripting is done using the underscore: <source lang="latex" enclose="none">_{}</source>. 

For example, the formula for water is written as:

{|
|style="border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">
H$_2$O is the formula for water
</source>
|style="padding-left:3em;"|
<math>\text{H}_2\text{O is the formula for water}</math>
|}
See also the above mentioned package mhchem for chemical symbols and formulas.

Note that in math mode text will appear in a font suitable for mathematical variables. In math mode, to generate roman text, for example, one would use the <source lang="latex" enclose="none">\mathrm</source> command:

{|
|style="border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">
This is $\mathrm{normal\ roman\ and}_\mathrm{subscript\ roman}$ text
</source>
|style="padding-left:3em;"|<math>\text{This is }\mathrm{normal\ roman\ and}_\mathrm{subscript\ roman}\text{  text}</math>
|}
Note the use of \<space> to insert a space in math mode.

Similarly, you can superscript using:
{|
|style="border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">
This is $\mathrm{normal\ roman\ and}^\mathrm{superscript\ roman}$ text
</source>
|style="padding-left:3em;"|<math>\text{This is }\mathrm{normal\ roman\ and}^\mathrm{superscript\ roman}\text{  text}</math>
|}

====Text figures ("old style" numerals)====

Many typographers prefer to use titling figures, sometimes called lining figures, when numerals are interspersed with full caps, when they appear in tables, and when they appear in equations, using [[w:Text figures|text figures]] elsewhere.  LaTeX allows this usage through the <source lang="latex" enclose="none">\oldstylenums{}</source> command:

<source lang="latex">
\oldstylenums{1234567890}
</source>

Some fonts do not have text figures built in; the <tt>textcomp</tt> package attempts to remedy this by effectively generating text figures from the currently-selected font.  Put <source lang="latex" enclose="none">\usepackage{textcomp}</source> in your preamble.  <tt>textcomp</tt> also allows you to use decimal points, properly formatted dollar signs, etc. within <source lang="latex" enclose="none">\oldstylenums{}</source>.

One common use for text figures is in section, paragraph, and page numbers.  These can be set to use text figures by placing some code in your preamble:

<source lang="latex">
\usepackage{textcomp}

% Enclose everything in an \AtBeginDocument{}
\AtBeginDocument{%
  % Make \section{} use text figures
  \let\myTheSection\thesection
  \renewcommand{\thesection}{\oldstylenums{\myTheSection}}

  % Make \paragraph{} use text figures
  \let\myTheParagraph\theparagraph
  \renewcommand{\theparagraph}{\oldstylenums{\myTheParagraph}}

  % Make the page numbers in text figures
  \let\myThePage\thepage
  \renewcommand{\thepage}{\oldstylenums{\myThePage}}
}
</source>

Should you use additional sectioning or paragraphing commands, you may adapt the previous code listing to include them as well.

NOTE: A subsequent use of the <source lang="latex" enclose="none">\pagenumbering</source> command, e.g., <source lang="latex" enclose="none">\pagenumbering{arabic}</source>, will reset the <source lang="latex" enclose="none">\thepage</source> command back to the original. Thus, if you use the <source lang="latex" enclose="none">\pagenumbering</source> command in your document, be sure to reinstate your <source lang="latex" enclose="none">\myThePage definition</source> from the code above:

<source lang="latex">
...
\tableofcontents
\pagenumbering{roman}
\chapter{Preface}
...
\chapter{Introduction}
...
\pagenumbering{arabic}
\renewcommand{\thepage}{\oldstylenums{\myThePage}} % without this, the \thepage command will not be in oldstyle (e.g., in your Table of Contents}
\Chapter{Foo}
...
</source>

=== Symbols and special characters===

====Dashes and Hyphens====
LaTeX knows four kinds of dashes: a [[w:hyphen|hyphen]] (-), [[w:Dash#En dash|en dash]] (–), [[w:Dash#Em dash|em dash]] (—), or a [[w:Plus and minus signs#Minus sign|minus sign]] (−). You can access three of them with different numbers of consecutive dashes. The fourth sign is actually not a dash at all—it is the mathematical minus sign:

{|
|style="border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">
Hyphen: daughter-in-law, X-rated\\
En dash: pages 13--67\\
Em dash: yes---or no? \\
Minus sign: $0$, $1$ and $-1$
</source>
|style="padding-left:3em;"|[[Image:Latex dashes example.png|200px]]
|}

The names for these dashes are: ‘-’(-) hyphen , ‘--’(&ndash;) en-dash , ‘---’(&mdash;) em-dash  and ‘<math>-</math>’(&minus;) minus sign. They have different purposes:

[[Image:dashes.png]]

Use <source lang="latex" enclose="none">\hyp{}</source> macro from <tt>hyphenat</tt> package instead of hyphen if you want LaTeX to break compound words between lines.

====Euro <big>€</big> currency symbol====
When writing about money these days, you need the [[w:euro sign|euro sign]]. You have several choices. If the fonts you are using have a euro symbol and you want to use that one, first you have to load the ''textcomp'' package in the preamble:
<source lang="latex" enclose="none">
\usepackage{textcomp}
</source>
then you can insert the euro symbol with the command <source lang="latex" enclose="none">\texteuro</source>. If you want to use the official version of the euro symbol, then you have to use <tt>eurosym</tt>, load it with the <source lang="latex" enclose="none">official</source> option in the preamble:
<source lang="latex" enclose="none">
\usepackage[official]{eurosym}
</source>
then you can insert it with the <source lang="latex" enclose="none">\euro</source> command. Finally, if you want a euro symbol that matches with the current font style (e.g., bold, italics, etc.) but your current font does not provide it, you can use the <tt>eurosym</tt> package again but with a different option:
<source lang="latex" enclose="none">
\usepackage[gen]{eurosym}
</source>
again you can insert the euro symbol with <source lang="latex" enclose="none">\euro</source>

==== Ellipsis (…) ====

A sequence of three dots is known as an ''[[wikipedia:Ellipsis|ellipsis]]'', which is commonly used to indicate omitted text. On a typewriter, a comma or a period takes the same amount of space as any other letter. In book printing, these characters occupy only a little space and are set very close to the preceding letter. Therefore, you cannot enter ‘ellipsis’ by just typing three dots, as the spacing would be wrong. Instead, there is a special command for these dots. It is called <source lang="latex" enclose="none">\ldots</source>:

{|
|- 
|style="border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">
Not like this ... but like this:\\
New York, Tokyo, Budapest, \ldots
</source>
|style="padding-left:3em;"| [[Image:Latex example text dots.png|200px]]
|-
|}

Alternatively, you can use the <source lang="latex" enclose="none">\textellipsis</source> command which allows the spacing between the dots to vary.

====Ready-made strings====

There are some very simple LaTeX commands for typesetting special text strings:

[[Image:Latex ready-made strings.png|center|300px]]

====Other symbols====

LaTeX has ''lots'' of symbols at its disposal. The majority of them are within the mathematical domain, and later chapters will cover how to get access to them. For the more common text symbols, use the following commands:

[[Image:symbols.png]]

Not mentioned in above table, tilde (~) is used in LaTeX code to produce [[#The Space Between Words|non-breakable space]]. To get printed tilde sign, either make it [[#Verbatim Text|verbatim text]] or write <source lang="latex" enclose="none">\~{}</source>. 

Of course, these are rather boring, and it just so happens that for some more interesting symbols, the Postscript ZipfDingbats font is available thanks to the <tt>pifont</tt>. Hopefully, you are beginning to notice now that when you want to use a package, you need to add the declaration to your preamble, in this instance: <source lang="latex" enclose="none">\usepackage{pifont}</source>. Next, the command <source lang="latex" enclose="none">\ding{number}</source>, will print the specified symbol. Here is a table of the available symbols:

[[Image:LaTeX-dingbats.png|ZapfDingbats symbols]].

== Paragraph Formatting ==

Altering the paragraph formatting is not often required, especially in academic writing. However, it is useful to know, and applications tend to be for formatting text in floats, or other more exotic documents.

=== Paragraph Alignment ===

Paragraphs in Latex are usually fully justified (i.e., flush with both the left and right margins). For whatever reason, should you wish to alter the justification of a paragraph, there are three environments at hand, and also Latex command equivalents.

{| class="wikitable"
! Alignment
! Environment
! Command
|-
| Left justified
| <tt>flushleft</tt>
| <tt>\raggedright</tt>
|-
| Right justified
| <tt>flushright</tt>
| <tt>\raggedleft</tt>
|-
| Center
| <tt>center</tt>
| <tt>\centering</tt>
|}

All text between the <source lang="latex" enclose="none">\begin</source> and <source lang="latex" enclose="none">\end</source> of the specified environment will be justified appropriately. The commands listed are for use within other environments. For example, <source lang="latex" enclose="none">p</source> (paragraph) columns in <source lang="latex" enclose="none">tabular</source>.

=== Paragraph Indents ===

By default, the first paragraph after a heading follows the standard Anglo-American publishers' practice of no indentation. The size of subsequent paragraph indents are determined by a parameter called <source lang="latex" enclose="none">\parindent</source>. The default length that this constant holds is set by the document class that you use. It is possible to override using the <source lang="latex" enclose="none">\setlength</source> command.

<source lang="latex">
\setlength{\parindent}{1cm}
</source>
This will set paragraph indents to 1cm.

Be careful, however, if you decide to set the indent to zero, then it means you will need a vertical space between paragraphs in order to make them clear. The space between paragraphs is held in <source lang="latex" enclose="none">\parskip</source>, which could be altered in a similar fashion as above. However, this parameter is used elsewhere too, such as in lists, which means you run the risk of making various parts of your document look very untidy by changing this setting. If you want to use the style of having no indentation with a space between paragraphs, use the <tt>parskip</tt> package, which does this for you, while making adjustments to the spacing of lists and other structures which use paragraph spacing, so they don't get too far apart. Add this to the preamble:

<source lang="latex">
\usepackage{parskip}
</source>

To indent subsequent lines of a paragraph, use the TeX command <source lang="latex" enclose="none">\hangindent</source>.  (While the default behaviour is to apply the hanging indent after the first line, this may be changed with the <source lang="latex" enclose="none">\hangafter</source> command.)  An example follows.

<source lang="latex">
\hangindent=0.7cm This paragraph has an extra indentation at the left.
</source>

The TeX commands <source lang="latex" enclose="none">\leftskip</source> and <source lang="latex" enclose="none">\rightskip</source> add additional space to the left and right sides of each line, allowing the formatting for subsequent paragraphs to differ from the overall document margins.  This space is in addition to the indentation added by <source lang="latex" enclose="none">\parindent</source> and <source lang="latex" enclose="none">\hangindent</source>.

To change the indentation of the last line in a paragraph, use the TeX command <source lang="latex" enclose="none">\parfillskip</source>.

White-space in LaTeX can also be made flexible (what Lamport calls "rubber" lengths). This means that values such as <source lang="latex" enclose="none">\parskip</source> can have a default dimension plus an amount of expansion minus an amount of contraction. This is useful on pages in complex documents where not every page may be an exact number of fixed-height lines long, so some give-and-take in vertical space is useful. You specify this in a <source lang="latex" enclose="none">\setlength</source> command like this:

<source lang="latex" enclose="none">
\setlength{\parskip}{1cm plus4mm minus3mm}
</source>

=== Line Spacing ===
To change line spacing in the whole document use the command <source lang="latex" enclose="none">\linespread</source> covered in [[LaTeX/Customizing_LaTeX#Spacing]]. 

To change line spacing in specific environments do the following:
# Add <source lang="latex" enclose="none">\usepackage{setspace}</source> to the document preamble.
# This then provides the following environments to use within your document: 
#* <source lang="latex" enclose="none">doublespace</source> - all lines are double spaced.  
#* <source lang="latex" enclose="none">onehalfspace</source> - line spacing set to one-and-half spacing.
#* <source lang="latex" enclose="none">singlespace</source> - normal line spacing.

After declaring the package in the preamble the use of the command <source lang="latex" enclose="none">\singlespacing</source>, <source lang="latex" enclose="none">\doublespacing</source>, or <source lang="latex" enclose="none">\onehalfspacing</source> will specify the line spacing for all sections and paragraphs until another command is used.

See the section on [[LaTeX/List_Structures#Customizing_Lists | customizing lists]] below for information on how to change the line spacing in lists.

== Special Paragraphs ==

For those of you who have read most/all of the tutorials so far, you will have already come across some of the following paragraph formats. Although seen before, it makes sense to re-introduce here, for the sake of completeness.

=== <tt>Verbatim</tt> Text ===

There are several ways to introduce text that won't be interpreted by the compiler. If you use the <source lang="latex" enclose="none">verbatim</source> environment, everything input between the ''begin'' and ''end'' commands are processed as if by a typewriter. All spaces and new lines are reproduced as given, and the text is displayed in an appropriate fixed-width font. Any LaTeX command will be ignored and handled as plain text. Ideal for typesetting program source code. This environment was used in an example in the previous tutorial. Here is an example:

{|
| style="vertical-align: middle;border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">
\begin{verbatim}
The verbatim environment
  simply reproduces every
 character you input,
including all  s p a c e s!
\end{verbatim}
</source>
| style="vertical-align: middle;padding-left:3em;" |
[[Image:verbatim.png]]
|}

Note: once in the <source lang="latex" enclose="none">verbatim</source> environment, the only command that will be recognized is <source lang="latex" enclose="none">\end{verbatim}</source>. Any others will be output. The font size in the verbatim environment can be adjusted by placing a [[#Sizing_text|font size command]] before <source lang="latex" enclose="none">\begin{verbatim}</source>. If this is an issue, then you can use the <tt>alltt</tt> package instead, providing an environment with the same name:

{|
| style="vertical-align: middle;border-right:2px black solid;padding-right:3em;" |
<source lang="latex" enclose="none">
\begin{alltt}
Verbatim extended with the ability
to use normal commands.  Therefore, it
is possible to \emph{emphasize} words in
this environment, for example.
\end{alltt}
</source>
| style="vertical-align: middle;padding-left:3em;" |
[[Image:alltt.png]]
|}

Remember to add <source lang="latex" enclose="none">\usepackage{alltt}</source> to your preamble to use it though! 
Within the <source lang="latex" enclose="none">alltt</source> environment, you can use the command <source lang="latex" enclose="none">\normalfont</source> to get back the normal font.
To write equations within the <source lang="latex" enclose="none">alltt</source> enviroment, you can use <source lang="latex" enclose="none">\(</source> and <source lang="latex" enclose="none">\)</source> to enclose them, instead of the usual<source lang="latex" enclose="none">$</source>.

When using <source lang="latex" enclose="none">\textbf{}</source> inside the <source lang="latex" enclose="none">alltt</source> enviroment, note that the standard font has no bold TT font. Txtfonts has bold fonts: just add <source lang="latex" enclose="none">\renewcommand{\ttdefault}{txtt}</source> after <source lang="latex" enclose="none">\usepackage{alltt}</source>.

If you just want to introduce a short verbatim phrase, you don't need to use the whole environment, but you have the <source lang="latex" enclose="none">\verb</source> command:

<source lang="latex">
\verb+my text+
</source>

The first character following <source lang="latex" enclose="none">\verb</source> is the delimiter: here we have used "+", but you can use any character you like but * and space; <source lang="latex" enclose="none">\verb</source> will print verbatim all the text after it until it finds the next delimiter. For example, the code:

<source lang="latex">
\verb|\textbf{Hi mate!}|
</source>
will print <source lang="latex" enclose="none">\textbf{Hi mate!}</source>, ignoring the effect <source lang="latex" enclose="none">\textbf</source> should have on text.

For more control over formatting, however, you can try the <tt>fancyvrb</tt> package, which provides a <source lang="latex" enclose="none">Verbatim</source> environment (note the capital letter) which lets you draw a rule round the verbatim text, change the font size, and even have typographic effects inside the <source lang="latex" enclose="none">Verbatim</source> environment. It can also be used in conjunction with the <tt>fancybox</tt> package and it can add reference line numbers (useful for chunks of data or programming), and it can even include entire external files.

====Typesetting URLs====

One of either the <tt>hyperref</tt> or <tt>url</tt> packages provides the <source lang="latex" enclose="none">\url</source> command, which properly typesets URLs, for example:

<source lang=latex>
Go to \url{http://www.uni.edu/~myname/best-website-ever.html} for my website.
</source>

will show this URL exactly as typed (similar to the <source lang="latex" enclose="none">\verb</source> command), but the <source lang="latex" enclose="none">\url</source> command also performs a hyphenless break at punctuation characters (only in pdflatex, not in plain latex + dvips). It was designed for Web URLs, so it understands their syntax and will never break mid-way through an unpunctuated word, only at slashes and full stops. Bear in mind, however, that spaces are forbidden in URLs, so using spaces in <source lang="latex" enclose="none">\url</source> arguments will fail, as will using other non-URL-valid characters.

When using this command through the <tt>hyperref</tt> package, the URL is "clickable" in the PDF document, whereas it is not linked to the web when using only the <tt>url</tt> package. Also when using the <tt>hyperref</tt> package, to remove the border placed around a URL, insert <source lang="latex" enclose="none">pdfborder = {0 0 0 0}</source> inside the <source lang="latex" enclose="none">\hypersetup{}</source>.

==== <tt>Listing</tt> Environment ====

This is also an extension of the verbatim environment provided by the <tt>moreverb</tt> package. The extra functionality it provides is that it can add line numbers along side the text. The command: <source lang="latex" enclose="none">\begin{listing}[step]{first line}</source>. The mandatory ''first line'' argument is for specifying which line the numbering shall commence. The optional ''step'' is the step between numbered lines (the default is 1, which means every line will be numbered).

To use this environment, remember to add <source lang="latex" enclose="none">\usepackage{moreverb}</source> to the document preamble.

===Multi-line comments===

As we have seen, the only way LaTeX allows you to add comments is by using the special character {{LaTeX/LaTeX|code=%}}, that will comment out all the rest of the line after itself. This approach is really time-consuming if you want to insert long comments or just comment out a part of your document that you want to improve later. Using the {{LaTeX/Package|verbatim}} package, to be loaded in the preamble as usual:
{{LaTeX/Usage|code=
\usepackage{verbatim} 
}}

you can use an environment called {{LaTeX/Environment|comment}} that will comment out everything within itself. Here is an example:

{{LaTeX/Example|code=
This is another
\begin{comment}
rather stupid,
but helpful
\end{comment}
example for embedding
comments in your document.
|render=
This is another example for embedding comments in your document.
}}

Note that this won’t work inside complex environments, like math for example. You may be wondering, why should I load a package called {{LaTeX/Package|verbatim}} to have the possibility to add comments? The answer is straightforward: commented text is interpreted by the compiler just like verbatim text, the only difference is that verbatim text is introduced within the document, while the comment is just dropped.

Alternatively, you can define a {{LaTeX/LaTeX|code=\comment{} }} command, by adding the following to the document's preamble: 
{{LaTeX/Usage|code=
\newcommand{\comment}[1]{}
}}

Then, to comment out text, simply do something like this:
{{LaTeX/Example|code=
\comment{ This is a long comment and can extend over multiple lines, etc.  }
|render=<code> </code>
}}

=== Quoting text===

LaTeX provides several environments for quoting text; they have small differences and they are aimed for different types of quotations. All of them are indented on either margin, and you will need to add your own quotation marks if you want them. The provided environments are:
;<source lang="latex" enclose="none">quote</source>
:for a short quotation, or a series of small quotes, separated by blank lines.
;<source lang="latex" enclose="none">quotation</source>
:for use with longer quotations, of more than one paragraph, because it indents the first line of each paragraph.
;<source lang="latex" enclose="none">verse</source>
:is for quotations where line breaks are important, such as poetry. Once in, new stanzas are created with a blank line, and new lines within a stanza are indicated using the newline command, <source lang="latex" enclose="none">\\</source>. If a line takes up more than one line on the page, then all subsequent lines are indented until explicitly separated with <source lang="latex" enclose="none">\\</source>.

=== Abstracts ===

In scientific publications it is customary to start with an abstract which gives the reader a quick overview of what to expect. LaTeX provides the <source lang="latex" enclose="none">abstract</source> environment for this purpose. It is available in <source lang="latex" enclose="none">article</source> and <source lang="latex" enclose="none">report</source> document classes; it's not available in the <source lang="latex" enclose="none">book</source>, but it's quite simple to create your own if you really need it.


== Footnotes ==

Footnotes are a very useful way of providing extra information to the reader. Usually, it is non-essential information which can be placed at the bottom of the page. This keeps the main body of text concise.

The footnote facility is easy to use. The command you need is: <source lang="latex" enclose="none">\footnote{text}</source>. Do not leave a space between the command and the word where you wish the footnote marker to appear, otherwise Latex will process that space and will leave the output not looking as intended.

{|
| style="vertical-align: middle;border-right:2px black solid;padding-right:3em;"|
<source lang="latex" enclose="none">
Creating a footnote is easy.\footnote{An example footnote.}
</source>
|style="padding-left:3em;" |
[[Image:LaTeX-footnote.png]]
|}

Latex will obviously take care of typesetting the footnote at the bottom of the page. Each footnote is numbered sequentially - a process that, as you should have guessed by now, is automatically done for you.

It is possible to customize the footnote marking. By default, they are numbered sequentially (Arabic). However, without going too much into the mechanics of Latex at this point, it is possible to change this using the following command (which needs to be placed at the beginning of the document, or at least before the first footnote command is issued).

{|
| <source lang="latex" enclose="none">\renewcommand{\thefootnote}{\arabic{footnote}}</source>
| Arabic numerals, e.g., 1, 2, 3...
|-
| <source lang="latex" enclose="none">\renewcommand{\thefootnote}{\roman{footnote}}</source>
| Roman numerals (lowercase), e.g., i, ii, iii...
|-
| <source lang="latex" enclose="none">\renewcommand{\thefootnote}{\Roman{footnote}}</source>
| Roman numerals (uppercase), e.g., I, II, III...
|-
| <source lang="latex" enclose="none">\renewcommand{\thefootnote}{\alph{footnote}}</source>
| Alphabetic (lowercase), e.g., a, b, c...
|-
| <source lang="latex" enclose="none">\renewcommand{\thefootnote}{\Alph{footnote}}</source>
| Alphabetic (uppercase), e.g., A, B, C...
|-
| <source lang="latex" enclose="none">\renewcommand{\thefootnote}{\fnsymbol{footnote}}</source>
| A sequence of nine symbols (try it and see!)
|}

The package [http://www.ctan.org/tex-archive/help/Catalogue/entries/footmisc.html footmisc] offers many possibilities for customizing the appearance of footnotes. It can be used, for example, to use a different font within footnotes.

=== Common problems and workarounds ===
* Footnotes unfortunately don't work with tables, as it is considered a bad practice.  You can overcome this limitation with several techniques: you can use <source lang="latex" enclose="none">\footnotemark[123]</source> in the table, and <source lang="latex" enclose="none">\footnotetext[123]{HelloWorld!}</source> somewhere on the page.  Or, you can add <source lang="latex" enclose="none">\usepackage{footnote}</source> and <source lang="latex" enclose="none">\makesavenoteenv{tabular}</source> to the preamble, and put your ''table'' environment in a <source lang="latex" enclose="none">\begin{savenotes}</source> environment.  Note that the latter does not work with the packages ''color'' or ''colortbl''. See [http://www.tex.ac.uk/cgi-bin/texfaq2html?label=footintab this FAQ page] for other approaches.

* Footnotes also don't work inside minipage environment (In fact, several environments break footnote support. the <source lang="latex" enclose="none">\makesavenoteenv{environmentname}</source> command of the footnote package might fix most). The minipage includes its own footnotes, independent of the document's. The package [http://www.cs.brown.edu/system/software/latex/doc/mpfnmark.pdf mpfnmark] allows greater flexibility in managing these two sets of footnotes.

* If the text within the footnote is very long, LaTeX may split the footnote over several pages. You can prevent LaTeX from doing so by increasing the penalty for such an operation. To do this, insert the following line into the preamble of your document:
:<source lang="latex" enclose="none">\interfootnotelinepenalty=10000</source>

* To make multiple references to the same footnote, you can use the following syntax:
:<source lang="latex" enclose="none">Text that has a footnote\footnote{This is the footnote} looks like this. Later text referring to same footnote\footnotemark[\value{footnote}] uses the other command.</source>
:If you need hyperref support, use instead:
:<source lang="latex" enclose="none">Text that has a footnote\footnote{This is the footnote}\addtocounter{footnote}{-1}\addtocounter{Hfootnote}{-1} looks like this. Later text referring to same footnote\footnotemark uses the other command.</source>
:Note that these approaches don't work if there are other footnotes between the first reference and any of the other "duplicates".

== Margin Notes ==
[[Image:LaTeX marginpar.png|thumb|250px|right|a margin note]]
Margin Notes are useful during the editorial process, to exchange comments among authors. To insert a margin note use <source lang="latex" enclose="none">\marginpar{margin text}</source>. For one-sided layout (simplex), the text will be placed in the right margin, starting from the line where it is defined. For two-sided layout (duplex), it will be placed in the outside margin and for two-column layout it will be placed in the nearest margin.

To swap the default side, use <source lang="latex" enclose="none">\reversemarginpar</source> and margin notes will then be placed on the opposite side, which would be the inside margin for two-sided layout.

If the text of your marginpar depends on which margin it is put in (say it includes an arrow pointing at the text or refers to a direction as in "as seen to the left..."), you can use <source lang="latex" enclose="none">\marginpar[left text]{right text}</source> to specify the variants. 

To insert a margin note in an area that <source lang="latex" enclose="none">\marginpar</source> can't handle, such as footnotes or equation environments, use the package <tt>marginnote</tt>.

Also see the package <tt>mparhack</tt>.

== Summary ==

Phew! What a busy tutorial! A lot of material was covered here, mainly because formatting is such a broad topic. Latex is so flexible that we actually only skimmed the surface, as you can have much more control over the presentation of your document if you wish. Having said that, one of the purposes of Latex is to take away the stress of having to deal with the physical presentation yourself, so you need not get too carried away!

{{A-Roberts}}

<noinclude>
{{LaTeX/Bottom|Floats, Figures and Captions|Page Layout}}
</noinclude>

[[pl:LaTeX/Formatowanie]]
