><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

== Accents and Special Characters ==

The rules differ somewhat depending whether you are in text mode, math mode, or the tabbing environment

== Text mode ==

The Unicode character encoding UTF8 includes several special characters and characters with accents. Following code specifies that the encoding of the LaTeX document source file is UTF8. As font encoding is specified T1, because it supports the encoding of extended character sets in fonts.

<pre>
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
</pre>

Of course, the encoding in the text editor needs to be set to utf8, as well. Depending on the used UTF8 characters, one needs to specify a font that actually includes them (and supports the T1 fontenconding). E.g. for german related special characters:

<pre>
\usepackage{lmodern}
</pre>

With XeTeX and LuaTeX the inputenc and fontenc package are no longer needed and replaced by the fontspec package. Both engines support UTF-8 directly and allow the use of TTF and OpenType fonts to support unicode characters. See the [[LaTeX/Fonts#XeTeX|XeTeX section]] in the Fonts chapter of this book for more information.

In addition to that, LaTeX supports the composition of special characters. This works without the UTF8 input encoding.

The following accents may be placed on letters. Although "<code>o</code>" is used in most of the example, the accents may be placed on any letter. Accents may even be placed above a "missing" letter; for example, <code>\~{}</code> produces a tilde over a blank space.

The following commands may be used only in paragraph or LR mode.
{|class="wikitable"
! LaTeX command
! Sample
! Description
|-
|<code>\`{o}</code> || ò || grave accent
|-
|<code>\'{o}</code> || ó || acute accent
|-
|<code>\^{o}</code> || ô || circumflex
|-
|<code>\"{o}</code> || ö || umlaut or dieresis
|-
|<code>\H{o}</code> || ő || long Hungarian umlaut (double acute)
|-
|<code>\~{o}</code> || õ || tilde
|-
|<code>\c{c}</code> || ç || cedilla
|-
|<code>\k{a}</code> || ą || ogonek
|-
|<code>\l</code> || ł || l with stroke
|-
|<code>\={o}</code> || ō || macron accent (a bar over the letter)
|-
|<code>\b{o}</code> || <u>o</u> || bar under the letter
|-
|<code>\.{o}</code> || &#559; || dot over the letter
|-
|<code>\d{u}</code> || u&#803; || dot under the letter
|-
|<code>\r{a}</code> || å || ring over the letter
|-
|<code>\u{o}</code> || ŏ || breve over the letter
|-
|<code>\v{s}</code> || š || caron/hacek ("v") over the letter
|-
|<code>\t{oo}</code> || o&#865;o || "tie" (inverted u) over the two letters 
|}

Note that the letters "i" and "j" require special treatment when they are given accents because it is often desirable to replace the dot with the accent. For this purpose, the commands <code>\i</code> and <code>\j</code> can be used to produce dotless letters.

For example,

* <code>\^{\i}</code> should be used for i, circumflex, î
* <code>\"{\i}</code> should be used for i, umlaut, ï

For umlauts, the babel package, configure like in the following code,

<pre>
\usepackage[ngerman]{babel}
</pre>

provides the short hand <code>"o</code> for <code>\"{o}</code>.

== Math mode ==

Several of the above and some similar accents can also be produced in math mode. The following commands may be used only in math mode.

{|class="wikitable"
! LaTeX command
! Sample
! Description
! Text-mode equivalence
|-
|<code>\hat{o}</code>
|<math>\hat{o}</math>
|circumflex
|<code>\^</code>
|-
|<code>\widehat{oo}</code>
|<math>\widehat{oo}</math>
|wide version of <code>\hat</code> over several letters
|
|-
|<code>\check{o}</code>
|<math>\check{o}</math>
|vee or check
|<code>\v</code>
|-
|<code>\tilde{o}</code>
|<math>\tilde{o}</math>
|tilde
|<code>\~</code>
|-
|<code>\widetilde{oo}</code>
|<!-- not in texvc! -->
|wide version of <code>\tilde</code> over several letters
|
|-
|<code>\acute{o}</code>
|<math>\acute{o}</math>
|acute accent
|<code>\'</code>
|-
|<code>\grave{o}</code>
|<math>\grave{o}</math>
|grave accent
|<code>\`</code>
|-
|<code>\dot{o}</code>
|<math>\dot{o}</math>
|dot over the letter
|<code>\.</code>
|-
|<code>\ddot{o}</code>
|<math>\ddot{o}</math>
|two dots over the letter (umlaut in text-mode)
|<code>\"</code>
|-
|<code>\breve{o}</code>
|<math>\breve{o}</math>
|breve
|<code>\u</code>
|-
|<code>\bar{o}</code>
|<math>\bar{o}</math>
|macron
|<code>\=</code>
|-
|<code>\vec{o}</code>
|<math>\vec{o}</math>
|vector (arrow) over the letter
|
|}

== Tabbing environment ==

Some of the accent marks used in running text have other uses in the tabbing environment. In that case they can be created with the following command:

* <code>\a'</code> for an acute accent
* <code>\a`</code> for a grave accent
* <code>\a=</code> for a macron accent 

== Related issues ==

The actual entering of special characters is system dependent. For example under X-WIndows umlauts are entered via <code>compose+caps+" o</code>, when the keyboard layout does not directly includes them.

Whether LaTeX is actually able to typeset certain UTF8 characters depends on the loaded packages. For example trying to typeset the euro sign (€) may yield following error message:

<pre>
Package inputenc Error: Unicode char \u8:€ not set up for use with LaTeX.
</pre>

Using the package <code>textcomp</code> sets this and other characters up.

The usage of the T1 font encoding influences the usability of pdf output (generated via pdflatex). For example without specifying T1, extracting the umlaut <code>Ä</code> via a PDF viewer actually extracts the two characters  <code>"A</code>. Analog to that, a PDF viewer cannot find words with umlauts in a PDF document which was generated via pdflatex without the T1 font enconding. With T1 font enconding (and even with composed special characters) the PDF contains the correct text information.

The package <code>ae</code> (almost european) is obsolete. It provided some workarounds for hyphenation of words with special characters. These are not necessary any more with fonts like lmodern. Using the ae package leads to text encoding problems in PDF files generated via <code>pdflatex</code> (e.g. text extraction and searching), besides typographic issues.

== External links ==

* [http://spectroscopy.mps.ohio-state.edu/symposium_53/latexinstruct.html A few other LaTeX accents and symbols]
* [http://www.giss.nasa.gov/tools/latex/ltx-401.html NASA GISS: Accents]

<noinclude>
{{LaTeX/Bottom|Internationalization|Useful Measurement Macros}}
</noinclude>
