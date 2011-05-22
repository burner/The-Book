><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

When you write documents in languages other than English, areas where LaTeX has to be configured appropriately:

# LaTeX needs to know how to hyphenate the language(s) you are using.
# You need to use language-specific typographic rules. In French for example, there is a mandatory space before each colon character (:).
# You want to be able to insert all the language-specific special characters directly from your keyboard instead of using cumbersome coding (for example, type <tt>ä</tt> instead of <tt>\"{a}</tt>).

If you simply need to add a few words from another language, you may find [[LaTeX/Accents]] an easier way.

== Hyphenating ==

The <tt>babel</tt> package by Johannes Braams will take care of everything. You can use it loading in your preamble, providing as an argument the language you want to use:
<source lang="latex">
\usepackage[language]{babel}
</source>

You should place it soon after the <tt>\documentclass</tt> command, so that all the other packages you load afterwards will know the language you are using. A list of the languages built into your LaTeX system will be displayed every time the compiler is started. Babel will automatically activate the appropriate hyphenation rules for the language you choose. If your LaTeX format does not support hyphenation in the language of your choice, babel will still work but will disable hyphenation, which has quite a negative effect on the appearance of the typeset document. Babel also specifies new commands for some languages, which simplify the input of special characters. See the sections about languages below for more information.

If you call babel with multiple languages:
<source lang="latex">
\usepackage[languageA,languageB]{babel}
</source>

then the last language in the option list will be active (i.e. languageB), and you can use the command
<source lang="latex">
\selectlanguage{languageA}
</source>
to change the active language. You can also add short pieces of text in another language using the command
<source lang="latex">
\foreignlanguage{languageB}{Text in another language}
</source>

Babel also offers various environments to enter larger pieces of text in another language:
<source lang="latex">
\begin{otherlanguage}{languageB}
Text in language B. This environment switches all language-related definitions, like the language specific names for figures, tables etc. to the other language.
\end{otherlanguage}
</source>
The starred version of this environment only does the typesetting according to the rules of the other language, but keeps the language specific string for thing like figures in the main language of the document. The environment <tt>hyphenrules</tt> only switches the hyphenation patterns used, and can be used to disallow hyphenation by using the language name 'nohyphenation'.

== Text encoding ==

Most of the modern computer systems allow you to input letter of national alphabets directly from the keyboard. In order to handle variety of input encoding used for different groups of languages and/or on different computer platforms LaTeX employs the <tt>inputenc</tt> package:
<source lang="latex">
\usepackage[encoding]{inputenc}
</source>

<tt>inputenc</tt> package tells LaTeX what the text encoding format of your .tex files is. The encoding depends on your operating system but a software's encoding can oftenly be changed from the settings (this happens at least with some editors, the [[w:PuTTY| PuTTY]] terminal and [[w:Texmaker| TeXmaker]]). 
You may choose whichever encoding you like, but you must say so in the preamble, so for example, if you prefere to use the ISO-8859-1, write 
<source lang="latex">
\usepackage[latin1]{inputenc}
</source>


Most modern operating systems use Unicode (utf-8) as a default encoding for text. On such system (for example Ubuntu) you can use:
<source lang="latex">
\usepackage[utf8]{inputenc}
</source>



The supported encoding by the LaTeX team is <tt>utf8</tt> and covers a fairly specific/limited range of unicode input characters. It only defines those symbols that are known to be available with the current ''font encoding''. <tt>utf8x</tt> is not officially supported, but covers a much broader range of input symbols.

You might encounter a situation where using <tt>\usepackage[utf8]{inputenc}</tt> might result in error:

<source lang="latex">
! Package inputenc Error: Unicode char \u8:ũ not set up for use with LaTeX.
</source>

This is due to the utf8 definition not necessarily having a mapping of all the character glyphs you are able to enter on your keyboard. Such characters are for example '''ŷ Ŷ ũ Ũ ẽ Ẽ ĩ Ĩ'''. In such case, you need to use the <tt>utf8x</tt> option to define more character combinations. This might break up compatibility with some packages like <tt>csquotes</tt>.

When using the <tt>inputenc</tt> package, you should consider that other people might not be able to display your input files on their computer, because they use a different encoding. For example, the German umlaut ä on OS/2 is encoded as 132, on Unix systems using ISO-LATIN 1 it is encoded as 228, while in Cyrillic encoding cp1251 for Windows this letter does not exist at all; therefore you should use this feature with care. The following encodings may come in handy, depending on the type of system you are working on:

{| class="wikitable"
!rowspan="2"|Operating system
!colspan="2"|Encodings
|-
|Western Latin
|Cyrillic
|-
|Mac
|<tt>applemac</tt>
|<tt>maccyr</tt>
|-
|Unix
|<tt>latin1</tt>
|<tt>koi8-ru</tt>
|-
|Windows
|<tt>ansinew</tt>
|<tt>cp1251</tt>
|-
|DOS, OS/2
|<tt>cp850</tt>
|<tt>cp866nav</tt>
|-
|}

== Output encoding ==

<tt>fontenc</tt> package tells LaTeX how to output the text you have produced. It defines at which position inside a TeX-font each letter is stored. Multiple input encodings could be mapped into one font encoding, which reduces number of required font sets. LaTeX can produce either bitmap-fonts (usually rasterized to 300 or 600 ppi) or scalable vector fonts (such as Type 1 fonts). There are many different font sets available containing different sets of glyphs (characters).

Font encoding is set with:
<source lang="latex">
\usepackage[encoding]{fontenc}
</source>
where encoding is font encoding. It is possible to load several encodings simultaneously.

The default LaTeX font encoding is OT1, the encoding of the original Computer Modern TeX text fonts. It contains only 128 characters, many from ASCII, but leaving out some others and including a number that are not in ASCII. When accented characters are required, TeX creates them by combining a normal character with an accent. While the resulting output looks perfect, this approach stops the automatic hyphenation from working inside words containing accented characters. Besides, some of Latin letters could not be created by combining a normal character with an accent, to say nothing about letters of non-Latin alphabets, such as Greek or Cyrillic.

To overcome these shortcomings, several 8-bit CM-like font sets were created. ''Extended Cork'' (EC) fonts in T1 encoding contains letters and punctuation characters for ''most of the European languages'' based on Latin script. The LH font set contains letters necessary to typeset documents in languages using Cyrillic script. Because of the large number of Cyrillic glyphs, they are arranged into four font encodings—T2A, T2B, T2C, and X2. The CB bundle contains fonts in LGR encoding for the composition of Greek text. By using these fonts you can improve/enable hyphenation in non-English documents. Another advantage of using new CM-like fonts is that they provide fonts of CM families in all weights, shapes, and optically scaled font sizes

Here is a collection of suggestions about writing a LaTeX document in a language other than English. If you have experience in a language not listed below, please add some notes about it.

<!-- please keep the list in alphabetical order-->

== Hyphenating languages ==

===Arabic script===

For languages which use the Arabic script, including Arabic, Persian, Urdu, Pashto, Kurdish, Uyghur, etc., add the following code to your preamble:

<source lang="latex">
\usepackage{arabtex}
</source>

You can input text in either romanized characters or native Arabic script encodings.  Use any of the following commands/environment to enter in text:

<source lang="latex">
\< … >
\RL{ … }
\begin{arabtext} … \end{arabtext}.
</source>

See the [[w:ArabTeX|ArabTeX]] Wikipedia article for further details.

You may also use the Arabi package within babel to typeset Arabic and Persian

<source lang="latex">
\usepackage[LAE,LFE]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage[farsi,arabic]{babel}
</source>

You may use Arabi with Lyx, or with tex4ht to produce HTML. You may also copy and paste from PDF files produced with Arabi thanks to the support of the cmap package.

See [http://tug.ctan.org/tex-archive/language/arabic/arabi/ Arabi page on CTAN]


For Persian language, there is a dedicated package called XePersian which use XeLaTeX as the typesetting engine. Just add the following code to your preamble:

<source lang="latex">
\usepackage{xepersian}
</source>

See [http://tug.ctan.org/tex-archive/macros/xetex/latex/xepersian/ XePersian page on CTAN]

===Cyrillic script===

Please add the section "Writing in Cyrillic" from http://www.ctan.org/tex-archive/info/lshort/english/lshort.pdf . You are allowed to copy it.
 
See also the Bulgarian translation of the "Not so Short Introduction to LaTeX 2e” from http://www.ctan.org/tex-archive/info/lshort/bulgarian/lshort-bg.pdf

This enables you to type cyrillic letters directly via your keyboard, but with a different distribution than a standard cyrillic keyboard!
To get the standard distribution, only include:
\usepackage[OT1]{fontenc}
\usepackage[russian]{babel}

=== Czech ===

Czech is fine using
<source lang="latex">
\usepackage[czech]{babel}
\usepackage[T1]{fontenc}
\usepackage[utf8x]{inputenc}
</source>
You may use different encoding, but UTF-8 is becoming standard and it allows you to have „czech quotation marks“ directly in your text. Otherwise, there are macros ''\glqq'' and ''\grqq'' to produce left and right quote.

=== Slovak ===

Basic settings are fine when left the same as Czech, but Slovak needs special signs for ď,ť,ľ. To be able to type them from keyboard use the following settings 
<source lang="latex">
\usepackage[slovak]{babel}
\usepackage[IL2]{fontenc}
\usepackage[utf8]{inputenc}
</source>

=== Finnish ===

Finnish language hyphenation is enabled with:
<source lang="latex">
\usepackage[finnish]{babel}
</source>
This will also automatically change document language (section names, etc.) to Finnish.

Remember to use Unicode encoding for Finnish if you're using an editor in utf8 mode:
<source lang="latex">
\usepackage[utf8]{inputenc}
</source>
The default encoding system can oftenly be changed regardless of the [[w:Operating system| operating system]] (this happens at least with some editors, the [[w:PuTTY| PuTTY]] terminal and [[w:Texmaker| TeXmaker]]). You may choose whichever encoding you like, but you must say so in the preamble, so for example, if you prefere to use the ISO-8859-1, write instead
<source lang="latex">
\usepackage[latin1]{inputenc}
</source>
The encoding is important and makes sure you can simply write the ''ääkköset'' ö ä å Ö Ä Å as such, which is less cumbersome than having to write 
<source lang="latex">
\"{a}</source>
to get the letter ä. Actually letters like õ ô ó Ò ñ work as well so the same idea applies also to other European languages like [[#Spanish|Spanish]].

If you want to use ''European Computer Modern'' fonts, you should use:
<source lang="latex">
\usepackage[T1]{fontenc}
</source>

For creating scalable (vector) Type1 fonts instead of bitmapped fonts, you can substitute the line above with:
<source lang="latex">
\usepackage{ae}
</source>
The ae-package changes encoding to T1 and also loads the scalable version of ''Almost European Computer Modern'' fonts.



=== French ===

Some hints for those creating French documents with LaTeX: you can load
French language support with the following command:
<source lang="latex">
\usepackage[frenchb]{babel}
</source>

There are multiple options for typesetting French documents, depending on the flavor of French: <tt>french</tt>, <tt>frenchb</tt>, and <tt>francais</tt> for Parisian French, and <tt>acadian</tt> and <tt>canadien</tt> for new-world French.
All enable French hyphenation, if you have configured your LaTeX system accordingly. All of these also change all automatic text into French: <tt>\chapter</tt> prints ''Chapitre'', <tt>\today</tt> prints the current date in French and so on. A set of new commands also becomes available, which allows you to write French input files more easily. Check out the following table for inspiration:

{| class="wikitable"
!input code
!rendered output
|-
|<tt>\og guillemets \fg{}</tt>
|« guillemets »
|-
|<tt>M\up{me}, D\up{r}</tt>
|M<sup>me</sup>, D<sup>r</sup>
|-
|<tt>1\ier{}, 1\iere{}, 1\ieres{}</tt>
|1<sup>er</sup>, 1<sup>re</sup>, 1<sup>res</sup>
|-
|<tt>2\ieme{} 4\iemes{}</tt>
|2<sup>e</sup> 4<sup>es</sup>
|-
|<tt>\No 1, \no 2</tt>
|N° 1, n° 2
|-
|<tt>20~\degres C, 45\degres</tt>
|20 °C, 45°
|-
|<tt>M. \bsc{Durand}</tt>
| M. <span style="font-variant: small-caps;">Durand</span>
|-
|<tt>\nombre{1234,56789}</tt>
|1 234,567 89
|-
|}

You will also notice that the layout of lists changes when switching to the French language. For more information on what the ''frenchb'' option of babel does and how you can customize its behavior, run LaTeX on file frenchb.dtx and read the produced file frenchb.pdf or frenchb.dvi.

=== German ===

You can load German language support using ''either one'' of the two following commands.

For old german orthography use
<source lang="latex">
\usepackage[german]{babel}
</source>
'''or''' for new german orthography use
<source lang="latex">
\usepackage[ngerman]{babel}
</source>
This enables German hyphenation, if you have configured your LaTeX
system accordingly. It also changes all automatic text into German. Eg.
“Chapter” becomes “Kapitel.” A set of new commands also becomes available,
which allows you to write German input files more quickly even when
you don’t use the inputenc package. Check out table 2.5 for inspiration.
With inputenc, all this becomes moot, but your text also is locked in a
particular encoding world.

{| class="wikitable"
|+German Special Characters.
|-
|<tt>"a</tt> ||ä
|- 
|<tt>"s</tt> ||ß
|-
|<tt>"`</tt> or <tt>\glqq</tt> || „ 
|-
|<tt>"'</tt> or <tt>\grqq</tt> || “
|-
|<tt>"<</tt> or <tt>\flqq</tt> || «
|- 
|<tt>"></tt> or <tt>\frqq</tt> || »
|-
|<tt>\flq</tt> || ‹
|- 
|<tt>\frq</tt> || ›
|-
|<tt>\dq</tt> || "
|-
|}

In German books you often find French quotation marks («guillemets»). German typesetters, however, use them differently. A quote in a German book would look like »this«. In the German speaking part of Switzerland, typesetters use «guillemets» the same way the French do. A major problem arises from the use of commands like <tt>\flq</tt>: If you use the OT1 font (which is the default font) the guillemets will look like the math symbol "<math>\ll</math>", which turns a typesetter’s stomach. T1 encoded fonts, on the other hand, do contain the required symbols. So if you are using this type of quote, make sure you use the T1 encoding. (<tt>\usepackage[T1]{fontenc}</tt>)

=== Greek ===

This is the preamble you need to write in the Greek language. 

<source lang="latex">
\usepackage[english,greek]{babel}
\usepackage[iso-8859-7]{inputenc}
</source>

This preamble enables hyphenation and changes all automatic text to Greek. A set of new commands also becomes available, which allows you to write Greek input files more easily. In order to temporarily switch to English and vice versa, one can use the commands <tt>\textlatin{english text}</tt> and <tt>\textgreek{greek text}</tt> that both take one argument which is then typeset using the requested font encoding. Otherwise you can use the command <tt>\selectlanguage{...}</tt> described in a previous section. Use <tt>\euro</tt> for the Euro symbol.

=== Hungarian ===

Similar to Italian, but use the following lines:
<source lang="latex">
\usepackage[magyar]{babel}
\usepackage[latin2]{inputenc}
\usepackage[T1]{fontenc}
</source>

* More information [http://www.math.bme.hu/latex/ in hungarian].

The Hungarian version of BaBeL included with standard LaTeX distribution is not perfect, a much better version can be downloaded from the previous page.

=== Icelandic /  Faroese ===

The following lines can be added to write Icelandic text:

<source lang="latex">
\usepackage[icelandic]{babel}
\usepackage[T1]{fontenc}
</source>

and for some users 
<source lang="latex">
\usepackage[utf8]{inputenc}
</source>
is needed. This allows the user to write with Icelandic characters and changes text like the ''Abstract'' in
<source lang="latex">
\begin{abstract}
Þetta er útdráttur.
\end{abstract}
</source>
into "Útdráttur" and turns ''Part''' into "Hluti".

{| class="wikitable"
|+Icelandic Special Characters.
|-
|<tt>"`</tt> or <tt>\glqq</tt> || „ 
|-
|<tt>"'</tt> or <tt>\grqq</tt> || “
|- 
|<tt>\TH</tt> || Þ
|-
|<tt>\th</tt> || þ
|-
|<tt>\"{O}</tt> || Ö
|-
|<tt>\"{o}</tt> || ö
|-
|<tt>\AE</tt> || Æ
|-
|<tt>\ae</tt> || æ
|-
|}

=== Italian ===

Italian is well supported by LaTeX. Just add ''\usepackage[italian]{babel}'' at the beginning of your document and the output of all the commands will be translated properly. You can add letters with accents without any particular setting, just write <tt>\`a \`e \'e \`i \`o \`u</tt> and you will get ''à è é ì ò ù'' (NB: the symbol changes if the inclination of the accent changes). Anyway, if you do so, it could be quite annoying since it's time-wasting. Moreover, if you are using any spell-checking program, "città" is correct, but "citt\`a" will be seen as a mistake. If you add ''\usepackage[latin1]{inputenc}'' at the beginning of your document, LaTeX will include correctly all your accented letters. To sum up, just add
<source lang="latex">
\usepackage[italian]{babel}
\usepackage[latin1]{inputenc}
</source>
at the beginning of your document and you can write in Italian without being worried of translations and fonts. If you are writing your document without getting any error, then don't worry about anything else. If you start getting some unknown errors whenever you use an Italian letter, then you have to worry about the encoding of your files. As known, any LaTeX source is just plain text, so you'll have to insert accented letters properly within the text file. If you write your document using always the same program on the same computer, you should not have any problem. If you are writing your document using different programs, if could start getting some strange errors from the compiler. The reason could be that the accented letters were not included properly within your source file and LaTeX can't recognize them. The reason is that an editor modified your document with a different encoding from the one that was used when creating it. Most of the operating systems use UTF-8 as default, but this could create problems if are using programs based on different libraries or different operating systems. The best way to solve this problem is to change the encoding to ISO-8859-1, that includes all the letters you need. Some text editors let you change the encoding in the settings.

=== Korean ===

To use LATEX for typesetting Korean, we need to solve three problems:

# We must be able to edit Korean input files. Korean input files must be in plain text format, but because Korean uses its own character set outside the repertoire of US-ASCII, they will look rather strange with a normal ASCII editor. The two most widely used encodings for Korean text files are EUC-KR and its upward compatible extension used in Korean MS-Windows, CP949/Windows-949/UHC. In these encodings each US-ASCII character represents its normal ASCII character similar to other ASCII compatible encodings such as ISO-8859-x, EUC-JP, Big5, or Shift_JIS. On the other hand, Hangul syllables, Hanjas (Chinese characters as used in Korea), Hangul Jamos, Hiraganas, Katakanas, Greek and Cyrillic characters and other symbols and letters drawn from KS X 1001 are represented by two consecutive octets. The first has its MSB set. Until the mid-1990’s, it took a considerable amount of time and effort to set up a Korean-capable environment under a non-localized (non-Korean) operating system. You can skim through the now much-outdated http://jshin.net/faq to get a glimpse of what it was like to use Korean under non-Korean OS in mid-1990’s. These days all three major operating systems (Mac OS, Unix, Windows) come equipped with pretty decent multilingual support and internationalization features so that editing Korean text file is not so much of a problem anymore, even on non-Korean operating systems.
# TEX and LATEX were originally written for scripts with no more than 256 characters in their alphabet. To make them work for languages with considerably more characters such as Korean or Chinese, a subfont mechanism was developed. It divides a single CJK font with thousands or tens of thousands of glyphs into a set of subfonts with 256 glyphs each. For Korean, there are three widely used packages; HLATEX by UN Koaunghi, hLATEXp by CHA Jaechoon and the CJK package byWerner Lemberg. HLATEX and hLATEXp are specific to Korean and provide Korean localization on top of the font support. They both can process Korean input text files encoded in EUC-KR. HLATEX can even process input files encoded in CP949/Windows-949/UHC and UTF-8 when used along with Λ, Ω. The CJK package is not specific to Korean. It can process input files in UTF-8 as well as in various CJK encodings including EUC-KR and CP949/Windows-949/UHC, it can be used to typeset documents with multilingual content (especially Chinese, Japanese and Korean). The CJK package has no Korean localization such as the one offered by HLATEX and it does not come with as many special Korean fonts as HLATEX.
# The ultimate purpose of using typesetting programs like TEX and LATEX is to get documents typeset in an ‘aesthetically’ satisfying way. Arguably the most important element in typesetting is a set of welldesigned fonts. The HLATEX distribution includes UHC PostScript fonts of 10 different families and Munhwabu fonts (TrueType) of 5 different families. The CJK package works with a set of fonts used by earlier versions of HLATEX and it can use Bitstream’s cyberbit True-Type font.

To use the HLATEX package for typesetting your Korean text, put the following declaration into the preamble of your document:
<source lang="latex">
\usepackage{hangul}
</source>
This command turns the Korean localization on. The headings of chapters, sections, subsections, table of content and table of figures are all translated into Korean and the formatting of the document is changed to follow Korean conventions. The package also provides automatic “particle selection.” In Korean, there are pairs of post-fix particles grammatically equivalent but different in form. Which of any given pair is correct depends on whether the preceding syllable ends with a vowel or a consonant. (It is a bit more complex than this, but this should give you a good picture.) Native Korean speakers have no problem picking the right particle, but it cannot be determined which particle to use for references and other automatic text that will change while you edit the document. It takes a painstaking effort to place appropriate particles manually every time you add/remove references or simply shuffle parts of your document around. HLATEX relieves its users from this boring and error-prone process. 

In case you don’t need Korean localization features but just want to typeset Korean text, you can put the following line in the preamble, instead.
<source lang="latex">
\usepackage{hfont}
</source>
For more details on typesetting Korean with HLATEX, refer to the HLATEX Guide. Check out the web site of the Korean TEX User Group (KTUG) at http://www.ktug.or.kr/.

=== Polish ===

If you plan to use Polish in your utf-8 encoded document, use the following code
<source lang="latex">
\usepackage[utf8]{inputenc}
\usepackage{polski}
\usepackage[polish]{babel}
</source>

The above code merely allows to use polish letters and translates the automatic text to polish, so that "chapter" becomes "rozdział".
There are a few additional things one must remember about.

==== Connectives ====

Polish has many single letter connectives: "a", "o", "w", "i", etc., grammar and typography rules don't allow for them to end a printed line.
To ensure that LaTeX won't set them as last letter in the line, you have to use non breakable space:
<source lang="latex">
Noc była sierpniowa, ciepła i~słodka, Księżyc oświecał srebrnem światłem wgłębienie, tak,
że twarze małego rycerza i~Basi były skąpane w blasku.
Poniżej, na podwórzu zamkowem, widać było uśpione kupy żołnierzy, a~także i~ciała zabitych
podczas dziennej strzelaniny, bo nie znaleziono dotąd czasu na ich pogrzebanie.
</source>

==== Numerals ====

According to polish grammar rules, you have to put dots after numerals in chapter, section, subsection, etc. headers.

This is achieved by redefining few LaTeX macros.

For books:
<source lang="latex">
\renewcommand\thechapter{\arabic{chapter}.}
\renewcommand\thesection{\arabic{chapter}.\arabic{section}.}
\renewcommand\thesubsection{\arabic{chapter}.\arabic{section}.\arabic{subsection}.}
\renewcommand\thesubsubsection{\arabic{chapter}.\arabic{section}.\arabic{subsection}.%
                                                           \arabic{subsubsection}.}
</source>

For articles:
<source lang="latex">
\renewcommand\thesection{\arabic{section}.}
\renewcommand\thesubsection{\arabic{section}.\arabic{subsection}.}
\renewcommand\thesubsubsection{\arabic{section}.\arabic{subsection}.\arabic{subsubsection}.}
</source>

==== Indentation ====

It's customary (depends on publisher) to indent first paragraph in sections and chapters:
<source lang="latex">
\usepackage{indentfirst}
</source>

==== Hyphenation and typography ====

It's much more frowned upon to set pages with hyphenation between pages than it is customary in American typesetting.

To adjust penalties for hyphenation spanning pages, use this command:
<source lang="latex">
\brokenpenalty=1000
</source>

To adjust penalties for leaving widows and orphans (clubs in TeX nomenclature) use those commands:
<source lang="latex">
\clubpenalty=1000
\widowpenalty=1000
</source>

==== Further information ====

Refer the [http://so.pwn.pl/zasady.php Słownik Ortograficzny] (in Polish) for additional information on polish grammar and typography rules.

Good extract is available at [http://dtp.msstudio.com.pl/typo.html Zasady Typograficzne Składania Tekstu] (in Polish)

=== Portuguese ===

Add the following code to your preamble:

<source lang="latex">
\usepackage[portuguese]{babel}
\usepackage[latin1]{inputenc}
\usepackage[T1]{fontenc}
</source>

if you are in Brazil, you can substitute the language for brazilian portuguese by choosing: <tt>brazilian</tt>. The first line is to get everything translated properly, the second is for being able to input text correctly and the third one to get hyphenation right. Note that we are using the latin1 input encoding here, so this will not work on a Mac or on DOS. Just use the appropriate encoding for your system. If you are using Linux, use 

<source lang="latex">
\usepackage[utf8]{inputenc}
</source>

=== Spanish ===

To enable Spanish writing, besides installing the appropriate hyphenation patterns, you type:

<source lang="latex">
\usepackage[spanish]{babel}
</source>

The trick is that Spanish has several options and commands to control the layout. The options may be loaded either at the call to Babel, or before, by defining the command <code>\spanishoptions</code>. Therefore, the following commands are roughly equivalent:

<source lang="latex">
\def\spanishoptions{mexico}
\usepackage[spanish]{babel}
</source>

<source lang="latex">
\usepackage[spanish,mexico]{babel}
</source>

On average, the former syntax should be preferred, as the latter is a deviation from standard Babel behavior, and thus may break other programs (LyX, latex2rtf2e) interacting with LaTeX.

Two particularly useful options are <code>es-noquoting,es-nolists</code>: some packages and classes are known to collide with Spanish in the way they handle active characters, and these options disable the internal workings of Spanish to allow you to overcome these common pitfalls. Moreover, these options may simplify the way LyX customizes some features of the Spanish layout from inside the GUI.

The options <code>mexico,mexico-com</code> provide support for local custom in Mexico: the former using decimal dot, as customary, and the latter allowing decimal comma, as required by the Mexican Official Norm (NOM) of the Department of Economy for labels in foods and goods. More localizations are in the making.

The other commands modify the spanish layout after loading babel. Two particularly useful commands are <code>\spanishoperators</code> and <code>\spanishdeactivate</code>. 

The macro <code>\spanishoperators{</code>''list of operators''<code>}</code> contains a list of spanish mathematical operators, and may be redefined at will. For instance, the command <code>\def\spanishoperators{sen}</code> only defines sen, overriding all other definitions; the command <code>\let\spanishoperators\relax</code> disables them all. This command supports accented or spaced operators: the <code>\acute{</code><''letter''><code>}</code> command puts an accent, and the <code>\,</code> command adds a small space. 
For instance, the following operators are defined by default.

<source lang="latex">
l\acute{i}m l\acute{i}m\,sup l\acute{i}m\,inf m\acute{a}x 
\acute{i}nf m\acute{i}n sen tg arc\,sen arc\,cos arc\,tg 
cotg cosec senh tgh
</source>

Finally, the macro <code>\spanishdeactivate{</code>''list of characters''<code>}</code> disables some active characters, to keep you out of trouble if they are redefined by other packages. The candidates for deactivation are the set <code><>."'</code>. Please, beware that some option preempty the availability of some active characters. In particular, you should not combine the es-noquoting option with <code>\spanishdeactivate{<>}</code>, or the es-noshorthands with <code>\spanishdeactivate{<>."}</code>. 

Please check the documentation for Babel or spanish.dtx for further details.

<noinclude>
{{LaTeX/Bottom|Export To Other Formats|Command Glossary}}
</noinclude>
