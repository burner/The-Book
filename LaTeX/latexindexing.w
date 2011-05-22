><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>
A useful feature of many books, index is an alphabetical list of words and expressions with the pages of the book upon which they are to be found. LaTeX supports the creation of indices with its package {{LaTeX/Package|makeidx}}, and its support program <code>makeindex</code>, called on some systems <code>makeidx</code>.

= Using <tt>makeidx</tt> =
To enable the indexing feature of LaTeX, the {{LaTeX/Package|makeidx}} package must be loaded in the preamble with:
{{LaTeX/Usage|code=
\usepackage{makeidx}
}}

and the special indexing commands must be enabled by putting the
{{LaTeX/Usage|code=
\makeindex
}}

command into the input file preamble. This should be done within the preamble, since it tells LaTeX to create the files needed for indexing. To tell LaTeX what to index, use
{{LaTeX/Usage|code=
\index{key}
}}

where ''key'' is the index entry and does not appear in the final layout. You enter the index commands at the points in the text that you want to be referenced in the index, likely near the reason for the ''key''. For example, the text
{{LaTeX/Usage|code=
To solve various problems in physics, it can be advantageous
to express any arbitrary piecewise-smooth function as a
Fourier Series composed of multiples of sine and cosine functions.
}}
can be re-written as
{{LaTeX/Usage|code=
To solve various problems in physics, it can be advantageous
to express any arbitrary piecewise-smooth function as a Fourier Series
\index{Fourier Series}
composed of multiples of sine and cosine functions.
}}
to create an entry called 'Fourier Series' with a reference to the target page. Multiple uses of ''\index'' with the same ''key'' on different pages will add those target pages to the same index entry.


To show the index within the document, merely use the command
{{LaTeX/Usage|code=
\printindex
}}
It is common to place it at the end of the document. The default index format is two columns.


The {{LaTeX/Package|showidx}} package that comes with LaTeX prints out all index entries in the left margin of the text. This is quite useful for proofreading a document and verifying the index.

== Compiling Indexes ==
When the input file is processed with LaTeX, each {{LaTeX/LaTeX|code=\index}} command writes an appropriate index entry, together with the current page number, to a special file. The file has the same name as the LaTeX input file, but a different extension (<code>.idx</code>). This <code>.idx</code> file can then be processed with the <code>makeindex</code> program. Type in the command line:

 <tt>makeindex</tt> ''filename''

Note that ''filename'' is without extension: the program will look for ''filename.idx'' and use that. You can optionally pass ''filename.idx'' directly to the program as an argument. The <code>makeindex</code> program generates a sorted index with the same base file name, but this time with the extension <code>.ind</code>. If now the LaTeX input file is processed again, this sorted index gets included into the document at the point where LaTeX finds {{LaTeX/LaTeX|code=\printindex}}.

The index created by latex with the default options may not look as nice or as suitable as you would like it. To improve the looks of the index <code>makeindex</code> comes with a set of style files, usually located somewhere in the tex directory structure, usually below the <code>makeindex</code> subdirectory. To tell <tt>makeindex</tt> to use a specific style file, run it with the command line option:

  <tt>makeindex</tt> -s <style file> ''filename''

If you use a GUI for compiling latex and index files, you may have to set this in the options. Here are some configuration tips for typical tools:
=== MakeIndex Settings in WinEdt  ===
Say you want to add an index style file named <code>simpleidx.ist</code>
* Texify/PDFTexify: Options→Execution Modes→Accessories→PDFTeXify, add to the Switches: <code>--mkidx-option="-s simpleidx.ist"</code>
* MakeIndex alone: Options->Execution Modes→Accessories→MakeIndex, add to command line: <code>-s simpleidx.ist</code>

== Sophisticated Indexing ==
Below are examples of {{LaTeX/LaTeX|code=\index}} entries:

{|class="wikitable"
!Example
!Index Entry
!Comment
|-
|{{LaTeX/LaTeX|code=\index{hello}<!---->}}
|hello, 1
|Plain entry
|-
|{{LaTeX/LaTeX|code=\index{hello!Peter}<!---->}}
|&&Peter, 3
|Subentry under 'hello'
|-
|{{LaTeX/LaTeX|code=\index{Sam@\textsl{Sam}<!---->}<!---->}}
|''Sam'', 2
|Formatted entry
|-
|{{LaTeX/LaTeX|code=\index{Lin@\textbf{Lin}<!---->}<!---->}}
|'''Lin''', 7
|Same as above
|-
|{{LaTeX/LaTeX|code=\index{Jenny{{!}}textbf}<!---->}}
|Jenny, '''3'''
|Formatted page number
|-
|{{LaTeX/LaTeX|code=\index{Joe{{!}}textit}<!---->}}
|Joe, ''5''
|Same as above
|-
|{{LaTeX/LaTeX|code=\index{ecole@\'ecole}<!---->}}
|école, 4
|Handling of accents
|-
|{{LaTeX/LaTeX|code=\index{Peter{{!}}see{hello}<!---->}<!---->}}
|Peter, ''see'' hello
|Cross-references
|-
|{{LaTeX/LaTeX|code=\index{Jen{{!}}seealso{Jenny}<!---->}<!---->}}
|Jen, ''see also'' Jenny
|Same as above
|}

=== Subentries ===
If some entry has subsections, these can be marked off with <code>!</code>. For example,
{{LaTeX/Usage|code=
\index{encodings!input!cp850}
}}
would an index with 'cp850' categorized under 'input' (which itself is categorized into 'encodings'). These are called subsubentries and subentries in makeidx terminology.

=== Controlling Sorting ===
In order to determine how an index key is sorted, place a value to sort by before the key with the <code>@</code> as a separator. This is useful if there is any formatting or math mode, so one example may be
{{LaTeX/Usage|code=
\index{F@$\vec{F}$}
}}
so that the entry in the index will show as '<math>\vec{F}</math>' but be sorted as 'F'.

=== Changing Page Number Style ===
To change the formatting of a page number, append a <code>|</code> and the name of some command which does the formatting. This command should only accept one argument.

For example, if on page 3 of a book you introduce bulldogs and include the command
{{LaTeX/Usage|code=
\index{bulldog}
}}
and on page 10 of the same book you wish to show the main section on bulldogs with a bold page number, use
{{LaTeX/Usage|code=
\index{bulldog{{!}}textbf}
}}
This will appear in the index as
bulldog, 3, '''10'''

If you use <code>texindy</code> in place of <code>makeindex</code>, the classified entries will be sorted too, such that all the bolded entries will be placed before all others by default.

=== Multiple Pages ===
To perform multi-page indexing, add a <code>|(</code> and <code>|)</code> to the end of the {{LaTeX/LaTeX|code=\index}} command, as in
{{LaTeX/Usage|code=
\index{Quantum Mechanics!History{{!}}(}
In 1901, Max Planck released his theory of radiation dependant on quantized energy.
While this explained the ultraviolet catastrophe in the spectrum of 
blackbody radiation, this had far larger consequences as the beginnings of quantum mechanics.
...
\index{Quantum Mechanics!History{{!}})}
}}
The entry in the index for the subentry 'History' will be the range of pages between the two {{LaTeX/LaTeX|code=\index}} commands.

=== Using special characters ===
In order to place values with <code>!</code>, <code>@</code>, or <code>|</code> in the {{LaTeX/LaTeX|code=\index}} command, one must quote these characters by using a double quotation mark (") and can only show " by quoting it (i.e., a key for " would be {{LaTeX/LaTeX|code=\index{""}<!---->}}).

This rule does not hold for \", so to put ä in the index, one may still use {{LaTeX/LaTeX|code=\index{a@\"{a}<!---->}<!---->}}.

== Warnings ==
Note that the {{LaTeX/LaTeX|code=\index}} command can affect your layout if not used carefully. Here is an example:

{{LaTeX/Example|code=
My Word \index{Word}. As opposed
to Word\index{Word}. Note the
position of the full stop.
|render=
My Word . As opposed to Word. Note
the position of the full stop.
}}

= Abbreviation list =

You can make a list of abbreviations with the package {{LaTeX/Package|nomencl}} [http://www.ctan.org/tex-archive/macros/latex/contrib/nomencl/].
You may also be interested in using the {{LaTeX/Package|glossaries}} package described in the [[LaTeX/Glossary|Glossary]] chapter.

To enable the Nomenclature feature of LaTeX, the {{LaTeX/Package|nomencl}} package must be loaded in the preamble with:
{{LaTeX/Usage|code=
\usepackage[⟨options ⟩]{nomencl}
\makenomenclature
}}

Issue the {{LaTeX/LaTeX|code=\nomenclature[⟨preﬁx⟩]{⟨symbol⟩}{⟨description⟩}<!---->}} command for each symbol you want to have included in the nomenclature list. The best place for this command is immediately after you introduce the symbol for the ﬁrst time. Put {{LaTeX/LaTeX|code=\printnomenclature}} at the place you want to have your nomenclature list. 

Run LaTeX 2 times then

 <tt>makeindex</tt> ''filename.nlo''  -s nomencl.ist -o ''filename.nls''

followed by running LaTeX once again.

To add the abbreviation list to the table of content, {{LaTeX/Parameter|intoc}} option can be used when declare the {{LaTeX/Package|nomencl}} package,  i.e. {{LaTeX/Usage|code=\usepackage[intoc]{nomencl} }} instead of using the code in [[#Adding Index to Table Of Contents|Adding Index to Table Of Contents]] section.

The title of the list can be changed using the following command:
{{LaTeX/Usage|code=\renewcommand{\nomname}{List of Abbreviations}
}}

= Multiple indexes =

If you need multiple indexes you can use the package {{LaTeX/Package|multind}} [http://www.tex.ac.uk/cgi-bin/texfaq2html?label=multind]. 

This package provides the same commands as {{LaTeX/Package|makeidx}}, but now you also have to pass a name as the first argument to every command.
{{LaTeX/Usage|code=
\usepackage{multind}
\makeindex{books}
\makeindex{authors}
...
\index{books}{A book to index}
\index{authors}{Put this author in the index}
...
\printindex{books}{The Books index}
\printindex{authors}{The Authors index}
}}

= Adding Index to Table Of Contents =

By default, Index won't show in Table Of Contents, you have to add it manually.

To add index as a chapter, use this commands:
{{LaTeX/Usage|code=
\clearpage
\addcontentsline{toc}{chapter}{Index}
\printindex
}}

If you use book class, you may want to start it on odd page, for this, use {{LaTeX/LaTeX|code=\cleardoublepage}}.

= International indexes =
If you want to sort entries that have international characters (such as ő, ą, ó, ç, etc.) you may find that the sorting "is not quite right". In most cases the characters are treated as special characters and end up in the same group as @, ¶ or µ. In most languages that use Latin alphabet it's not correct.

== Generating index ==
<small>Unfortunately, current version of <code>xindy</code> and {{LaTeX/Package|hyperref}} are incompatible. When you use {{LaTeX/Parameter|textbf}} or {{LaTeX/Parameter|textit}} modifiers, <code>texindy</code> will print error message:<code>unknown cross-reference-class `hyperindexformat'! (ignored)</code> and won't add those pages to index. Work-around for this bug is described on the [[Talk:LaTeX/Indexing#Texindy, hyperref and textbf, textit modifiers|talk page]].</small>

To generate international index file you have to use <code>texindy</code> instead of <code>makeindex</code>.

[http://xindy.sourceforge.net/ xindy] is a much more extensible and robust indexing system than the <code>makeindex</code> system.

For example, one does not need to write:
{{LaTeX/Usage|code=
\index{Lin@\textbf{Lin}<!---->}<!---->}}
to get the <tt>Lin</tt> entry after <tt>LAN</tt> and before <tt>LZA</tt>, instead, it's enough to write
{{LaTeX/Usage|code=
\index{\textbf{Lin}<!---->}
}}

But what is much more important, it can properly sort index files in many languages, not only english. 

Unfortunately, generating indexes ready to use by <tt>LaTeX</tt> using <code>xindy</code> is a bit more complicated than with <code>makeindex</code>.

First, we need to know in what encoding the <tt>.tex</tt> project file is saved. In most cases it will be UTF-8 or ISO-8859-1, though if you live, for example in Poland it may be ISO-8859-2 or CP-1250. Check the parameter to the <code>inputenc</code> package.

Second, we need to know which language is prominently used in our document. <tt>xindy</tt> can natively sort indexes in albanian,    dutch,      hebrew,     latin,          norwegian,   slovak,     
belarusian,  english,    georgian,  hungarian,  latvian,        polish,      slovenian,  vietnamese,
bulgarian,   esperanto,  german,    icelandic,  lithuanian,     portuguese,  spanish,
croatian,    estonian,   greek,     italian,    romanian,       sorbian,     swedish,
czech,       finnish,    gypsy,     klingon,    macedonian,     russian,     turkish,
danish,      french,     hausa,     kurdish,    mongolian,      serbian and  ukrainian.

I don't know if other languages have similar problems, but with polish, if your <code>.tex</code> is saved using UTF-8, the <code>.ind</code> produced by texindy will be encoded in ISO-8859-2 if you use only <code>-L polish</code>.
While it's not a problem for entries containing polish letters, as LaTeX internally encodes all letters to plain ASCII, it is for accented letters at beginning of words, they create new index entry groups, if you have, for example an "średnia" entry, you'll get a "Ś" encoded in ISO-8859-2 <code>.ind</code> file.
LaTeX doesn't like if part of the file is in UTF-8 and part is in IS-8859-2.
The obvious solution (adding <code>-C utf8</code>) doesn't work, <code>texindy</code> stops with
 ERROR: Could not find file "tex/inputenc/utf8.xdy"
error.
The fix this, you have to load the definiton style for the headings using <code>-M switch</code>:
 -M lang/polish/utf8

In the end we have to run such command:
 texindy -L polish -M lang/polish/utf8 ''filename.idx''

Additional way to fix this problem is use "iconv" to create utf8.xdy from latin2.xdy 
 <code> iconv -f latin2 -t utf8 latin2.xdy >utf8.xdy </code>
in folder 
 <code>  /usr/share/xindy/tex/inputenc </code>
(You must have root privileges)


=== xindy in kile ===
To use <code>texindy</code> instead of <code>makeindex</code> in kile, you have to either redefine the MakeIndex tool in Settings → Configure Kile... → Tools → Build, or define new tool and redefine other tools to use it.

The <code>xindy</code> definition should look similar to this:
 General:
  Command: texindy
  Options: -L polish -M lang/polish/utf8 -I latex '%S.idx'
 Advanced:
  Type: Run Outside of Kile
  Class: Compile
  Source extension: idx
  Target extension: ind
  Target file: <empty>
  Relative dir: <empty>
  State: Editor
 Menu:
  Add tool to Build menu: Compile
  Icon: the one you like


<noinclude>
{{LaTeX/Bottom|Labels and Cross-referencing|Glossary}}
</noinclude>
