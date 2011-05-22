><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>
Many technical documents use terms or acronyms unknown to general population.
It's common practice to add glossaries to make those works more understandable.

The {{LaTeX/Package|glossaries}} package had been created to assist users in creating glossary.
It supports multiple glossaries, acronyms and symbols.

It replaces the {{LaTeX/Package|glossary}} package and can be used instead of the {{LaTeX/Package|nomencl}} package.

= Using {{LaTeX/Package|glossaries}} =
To enable the use of {{LaTeX/Package|glossaries}} package, you have to load the package:
{{LaTeX/Usage|code=
\usepackage{glossaries}
}}
if you will be using <tt>xindy</tt> (highly recommended) rather than <tt>makeindex</tt> you need to specify {{LaTeX/Parameter|xindy}} option:
{{LaTeX/Usage|code=
\usepackage[xindy]{glossaries}
}}
for the glossary to show up in Table of Contents you need to additionally add {{LaTeX/Parameter|toc}} option:
{{LaTeX/Usage|code=
\usepackage[toc]{glossaries}
}}
the glossary index won't be generated until you place the following command in document preamble:
{{LaTeX/Usage|code=
\makeglossaries
}}

Note that the links in generated glossary won't be “clickable” unless you load this package ''after'' the {{LaTeX/Package|hyperref}} package.

Windows users will need to install Perl for makeglossaries to work.

= Defining glossary entries =
To use an entry from glossary you first need to define it.
There are few ways to define an entry depending on what you define and how it is going to be used.

Note that a defined entry ''won't'' be included in the printed glossary ''unless'' it is used in the document.
This enables you to create a glossary of general terms and just {{LaTeX/LaTeX|code=\include}} it in all your documents.

== Defining terms ==
To define a term in glossary you use {{LaTeX/LaTeX|code=\newglossaryentry}} macro:
{{LaTeX/Usage|code=
\newglossaryentry{<label>}{<settings>}
}}
<label> is a unique label used to identify an entry in glossary, <settings> are comma separated <tt>key=value</tt> pairs used to define an entry.

For example, to define a computer entry:
{{LaTeX/Usage|code=
\newglossaryentry{computer}
{
  description={is a programmable machine that receives input,
               stores and manipulates data, and provides
               output in a useful format}
}
}}
The above example defines an entry that has the same label and entry name.
This is not always the case as the next entry will show:
{{LaTeX/Usage|code=
\newglossaryentry{naiive}
{
  name=na\"{\i}ve,
  description={is a French loanword (adjective, form of naïf)
               indicating having or showing a lack of experience,
               understanding or sophistication}
}
}}

When you define terms, you need to remember that they will be sorted by <tt>makeindex</tt> or <tt>xindy</tt>.
While <tt>xindy</tt> is a bit more LaTeX aware, it does it by ommiting latex macros ({{LaTeX/LaTeX|code=\"{\i}<!---->}}) thus incorrectly sorting the above example as <tt>nave</tt>.
<tt>makeindex</tt> won't fare much better, because it doesn't understand TeX macros, it will interpret the word exactly as it was defined, putting it inside symbol class, before words beginning with <tt>naa</tt>.
Therefore it's needed to extend our example and specify how to sort the word:
{{LaTeX/Usage|code=
\newglossaryentry{naiive}
{
  name=na\"{\i}ve,
  description={is a French loanword (adjective, form of naïf)
               indicating having or showing a lack of experience,
               understanding or sophistication},
  sort=naive
}
}}

You can also specify plural forms, if they are not formed by adding “s” (we will learn how to use them in next section):
{{LaTeX/Usage|code=
\newglossaryentry{Linux}
{
  description={is a generic term referring to the family of Unix-like
               computer operating systems that use the Linux kernel},
  plural=Linuces
}
}}

== Defining symbols ==
Defined entries can also by symbols:
{{LaTeX/Usage|code=
\newglossaryentry{pi}
{
  name={\ensuremath{\pi}<!---->},
  description={ratio of circumference of circle to its
               diameter},
  sort=pi
}
}}

You can also define both a name and a symbol:
{{LaTeX/Usage|code=
\newglossaryentry{real number}
{
  name={real number},
  description={include both rational numbers, such as $42$ and 
               $\frac{-23}{129}$, and irrational numbers, 
               such as $\pi$ and the square root of two; or,
               a real number can be given by an infinite decimal
               representation, such as $2.4871773339\ldots$ where
               the digits continue in some way; or, the real
               numbers may be thought of as points on an infinitely
               long number line},
  symbol={\ensuremath{\mathbb{R}<!---->}<!---->}
}
}}
Note that not all glossary styles show defined symbols.

== Defining acronyms ==
Defined acronyms can be put in separate list if you use <tt>acronym</tt> package option:
{{LaTeX/Usage|code=
\usepackage[acronym]{glossaries}
}}

To define a new acronym you use the {{LaTeX/LaTeX|code=\newacronym}} macro:
{{LaTeX/Usage|code=
\newacronym{<label>}{<abbrv>}{<full>}
}}
where <label> is the unique label identifying the acronym, <abbrv> is the abbreviated form of the acronym and <full> is the expanded text. For example:
{{LaTeX/Usage|code=
\newacronym{lvm}{LVM}{Logical Volume Manager}
}}

= Using defined terms =
When you have defined a term, you can use it in a document.
There are many different commands used to refer to glossary terms.

== General references ==
A general reference is used with {{LaTeX/LaTeX|code=\gls}} command.
If, for example, you have glossary entries defined as those above, you might use it in this way:
{{LaTeX/Example|code=
\Gls{naiive} people don't know about
alternative \gls{computer} operating systems:
\glspl{Linux}, BSDs and GNU/Hurd.
|render=
Naïve people don't know about alternative computer opera-<br/>
ting systems: Linuces, BSDs and GNU/Hurd.
</pre>}}

Description of commands used in above example:
{{LaTeX/Usage|code=
\gls{<label>}
}}
This command prints the term associated with <label> passed as its argument.
If the {{LaTeX/Package|hyperref}} package was loaded before {{LaTeX/Package|glossaries}} it will also be hyperlinked the the entry in glossary.

{{LaTeX/Usage|code=
\glspl{<label>}
}}
This command prints the plural of the defined therm, other than that it behaves in the same way as {{LaTeX/LaTeX|code=gls}}.

{{LaTeX/Usage|code=
\Gls{<label>}
}}
This command prints the singular form of the term with the first character converted to upper case.

{{LaTeX/Usage|code=
\Glspl{<label>}
}}
This command prints the plural form with first letter of the term converted to upper case.

== Referring acronyms ==
Acronyms behave a bit differently than normal glossary terms.
On first use the {{LaTeX/LaTeX|code=\gls}} command will display "<full> (<abbrv>)".
On subsequent uses only the abbreviation will be displayed.

To reset the first use of an acronym use:
{{LaTeX/Usage|code=
\glsreset{<label>}
}}
or, if you want to reset the use status of all acronyms:
{{LaTeX/Usage|code=
\glsresetall
}}

= Displaying the Glossary =
To display the sorted list of terms you need to put:
{{LaTeX/Usage|code=
\printglossaries
}}
at the place you want the glossary and the list of acronyms to be printed.

Then you have to do three steps:
# Build LaTeX document, this will generate the files used by <tt>makeglossaries</tt>
# Run the indexing/sorting, the recommended way is to use <tt>makeglossaries</tt> (a script that runs <tt>xindy</tt> or <tt>makeindex</tt> depending on options in the document with correct encoding and language settings):<pre>makeglossaries <myDocument></pre>
# Build LaTeX document again to get document with glossary entries

If your entries are interlinked (entries themselves link to other entries with {{LaTeX/LaTeX|code=\gls}}) you will need to run points 1 and 2 twice for a total of five program invocations: 1, 2, 1, 2, 3.

= References =
* <cite id="officialManual">The {{LaTeX/Package|glossaries}} documentation, http://tug.ctan.org/tex-archive/macros/latex/contrib/glossaries/</cite>

* <cite>''Using LaTeX to Write a PhD Thesis'', Nicola L.C. Talbot, http://theoval.cmp.uea.ac.uk/~nlct/latex/thesis/node25.html</cite>

{{-}}
{{TODO|
* Add advanced usage
* Add examples of styles}}
{{status|50%}}

<noinclude>
{{LaTeX/Bottom|Indexing|Algorithms and Pseudocode}}
</noinclude>
