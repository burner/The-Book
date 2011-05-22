><noinclude>{{LaTeX/Top}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}
__TOC__</noinclude>

==Introduction==
Another good point of LaTeX is that you can easily reference almost anything that is numbered (sections, figures, formulas), and LaTeX will take care of numbering, updating it whenever necessary. The commands to be used do not depend on what you are referencing, and they are:
;<code>\label{''marker''}</code>
:you give the object you want to reference a ''marker'', you can see it like a name.
;<code>\ref{''marker''}</code>
:you can reference the object you have ''marked'' before. This prints the number that was assigned to the object.
;<code>\pageref{''marker''}</code>
:It will print the number of the page where the object is.

LaTeX will calculate the right numbering for the objects in the document; the ''marker'' you have used to label the object will not be shown anywhere in the document. Then LaTeX will replace the string "<code>\ref{''marker''}</code>" with the right number that was assigned to the object. If you reference a ''marker'' that does not exist, the compilation of the document will be successful but LaTeX will return a warning:
 LaTeX Warning: There were undefined references.
and it will replace "<code>\ref{''unknown-marker''}</code>" with "??" (so it will be easy to find in the document).

As you may have noticed reading how it works, it is a two-step process: first the compiler has to store the labels with the right number to be used for referencing, then it has to replace the <code>\ref</code> with the right number. That is why, when you use references, you have to compile your document twice to see the proper output. If you compile it only once, LaTeX will use the older information it collected in previous compilations (that might be outdated), but the compiler will inform you printing on the screen at the end of the compilation:
:<tt>LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.</tt>
Using the command <code>\pageref{}</code> you can help the reader to find the referenced object by providing also the page number where it can be found. You could write something like:
<source lang="latex">
See figure~\ref{fig:test} on page~\pageref{fig:test}.
</source>

Since you can use exactly the same commands to reference almost anything, you might get a bit confused after you have introduced a lot of references. It is common practice among LaTeX users to add a few letters to the label to describe ''what'' you are referencing. Here is an example:

{| class="wikitable"
|-
|'''<tt>chap:</tt>'''
|chapter
|-
|'''<tt>sec:</tt>'''
|section
|-
|'''<tt>fig:</tt>'''
|figure
|-
|'''<tt>tab:</tt>'''
|table
|-
|'''<tt>eq:</tt>'''
|equation
|-
|'''<tt>lst:</tt>'''
|code listing
|-
|}

Following this convention, the label of a figure will look like <code>\label{fig:''my_figure''}</code>, etc. You are not obligated to use these prefixes. You can use any string as argument of <code>\label{...}</code>, but these prefixes become increasingly useful as your document grows in size.

Another suggestion: try to avoid using numbers within labels. You are better off describing ''what'' the object is about. This way, if you change the order of the objects, you will not have to rename all your labels and their references.

If you want to be able to see the markers you are using in the output document as well, you can use the <code>showkeys</code> package; this can be very useful while developing your document. For more information see the [[LaTeX/Packages|Packages]] section.

==Examples==

Here are some practical examples, but you will notice that they are all the same because they all use the same commands.

===Sections===

{|
|-
|<source lang="latex">
\section{Greetings}
\label{sec:greetings}

Hello!

\section{Referencing}

I greeted in section~\ref{sec:greetings}.
</source>
|[[Image:Latex example referencing section.png|150px]]
|-
|}

You could place the label anywhere in the section; anyway, in order to avoid confusion, it is better to place it immediately after the beginning of the section. Note how the marker starts with ''sec:'', as suggested before. The label is then referenced in a different section.

===Pictures===

You can reference a picture by inserting it in the <tt>figure</tt> floating environment.

{|
|<source lang="latex">
\begin{figure}
  \centering
    \includegraphics[width=0.5\textwidth]{gull}
  \caption{Close-up of a gull}
  \label{gull}
\end{figure}
Figure~\ref{gull} shows a photograph of a gull.
</source>
|[[Image:Latex example figure referencing.png|center|300px]]
|}

When a label is declared within a float environment, the <code>\ref{...}</code> will return the respective fig/table number, but it must occur '''after''' the caption. When declared outside, it will give the section number. To be completely safe, the label for any picture or table can go within the <code>\caption{}</code> command, as in
<source lang="latex">
\caption{Close-up of a gull\label{gull}}
</source>

See the [[LaTeX/Floats, Figures and Captions|Floats, Figures and Captions]] section for more about the <code>figure</code> and related environments.

==== Fixing wrong labels ====

The command <code>\label</code> must appear after (or inside) <code>\caption</code>. Otherwise, it will pick up the current section or list number instead of what you intended.

<source lang="latex">
\begin{figure}
  \begin{center}
    \includegraphics[width=0.5\textwidth]{gull}
    \caption{Close-up of a gull} \label{fig:gull} 
  \end{center}
\end{figure}
</source>

====Issues with links to tables and figures handled by [[LaTeX/Packages/Hyperref|hyperref]]====

In case you use the package <code>hyperref</code> to create a PDF, the links to tables or figures will point to the caption of the table or figure, which is always below the table or figure itself<ref>http://www.ctan.org/tex-archive/macros/latex/contrib/hyperref/README</ref>. Therefore the table or figure will not be visible, if it is above the pointer and one has to scroll up in order to see it. If you want the link point to the top of the image you can use the package <code>hypcap</code> [http://www.ctan.org/tex-archive/macros/latex/contrib/oberdiek/hypcap.pdf] with:
<source lang="latex">
\usepackage[all]{hypcap}
</source>
Be sure to call this package ''after'' the package <code>hyperref</code>, which should otherwise be loaded last.

===Formulas===

Here is an example showing how to reference formulas:

{|
|<source lang="latex">
\begin{equation} \label{eq:solve}
x^2 - 5 x + 6 = 0
\end{equation}

\begin{equation}
x_1 = \frac{5 + \sqrt{25 - 4 \times 6}}{2} = 3
\end{equation}

\begin{equation}
x_2 = \frac{5 - \sqrt{25 - 4 \times 6}}{2} = 2
\end{equation}

and so we have solved equation \ref{eq:solve}
</source>
|[[Image:Latex example math referencing.png|350px]]
|}

As you can see, the label is placed soon after the beginning of the math mode. In order to reference a formula, you have to use an environment that adds numbers. Most of the times you will be using the <code>equation</code> environment; that is the best choice for one-line formulas, whether you are using <code>amsmath</code> or not. Note also the ''eq:'' prefix in the label.

====<tt>eqref</tt>====

The <code>amsmath</code> package adds a new command for referencing formulas; it is <code>\eqref{}</code>. It works exactly like <code>\ref{}</code>, but it adds brackets so that, instead of printing a plain number as ''5'', it will print ''(5)''. This can be useful to help the reader distinguish between formulas and other things, without the need to repeat the word "formula" before any reference. Its output can be changed as you wish; for more information see the <code>amsmath</code> documentation.

====<tt>numberwithin</tt>====
The <code>amsmath</code> package adds the <code>\numberwithin{countera}{counterb}</code> command which replaces the simple <code>countera</code> by a more sophisticated
<code>counterb.countera</code>.  For example <code>\numberwithin{equation}{section}</code> in the preamble will prepend the section number to all equation numbers.

==The <tt>varioref</tt> package==

The <code>varioref</code> package introduces a new command called <code>\vref{}</code>. This command is used exactly like the basic <code>\ref</code>, but it has a different output according to the context. If the object to be referenced is in the same page, it works just like <code>\ref</code>; if the object is far away it will print something like "5 on page 25", i.e. it adds the page number automatically. If the object is close, it can use more refined sentences like "on the next page" or "on the facing page" automatically, according to the context and the document class.

This command has to be used very carefully. It outputs more than one word, so it may happen its output falls on two different pages. In this case, the algorithm can get confused and cause a loop. Let's make an example. You label an object on page 23 and the <code>\vref</code> output happens to stay between page 23 and 24. If it were on page 23, it would print like the basic <code>ref</code>, if it were on page 24, it would print "on the previous page", but it is on both, and this may cause some strange errors at compiling time that are very hard to be fixed. You could think that this happens very rarely; unfortunately, if you write a long document it is not uncommon to have hundreds of references, so situations like these are likely to happen. One way to avoid problems during development is to use the standard <code>ref</code> all the time, and convert it to <code>vref</code> when the document is close to its final version, and then making adjustments to fix possible problems.

==The <tt>hyperref</tt> package and <tt>\autoref{}</tt>==

The <code>[[LaTeX/Packages/Hyperref|hyperref]]</code> package introduces another useful command; <code>\autoref{}</code>. This command creates a reference with additional text corresponding to the target's type, all of which will be a hyperlink.  For example, the command <code>\autoref{sec:intro}</code> would create a hyperlink to the <code>\label{sec:intro}</code> command, wherever it is. Assuming that this label is pointing to a section, the hyperlink would contain the text "section 3.4", or similar (the full list of default names can be found [http://www.tug.org/applications/hyperref/manual.html#TBL-23 here]). Note that, while there's an <source lang="latex" enclose="none">\autoref*</source> command that produces an unlinked prefix (useful if the label is on the same page as the reference), no alternative <source lang="latex" enclose="none">\Autoref</source> command is defined to produce capitalized versions (useful, for instance, when starting sentences); but since the capitalization or autoref names was chosen by the package author, you can customize the prefixed text by redefining <code>\''type''autorefname</code> to the prefix you want, as in:
<source lang="latex">
\def\sectionautorefname{Section}
</source>
This renaming trick can, of course, be used for other purposes as well.

* If you would like a hyperlink reference, but do not want the predefined text that <code>\autoref{}</code> provides, you can do this with a command such as <code>\hyperref[sec:intro]{Appendix~\ref*{sec:intro}}</code>. Note that you can disable the creation of hyperlinks in <code>hyperref</code>, and just use these commands for automatic text.

* Keep in mind that the \label '''must''' be placed inside an environment with a counter, such as a table or a figure. Otherwise, not only the number will refer to the current section, as mentioned [[#Fixing wrong labels|above]], but the name will refer to the previous environment with a counter. For example, if you put a label after closing a figure, the label will still say "figure n", on which n is the current section number.

==The <tt>hyperref</tt> package and <tt>\nameref{}</tt>==

The <code>hyperref</code> package also automatically includes the <code>nameref</code> package, and a similarly named command.  It is similar to <code>\autoref{}</code>, but inserts text corresponding to the section name, for example.
<source lang="latex">
\section{MyFirstSection} \label{marker}
\section{MySecondSection}
In section~\nameref{marker} we defined...
</source>

==The <tt>hyperref</tt> package and <tt>\phantomsection</tt>==

When you define a <code>\label</code> outside a figure, a table, or other floating objects, the label points to the current section. In some case, this behavior is
not what you'd like and you'd prefer the generated link to point to the line where the <code>\label</code> is defined. This can be achieved with the command
<code>\phantomsection</code> as in this example:

<source lang="latex">
%The link location will be placed on the line below.
\phantomsection
\label{the_label}
</source>

==References==
<references/>

<noinclude>
{{LaTeX/Bottom|Theorems|Indexing}}
</noinclude>
