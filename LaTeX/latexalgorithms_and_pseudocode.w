><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

LaTeX has a variety of packages that can help to format algorithms, code, and "[[w:pseudocode|pseudocode]]".  These packages provide stylistic enhancements over a uniform style (i.e., typewriter fonts) so that constructs such as loops or conditionals are visually separated from other text.

= Typesetting Algorithms =
==Typesetting using the <tt>algorithmic</tt> package==

The <tt>algorithmic</tt> environment provides a number of popular constructs for algorithm designs. The command <tt>\begin{algorithmic}</tt> can be given the optional argument of a positive integer, which if given will cause line numbering to occur at multiples of that integer. E.g. <tt>\begin{algorithmic}[5]</tt> will enter the algorithmic environment and number every fifth line. 

Below is an example of typesetting a basic algorithm using the <tt>algorithmic</tt> package (remember to add the <tt>\usepackage{algorithmic}</tt> statement to your document preamble):

<source lang="latex">
\begin{algorithmic}
\IF {$i\geq maxval$} 
        \STATE $i\gets 0$
\ELSE
        \IF {$i+k\leq maxval$}
                \STATE $i\gets i+k$
        \ENDIF
\ENDIF 
\end{algorithmic}
</source>

The LaTeX source can be written to a format familiar to programmers so that it is easy to read. This will not, however, affect the final layout in the document.

[[File:Latex-algorithmic-if-else.png|300px]]

There are several constructs provided by <tt>algorithmic</tt> detailed below

=== Single line statements ===
<source lang="latex">\STATE <text></source>
: A simple statement, e.g. for setting a variable. For example,
<source lang="latex">
\begin{algorithmic}
\STATE i=0
\end{algorithmic}
</source>
would produce <br>
<tt>i = 0</tt> <br> <br>

=== If-statements ===

There are three forms of this construct
<source lang="latex">\IF{<condition>} <text> \ENDIF</source>
<source lang="latex">\IF{<condition>} <text> \ELSE <text> \ENDIF</source>
<source lang="latex">\IF{<condition>} <text> \ELSIF{<condition>} <text> \ELSE <text> \ENDIF</source>
The third form accepts as many \ELSIF{} clauses as required.

=== For-loops ===
There are two forms
<source lang="latex">\FOR{<condition>} <text> \ENDFOR</source>
<source lang="latex">\FORALL{<condition>} <text> \ENDFOR</source>
: A traditional "for" loop. The method of iteration is usually described in the first argument, 
e.g. <br>
<source lang="latex">
\FOR{$i = 1 \to 10$} 
\STATE $i \gets i + 1$
\ENDFOR
</source>

=== While-loops ===
<source lang="latex">\WHILE{<condition>} <text> \ENDWHILE</source>

=== Repeat until condition ===
<source lang="latex">\REPEAT <text> \UNTIL{<condition>}</source>

=== Infinite loops ===
<source lang="latex">\LOOP <text> \ENDLOOP</source>

=== Precondition ===
<source lang="latex">\REQUIRE <text></source>

=== Postcondition ===
<source lang="latex">\ENSURE <text></source>

=== Returning variables ===
<source lang="latex">\RETURN <text></source>

=== Printing variables ===
<source lang="latex">\PRINT <text></source>
: This is included because it is used so frequently it is considered an operation in its own right.

=== Comments ===
<source lang="latex">\COMMENT{<text>}</source>

Note that you cannot use \COMMENT as the first statement of any closed structure, such as \IF..\ENDIF, \FOR..\ENDFOR, \FORALL..\ENDFORALL, \WHILE..\ENDWHILE, and \begin{algorithmic}..\end{algorithmic}. An error "LaTeX Error: Something's wrong--perhaps a missing \item" will be reported (It does not make much sense). There are two workarounds:

# Use \STATE \COMMENT{<text>}.
# Use the optional arguments in those closed structures. For example, \WHILE[<comment-text>]{<condition>}. To use math in comment text, replace $..$ by \ensuremath{..}

=== Compatibility with hyperref ===
Due to a bug, the algorithmic package is not compatible with hyperref. A workaround is the [http://mrunix.de/forums/showpost.php?s=f85d42fd6f15e8f112bbc31d94d21424&p=258363&postcount=6 algorithmic-fix] package. Copy the code found on the linked page to a file called algorithmic-fix.sty and include it with \usepackage{algorithmic,algorithmic-fix}. However, if this trick fails, try using \usepackage{hyperref} before using \usepackage{algorithmic}. In this case, you might not even need the algorithmic-fix.sty.

=== Renaming things: algorithm to procedure, require/ensure to input/output ===

<source lang="latex">
\floatname{algorithm}{Procedure}
\renewcommand{\algorithmicrequire}{\textbf{Input:}}
\renewcommand{\algorithmicensure}{\textbf{Output:}}
</source>

== The <tt>algorithm</tt> environment ==

It is often useful for the algorithm produced by <tt>algorithmic</tt> to be "floated" to the optimal point
in the document to avoid it being split across pages. The <tt>algorithm</tt> environment provides this and a few other useful features. Include it by adding the <br>
<tt>\usepackage{algorithm}</tt>
to your document's preamble. It is entered into by
<source lang="latex">
\begin{algorithm}
\caption{<your caption for this algorithm>}
\label{<your label for references later in your document>}
\begin{algorithmic}
<algorithmic environment>
\end{algorithmic}
\end{algorithm}
</source>

=== Algorithm numbering ===
The default numbering system for the <tt>algorithm</tt> package is to number algorithms sequentially. This is often not desirable, particularly in large documents where numbering according to chapter is more appropriate. The numbering of algorithms can be influenced by providing the name of the document component within which numbering should be recommenced. The legal values for this option are: part, chapter, section, subsection, subsubsection or nothing (default). For example:
<source lang="latex">
\usepackage[chapter]{algorithm}
</source>

=== List of algorithms===

When you use figures or tables, you can add a list of them close to the table of contents; the <tt>algorithm</tt> package provides a similar command. Just put
<source lang="latex">
\listofalgorithms
</source>
anywhere in the document, and LaTeX will print a list of the "algorithm" environments in the document with the corresponding page and the caption.

=== An example from the manual ===
This is an example taken from the manual ([[#refOfficialManual|official manual, p.7]])
<source lang="latex">
\begin{algorithm}                      % enter the algorithm environment
\caption{Calculate $y = x^n$}          % give the algorithm a caption
\label{alg1}                           % and a label for \ref{} commands later in the document
\begin{algorithmic}                    % enter the algorithmic environment
\REQUIRE $n \geq 0 \vee x \neq 0$
\ENSURE $y = x^n$
\STATE $y \Leftarrow 1$
\IF{$n < 0$}
\STATE $X \Leftarrow 1 / x$
\STATE $N \Leftarrow -n$
\ELSE
\STATE $X \Leftarrow x$
\STATE $N \Leftarrow n$
\ENDIF
\WHILE{$N \neq 0$}
\IF{$N$ is even}
\STATE $X \Leftarrow X \times X$
\STATE $N \Leftarrow N / 2$
\ELSE[$N$ is odd]
\STATE $y \Leftarrow y \times X$
\STATE $N \Leftarrow N - 1$
\ENDIF
\ENDWHILE
\end{algorithmic}
\end{algorithm}
</source>


; More information about all possible commands available at the project page: http://developer.berlios.de/docman/?group_id=3442
<!-- http://wwwcsif.cs.ucdavis.edu/~zhangji/latex/algorithms.pdf -->
; The official manual is located at: http://developer.berlios.de/docman/display_doc.php?docid=800&group_id=3442

<!-- ===================================================================================== -->

==Typesetting using the <tt>program</tt> package==

The <tt>program</tt> package provides macros for typesetting algorithms.
Each line is set in math mode, so all the indentation and spacing is done automatically. 
The notation  <tt>|variable_name|</tt> can be used within normal text,
maths expressions or programs to indicate a variable name.
Use <tt>\origbar</tt> to get a normal <tt>|</tt> symbol in a program.
The commands <tt>\A</tt>, <tt>\B</tt>, <tt>\P</tt>, <tt>\Q</tt>, <tt>\R</tt>, <tt>\S</tt>, <tt>\T</tt> and <tt>\Z </tt>typeset the corresponding bold
letter with the next object as a subscript (eg <tt>\S1</tt> typesets <tt>{\bf
S$_1$}</tt> etc). Primes work normally, eg <tt>\S‘‘</tt>.

Below is an example of typesetting a basic algorithm using the
<tt>program</tt> package (remember to add the
<tt>\usepackage{program}</tt> statement to your document
preamble):

<source lang="latex">
\begin{program}
\mbox{A fast exponentiation procedure:}
\BEGIN %
  \FOR i:=1 \TO 10 \STEP 1 \DO
     |expt|(2,i); \\ |newline|() \OD %
\rcomment{This text will be set flush to the right margin}
\WHERE
\PROC |expt|(x,n) \BODY
          z:=1;
          \DO \IF n=0 \THEN \EXIT \FI;
             \DO \IF |odd|(n) \THEN \EXIT \FI;
\COMMENT{This is a comment statement};
                n:=n/2; x:=x*x \OD;
             \{ n>0 \};
             n:=n-1; z:=z*x \OD;
          |print|(z) \ENDPROC
\END
\end{program}
</source>

[[File:LaTeX_program_package_example01.png]]

The commands <tt>\(</tt> and <tt>\)</tt> are redefined 
to typeset an algorithm in a minipage, so an algorithm
can appear as a single box in a formula. For example,
to state that a particular action system is equivalent
to a WHILE loop you can write:

<source lang="latex">
\[
\( \ACTIONS A:
        A \EQ \IF \B{} \THEN \S{}; \CALL A
                       \ELSE \CALL Z \FI \QE
   \ENDACTIONS \)
\EQT
\( \WHILE \B{} \DO \S{} \OD \)
\]
</source>

Dijkstra conditionals and loops:
<source lang="latex">
\begin{program}
\IF x = 1 \AR y:=y+1
\BAR x = 2 \AR y:=y^2
\utdots
\BAR x = n \AR y:=\displaystyle\sum_{i=1}^n y_i \FI

\DO 2 \origbar x \AND x>0 \AR x:= x/2
\BAR \NOT 2 \origbar x    \AR x:= \modbar{x+3} \OD
\end{program}
</source>

Loops with multiple exits:
<source lang="latex">
\begin{program} 
\DO \DO \IF \B1 \THEN \EXIT \FI;
        \S1;
        \IF \B2 \THEN \EXIT(2) \FI \OD;
    \IF \B1 \THEN \EXIT \FI \OD
\end{program} 
</source>

A Reverse Engineering Example.

Here's the original program:
<source lang="latex">
\begin{program} 
 \VAR \seq{m := 0, p := 0, |last| := `` ''}; 
 \ACTIONS |prog|: 
|prog| \ACTIONEQ %
    \seq{|line| := `` '', m := 0, i := 1};
    \CALL |inhere| \ENDACTION
l \ACTIONEQ %
    i := i+1; 
    \IF (i=(n+1)) \THEN \CALL |alldone| \FI ; 
    m := 1; 
    \IF |item|[i] \neq |last|
        \THEN |write|(|line|); |line| := `` ''; m := 0;
              \CALL |inhere| \FI ; 
    \CALL |more| \ENDACTION
|inhere| \ACTIONEQ %
    p := |number|[i]; |line| := |item|[i];
    |line| := |line| \concat `` '' \concat p;
    \CALL |more| \ENDACTION
|more| \ACTIONEQ %
    \IF (m=1) \THEN p := |number|[i];
    |line| := |line| \concat ``, '' \concat p \FI ; 
    |last| := |item|[i]; 
    \CALL l  \ENDACTION  
|alldone| \ACTIONEQ |write|(|line|); \CALL Z \ENDACTION \ENDACTIONS \END 
\end{program} 
</source>

And here's the transformed and corrected version:
<source lang="latex">
\begin{program} 
\seq{|line| := `` '', i := 1};
\WHILE i \neq n+1 \DO 
  |line| := |item|[i] \concat `` '' \concat |number|[i]; 
  i := i+1; 
  \WHILE i \neq n+1 \AND |item|[i] = |item|[i-1] \DO 
    |line| := |line| \concat ``, '' \concat |number|[i]);
    i := i+1 \OD ; 
  |write|(|line|) \OD 
\end{program}
</source>

The package also provides a macro for typesetting a set
like this: <tt>\set{x \in N | x > 0}</tt>.

Lines can be numbered by setting <tt>\NumberProgramstrue</tt>
and numbering turned off with <tt>\NumberProgramsfalse</tt>

== Source Code Formatting using the <tt>Listings</tt> package ==
(See the [[LaTeX/Packages/Listings | Listings package reference page]] for more information.)

A complete reference manual can be found at http://tug.ctan.org/tex-archive/macros/latex/contrib/listings/listings.pdf

This is a basic example for some Pascal code:
<source lang="latex">
\documentclass{article}
\usepackage{listings}             % Include the listings-package
\begin{document}
\lstset{language=Pascal}          % Set your language (you can change the language for each code-block optionally)

\begin{lstlisting}[frame=single]                % Start your code-block
for i:=maxint to 0 do
begin
{ do nothing }
end;
Write(’Case insensitive ’);
Write(’Pascal keywords.’);
\end{lstlisting}

\end{document}
</source>
[[File:Latex_sample_code.PNG]]

= References =
*<cite id=refOfficialManual> The official manual for the algorithms package, Rogério Brito (2009), http://mirrors.ctan.org/macros/latex/contrib/algorithms/algorithms.pdf</cite>

{{-}}
{{TODO|write more and add some pictures for example code; how to generate pngs?}}

<noinclude>
{{LaTeX/Bottom|Glossary|Letters}}
</noinclude>
