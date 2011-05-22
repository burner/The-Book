><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>

== Intro ==

LaTeX has specific features for teachers. We present the '''exam''' class<ref>[http://www-math.mit.edu/~psh/exam/examdoc.pdf examdoc] Using the exam document class</ref> which is useful for designing exams and exercise with solutions. Interested people could also have a look at the '''probsoln''' package<ref>[http://www.tex.ac.uk/tex-archive/macros/latex/contrib/probsoln/probsoln.pdf Probsoln] creating problem sheets optionally
with solutions</ref> or the '''mathexm''' document class<ref>http://mat140.bham.ac.uk/~richard/programming/tex/exams/msexdoc.pdf</ref>.

== The exam class ==

We present the '''exam''' class. 
The exam class is well suited to design exams with solutions. You just have to specify in the preamble if you want the solutions to be printed or not. You can also count the number of points. 

=== Preamble ===

In the preamble you can specify the following lines : 
{{LaTeX/Usage|code=
\documentclass[a4paper,11pt]{exam}
\printanswers % If you want to print answers
% \noprintanswers % If you don't want to print answers
\addpoints % if you want to count the points
% \noaddpoints % if you don't want to count the points
% Specifies the way question are displayed:
\qformat{\textbf{Question\thequestion}\quad(\thepoints)\hfill}
\usepackage{color} % defines a new color
\definecolor{SolutionColor}{rgb}{0.8,0.9,1} % light blue
\shadedsolutions % defines the style of the solution environment
% \framedsolutions % defines the style of the solution environment
% Defines the title of the solution environment:
\renewcommand{\solutiontitle}{\noindent\textbf{Solution:}\par\noindent}
}}

You can replace the 3 first lines with the following : 
{{LaTeX/Usage|code=
\documentclass[a4paper,11pt,answers,addpoints]{exam}
}}

=== Document ===

* The exam is included in the '''questions''' environment.
* The command '''\question''' introduces a new question. 
* The number of points is specified in squared brackets.
* The solution is given in the '''solution''' environment. It appears only if '''\printanswers''' or '''answers''' as an option of the '''\documentclass''' are specified in the preamble.

Here is an example : 

<source lang="latex">
\begin{questions} % Begins the questions environment
\question[2] What is the solution ? % Introduces a new questions which gives 2 points
\begin{solution} 
Here is the solution 
\end{solution}
\question[5] What is your opinion ?
\begin{solution}
This is my opinion
\end{solution}
\end{questions}
</source>

It is also possible to add stuff only if answers are printed using the '''\ifprintanswers''' command. 

<source lang = "latex">
\ifprintanswers
Only if answers are printed
\else
Only if answers are not printed
\fi
</source>

=== Introduction ===

The macro '''\numquestions''' gives the total number of questions. 
The macro '''\numpoints''' gives the total number of points.

<source lang="latex">
\begin{minipage}{.8\textwidth}
This exam includes \numquestions\ questions. The total number of points is \numpoints.
\end{minipage}
</source>

The backslash after '''\numquestion''' prevents the macro from gobbling the following whitespace as it normally would.

== References ==

{{reflist}}

<noinclude>
{{LaTeX/Bottom|Letters|Importing Graphics}}
</noinclude>
