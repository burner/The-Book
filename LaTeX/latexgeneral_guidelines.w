><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>
During this guide we have seen what it is possible to do and how this can be achieved, but the question is: I want to write a proper text with LaTeX, what to do then? Where should I start from? This is a short step-by-step guide about how to start a document properly, keeping a good high-level structure. This way it will be very easy to make modifications even when the document is almost finished. These are all just suggestions, but you might take inspiration from that to create your own document.

==Project structure==
Create a clear structure of the whole project this way:
# create a directory only for the project. We'll refer to that in the following parts as the ''root directory''
# create two other directories inside the root, one for LaTeX documents, the other one for images. Since you'll have to write their name quite often, choose short names. A suggestion would be simply ''tex'' and ''img''.
# create your document (we'll call it document.tex, but you can use the name you prefer) and your own package (for example ''mystyle.sty''); this second file will help you to keep the code cleaner.

If you followed all those steps, these files should be in your root directory, using "/" for each directory:
 ./document.tex
 ./mystyle.sty
 ./tex/
 ./img/
nothing else.

==The file <tt>mystyle.sty</tt>==

Instead of putting all the packages you need at the beginning of your document as you could, the best way is to load all the packages you need inside another dummy package called ''mystyle'' you will create just for your document. The good point of doing this is that you will just have to add one single <tt>\usepackage</tt> in your document, keeping your code much cleaner. Moreover, all the info about your style will be within one file, so when you will start another document you'll just have to copy that file and include it properly, so you'll have exactly the same style you have used.

Creating your own style is very simple: create a file called <tt>mystyle.sty</tt> (you could name it as you wish, but it has to end with ".sty"). Write at the beginning:
<source lang="latex">
\ProvidesPackage{mystyle}
</source>
Then add all the packages you want with the standard command <tt>\usepackage{...}</tt> as you would do normally, change the value of all the variables you want, etc. It will work like the code you put here would be copied and pasted within your document. 

For a list of several packages you can use, see  the List of Packages section.

==The main document <tt>document.tex</tt>==

Then create a file called <tt>document.tex</tt>; this will be the main file, the one you will compile, even if you shouldn't need to edit it very often because you will be working on other files. It should be looking like this (it's the sample code for a ''report'', but you might easily change it for an ''article'' or whatever else):
<source lang="latex">
\documentclass[12pt,a4paper]{report}
\usepackage{graphicx}
\usepackage{ifpdf}
\ifpdf
   % put here packages only for the PDF:
   \DeclareGraphicsExtensions{.pdf,.png,.jpg,.mps}
   \usepackage{hyperref} 
\else
   % put here packages only for the DVI:
\fi

% put all the other packages here:

\usepackage{mystyle}

\begin{document}

\input{./tex/title.tex}
%\maketitle
\tableofcontents
\listoffigures
\listoftables

\input{./tex/intro.tex}
\input{./tex/main_part.tex}
\input{./tex/conclusions.tex}

\appendix
\input{./tex/myappendix.tex}


% Bibliography:
\clearpage
\addcontentsline{toc}{chapter}{Bibliography}
\input{./tex/mybibliography.tex}

\end{document}
</source>
Here a lot of code expressed in previous sections has been used. At the beginning there is the header discussed in the [[LaTeX/Tips_and_Tricks|Tips & Tricks]] section, so you will be able to compile in both DVI and PDF. Then you import the only package you need, that is your ''mystyle.sty'' (note that in the code it has to be imported without the extension), then your document starts. Then it inserts the title: we don't like the output of <tt>\maketitle</tt> so we created our own, the code for it will be in a file called <tt>title.tex</tt> in the folder called <tt>tex</tt> we created before. How to write it is explained in the [[LaTeX/Title Creation|Title Creation]] section. Then tables of contents, figure and tables are inserted. If you don't want them, just comment out those lines. Then the main part of the document in inserted. As you can see, there is no text in <tt>document.tex</tt>: everything is in other files in the <tt>tex</tt> directory so that you can easily edit them. We are separating our text from the structural code, so we are improving the "What You See is What You Mean" nature of LaTeX. Then we can see the appendix and finally the Bibliography. It is in a separated file and it is manually added to the table of contents using a tip suggested in the [[LaTeX/Tips_and_Tricks|Tips & Tricks]].

Once you created your <tt>document.tex</tt> you won't need to edit it anymore, unless you want to add other files in the <tt>tex</tt> directory, but this is not going to happen very often. Now you can write your document separating it in as many files as you want and adding many pictures without getting confused: thanks to the rigid structure you gave to the project, you will be able to keep track of all your edits clearly.

A suggestion: do not call your files like "chapter_01.tex" or "figure_03.png", i.e. try to avoid using numbers in file-names: if the numbering LaTeX gives them automatically is different from the one you gave (and this will likely happen) you will get really confused. When naming a file, stop for a second, think about a short name that can fully explain what is inside the file without being ambiguous, it will let you save a lot of time as soon as the document gets larger.

==Writing your document==
While writing, whenever you have to take a decision about formatting, define your own command for it and add it to your <tt>mystyle.sty</tt>:let LaTeX work for you.
If you do so, it will be very easy to change it if you change your mind. Here is an example: if you are writing a book about Mathematics and you have to use
vectors, you have to decide how they will look. There are several different standards, used in many books.
If ''a'' is a vector, some people like to add an arrow over it (<math>\vec{a}</math>), other people write it underlined (''<u>a</u>''); another common
version is to write it bold ('''a'''). Let us assume you want to write your vectors with an arrow over them; then add the following line in your <tt>mystyle.sty</tt>.
<source lang="latex">
\newcommand{\myvec}[1]{\vec{#1}}
</source>
and write your vectors inside the new <tt>\myvec{...}</tt> command. You can call it as you wish, but you'd better choose a short name because you will probably write it very often.
Then, if you change your mind and you want your vectors to look differently you just have to change the definition of your <tt>\myvec{...}</tt>.
Use this approach whenever you can: this will save you a lot of time.

<noinclude>
{{LaTeX/Bottom|Tips and Tricks|Export To Other Formats}}
</noinclude>
