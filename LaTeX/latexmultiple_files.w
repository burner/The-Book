><noinclude>{{LaTeX/Top}}</noinclude>
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1"><noinclude>{{LaTeX/Top}}</noinclude>
As the typing goes on, your LaTeX file could get big and confusing, so it could be a good idea to split it into several other documents. For example, if you are writing a book, you could consider writing every chapter in its own <tt>.tex</tt> file. LaTeX makes this very easy thanks to the command:
<source lang="latex">
\input{filename}
</source>
While processing the document, the compiler will simply read the content of ''filename.tex'' and put it within the document instead of the ''input'' command. This way you can put all the formatting options in your "root" document and then ''input'' other files containing only text and very basic commands such as <code>\section</code>, etc. The code will be much cleaner and more readable.

Another method of including a file is to use <code>\include{filename}</code>. However, you cannot nest <code>\include</code> statements within a file added via <code>\include</code>, whereas you can use input. Note that this will also have an effect on the number of compiles you'll need to do. Note that using <code>\include</code> will force a page break, whereas the ''input'' command does not.

== Using different paths ==

When referring to an external file in the LaTeX document source, if you just write a filename the compiler will look for it in the same directory of the source. In general you can refer to any document on your system, using both relative and absolute paths. In general, you might want to make your source portable (to another computer or to a different location of you harddisk), so always use relative paths.
A relative path is always defined in terms of the current directory where you are running the compiler, i.e. the directory where the source you are compiling is. LaTeX uses the standard *nix notation: with a simple dot '''.''' you refer to the current directory, and by two dots '''..''' you refer to the previous directory, that is the upper one in the file system tree. By a slash '''/''' you can separate names of different directories. So by '''./''' you refer to the current directory, by '''../''' you refer to the previous directory, by '''../../''' you refer to two upper directories in the filesystem tree.
Writing
<source lang="latex">
\input{./filename.tex}
</source>
will have ''exactly'' the same effect as writing
<source lang="latex">
\input{filename.tex}
</source>
but if you want to put all your files in a different directory called ''myfiles'', you can refer to that file by
<source lang="latex">
\input{./myfiles/filename.tex}
</source>
This lets you put your files wherever you want. Do not leave empty spaces in the filenames: they can cause ambiguous behavior. Use underscores '''_''' instead.

It should be noted that LaTeX uses forward slashes '''/''' even when on a Microsoft Windows platform that normally uses backslashes '''\'''.

== Using \includeonly ==

When writing a large document, it is sometimes useful to work on one section of the document. In LaTeX, the command
<source lang="latex">
\includeonly{filename1,filename2,...}
</source>
includes only the files specified between the brackets when used in the preamble of the document.

This requires that there are <code>\include</code> commands in the document specifying these files. The filename should be written without the <tt>.tex</tt> file extension
<source lang="latex">
\include{filename1}
\include{filename2}
</source>

==Subfiles package==
A disadvantage of using <code>\input</code> and <code>\include</code> is that only the "root" document can be compiled and not the "child" documents individually. The package subfiles resolves this problem.

In the "root" document the package must be loaded as:
<source lang="latex">
\usepackage{subfiles}
</source>

Instead of using <code>\input</code> and <code>\include</code>, "child" documents must be loaded as follows:
<source lang="latex">
\subfile{filename}
</source>

The "child" documents must start with the following statements:
<source lang="latex">
\documentclass[rootdocument.tex]{subfiles} 
\begin{document}
</source>
and end with:
<source lang="latex">
\end{document}
</source>

==Merging in other, compiled PDFs==

If you need to insert an existing, possibly multi-page, PDF file into your LaTeX document, whether or not the included PDF was compiled with LaTeX or another tool, consider using the [http://www.ctan.org/tex-archive/macros/latex/contrib/pdfpages/ pdfpages package]. In the preamble, include the package:

<source lang="latex">
\usepackage[final]{pdfpages}
</source>

Then, to insert pages 3 through 6 from some file insertme.pdf, use:

<source lang="latex">
\includepdf[pages=3-6]{insertme.pdf}
</source>

To insert the whole of insertme.pdf:

<source lang="latex">
\includepdf[pages=-]{insertme.pdf}
</source>

For full functionality, compile the output with pdflatex.

==External Links==
* [http://tug.ctan.org/tex-archive/macros/latex/contrib/subfiles/subfiles.pdf Subfiles package documentation]
* [http://mirror.ctan.org/macros/latex/contrib/pdfpages/pdfpages.pdf pdfpages package documentation]

<noinclude>
{{LaTeX/Bottom|Customizing LaTeX|Collaborative Writing of LaTeX Documents}}
</noinclude>
