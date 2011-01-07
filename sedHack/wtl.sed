#!/bin/sed -f

#code
/{{code\:Output|\([a-zA-Z ]*\)/ {
	N
	s/{{code\:Output|\([a-zA-Z ]*\)|<source lang=\"\([a-zA-Z]*\)\">/\\lstset{basicstyle=\scriptsize, numbers=left, captionpos=b, tabsize=4}\n\\begin{lstlisting}[caption=\1,language={\2},\nxleftmargin=15pt, label=lst:\1]\n/
	s/{{code\:Output|\([a-zA-Z ]*\)\n|<source lang=\"\([a-zA-Z]*\)\">/\\lstset{basicstyle=\scriptsize, numbers=left, captionpos=b, tabsize=4}\n\\begin{lstlisting}[caption=\1,language={\2},\nxleftmargin=15pt, label=lst:\1]\n/
	}

#code basic
/{{code\:Basic|\([a-zA-Z ]*\)/ {
	N
	s/{{code\:Basic|\([a-zA-Z ]*\)|<source lang=\"\([a-zA-Z]*\)\">/\\lstset{basicstyle=\scriptsize, numbers=left, captionpos=b, tabsize=4}\n\\begin{lstlisting}[caption=\1,language={\2},\nxleftmargin=15pt, label=lst:\1]\n/
	s/{{code\:Basic|\([a-zA-Z ]*\)\n|<source lang=\"\([a-zA-Z]*\)\">/\\lstset{basicstyle=\scriptsize, numbers=left, captionpos=b, tabsize=4}\n\\begin{lstlisting}[caption=\1,language={\2},\nxleftmargin=15pt, label=lst:\1]\n/
	}

#code example
s/{{bcode\:Example|\([^}]*\)}}$/\\lstset{basicstyle=\scriptsize, numbers=left, captionpos=b, tabsize=4}\n\\begin{lstlisting}[language={bash},\nxleftmargin=15pt]\n\1\n\\end{lstlisting}/
	

#label
#/label=lst:[a-zA-Z ]*\]/ {
#	s/ //g
#}


#end of listing
s/<\/source>}}/\n\\end{lstlisting}\n/g
s/<\/source>/\n\\end{lstlisting}\n/g

#output
s/|\([a-zA-Z ]*\)|}}/Output:\n\\scriptsize\n\\begin{verbatim}\n\1 \n\\end{verbatim}\n\\normalsize/g

#subsubsection
s/====\([a-zA-Z:\'. ]*\)====/\\paragraph{\1}/g

#subsection
s/===\([a-zA-Z:\'. ]*\)===/\\subsubsection{\1}/g

#section
s/==\([a-zA-Z:.\' ]*\)==/\\subsection{\1}/g

#bolditalic
s/'''''\([a-zA-Z0-9\:. _-!]*\)'''''/\\\textit{\\textbf{\1}}/g

#bold
s/'''\([a-zA-Z0-9\:. _-\!]*\)'''/\\textbf{\1}/g

#bold
s/''\([a-zA-Z0-9\:. _-]*\)''/\\textit{\1}/g

#links [asdfsdf|asdf]
s/\[\[[a-zA-Z \/-]*\|\([a-zA-Z \/-]\)\]\]/\1/g

#description
s/\;\(-[a-z]\) \([a-zA-Z\:\\{}]*\) /\\item\[\1 \2\] /g

#delete <br/>
s/<br\/>//g

#html chars
s/&ndash;/-/g
s/&mdash;/-/g
s/&nbsp;/ /g
s/&amp;/&/g
s/\$/\\$/g

#underscore
s/\_/\\_/g

#tt html tag
s/:<tt>/\\texttt{/g
s/<\/tt>/}/g

#tabel
#s/.{\| class=wikitable*$/\\begin{tabular}/g
#s/\|\}/\\end{tabular}/g
