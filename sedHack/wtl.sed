#!/bin/sed -f

#code
s/{{code\:Output|\([a-zA-Z ]*\)$/\1/g

#output
s/|\([a-zA-Z ]*\)|}}/\\scriptsize\n\\\begin{verbatim}\n\1 \n\\end{verbatim}\n\\normalsize/g

#subsubsection
s/====\([a-zA-Z: ]*\)====/\\subsubsection{\1}/g

#subsection
s/===\([a-zA-Z: ]*\)===/\\subsection{\1}/g

#section
s/==\([a-zA-Z: ]*\)==/\\section{\1}/g

#bolditalic
s/'''''\([a-zA-Z\: _-]*\)'''''/\\\textit{\\textbf{\1}}/g

#bold
s/'''\([a-zA-Z\: _-]*\)'''/\\textbf{\1}/g

#bold
s/''\([a-zA-Z\: _-]*\)''/\\textit{\1}/g

#links [asdfsdf|asdf]
s/\[\[[a-zA-Z \/-]*\|\([a-zA-Z \/-]\)\]\]/\1/g
