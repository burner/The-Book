TARGET=pdf

all:$(TARGET)

pdf: 
	pdflatex -hald-on-error thebook.tex
	pdflatex -hald-on-error thebook.tex

show: pdf
	evince thebook.pdf

showf:
	pdflatex -hald-on-error thebook.tex
	evince thebook.pdf

clean:
	rm -rf *.aux *.log *.pdf *.toc

fshow:
	pdflatex -hald-on-error thebook.tex
	evince thebook.pdf
