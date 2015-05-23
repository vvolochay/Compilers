main: show

all: head.tex report.tex
	xelatex -shell-escape head.tex

show: all
	evince head.pdf

clean:
	rm -rf *.aux *.log *.pyg *.pdf
