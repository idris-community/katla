project:build/exec/katla

.PHONY:

build/exec/katla: .PHONY
	idris2 --build katla.ipkg

temp/Example.pdf: build/exec/katla
	build/exec/katla
	pdflatex -output-directory=temp temp/Example.tex 

clean:
	idris2 --clean katla.ipkg
	rm -f temp/*.aux temp/*.log temp/*.pdf
