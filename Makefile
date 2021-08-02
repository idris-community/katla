project:build/exec/katla

.PHONY:

build/exec/katla: .PHONY
	idris2 --build katla.ipkg

temp/Example.pdf: build/exec/katla
	build/exec/katla  ./src/Katla/Engine.idr ./build/ttc/Katla/Engine.ttm \
          | pdflatex -output-directory=temp

clean:
	idris2 --clean katla.ipkg
	rm -f temp/*.aux temp/*.log temp/*.pdf
