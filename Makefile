.PHONY: project src install test doc clean

project: build/exec/katla build/exec/katla-pandoc

src: src/**/*.idr

build/exec/katla: src 
	idris2 --build katla.ipkg

build/exec/katla-pandoc:
	idris2 --build katla-pandoc.ipkg

install: build/exec/katla build/exec/katla-pandoc
	idris2 --install katla.ipkg
	idris2 --install katla-pandoc.ipkg
	cp -R build/exec/* ~/.idris2/bin/

test:
	make -C tests

doc: src
	idris2 --mkdoc katla.ipkg

clean:
	idris2 --clean katla.ipkg
