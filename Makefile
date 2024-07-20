.PHONY: project src install test doc clean

project: build/exec/katla

src: src/**/*.idr

build/exec/katla: src 
	idris2 --build katla.ipkg

install: build/exec/katla
	idris2 --install katla.ipkg
	cp -R build/exec/* ~/.idris2/bin/

test:
	make -C tests

doc: src
	idris2 --mkdoc katla.ipkg

clean:
	idris2 --clean katla.ipkg
