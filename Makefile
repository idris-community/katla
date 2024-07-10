project:build/exec/katla

.PHONY:

build/exec/katla: .PHONY
	idris2 --build katla.ipkg

build/exec/katla-pandoc: .PHONY
	idris2 --build katla-pandoc.ipkg

install: build/exec/katla build/exec/katla-pandoc
	idris2 --install katla.ipkg
	idris2 --install katla-pandoc.ipkg
	cp -R build/exec/* ~/.idris2/bin/

test: .PHONY
	make -C tests

doc:
	idris2 --mkdoc katla.ipkg

clean:
	idris2 --clean katla.ipkg
