project:build/exec/katla

.PHONY:

build/exec/katla: .PHONY
	idris2 --build katla.ipkg

install: build/exec/katla
	idris2 --install katla.ipkg
	cp -R build/exec/* ~/.idris2/bin/

test: .PHONY
	make -C tests

doc:
	idris2 --mkdoc katla.ipkg

clean:
	idris2 --clean katla.ipkg
