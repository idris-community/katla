project:build/exec/katla

.PHONY:

build/exec/katla: .PHONY
	idris2 --build katla.ipkg

test: .PHONY
	make -C tests

doc:
	idris2 --mkdoc katla.ipkg

clean:
	idris2 --clean katla.ipkg
