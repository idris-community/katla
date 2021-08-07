INTERACTIVE ?= --interactive

all: build run

build:
	idris2 --build tests.ipkg

run:
	./build/exec/runtests ../../../build/exec/katla $(INTERACTIVE) --failure-file failures

.PHONY: all build run
