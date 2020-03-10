STACK = stack
EXE   = c-compiler-exe

.PHONY: all

.SILENT:

all: build test

build:
	$(STACK) build

exec: build
	$(STACK) exec $(EXE) $(target)

test: build
	$(STACK) test

clean: clean_test
	$(STACK) clean

purge: clean_test
	$(STACK) purge

clean_test:
	find test/output_files ! -type d -delete