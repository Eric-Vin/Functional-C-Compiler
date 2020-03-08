STACK = stack
EXE   = c-compiler-exe

.PHONY: all

.SILENT:

all: clean_test build test

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
	rm -rf test/output_files/*