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
	$(STACK) test --test-arguments="--timeout=5 --num-threads 1"

clean: clean_test
	$(STACK) clean

purge: clean_test
	$(STACK) purge

clean_test:
	find test/output ! -type d -delete