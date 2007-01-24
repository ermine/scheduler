OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES		= heapqueue.ml scheduler.ml
THREADS		= yes
RESULT		= scheduler

include ../../Makefile.global
include ../Makefile.inc

all: ncl bcl

test: test.ml
	ocamlfind ocamlc -thread -package unix,threads -linkpkg \
	   scheduler.cma test.ml -o test

include $(OCAMLMAKEFILE)

