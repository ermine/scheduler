OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES := scheduler.ml
THREADS := yes
RESULT := scheduler

all: ncl bcl

include $(OCAMLMAKEFILE)

