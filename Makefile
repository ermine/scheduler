OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES := scheduler.ml
THREADS := yes
 RESULT := scheduler

all: native-code-library

include $(OCAMLMAKEFILE)

