OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES := scheduler.ml
THREADS := yes
OCAMLFLAGS = -cclib '-g'
OCAMLLDFLAGS = -cclib -g -verbose
RESULT := scheduler

all: ncl

include $(OCAMLMAKEFILE)

