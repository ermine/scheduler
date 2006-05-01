OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES		= scheduler.ml
THREADS		= yes
RESULT		= scheduler

include ../../Makefile.global
include ../Makefile.inc

all: ncl bcl

include $(OCAMLMAKEFILE)

