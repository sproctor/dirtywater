OBJECTS := $(patsubst %.ml,%.cmo,$(wildcard */*.ml))
OCAMLC=ocamlc
OFLAGS=-I ../_build/src -c

all: $(OBJECTS)

%.cmo: %.ml
	$(OCAMLC) $(OFLAGS) $< -o $@
