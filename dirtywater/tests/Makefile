OCAMLMAKEFILE := ../OCamlMakefile

SOURCES := ../helpers.ml ../debug.ml ../types.ml ../base.ml \
	   ../parser.ml ../lexer.ml ../container.ml ../tangible.ml \
	   ../creature.ml ../character_collection.ml ../player_collection.ml \
	   ../time.ml ../events.ml ../location_collection.ml \
	   ../connection_collection.ml ../state.ml ../location.ml \
	   ../character.ml tests.ml
RESULT := test
LIBS = unix str

all: set-perms make-dep-dir nc test-run

include $(OCAMLMAKEFILE)

clean: clean-test

clean-test:
	-rm -f *.d
lines:
	wc $(SOURCES) $(INTERFACES) $(OTHERSOURCES)

print:
	echo $(SOURCES) $(INTERFACES) $(OTHERSOURCES) | tr ' ' '\n' | sort | \
	xargs a2ps $(A2PSOPTIONS)

set-perms:
	(chgrp -Rf mud .; chmod -Rf g+w .) || echo

make-dep-dir:
	-@test -d ._d || (mkdir ._d; chgrp mud ._d; chmod 775 ._d)

test-run:
	(./test)
