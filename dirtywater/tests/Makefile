OCAMLMAKEFILE := ../OCamlMakefile

SOURCES := ../helpers.ml \
           ../debug.ml \
           ../types.ml \
           ../base.ml \
	   ../parser.mly \
           ../lexer.mll \
           ../container.ml \
           ../character_collection.ml \
           ../player_collection.ml \
	   ../time.ml \
           ../events.ml \
           ../location_collection.ml \
	   ../connection_collection.ml \
           ../race_collection.ml \
           ../tangible_collection.ml \
           ../template_collection.ml \
           ../state.ml \
           ../tangible.ml \
	   ../creature.ml \
           ../location.ml \
	   ../character.ml \
           tests.ml
RESULT := test
LIBS = unix str dynlink
PACKS = extlib

include $(OCAMLMAKEFILE)

clean::
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

all:
	(./test)
