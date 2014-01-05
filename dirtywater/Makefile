OCAMLMAKEFILE := OCamlMakefile

VERSION = 0.1.0

include Makefile.debug

SOURCES := helpers.ml \
           debug.ml \
	   types.ml \
	   base.ml \
	   container.ml \
	   tangible.ml \
	   building.ml \
           creature.ml \
	   time.ml \
	   events.ml \
	   parser.mly \
	   lexer.mll \
	   player_collection.ml \
	   character_collection.ml \
	   location_collection.ml \
	   connection_collection.ml \
	   state.ml \
	   character.ml \
	   location.ml \
	   ai.ml \
	   make_world.ml \
	   player.ml \
	   connection.ml \
	   scheduler.ml \
	   server.ml \
	   main.ml
RESULT := mud
LIBS = unix str
THREADS = 1
OCAMLBCFLAGS := -g

all: set-perms make-dep-dir bc

include $(OCAMLMAKEFILE)

lines:
	wc $(SOURCES) $(INTERFACES) $(OTHERSOURCES)

print:
	echo $(SOURCES) $(INTERFACES) $(OTHERSOURCES) | tr ' ' '\n' | sort | \
	xargs a2ps $(A2PSOPTIONS)

package: clean
	(cp -r . ../mud-$(VERSION) && cd .. && tar -zcf mud-$(VERSION).tar.gz \
	mud-$(VERSION) && rm -rf mud-$(VERSION))

clean: clean-test clean-other

clean-test:
	cd tests && $(MAKE) clean

clean-other:
	rm -f parser.ml
set-perms:
	(chgrp -Rf mud .; chmod -Rf g+w .) || echo -n

make-dep-dir:
	-@test -d ._d || (mkdir ._d; chgrp mud ._d; chmod 775 ._d)

test: test-run

test-run:
	cd tests && $(MAKE)
