VERSION = 0.1.1

SOURCES := helpers.ml \
           debug.ml \
	   types.ml \
	   mud_string.ml \
	   base.ml \
	   container.ml \
	   time.ml \
	   events.ml \
	   parser.mly \
	   lexer.mll \
	   tangible_collection.ml \
	   player_collection.ml \
	   character_collection.ml \
	   location_collection.ml \
	   connection_collection.ml \
	   template_collection.ml \
	   race_collection.ml \
	   state.ml \
	   tangible.ml \
           creature.ml \
	   character.ml \
	   location.ml \
	   race.ml \
	   template.ml \
	   ai.ml \
	   player.ml \
	   load.ml \
	   connection.ml \
	   scheduler.ml \
	   server.ml \
	   main.ml \
	   data/state/tangibles/tangibles.ml \
	   data/state/locations/rooms.ml \
	   data/tangibles/rock.ml \
           data/races/human.ml
RESULT = mud
LIBS = unix str dynlink # stream
OCAMLBCFLAGS := -g
YFLAGS := -v
#PACKS = pxp
USE_CAMLP4 := yes

include OCamlMakefile

what:
	echo $(DINCFLAGS)

all: set-perms make-dep-dir nc

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
