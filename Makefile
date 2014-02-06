VERSION = 0.1.1

SOURCES := src/helpers.ml \
           src/debug.ml \
	   src/types.ml \
	   src/mud_string.ml \
	   src/base.ml \
	   src/container.ml \
	   src/time.ml \
	   src/events.ml \
	   src/parser.mly \
	   src/lexer.mll \
	   src/player_collection.ml \
	   src/character.ml \
	   src/character_collection.ml \
	   src/location_collection.ml \
	   src/connection_collection.ml \
	   src/template_collection.ml \
	   src/tangible.ml \
           src/creature.ml \
	   src/race.ml \
	   src/race_collection.ml \
	   src/location.ml \
	   src/template.ml \
	   src/ai.ml \
	   src/player.ml \
	   src/load.ml \
	   src/connection.ml \
	   src/scheduler.ml \
	   src/server.ml \
	   src/convenience.ml \
	   src/main.ml \
	   data/state/locations/rooms.ml \
	   data/tangibles/items.ml \
           data/races/human.ml
RESULT = mud
LIBS = unix str dynlink
OCAMLBCFLAGS := -g -warn-error +a
YFLAGS := -v
#PACKS = pxp
PACKS = extlib
USE_CAMLP4 := yes

-include OCamlMakefile

what:
	echo $(DINCFLAGS)

lines:
	wc $(SOURCES) $(INTERFACES) $(OTHERSOURCES)

print:
	echo $(SOURCES) $(INTERFACES) $(OTHERSOURCES) | tr ' ' '\n' | sort | \
	xargs a2ps $(A2PSOPTIONS)

package: clean
	(cp -r . ../mud-$(VERSION) && cd .. && tar -zcf mud-$(VERSION).tar.gz \
	mud-$(VERSION) && rm -rf mud-$(VERSION))

clean:: clean-test

clean-test:
	cd tests && $(MAKE) clean

test:
	cd tests && $(MAKE)
