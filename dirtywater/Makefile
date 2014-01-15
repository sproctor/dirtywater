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
	   player_collection.ml \
	   character.ml \
	   character_collection.ml \
	   location_collection.ml \
	   connection_collection.ml \
	   template_collection.ml \
	   tangible.ml \
           creature.ml \
	   race.ml \
	   race_collection.ml \
	   location.ml \
	   template.ml \
	   ai.ml \
	   player.ml \
	   load.ml \
	   connection.ml \
	   scheduler.ml \
	   server.ml \
	   convenience.ml \
	   main.ml \
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
