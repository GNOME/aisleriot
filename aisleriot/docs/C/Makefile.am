# Each game needs a separate html help file with the same name as its
# scm file. These are generated from docbook sgml sources using
# the Modular DocBook stylesheets. Four further html files are 
# generated to tie the game files together into a tree for the 
# help browser.

helpdir = $(datadir)/gnome/help/aisleriot/C

help_DATA = index.html intro.html glossary.html gpl.html \
	camelot.html canfield.html clock.html doublets.html \
	eagle-wing.html elevator.html fortunes.html \
	kansas.html klondike.html \
	fourteen.html pileon.html auld-lang-syne.html \
	hopscotch.html scorpion.html thumb-and-pouch.html \
	neighbor.html odessa.html osmosis.html spider.html \
	yukon.html westhaven.html royal-east.html \
	beleaguered-castle.html thirteen.html thieves.html \
	freecell.html bristol.html union-square.html \
	camelot-goal.gif clock-rules.gif docbook.css topic.dat

# These are the sources:

help_SGML = aisleriot.sgml definitions.sgml gpl.sgml 
	camelot.sgml canfield.sgml clock.sgml doublets.sgml \
	eagle-wing.sgml elevator.sgml fortunes.sgml \
	kansas.sgml pileon.sgml \
	fourteen.sgml klondike.sgml auld-lang-syne.sgml \
	bristol.sgml hopscotch.sgml union_square.sgml\
	neighbor.sgml odessa.sgml osmosis.sgml spider.sgml \
	yukon.sgml westhaven.sgml royal-east.sgml \
	beleaguered-castle.sgml thirteen.sgml thieves.sgml \
	freecell.sgml scorpion.sgml thumb-and-pouch.sgml

EXTRA_DIST = $(help_DATA) $(help_SGML)

# Automake does not understand how to build docbook sources.
# The stylesheets being used to build the html are available from 
# ftp://ftp.cygnus.com/pub/home/rosalia/ (aka Mark Galassi):

DB_STYLESHEET=/usr/lib/sgml/stylesheets/cygnus-both.dsl\#html

%.html: $(help_SGML)
	-db2html aisleriot.sgml >/dev/null;\
	mv aisleriot/*.html .;\
	rm -rf aisleriot 

