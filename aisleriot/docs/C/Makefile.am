# Each game needs a separate html help file with the same name as its
# scm file. These are generated from docbook sgml sources using
# the Modular DocBook stylesheets. Four further html files are 
# generated to tie the game files together into a tree for the 
# help browser.

helpdir = $(datadir)/gnome/help/aisleriot/C

help_DATA = index.html	\
	intro.html	\
	glossary.html	\
	topic.dat	\
	docbook.css	\
	camelot-goal.png	\
	clock-rules.png	\
	agnes.html	\
	auld-lang-syne.html	\
	bakers-dozen.html	\
	bakers-game.html	\
	beleaguered-castle.html	\
	block-ten.html	\
	bristol.html	\
	camelot.html	\
	canfield.html	\
	clock.html	\
	cover.html	\
	diamond-mine.html	\
	doublets.html	\
	eagle-wing.html	\
	easthaven.html	\
	eight-off.html	\
	elevator.html	\
	escalator.html	\
	first-law.html	\
	fortress.html	\
	fortunes.html	\
	fourteen.html	\
	freecell.html	\
	gay-gordons.html	\
	glenwood.html	\
	golf.html	\
	gypsy.html	\
	helsinki.html	\
	hopscotch.html	\
	isabel.html	\
	jamestown.html	\
	jumbo.html	\
	kansas.html	\
	king-albert.html	\
	klondike.html	\
	labyrinth.html	\
	lady-jane.html	\
	maze.html	\
	monte-carlo.html	\
	neighbor.html	\
	odessa.html	\
	osmosis.html	\
	peek.html	\
	pileon.html 	\
	poker.html	\
	quatorze.html	\
	royal-east.html	\
	scorpion.html	\
	scuffle.html	\
	seahaven.html	\
	sir-tommy.html	\
	spider.html	\
	spiderette.html	\
	straight-up.html	\
	streets-and-alleys.html	\
	thieves.html	\
	thirteen.html	\
	thumb-and-pouch.html	\
	treize.html	\
	union-square.html	\
	valentine.html	\
	westhaven.html	\
	whitehead.html	\
	will-o-the-wisp.html	\
	yield.html	\
	yukon.html 	\
	zebra.html 

# These are the sources:

help_SGML = \
	aisleriot.sgml	\
	definitions.sgml	\
	agnes.sgml	\
	auld_lang_syne.sgml	\
	bakers_dozen.sgml	\
	bakers_game.sgml	\
	beleaguered_castle.sgml	\
	block_ten.sgml	\
	bristol.sgml	\
	camelot.sgml	\
	canfield.sgml	\
	clock.sgml	\
	cover.sgml	\
	diamond_mine.sgml	\
	doublets.sgml	\
	eagle-wing.sgml	\
	easthaven.sgml	\
	eight_off.sgml	\
	elevator.sgml	\
	escalator.sgml	\
	first_law.sgml	\
	fortress.sgml	\
	fortunes.sgml	\
	fourteen.sgml	\
	freecell.sgml	\
	gay_gordons.sgml	\
	glenwood.sgml	\
	golf.sgml	\
	gypsy.sgml	\
	helsinki.sgml	\
	hopscotch.sgml	\
	isabel.sgml	\
	jamestown.sgml	\
	jumbo.sgml	\
	kansas.sgml	\
	king_albert.sgml	\
	klondike.sgml	\
	labyrinth.sgml	\
	lady_jane.sgml	\
	maze.sgml	\
	monte_carlo.sgml	\
	neighbor.sgml	\
	odessa.sgml	\
	osmosis.sgml	\
	peek.sgml	\
	pileon.sgml 	\
	poker.sgml	\
	quatorze.sgml	\
	royal_east.sgml	\
	scorpion.sgml	\
	scuffle.sgml	\
	seahaven.sgml	\
	sir_tommy.sgml	\
	spider.sgml	\
	spiderette.sgml	\
	straight_up.sgml	\
	streets_and_alleys.sgml	\
	thieves.sgml	\
	thirteen.sgml	\
	thumb_and_pouch.sgml	\
	treize.sgml	\
	union_square.sgml	\
	valentine.sgml	\
	westhaven.sgml	\
	whitehead.sgml	\
	will_o_the_wisp.sgml	\
	yield.sgml	\
	yukon.sgml 	\
	zebra.sgml 


# help_SGML = aisleriot.sgml definitions.sgml \
# 	camelot.sgml canfield.sgml clock.sgml doublets.sgml \
# 	eagle-wing.sgml elevator.sgml fortunes.sgml \
# 	kansas.sgml pileon.sgml first_law.sgml poker.sgml \
# 	fourteen.sgml klondike.sgml auld_lang_syne.sgml \
# 	bristol.sgml hopscotch.sgml union_square.sgml\
# 	neighbor.sgml odessa.sgml osmosis.sgml spider.sgml \
# 	yukon.sgml westhaven.sgml royal_east.sgml \
# 	beleaguered_castle.sgml thirteen.sgml thieves.sgml \
# 	freecell.sgml scorpion.sgml thumb_and_pouch.sgml \
# 	straight_up.sgml zebra.sgml lady_jane.sgml \
# 	maze.sgml glenwood.sgml gypsy.sgml monte_carlo.sgml \
# 	bakers_dozen.sgml seahaven.sgml valentine.sgml \
# 	cover.sgml labyrinth.sgml scuffle.sgml peek.sgml \
# 	treize.sgml quatorze.sgml escalator.sgml isabel.sgml \
# 	sir_tommy.sgml jamestown.sgml bakers_game.sgml \
# 	eight_off.sgml yield.sgml block_ten.sgml gay_gordons.sgml \
# 	jumbo.sgml

EXTRA_DIST = $(help_DATA) $(help_SGML)

# Automake does not understand how to build docbook sources.
# The stylesheets being used to build the html are available from 
# ftp://ftp.cygnus.com/pub/home/rosalia/ (aka Mark Galassi):

DB_STYLESHEET=/usr/lib/sgml/stylesheets/cygnus-both.dsl\#html

%.html: $(help_SGML)
	-cd $(srcdir) && db2html aisleriot.sgml >/dev/null &&\
	mv aisleriot/*.html . &&\
	rm -rf aisleriot 

