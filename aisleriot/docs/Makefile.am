# Each game needs a separate html help file with the same name as its
# scm file. These are generated from docbook sgml sources using
# the Modular DocBook stylesheets. Four further html files are 
# generated to tie the game files together into a tree for the 
# help browser.

helpdir = $(datadir)/gnome/help/aisleriot/C

help_DATA = index.html intro.html glossary.html gpl.html \
	camelot.html canfield.html clock.html \
	eagle-wing.html fortunes.html klondike.html \
	odessa.html osmosis.html spider.html \
	camelot-goal.gif clock-rules.gif docbook.css topic.dat

# These are the sources:

help_SGML = aisleriot.sgml definitions.sgml gpl.sgml 
	camelot.sgml canfield.sgml clock.sgml \
	eagle-wing.sgml fortunes.sgml klondike.sgml \
	odessa.sgml osmosis.sgml spider.sgml

EXTRA_DIST = $(help_DATA) $(help_SGML)

# Automake does not understand how to build docbook sources.
# The stylesheets being used to build the html are available from 
# ftp://ftp.cygnus.com/pub/home/rosalia/ (aka Mark Galassi):

DB_STYLESHEET=/usr/lib/sgml/stylesheets/cygnus-both.dsl\#html

%.html: $(help_SGML)
	jade -t sgml -ihtml -d $(DB_STYLESHEET) aisleriot.sgml
