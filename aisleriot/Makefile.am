cardimagedir = $(datadir)/pixmaps/cards
icondir = $(datadir)/pixmaps
Gamesdir  = $(datadir)/applications
typesdir  = $(datadir)/sol-games
gladedir = $(datadir)/sol-games/glade

schemadir   = @GCONF_SCHEMA_FILE_DIR@
schema_in_files = aisleriot.schemas.in
schema_DATA = $(schema_in_files:.schemas.in=.schemas)
@INTLTOOL_SCHEMAS_RULE@

SUBDIRS=help

INCLUDES =  \
	-I$(top_srcdir)/libgames-support           \
	-DDATADIR=\""$(datadir)"\"		   \
	$(GNOME_GAMES_CFLAGS)			   \
	@UCB_CFLAGS@				   \
	-DGLADEDIR=\""$(gladedir)"\" \
	-DGNOMELOCALEDIR=\""$(datadir)/locale"\"   \
	$(GUILE_INCS)

bin_PROGRAMS = sol

sol_SOURCES = 		\
	card.h		\
	cscmi.h		\
	dialog.h	\
	draw.h		\
	events.h	\
	menu.h		\
	press_data.h	\
	slot.h		\
	sol.h		\
	sol.c		\
	slot.c		\
	dialog.c	\
	cscmi.c		\
	events.c	\
	press_data.c	\
	draw.c		\
	menu.c		\
	card.c		\
	statistics.c	\
	statistics.h

sol_LDADD = $(GNOME_GAMES_LIBS) \
	../libgames-support/libgames-support.la \
	@UCB_LDFLAGS@ \
	$(GUILE_LIBS) 

glade_DATA = \
	statistics.glade

types_DATA  = 		\
	agnes.scm	\
	auld_lang_syne.scm	\
	bakers_dozen.scm	\
	bakers_game.scm	\
	beleaguered_castle.scm	\
	bristol.scm	\
	camelot.scm	\
	canfield.scm	\
	chessboard.scm	\
	clock.scm	\
	cover.scm	\
	diamond_mine.scm	\
	doublets.scm	\
	eagle_wing.scm	\
	easthaven.scm	\
	eight_off.scm	\
	elevator.scm	\
	escalator.scm	\
	first_law.scm	\
	fortress.scm	\
	fortunes.scm	\
	fourteen.scm	\
	freecell.scm	\
	gay_gordons.scm	\
	glenwood.scm	\
	golf.scm	\
	gypsy.scm	\
	helsinki.scm	\
	hopscotch.scm	\
	isabel.scm	\
	jamestown.scm	\
	jumbo.scm	\
	kansas.scm	\
	king_albert.scm	\
	klondike.scm	\
	labyrinth.scm	\
	lady_jane.scm	\
	maze.scm	\
	monte_carlo.scm	\
	neighbor.scm	\
	odessa.scm	\
	osmosis.scm	\
	peek.scm	\
	pileon.scm	\
	poker.scm	\
	quatorze.scm	\
	royal_east.scm	\
	scorpion.scm	\
	scuffle.scm	\
	seahaven.scm	\
	sir_tommy.scm	\
	sol.scm		\
	spider.scm	\
	spiderette.scm	\
	spider_two_suit.scm \
	spider_one_suit.scm \
	straight_up.scm	\
	streets_and_alleys.scm	\
	ten_across.scm  \
	thieves.scm     \
	thirteen.scm	\
	thumb_and_pouch.scm	\
	treize.scm	\
	union_square.scm	\
	valentine.scm	\
	westhaven.scm	\
	whitehead.scm	\
	will_o_the_wisp.scm	\
	yield.scm	\
	yukon.scm	\
	zebra.scm	\
	plait.scm

cardimage_DATA = baize.png slot.png

icon_DATA = gnome-aisleriot.png gnome-cardgame.png

EXTRA_DIST = AUTHORS $(types_DATA) $(cardimage_DATA) \
	$(icon_DATA) sol.desktop.in freecell.desktop.in \
	template.scm translatable_game_names.h \
	$(schema_in_files) $(glade_DATA)


sysdir = $(datadir)/applications/
sys_in_files = sol.desktop.in freecell.desktop.in
sys_DATA = $(sys_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

DISTCLEANFILES = $(sys_DATA) $(schema_DATA)

if GCONF_SCHEMAS_INSTALL
install-data-local:
	GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $(top_builddir)/aisleriot/$(schema_DATA)
endif
