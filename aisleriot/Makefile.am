scoredir=$(localstatedir)/games
cardimagedir = $(datadir)/pixmaps/cards
icondir = $(datadir)/pixmaps
Gamesdir  = $(datadir)/gnome/apps/Games
typesdir  = $(datadir)/sol-games

SUBDIRS=docs pixmaps

INCLUDES = \
	-DGNOMELOCALEDIR=\""$(datadir)/locale"\" \
	-I$(includedir)				 \
	$(GNOME_INCLUDEDIR)                      \
	$(GUILE_INCS)

bin_PROGRAMS = sol

sol_SOURCES = 		\
	card.h		\
	cscmi.h		\
	scroll-menu.c	\
	scroll-menu.h	\
	dialog.h	\
	draw.h		\
	events.h	\
	menu.h		\
	press_data.h	\
	slot.h		\
	splash.h	\
	sol.h		\
	sol.c		\
	splash.c	\
	slot.c		\
	dialog.c	\
	cscmi.c		\
	events.c	\
	press_data.c	\
	draw.c		\
	menu.c		\
	card.c

sol_LDADD = -lgdk_imlib $(GNOME_LIBDIR) $(GNOMEUI_LIBS) \
	../freecell/gdk-card-image/libgdkcardimage.la \
	$(INTLLIBS) $(GUILE_LIBS)

types_DATA  = 		\
	agnes.scm	\
	auld_lang_syne.scm	\
	bakers_dozen.scm	\
	bakers_game.scm	\
	beleaguered_castle.scm	\
	block_ten.scm	\
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
	straight_up.scm	\
	streets_and_alleys.scm	\
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

cardimage_DATA = baize.png splash.png

icon_DATA = gnome-aisleriot.png

EXTRA_DIST = AUTHORS $(types_DATA) $(cardimage_DATA) \
	$(icon_DATA) sol.desktop SCHEME_GAMES.h \
	template.scm translatable_game_names.h


sysdir = $(datadir)/gnome/apps/Games
sys_DATA = sol.desktop

