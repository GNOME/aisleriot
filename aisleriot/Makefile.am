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
	auld_lang_syne.scm \
	beleaguered_castle.scm \
	bristol.scm	\
	camelot.scm	\
	canfield.scm	\
	clock.scm	\
	doublets.scm	\
	eagle_wing.scm	\
	elevator.scm	\
	first_law.scm	\
	fortunes.scm	\
	fourteen.scm	\
	freecell.scm	\
	hopscotch.scm	\
	kansas.scm	\
	klondike.scm	\
	lady_jane.scm	\
	neighbor.scm	\
	odessa.scm	\
	osmosis.scm	\
	pileon.scm	\
	royal_east.scm	\
	scorpion.scm	\
	sol.scm		\
	spider.scm	\
	straight_up.scm	\
	thieves.scm     \
	thirteen.scm	\
	thumb_and_pouch.scm \
	union_square.scm \
	westhaven.scm	\
	yukon.scm	\
	zebra.scm

cardimage_DATA = baize.png splash.png

icon_DATA = gnome-aisleriot.png

EXTRA_DIST = AUTHORS $(types_DATA) $(cardimage_DATA) \
	$(icon_DATA) sol.desktop SCHEME_GAMES.h

sysdir = $(datadir)/gnome/apps/Games
sys_DATA = sol.desktop

