scoredir=$(localstatedir)/games
cardimagedir = $(datadir)/pixmaps/cards
icondir = $(datadir)/pixmaps
Gamesdir  = $(datadir)/gnome/apps/Games
typesdir  = $(datadir)/sol-games

SUBDIRS=docs pixmaps

INCLUDES = \
	-DGNOMELOCALEDIR=\""$(datadir)/locale"\" \
	-I$(includedir)				 \
	$(GNOME_INCLUDEDIR)

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
	fortunes.scm	\
	fourteen.scm	\
	freecell.scm	\
	doublets.scm	\
	hopscotch.scm	\
	auld_lang_syne.scm \
	beleaguered_castle.scm \
	camelot.scm	\
	canfield.scm	\
	bristol.scm	\
	clock.scm	\
	eagle_wing.scm	\
	elevator.scm	\
	klondike.scm	\
	pileon.scm	\
	neighbor.scm	\
	odessa.scm	\
	osmosis.scm	\
	plait.scm	\
	royal_east.scm	\
	sol.scm		\
	spider.scm	\
	thieves.scm     \
	thirteen.scm	\
	union_square.scm \
	westhaven.scm	\
	yukon.scm

cardimage_DATA = baize.png splash.png

icon_DATA = gnome-aisleriot.png

EXTRA_DIST = TODO FIXME  AUTHORS $(types_DATA) $(cardimage_DATA) \
	$(icon_DATA) sol.desktop SCHEME_GAMES.h

sysdir = $(datadir)/gnome/apps/Games
sys_DATA = sol.desktop

