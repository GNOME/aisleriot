scoredir=$(localstatedir)/games
cardimagedir = $(datadir)/pixmaps/cards
icondir = $(datadir)/pixmaps
Gamesdir  = $(datadir)/apps/Games
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

sol_LDADD = $(GUILE_LIBS) -lgdk_imlib $(GNOME_LIBDIR) $(GNOMEUI_LIBS) \
	../freecell/gdk-card-image/libgdkcardimage.la \
	$(INTLLIBS) 

types_DATA  = 		\
	fortunes.scm	\
	freecell.scm	\
	doublets.scm	\
	camelot.scm	\
	canfield.scm	\
	clock.scm	\
	eagle_wing.scm	\
	klondike.scm	\
	pileon.scm	\
	odessa.scm	\
	osmosis.scm	\
	sol.scm		\
	spider.scm	\
	yukon.scm

cardimage_DATA = baize.png splash.png

icon_DATA = gnome-aisleriot.png

EXTRA_DIST = TODO FIXME RULES AUTHORS $(types_DATA) $(cardimage_DATA) \
	$(icon_DATA) sol.desktop

sysdir = $(datadir)/apps/Games
sys_DATA = sol.desktop

