scoredir=$(localstatedir)/games
cardimagedir = $(datadir)/pixmaps/cards
Gamesdir  = $(datadir)/apps/Games
typesdir  = $(datadir)/sol-games

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
	sol.h		\
	sol.c		\
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
	freecell.scm	\
	camelot.scm	\
	klondike.scm	\
	odessa.scm	\
	osmosis.scm	\
	sol.scm		\
	spider.scm

cardimage_DATA = Baize.xpm

EXTRA_DIST = TODO FIXME RULES AUTHORS $(types_DATA) $(cardimage_DATA) \
	sol.desktop

sysdir = $(datadir)/apps/Games
sys_DATA = sol.desktop

