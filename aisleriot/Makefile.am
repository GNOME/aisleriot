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
	$(INTLLIBS)

types_DATA  = 		\
	freecell.scm	\
	camelot.scm	\
	klondike.scm	\
	odessa.scm	\
	osmosis.scm	\
	sol.scm		\
	spider.scm

EXTRA_DIST = TODO FIXME RULES AUTHORS $(types_DATA) sol.desktop

sysdir = $(datadir)/apps/Games
sys_DATA = sol.desktop

dist-hook:
	mkdir $(distdir)/pixmaps
	cp $(srcdir)/pixmaps/* $(distdir)/pixmaps
