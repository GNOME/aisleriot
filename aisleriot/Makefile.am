Gamesdir  = $(datadir)/apps/Games
typesdir  = $(datadir)/sol-games

INCLUDES = -I$(includedir)

SUBDIRS = pixmaps

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
	cscmi.c		\
	events.c	\
	press_data.c	\
	draw.c		\
	menu.c		\
	card.c

sol_LDADD = $(GUILE_LIBS) -lgdk_imlib $(GNOMEUI_LIBS) \
	$(INTLLIBS)

types_DATA  = 		\
	freecell.scm	\
	helper.scm	\
	klondike.scm	\
	odessa.scm	\
	osmosis.scm	\
	sol.scm		\
	spider.scm

EXTRA_DIST = TODO FIXME RULES AUTHORS $(types_DATA) sol.desktop

