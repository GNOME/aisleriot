## process this file with automake to produce Makefile.in
Gamesdir = $(datadir)/gnome/apps/Games

INCLUDES = \
	-DGNOMELOCALEDIR=\""$(datadir)/locale"\"	\
	-I$(includedir)					\
	$(GNOME_INCLUDEDIR)				\
	-I$(srcdir)/../gdk-card-image

bin_PROGRAMS = freecell

freecell_SOURCES = card.c game.c io-gtk.c freecell.c card-draw.c	\
score.c score-dialog.c option.c option-dialog.c menu.c

noinst_HEADERS = card.h game.h io-gtk.h card-draw.h score.h	\
score-dialog.h option.h option-dialog.h menu.h tb-xpms.h

freecell_LDADD = \
	../gdk-card-image/libgdkcardimage.la	\
	$(GNOME_LIBDIR)				\
	$(GNOMEUI_LIBS)				\
	$(INTLLIBS)

Games_DATA = freecell.desktop

EXTRA_DIST = $(Games_DATA)

