# process this file with automake to produce Makefile.in
Gamesdir = $(datadir)/apps/Games

INCLUDES = -I$(includedir) \
	-DLOCALEDIR=\""$(datadir)/locale"\" \
	-I$(srcdir)/../gdk-card-image \
	$(GNOME_INCLUDEDIR)

bin_PROGRAMS = freecell
freecell_SOURCES = card.c game.c io-gtk.c freecell.c \
card-draw.c \
score.c score-dialog.c option.c option-dialog.c menu.c
noinst_HEADERS = card.h game.h io-gtk.h freecell.h \
card-draw.h \
score.h score-dialog.h option.h option-dialog.h menu.h

freecell_LDADD = \
	../gdk-card-image/libgdkcardimage.la \
	$(GNOME_LIBDIR)	$(GNOMEUI_LIBS) $(INTLLIBS)

EXTRA_DIST = freecell.desktop

Games_DATA = freecell.desktop
