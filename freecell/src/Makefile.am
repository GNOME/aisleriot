# process this file with automake to produce Makefile.in
Gamesdir = $(datadir)/apps/Games

INCLUDES = -I$(includedir) \
	-I$(srcdir)/../lib \
	-DLOCALEDIR=\""$(datadir)/locale"\" 

bin_PROGRAMS = freecell
freecell_SOURCES = card.c game.c io-gtk.c freecell.c \
card-image.c card-draw.c \
score.c score-dialog.c option.c option-dialog.c menu.c
noinst_HEADERS = card.h game.h io-gtk.h freecell.h \
card-image.h card-draw.h \
score.h score-dialog.h option.h option-dialog.h menu.h

freecell_LDADD = $(GNOMEUI_LIBS)

EXTRA_DIST = freecell.desktop

