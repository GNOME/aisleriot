# process this file with automake to produce Makefile.in
bin_PROGRAMS=freecell
freecell_SOURCES= card.c game.c io-gtk.c freecell.c \
card-image.c card-draw.c \
score.c score-dialog.c option.c option-dialog.c
noinst_HEADERS= card.h game.h io-gtk.h freecell.h \
card-image.h card-draw.h \
score.h score-dialog.h option.h option-dialog.h

datadir = $(prefix)/@DATADIRNAME@
localedir = $(datadir)/locale

INCLUDES = -I. -I.. -I../lib -I$(top_srcdir) -I$(top_srcdir)/intl 
DEFS = -DLOCALEDIR=\"$(localedir)\" -DPKGDATADIR=\"$(pkgdatadir)\" \
	@DEFS@
CFLAGS = -Wall @CFLAGS@

freecell_LDADD = @INTLLIBS@ \
	        $(X_LIBS) $(GNOME_LIBS) $(GTK_LIBS) $(x_libs)


