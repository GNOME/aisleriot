# process this file with automake to produce Makefile.in
Gamesdir = $(datadir)/apps/Games

bin_PROGRAMS=freecell
freecell_SOURCES= card.c game.c io-gtk.c freecell.c \
card-image.c card-draw.c \
score.c score-dialog.c option.c option-dialog.c menu.c
noinst_HEADERS= card.h game.h io-gtk.h freecell.h \
card-image.h card-draw.h \
score.h score-dialog.h option.h option-dialog.h menu.h

datadir = $(prefix)/@DATADIRNAME@
localedir = $(datadir)/locale

INCLUDES = -I. -I.. -I../../.. -I../lib -I$(top_srcdir) -I$(top_srcdir)/intl
DEFS = -DLOCALEDIR=\"$(localedir)\" -DPKGDATADIR=\"$(pkgdatadir)\" \
	@DEFS@
CFLAGS = -Wall @CFLAGS@

if IN_GNOME_TREE
  freecell_LDADD = $(X_LIBS) \
	$(srcdir)/../../../libgnomeui/libgnomeui.la \
	$(srcdir)/../../../libgnome/libgnome.la \
	-ldl -lSM -lICE \
	$(GTK_LIBS) $(x_libs) $(INTLLIBS)
else
  freecell_LDADD = $(X_LIBS) $(GNOME_LIBS) \
	$(GTK_LIBS) $(x_libs) $(INTLLIBS)
endif


EXTRA_DIST = freecell.desktop

install-data-local:
	$(mkinstalldirs) $(Gamesdir)
	$(INSTALL_DATA) $(srcdir)/freecell.desktop $(datadir)/apps/Games/"FreeCell.desktop"
