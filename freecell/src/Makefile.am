Gamesdir = $(datadir)/applications

INCLUDES = \
	$(GNOME_GAMES_CFLAGS)				\
	-DDATADIR=\""$(datadir)"\"		   \
	-DGNOMELOCALEDIR=\""$(datadir)/locale"\"	\
	-I$(srcdir)/../gdk-card-image			\
	-DG_DISABLE_DEPRECATED -DGDK_DISABLE_DEPRECATED \
	-DGTK_DISABLE_DEPRECATED -DGNOME_DISABLE_DEPRECATED

bin_PROGRAMS = freecell

freecell_SOURCES = card.c game.c io-gtk.c freecell.c card-draw.c	\
score.c score-dialog.c option.c option-dialog.c menu.c

noinst_HEADERS = card.h game.h io-gtk.h card-draw.h score.h	\
score-dialog.h option.h option-dialog.h menu.h tb-xpms.h

freecell_LDADD = \
	../gdk-card-image/libgdkcardimage.la	\
	$(GNOME_GAMES_LIBS) \
	$(INTLLIBS)

Games_in_files = freecell.desktop.in
Games_DATA = $(Games_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

schemadir = $(sysconfdir)/gconf/schemas
schema_in_files = freecell.schemas.in
schema_DATA = $(schema_in_files:.schemas.in=.schemas)
@INTLTOOL_SCHEMAS_RULE@

EXTRA_DIST = $(Games_in_files) $(schema_DATA)

install-data-local:
	GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $(srcdir)/$(schema_DATA)

