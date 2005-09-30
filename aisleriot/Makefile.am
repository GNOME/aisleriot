cardimagedir = $(datadir)/pixmaps/cards
icondir = $(datadir)/pixmaps
Gamesdir  = $(datadir)/applications
scmdir  = $(datadir)/sol-games
gladedir = $(datadir)/sol-games/glade

schemadir   = @GCONF_SCHEMA_FILE_DIR@
schema_in_files = aisleriot.schemas.in
schema_DATA = $(schema_in_files:.schemas.in=.schemas)
@INTLTOOL_SCHEMAS_RULE@

SUBDIRS=help rules

INCLUDES =  \
	-I$(top_srcdir)/libgames-support           \
	-DDATADIR=\""$(datadir)"\"		   \
	$(GNOME_GAMES_CFLAGS)			   \
	$(GLADE_CFLAGS)				   \
	-DGLADEDIR=\""$(gladedir)"\" 		   \
	-DGNOMELOCALEDIR=\""$(datadir)/locale"\"   \
	-DVERSION=\"$(VERSION)\"		   \
	$(GUILE_CFLAGS)

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
	card.c		\
	statistics.c	\
	statistics.h

sol_LDADD = \
	../libgames-support/libgames-support.la \
	$(GNOME_GAMES_LIBS) \
	$(GLADE_LIBS) \
	$(GUILE_LDFLAGS) 

glade_DATA = \
	statistics.glade

scm_DATA  = 		\
	sol.scm

cardimage_DATA = baize.png slot.svg

icon_DATA = gnome-aisleriot.png gnome-cardgame.png

EXTRA_DIST = AUTHORS $(scm_DATA) $(cardimage_DATA) \
	$(icon_DATA) sol.desktop.in freecell.desktop.in \
	translatable_game_names.h \
	$(schema_in_files) $(glade_DATA)


sysdir = $(datadir)/applications/
sys_in_files = sol.desktop.in freecell.desktop.in
sys_DATA = $(sys_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

DISTCLEANFILES = $(sys_DATA) $(schema_DATA)

if GCONF_SCHEMAS_INSTALL
install-data-local:
	GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $(top_builddir)/aisleriot/$(schema_DATA)
endif
