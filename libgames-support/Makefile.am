NULL =

noinst_LTLIBRARIES = libgames-support.la

schemadir   = @GCONF_SCHEMA_FILE_DIR@
schema_in_files = libgnomegames.schemas.in
schema_DATA = $(schema_in_files:.schemas.in=.schemas)
@INTLTOOL_SCHEMAS_RULE@

carddir = $(datadir)/pixmaps/gnome-games-common/cards
card_DATA = bonded.png paris.svg

icondir = $(datadir)/pixmaps/gnome-games-common/icons
icon_DATA = icons/teleport.png icons/rteleport.png

INCLUDES =							\
	-DDATADIR=\""$(datadir)"\"				\
	-DICONDIR=\""$(icondir)"\"				\
	-DSCORESDIR=\""$(scoredir)"\"				\
	-DGCONF_SCHEMA_CONFIG_SOURCE=\""$(GCONF_SCHEMA_CONFIG_SOURCE)"\" \
	$(HOWL_CFLAGS) \
	$(BONJOUR_CFLAGS) \
	$(WARN_CFLAGS)						\
	$(GNOME_GAMES_CFLAGS)

libgames_support_la_LDFLAGS= $(HOWL_LIBS) $(BONJOUR_LIBS)


bin_SCRIPTS = games-server.py

GOB_FILES= \
	games-scores-dialog.gob		\
	$(NULL)

GOB_SOURCE= \
	games-scores-dialog.c		\
	games-scores-dialog.h		\
	games-scores-dialog-private.h	\
	$(NULL)

libgames_support_la_SOURCES =		\
	$(GOB_FILES)			\
	$(GOB_SOURCE)			\
	games-clock.c			\
	games-clock.h			\
	games-gconf.c			\
	games-gconf.h			\
	games-frame.c			\
	games-frame.h			\
	games-files.c			\
	games-files.h			\
	games-gridframe.c		\
	games-gridframe.h		\
	games-card-common.h		\
	games-card-images.c		\
	games-card-images.h		\
	games-card-pixmaps.c		\
	games-card-pixmaps.h		\
	games-card-selector.c		\
	games-card-selector.h		\
	games-controls.c		\
	games-controls.h		\
	games-network.c			\
	games-network.h			\
        games-network-dialog.c          \
        games-network-dialog.h          \
        games-preimage.c                \
        games-preimage.h                \
	games-stock.c			\
	games-stock.h			\
	games-score.h			\
	games-score.c			\
	games-scores.c			\
	games-scores.h			\
	games-scores-backend.c		\
	games-scores-backend.h		\
	$(NULL)

%.c %.h %-private.h: %.gob
	if [ "x$(GOB2)" = "x" ] ; then \
		touch $@ ; \
	else \
		@GOB2@ $< ; \
	fi

install-data-local:
if GCONF_SCHEMAS_INSTALL
        GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $(top_builddir)/libgames-support/$(schema_DATA)
endif

EXTRA_DIST = $(card_DATA) $(icon_DATA) $(bin_SCRIPTS) $(schema_in_files) $(GOB_SOURCE)

DISTCLEANFILES = $(schema_DATA)
