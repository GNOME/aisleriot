NULL =

noinst_LTLIBRARIES = libgames-support.la

INCLUDES =							\
	-DDATADIR=\""$(datadir)"\"				\
	-DGCONF_SCHEMA_CONFIG_SOURCE=\""$(GCONF_SCHEMA_CONFIG_SOURCE)"\" \
	$(WARN_CFLAGS)						\
	$(GNOME_GAMES_CFLAGS)

carddir = $(datadir)/pixmaps/gnome-games-common/cards
card_DATA = bonded.png dondorf.svg bellot.svg

libgames_support_la_SOURCES =		\
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
	$(NULL)

EXTRA_DIST = $(card_DATA)
