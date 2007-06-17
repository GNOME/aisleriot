NULL =

carddir = $(datadir)/gnome-games-common/cards
card_DATA = \
	bonded.svg	\
	$(NULL)

slotdir = $(pkgdatadir)/pixmaps
slot_DATA = slot.svg

EXTRA_DIST = \
	$(schema_in_files)	\
	$(card_DATA)		\
	$(slot_DATA)		\
	$(NULL)

if !HAVE_MAEMO
icondir = $(datadir)/gnome-games-common/icons
icon_DATA = \
	icons/teleport.png	\
	icons/rteleport.png	\
	$(NULL)

EXTRA_DIST += $(icon_DATA)
endif

noinst_LTLIBRARIES = libgames-support.la

libgames_support_la_SOURCES =		\
	games-card-common.h		\
	games-card-images.c		\
	games-card-images.h		\
	games-card-theme.c		\
	games-card-theme.h		\
	games-files.c			\
	games-files.h			\
	games-pixbuf-utils.c		\
	games-pixbuf-utils.h		\
	games-stock.c			\
	games-stock.h			\
	$(NULL)

if !HAVE_MAEMO
libgames_support_la_SOURCES +=		\
	games-clock.c			\
	games-clock.h			\
	games-frame.c			\
	games-frame.h			\
	games-find-file.c		\
	games-find-file.h		\
	games-gridframe.c		\
	games-gridframe.h		\
	games-card-selector.c		\
	games-card-selector.h		\
	games-preimage.c                \
	games-preimage.h                \
	games-score.h			\
	games-score.c			\
	games-scores.c			\
	games-scores.h			\
	games-scores-dialog.c		\
	games-scores-dialog.h		\
	games-scores-dialog-private.h	\
	$(NULL)
endif

if HAVE_GNOME
libgames_support_la_SOURCES +=		\
	games-controls.c		\
	games-controls.h		\
	games-gconf.c			\
	games-gconf.h			\
	games-setgid-io.c		\
	games-setgid-io.h		\
	games-scores-backend.c		\
	games-scores-backend.h		\
	$(NULL)

if GGZ_CLIENT
libgames_support_la_SOURCES +=		\
	games-dlg-chat.c		\
	games-dlg-chat.h		\
	games-dlg-players.c		\
	games-dlg-players.h		\
	$(NULL)	
endif
endif

if HAVE_GSTREAMER
libgames_support_la_SOURCES +=		\
	games-sound.c			\
	games-sound.h			\
	$(NULL)
endif

libgames_support_la_CPPFLAGS = \
	-DPKGDATADIR="\"$(pkgdatadir)\""			\
	-DDATADIR="\"$(datadir)\""				\
	-DCOMMON_DATADIR="\"$(datadir)/gnome-games-common\""	\
	-DCARDDIR="\"$(carddir)\""				\
	-DICONDIR="\"$(icondir)\""				\
	-DSCORESDIR="\"$(scoredir)\""				\
	-DSOUNDDIR="\"$(pkgdatadir)/sounds\""			\
	$(NULL)

libgames_support_la_CFLAGS = 	\
	$(WARN_CFLAGS)		\
	$(GTK_CFLAGS)

libgames_support_la_LIBADD = 	\
	$(GTK_LIBS)

if HAVE_GNOME
libgames_support_la_CFLAGS += $(GNOME_CFLAGS)
libgames_support_la_LIBADD += $(GNOME_LIBS)

if GGZ_CLIENT
libgames_support_la_CFLAGS += 	\
	$(GGZMOD_INCLUDES) 	\
	$(GGZ_GTK_INCLUDES) 	\
	$(GGZCORE_INCLUDES)	\
	$(LIBGGZ_INCLUDES)
endif
endif

if HAVE_RSVG
libgames_support_la_CFLAGS += $(RSVG_CFLAGS)
libgames_support_la_LIBADD += $(RSVG_LIBS)
endif

if HAVE_GSTREAMER
libgames_support_la_CFLAGS += $(GSTREAMER_CFLAGS)
libgames_support_la_LIBADD += $(GSTREAMER_LIBS)
endif

# Auxiliary programme to prerender card images

if HAVE_RSVG
if HAVE_CAIRO

utildir = $(pkglibdir)
util_PROGRAMS = gnome-games-render-cards

gnome_games_render_cards_SOURCES = \
	render-cards.c

gnome_games_render_cards_CPPFLAGS = \
	$(AM_CPPFLAGS)

gnome_games_render_cards_CFLAGS = \
	$(GTK_CFLAGS)	\
	$(RSVG_CFLAGS)	\
	$(AM_CFLAGS)

gnome_games_render_cards_LDFLAGS = \
	$(AM_LDFLAGS)

gnome_games_render_cards_LDADD = \
	libgames-support.la	\
	$(GTK_LIBS)		\
	$(RSVG_LIBS)

if WITH_GTHREAD
gnome_games_render_cards_CFLAGS += $(GHTREAD_CFLAGS)
gnome_games_render_cards_LDADD += $(GTHREAD_LIBS)
endif

endif
endif

install-data-local:
if GCONF_SCHEMAS_INSTALL
	if test -z "$(DESTDIR)" ; then \
		for p in $^ ; do \
			GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $$p 2>&1 > /dev/null; \
		done \
	fi
endif

DISTCLEANFILES = $(schema_DATA)
