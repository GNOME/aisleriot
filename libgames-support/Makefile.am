NULL =

carddir = $(datadir)/gnome-games-common/cards
card_DATA = \
	gnomangelo_bitmap.svg	\
	$(NULL)

slotdir = $(pkgdatadir)/pixmaps
slot_DATA = slot.svg

EXTRA_DIST = \
	$(schema_in_files)	\
	$(card_DATA)		\
	$(slot_DATA)		\
	games-marshal.list	\
	$(NULL)

noinst_LTLIBRARIES = libgames-support.la

BUILT_SOURCES = \
	games-marshal.c			\
	games-marshal.h			\
	$(NULL)

libgames_support_la_SOURCES =		\
	$(BUILT_SOURCES)		\
	games-card.c			\
	games-card.h			\
	games-card-images.c		\
	games-card-images.h		\
	games-card-theme.c		\
	games-card-theme.h		\
	games-conf.c			\
	games-conf.h			\
	games-files.c			\
	games-files.h			\
	games-help.c			\
	games-help.h			\
	games-pixbuf-utils.c		\
	games-pixbuf-utils.h		\
	games-runtime.c			\
	games-runtime.h			\
	games-sound.c			\
	games-sound.h			\
	games-stock.c			\
	games-stock.h			\
	$(NULL)

if !HAVE_HILDON
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
	games-controls.c		\
	games-controls.h		\
	games-setgid-io.c		\
	games-setgid-io.h		\
	games-scores-backend.c		\
	games-scores-backend.h		\
	$(NULL)
endif

if HAVE_GNOME
if GGZ_CLIENT
libgames_support_la_SOURCES +=		\
	games-dlg-chat.c		\
	games-dlg-chat.h		\
	games-dlg-players.c		\
	games-dlg-players.h		\
	$(NULL)	
endif
endif

if HAVE_CLUTTER
libgames_support_la_SOURCES += \
	games-card-textures-cache.c	\
	games-card-textures-cache.c	\
	games-clutter-embed.c		\
	games-clutter-embed.h		\
	$(NULL)
endif

libgames_support_la_CPPFLAGS = \
	-DPKGDATADIR="\"$(pkgdatadir)\""			\
	-DDATADIR="\"$(datadir)\""				\
	-DCOMMON_DATADIR="\"$(datadir)/gnome-games-common\""	\
	-DSCORESDIR="\"$(scoredir)\""				\
	$(NULL)

libgames_support_la_CFLAGS = 	\
	$(WARN_CFLAGS)		\
	$(GTK_CFLAGS)

libgames_support_la_LIBADD = 	\
	$(GTK_LIBS)

if WITH_SMCLIENT
libgames_support_la_SOURCES += \
	eggdesktopfile.c \
	eggdesktopfile.h \
	eggsmclient.c \
	eggsmclient.h \
	eggsmclient-private.h \
	$(NULL)
if WITH_SMCLIENT_XSMP
libgames_support_la_SOURCES += eggsmclient-xsmp.c
libgames_support_la_CPPFLAGS += -DEGG_SM_CLIENT_BACKEND_XSMP
libgames_support_la_LIBADD += -lSM
endif
if WITH_SMCLIENT_WIN32
libgames_support_la_SOURCES += eggsmclient-win32.c
endif
if WITH_SMCLIENT_QUARTZ
libgames_support_la_SOURCES += eggsmclient-osx.c
endif
endif

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

if HAVE_CLUTTER
libgames_support_la_CFLAGS += $(CLUTTER_CFLAGS)
libgames_support_la_LIBADD += $(CLUTTER_LIBS)
endif

if HAVE_GSTREAMER
libgames_support_la_CFLAGS += $(GSTREAMER_CFLAGS)
libgames_support_la_LIBADD += $(GSTREAMER_LIBS)
endif

if HAVE_SDL_MIXER
libgames_support_la_CFLAGS += $(SDL_MIXER_CFLAGS)
libgames_support_la_LIBADD += $(SDL_MIXER_LIBS)
endif

# Auxiliary programme to prerender card images

if HAVE_RSVG

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

endif # HAVE_RSVG

install-data-local:
if GCONF_SCHEMAS_INSTALL
	if test -z "$(DESTDIR)" ; then \
		for p in $^ ; do \
			GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $$p 2>&1 > /dev/null; \
		done \
	fi
endif

stamp_files = \
	stamp-games-marshal.c \
	stamp-games-marshal.h \
	$(NULL)

CLEANFILES = \
	$(BUILT_SOURCES) \
	$(stamp_files) \
	$(NULL)

DISTCLEANFILES = $(schema_DATA)

games-marshal.c: stamp-games-marshal.c
	@true
stamp-games-marshal.c: games-marshal.list Makefile
	$(GLIB_GENMARSHAL) --prefix=games_marshal $< --header --body $(GLIB_GENMARSHAL_INTERNAL) > games-marshal.c \
	&& echo timestamp > $(@F)

games-marshal.h: stamp-games-marshal.h
	@true
stamp-games-marshal.h: games-marshal.list Makefile
	$(GLIB_GENMARSHAL) --prefix=games_marshal $< --header $(GLIB_GENMARSHAL_INTERNAL) > games-marshal.h \
	&& echo timestamp > $(@F)
