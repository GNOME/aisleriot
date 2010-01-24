NULL =

commondatadir = $(datadir)/gnome-games-common

EXTRA_DIST = \
	$(schema_in_files)	\
	games-marshal.list	\
	$(NULL)

noinst_LTLIBRARIES = libgames-support.la

if BUILD_LIBGAMESCARDS
noinst_LTLIBRARIES += libgamescards.la
endif

pkglib_LTLIBRARIES =

# libgames-support.la

BUILT_SOURCES = \
	games-marshal.c			\
	games-marshal.h			\
	$(NULL)

libgames_support_la_SOURCES =		\
	games-conf.c			\
	games-conf.h			\
	games-debug.c			\
	games-debug.h			\
	games-glib-compat.h		\
	games-gtk-compat.h		\
	games-help.c			\
	games-help.h			\
	games-profile.c			\
	games-profile.h			\
	games-runtime.c			\
	games-runtime.h			\
	games-show.c			\
	games-show.h			\
	games-sound.c			\
	games-sound.h			\
	games-stock.c			\
	games-stock.h			\
	games-string-utils.c		\
	games-string-utils.h		\
	$(NULL)

nodist_libgames_support_la_SOURCES = $(BUILT_SOURCES)

if !HAVE_HILDON
libgames_support_la_SOURCES +=		\
	games-atk-utils.c		\
	games-atk-utils.h		\
	games-clock.c			\
	games-clock.h			\
	games-preimage.c                \
	games-preimage.h                \
	games-preimage-private.h	\
	$(NULL)
endif # !HAVE_HILDON

if LIBGAMES_SUPPORT_FULL
libgames_support_la_SOURCES +=		\
	games-files.c			\
	games-files.h			\
	games-frame.c			\
	games-frame.h			\
	games-gridframe.c		\
	games-gridframe.h		\
	games-score.h			\
	games-score.c			\
	games-scores.c			\
	games-scores.h			\
	games-scores-dialog.c		\
	games-scores-dialog.h		\
	games-scores-dialog-private.h	\
	games-controls.c		\
	games-controls.h		\
	games-scores-backend.c		\
	games-scores-backend.h		\
	$(NULL)

if ENABLE_SETGID
libgames_support_la_SOURCES +=		\
	games-setgid-io.c		\
	games-setgid-io.h		\
	$(NULL)
endif # ENABLE_SETGID

endif # LIBGAMES_SUPPORT_FULL

libgames_support_la_CPPFLAGS = \
	-DPKGDATADIR="\"$(pkgdatadir)\""			\
	-DDATADIR="\"$(datadir)\""				\
	-DCOMMON_DATADIR="\"$(datadir)/gnome-games-common\""	\
	-DSCORESDIR="\"$(scoredir)\""				\
	$(AM_CPPFLAGS)

libgames_support_la_CFLAGS = 	\
	$(GTK_CFLAGS)		\
	$(AM_CFLAGS)

libgames_support_la_LIBADD = 	\
	$(GTK_LIBS)

if WITH_SMCLIENT
libgames_support_la_SOURCES += \
	eggsmclient.c \
	eggsmclient.h \
	eggsmclient-private.h \
	$(NULL)
libgames_support_la_CFLAGS += $(SMCLIENT_CFLAGS)
libgames_support_la_LIBADD += $(SMCLIENT_LIBS)

if WITH_SMCLIENT_XSMP
libgames_support_la_SOURCES += \
	eggdesktopfile.c \
	eggdesktopfile.h \
	eggsmclient-xsmp.c \
	$(NULL)
libgames_support_la_CPPFLAGS += -DEGG_SM_CLIENT_BACKEND_XSMP
endif
if WITH_SMCLIENT_WIN32
libgames_support_la_SOURCES += eggsmclient-win32.c
endif
if WITH_SMCLIENT_QUARTZ
libgames_support_la_SOURCES += eggsmclient-osx.c
endif
endif # WITH_SMCLIENT

if HAVE_GNOME
libgames_support_la_CFLAGS += $(GNOME_CFLAGS)
libgames_support_la_LIBADD += $(GNOME_LIBS)
endif

if HAVE_RSVG
libgames_support_la_CFLAGS += $(RSVG_CFLAGS)
libgames_support_la_LIBADD += $(RSVG_LIBS)
endif

if ENABLE_SOUND
libgames_support_la_CFLAGS += $(CANBERRA_GTK_CFLAGS)
libgames_support_la_LIBADD += $(CANBERRA_GTK_LIBS)
endif

if HAVE_HILDON
libgames_support_la_CFLAGS += $(HILDON_CFLAGS)
libgames_support_la_LIBADD += $(HILDON_LIBS)
endif

# libgamescards.la

if BUILD_LIBGAMESCARDS

libgamescards_la_SOURCES = \
	games-card.c \
	games-card.h \
	games-card-images.c \
	games-card-images.h \
	games-card-private.h \
	games-card-theme.c \
	games-card-theme.h \
	games-card-theme-private.h \
	games-card-themes.c \
	games-card-themes.h \
	games-pixbuf-utils.c \
	games-pixbuf-utils.h \
	$(NULL)

if HAVE_CLUTTER
libgamescards_la_SOURCES += \
	games-card-textures-cache.c \
	games-card-textures-cache.h \
	$(NULL)
endif # HAVE_CLUTTER

if HAVE_RSVG
libgamescards_la_SOURCES += games-card-theme-preimage.c

if ENABLE_CARD_THEME_FORMAT_SVG
libgamescards_la_SOURCES += games-card-theme-svg.c
endif

if ENABLE_CARD_THEME_FORMAT_KDE
libgamescards_la_SOURCES += games-card-theme-kde.c
endif

endif # HAVE_RSVG

if ENABLE_CARD_THEME_FORMAT_SLICED
if !HAVE_RSVG
libgamescards_la_SOURCES += games-card-theme-preimage.c
endif
libgamescards_la_SOURCES += games-card-theme-sliced.c
endif # ENABLE_CARD_THEME_FORMAT_SLICED

if ENABLE_CARD_THEME_FORMAT_PYSOL
libgamescards_la_SOURCES += games-card-theme-pysol.c
endif

if ENABLE_CARD_THEME_FORMAT_FIXED
libgamescards_la_SOURCES += games-card-theme-fixed.c
endif

libgamescards_la_CPPFLAGS = \
	$(AM_CPPFLAGS)

libgamescards_la_CFLAGS = \
	$(GTK_CFLAGS) \
	$(AM_CFLAGS)

libgamescards_la_LIBADD = \
	libgames-support.la \
	$(GTK_LIBS)

if HAVE_RSVG
libgamescards_la_CFLAGS += $(RSVG_CFLAGS)
libgamescards_la_LIBADD += $(RSVG_LIBS)
endif # HAVE_RSVG

if HAVE_CLUTTER
libgamescards_la_CFLAGS += $(CLUTTER_CFLAGS)
libgamescards_la_LIBADD += $(CLUTTER_LIBS)
endif # HAVE_CLUTTER

if ENABLE_CARD_THEMES_INSTALLER
libgamescards_la_CFLAGS += $(DBUS_GLIB_CFLAGS)
libgamescards_la_LIBADD += $(DBUS_GLIB_LIBS)
endif # ENABLE_CARD_THEMES_INSTALLER

if HAVE_HILDON
libgamescards_la_CFLAGS += $(HILDON_CFLAGS)
libgamescards_la_LIBADD += $(HILDON_LIBS)
endif

# Data used in libgamescards

if ENABLE_CARD_THEME_FORMAT_SVG
carddir = $(commondatadir)/cards
dist_card_DATA = \
	gnomangelo_bitmap.svg	\
	$(NULL)
endif # ENABLE_CARD_THEME_FORMAT_SVG

if HAVE_RSVG
slotdir = $(pkgdatadir)/pixmaps
dist_slot_DATA = slot.svg
endif # HAVE_RSVG

if ENABLE_CARD_THEMES_INSTALLER
themeinstalldir = $(commondatadir)
dist_themeinstall_DATA = theme-install.ini
endif # ENABLE_CARD_THEMES_INSTALLER

endif # BUILD_LIBGAMESCARDS

# Auxiliary programme to prerender card images

if BUILD_LIBGAMESCARDS
if HAVE_RSVG

pkglib_PROGRAMS = gnome-games-render-cards

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
	libgamescards.la	\
	$(GTK_LIBS)		\
	$(RSVG_LIBS)

if WITH_GTHREAD
gnome_games_render_cards_CFLAGS += $(GHTREAD_CFLAGS)
gnome_games_render_cards_LDADD += $(GTHREAD_LIBS)
endif # WITH_GTHREAD

endif # HAVE_RSVG
endif # BUILD_LIBGAMESCARDS

install-data-local:
if GCONF_SCHEMAS_INSTALL
	if test -z "$(DESTDIR)" ; then \
		for p in $^ ; do \
			GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $$p 2>&1 > /dev/null; \
		done \
	fi
endif # GCONF_SCHEMAS_INSTALL

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
	$(AM_V_GEN) $(GLIB_GENMARSHAL) --prefix=games_marshal $< --header --body $(GLIB_GENMARSHAL_INTERNAL) > games-marshal.c \
	&& echo timestamp > $(@F)

games-marshal.h: stamp-games-marshal.h
	@true
stamp-games-marshal.h: games-marshal.list Makefile
	$(AM_V_GEN) $(GLIB_GENMARSHAL) --prefix=games_marshal $< --header $(GLIB_GENMARSHAL_INTERNAL) > games-marshal.h \
	&& echo timestamp > $(@F)

if HAVE_INTROSPECTION

pkglib_LTLIBRARIES += libgames-support-gi.la

libgames_support_gi_la_SOURCES = $(libgames_support_la_SOURCES) $(nodist_libgames_support_la_SOURCES)
libgames_support_gi_la_CPPFLAGS = $(libgames_support_la_CPPFLAGS) -DLIBGAMES_SUPPORT_GI
libgames_support_gi_la_CFLAGS = $(libgames_support_la_CFLAGS)
libgames_support_gi_la_LIBADD = $(libgames_support_la_LIBADD) -lm

BUILT_GIRSOURCES = GnomeGamesSupport-1.0.gir

GnomeGamesSupport-1.0.gir: $(INTROSPECTION_SCANNER) libgames-support-gi.la $(libgames_support_gi_la_SOURCES)
	$(AM_V_GEN) $(INTROSPECTION_SCANNER) -v \
	--namespace GnomeGamesSupport --nsversion=1.0 \
	--strip-prefix=games \
	--add-include-path=$(srcdir) --add-include=path=. \
	--include=Clutter-1.0 \
	--include=Cogl-1.0 \
	--include=Gtk-2.0 \
	--library=games-support-gi \
	--libtool="$(LIBTOOL)" \
	--output $@ \
	--pkg gobject-2.0 \
	--pkg gtk+-2.0 \
	--pkg clutter-1.0 \
	--pkg cogl-1.0 \
	-I$(top_srcdir) \
	-I$(top_builddir) \
	$(filter %.h,$^)

# FIXMEchpe: this looks bogus. If the introspection.m4 macro gets it wrong, fix it!
# INTROSPECTION_GIRDIR/INTROSPECTION_TYPELIBDIR aren't the right place to
# install anything - we need to install inside our prefix.
girdir = $(datadir)/gir-1.0
gir_DATA = $(BUILT_GIRSOURCES)

typelibsdir = $(libdir)/girepository-1.0
typelibs_DATA = $(BUILT_GIRSOURCES:.gir=.typelib)

%.typelib: %.gir $(INTROSPECTION_COMPILER)
	$(AM_V_GEN) $(DEBUG) $(INTROSPECTION_COMPILER) --includedir=$(srcdir) --includedir=. $(INTROSPECTION_COMPILER_OPTS) $< -o $@

CLEANFILES += GnomeGamesSupport-1.0.gir $(typelibs_DATA)

endif # HAVE_INTROSPECTION


-include $(top_srcdir)/git.mk
