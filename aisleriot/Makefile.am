SUBDIRS = rules data lib

if BUILD_HELP
SUBDIRS += help
endif

NULL =

# Same as in help/Makefile.am
DOC_MODULE = aisleriot

cardimagedir = $(pkgdatadir)/pixmaps
cardimage_DATA = \
	baize.png

scmdir  = $(pkgdatadir)/aisleriot/games
scm_DATA = \
	sol.scm

bin_PROGRAMS = sol

sol_SOURCES = \
	ar-cursor.c	\
	ar-cursor.h	\
	ar-game-chooser.c \
	ar-game-chooser.h \
	ar-style.c	\
	ar-style.h	\
	ar-style-private.h \
	ar-style-gtk.c	\
	ar-style-gtk.h	\
	board-noclutter.c \
	board-noclutter.h \
	conf.c		\
	conf.h		\
	game.c		\
	game.h		\
	sol.c		\
	stats-dialog.c	\
	stats-dialog.h	\
	util.c		\
	util.h		\
	window.c	\
	window.h	\
	$(NULL)

if HAVE_MAEMO_5
sol_SOURCES += \
	ar-fullscreen-button.c \
	ar-fullscreen-button.h \
	$(NULL)
endif

if !HAVE_GUILE_1_8
sol_SOURCES += guile16-compat.h
endif

if ENABLE_DEBUG_UI
sol_SOURCES += \
	prop-editor.c	\
	prop-editor.h	\
	$(NULL)
endif

# FIXMEchpe: fix localedir for maemo? $PKG_CONFIG osso-af-settings --variable=localedir

sol_CPPFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/aisleriot/lib \
	-Ilib \
	-DDOC_MODULE=\"$(DOC_MODULE)\" \
	$(AM_CPPFLAGS)

sol_CFLAGS = \
	$(GTK_CFLAGS)	      \
	$(GUILE_CFLAGS)	      \
	$(AM_CFLAGS)

sol_LDFLAGS = \
	$(AM_LDFLAGS)

sol_LDADD = \
	lib/libaisleriot.la \
	$(top_builddir)/libgames-support/libgames-support.la \
	$(GTK_LIBS) \
	$(GUILE_LIBS) \
	-lm

if HAVE_HILDON
sol_CFLAGS += $(HILDON_CFLAGS)
sol_LDADD += $(HILDON_LIBS)
endif

if HAVE_GNOME
sol_CFLAGS += $(GNOME_CFLAGS)
sol_LDADD += $(GNOME_LIBS)
endif

if HAVE_RSVG
sol_CFLAGS += $(RSVG_CFLAGS)
sol_LDADD += $(RSVG_LIBS)
endif

if WITH_GTHREAD
sol_CFLAGS += $(GHTREAD_CFLAGS)
sol_LDADD += $(GTHREAD_LIBS)
endif

if PLATFORM_WIN32_NATIVE
sol_LDFLAGS += -mwindows
endif

if HAVE_CLUTTER
if ENABLE_AISLERIOT_CLUTTER

noinst_PROGRAMS = sol-clutter

sol_clutter_SOURCES = \
	ar-clutter-embed.c \
	ar-clutter-embed.h \
	ar-cursor.c	\
	ar-cursor.h	\
	ar-game-chooser.c \
	ar-game-chooser.h \
	ar-style.c	\
	ar-style.h	\
	ar-style-private.h \
	ar-style-gtk.c	\
	ar-style-gtk.h	\
	baize.c		\
	baize.h		\
	board.c		\
	board.h		\
	card.c          \
	card.h          \
	conf.c		\
	conf.h		\
	game.c		\
	game.h		\
	sol.c		\
	slot-renderer.c	\
	slot-renderer.h \
	stats-dialog.c	\
	stats-dialog.h	\
	util.c		\
	util.h		\
	window.c	\
	window.h	\
	$(NULL)

if HAVE_MAEMO_5
sol_clutter_SOURCES += \
	ar-fullscreen-button.c \
	ar-fullscreen-button.h \
	$(NULL)
endif

if !HAVE_GUILE_1_8
sol_clutter_SOURCES += guile16-compat.h
endif
		
if ENABLE_DEBUG_UI
sol_clutter_SOURCES += \
	prop-editor.c	\
	prop-editor.h	\
	$(NULL)
endif

sol_clutter_CPPFLAGS = $(sol_CPPFLAGS) -DHAVE_CLUTTER
sol_clutter_CFLAGS = $(sol_CFLAGS) $(CLUTTER_CFLAGS) $(CLUTTER_GTK_CFLAGS)
sol_clutter_LDFLAGS = $(sol_LDFLAGS)
sol_clutter_LDADD = $(sol_LDADD) $(CLUTTER_LIBS) $(CLUTTER_GTK_LIBS)


endif # ENABLE_AISLERIOT_CLUTTER
endif # HAVE_CLUTTER

desktop_in_files = \
	sol.desktop.in.in

if !HAVE_HILDON
desktop_in_files += \
	freecell.desktop.in.in
endif

if HAVE_MAEMO
desktopdir = $(shell $(PKG_CONFIG) osso-af-settings --variable=desktopentrydir)
else
desktopdir = $(datadir)/applications
endif

desktop_DATA = $(desktop_in_files:.desktop.in.in=.desktop)

schema_in_files = aisleriot.schemas.in
if HAVE_GNOME
schemadir = $(GCONF_SCHEMA_FILE_DIR)
schema_DATA = $(schema_in_files:.schemas.in=.schemas)
endif

man_MANS = sol.6

ossobackup_in_files = aisleriot-backup.conf
ossoscript_in_files = aisleriot-clean.sh
ossoservice_in_files = org.gnome.Games.AisleRiot.service.in

if HAVE_MAEMO

ossobackupdir = $(sysconfdir)/osso-backup/applications
ossobackup_DATA = $(ossobackup_in_files)

ossocuddir = $(sysconfdir)/osso-cud-scripts
ossocud_SCRIPTS = $(ossoscript_in_files)

servicedir = $(shell $(PKG_CONFIG) osso-af-settings --variable=dbusservicedir)
service_DATA = $(ossoservice_in_files:.service.in=.service)

if HAVE_MAEMO_5
rcdir = $(pkgdatadir)/aisleriot
dist_rc_DATA = gtkrc-maemo
endif # HAVE_MAEMO_5

endif # HAVE_MAEMO

%.service: %.service.in Makefile
	$(AM_V_GEN) $(SED) -e "s|%bindir%|$(bindir)|" $< > $@

install-schemas-local: $(schema_DATA)
if GCONF_SCHEMAS_INSTALL
	if test -z "$(DESTDIR)" ; then \
		for p in $^ ; do \
			GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $$p 2>&1 > /dev/null; \
		done \
	fi
endif

install-data-local: install-schemas-local

EXTRA_DIST = \
	AUTHORS			\
	README			\
	Rules.HOWTO		\
	TODO			\
	$(scm_DATA)		\
	$(cardimage_DATA)	\
	$(icon_DATA)		\
	$(allicons)		\
	$(ossobackup_in_files)	\
	$(ossoscript_in_files)	\
	$(ossoservice_in_files)	\
	$(schema_in_files)	\
	$(man_MANS)		\
	aisleriot.supp		\
	game-names.h

CLEANFILES = \
	$(desktop_DATA)	\
	$(schema_DATA)	\
	$(service_DATA)

DISTCLEANFILES = \
	$(desktop_DATA)	\
	$(schema_DATA)	\
	$(service_DATA)

@INTLTOOL_DESKTOP_RULE@
@INTLTOOL_SCHEMAS_RULE@

-include $(top_srcdir)/git.mk
