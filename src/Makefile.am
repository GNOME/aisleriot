SUBDIRS = lib

NULL =

# Same as in help/Makefile.am
DOC_MODULE = aisleriot

cardimagedir = $(pkgdatadir)/pixmaps
cardimage_DATA = \
	baize.png

bin_PROGRAMS = sol

sol_SOURCES = \
	ar-clock.c \
	ar-clock.h \
	ar-cursor.c	\
	ar-cursor.h	\
	ar-game-chooser.c \
	ar-game-chooser.h \
	ar-resources.c	\
	ar-resources.h	\
	ar-stock.c	\
	ar-stock.h	\
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

if ENABLE_DEBUG_UI
sol_SOURCES += \
	prop-editor.c	\
	prop-editor.h	\
	$(NULL)
endif

sol_CPPFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/src/lib \
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
	$(GTK_LIBS) \
	$(GUILE_LIBS) \
	-lm

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

noinst_PROGRAMS = sol-clutter

sol_clutter_SOURCES = \
	ar-clock.c	\
	ar-clock.h	\
	ar-clutter-embed.c \
	ar-clutter-embed.h \
	ar-cursor.c	\
	ar-cursor.h	\
	ar-game-chooser.c \
	ar-game-chooser.h \
	ar-resources.c	\
	ar-resources.h	\
	ar-stock.c	\
	ar-stock.h	\
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

if ENABLE_DEBUG_UI
sol_clutter_SOURCES += \
	prop-editor.c	\
	prop-editor.h	\
	$(NULL)
endif

sol_clutter_CPPFLAGS = $(sol_CPPFLAGS) -DHAVE_CLUTTER
sol_clutter_CFLAGS = $(sol_CFLAGS) $(CLUTTER_GTK_CFLAGS) $(CLUTTER_CFLAGS)
sol_clutter_LDFLAGS = $(sol_LDFLAGS)
sol_clutter_LDADD = $(sol_LDADD) $(CLUTTER_GTK_LIBS) $(CLUTTER_LIBS)

endif # HAVE_CLUTTER

# FIXME autogen the dependencies from the gresource file too?
ar-resources.c: aisleriot.gresource.xml Makefile
	$(AM_V_GEN) XMLLINT=$(XMLLINT) $(GLIB_COMPILE_RESOURCES) --target $@ --sourcedir . --generate-source --c-name aisleriot $<

ar-resources.h: aisleriot.gresource.xml Makefile
	$(AM_V_GEN) XMLLINT=$(XMLLINT) $(GLIB_COMPILE_RESOURCES) --target $@ --sourcedir . --generate-header --c-name aisleriot $<

schema_in_files = aisleriot.schemas.in
if HAVE_GNOME
schemadir = $(GCONF_SCHEMA_FILE_DIR)
schema_DATA = $(schema_in_files:.schemas.in=.schemas)
endif

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
	TODO			\
	$(cardimage_DATA)	\
	$(icon_DATA)		\
	$(allicons)		\
	$(ossobackup_in_files)	\
	$(ossoscript_in_files)	\
	$(ossoservice_in_files)	\
	$(schema_in_files)	\
	game-names.h

CLEANFILES = \
	ar-resources.c \
	ar-resources.h \
	$(schema_DATA)	\
	$(service_DATA)

DISTCLEANFILES = \
	$(schema_DATA)	\
	$(service_DATA)

@INTLTOOL_SCHEMAS_RULE@

-include $(top_srcdir)/git.mk
