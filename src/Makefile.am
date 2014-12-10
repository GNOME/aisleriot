SUBDIRS = lib

NULL =

# Same as in help/Makefile.am
DOC_MODULE = aisleriot

bin_PROGRAMS = sol

sol_SOURCES = \
	ar-application.c \
	ar-application.h \
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

sol_CPPFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/src/lib \
	-Ilib \
	-DDOC_MODULE=\"$(DOC_MODULE)\" \
	-DGDK_DISABLE_DEPRECATION_WARNINGS \
	$(AM_CPPFLAGS)

sol_CFLAGS = \
	$(AISLERIOT_CFLAGS) \
	$(AM_CFLAGS)

sol_LDFLAGS = \
	$(EXTRA_CXX_LIBS) \
	$(AM_LDFLAGS)

sol_LDADD = \
	lib/libaisleriot.la \
	$(AISLERIOT_LIBS) \
	-lm

if PLATFORM_WIN32_NATIVE
sol_LDFLAGS += -mwindows
endif

BUILT_SOURCES = \
	ar-resources.c \
	ar-resources.h \
	$(NULL)

ar-resources.h ar-resources.c: aisleriot.gresource.xml Makefile $(shell $(GLIB_COMPILE_RESOURCES) --generate-dependencies aisleriot.gresource.xml)
	$(AM_V_GEN) XMLLINT=$(XMLLINT) $(GLIB_COMPILE_RESOURCES) --target $@ --sourcedir . --generate --c-name aisleriot $<

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
	$(schema_in_files)	\
	aisleriot-ui.xml	\
	aisleriot-debug-ui.xml	\
	game-names.h

CLEANFILES = \
	$(BUILT_SOURCES) \
	$(schema_DATA)	\
	$(service_DATA)

DISTCLEANFILES = \
	$(schema_DATA)	\
	$(service_DATA)

@INTLTOOL_SCHEMAS_RULE@

-include $(top_srcdir)/git.mk
