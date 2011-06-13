NULL =

noinst_LTLIBRARIES = libaisleriot.la

BUILT_SOURCES = \
	ar-marshal.c \
	ar-marshal.h \
	$(NULL)

libaisleriot_la_SOURCES = \
	ar-card.c \
	ar-card.h \
	ar-card-private.h \
	ar-card-surface-cache.c \
	ar-card-surface-cache.h \
	ar-card-theme.c \
	ar-card-theme.h \
	ar-card-theme-private.h \
	ar-card-themes.c \
	ar-card-themes.h \
	ar-conf.c \
	ar-conf.h \
	ar-debug.c \
	ar-debug.h \
	ar-gsettings.c \
	ar-gsettings.h \
	ar-help.c \
	ar-help.h \
	ar-profile.c \
	ar-profile.h \
	ar-runtime.c \
	ar-runtime.h \
	ar-show.c \
	ar-show.h \
	ar-sound.c \
	ar-sound.h \
	ar-string-utils.c \
	ar-string-utils.h \
	$(BUILT_SOURCES) \
	$(NULL)

if HAVE_CLUTTER
libaisleriot_la_SOURCES += \
	ar-card-textures-cache.c \
	ar-card-textures-cache.h \
	$(NULL)
endif # HAVE_CLUTTER

if HAVE_RSVG
libaisleriot_la_SOURCES += \
	ar-svg.c \
	ar-svg.h \
	ar-card-theme-preimage.c \
	$(NULL)

if ENABLE_CARD_THEME_FORMAT_SVG
libaisleriot_la_SOURCES += ar-card-theme-svg.c
endif

if ENABLE_CARD_THEME_FORMAT_KDE
libaisleriot_la_SOURCES += ar-card-theme-kde.c
endif

endif # HAVE_RSVG

if ENABLE_CARD_THEME_FORMAT_PYSOL
libaisleriot_la_SOURCES += ar-card-theme-pysol.c
endif

if ENABLE_CARD_THEME_FORMAT_FIXED
libaisleriot_la_SOURCES += ar-card-theme-fixed.c
endif

libaisleriot_la_CPPFLAGS = \
	-I$(top_srcdir) \
	-I$(top_builddir) \
	-DPKGDATADIR="\"$(pkgdatadir)\"" \
	-DPREFIX="\"$(prefix)\"" \
	-DDATADIR="\"$(datadir)\"" \
	-DCOMMON_DATADIR="\"$(datadir)/gnome-games-common\"" \
	-DSCORESDIR="\"$(scoredir)\"" \
	$(AM_CPPFLAGS)

libaisleriot_la_CFLAGS = \
	$(GTK_CFLAGS) \
	$(GIO_CFLAGS) \
	$(AM_CFLAGS)

libaisleriot_la_LIBADD = \
	$(GTK_LIBS) \
	$(GIO_LIBS)

if HAVE_RSVG
libaisleriot_la_CFLAGS += $(RSVG_CFLAGS)
libaisleriot_la_LIBADD += $(RSVG_LIBS)
endif # HAVE_RSVG

if HAVE_CLUTTER
libaisleriot_la_CFLAGS += $(CLUTTER_CFLAGS)
libaisleriot_la_LIBADD += $(CLUTTER_LIBS)
endif # HAVE_CLUTTER

if HAVE_GNOME
libaisleriot_la_CFLAGS += $(GNOME_CFLAGS)
libaisleriot_la_LIBADD += $(GNOME_LIBS)
endif

if ENABLE_SOUND
libaisleriot_la_CFLAGS += $(CANBERRA_GTK_CFLAGS)
libaisleriot_la_LIBADD += $(CANBERRA_GTK_LIBS)
endif

gsettingsschema_in_files = org.gnome.Patience.WindowState.gschema.xml.in
gsettings_SCHEMAS = $(gsettingsschema_in_files:.gschema.xml.in=.gschema.xml)

ar-marshal.c: stamp-ar-marshal.c
	@true
stamp-ar-marshal.c: ar-marshal.list Makefile
	$(AM_V_GEN) $(GLIB_GENMARSHAL) --prefix=ar_marshal $< --header --body --internal > ar-marshal.c \
	&& echo timestamp > $(@F)

ar-marshal.h: stamp-ar-marshal.h
	@true
stamp-ar-marshal.h: ar-marshal.list Makefile
	$(AM_V_GEN) $(GLIB_GENMARSHAL) --prefix=ar_marshal $< --header --internal > ar-marshal.h \
	&& echo timestamp > $(@F)

# Auxiliary programme to prerender card images

if HAVE_RSVG

pkglib_PROGRAMS = ar-cards-renderer

ar_cards_renderer_SOURCES = \
	render-cards.c

ar_cards_renderer_CPPFLAGS = \
	-I$(top_srcdir) \
	-I$(top_builddir) \
	$(AM_CPPFLAGS)

ar_cards_renderer_CFLAGS = \
	$(GTK_CFLAGS) \
	$(RSVG_CFLAGS) \
	$(AM_CFLAGS)

ar_cards_renderer_LDFLAGS = \
	$(AM_LDFLAGS)

ar_cards_renderer_LDADD = \
	libaisleriot.la \
	$(GTK_LIBS) \
	$(RSVG_LIBS)

if WITH_GTHREAD
ar_cards_renderer_CFLAGS += $(GHTREAD_CFLAGS)
ar_cards_renderer_LDADD += $(GTHREAD_LIBS)
endif # WITH_GTHREAD

endif # HAVE_RSVG

stamp_files = \
	stamp-ar-marshal.c \
	stamp-ar-marshal.h \
	$(NULL)

EXTRA_DIST = \
	ar-marshal.list \
	$(gsettingsschema_in_files) \
        $(NULL)

CLEANFILES = \
	$(BUILT_SOURCES) \
        $(gsettings_SCHEMAS) \
	$(stamp_files) \
	$(NULL)

DISTCLEANFILES = \
	$(BUILT_SOURCES) \
        $(gsettings_SCHEMAS) \
	$(stamp_files) \
	$(NULL)

@INTLTOOL_XML_NOMERGE_RULE@
@GSETTINGS_RULES@

-include $(top_srcdir)/git.mk
