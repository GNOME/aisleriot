NULL =

noinst_LTLIBRARIES = libaisleriot.la

libaisleriot_la_SOURCES = \
	ar-card.c \
	ar-card.h \
	ar-card-private.h \
	ar-card-theme.c \
	ar-card-theme.h \
	ar-card-theme-private.h \
	ar-card-themes.c \
	ar-card-themes.h \
	ar-pixbuf-utils.c \
	ar-pixbuf-utils.h \
	$(NULL)

if HAVE_GTK_2
libaisleriot_la_SOURCES += \
	ar-card-images.c \
	ar-card-images.h \
	$(NULL)
endif

# FIXME conditional
libaisleriot_la_SOURCES += \
	ar-card-surface-cache.c \
	ar-card-surface-cache.h \
	$(NULL)

if HAVE_CLUTTER
libaisleriot_la_SOURCES += \
	ar-card-textures-cache.c \
	ar-card-textures-cache.h \
	$(NULL)
endif # HAVE_CLUTTER

if HAVE_RSVG
libaisleriot_la_SOURCES += ar-card-theme-preimage.c

if ENABLE_CARD_THEME_FORMAT_SVG
libaisleriot_la_SOURCES += ar-card-theme-svg.c
endif

if ENABLE_CARD_THEME_FORMAT_KDE
libaisleriot_la_SOURCES += ar-card-theme-kde.c
endif

endif # HAVE_RSVG

if ENABLE_CARD_THEME_FORMAT_SLICED
if !HAVE_RSVG
libaisleriot_la_SOURCES += ar-card-theme-preimage.c
endif
libaisleriot_la_SOURCES += ar-card-theme-sliced.c
endif # ENABLE_CARD_THEME_FORMAT_SLICED

if ENABLE_CARD_THEME_FORMAT_PYSOL
libaisleriot_la_SOURCES += ar-card-theme-pysol.c
endif

if ENABLE_CARD_THEME_FORMAT_FIXED
libaisleriot_la_SOURCES += ar-card-theme-fixed.c
endif

libaisleriot_la_CPPFLAGS = \
	-I$(top_srcdir) \
	-I$(top_builddir) \
	$(AM_CPPFLAGS)

libaisleriot_la_CFLAGS = \
	$(GTK_CFLAGS) \
	$(AM_CFLAGS)

libaisleriot_la_LIBADD = \
	$(top_builddir)/libgames-support/libgames-support.la \
	$(GTK_LIBS)

if HAVE_GIO_2_26
libaisleriot_la_CFLAGS += $(GIO_CFLAGS)
libaisleriot_la_LIBADD += $(GIO_LIBS)
endif # HAVE_GIO_2_26

if HAVE_RSVG
libaisleriot_la_CFLAGS += $(RSVG_CFLAGS)
libaisleriot_la_LIBADD += $(RSVG_LIBS)
endif # HAVE_RSVG

if HAVE_CLUTTER
libaisleriot_la_CFLAGS += $(CLUTTER_CFLAGS)
libaisleriot_la_LIBADD += $(CLUTTER_LIBS)
endif # HAVE_CLUTTER

if HAVE_HILDON
libaisleriot_la_CFLAGS += $(HILDON_CFLAGS)
libaisleriot_la_LIBADD += $(HILDON_LIBS)
endif

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
	libaisleriot.la	\
	$(top_builddir)/libgames-support/libgames-support.la \
	$(GTK_LIBS) \
	$(RSVG_LIBS)

if WITH_GTHREAD
ar_cards_renderer_CFLAGS += $(GHTREAD_CFLAGS)
ar_cards_renderer_LDADD += $(GTHREAD_LIBS)
endif # WITH_GTHREAD

endif # HAVE_RSVG

EXTRA_DIST =

CLEANFILES =

DISTCLEANFILES =


-include $(top_srcdir)/git.mk
