NULL =

noinst_LTLIBRARIES = libsmclient.la

libsmclient_la_SOURCES = \
	eggsmclient.c \
	eggsmclient.h \
	eggsmclient-private.h \
	$(NULL)

libsmclient_la_CPPFLAGS = \
	-DPKGDATADIR="\"$(pkgdatadir)\"" \
	-DPREFIX="\"$(prefix)\"" \
	-DDATADIR="\"$(datadir)\"" \
	$(AM_CPPFLAGS)

libsmclient_la_CFLAGS = \
	$(GTK_CFLAGS) \
	$(SMCLIENT_CFLAGS) \
	$(AM_CFLAGS)

libsmclient_la_LIBADD = \
	$(GTK_LIBS) \
	$(SMCLIENT_LIBS)

if WITH_SMCLIENT_XSMP
libsmclient_la_SOURCES += \
	eggdesktopfile.c \
	eggdesktopfile.h \
	eggsmclient-xsmp.c \
	$(NULL)
libsmclient_la_CPPFLAGS += -DEGG_SM_CLIENT_BACKEND_XSMP
endif
if WITH_SMCLIENT_WIN32
libsmclient_la_SOURCES += eggsmclient-win32.c
endif
if WITH_SMCLIENT_QUARTZ
libsmclient_la_SOURCES += eggsmclient-osx.c
endif

-include $(top_srcdir)/git.mk
