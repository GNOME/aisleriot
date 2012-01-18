NULL =

SUBDIRS = icons

if ENABLE_SOUND
SUBDIRS += sounds
endif

dist_noinst_DATA = \
	baize.png \
	slot.svg \
	$(NULL)

pkcatalogdir = $(pkgdatadir)
pkcatalog_DATA = aisleriot.catalog

desktop_in_files = \
	sol.desktop.in.in \
	freecell.desktop.in.in \
	$(NULL)

desktopdir = $(datadir)/applications
desktop_DATA = $(desktop_in_files:.desktop.in.in=.desktop)

EXTRA_DIST = \
	aisleriot.supp \
	$(NULL)

CLEANFILES = \
	$(desktop_DATA) \
	$(pkcatalog_DATA) \
	$(NULL)

DISTCLEANFILES = \
	$(desktop_DATA) \
	$(pkcatalog_DATA) \
	$(NULL)

@INTLTOOL_DESKTOP_RULE@

-include $(top_srcdir)/git.mk
