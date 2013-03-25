NULL =

SUBDIRS = icons

if ENABLE_SOUND
SUBDIRS += sounds
endif

dist_noinst_DATA = \
	baize.png \
	baize.xcf \
	slot.svg \
	$(NULL)

pkcatalogdir = $(pkgdatadir)
pkcatalog_DATA = aisleriot.catalog

desktop_in_files = \
	sol.desktop.in.in \
	$(NULL)

desktopdir = $(datadir)/applications
desktop_DATA = $(desktop_in_files:.desktop.in.in=.desktop)

desktop-file-validate: $(desktop_DATA)
	for f in $^; do \
		$(DESKTOP_FILE_VALIDATE) $$f; \
	done

check-local: desktop-file-validate


dist_valgrind_DATA = \
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
