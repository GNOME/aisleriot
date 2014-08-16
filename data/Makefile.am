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
nodist_desktop_DATA = $(desktop_in_files:.desktop.in.in=.desktop)

appdatadir = $(datadir)/appdata
appdata_in_files = aisleriot.appdata.xml.in
nodist_appdata_DATA = $(appdata_in_files:.xml.in=.xml)

APPDATA_VALIDATE_FLAGS = --nonet
appdata-validate: $(nodist_appdata_DATA)
	for f in $^; do \
		$(APPDATA_VALIDATE) $(APPDATA_VALIDATE_FLAGS) $$f; \
	done

desktop-file-validate: $(nodist_desktop_DATA)
	for f in $^; do \
		$(DESKTOP_FILE_VALIDATE) $$f; \
	done


check-local: desktop-file-validate

dist_valgrind_DATA = \
	aisleriot.supp \
	$(NULL)

EXTRA_DIST = \
	$(appdata_in_files) \
	$(NULL)

CLEANFILES = \
	$(nodist_appdata_DATA) \
	$(nodist_desktop_DATA) \
	$(pkcatalog_DATA) \
	$(NULL)

DISTCLEANFILES = \
	$(nodist_appdata_DATA) \
	$(nodist_desktop_DATA) \
	$(pkcatalog_DATA) \
	$(NULL)

@INTLTOOL_DESKTOP_RULE@
@INTLTOOL_XML_RULE@

-include $(top_srcdir)/git.mk
