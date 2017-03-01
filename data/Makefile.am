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

desktop_in_files = \
	sol.desktop.in.in \
	$(NULL)

desktopdir = $(datadir)/applications
nodist_desktop_DATA = $(desktop_in_files:.desktop.in.in=.desktop)

appdatadir = $(datadir)/metainfo
appdata_in_files = sol.appdata.xml.in
nodist_appdata_DATA = $(appdata_in_files:.xml.in=.xml)

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
	$(NULL)

DISTCLEANFILES = \
	$(nodist_appdata_DATA) \
	$(nodist_desktop_DATA) \
	$(NULL)

@INTLTOOL_DESKTOP_RULE@
@INTLTOOL_XML_RULE@

-include $(top_srcdir)/git.mk
