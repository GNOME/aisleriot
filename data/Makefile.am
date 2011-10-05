NULL =

SUBDIRS = icons

if ENABLE_SOUND
SUBDIRS += sounds
endif

XMLLINT_FLAGS = --nonet --noblanks
GZIP_FLAGS = -9

SVGS = \
	slot.svg \
	$(NULL)

if HAVE_RSVG
slotdir = $(pkgdatadir)/pixmaps
nodist_slot_DATA = $(SVGS:.svg=.svgz)
endif # HAVE_RSVG

themeinstalldir = $(pkgdatadir)
dist_themeinstall_DATA = theme-install.ini

desktop_in_files = \
	sol.desktop.in.in \
	freecell.desktop.in.in \
	$(NULL)

desktopdir = $(datadir)/applications
desktop_DATA = $(desktop_in_files:.desktop.in.in=.desktop)

EXTRA_DIST = \
	aisleriot.supp \
	$(SVGS)
	$(NULL)

CLEANFILES = \
	$(desktop_DATA) \
	$(nodist_slot_DATA) \
	$(NULL)

DISTCLEANFILES = \
	$(desktop_DATA) \
	$(nodist_slot_DATA) \
	$(NULL)

%.svgz: %.svg
	$(AM_V_GEN)$(XMLLINT) $(XMLLINT_FLAGS) $< | $(GZIP) $(GZIP_FLAGS) > $@

@INTLTOOL_DESKTOP_RULE@

-include $(top_srcdir)/git.mk
