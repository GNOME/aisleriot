SUBDIRS = icons

if ENABLE_CARD_THEME_FORMAT_SVG
carddir = $(pkgdatadir)/cards
dist_card_DATA = \
	bonded.svg \
	$(NULL)
endif # ENABLE_CARD_THEME_FORMAT_SVG

if HAVE_RSVG
slotdir = $(pkgdatadir)/pixmaps
dist_slot_DATA = slot.svg
endif # HAVE_RSVG

themeinstalldir = $(pkgdatadir)
dist_themeinstall_DATA = theme-install.ini

desktop_in_files = \
	sol.desktop.in.in

if !HAVE_HILDON
desktop_in_files += \
	freecell.desktop.in.in
endif

if HAVE_MAEMO
desktopdir = $(shell $(PKG_CONFIG) osso-af-settings --variable=desktopentrydir)
else
desktopdir = $(datadir)/applications
endif

desktop_DATA = $(desktop_in_files:.desktop.in.in=.desktop)

CLEANFILES = \
	$(desktop_DATA) \
	$(NULL)

DISTCLEANFILES = \
	$(desktop_DATA) \
	$(NULL)

@INTLTOOL_DESKTOP_RULE@

-include $(top_srcdir)/git.mk
