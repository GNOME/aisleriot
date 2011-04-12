NULL =

SUBDIRS = icons

if ENABLE_SOUND
SUBDIRS += sounds
endif

if HAVE_RSVG
slotdir = $(pkgdatadir)/pixmaps
dist_slot_DATA = slot.svg
endif # HAVE_RSVG

themeinstalldir = $(pkgdatadir)
dist_themeinstall_DATA = theme-install.ini

desktop_in_files = \
	sol.desktop.in.in \
	freecell.desktop.in.in \
	$(NULL)

desktopdir = $(datadir)/applications
desktop_DATA = $(desktop_in_files:.desktop.in.in=.desktop)

CLEANFILES = \
	$(desktop_DATA) \
	$(NULL)

DISTCLEANFILES = \
	$(desktop_DATA) \
	$(NULL)

@INTLTOOL_DESKTOP_RULE@

-include $(top_srcdir)/git.mk
