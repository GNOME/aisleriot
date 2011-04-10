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

-include $(top_srcdir)/git.mk
