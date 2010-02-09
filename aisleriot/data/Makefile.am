commondatadir = $(datadir)/gnome-games-common

if ENABLE_CARD_THEME_FORMAT_SVG
carddir = $(commondatadir)/cards
dist_card_DATA = \
	gnomangelo_bitmap.svg	\
	$(NULL)
endif # ENABLE_CARD_THEME_FORMAT_SVG

if HAVE_RSVG
slotdir = $(pkgdatadir)/pixmaps
dist_slot_DATA = slot.svg
endif # HAVE_RSVG

if ENABLE_CARD_THEMES_INSTALLER
themeinstalldir = $(commondatadir)
dist_themeinstall_DATA = theme-install.ini
endif # ENABLE_CARD_THEMES_INSTALLER

-include $(top_srcdir)/git.mk
