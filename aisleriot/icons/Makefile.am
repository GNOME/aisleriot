NULL =

SUBDIRS =

if HAVE_HILDON
SUBDIRS += hildon
else
SUBDIRS += gnome
endif

private_icons = \
	hicolor_actions_16x16_cards-deal.png \
	hicolor_actions_22x22_cards-deal.png \
	hicolor_actions_24x24_cards-deal.png \
	hicolor_actions_32x32_cards-deal.png \
	hicolor_actions_48x48_cards-deal.png \
	hicolor_actions_scalable_cards-deal.svg \
	$(NULL)

EXTRA_DIST = \
	$(private_icons) \
	$(NULL)

install-private-icons:
	for icon in $(private_icons); do \
		THEME=`echo $$icon | cut -d_ -f1`; \
		CONTEXT=`echo $$icon | cut -d_ -f2`; \
		SIZE=`echo $$icon | cut -d_ -f3`; \
		ICONFILE=`echo $$icon | cut -d_ -f4`; \
		$(mkdir_p) $(DESTDIR)$(pkgdatadir)/icons/$$THEME/$$SIZE/$$CONTEXT; \
		$(INSTALL_DATA) $(srcdir)/$$icon $(DESTDIR)$(pkgdatadir)/icons/$$THEME/$$SIZE/$$CONTEXT/$$ICONFILE; \
	done

uninstall-private-icons:
	for icon in $(private_icons); do \
		THEME=`echo $$icon | cut -d_ -f1`; \
		CONTEXT=`echo $$icon | cut -d_ -f2`; \
		SIZE=`echo $$icon | cut -d_ -f3`; \
		ICONFILE=`echo $$icon | cut -d_ -f4`; \
		rm -f $(DESTDIR)$(pkgdatadir)/icons/$$THEME/$$SIZE/$$CONTEXT/$$ICONFILE; \
	done

install-data-local: install-private-icons
uninstall-local: uninstall-private-icons

-include $(top_srcdir)/git.mk
