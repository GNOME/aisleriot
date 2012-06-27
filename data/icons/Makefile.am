NULL =

public_icons_themes = hicolor

dist_noinst_DATA = \
	hicolor_apps_16x16_gnome-aisleriot.png \
	hicolor_apps_22x22_gnome-aisleriot.png \
	hicolor_apps_24x24_gnome-aisleriot.png \
	hicolor_apps_32x32_gnome-aisleriot.png \
	hicolor_apps_48x48_gnome-aisleriot.png \
	hicolor_apps_256x256_gnome-aisleriot.png \
	hicolor_apps_16x16_gnome-freecell.png \
	hicolor_apps_22x22_gnome-freecell.png \
	hicolor_apps_24x24_gnome-freecell.png \
	hicolor_apps_32x32_gnome-freecell.png \
	hicolor_apps_48x48_gnome-freecell.png \
	hicolor_apps_256x256_gnome-freecell.png \
	$(NULL)

private_icons = \
	hicolor_actions_16x16_cards-deal.png \
	hicolor_actions_22x22_cards-deal.png \
	hicolor_actions_24x24_cards-deal.png \
	hicolor_actions_32x32_cards-deal.png \
	hicolor_actions_48x48_cards-deal.png \
	hicolor_actions_scalable_cards-deal.svg \
	$(NULL)

EXTRA_DIST = \
	gnome-aisleriot.svg \
	$(private_icons) \
	$(NULL)

install-public-icons:
	for icon in $(dist_noinst_DATA); do \
		THEME=`echo $$icon | cut -d_ -f1`; \
		CONTEXT=`echo $$icon | cut -d_ -f2`; \
		SIZE=`echo $$icon | cut -d_ -f3`; \
		ICONFILE=`echo $$icon | cut -d_ -f4`; \
		$(MKDIR_P) $(DESTDIR)$(datadir)/icons/$$THEME/$$SIZE/$$CONTEXT; \
		$(INSTALL_DATA) $(srcdir)/$$icon $(DESTDIR)$(datadir)/icons/$$THEME/$$SIZE/$$CONTEXT/$$ICONFILE; \
	done

install-private-icons:
	for icon in $(private_icons); do \
		THEME=`echo $$icon | cut -d_ -f1`; \
		CONTEXT=`echo $$icon | cut -d_ -f2`; \
		SIZE=`echo $$icon | cut -d_ -f3`; \
		ICONFILE=`echo $$icon | cut -d_ -f4`; \
		$(MKDIR_P) $(DESTDIR)$(pkgdatadir)/icons/$$THEME/$$SIZE/$$CONTEXT; \
		$(INSTALL_DATA) $(srcdir)/$$icon $(DESTDIR)$(pkgdatadir)/icons/$$THEME/$$SIZE/$$CONTEXT/$$ICONFILE; \
	done

uninstall-public-icons:
	-for icon in $(dist_noinst_DATA); do \
		THEME=`echo $$icon | cut -d_ -f1`; \
		CONTEXT=`echo $$icon | cut -d_ -f2`; \
		SIZE=`echo $$icon | cut -d_ -f3`; \
		ICONFILE=`echo $$icon | cut -d_ -f4`; \
		rm -f $(DESTDIR)$(datadir)/icons/$$THEME/$$SIZE/$$CONTEXT/$$ICONFILE; \
	done

uninstall-private-icons:
	for icon in $(private_icons); do \
		THEME=`echo $$icon | cut -d_ -f1`; \
		CONTEXT=`echo $$icon | cut -d_ -f2`; \
		SIZE=`echo $$icon | cut -d_ -f3`; \
		ICONFILE=`echo $$icon | cut -d_ -f4`; \
		rm -f $(DESTDIR)$(pkgdatadir)/icons/$$THEME/$$SIZE/$$CONTEXT/$$ICONFILE; \
	done

install-data-local: install-public-icons install-private-icons
uninstall-local: uninstall-public-icons uninstall-private-icons

install-data-hook: update-icon-cache
uninstall-hook: update-icon-cache

gtk_update_icon_cache = gtk-update-icon-cache -f -t

update-icon-cache:
	@-if test -z "$(DESTDIR)"; then \
		echo "Updating Gtk icon cache."; \
		for theme in $(public_icons_themes); do \
			$(gtk_update_icon_cache) $(datadir)/icons/$$theme; \
		done; \
	else \
		echo "*** Icon cache not updated.  After (un)install, run this:"; \
		for theme in $(public_icons_themes); do \
			echo "***   $(gtk_update_icon_cache) $(datadir)/icons/$$theme"; \
		done; \
	fi

-include $(top_srcdir)/git.mk
