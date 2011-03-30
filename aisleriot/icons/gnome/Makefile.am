NULL =

public_icons_themes = hicolor

dist_noinst_DATA = \
	hicolor/16x16/apps/gnome-aisleriot.png \
	hicolor/22x22/apps/gnome-aisleriot.png \
	hicolor/24x24/apps/gnome-aisleriot.png \
	hicolor/26x26/apps/gnome-aisleriot.png \
	hicolor/32x32/apps/gnome-aisleriot.png \
	hicolor/34x34/apps/gnome-aisleriot.png \
	hicolor/40x40/apps/gnome-aisleriot.png \
	hicolor/48x48/apps/gnome-aisleriot.png \
	hicolor/50x50/apps/gnome-aisleriot.png \
	hicolor/64x50/apps/gnome-aisleriot.png \
	hicolor/256x256/apps/gnome-aisleriot.png \
	hicolor/16x16/apps/gnome-freecell.png \
	hicolor/22x22/apps/gnome-freecell.png \
	hicolor/24x24/apps/gnome-freecell.png \
	hicolor/32x32/apps/gnome-freecell.png \
	hicolor/48x48/apps/gnome-freecell.png \
	$(NULL)

install-public-icons:
	for icon in $(dist_noinst_DATA); do \
		THEME=`echo $$icon | cut -d/ -f1`; \
		SIZE=`echo $$icon | cut -d/ -f2`; \
		CONTEXT=`echo $$icon | cut -d/ -f3`; \
		ICONFILE=`echo $$icon | cut -d/ -f4`; \
		mkdir -p $(DESTDIR)$(datadir)/icons/$$THEME/$$SIZE/$$CONTEXT; \
		$(INSTALL_DATA) $(srcdir)/$$icon $(DESTDIR)$(datadir)/icons/$$THEME/$$SIZE/$$CONTEXT/$$ICONFILE; \
	done

uninstall-public-icons:
	-for icon in $(dist_noinst_DATA); do \
		THEME=`echo $$icon | cut -d/ -f1`; \
		SIZE=`echo $$icon | cut -d/ -f2`; \
		CONTEXT=`echo $$icon | cut -d/ -f3`; \
		ICONFILE=`echo $$icon | cut -d/ -f4`; \
		rm -f $(DESTDIR)$(datadir)/icons/$$THEME/$$SIZE/$$CONTEXT/$$ICONFILE; \
	done

install-data-local: install-public-icons
uninstall-local: uninstall-public-icons

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
