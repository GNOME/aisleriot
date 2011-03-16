NULL =

public_icons_themes = hicolor

# These sizes work for maemo3 at least. The "scalable" icon really
# is a 64x54 icon, which is the size that apparently is required for
# the app menu on maemo3.
#
# FIXME: check which sizes are required on maemo4 and maemo5

dist_noinst_DATA = \
	hicolor_apps_16x16_gnome-aisleriot.png \
	hicolor_apps_26x26_gnome-aisleriot.png \
	hicolor_apps_34x34_gnome-aisleriot.png \
	hicolor_apps_40x40_gnome-aisleriot.png \
	hicolor_apps_50x50_gnome-aisleriot.png \
	hicolor_apps_scalable_gnome-aisleriot.png \
	$(NULL)

# The extra treatment for Hildon is necessary because hildon only looks for
# icons in the "hildon" context [https://bugs.maemo.org/show_bug.cgi?id=4766].

install-public-icons:
	for icon in $(dist_noinst_DATA); do \
		THEME=`echo $$icon | cut -d_ -f1`; \
		CONTEXT=hildon; \
		SIZE=`echo $$icon | cut -d_ -f3`; \
		ICONFILE=`echo $$icon | cut -d_ -f4`; \
		mkdir -p $(DESTDIR)$(datadir)/icons/$$THEME/$$SIZE/$$CONTEXT; \
		$(INSTALL_DATA) $(srcdir)/$$icon $(DESTDIR)$(datadir)/icons/$$THEME/$$SIZE/$$CONTEXT/$$ICONFILE; \
	done

uninstall-public-icons:
	-for icon in $(dist_noinst_DATA); do \
		THEME=`echo $$icon | cut -d_ -f1`; \
		CONTEXT=hildon; \
		SIZE=`echo $$icon | cut -d_ -f3`; \
		ICONFILE=`echo $$icon | cut -d_ -f4`; \
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
