SUBDIRS= \
	po \
	libgames-support \
	$(gamelist)

DIST_SUBDIRS = po libgames-support $(allgames)

EXTRA_DIST = \
	gnome-games.spec.in \
	gnome-games.spec \
	COPYING-DOCS \
	xmldocs.make \
	omf.make \
	MAINTAINERS	\
	HACKING		 \
	intltool-extract.in \
	intltool-merge.in    \
	intltool-update.in

DISTCLEANFILES = \
	intltool-extract \
	intltool-merge \
	intltool-update

gtk_update_icon_cache = gtk-update-icon-cache -f -t $(datadir)/icons/hicolor

install-data-hook:
	@-if test -z "$(DESTDIR)"; then \
		echo "Updating Gtk icon cache."; \
		$(gtk_update_icon_cache); \
	else \
		echo "*** Icon cache not updated.  After install, run this:"; \
		echo "***   $(gtk_update_icon_cache)"; \
	fi

# Ignore score files, these *should* be left behind.
distuninstallcheck_listfiles = find . -type f -print | grep -v '.scores' | grep-v scrollkeeper
