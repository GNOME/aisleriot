ACLOCAL_AMFLAGS = -I m4 ${ACLOCAL_FLAGS}

NULL =

SUBDIRS = src games data po

if ENABLE_CARD_THEME_FORMAT_SVG
SUBDIRS += cards
endif

if BUILD_HELP
SUBDIRS += help
endif

DISTCHECK_CONFIGURE_FLAGS = \
	--with-platform=gnome \
	--disable-schemas-install \
	--disable-scrollkeeper \
	--disable-silent-rules

EXTRA_DIST = \
	aisleriot.doap \
	autogen.sh \
	COPYING-DOCS \
	COPYING.GPL3 \
	COPYING.LGPL3 \
	gnome-doc-utils.make \
	ChangeLog \
	$(NULL)

DISTCLEANFILES = \
	intltool-extract \
	intltool-merge \
	intltool-update \
	gnome-doc-utils.make \
	$(NULL)

MAINTAINERCLEANFILES = \
	ChangeLog \
	$(srcdir)/INSTALL \
	$(srcdir)/aclocal.m4 \
	$(srcdir)/autoscan.log \
	$(srcdir)/compile \
	$(srcdir)/py-compile \
	$(srcdir)/config.guess \
	$(srcdir)/config.h.in \
	$(srcdir)/config.sub \
	$(srcdir)/configure \
	$(srcdir)/configure.scan \
	$(srcdir)/depcomp \
	$(srcdir)/install-sh \
	$(srcdir)/ltmain.sh \
	$(srcdir)/missing \
	$(srcdir)/mkinstalldirs \
	$(srcdir)/gtk-doc.make \
	$(srcdir)/po/Makefile.in.in \
	`find "$(srcdir)" -type f -name Makefile.in -print` \
	$(NULL)

# Ignore score files, these *should* be left behind.
distuninstallcheck_listfiles = find . -type f -print | grep -v '.scores' | grep-v scrollkeeper

# Build ChangeLog from git log
ChangeLog:
	$(AM_V_GEN) if test -f $(top_srcdir)/.git/HEAD; then \
		git log --stat --no-color --since=2008-06-21 > $@; \
	fi

dist: ChangeLog

.PHONY: ChangeLog

-include $(top_srcdir)/git.mk
	
