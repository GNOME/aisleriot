ACLOCAL_AMFLAGS = -I m4

SUBDIRS= \
	po \
	icons \
	libgames-support \
	$(gamelist)

if ENABLE_SOUND
SUBDIRS += sounds
endif

if ENABLE_TESTS
SUBDIRS += tests
endif

DIST_SUBDIRS = po icons sounds libgames-support tests $(allgames) $(staginggames)

EXTRA_DIST = \
	autogen.sh \
	COPYING-DOCS \
	COPYING.GPL3 \
	xmldocs.make \
	omf.make \
	MAINTAINERS	\
	HACKING		 \
	gnome-doc-utils.make \
	ChangeLog

DISTCLEANFILES = \
	intltool-extract \
	intltool-merge \
	intltool-update \
	gnome-doc-utils.make

# Ignore score files, these *should* be left behind.
distuninstallcheck_listfiles = find . -type f -print | grep -v '.scores' | grep-v scrollkeeper

DISTCHECK_CONFIGURE_FLAGS = \
	--with-platform=gnome \
	--disable-schemas-install \
	--disable-scrollkeeper

# Build ChangeLog from git log
ChangeLog:
	$(AM_V_GEN) if test -f $(top_srcdir)/.git/HEAD; then \
		git log --stat --no-color --since=2008-06-21 > $@; \
	fi

dist: ChangeLog

.PHONY: ChangeLog
