ACLOCAL_AMFLAGS = -I m4

SUBDIRS= \
	po \
	dependencies \
	icons \
	libgames-support \
	tests \
	$(gamelist)

if ENABLE_SOUND
SUBDIRS += sounds
endif

DIST_SUBDIRS = po dependencies icons sounds libgames-support tests $(allgames)

EXTRA_DIST = \
	autogen.sh \
	gnome-games.spec.in \
	gnome-games.spec \
	COPYING-DOCS \
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

# Build ChangeLog from SVN history (r7722 was the first ChangeLog-less checkin).
# Only build this when in an svn checkout.

ChangeLog:
	@if test -f $(top_srcdir)/.svn/entries; then \
		svn log -v --xml -r HEAD:7722 $(SVN_ROOT)/$(SVN_MODULE)/$(SVN_BRANCH) | \
			xsltproc --stringparam strip-prefix "$(SVN_MODULE)/$(SVN_BRANCH)" \
				--stringparam include-rev "yes" $(top_srcdir)/svn2cl.xsl - > $@; \
	fi

dist: ChangeLog

.PHONY: ChangeLog
