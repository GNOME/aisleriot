
if GUILE
guile_DIRS = aisleriot
endif

if CXX_PRESENT
cxx_DIRS = gnometris xbill
endif

always_built_SUBDIRS =		\
	po			\
	libgames-support	\
	gnect                   \
	gnomine			\
	same-gnome		\
	mahjongg		\
	gtali			\
	gnobots2		\
	gnome-stones		\
	gataxx			\
	gnotravex		\
	gnotski			\
	gnibbles		\
	glines			\
	iagno			\
	freecell

SUBDIRS = \
	$(always_built_SUBDIRS)	\
	$(cxx_DIRS)		\
	$(guile_DIRS)

DIST_SUBDIRS = \
	$(always_built_SUBDIRS)	\
	aisleriot		\
	gnometris xbill

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

CLEANFILES = \
	intltool-extract \
	intltool-merge \
	intltool-update
