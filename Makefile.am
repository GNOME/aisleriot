
if GUILE
guile_DIRS = aisleriot
endif

if HAVE_CXX
cxx_DIRS = gnometris \
	blackjack
endif

always_built_SUBDIRS =		\
	po			\
	libgames-support	\
	gnect                   \
	gnomine			\
	same-gnome		\
	mahjongg		\
	gtali			\
	gnome-stones		\
	gataxx			\
	gnotravex		\
	gnotski			\
	glines			\
	iagno			\
	gdk-card-image		\
	gnobots2		\
	gnibbles		

SUBDIRS = \
	$(always_built_SUBDIRS)	\
	$(cxx_DIRS)		\
	$(guile_DIRS)

DIST_SUBDIRS = \
	$(always_built_SUBDIRS)	\
	aisleriot		\
	blackjack		\
	gnometris

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
