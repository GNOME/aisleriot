
if GUILE
guile_DIRS = aisleriot
endif

if CXX_PRESENT
cxx_DIRS = gnometris xbill
endif

SUBDIRS =			\
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
	freecell 		\
	$(cxx_DIRS)		\
	$(guile_DIRS)

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

