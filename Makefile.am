
#if GUILE
#guile_DIRS = aisleriot
#endif

# if CXX_PRESENT
# cxx_DIRS = gnometris xbill
# endif

SUBDIRS =			\
	po			\
	libgames-support	\
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
	omf-install

# Not ported yet
EXTRA_SUBDIRS =		\
	$(guile_DIRS)	\
	$(cxx_DIRS)

EXTRA_DIST = \
	gnome-games.spec.in \
	gnome-games.spec \
	COPYING-DOCS \
	sgmldocs.make \
	game-docs.make \
	MAINTAINERS	\
	HACKING

