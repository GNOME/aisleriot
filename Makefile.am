
#if GUILE
#guile_DIRS = aisleriot
#endif

# if CXX_PRESENT
# cxx_DIRS = gnometris xbill
# endif

SUBDIRS = po\
	  gnomine same-gnome mahjongg gtali idl \
	  gnobots2 gnome-stones gataxx  gnotravex  gnotski \
	  gnibbles glines omf-install  \
	  iagno
##	  $(guile_DIRS) $(cxx_DIRS) freecell 


EXTRA_DIST = \
	gnome-games.spec.in \
	COPYING-DOCS \
	sgmldocs.make \
	game-docs.make \
	MAINTAINERS	\
	HACKING

dist-hook:
	cp gnome-games.spec $(distdir)

## to automatically rebuild aclocal.m4 if any of the macros in
## `macros/' change
## @MAINT@include macros/macros.dep
## @MAINT@macros/macros.dep: macros/Makefile.am
## @MAINT@	cd macros && $(MAKE) macros.dep
