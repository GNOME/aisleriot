#if OBJECTIVE_C
#objc_DIRS = 
#endif

#if GUILE
#guile_DIRS = aisleriot
#endif

if CXX_PRESENT
cxx_DIRS = gnometris xbill
endif

SUBDIRS = intl  po macros \
	  gnomine same-gnome mahjongg gtali iagno  idl \
	  gnobots2 gnome-stones gataxx  gnotravex  gnotski \
	  gnibbles glines omf-install $(cxx_DIRS) \
	  aisleriot  freecell iagno
##	  $(guile_DIRS) 

#$(objc_DIRS)

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
@MAINT@include macros/macros.dep
@MAINT@macros/macros.dep: macros/Makefile.am
@MAINT@	cd macros && $(MAKE) macros.dep
