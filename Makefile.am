#if OBJECTIVE_C
#objc_DIRS = 
#endif

if GUILE
guile_DIRS = aisleriot
endif

if CXX_PRESENT
cxx_DIRS = gnometris xbill
endif

#if GDK_PIXBUF
#gdk_pixbuf_DIRS = gnibbles
#endif

#$(gdk_pixbuf_DIRS) \

SUBDIRS = intl po macros \
	  freecell gnomine same-gnome scottfree mahjongg gturing \
          gtali gnobots2 iagno gnotravex gnome-stones gnotski \
	  $(guile_DIRS) $(cxx_DIRS) idl glines gataxx gnibbles omf-install

#$(objc_DIRS)

EXTRA_DIST = \
	gnome-games.spec.in \
	COPYING-DOCS \
	sgmldocs.make \
	game-docs.make \
	MAINTAINERS

dist-hook:
	cp gnome-games.spec $(distdir)

## to automatically rebuild aclocal.m4 if any of the macros in
## `macros/' change
@MAINT@include macros/macros.dep
@MAINT@macros/macros.dep: macros/Makefile.am
@MAINT@	cd macros && $(MAKE) macros.dep
