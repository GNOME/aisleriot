#if OBJECTIVE_C
#objc_DIRS = 
#endif

if GUILE
guile_DIRS = aisleriot trypeaks
endif

if CXX_PRESENT
cxx_DIRS = gnometris
endif

SUBDIRS = po macros \
	  freecell gnomine same-gnome scottfree mahjongg gturing \
          gyahtzee gnobots gnobots2 gnothello gnotravex gnome-stones \
	  $(guile_DIRS) $(cxx_DIRS) idl metatris

#$(objc_DIRS)

EXTRA_DIST = gnome-games.spec

## to automatically rebuild aclocal.m4 if any of the macros in
## `macros/' change
@MAINT@include macros/macros.dep
@MAINT@macros/macros.dep: macros/Makefile.am
@MAINT@	cd macros && $(MAKE) macros.dep
