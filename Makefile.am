#if OBJECTIVE_C
#objc_DIRS = 
#endif

if GUILE
guile_DIRS = aisleriot
endif

SUBDIRS = macros \
	 freecell gnomine same-gnome scottfree mahjongg gturing \
         gyahtzee gnobots gnothello gnotravex gnome-stones $(guile_DIRS) idl
	 
#$(objc_DIRS)

EXTRA_DIST = gnome-games.spec

## to automatically rebuild aclocal.m4 if any of the macros in
## `macros/' change
@MAINT@include macros/macros.dep
@MAINT@macros/macros.dep: macros/Makefile.am
@MAINT@	cd macros && $(MAKE) macros.dep
