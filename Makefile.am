#if OBJECTIVE_C
#objc_DIRS = 
#endif

if GUILE
guile_DIRS = aisleriot
endif

SUBDIRS = intl po macros \
	 freecell gnomine same-gnome scottfree mahjongg \
         gyahtzee gnobots gnothello $(guile_DIRS)
#$(objc_DIRS)

## to automatically rebuild aclocal.m4 if any of the macros in
## `macros/' change
@MAINT@include macros/macros.dep
@MAINT@macros/macros.dep: macros/Makefile.am
@MAINT@	cd macros && $(MAKE) macros.dep
