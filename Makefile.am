if OBJECTIVE_C
objc_DIRS = phaser-chess
endif

if GUILE
guile_DIRS = aisleriot
endif

SUBDIRS = intl po macros support \
	 freecell gnomine same-gnome scottfree mahjongg $(guile_DIRS) $(objc_DIRS)

## to automatically rebuild aclocal.m4 if any of the macros in
## `macros/' change
@MAINT@include macros/macros.dep
@MAINT@macros/macros.dep: macros/Makefile.am
@MAINT@	cd macros && $(MAKE) macros.dep
