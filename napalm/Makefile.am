INCLUDES = -I$(includedir) $(GNOME_INCLUDEDIR) \
        -DGNOMELOCALEDIR=\""$(datadir)/locale"\"

bin_PROGRAMS = napalm

napalm_SOURCES = game.c	\
		 game.h	\
		 player.c	\
		 gtkdial_hacked.c	\
		 gtkdial_hacked.h

napalm_LDADD = \
	$(GNOME_LIBDIR) $(GNOMEUI_LIBS) $(INTLLIBS)

CLEANFILES=*~
