cardimagedir = $(datadir)/pixmaps/cards
cardimage_DATA = Background.xpm Cardback2.xpm Cardback4.xpm \
Pictures.xpm Suits.xpm Cardback1.xpm Cardback3.xpm Cardback5.xpm \
Ranks.xpm

LINK = $(LIBTOOL) --mode=link $(CC) $(CFLAGS) $(LDFLAGS) -o $@

INCLUDES = -I$(includedir) -DCARDIMAGEDIR=\"$(cardimagedir)\"

lib_LTLIBRARIES = libgdkcardimage.la
libgdkcardimage_la_SOURCES = gdk-card-image.c
libgdkcardimageincludedir = $(includedir)/gdkcardimage
libgdkcardimageinclude_HEADERS = gdk-card-image.h
libgdkcardimage_la_LDFLAGS = -version-info 0:0:0 -rpath $(libdir) $(X_LIBS)
libgdkcardimage_la_LIBADD = -lgdk -lglib -lm $(x_libs)

