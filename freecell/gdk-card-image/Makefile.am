cardbackdir = $(datadir)/pixmaps/cards/backs/
cardback_DATA = beige.png face.png rods.png grey.png

cardhonordir = $(datadir)/pixmaps/cards/honors/
cardhonor_DATA = bonded.png xpat.png

cardjokerdir = $(datadir)/pixmaps/cards/jokers/
cardjoker_DATA = gnome.png

cardrankdir = $(datadir)/pixmaps/cards/ranks/
cardrank_DATA = bold-09x14.png fine-07x12.png

cardsuitsmalldir = $(datadir)/pixmaps/cards/suits_small/
cardsuitsmall_DATA = \
	knuth-09x10.png xpat-11x11.png xpat-15x15.png

cardsuitmediumdir = $(datadir)/pixmaps/cards/suits_medium/
cardsuitmedium_DATA = \
	knuth-09x10.png knuth-15x19.png knuth-18x21.png knuth-21x25.png \
	xpat-11x11.png xpat-15x15.png xpat-21x21.png

cardsuitlargedir = $(datadir)/pixmaps/cards/suits_large/
cardsuitlarge_DATA = \
	knuth-09x10.png knuth-15x19.png knuth-18x21.png knuth-21x25.png \
	xpat-11x11.png xpat-15x15.png xpat-21x21.png xpat-41x41.png

cardslotdir = $(datadir)/pixmaps/cards/slots/
cardslot_DATA = hand.png plain.png

INCLUDES = -I$(includedir) $(GNOME_INCLUDEDIR)

EXTRA_DIST = $(cardback_DATA) $(cardhonor_DATA) $(cardjoker_DATA) \
	$(cardrank_DATA) $(cardsuitsmall_DATA) $(cardsuitlarge_DATA)\
	$(cardsuitmedium_DATA) $(cardslot_DATA)

lib_LTLIBRARIES = libgdkcardimage.la
libgdkcardimage_la_SOURCES = gdk-card-image.c
libgdkcardimageincludedir = $(includedir)/gdkcardimage
libgdkcardimageinclude_HEADERS = gdk-card-image.h
libgdkcardimage_la_LDFLAGS = -version-info 0:0:0
libgdkcardimage_la_LIBADD = $(GTK_LIBS)

