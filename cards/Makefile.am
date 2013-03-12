NULL =

XMLLINT_FLAGS = --nonet --noblanks
GZIP_FLAGS = -9

SVGS = \
	anglo.svg \
	anglo_bitmap.svg \
	bellot.svg \
	bonded.svg \
	dondorf.svg \
	gnomangelo.svg \
	gnomangelo_bitmap.svg \
	guyenne-classic.svg \
	ornamental.svg \
	paris.svg \
	tango.svg \
	$(NULL)

carddir = $(pkgdatadir)/cards
nodist_card_DATA = $(SVGS:.svg=.svgz)

EXTRA_DIST = \
	README.bellot \
	README.dondorf \
	README.ornamental \
	README.paris \
	$(SVGS)
	$(NULL)

CLEANFILES = \
	$(nodist_card_DATA) \
	$(NULL)

DISTCLEANFILES = \
	$(nodist_card_DATA) \
	$(NULL)

%.svgz: %.svg
	$(AM_V_GEN)$(XMLLINT) $(XMLLINT_FLAGS) $< | $(GZIP) $(GZIP_FLAGS) > $@

# SVG crush utility

svgcrush: svgcrush.c Makefile
	$(AM_V_GEN) $(CC) `$(PKG_CONFIG) --libs --cflags glib-2.0 libxml-2.0` $(AM_CPPFLAGS) $(AM_CFLAGS) $(AM_LDFLAGS) -o $@ $<

EXTRA_DIST += svgcrush.c

# .gitignore

-include $(top_srcdir)/git.mk
