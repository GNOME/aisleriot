# This file was shamelessly stolen and adapted from applet-docs.make
# in gnome-core
#
# This file gets included by the games' help-dir Makefile.am
# They should set $(game) to their name, and $(figs) to the
# images they reference
#
# Sample:
#
# game = iagno
# lang = C
# figs = ANIMATION.png BLOCK.png FIRST.png PLAYER.png START.png
# include $(top_srcdir)/game-docs.make

docdir = $(datadir)/gnome/help/$(game)/$(lang)
doc_DATA = \
	index.html \
	topic.dat \
	$(figs)

sgml_files = \
	$(sgml_ents) \
	$(game).sgml

# automake does not know anything about .sgml files yet -> EXTRA_DIST
EXTRA_DIST = $(sgml_files) $(doc_DATA)

all: index.html

index.html: $(game)/index.html
	-cp $(game)/index.html .

$(game).sgml: $(sgml_ents)
	-ourdir=`pwd`; \
	cd $(srcdir); \
	cp $(sgml_ents) $$ourdir

$(game)/index.html: $(game).sgml
	-srcdir=`cd $(srcdir) && pwd`; \
	db2html $$srcdir/$(game).sgml

$(game)-dist-hook: index.html
	-$(mkinstalldirs) $(distdir)/$(game)/stylesheet-images
	-cp $(srcdir)/$(game)/*.html ($distdir)/$(game)
	-cp $(srcdir)/$(game)/*.css  ($distdir)/$(game)
	-cp $(srcdir)/$(game)/*.png  ($distdir)/$(game)
	-cp $(srcdir)/$(game)/stylesheet-images/* \
		$(distdir)/$(game)/stylesheet-images

install-data-am: index.html
	-$(mkinstalldirs) $(DESTDIR)$(docdir)/stylesheet-images
	-cp $(srcdir)/topic.dat $(DESTDIR)$(docdir)
	-for file in \
			$(game)/*.html \
			$(game)/*.css \
			$(srcdir)/*.png do\
		basefile=`echo $$file | sed -e 's,^.*/,,'`; \
		$(INSTALL_DATA) $$file $(DESTDIR)$(docdir)/$$basefile; \
	done
	-for file in \
			$(game)/stylesheet-images/*; do \
		basefile=`echo $$file | sed -e 's,^.*/,,'`; \
		$(INSTALL_DATA) $$file $(DESTDIR)$(docdir)/stylesheet-images/$$basefile; \
	done
