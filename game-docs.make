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

#sgml_files = \
#	$(sgml_ents) \
#	$(game).sgml

# automake does not know anything about .sgml files yet -> EXTRA_DIST
# EXTRA_DIST = $(game).sgml $(doc_DATA)
EXTRA_DIST = index.html topic.dat $(figs) $(game).sgml

all: index.html

#index.html: $(game)/index.html
#	-cp $(game)/index.html .

index.html: $(srcdir)/$(game).sgml
	cd $(srcdir)
	db2html $(game).sgml
	-cp $(game)/index.html .

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
		$(srcdir)/*.png; do\
	  basefile=`echo $$file | sed -e 's,^.*/,,'`; \
	  $(INSTALL_DATA) $$file $(DESTDIR)$(docdir)/$$basefile; \
	done
	-for file in \
			$(game)/stylesheet-images/*; do \
		basefile=`echo $$file | sed -e 's,^.*/,,'`; \
		$(INSTALL_DATA) $$file $(DESTDIR)$(docdir)/stylesheet-images/$$basefile; \
	done
