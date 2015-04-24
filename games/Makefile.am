NULL = 

module_GUILE = \
	api.scm \
	$(NULL)

games_GUILE = \
	accordion.scm \
	agnes.scm \
	athena.scm \
	auld-lang-syne.scm \
	aunt-mary.scm \
	backbone.scm \
	bakers-dozen.scm \
	bakers-game.scm \
	bear-river.scm \
	beleaguered-castle.scm \
	block-ten.scm \
	bristol.scm \
	camelot.scm \
	canfield.scm \
	carpet.scm \
	chessboard.scm \
	clock.scm \
	cover.scm \
	cruel.scm \
	diamond-mine.scm \
	doublets.scm \
	eagle-wing.scm \
	easthaven.scm \
	eight-off.scm \
	elevator.scm \
	eliminator.scm \
	escalator.scm \
	first-law.scm \
	fortress.scm \
	fortunes.scm \
	forty-thieves.scm \
	fourteen.scm \
	freecell.scm \
	gaps.scm \
	gay-gordons.scm \
	giant.scm \
	glenwood.scm \
	gold-mine.scm \
	golf.scm \
	gypsy.scm \
	hamilton.scm \
	helsinki.scm \
	hopscotch.scm \
	isabel.scm \
	jamestown.scm \
	jumbo.scm \
	kansas.scm \
	king-albert.scm \
	kings-audience.scm \
	klondike.scm \
	labyrinth.scm \
	lady-jane.scm \
	maze.scm \
	monte-carlo.scm \
	napoleons-tomb.scm \
	neighbor.scm \
	odessa.scm \
	osmosis.scm \
	peek.scm \
	pileon.scm \
	plait.scm \
	poker.scm \
	quatorze.scm \
	royal-east.scm \
	saratoga.scm \
	scorpion.scm \
	scuffle.scm \
	seahaven.scm \
	sir-tommy.scm \
	spider.scm \
	spider-three-decks.scm \
	spiderette.scm \
	straight-up.scm \
	streets-and-alleys.scm \
	ten-across.scm \
	terrace.scm \
	thieves.scm \
	thirteen.scm \
	thumb-and-pouch.scm \
	treize.scm \
	triple-peaks.scm \
	union-square.scm \
	valentine.scm \
	wall.scm \
	westhaven.scm \
	whitehead.scm \
	will-o-the-wisp.scm \
	yield.scm \
	yukon.scm \
	zebra.scm \
	$(NULL)

pkglibguiledir = $(pkglibdir)/guile/$(GUILE_EFFECTIVE_VERSION)
compiledmoduledir = $(pkglibguiledir)/aisleriot

nodist_pkglibguile_DATA = $(games_GUILE:.scm=.go)
nodist_compiledmodule_DATA = $(module_GUILE:.scm=.go)

CLEANFILES = $(nodist_pkglibguile_DATA) $(nodist_compiledmodule_DATA)

AM_V_GUILEC = $(AM_V_GUILEC_$(V))
AM_V_GUILEC_ = $(AM_V_GUILEC_$(AM_DEFAULT_VERBOSITY))
AM_V_GUILEC_0 = @echo "  GUILEC" $@;

GUILE_FLAGS =
# GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat

%.go: %.scm
	$(AM_V_GUILEC) GUILE=$(GUILE) GUILE_FLAGS=$(GUILE_FLAGS) GUILE_AUTO_COMPILE=0 GUILE_LOAD_PATH=.:$(srcdir) GUILE_LOAD_COMPILED_PATH=. $(srcdir)/guile-compile compile $(GUILE_WARNINGS) -o "$@" "$<"

EXTRA_DIST = \
	$(games_GUILE) \
	$(module_GUILE) \
	guile-compile \
	template.scm \
	Rules.HOWTO \
	$(NULL)

-include $(top_srcdir)/git.mk
