# Copyright © 2019 Christian Persch
#
# This programme is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at your
# option) any later version.
#
# This programme is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this programme.  If not, see <https://www.gnu.org/licenses/>.

games_sources = [
  'accordion.scm',
  'agnes.scm',
  'athena.scm',
  'auld-lang-syne.scm',
  'aunt-mary.scm',
  'australian-patience.scm',
  'backbone.scm',
  'bakers-dozen.scm',
  'bakers-game.scm',
  'bear-river.scm',
  'beleaguered-castle.scm',
  'block-ten.scm',
  'bristol.scm',
  'camelot.scm',
  'canfield.scm',
  'carpet.scm',
  'chessboard.scm',
  'clock.scm',
  'cover.scm',
  'cruel.scm',
  'diamond-mine.scm',
  'doublets.scm',
  'eagle-wing.scm',
  'easthaven.scm',
  'eight-off.scm',
  'elevator.scm',
  'eliminator.scm',
  'escalator.scm',
  'first-law.scm',
  'fortress.scm',
  'fortunes.scm',
  'forty-thieves.scm',
  'fourteen.scm',
  'freecell.scm',
  'gaps.scm',
  'gay-gordons.scm',
  'giant.scm',
  'glenwood.scm',
  'gold-mine.scm',
  'golf.scm',
  'gypsy.scm',
  'hamilton.scm',
  'helsinki.scm',
  'hopscotch.scm',
  'isabel.scm',
  'jamestown.scm',
  'jumbo.scm',
  'kansas.scm',
  'king-albert.scm',
  'kings-audience.scm',
  'klondike.scm',
  'labyrinth.scm',
  'lady-jane.scm',
  'maze.scm',
  'monte-carlo.scm',
  'napoleons-tomb.scm',
  'neighbor.scm',
  'odessa.scm',
  'osmosis.scm',
  'peek.scm',
  'pileon.scm',
  'plait.scm',
  'poker.scm',
  'quatorze.scm',
  'royal-east.scm',
  'saratoga.scm',
  'scorpion.scm',
  'scuffle.scm',
  'seahaven.scm',
  'sir-tommy.scm',
  'spider-three-decks.scm',
  'spider.scm',
  'spiderette.scm',
  'straight-up.scm',
  'streets-and-alleys.scm',
  'ten-across.scm',
  'terrace.scm',
  'thieves.scm',
  'thirteen.scm',
  'thumb-and-pouch.scm',
  'treize.scm',
  'triple-peaks.scm',
  'union-square.scm',
  'valentine.scm',
  'wall.scm',
  'westhaven.scm',
  'whitehead.scm',
  'will-o-the-wisp.scm',
  'yield.scm',
  'yukon.scm',
  'zebra.scm',
]

module_sources = [
  'api.scm',
]

guile_warnings = [
  '-Warity-mismatch',
  '-Wbad-case-datum',
  '-Wduplicate-case-datum',
  '-Wformat',
  '-Wunbound-variable',
  '-Wunsupported-warning',
  '-Wunused-toplevel',
  '-Wunused-variable',
  '-W1',
]

guile_flags = []

if guile_version.version_compare('>= 3.0')
  guile_flags += [
    '-Ono-letrectify',
  ]
endif

# FIXME: this should use files('...') (allowed as per docs),
# but that crashes meson, see https://gitlab.gnome.org/chpe/meson/issues/2
guile_compile_prog = find_program('guile-compile')

guile_compile = [
  guile_compile_prog,
  meson.current_source_dir(),
  meson.current_build_dir(),
  guile,
  'compile',
  guile_flags,
  guile_warnings,
  '-o',
  '@OUTPUT@',
  '@INPUT@',
]

games_objects = []
modules_objects = []

foreach src : games_sources
  obj = src.split('.')[0] + '.go'
  games_objects += custom_target(
    obj,
    command: guile_compile,
    input: src,
    install: true,
    install_dir: ar_prefix / ar_pkgguiledir,
    output: obj,
)
endforeach

foreach src : module_sources
  obj = src.split('.')[0] + '.go'
  modules_objects += custom_target(
    obj,
    command: guile_compile,
    input: src,
    install: true,
    install_dir: ar_prefix / ar_pkgguiledir / 'aisleriot',
    output: obj,
)
endforeach
