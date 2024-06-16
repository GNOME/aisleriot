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

# Cards

xmllint_flags = [
  '--nonet',
  '--noblanks',
]

gzip_flags = [
  '-9',
  '-n',
]

cards_sources = [
  'adler.svg',
  'anglo.svg',
  'anglo_poker.svg',
  'atlasnye.svg',
  'atlasnye_skat.svg',
  'clubkarte.svg',
  'guyenne-classic.svg',
  'l-&-h.svg',
  'minimum.svg',
  'minimum_dark.svg',
  'mittelalter.svg',
  'neoclassical.svg',
  'neoclassical_4_color.svg',
  'ornamental.svg',
  'paris.svg',
  'pixelangelo.svg',
  'pixelangelo_compact.svg',
  'plastic.svg',
  'simplistic.svg',
  'swiss-XVII.svg',
  'tango_nuevo.svg',
  'tarot.svg',
  'tigullio.svg',
  'tragy.svg',
  'XSkat.svg',
  'Атласные.svg',
]

cardsdir = ar_pkgdatadir / 'cards'

# FIXME: this should use files('...') (allowed as per docs),
# but that crashes meson, see https://gitlab.gnome.org/chpe/meson/issues/2
compress_svg = find_program('meson_svgz.sh')

cards_svgz = []
foreach svg : cards_sources
  svgz = svg.split('.')[0] + '.svgz'
  cards_svgz += custom_target(
    svgz,
    command: [
      compress_svg,
      xmllint,
      ' '.join(xmllint_flags),
      gzip,
      ' '.join(gzip_flags),
      '@INPUT@',
      '@OUTPUT@'
    ],
    input: svg,
    install: true,
    install_dir: cardsdir,
    output: svgz,
  )
endforeach

# SVGcrush

libxml_dep = dependency('libxml-2.0')

svgcrush_sources = files(
  'svgcrush.c',
)

svgcrush = executable(
  'svgcrush',
  svgcrush_sources,
  dependencies: [glib_dep, libxml_dep,],
  include_directories: [top_inc,],
  install: false,
)
