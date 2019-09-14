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

lib_inc = include_directories('.')

# Library

libaisleriot_deps = [
  cairo_dep,
  gconf_dep,
  glib_dep,
  gtk_dep,
  libcanberra_gtk_dep,
  libm_dep,
  qtsvg_dep,
  rsvg_dep,
]

libaisleriot_sources = files(
  'ar-card-private.h',
  'ar-card-surface-cache.c',
  'ar-card-surface-cache.h',
  'ar-card-theme-private.h',
  'ar-card-theme.c',
  'ar-card-theme.h',
  'ar-card-themes.c',
  'ar-card-themes.h',
  'ar-card.c',
  'ar-card.h',
  'ar-conf.c',
  'ar-conf.h',
  'ar-debug.c',
  'ar-debug.h',
  'ar-gsettings.c',
  'ar-gsettings.h',
  'ar-help.c',
  'ar-help.h',
  'ar-profile.c',
  'ar-profile.h',
  'ar-runtime.c',
  'ar-runtime.h',
  'ar-show.c',
  'ar-show.h',
  'ar-sound.c',
  'ar-sound.h',
  'ar-string-utils.c',
  'ar-string-utils.h',
)

if get_option('theme_fixed')
  libaisleriot_sources += files(
    'ar-card-theme-fixed.c',
  )
endif

if get_option('theme_kde')
  libaisleriot_sources += files(
    'ar-card-theme-kde.cpp',
  )
endif

if get_option('theme_svg_rsvg')
  libaisleriot_sources += files(
    'ar-card-theme-preimage.c',
    'ar-card-theme-svg.c',
    'ar-svg.c',
    'ar-svg.h',
  )
endif

if get_option('theme_pysol')
  libaisleriot_sources += files(
    'ar-card-theme-pysol.c',
  )
endif

if get_option('theme_svg_qtsvg')
  libaisleriot_sources += files(
    'ar-card-theme-native.cpp',
  )
endif

if get_option('theme_svg_qtsvg') or get_option('theme_kde')
  libaisleriot_sources += files(
    'ar-card-theme-qsvg-private.h',
    'ar-card-theme-qsvg.cpp',
  )
endif

libaisleriot_sources += gnome.genmarshal(
  'ar-marshal',
  internal: true,
  prefix: 'ar_marshal',
  sources: 'ar-marshal.list',
)

libaisleriot_cflags = [
  '-DDATADIR="@0@"'.format(ar_prefix / ar_datadir),
  '-DLIBDIR="@0@"'.format(ar_prefix / ar_libdir),
  '-DPKGDATADIR="@0@"'.format(ar_prefix / ar_pkgdatadir),
  '-DPKGLIBDIR="@0@"'.format(ar_prefix / ar_pkglibdir),
  '-DPREFIX="@0@"'.format(ar_prefix),
  '-DGDK_DISABLE_DEPRECATION_WARNINGS',
]

# Auxiliary programme to prerender card images

if get_option('theme_svg_rsvg')
  render_cards_sources = libaisleriot_sources + files(
    'render-cards.c',
  )

  render_cards = executable(
    'ar-cards-renderer',
    c_args: libaisleriot_cflags,
    cpp_args: libaisleriot_cflags,
    dependencies: libaisleriot_deps,
    include_directories: [top_inc, lib_inc,],
    install: true,
    install_dir: ar_pkglibexecdir,
    sources: render_cards_sources,
  )
endif

# Settings schemas

schemadir = ar_prefix / ar_datadir / 'glib-2.0' / 'schemas'

install_data(
  'org.gnome.Patience.WindowState.gschema.xml',
  install_dir: schemadir,
)

meson.add_install_script(
  'meson_compileschemas.py',
  schemadir,
)
