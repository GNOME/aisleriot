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

src_inc = include_directories('.')

subdir('lib')

# Aisleriot Solitaire

sol_help_id = 'aisleriot'

sol_sources = libaisleriot_sources + files(
  'ar-application.c',
  'ar-application.h',
  'ar-clock.c',
  'ar-clock.h',
  'ar-cursor.c',
  'ar-cursor.h',
  'ar-game-chooser.c',
  'ar-game-chooser.h',
  'ar-stock.c',
  'ar-stock.h',
  'ar-style-gtk.c',
  'ar-style-gtk.h',
  'ar-style-private.h',
  'ar-style.c',
  'ar-style.h',
  'board-noclutter.c',
  'board-noclutter.h',
  'conf.c',
  'conf.h',
  'game.c',
  'game.h',
  'sol.c',
  'stats-dialog.c',
  'stats-dialog.h',
  'util.c',
  'util.h',
  'window.c',
  'window.h',
)

sol_sources += gnome.compile_resources(
  'ar-resources',
  'aisleriot.gresource.xml',
  c_name: 'aisleriot',
  export: true,
)

sol_cflags = libaisleriot_cflags + [
  '-DDOC_MODULE="@0@"'.format(sol_help_id)
]

sol_deps = libaisleriot_deps + [
  guile_dep,
]

sol = executable(
  'sol',
  c_args: sol_cflags,
  cpp_args: sol_cflags,
  dependencies: sol_deps,
  include_directories: [top_inc, src_inc, lib_inc,],
  install: true,
  sources: sol_sources,
)

# Install gconf schemas

if get_option('gconf')
  gconfschemadir = ar_sysconfdir / 'gconf' / 'schemas'

  gconf_schema_sources = [
    'aisleriot.schemas',
  ]

  install_data(
    gconf_schema_sources,
    install_dir: gconfschemadir,
  )

  meson.add_install_script(
    'meson_gconf.py',
    gconfschemadir,
    gconf_schema_sources,
  )
endif
