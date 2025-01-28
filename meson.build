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

project(
  'aisleriot',
  ['c', 'cpp',],
  default_options: [
    'buildtype=release',
    'c_std=gnu11',
    'cpp_std=gnu++11',
    'warning_level=0',
    'b_ndebug=false',
  ],
  license: [
    'GPL-3.0-or-later',
    'LGPL-3.0-or-later',
    'GFDL-1.1-or-later',
    'GFDL-1.3-only',
  ],
  meson_version: '>= 0.62.0',
  version: '3.22.36',
)

# Naming

ar_name = 'aisleriot'

# Requirements

cairo_req_version           = '1.10.0'
gconf_req_version           = '2.0'
gio_req_version             = '2.32.0'
glib_req_version            = '2.32.0'
gtk_req_version             = '3.18.0'
guile_2_0_req_version       = '2.0.0'
guile_2_2_req_version       = '2.2.0'
guile_3_0_req_version       = '3.0.0'
libcanberra_gtk_req_version = '0.26'
qt5svg_req_version          = '5.0.0'
rsvg_req_version            = '2.32.0'

# Versioning

ar_version = meson.project_version()

version_split = meson.project_version().split('.')
ar_major_version = version_split[0].to_int()
ar_minor_version = version_split[1].to_int()
ar_micro_version = version_split[2].to_int()

# i18n

ar_gettext_domain = ar_name

# Directories

ar_prefix        = get_option('prefix')

ar_bindir        = get_option('bindir')
ar_datadir       = get_option('datadir')
ar_includedir    = get_option('includedir')
ar_libdir        = get_option('libdir')
ar_libexecdir    = get_option('libexecdir')
ar_localedir     = get_option('localedir')
ar_mandir        = get_option('mandir')
ar_sysconfdir    = get_option('sysconfdir')

ar_pkgdatadir    = ar_datadir    / ar_name
ar_pkglibdir     = ar_libdir     / ar_name
ar_pkglibexecdir = ar_libexecdir / ar_name

# Debug

enable_debug = get_option('dbg') or get_option('debug') or get_option('buildtype').contains('debug')
enable_debug_ui = get_option('dbgui') and enable_debug

# Meson modules

gnome = import('gnome')
i18n  = import('i18n')
pkg   = import('pkgconfig')

# Compilers

cc = meson.get_compiler('c')
cxx = meson.get_compiler('cpp')

# Meson has a misfeature where it allows the user to override the -std option
# for the C/C++ compiler. Disallow that.

assert(get_option('c_std') == 'gnu11', 'cannot override C std version')
assert(get_option('cpp_std') == 'gnu++11', 'cannot override C++ std version')

# Asserts must not be disabled

assert(get_option('b_ndebug') == 'false', 'assertions may not be disabled')

# Start config.h

config_h = configuration_data()

config_h.set_quoted('GETTEXT_PACKAGE', ar_gettext_domain)
config_h.set_quoted('PACKAGE', ar_name)
config_h.set_quoted('VERSION', ar_version)
config_h.set('ENABLE_DEBUG', enable_debug)
config_h.set('ENABLE_DEBUG_UI', enable_debug_ui)
config_h.set('GNOME_ENABLE_DEBUG', enable_debug)

# FIXME AC_USE_SYSTEM_EXTENSIONS also supported non-gnu systems
config_h.set10('_GNU_SOURCE', true)

# Options

config_h.set('ENABLE_CARD_THEME_FORMAT_FIXED', get_option('theme_fixed'))
config_h.set('ENABLE_CARD_THEME_FORMAT_KDE', get_option('theme_kde'))
config_h.set('ENABLE_CARD_THEME_FORMAT_NATIVE', get_option('theme_svg_qtsvg'))
config_h.set('ENABLE_CARD_THEME_FORMAT_PYSOL', get_option('theme_pysol'))
config_h.set('ENABLE_CARD_THEME_FORMAT_SVG', get_option('theme_svg_rsvg'))

config_h.set_quoted('AR_CARD_THEME_DEFAULT', get_option('default_theme'))

if get_option('default_theme_format') == 'svg-rsvg'
  default_theme_format='svg'
elif get_option('default_theme_format') == 'svg-qtsvg'
  default_teme_format='native'
else
  default_theme_format = get_option('default_theme_format')
endif

config_h.set_quoted('AR_CARD_THEME_DEFAULT_FORMAT_STRING', default_theme_format)

# Asserts must not be disabled

assert(get_option('b_ndebug') == 'false', 'assertions may not be disabled')

# LTO very much NOT supported

assert(get_option('b_lto') == false, 'LTO not supported')

# Compiler flags

compiler_flags_common = [
  '-Wall',
  '-Wcast-align',
  '-Wempty-body',
  '-Wformat-signedness',
  '-Werror=implicit-function-declaration',
  '-Wextra',
  '-Wformat-nonliteral',
  '-Winit-self',
  '-Wlogical-op',
  '-Wmissing-declarations',
  '-Wmissing-format-attribute',
  '-Wno-missing-field-initializers',
  '-Wno-switch-enum',
  '-Wno-unused-parameter',
  '-Wpacked',
  '-Wpointer-arith',
  '-Wshadow',
  '-Wstrict-aliasing=2',
  '-Wundef',
  '-Wuninitialized',
  '-Wunsafe-loop-optimizations',
  '-Wwrite-strings',
  '-fno-common',
]

compiler_flags_c_only = [
  '-Waggregate-return',
  '-Wimplicit',
  '-Wmissing-prototypes',
  '-Wnested-externs',
  '-Wold-style-definition',
  '-Wsign-compare',
  '-Wstrict-prototypes',
]

compiler_flags_cxx_only = [
]

if enable_debug
  compiler_flags_common += [
    '-ggdb3',
  ]
endif

# These are currently needed but the code should be fixed instead
compiler_flags_common_undesirable = [
  '-fno-strict-aliasing'
]

compiler_flags_c_required = [
]

compiler_flags_cxx_required = [
]

global_cflags = cc.get_supported_arguments(compiler_flags_common +
                                           compiler_flags_common_undesirable +
                                           compiler_flags_c_only +
                                           compiler_flags_c_required)

global_cxxflags = cxx.get_supported_arguments(compiler_flags_common +
                                              compiler_flags_common_undesirable +
                                              compiler_flags_cxx_only +
                                              compiler_flags_cxx_required)

foreach flag : compiler_flags_c_required
  assert(cc.has_argument(flag), flag + ' is required but not supported')
endforeach

foreach flag : compiler_flags_cxx_required
  assert(cxx.has_argument(flag), flag + ' is required but not supported')
endforeach

# These flags have to be tested together

compiler_flags_common_multi = [
  ## These only work together with -Wformat
  #[
  #  '-Werror=format=2',
  #  '-Werror=format-nonliteral',
  #  '-Werror=format-security',
  #],
]

foreach flags : compiler_flags_common_multi
  if cc.has_multi_arguments(flags)
    global_cflags += flags
  endif
  if cxx.has_multi_arguments(flags)
    global_cxxflags += flags
  endif
endforeach

# ... and now make these flags the default

add_project_arguments(global_cflags, language: 'c',)
add_project_arguments(global_cxxflags, language: 'cpp',)

# Linker flags

linker_flags = [
]

foreach flag: linker_flags
  assert(cc.has_link_argument(flag), flag + ' is required but not supported')
  add_project_link_arguments(flag, language: 'c',)
  add_project_link_arguments(flag, language: 'cpp',)
endforeach

# Distribution

distro = run_command(
  'bash',
  '-c',
  'source /etc/os-release && echo $ID || echo unknown; exit 0'
).stdout().strip()

# Options

theme_kde_base_paths = {
  'centos':   '/usr/share/carddecks',
  'debian':   '/usr/share/kde4/apps/carddecks',
  'fedora':   '/usr/share/carddecks',
  'opensuse': '/usr/share/kde4/apps/carddecks',
  'rhel':     '/usr/share/carddecks',
  'ubuntu':   '/usr/share/kde4/apps/carddecks',
}

theme_pysol_base_paths = {
  'centos':   '/usr/share/PySolFC',
  'debian':   '/usr/share/games/pysol',
  'fedora':   '/usr/share/PySolFC',
  'opensuse': '/usr/share/games/pysol/data',
  'rhel':     '/usr/share/PySolFC',
  'ubuntu':   '/usr/share/games/pysol',
}

theme_kde_path = get_option('theme_kde_path')
if get_option('theme_kde')
  if theme_kde_path == ''
    foreach id, path : theme_kde_base_paths
      if distro == id
        theme_kde_path = path
        break
      endif
    endforeach
  endif

  assert(theme_kde_path != '', 'Must specify base path to KDE card themes',)
endif

theme_pysol_path = get_option('theme_pysol_path')
if get_option('theme_pysol')
  if theme_pysol_path == ''
    foreach id, path : theme_pysol_base_paths
      if distro == id
        theme_pysol_path = path
        break
      endif
    endforeach
  endif

  assert(theme_pysol_path != '', 'Must specify base path to PySolFC card themes',)
endif

config_h.set_quoted('KDE_CARD_THEME_PATH', theme_kde_path,)
config_h.set_quoted('PYSOL_CARD_THEME_PATH', theme_pysol_path,)

# Guile

if get_option('guile') == 'auto'
  if dependency('guile-3.0', required: false,).found()
    guile_version = '3.0'
  elif dependency('guile-2.2', required: false,).found()
    guile_version = '2.2'
  elif dependency('guile-2.0', required: false,).found()
    guile_version = '2.0'
  else
    assert(false, 'Guile not found')
  endif
else
  guile_version = get_option('guile')
endif

guile_req = 'guile-' + guile_version

if guile_version == '3.0'
  guile_req_version = guile_3_0_req_version
elif guile_version == '2.2'
  guile_req_version = guile_2_2_req_version
elif guile_version == '2.0'
  guile_req_version = guile_2_0_req_version
else
  assert(false, 'unsupported guile version')
endif

ar_pkgguiledir = ar_pkglibdir / 'guile' / guile_version

# Dependencies

cairo_dep = dependency('cairo',     version: '>=' + cairo_req_version,)
gio_dep   = dependency('gio-2.0',   version: '>=' + gio_req_version,)
glib_dep  = dependency('glib-2.0',  version: '>=' + glib_req_version,)
gtk_dep   = dependency('gtk+-3.0',  version: '>=' + gtk_req_version,)
guile_dep = dependency(guile_req,   version: '>=' + guile_req_version,)

if get_option('gconf')
  gconf_dep = dependency('gconf-2.0', version: '>=' + gconf_req_version, required: true,)
  config_h.set('HAVE_GNOME', true)
else
  gconf_dep = dependency('', required: false,)
  config_h.set('HAVE_GNOME', false)
endif

if get_option('sound')
  libcanberra_gtk_dep = dependency('libcanberra-gtk3', version: '>=' + libcanberra_gtk_req_version, required: true,)
  config_h.set('ENABLE_SOUND', true,)
else
  libcanberra_gtk_dep = dependency('', required: false,)
  config_h.set('ENABLE_SOUND', false,)
endif

if get_option('theme_svg_rsvg')
  rsvg_dep = dependency('librsvg-2.0', version: '>=' + rsvg_req_version, required: true,)
  config_h.set('HAVE_RSVG', true)
else
  rsvg_dep = dependency('', required: false,)
  config_h.set('HAVE_RSVG', false)
endif

if get_option('theme_svg_qtsvg') or get_option('theme_kde')
  qt5 = import('qt5')
  qtsvg_dep = dependency('qt5', modules: ['Svg',],)

#  qtsvg_dep = dependency('Qt5Svg', version: '>=' + qt5svg_req_version, required: true,)
  config_h.set('HAVE_QTSVG', true)
else
  qtsvg_dep = dependency('', required: false,)
  config_h.set('HAVE_QTSVG', false)
endif

libm_dep = cc.find_library('m')

# Binreloc

config_h.set('ENABLE_BINRELOC', get_option('binreloc'))

# Auxiliary programmes

guild = guile_dep.get_pkgconfig_variable('guild')
guile = guile_dep.get_pkgconfig_variable('guile')
gzip = find_program('gzip', required: true,)
xmllint = find_program('xmllint', required: true,)

if get_option('gconf')
  gconftool = find_program('gconftool-2', required: true,)
endif

# Documentation

# We support various ways to present help to the user:
# ghelp: using the ghelp: protocol, most likely displaying in Yelp
# file: showing html or xhtml files in the web browser
# library: in the web browser loaded remotedly from library.gnome.org
#
# Note that for help using [x]html files, we can't use gnome-doc-utils.make
# since it doesn't currently support this. The packager will have to use
# gnome-doc-tool directly to build the documentation in the right format.
# The help files must be installed in $(pkgdatadir)/$(DOC_MODULE)/$(LOCALE) .

config_h.set('WITH_HELP_METHOD_GHELP', get_option('help_method') == 'ghelp')
config_h.set('WITH_HELP_METHOD_FILE', get_option('help_method') == 'file')
config_h.set('WITH_HELP_METHOD_LIBRARY', get_option('help_method') == 'library')
config_h.set_quoted('HELP_FILE_FORMAT', get_option('help_format'))

# Write config.h

configure_file(
  output: 'config.h',
  configuration: config_h,
)

# Subdirs

top_inc = include_directories('.')

subdir('po')
subdir('data')
subdir('games')
subdir('src')

if get_option('docs')
  subdir('help')
endif

if get_option('theme_svg_rsvg') or get_option('theme_svg_qtsvg')
  subdir('cards')
endif

# Dist

meson.add_dist_script(
  find_program('meson_changelog.sh'),
)

# Simple compat Makefile

makefile_conf = configuration_data()
makefile_conf.set('srcdir', meson.current_source_dir())
makefile_conf.set('builddir', meson.current_build_dir())

configure_file(
  input: 'Makefile.meson',
  output: '@BASENAME@',
  configuration: makefile_conf,
)

# .gitignore everything in the build directory

configure_file(
  output: '.gitignore',
  command: ['echo', '**/**',],
  capture: true,
  install: false,
)

# Summary

output = '\n'
output += 'Configuration for Aisleriot Solitaire:\n\n'
output += '  Version:                 ' + ar_version + '\n'
output += '\n'
output += '  C compiler:              ' + cc.get_id() + ' (version ' + cc.version() + ')\n'
output += '  C++ compiler:            ' + cxx.get_id() + ' (version ' + cxx.version() + ')\n'
output += '  GUILE compiler:          ' + guile + ' (version ' + guile_version + ')\n'
output += '\n'
output += '  Coverage:                ' + get_option('b_coverage').to_string() + '\n'
output += '  Debug:                   ' + enable_debug.to_string() + '\n'
output += '  Debug UI:                ' + enable_debug_ui.to_string() + '\n'
output += '\n'
output += '  Distribution:            ' + distro + '\n'
output += '  Prefix:                  ' + get_option('prefix') + '\n'
output += '  Desktop data dir:        ' + desktopdatadir + '\n'
output += '  Schemas dir:             ' + schemadir + '\n'
output += '\n'
output += '  Card themes support:\n'
output += '    Prerendered:           ' + get_option('theme_fixed').to_string() + '\n'
output += '    KDE:                   ' + get_option('theme_kde').to_string() + '\n'
if get_option('theme_kde')
  output += '      Base path:           ' + theme_kde_path + '\n'
endif
output += '    PySolFC:               ' + get_option('theme_pysol').to_string() + '\n'
if get_option('theme_pysol')
  output += '      Base path:           ' + theme_pysol_path + '\n'
endif
output += '    SVG using Rsvg:        ' + get_option('theme_svg_rsvg').to_string() + '\n'
output += '    SVG using Qt5Svg:      ' + get_option('theme_svg_qtsvg').to_string() + '\n'
output += '\n'
output += '  Documentation:           ' + get_option('docs').to_string() + '\n'
output += '    Method:                ' + get_option('help_method') + '\n'
if get_option('help_method') == 'file'
output += '      Format:                ' + get_option('help_format') + '\n'
endif
output += '\n'
output += '  Binreloc:                ' + get_option('binreloc').to_string() + '\n'

message(output)

# Done
