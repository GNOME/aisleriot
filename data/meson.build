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

# Appstream data

metainfo_its = [
  'metainfo.its',
]

metainfodir = ar_prefix / ar_datadir / 'metainfo'

i18n.itstool_join(
  input: 'org.gnome.Aisleriot.metainfo.xml.in',
  install: true,
  install_dir: metainfodir,
  install_tag: 'metadata',
  its_files: metainfo_its,
  mo_targets: gettext_targets[0],
  output: '@BASENAME@',
)

# Desktop file

desktopdatadir = ar_prefix / ar_datadir / 'applications'

desktop_in = files(
  'org.gnome.Aisleriot.desktop.in',
)

i18n.merge_file(
  input: desktop_in,
  output: '@BASENAME@',
  type: 'desktop',
  po_dir: po_dir,
  install: true,
  install_dir: desktopdatadir,
)

meson.add_install_script(
  'meson_desktopfile.py',
  desktopdatadir,
  'org.gnome.Aisleriot.desktop',
)

# Valgrind suppressions

valgrinddir = ar_prefix / ar_libdir / 'valgrind'

valgrind_data = [
  'aisleriot.supp',
]

install_data(
  sources: valgrind_data,
  install_dir: valgrinddir,
)

# Subdirs

subdir('icons')
subdir('sounds')
