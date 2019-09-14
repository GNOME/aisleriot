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

public_icons_themes = [
  'hicolor',
]

public_icons_sources = [
  'hicolor_apps_16x16_gnome-aisleriot.png',
  'hicolor_apps_16x16_gnome-freecell.png',
  'hicolor_apps_22x22_gnome-aisleriot.png',
  'hicolor_apps_22x22_gnome-freecell.png',
  'hicolor_apps_24x24_gnome-aisleriot.png',
  'hicolor_apps_24x24_gnome-freecell.png',
  'hicolor_apps_256x256_gnome-aisleriot.png',
  'hicolor_apps_256x256_gnome-freecell.png',
  'hicolor_apps_32x32_gnome-aisleriot.png',
  'hicolor_apps_32x32_gnome-freecell.png',
  'hicolor_apps_48x48_gnome-aisleriot.png',
  'hicolor_apps_48x48_gnome-freecell.png',
  'hicolor_apps_symbolic_gnome-aisleriot-symbolic.svg',
]

private_icons_sources = [
  'hicolor_actions_16x16_cards-deal.png',
  'hicolor_actions_22x22_cards-deal.png',
  'hicolor_actions_24x24_cards-deal.png',
  'hicolor_actions_32x32_cards-deal.png',
  'hicolor_actions_48x48_cards-deal.png',
  'hicolor_actions_scalable_cards-deal.svg',
]

public_icons_renames = []
private_icons_renames = []

foreach icon : public_icons_sources
  components = icon.split('_')
  public_icons_renames += components[0] / components[2] / components[1] / components[3]
endforeach

foreach icon : private_icons_sources
  components = icon.split('_')
  private_icons_renames += components[0] / components[2] / components[1] / components[3]
endforeach

icondir = ar_datadir / 'icons'
pkgicondir = ar_pkgdatadir / 'icons'

install_data(
  sources: public_icons_sources,
  rename: public_icons_renames,
  install_dir: icondir,
)

install_data(
  sources: private_icons_sources,
  rename: private_icons_renames,
  install_dir: pkgicondir,
)

foreach theme : public_icons_themes
  meson.add_install_script(
    'meson_updateiconcache.py',
    icondir / theme,
  )
endforeach
