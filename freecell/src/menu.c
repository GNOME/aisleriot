/* menu.c

   This program is free software; you can redistribute it and'or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Ryu Changwoo <cwryu@eve.kaist.ac.kr>. */

#include <config.h>
#include <libgnomeui/gnome-stock.h>
#include <gnome.h>
#include <gtk/gtk.h>

#include "io-gtk.h"
#include "menu.h"

GnomeMenuInfo game_menuinfo[] =
{
  {GNOME_APP_MENU_ITEM, N_("New"), callback_new, NULL},
  {GNOME_APP_MENU_ITEM, N_("Option"), callback_option, NULL},
  {GNOME_APP_MENU_ITEM, N_("Score"), callback_score, NULL}, 
  {GNOME_APP_MENU_ITEM, N_("Exit"), callback_exit, NULL}, 
  {GNOME_APP_MENU_ENDOFINFO, NULL, NULL, NULL}
};

GnomeMenuInfo help_menuinfo[] =
{
  {GNOME_APP_MENU_ITEM, N_("About"), callback_about, NULL},
  {GNOME_APP_MENU_ENDOFINFO, NULL, NULL, NULL}
};
  
GnomeMenuInfo main_menuinfo[] =
{
  {GNOME_APP_MENU_SUBMENU, N_("Game"), game_menuinfo, NULL},
  {GNOME_APP_MENU_SUBMENU, N_("Help"), help_menuinfo, NULL},
  {GNOME_APP_MENU_ENDOFINFO, NULL, NULL, NULL}
};

GnomeToolbarInfo main_toolbarinfo[] =
{
  {GNOME_APP_TOOLBAR_ITEM, N_("New"), N_("Start a new game"),
   GNOME_APP_PIXMAP_NONE, NULL, callback_new},
  {GNOME_APP_TOOLBAR_ENDOFINFO, NULL, NULL,
   0, NULL, NULL}
};

#define ELEMENTS(x) (sizeof(x)/sizeof(x[0]))


static GtkWidget *window;

void
create_menus (GnomeApp *app)
{
  window = (GtkWidget *)app;
  
  {
    int i;

    for (i = 0; i < ELEMENTS(main_menuinfo); i++)
      main_menuinfo[i].label = _(main_menuinfo[i].label);

    for (i = 0; i < ELEMENTS(game_menuinfo); i++)
      game_menuinfo[i].label = _(game_menuinfo[i].label);

    for (i = 0; i < ELEMENTS(help_menuinfo); i++)
      help_menuinfo[i].label = _(help_menuinfo[i].label);

    for (i = 0; i < ELEMENTS(main_toolbarinfo); i++)
      {
	main_toolbarinfo[i].text = _(main_toolbarinfo[i].text);
	main_toolbarinfo[i].tooltip_text = _(main_toolbarinfo[i].tooltip_text);
      }
  }
  
  gnome_app_create_menus (app, main_menuinfo);
  gnome_app_create_toolbar (app, main_toolbarinfo);
}



