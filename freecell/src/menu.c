/* menu.c --
   Copyright (C) 1998 Free Software Foundation, Inc.

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
   USA */

/* Written by Changwoo Ryu <cwryu@adam.kaist.ac.kr>. */

#include <config.h>
#include <libgnomeui/gnome-stock.h>
#include <gnome.h>
#include <gtk/gtk.h>

#include "io-gtk.h"
#include "menu.h"
#include "tb-xpms.h"

GnomeUIInfo game_menuinfo[] =
{
  GNOMEUIINFO_MENU_NEW_GAME_ITEM(callback_new, NULL),

  GNOMEUIINFO_MENU_RESTART_GAME_ITEM(callback_restart, NULL),

  GNOMEUIINFO_ITEM_STOCK(N_("New game with seed..."),
			 N_("Start a new game with a different seed"),
			 callback_new_with_seed, GNOME_STOCK_MENU_OPEN),

  GNOMEUIINFO_SEPARATOR,

  GNOMEUIINFO_MENU_UNDO_MOVE_ITEM(callback_undo, NULL),
  
  GNOMEUIINFO_SEPARATOR,
  
  GNOMEUIINFO_MENU_SCORES_ITEM(callback_score, NULL),

  GNOMEUIINFO_SEPARATOR,

  GNOMEUIINFO_MENU_EXIT_ITEM(callback_exit, NULL),

  GNOMEUIINFO_END
};

GnomeUIInfo settings_menuinfo[] = {
  
  /* GNOMEUIINFO_ITEM_STOCK(N_("Game _options..."),
			 N_("Modify the options for this game"),
			 show_rules_options_dialog, GNOME_STOCK_MENU_PREF),

			 GNOMEUIINFO_SEPARATOR,*/

  GNOMEUIINFO_MENU_PREFERENCES_ITEM(callback_option, NULL),

  GNOMEUIINFO_END
};

GnomeUIInfo help_menuinfo[] =
{
  GNOMEUIINFO_HELP("freecell"),

  GNOMEUIINFO_MENU_ABOUT_ITEM(callback_about, NULL),

  GNOMEUIINFO_END
};
  
GnomeUIInfo main_menuinfo[] =
{
  GNOMEUIINFO_MENU_GAME_TREE(game_menuinfo),

  GNOMEUIINFO_MENU_SETTINGS_TREE(settings_menuinfo),

  GNOMEUIINFO_MENU_HELP_TREE(help_menuinfo),

  GNOMEUIINFO_END
};

GnomeUIInfo main_toolbarinfo[] =
{
  GNOMEUIINFO_ITEM_STOCK(N_("New"), N_("Deal a new game"),
			 callback_new, GNOME_STOCK_PIXMAP_NEW),

  GNOMEUIINFO_ITEM_STOCK(N_("Restart"), N_("Start this game over"),
			 callback_restart, GNOME_STOCK_PIXMAP_REFRESH),

  GNOMEUIINFO_ITEM_STOCK(N_("Seed"),
			 N_("Start a new game with a different seed"),
			 callback_new_with_seed, GNOME_STOCK_PIXMAP_OPEN),

  GNOMEUIINFO_SEPARATOR,

  GNOMEUIINFO_ITEM_STOCK(N_("Undo"), N_("Undo the last move"),
			 callback_undo, GNOME_STOCK_PIXMAP_UNDO),

  GNOMEUIINFO_SEPARATOR,

  GNOMEUIINFO_ITEM_STOCK(N_("Score"), N_("View the scores"),
			 callback_score, GNOME_STOCK_PIXMAP_SCORES),

  GNOMEUIINFO_ITEM_STOCK(N_("Props"), N_("Configure Freecell"),
			 callback_option, GNOME_STOCK_PIXMAP_PROPERTIES),

  GNOMEUIINFO_SEPARATOR,

  GNOMEUIINFO_ITEM_STOCK(N_("Exit"), N_("Quit Freecell"),
			 callback_exit, GNOME_STOCK_PIXMAP_EXIT),

  GNOMEUIINFO_END
};

#define ELEMENTS(x) (sizeof(x)/sizeof(x[0]))


void
create_menus (GnomeApp *app)
{
  gnome_app_create_menus (app, main_menuinfo);
  gnome_app_create_toolbar (app, main_toolbarinfo);
}



