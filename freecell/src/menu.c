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

GnomeUIInfo game_menuinfo[] =
{
  {GNOME_APP_UI_ITEM, N_("New Game"), NULL,
   callback_new, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_NEW, 0, 0, NULL},

  {GNOME_APP_UI_ITEM, N_("Restart Game"), NULL,
   callback_restart, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
  
  {GNOME_APP_UI_ITEM, N_("New Game with a seed..."), NULL,
   callback_new_with_seed, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},

  GNOMEUIINFO_SEPARATOR,

  {GNOME_APP_UI_ITEM, N_("Undo"), NULL,
   callback_undo, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_UNDO, 0, 0, NULL},
  
  GNOMEUIINFO_SEPARATOR,

  {GNOME_APP_UI_ITEM, N_("Properties..."), NULL,
   callback_option, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PROP, 0, 0, NULL},
  
  {GNOME_APP_UI_ITEM, N_("Score..."), NULL,
   callback_score, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
  
  {GNOME_APP_UI_SEPARATOR, NULL, NULL,
   NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
  
  {GNOME_APP_UI_ITEM, N_("Exit"), NULL,
   callback_exit, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_EXIT, 0, 0, NULL},

  {GNOME_APP_UI_ENDOFINFO, NULL, NULL,
   NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL}
};

GnomeUIInfo help_menuinfo[] =
{
  {GNOME_APP_UI_HELP, NULL, NULL,
   NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},

  {GNOME_APP_UI_ITEM, N_("About..."), NULL,
   callback_about, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_ABOUT, 0, 0, NULL},
  
  {GNOME_APP_UI_ENDOFINFO, NULL, NULL,
   NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL}
};
  
GnomeUIInfo main_menuinfo[] =
{
  {GNOME_APP_UI_SUBTREE, N_("Game"), NULL,
   game_menuinfo, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
  
  {GNOME_APP_UI_SUBTREE, N_("Help"), NULL,
   help_menuinfo, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
  
  {GNOME_APP_UI_ENDOFINFO, NULL, NULL,
   NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL}
};

GnomeUIInfo main_toolbarinfo[] =
{
  {GNOME_APP_UI_ITEM, N_("New"), N_("Start a new game"),
   callback_new, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_NEW, 0, 0, NULL},
  
  {GNOME_APP_UI_ITEM, N_("Seed"), NULL,
   callback_new_with_seed, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},

  {GNOME_APP_UI_ITEM, N_("Undo"), N_("Cancel a last move"),
   callback_undo, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_UNDO, 0, 0, NULL},

  {GNOME_APP_UI_ITEM, N_("Score"), N_("Show the score"),
   callback_score, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
  
  {GNOME_APP_UI_ITEM, N_("Props"), N_("Setup Freecell"),
   callback_option, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_PROPERTIES, 0, 0, NULL},
  
  {GNOME_APP_UI_ITEM, N_("Exit"), NULL,
   callback_exit, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_EXIT, 0, 0, NULL},

  {GNOME_APP_UI_ENDOFINFO, NULL, NULL,
   NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL}
};

#define ELEMENTS(x) (sizeof(x)/sizeof(x[0]))


static GtkWidget *window;

void
create_menus (GnomeApp *app)
{
  gnome_app_create_menus (app, main_menuinfo);
  gnome_app_create_toolbar (app, main_toolbarinfo);
}



