/* Aisleriot - menu.c
 * Copyright (C) 1998 Jonathan Blandford <jrb@mit.edu>
 *
 * This game is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#define MENU_C

#include <guile/gh.h>
#include "sol.h"
#include "menu.h"
#include "draw.h"
#include "cscmi.h"
/*
 * Menu stuff...
 */


/* Call backs... */

int file_quit_callback (GtkWidget *app, void *data )
{
  gtk_widget_destroy (app);
  gtk_main_quit ();
  
  return TRUE;
}

int game_load_game_callback (GtkWidget *app, void *data )
{
  eval_installed_file((char*) data);
  gh_apply(game_data->start_game_lambda, SCM_EOL);
  refresh_screen();
  return TRUE;
}

int file_new_game_callback (GtkWidget *app, void *data )
{
  gh_apply(game_data->start_game_lambda, SCM_EOL);
  refresh_screen();
  return TRUE;
}

int help_about_callback (GtkWidget *widget, void *data)
{
  return TRUE;
}


GtkMenuEntry same_menu [] = {
	{ _("File/Quit"),"<control>Q", (GtkMenuCallback) file_quit_callback,NULL }, 
	{ _("Game/Variation/Klondike"),NULL, (GtkMenuCallback) game_load_game_callback, "klondike.scm" },
	{ _("Game/Variation/Odessa"),NULL, (GtkMenuCallback) game_load_game_callback, "odessa.scm" },
	{ _("Game/Variation/Osmosis"),NULL, (GtkMenuCallback) game_load_game_callback, "osmosis.scm" },
	{ _("Game/Variation/Spider"),NULL, (GtkMenuCallback) game_load_game_callback, "spider.scm" },
	{ _("Game/Variation/Free Cell"),NULL, (GtkMenuCallback) game_load_game_callback, "freecell.scm" },
	{ _("Game/Variation/<separator>"), NULL, NULL, NULL},
	{ _("Game/Variation/Other..."),NULL, NULL, NULL },
	{ _("Game/New Game"),"<control>N", (GtkMenuCallback) file_new_game_callback, NULL },
	{ _("Game/Select Game Number..."), NULL, NULL, NULL },
	{ _("Game/Options..."), NULL, NULL, NULL },
	{ _("Game/<separator>"), NULL, NULL, NULL},
	{ _("Game/Hint"),"<control>H", NULL, NULL},
	{ _("Game/Pause"),"<control>P", NULL, NULL},
	{ _("Options/Preferences"),"<control>R", NULL, NULL }, 
	{ _("Help/About"), NULL, (GtkMenuCallback) help_about_callback, NULL },
};


GtkMenuFactory* create_menu ()
{
	GtkMenuFactory *subfactory;
	
	subfactory = gtk_menu_factory_new  (GTK_MENU_FACTORY_MENU_BAR);
	gtk_menu_factory_add_entries (subfactory, same_menu, ELEMENTS(same_menu));

	return subfactory;
}
