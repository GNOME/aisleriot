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
#include <config.h>
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
  GtkWidget *about;
  gchar *authors[] = {
	  "Main program:  Jonathan Blandford (jrb@MIT.EDU)",
	  "Freecell game: ",
	  "Extra games:   ",
          NULL
          };

  about = gnome_about_new ( _("GNOME Solitaire"), VERSION,
        		/* copyrigth notice */
                        "(C) 1998 Jonathan Blandford (jrb@MIT.EDU)",
                        authors,
                        /* another comments */
                        _("The GNOME Generic Solitaire provides a rule-based "
			  "solitaire engine that allows many different games to be played"),
                        NULL);
  gtk_widget_show (about);

  return;
}

/* We fill the items in at runtime... */
GnomeUIInfo variation_menu[] = {
  {GNOME_APP_UI_ENDOFINFO, NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
};

GnomeUIInfo help_menu[] = {
  {GNOME_APP_UI_ITEM, N_("About..."), NULL, help_about_callback,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_ABOUT, 0, 0, NULL},
  {GNOME_APP_UI_ENDOFINFO, NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
};

GnomeUIInfo file_menu[] = {
  {GNOME_APP_UI_ITEM, N_("New"), NULL, file_new_game_callback,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_NEW, 'n', GDK_CONTROL_MASK, NULL},
  {GNOME_APP_UI_SUBTREE, N_("Variation"), NULL, variation_menu,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
  {GNOME_APP_UI_ITEM, N_("Exit"), NULL, file_quit_callback,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_EXIT, 0, 0, NULL},
  {GNOME_APP_UI_ENDOFINFO, NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
};

GnomeUIInfo main_menu[] = {
  {GNOME_APP_UI_SUBTREE, N_("Game"), NULL, file_menu,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
  {GNOME_APP_UI_SUBTREE, N_("Help"), NULL, help_menu,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
  {GNOME_APP_UI_ENDOFINFO, NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
};

typedef struct {
  char *desc, *filename;
} AGame;

/* These should be in *reverse* alphabetical order */
AGame gamelist[] = {
  {N_("Spider"), "spider.scm"},
  {N_("Osmosis"), "osmosis.scm"},
  {N_("Odessa"), "odessa.scm"},
  {N_("Klondike"), "klondike.scm"},
  {N_("Freecell"), "freecell.scm"},
  {NULL, NULL}
};

void create_menus(GnomeApp *app)
{
  int i;
  GtkWidget *w;
  gnome_app_create_menus(app, main_menu);

  for(i = 0; gamelist[i].desc; i++)
    {
      w = gtk_menu_item_new_with_label(_(gamelist[i].desc));
      gtk_widget_show(w);
      gtk_menu_shell_prepend(GTK_MENU_SHELL(variation_menu[0].widget), w);
      gtk_signal_connect(GTK_OBJECT(w), "activate", game_load_game_callback,
			 (gpointer)gamelist[i].filename);
    }
}
