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

#include <config.h>
#include <dirent.h>
#include "gnome.h"
#include "sol.h"
#include "menu.h"
#include "dialog.h"

void restart_game ()
{
  new_game (NULL, &seed);
};

void random_seed ()
{
  new_game (NULL, NULL);
};

void new_rules (GtkWidget* w, gchar* file) 
{
  new_game (file, NULL);
};

void undo_callback ()
{
  gh_eval_str ("(undo)");
  refresh_screen();
}

void redo_callback ()
{
  gh_eval_str ("(redo)");
  refresh_screen();
}

void help_about_callback ()
{
  GtkWidget *about;
  const gchar *authors[] = {
	  "Main program:  Jonathan Blandford (jrb@mit.edu)",
	  "                      Felix Bellaby (felix@pooh.u-net.com)",
	  "Card Games:    Jonathan Blandford (jrb@mit.edu)",
	  "                      Ryu Changwoo (cwryu@eve.kaist.ac.kr)",
	  "                      Rosanna Yuen (rwsy@mit.edu)",
          NULL
          };

  about = gnome_about_new ( _("GNOME Solitaire"), VERSION,
        		/* copyright notice */
                        "(C) 1998 Jonathan Blandford (jrb@MIT.EDU)",
                        (const char **)authors,
                        /* another comments */
                        _("The GNOME Generic Solitaire provides a rule-based "
			  "solitaire engine that allows many different games to be played"),
                        NULL);
  gtk_widget_show (about);

  return;
}

GnomeUIInfo rules_sub_menu[] = {
  GNOMEUIINFO_END
};

GnomeUIInfo rules_menu[] = {
  GNOMEUIINFO_SUBTREE(N_("_Select"), rules_sub_menu),

  GNOMEUIINFO_ITEM_STOCK(N_("_Options..."), NULL, 
			 show_rules_options_dialog, GNOME_STOCK_MENU_PREF),

  GNOMEUIINFO_ITEM_STOCK(N_("S_tatistics..."), NULL, 
			 show_rules_stats_dialog, GNOME_STOCK_MENU_BOOK_BLUE),

  GNOMEUIINFO_END
};

GnomeUIInfo help_menu[] = {
  GNOMEUIINFO_ITEM_STOCK(N_("_About..."), NULL, 
			 help_about_callback, GNOME_STOCK_MENU_ABOUT),
  GNOMEUIINFO_SEPARATOR,

  GNOMEUIINFO_HELP("aisleriot"),

  GNOMEUIINFO_END
};

GnomeUIInfo game_menu[] = {
  GNOMEUIINFO_ITEM_STOCK(N_("_New"), NULL, 
			 random_seed, GNOME_STOCK_MENU_NEW),

  GNOMEUIINFO_ITEM_STOCK(N_("_Restart"), NULL,
			 restart_game, GNOME_STOCK_MENU_REFRESH),

  GNOMEUIINFO_ITEM_STOCK(N_("_Select..."), NULL, 
			 show_select_game_dialog, GNOME_STOCK_MENU_OPEN),

  GNOMEUIINFO_ITEM_STOCK(N_("_Properties..."), NULL, 
			 show_property_dialog, GNOME_STOCK_MENU_PREF),

  GNOMEUIINFO_ITEM_STOCK(N_("S_tatistics..."), NULL, 
			 show_global_stats_dialog, GNOME_STOCK_MENU_BOOK_BLUE),

  GNOMEUIINFO_SEPARATOR,

  GNOMEUIINFO_ITEM_STOCK(N_("E_xit"), NULL, 
			 quit_app, GNOME_STOCK_MENU_EXIT),
  GNOMEUIINFO_END
};

GnomeUIInfo move_menu[] = {
  GNOMEUIINFO_ITEM_STOCK(N_("_Hint"), NULL, 
			 show_hint_dialog, GNOME_STOCK_MENU_JUMP_TO),

  GNOMEUIINFO_ITEM_STOCK(N_("_Undo"), NULL, 
			 undo_callback, GNOME_STOCK_MENU_UNDO),

  GNOMEUIINFO_ITEM_STOCK(N_("_Redo"), NULL, 
			 redo_callback, GNOME_STOCK_MENU_REDO),

  GNOMEUIINFO_END
};

GnomeUIInfo top_menu[] = {
  GNOMEUIINFO_SUBTREE(N_("_Game"), game_menu),

  GNOMEUIINFO_SUBTREE(N_("_Variation"), rules_menu),

  GNOMEUIINFO_SUBTREE(N_("_Move"), move_menu),

  GNOMEUIINFO_SUBTREE(N_("_Help"), help_menu),

  GNOMEUIINFO_END
};

GnomeUIInfo toolbar[] =
{
  GNOMEUIINFO_ITEM_STOCK(N_("New"), N_("Deal a new game."),
			 random_seed, GNOME_STOCK_PIXMAP_NEW),

  GNOMEUIINFO_ITEM_STOCK(N_("Restart"), N_("Start this game over."),
			 restart_game, GNOME_STOCK_PIXMAP_REFRESH),

  GNOMEUIINFO_ITEM_STOCK(N_("Select"), N_("Select a new game."),
			 show_select_game_dialog, GNOME_STOCK_PIXMAP_OPEN),

  GNOMEUIINFO_SEPARATOR,

  GNOMEUIINFO_ITEM_STOCK(N_("Hint"), N_("Suggest a move."),
			 show_hint_dialog, GNOME_STOCK_PIXMAP_JUMP_TO),

  GNOMEUIINFO_ITEM_STOCK(N_("Undo"), N_("Undo the last move."),
			 undo_callback, GNOME_STOCK_PIXMAP_UNDO),

  GNOMEUIINFO_ITEM_STOCK(N_("Redo"), N_("Redo the last move."),
			 redo_callback, GNOME_STOCK_PIXMAP_REDO),

  GNOMEUIINFO_SEPARATOR,

  GNOMEUIINFO_ITEM_STOCK(N_("Exit"), N_("Quit Aisleriot."),
			 quit_app, GNOME_STOCK_PIXMAP_EXIT),

  GNOMEUIINFO_END
};

void create_menus ()
{
  int i;
  GtkWidget *w;
  gnome_app_create_menus (GNOME_APP(app), top_menu);
  gnome_app_create_toolbar (GNOME_APP(app), toolbar);

  for(i = 0; i < n_games; i++) {
    w = gtk_menu_item_new_with_label 
      (game_file_to_name (game_dents[i]->d_name));
    gtk_widget_show(w);
    gtk_menu_shell_append (GTK_MENU_SHELL(rules_sub_menu[0].widget), w);
    gtk_signal_connect (GTK_OBJECT(w), "activate", 
			(GtkSignalFunc) new_rules,
			(gpointer) game_dents[i]->d_name);
  }
}
