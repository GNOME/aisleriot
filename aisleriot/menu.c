/* AisleRiot - menu.c
 * Copyright (C) 1998 Jonathan Blandford <jrb@alum.mit.edu>
 *
 * This game is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 */

#include <config.h>
#include <dirent.h>
#include "gnome.h"
#include "sol.h"
#include "menu.h"
#include "dialog.h"
#include "cscmi.h"
#include "draw.h"
#include "events.h"
#include "scroll-menu.h"

static GtkWidget *about = NULL;


void restart_game ()
{
  if (waiting_for_mouse_up()) return;
  new_game (NULL, &seed);
};

void random_seed ()
{
  if (waiting_for_mouse_up()) return;
  new_game (NULL, NULL);
};

void new_rules (GtkWidget* w, gchar* file) 
{
  if (waiting_for_mouse_up()) return;
  new_game (file, NULL);
};

void undo_callback ()
{
  if (waiting_for_mouse_up()) return;
  gh_eval_str ("(undo)");
  refresh_screen();
}

void redo_callback ()
{
  if (waiting_for_mouse_up()) return;
  gh_eval_str ("(redo)");
  refresh_screen();
}
static void
about_destroy_callback (void)
{
	about = NULL;
}

 
void help_about_callback ()
{
  const gchar *authors[] = {
	  N_("Main program:  Jonathan Blandford (jrb@redhat.com)"),
	  N_("                      Felix Bellaby (felix@pooh.u-net.com)"),
	  N_("                      Rosanna Yuen (zana@webwynk.net)"),
	  N_("Card Games:    Jonathan Blandford (jrb@redhat.com)"),
	  N_("                      W. Borgert (debacle@debian.org)"),
	  N_("                      Robert Brady (rwb197@ecs.soton.ac.uk)"),
	  N_("                      Nick Lamb (njl195@zepler.org.uk)"),
	  N_("                      Changwoo Ryu (cwryu@adam.kaist.ac.kr)"),
          N_("                      Matthew Wilcox (matthew@wil.cx)"),
	  N_("                      Rosanna Yuen (zana@webwynk.net)"),
	  NULL
  };

#ifdef ENABLE_NLS
  {
	int i=0;
	while (authors[i] != NULL) { authors[i]=_(authors[i]); i++; }
  }
#endif

  if (about) {
    gdk_window_raise (about->window);
    return;
  }
  about = gnome_about_new ( _("AisleRiot"), VERSION,
			    /* copyright notice */
			    _("(C) 1998 Jonathan Blandford (jrb@redhat.com)"),
			    (const char **)authors,
			    /* another comments */
			    _("AisleRiot provides a rule-based "
			      "solitaire card engine that allows many different games to be played"),
			    NULL);
  gtk_signal_connect (GTK_OBJECT (about),
		      "destroy",
		      (GtkSignalFunc) about_destroy_callback,
		      NULL);
  gtk_widget_show (about);
  return;
}

GnomeUIInfo rules_sub_menu[] = {
  GNOMEUIINFO_END
};

GnomeUIInfo file_menu[] = {
  GNOMEUIINFO_END
};

GnomeUIInfo settings_menu[] = {

#if 0
  GNOMEUIINFO_ITEM_STOCK(N_("Game _options..."),
			 N_("Modify the options for this game"),
			 show_rules_options_dialog, GNOME_STOCK_MENU_PREF),

  GNOMEUIINFO_SEPARATOR,
#endif
  GNOMEUIINFO_MENU_PREFERENCES_ITEM(show_preferences_dialog, NULL),

  GNOMEUIINFO_END
};

GnomeUIInfo help_menu[] = {
  GNOMEUIINFO_HELP("aisleriot"),

  GNOMEUIINFO_MENU_ABOUT_ITEM(help_about_callback, NULL),

  GNOMEUIINFO_END
};

GnomeUIInfo game_menu[] = {

  GNOMEUIINFO_MENU_NEW_GAME_ITEM(random_seed, NULL),

  GNOMEUIINFO_ITEM (N_("New _game of..."), N_("Start a new game of a different variation"), NULL, NULL),

  GNOMEUIINFO_MENU_RESTART_GAME_ITEM(restart_game, NULL),

  GNOMEUIINFO_ITEM_STOCK(N_("_Select..."), N_("Select a new game variation"),
			 show_select_game_dialog, GNOME_STOCK_MENU_OPEN),

  GNOMEUIINFO_SEPARATOR,

  GNOMEUIINFO_MENU_UNDO_MOVE_ITEM(undo_callback, NULL),

  GNOMEUIINFO_MENU_REDO_MOVE_ITEM(redo_callback, NULL),

  GNOMEUIINFO_MENU_HINT_ITEM(show_hint_dialog, NULL),
  /*  GNOMEUIINFO_ITEM_STOCK(N_("_Properties..."), NULL, 
      show_property_dialog, GNOME_STOCK_MENU_PREF), */

  GNOMEUIINFO_SEPARATOR,
#if 0
  GNOMEUIINFO_MENU_SCORES_ITEM(show_global_stats_dialog, NULL),
  /*  GNOMEUIINFO_ITEM_STOCK(N_("S_tatistics..."), NULL, 
      show_global_stats_dialog, GNOME_STOCK_MENU_BOOK_BLUE), */

  GNOMEUIINFO_SEPARATOR,
#endif
  GNOMEUIINFO_MENU_EXIT_ITEM(quit_app, NULL),

  GNOMEUIINFO_END
};


GnomeUIInfo top_menu[] = {

  GNOMEUIINFO_MENU_GAME_TREE(game_menu),

  GNOMEUIINFO_MENU_SETTINGS_TREE(settings_menu),

  GNOMEUIINFO_MENU_HELP_TREE(help_menu),

  GNOMEUIINFO_END
};

GnomeUIInfo toolbar[] =
{
  GNOMEUIINFO_ITEM_STOCK(N_("New"), N_("Deal a new game"),
			 random_seed, GNOME_STOCK_PIXMAP_NEW),

  GNOMEUIINFO_ITEM_STOCK(N_("Restart"), N_("Start this game over"),
			 restart_game, GNOME_STOCK_PIXMAP_REFRESH),

  GNOMEUIINFO_ITEM_STOCK(N_("Select"), N_("Select a new game"),
			 show_select_game_dialog, GNOME_STOCK_PIXMAP_OPEN),

  GNOMEUIINFO_SEPARATOR,

  GNOMEUIINFO_ITEM_STOCK(N_("Hint"), N_("Suggest a move"),
			 show_hint_dialog, GNOME_STOCK_PIXMAP_JUMP_TO),

  GNOMEUIINFO_ITEM_STOCK(N_("Undo"), N_("Undo the last move"),
			 undo_callback, GNOME_STOCK_PIXMAP_UNDO),

  GNOMEUIINFO_ITEM_STOCK(N_("Redo"), N_("Redo the last move"),
			 redo_callback, GNOME_STOCK_PIXMAP_REDO),

  GNOMEUIINFO_SEPARATOR,

  GNOMEUIINFO_ITEM_STOCK(N_("Exit"), N_("Quit Aisleriot"),
			 quit_app, GNOME_STOCK_PIXMAP_EXIT),
  GNOMEUIINFO_END
};

void create_menus ()
{
  int i;
  GtkWidget *w;
  GtkWidget *menu_item;
  GtkWidget *menu;
  gnome_app_create_menus (GNOME_APP(app), top_menu);
  gnome_app_create_toolbar (GNOME_APP(app), toolbar);

  /* Kids, don't try this at home */
  menu_item = game_menu [1].widget;
  menu = scroll_menu_new ();
  gtk_menu_item_set_submenu (GTK_MENU_ITEM (menu_item), menu);
  w = gtk_tearoff_menu_item_new ();
  gtk_widget_show(w);
  gtk_menu_shell_append (GTK_MENU_SHELL(menu), w);

  for(i = 0; i < n_games; i++) {
    w = gtk_menu_item_new_with_label 
      (game_file_to_name (game_dents[i]->d_name));
    gtk_widget_show(w);
    gtk_menu_shell_append (GTK_MENU_SHELL(menu), w);
    gtk_signal_connect (GTK_OBJECT(w), "activate", 
			(GtkSignalFunc) new_rules,
			(gpointer) game_dents[i]->d_name);
  }
  gtk_widget_show_all (menu);
}

void install_menu_hints (GnomeApp *app)
{
  gnome_app_install_menu_hints(GNOME_APP (app), top_menu);
}

