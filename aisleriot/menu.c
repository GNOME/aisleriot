/* AisleRiot - menu.c
 * Copyright (C) 1998, 2003 Jonathan Blandford <jrb@alum.mit.edu>
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
  GdkPixbuf *pixbuf = NULL;
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

  const gchar *documenters[] = {
	  NULL
  };

  const gchar *translator_credits = _("translator_credits");
	char *filename = NULL;

	filename = gnome_program_locate_file (NULL,
		GNOME_FILE_DOMAIN_APP_PIXMAP,  ("gnome-aisleriot.png"),
		TRUE, NULL);
	if (filename != NULL)
	{
		pixbuf = gdk_pixbuf_new_from_file(filename, NULL);
		g_free (filename);
	}
  
  {
	int i=0;
	while (authors[i] != NULL) { authors[i]=_(authors[i]); i++; }
  }

  if (about) {
    gtk_window_present (GTK_WINDOW (about));
    return;
  }
  about = gnome_about_new ( _("AisleRiot"), VERSION,
			    /* copyright notice */
			    _("(C) 1998 Jonathan Blandford (jrb@redhat.com)"),
			    _("AisleRiot provides a rule-based "
			    "solitaire card engine that allows many different games to be played"),
			    (const char **)authors,
			    (const char **)documenters,
			    strcmp (translator_credits, "translator_credits") != 0 ? translator_credits : NULL,
			    pixbuf);
	
	if (pixbuf != NULL)
		gdk_pixbuf_unref (pixbuf);
        gtk_window_set_transient_for (GTK_WINDOW (about), GTK_WINDOW (app));
  g_signal_connect (GTK_OBJECT (about),
		      "destroy",
		      (GtkSignalFunc) about_destroy_callback,
		      NULL);
  gtk_widget_show (about);
  return;
}

void toolbar_show (void)
{
  GtkWidget * toolbar;

  toolbar = GTK_WIDGET(gnome_app_get_dock_item_by_name (GNOME_APP(app),
                                                        GNOME_APP_TOOLBAR_NAME));
  gtk_widget_show (toolbar);
}

void toolbar_hide (void)
{
  GtkWidget * toolbar;

  toolbar = GTK_WIDGET(gnome_app_get_dock_item_by_name (GNOME_APP(app),
                                                        GNOME_APP_TOOLBAR_NAME));
  gtk_widget_hide (toolbar);
}

void toolbar_toggle_callback(GtkWidget * togglebutton, gpointer data)
{
  gboolean state;
  
  state = gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM (togglebutton));

  if (state) {
    toolbar_show();
    gconf_client_set_bool (gconf_client, "/apps/aisleriot/show_toolbar", TRUE,
                           NULL);
  } else {
    toolbar_hide();
    gconf_client_set_bool (gconf_client, "/apps/aisleriot/show_toolbar", FALSE,
                           NULL);
  }
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

  GNOMEUIINFO_TOGGLEITEM (N_("_Toolbar"), N_("Show or hide the toolbar"),
                          toolbar_toggle_callback, NULL),

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

  GNOMEUIINFO_MENU_RESTART_GAME_ITEM(restart_game, NULL),

  GNOMEUIINFO_ITEM_STOCK (N_("_Select..."), N_("Select a different game"), show_select_game_dialog, GTK_STOCK_OPEN),

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
  GNOMEUIINFO_ITEM_STOCK(N_("New"), N_("Start a new game"),
			 random_seed, GTK_STOCK_NEW),

  GNOMEUIINFO_ITEM_STOCK(N_("Restart"), N_("Restart the game"),
			 restart_game, GTK_STOCK_REFRESH),

  GNOMEUIINFO_ITEM_STOCK(N_("Select"), N_("Select a different game"),
			 show_select_game_dialog, GTK_STOCK_OPEN),

  GNOMEUIINFO_SEPARATOR,

  GNOMEUIINFO_ITEM_STOCK(N_("Undo"), N_("Undo the last move"),
			 undo_callback, GTK_STOCK_UNDO),

  GNOMEUIINFO_ITEM_STOCK(N_("Redo"), N_("Redo the undone move"),
			 redo_callback, GTK_STOCK_REDO),

  GNOMEUIINFO_ITEM_STOCK(N_("Hint"), N_("Get a hint for your next move"),
			 show_hint_dialog, GTK_STOCK_HELP),

  GNOMEUIINFO_END
};

void undo_set_sensitive (gboolean state)
{
  gtk_widget_set_sensitive (game_menu[4].widget, state);
  gtk_widget_set_sensitive (toolbar[4].widget, state);
}

void redo_set_sensitive (gboolean state)
{
  gtk_widget_set_sensitive (game_menu[5].widget, state);
  gtk_widget_set_sensitive (toolbar[5].widget, state);
}

void create_menus ()
{
  gnome_app_create_menus (GNOME_APP(app), top_menu);
  gnome_app_create_toolbar (GNOME_APP(app), toolbar);

  gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (settings_menu[0].widget),
                                  gconf_client_get_bool (gconf_client,
                                                         "/apps/aisleriot/show_toolbar",
                                                         NULL));
}

void install_menu_hints (GnomeApp *app)
{
  gnome_app_install_menu_hints(GNOME_APP (app), top_menu);
}

