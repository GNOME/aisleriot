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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <gnome.h>

#include "sol.h"
#include "menu.h"
#include "dialog.h"
#include "cscmi.h"
#include "draw.h"
#include "events.h"
#include "statistics.h"

GtkUIManager *ui_manager;

/* Cached widgets which we alter the state of frequently. */
GtkWidget *undomenuitem;
GtkWidget *redomenuitem;
GtkWidget *restartmenuitem;
GtkWidget *undotoolbaritem;
GtkWidget *redotoolbaritem;
GtkWidget *restarttoolbaritem;
GtkWidget *ctmtoggle;
GtkWidget *toolbartoggle;
GtkWidget *helpitem;
GtkWidget *menubar;
GtkWidget *toolbar;

static GtkWidget *about = NULL;
static gchar * gamename = NULL;

static void restart_game ()
{
  if (waiting_for_mouse_up()) return;
  /* Treat a restart as part of the same game. Eventually either
   * the player will win or loose and then it gets counted. */
  game_in_progress = FALSE;
  new_game (NULL, &seed);
};

void random_seed ()
{
  if (waiting_for_mouse_up()) return;
  new_game (NULL, NULL);
};

void undo_callback ()
{
  if (waiting_for_mouse_up()) return;
  scm_c_eval_string ("(undo)");
  refresh_screen();
}

void redo_callback ()
{
  if (waiting_for_mouse_up()) return;
  scm_c_eval_string ("(redo)");
  refresh_screen();
  /* We need this now that you can undo a losing move. */
  end_of_game_test ();
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
	  N_("Main game:"),
	  "Jonathan Blandford (jrb@redhat.com)",
	  "Felix Bellaby (felix@pooh.u-net.com)",
	  "Rosanna Yuen (zana@webwynk.net)",
	  "Callum McKenzie (callum@physics.otago.ac.nz)",
	  "",
	  N_("Card games:"),
	  "Jonathan Blandford (jrb@redhat.com)",
	  "W. Borgert (debacle@debian.org)",
	  "Robert Brady (rwb197@ecs.soton.ac.uk)",
	  "Nick Lamb (njl195@zepler.org.uk)",
	  "Changwoo Ryu (cwryu@adam.kaist.ac.kr)",
	  "Matthew Wilcox (matthew@wil.cx)",
	  "Rosanna Yuen (zana@webwynk.net)",
	  NULL
  };

  const gchar *documenters[] = {
	  NULL
  };

  const gchar *translator_credits = _("translator-credits");
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
			    "Copyright \xc2\xa9 1998-2004 Jonathan Blandford",
			    _("AisleRiot provides a rule-based solitaire card "
			      "engine that allows many different games to be "
			      "played."),
			    (const char **)authors,
			    (const char **)documenters,
			    g_str_equal (translator_credits, "translator-credits") ? NULL : translator_credits,
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

static void general_help (void)
{
  gnome_help_display ("aisleriot.xml", NULL, NULL);
}

static void help_on_specific_game (void)
{
  gnome_help_display ("aisleriot.xml", gamename, NULL);
}

static void toolbar_toggle_callback(GtkToggleAction * togglebutton, 
				    gpointer data)
{
  gboolean state;
  
  state = gtk_toggle_action_get_active (togglebutton);

  if (state) {
    gtk_widget_show (toolbar);
    gconf_client_set_bool (gconf_client, "/apps/aisleriot/show_toolbar", TRUE,
                           NULL);
  } else {
    gtk_widget_hide (toolbar);
    gconf_client_set_bool (gconf_client, "/apps/aisleriot/show_toolbar", FALSE,
                           NULL);
  }
}

static void clickmove_toggle_callback(GtkToggleAction * 
				      togglebutton, gpointer data)
{
  click_to_move = gtk_toggle_action_get_active (togglebutton);

  gconf_client_set_bool (gconf_client, "/apps/aisleriot/click_to_move", 
			 click_to_move, NULL);
}

void undo_set_sensitive (gboolean state)
{
  gtk_widget_set_sensitive (undomenuitem, state);
  gtk_widget_set_sensitive (undotoolbaritem, state); 

  /* The restart game validity condition is the same as for undo. */
  gtk_widget_set_sensitive (restartmenuitem, state);
  gtk_widget_set_sensitive (restarttoolbaritem, state);
}

void redo_set_sensitive (gboolean state)
{
  gtk_widget_set_sensitive (redomenuitem, state);
  gtk_widget_set_sensitive (redotoolbaritem, state);
}

const GtkActionEntry actions[] = {
  { "GameMenu", NULL, N_("_Game") },
  { "ViewMenu", NULL, N_("_View") },
  { "ControlMenu", NULL, N_("_Control") },
  { "HelpMenu", NULL, N_("_Help") },

  { "NewGame", GTK_STOCK_MEDIA_PLAY, N_("_New Game"), "<control>N", NULL, G_CALLBACK (random_seed) },
  { "RestartGame", GTK_STOCK_REFRESH, N_("_Restart Game"), "<control>R", NULL, G_CALLBACK (restart_game) },
  { "Select", GTK_STOCK_INDEX, N_("_Select Game..."), NULL, NULL, G_CALLBACK (show_select_game_dialog) },
  { "Statistics", GTK_STOCK_ADD, N_("S_tatistics..."), NULL, NULL, G_CALLBACK (show_statistics_dialog) },
  { "Quit", GTK_STOCK_QUIT, NULL, NULL, NULL, G_CALLBACK (quit_app) },

  { "Cards", NULL, N_("_Cards..."), NULL, NULL, G_CALLBACK (show_preferences_dialog) },

  { "UndoMove", GTK_STOCK_UNDO, N_("_Undo Move"), "<control>Z", NULL, G_CALLBACK (undo_callback) },
  { "RedoMove", GTK_STOCK_REDO, N_("_Redo Move"), "<shift><control>Z", NULL, G_CALLBACK (redo_callback) },
  { "Hint", GTK_STOCK_HELP, N_("_Hint"), NULL, NULL, G_CALLBACK (show_hint_dialog) },
  
  
  { "Contents", GTK_STOCK_HELP, N_("_Contents"), "F1", NULL, G_CALLBACK (general_help) },
  { "Help", GTK_STOCK_HELP, "", "", NULL, G_CALLBACK (help_on_specific_game) },
  {"About", GTK_STOCK_ABOUT, NULL, NULL, NULL, G_CALLBACK (help_about_callback) }

};

const GtkToggleActionEntry toggles[] = {
  { "Toolbar", NULL, N_("_Toolbar"), NULL, NULL, G_CALLBACK (toolbar_toggle_callback) },
  { "ClickToMove", NULL, N_("_Click to Move"), NULL, NULL, G_CALLBACK (clickmove_toggle_callback) },
};

const char *ui_description = 
"<ui>"
"  <menubar name='MainMenu'>"
"    <menu action='GameMenu'>"
"      <menuitem action='NewGame'/>"
"      <menuitem action='RestartGame'/>"
"      <menuitem action='Select'/>"
"      <menuitem action='Statistics'/>"
"      <separator/>"
"      <menuitem action='Quit'/>"
"    </menu>"
"    <menu action='ViewMenu'>"
"      <menuitem action='Toolbar'/>"
"      <menuitem action='Cards'/>"
"    </menu>"
"    <menu action='ControlMenu'>"
"      <menuitem action='ClickToMove'/>"
"      <separator/>"
"      <menuitem action='UndoMove'/>"
"      <menuitem action='RedoMove'/>"
"      <menuitem action='Hint'/>"
"    </menu>"
"    <menu action='HelpMenu'>"
"      <menuitem action='Contents'/>"
"      <menuitem action='Help'/>"
"      <menuitem action='About'/>"
"    </menu>"
"  </menubar>"
"  <toolbar name='Toolbar'>"
"    <toolitem action='NewGame'/>"
"    <toolitem action='RestartGame'/>"
"    <toolitem action='Select'/>"
"    <separator/>"
"    <toolitem action='UndoMove'/>"
"    <toolitem action='RedoMove'/>"
"    <toolitem action='Hint'/>"
"  </toolbar>"
"</ui>";

void create_menus ()
{
  GtkAccelGroup *accel_group;
  GtkActionGroup *action_group;

  action_group = gtk_action_group_new ("MenuActions");
  gtk_action_group_set_translation_domain (action_group, GETTEXT_PACKAGE);
  gtk_action_group_add_actions (action_group, actions, G_N_ELEMENTS (actions),
				NULL); 
  gtk_action_group_add_toggle_actions (action_group, toggles, 
				       G_N_ELEMENTS (toggles), NULL);

  ui_manager = gtk_ui_manager_new ();
  gtk_ui_manager_insert_action_group (ui_manager, action_group, 1); 
  gtk_ui_manager_add_ui_from_string (ui_manager, ui_description, -1, NULL);

  accel_group = gtk_ui_manager_get_accel_group (ui_manager);
  gtk_window_add_accel_group (GTK_WINDOW (app), accel_group);

  undomenuitem = gtk_ui_manager_get_widget (ui_manager, "/MainMenu/ControlMenu/UndoMove");
  redomenuitem = gtk_ui_manager_get_widget (ui_manager, "/MainMenu/ControlMenu/RedoMove");
  restartmenuitem = gtk_ui_manager_get_widget (ui_manager, "/MainMenu/GameMenu/RestartGame");
  undotoolbaritem = gtk_ui_manager_get_widget (ui_manager, "/Toolbar/UndoMove");
  redotoolbaritem = gtk_ui_manager_get_widget (ui_manager, "/Toolbar/RedoMove");
  restarttoolbaritem = gtk_ui_manager_get_widget (ui_manager, "/Toolbar/RestartGame");
  helpitem = gtk_ui_manager_get_widget (ui_manager, "/MainMenu/HelpMenu/Help");
  ctmtoggle = gtk_ui_manager_get_widget (ui_manager, 
					 "/MainMenu/ControlMenu/ClickToMove");
  toolbartoggle = gtk_ui_manager_get_widget (ui_manager,
					     "/MainMenu/ViewMenu/Toolbar");

  menubar = gtk_ui_manager_get_widget (ui_manager, "/MainMenu");
  toolbar = gtk_ui_manager_get_widget (ui_manager, "/Toolbar");
  
  gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (toolbartoggle),
				gconf_client_get_bool (gconf_client,
						       "/apps/aisleriot/show_toolbar",
						       NULL));
  gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (ctmtoggle),
				gconf_client_get_bool (gconf_client,
						       "/apps/aisleriot/click_to_move",
						       NULL));
}

void help_update_game_name (gchar * name)
{

  if (gamename)
    g_free (gamename);

  gamename = g_strdup (name);

  gtk_label_set_text (GTK_LABEL (GTK_BIN (helpitem)->child), gamename);

}

