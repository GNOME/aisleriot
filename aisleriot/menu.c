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

#include <games-stock.h>

#include "sol.h"
#include "menu.h"
#include "dialog.h"
#include "cscmi.h"
#include "draw.h"
#include "events.h"
#include "statistics.h"

GtkUIManager *ui_manager;
GtkActionGroup *action_group;

/* Cached items which we alter the state of frequently. */
GtkAction *undoaction;
GtkAction *redoaction;
GtkAction *restartaction;
GtkAction *fullscreenaction;
GtkAction *leavefullscreenaction;
GtkAction *menuaction = NULL;
GtkWidget *helpitem;
GtkWidget *menubar;
GtkWidget *toolbar;

static gchar * gamename = NULL;
static gchar * ugamename = NULL;

static gchar *convert_name_to_underscored (const gchar *inname)
{
  gchar *outname;
  gchar *s;

  outname = g_strdup (inname);

  s = outname;
  while (*s) {
    if (*s == ' ')
      *s = '_';
    s++;
  }

  return outname;
}

static void restart_game ()
{
  if (waiting_for_mouse_up()) return;
  /* Treat a restart as part of the same game. Eventually either
   * the player will win or loose and then it gets counted. */
  game_in_progress = FALSE;
  option_list_set_sensitive ();
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

static void help_about_callback (void)
{
  const gchar *authors[] = {
	  _("Main game:"),
	  "Jonathan Blandford (jrb@redhat.com)",
	  "Felix Bellaby (felix@pooh.u-net.com)",
	  "Rosanna Yuen (zana@webwynk.net)",
	  "Callum McKenzie (callum@physics.otago.ac.nz)",
	  "",
	  _("Card games:"),
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
	  "Rosanna Yuen (zana@webwynk.net)",
	  NULL
  };

  gtk_show_about_dialog (GTK_WINDOW (app),
			 "name", _("AisleRiot"),
			 "version", VERSION,
			 "comments", _("AisleRiot provides a rule-based solitaire\n"
				       "card engine that allows many different\n"
				       "games to be played."),
			 "copyright", "Copyright \xc2\xa9 1998-2006 Jonathan Blandford",
			 "license", "GPL 2+",
			 "authors", authors,
			 "documenters", documenters,
			 "translator_credits", _("translator-credits"),
			 "logo-icon-name", "gnome-aisleriot.png",
			 "website", "http://www.gnome.org/projects/gnome-games/",
			 "wrap-license", TRUE,
			 NULL);
}

static void general_help (void)
{
  gnome_help_display ("aisleriot.xml", NULL, NULL);
}

static void help_on_specific_game (void)
{
  gnome_help_display ("aisleriot.xml", ugamename, NULL);
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

static void set_fullscreen_actions (gboolean is_fullscreen)
{
  if (is_fullscreen) {
    gtk_widget_hide (menubar);
  } else {
    gtk_widget_show (menubar);
  }

  gtk_action_set_sensitive (leavefullscreenaction, is_fullscreen);
  gtk_action_set_visible (leavefullscreenaction, is_fullscreen);

  gtk_action_set_sensitive (fullscreenaction, !is_fullscreen);
  gtk_action_set_visible (fullscreenaction, !is_fullscreen);
}

static void
fullscreen_callback (GtkAction *action)
{
  if (action == fullscreenaction)
    gtk_window_fullscreen (GTK_WINDOW (app));
  else
    gtk_window_unfullscreen (GTK_WINDOW (app));
}

static void window_state_callback (GtkWidget *widget, 
				   GdkEventWindowState *event)
{
  if (!(event->changed_mask & GDK_WINDOW_STATE_FULLSCREEN))
    return;

  set_fullscreen_actions (event->new_window_state &
			  GDK_WINDOW_STATE_FULLSCREEN);
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
  gtk_action_set_sensitive (undoaction, state);

  /* The restart game validity condition is the same as for undo. */
  gtk_action_set_sensitive (restartaction, state);
}

void redo_set_sensitive (gboolean state)
{
  gtk_action_set_sensitive (redoaction, state);
}

static const GtkActionEntry actions[] = {
  { "GameMenu", NULL, N_("_Game") },
  { "ViewMenu", NULL, N_("_View") },
  { "ControlMenu", NULL, N_("_Control") },
  { "OptionsMenu", NULL, "Options" /* not translated on purpose */ },
  { "HelpMenu", NULL, N_("_Help") },

  { "NewGame", GAMES_STOCK_NEW_GAME, NULL, NULL, NULL, G_CALLBACK (random_seed) },
  { "RestartGame", GAMES_STOCK_RESTART_GAME, NULL, NULL, NULL, G_CALLBACK (restart_game) },
  { "Select", GTK_STOCK_INDEX, N_("_Select Game..."), "<Ctrl>o", N_("Play a different game"), G_CALLBACK (show_select_game_dialog) },
  { "Statistics", GTK_STOCK_ADD, N_("S_tatistics"), NULL, N_("Show gameplay statistics"), G_CALLBACK (show_statistics_dialog) },
  { "Quit", GTK_STOCK_QUIT, NULL, NULL, NULL, G_CALLBACK (quit_app) },
  { "Fullscreen", GAMES_STOCK_FULLSCREEN, NULL, NULL, NULL, G_CALLBACK (fullscreen_callback) },
  { "LeaveFullscreen", GAMES_STOCK_LEAVE_FULLSCREEN, NULL, NULL, NULL, G_CALLBACK (fullscreen_callback) },
  { "Cards", NULL, N_("_Cards..."), NULL, NULL, G_CALLBACK (show_preferences_dialog) },

  { "UndoMove", GAMES_STOCK_UNDO_MOVE, NULL, NULL, NULL, G_CALLBACK (undo_callback) },
  { "RedoMove", GAMES_STOCK_REDO_MOVE, NULL, NULL, NULL, G_CALLBACK (redo_callback) },
  { "Hint", GAMES_STOCK_HINT, NULL, NULL, NULL, G_CALLBACK (show_hint_dialog) },
  { "Contents", GAMES_STOCK_CONTENTS, NULL, NULL, N_("View help for Aisleriot"), G_CALLBACK (general_help) },
  { "Help", GTK_STOCK_HELP, NULL, "<Shift>F1", N_("View help for this game"), G_CALLBACK (help_on_specific_game) },
  { "About", GTK_STOCK_ABOUT, NULL, NULL, NULL, G_CALLBACK (help_about_callback) },
};

static const GtkToggleActionEntry toggles[] = {
  { "Toolbar", NULL, N_("_Toolbar"), NULL, "Show or hide the toolbar", G_CALLBACK (toolbar_toggle_callback) },
  { "ClickToMove", NULL, N_("_Click to Move"), NULL, "Pick up and drop cards by clicking", G_CALLBACK (clickmove_toggle_callback) },
};

static const char ui_description[] = 
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
"      <menuitem action='Fullscreen'/>"
"      <menuitem action='LeaveFullscreen'/>"
"      <menuitem action='Toolbar'/>"
"      <separator/>"
"      <menuitem action='Cards'/>"
"    </menu>"
"    <menu action='ControlMenu'>"
"      <menuitem action='ClickToMove'/>"
"      <separator/>"
"      <menuitem action='UndoMove'/>"
"      <menuitem action='RedoMove'/>"
"      <menuitem action='Hint'/>"
"    </menu>"
"    <menu action='OptionsMenu'/>"
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
"    <toolitem action='LeaveFullscreen'/>"
"  </toolbar>"
"</ui>";

void create_menus ()
{
  GtkAccelGroup *accel_group;
  GtkAction *ctmtoggle;
  GtkAction *toolbartoggle;

  games_stock_init();
  action_group = gtk_action_group_new ("MenuActions");
  gtk_action_group_set_translation_domain (action_group, GETTEXT_PACKAGE);
  gtk_action_group_add_actions (action_group, actions, G_N_ELEMENTS (actions),
				NULL); 
  gtk_action_group_add_toggle_actions (action_group, toggles, 
				       G_N_ELEMENTS (toggles), NULL);

  ui_manager = gtk_ui_manager_new ();
  games_stock_prepare_for_statusbar_tooltips (ui_manager, statusbar);

  gtk_ui_manager_insert_action_group (ui_manager, action_group, 1); 
  gtk_ui_manager_add_ui_from_string (ui_manager, ui_description, -1, NULL);

  accel_group = gtk_ui_manager_get_accel_group (ui_manager);
  gtk_window_add_accel_group (GTK_WINDOW (app), accel_group);

  undoaction = gtk_action_group_get_action (action_group, "UndoMove");
  redoaction = gtk_action_group_get_action (action_group, "RedoMove");
  restartaction = gtk_action_group_get_action (action_group, "RestartGame");
  fullscreenaction = gtk_action_group_get_action (action_group, "Fullscreen");
  leavefullscreenaction = gtk_action_group_get_action (action_group, "LeaveFullscreen");
  ctmtoggle = gtk_action_group_get_action (action_group, "ClickToMove");
  toolbartoggle = gtk_action_group_get_action (action_group, "Toolbar");

  helpitem = gtk_ui_manager_get_widget (ui_manager, "/MainMenu/HelpMenu/Help");

  menubar = gtk_ui_manager_get_widget (ui_manager, "/MainMenu");
  toolbar = gtk_ui_manager_get_widget (ui_manager, "/Toolbar");
  
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (toolbartoggle),
				gconf_client_get_bool (gconf_client,
						       "/apps/aisleriot/show_toolbar",
						       NULL));
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (ctmtoggle),
				gconf_client_get_bool (gconf_client,
						       "/apps/aisleriot/click_to_move",
						       NULL));

  g_signal_connect (G_OBJECT (app), "window_state_event",
		    G_CALLBACK (window_state_callback), NULL);
  set_fullscreen_actions (FALSE);
}

void help_update_game_name (gchar * name)
{

  if (gamename)
    g_free (gamename);

  if (ugamename)
    g_free (ugamename);

  gamename = g_strdup (name);
  ugamename = convert_name_to_underscored (name);

  gtk_label_set_text (GTK_LABEL (GTK_BIN (helpitem)->child), gamename);

}

static gchar * make_option_gconf_key (void)
{
  static gchar *basekey = "/apps/aisleriot/rules/";
  gchar *r, *sk;
  GConfSchema *schema;
  GConfValue *def;
    
  r = g_strconcat (basekey, ugamename, NULL);

  sk = g_strconcat ("/schemas", r, NULL);

  /* Check if we have a schema for this key and make one if we don't. */
  schema = gconf_client_get_schema (gconf_client, sk, NULL);
  if (schema == NULL) {
    schema = gconf_schema_new ();
    gconf_schema_set_type (schema, GCONF_VALUE_INT);
    gconf_schema_set_owner (schema, "aisleriot");
    /* FIXME: Translation - how? */
    gconf_schema_set_short_desc (schema, "A per-game option");
    gconf_schema_set_long_desc (schema, "An integer encoding a list of boolean values (LSB = first item) for use as options in an solitaire game.");
    def = gconf_value_new (GCONF_VALUE_INT);
    gconf_value_set_int (def, 0);
    gconf_schema_set_default_value (schema, def);
    gconf_value_free (def);
    gconf_client_set_schema (gconf_client, sk, schema, NULL);
    gconf_engine_associate_schema (gconf_engine_get_default (), r, sk, NULL);
  } 
  gconf_schema_free (schema);
  g_free (sk);

  return r;
}

/* Make something that gconf can easily deal with. */
static gint compress_options_to_int (SCM options_list)
{
  gint l, i;
  guint bits;
  SCM entry;

  bits = 0;
  l = SCM_INUM (scm_length (options_list)) - 1;
  for (i=l; i>=0; i--) {
    entry  = scm_list_ref (options_list, SCM_MAKINUM (i));
    bits <<= 1;
    bits |= SCM_NFALSEP (scm_list_ref (entry, SCM_MAKINUM (1))) ? 1 : 0;
  }

  return bits;
}

/* Take the gconf bit-string value and set the option list from it. */
static void expand_options_from_int (SCM options_list, guint bits)
{
  gint l, i;
  SCM entry;

  l = SCM_INUM (scm_length (options_list));
  for (i=0; i<l; i++) {
    entry  = scm_list_ref (options_list, SCM_MAKINUM (i));
    scm_list_set_x (entry, SCM_MAKINUM (1), bits & 1 ? SCM_BOOL_T : SCM_BOOL_F);
    bits >>= 1;
  }
}

/* FIXME: There isn't a schema for these automatically generated keys.
 * Maybe we should automatically generate those too. */

void option_list_set_sensitive (void)
{
  if (menuaction == NULL)
    return;
  
  gtk_action_set_sensitive (menuaction, !game_in_progress);
}

static void save_option_list (SCM options_list)
{
  gchar *keyname;

  keyname = make_option_gconf_key ();

  gconf_client_set_int (gconf_client, keyname, 
			compress_options_to_int (options_list), 
			NULL);
			
  g_free (keyname);
}

static void load_option_list (SCM options_list)
{
  gchar *keyname;
  guint r;

  keyname = make_option_gconf_key ();

  r = gconf_client_get_int (gconf_client, keyname, 
			    NULL);
			
  g_free (keyname);

  expand_options_from_int (options_list, r);
}

static void option_cb (GtkToggleAction *action, gint n)
{
  SCM options_list;
  SCM entry;
  gboolean statec;

  options_list = cscmi_get_options_lambda ();

  entry = scm_list_ref (options_list, SCM_MAKINUM (n));

  statec = gtk_toggle_action_get_active (action);

  scm_list_set_x (entry, SCM_MAKINUM(1), statec ? SCM_BOOL_T : SCM_BOOL_F);

  save_option_list (options_list);
  cscmi_apply_options_lambda (options_list);
}

/* FIXME: This does not add an appropriate underscore. Is this easy? 
 * Maybe underscore the first letter that isn't underscored in another
 * menu. */
void install_options_menu (gchar *name)
{
  static int merge_id = 0;
  static GtkActionGroup *options_group = NULL;
  SCM options_list;
  gint l, i;

  if (merge_id) {
    gtk_ui_manager_remove_ui (ui_manager, merge_id);
    merge_id = 0;
  }

  if (options_group) {
    gtk_ui_manager_remove_action_group (ui_manager, options_group);
    options_group = NULL;
  }

  if (has_options ()) {
    menuaction = gtk_action_group_get_action (action_group, "OptionsMenu");
    g_object_set (G_OBJECT (menuaction), "label", name, NULL);

    options_group = gtk_action_group_new ("OptionsActions");
    gtk_ui_manager_insert_action_group (ui_manager, options_group, -1);
    g_object_unref (options_group);

    merge_id = gtk_ui_manager_new_merge_id (ui_manager);
    
    options_list = cscmi_get_options_lambda ();
    load_option_list (options_list);
    l = SCM_INUM (scm_length (options_list));
    /* FIXME: We still leak a little here, especially actions. */
    for (i=0; i<l; i++) {
      SCM entry;
      gchar *entryname;
      gboolean entrystate;
      GtkToggleAction *itemaction;
      gchar actionname[20];

      /* Each entry in the options list is a list consisting of a name and 
	 a variable. */
      entry = scm_list_ref (options_list, SCM_MAKINUM (i));
      entryname = SCM_STRING_CHARS (scm_list_ref (entry, SCM_MAKINUM (0)));
      entrystate = SCM_NFALSEP (scm_list_ref (entry, SCM_MAKINUM (1)));

      g_snprintf (actionname, sizeof (actionname), "Option%d", i);

      itemaction = gtk_toggle_action_new (actionname, entryname, 
					  NULL, NULL);
      g_signal_connect (G_OBJECT (itemaction), "toggled",
			G_CALLBACK (option_cb), GINT_TO_POINTER (i));
      gtk_action_group_add_action (action_group, GTK_ACTION (itemaction));
      gtk_toggle_action_set_active (itemaction, entrystate);
      g_object_unref (itemaction);

      gtk_ui_manager_add_ui (ui_manager, merge_id, "/MainMenu/OptionsMenu",
			     actionname, actionname,
			     GTK_UI_MANAGER_MENUITEM, FALSE);
    }
  } 
}
