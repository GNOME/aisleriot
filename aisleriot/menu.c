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
GtkActionGroup *action_group;

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

static void fullscreen_toggle_callback(GtkToggleAction * togglebutton, 
				       gpointer data)
{
  gboolean state;

  state = gtk_toggle_action_get_active (togglebutton);

  if (state) {
    gtk_window_fullscreen (GTK_WINDOW (app));
  } else {
    gtk_window_unfullscreen (GTK_WINDOW (app));
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
  { "OptionsMenu", NULL, "Options" /* not translated on purpose */ },
  { "HelpMenu", NULL, N_("_Help") },

  { "NewGame", GTK_STOCK_NEW, N_("_New Game"), "<control>N", NULL, G_CALLBACK (random_seed) },
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
  { "About", GTK_STOCK_ABOUT, NULL, NULL, NULL, G_CALLBACK (help_about_callback) },

  /*  { "Hidden", GTK_STOCK_NEW, "This should be hidden", NULL, NULL, NULL }, */
};

const GtkToggleActionEntry toggles[] = {
  { "Toolbar", NULL, N_("_Toolbar"), NULL, NULL, G_CALLBACK (toolbar_toggle_callback) },
  { "Fullscreen", NULL, N_("_Fullscreen"), "F11", NULL, G_CALLBACK (fullscreen_toggle_callback) },
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
"      <menuitem action='Fullscreen'/>"
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
/* "    <toolitem action='Hidden'/>" */
"  </toolbar>"
"</ui>";

void create_menus ()
{
  GtkAccelGroup *accel_group;

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

static gchar * make_option_gconf_key (void)
{
  static gchar *basekey = "/apps/aisleriot/rules/";
  gchar *name, *s, *r, *sk;
  GConfSchema *schema;
  GConfValue *def;

  name = g_strdup (gamename);
  s = name;
  while (*s) {
    if (*s == ' ')
      *s = '_';
    s++;
  }
    
  r = g_strconcat (basekey, name, NULL);
  g_free(name);

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
  l = SCM_INUM (scm_length (options_list));
  for (i=0; i<l; i++) {
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
  GtkAction *menuaction;
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
    g_object_set (G_OBJECT (menuaction), "label", _(name), NULL);

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
