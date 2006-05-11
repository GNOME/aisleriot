/* AisleRiot - sol.c
 * Copyright (C) 1998, 2001, 2003 Jonathan Blandford <jrb@alum.mit.edu>
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

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include <libguile.h>
#include <ctype.h>
#include "sol.h"
#include "events.h"
#include "draw.h"
#include <gnome.h>
#include <gconf/gconf-client.h>
#include <glade/glade.h>
#include "slot.h"
#include "card.h"
#include "cscmi.h"
#include "menu.h"
#include "statistics.h"
#include <games-clock.h>
#include <games-gconf.h>
#include <games-files.h>

/* The minimum size for the playing area. Almost completely arbitrary. */
#define BOARD_MIN_WIDTH 300
#define BOARD_MIN_HEIGHT 250


/*
 * Global Variables
 */
GtkWidget        *app;
GtkWidget        *vbox;
GtkWidget        *playing_area;
GtkWidget        *statusbar;
GtkWidget        *score_box;
GdkGC            *draw_gc;
GdkGC            *bg_gc;
GdkGC            *slot_gc;
GdkPixmap        *surface;
GdkPixmap        *moving_card_pixmap;
gchar            *card_style;
gboolean         dont_save = FALSE; /* If the game is selected on the
                                     * command line we assume that it is
                                     * special and don't save the state.
                                     * This is essential for Freecell
                                     * emulation.*/

gboolean	 droppable_is_featured;
gboolean	 score_is_hidden; 

guint            score;
guint            timeout;
guint32          seed;
gchar            *game_file = "";
gchar            *game_name;
gboolean         game_in_progress = FALSE;
gboolean         game_over;
gboolean         game_won;
gboolean         click_to_move = FALSE;
gchar            *variation = "";
gchar            *gamesdir;

GConfClient      *gconf_client = NULL;

#define DEFAULT_VARIATION "klondike.scm"
#define GNOME_SESSION_BUG

gchar* game_file_to_name (const gchar* file)
{
  char* p, *buf = g_path_get_basename(file);
  gchar *ts;

  if ((p = strrchr (buf, '.'))) *p = '\0';
  for (p = buf; p = strchr(p, '_'), p && *p;) *p = ' ';
  for (p = buf; p = strchr(p, '-'), p && *p;) *p = ' ';
  for (p = buf; p = strchr(p, ' '), p && *p;) {
    if (*(p+1)) {
      *(p+1) = g_ascii_toupper (*(p+1));
      *p++;
    }
  }
  buf[0] = g_ascii_toupper (buf[0]);

  ts = g_strdup (_(buf));
  g_free (buf);

  return ts;
}

/* Note that this is not the inverse of game_file_to_name. This
 * only works on untranslated names. game_file_to_name only produces
 * translated names. Be careful. */
static gchar * game_name_to_file (const gchar *name)
{
  char *p, *s;

  s = g_strdup (name);
  p = s;
  while (*p) {
    if (*p == ' ')
      *p = '_';
    *p = g_ascii_tolower (*p);
    p++;
  }

  if (!g_str_has_suffix (s, ".scm")) { /* We may have been given a filename. */
    p = s;
    s = g_strconcat (s, ".scm", NULL);
    g_free (p);
  }

  p = g_build_filename (gamesdir, s, NULL);
  g_free (s);

  return p;
}

void eval_installed_file (char *file)
{
  char *installed_filename;
  char *relative;
  
  if (g_file_test (file, G_FILE_TEST_EXISTS)){
    scm_c_primitive_load (file);
    return;
  }
  
  relative = g_strconcat (GAMESDIR, file, NULL);
  installed_filename = gnome_program_locate_file (NULL, 
                                                  GNOME_FILE_DOMAIN_APP_DATADIR,
                                                  relative,
                                                  FALSE, NULL);

  if (g_file_test (installed_filename, G_FILE_TEST_EXISTS)){
    scm_c_primitive_load (installed_filename);
  } else {
    char *message = g_strdup_printf (
         _("Aisleriot can't load the file: \n%s\n\n"
           "Please check your Aisleriot installation"), installed_filename);
    GtkWidget *w = gtk_message_dialog_new (NULL, 0,
					   GTK_MESSAGE_ERROR,
					   GTK_BUTTONS_OK,
					   message);

    gtk_dialog_run (GTK_DIALOG(w));
    gtk_widget_destroy(w);
    g_free (message);
    exit (1);
  }
  g_free (installed_filename);
  g_free (relative);
}

static int
save_state (GnomeClient *client)
{
  gconf_client_set_string (gconf_client, "/apps/aisleriot/game_file",
                           game_file, NULL);
  gconf_client_set_string (gconf_client, "/apps/aisleriot/card_style",
                           card_style, NULL);

  return TRUE;
}

/* Check for the existence of a particular variation and fall back to
   a default if it isn't found. */
/* FIXME: There is a lot of duplication with the eval_installed_file
   function, but we need this called earlier. */
static void check_game_file_name (void)
{
  gchar *fullpath;
  gchar *partialpath;
  GtkWidget *dialog;

  partialpath = g_strconcat (GAMESDIR, game_file, NULL);
  fullpath = gnome_program_locate_file (NULL,
					GNOME_FILE_DOMAIN_APP_DATADIR,
					partialpath,
					FALSE, NULL);
  if (!g_file_test (fullpath, G_FILE_TEST_EXISTS)) {
    dialog = gtk_message_dialog_new (NULL, 0,
				     GTK_MESSAGE_INFO,
				     GTK_BUTTONS_OK,
				     _("Aisleriot cannot find the last game you played."));
 gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog), _("This usually occurs when you run an older version of Aisleriot which does not have the game you last played. The default game, Klondike, is being started instead."));
    gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);
    game_file = DEFAULT_VARIATION;
  }

  g_free (fullpath);
  g_free (partialpath);
}

void new_game (gchar* file, guint *seedp)
{
  double width, height;

  /* If we're aborting an old game count it as a failure for
   * statistical purposes. */
  if (game_in_progress) {
    update_statistics (FALSE, 0);
  }
  /* The game isn't actually in progress until the user makes a move. */
  game_in_progress = FALSE;
  option_list_set_sensitive ();

  if (file && strcmp (file, game_file)) {
    game_file = file;
    
    /* Although this line slows down game switching by a noticeable amount, we
     * add it here in order to make sure all the original functions are
     * "clean". */
    eval_installed_file ("sol.scm");
    /* This is here so that the previous lines can catch bad installations 
       and report it before reporting the fallback to defaults. */
    check_game_file_name ();
    eval_installed_file (game_file);

    game_name = game_file_to_name (game_file);
    update_statistics_display ();
    help_update_game_name (game_name);
    install_options_menu (game_name);

    if (score_is_hidden)
      gtk_widget_hide (score_box);
    else
      gtk_widget_show (score_box);

    if (!dont_save)
      save_state (gnome_master_client ());

  }

  if (seedp) {
    seed = *seedp;
  }
  else {
    seed = g_random_int();
  }

  g_random_set_seed(seed);
  set_score(0);
  timer_reset ();

  cscmi_start_game_lambda(&width, &height);
  scm_c_eval_string ("(start-game)");

  set_geometry(width, height);

  /* It is possible for some games to not have any moves right from the
   * start. If this happens we redeal. */
  if (!cscmi_game_over_lambda()) {
    new_game (file, NULL);
  } else {
    if (surface)
      refresh_screen();
    
    undo_set_sensitive (FALSE);
    redo_set_sensitive (FALSE);

    game_over = FALSE;
    gtk_window_set_title (GTK_WINDOW (app), _(game_name)); 
  }
}

GtkWidget *score_value;

guint score;

void set_score(guint new_score) 
{
  char b [10];
  score = new_score;
  g_snprintf (b, sizeof (b), "%6d  ", score);
  gtk_label_set_text(GTK_LABEL(score_value), b);
}

guint get_score()
{
  return score;
}

GtkWidget *time_value;

static gint timer_cb ()
{
  timeout = 3600;
  /* All the games return #f and nothing else with this call, but just in
   * case someone changes their mind in the future. */
  if (cscmi_timeout_lambda ())
    end_of_game_test();
  return 0;	
}

guint timer_timeout = 0;

void timer_restart (void)
{
  games_clock_start (GAMES_CLOCK (time_value));
  timer_timeout = g_timeout_add (timeout*1000 - timer_get (), (GSourceFunc) (timer_cb), NULL);
}

void timer_start (void)
{
  if (timer_timeout)
    games_clock_stop (GAMES_CLOCK (time_value));
  timeout = 3600;
  games_clock_set_seconds (GAMES_CLOCK (time_value), 0);
  games_clock_start (GAMES_CLOCK (time_value));
  timer_timeout = g_timeout_add (timeout * 1000, (GSourceFunc) (timer_cb), NULL);
}

void timer_stop (void)
{
  games_clock_stop (GAMES_CLOCK (time_value));
  if (timer_timeout)
    g_source_remove (timer_timeout);
  timer_timeout = 0;
}

void timer_reset (void)
{
  timer_stop ();
  games_clock_set_seconds (GAMES_CLOCK (time_value), 0);
}

guint timer_get (void)
{
  return (guint) games_clock_get_seconds (GAMES_CLOCK (time_value));
}

/*
 * setup stuff
 */


static void create_sol_board ()
{
  playing_area = gtk_drawing_area_new ();
  gtk_widget_set_events (playing_area, 
			 gtk_widget_get_events (playing_area) | GAME_EVENTS);
  /* This only enforces the minimum size. It is actually set using the
   * window size. */
  gtk_widget_set_size_request (playing_area, BOARD_MIN_WIDTH, 
			       BOARD_MIN_HEIGHT);

  gtk_box_pack_start (GTK_BOX (vbox), playing_area, TRUE, TRUE, 0);
  
  gtk_widget_realize (playing_area);

  draw_gc = gdk_gc_new (playing_area->window);
  
  bg_gc = gdk_gc_new (playing_area->window);
  if (get_background_pixmap ())
    gdk_gc_set_tile (bg_gc, get_background_pixmap());
  gdk_gc_set_fill (bg_gc, GDK_TILED);  

  slot_gc = gdk_gc_new (playing_area->window);

  g_signal_connect (G_OBJECT(playing_area),"button_release_event",
		      GTK_SIGNAL_FUNC (button_release_event), NULL);
  g_signal_connect (G_OBJECT (playing_area), "motion_notify_event",
		      GTK_SIGNAL_FUNC (motion_notify_event), NULL);
  g_signal_connect (G_OBJECT (playing_area), "button_press_event",
		      GTK_SIGNAL_FUNC (button_press_event), NULL);
  g_signal_connect (G_OBJECT (playing_area), "configure_event",
		      GTK_SIGNAL_FUNC (configure_event), NULL);
  g_signal_connect (G_OBJECT (playing_area), "expose_event",
		    GTK_SIGNAL_FUNC (expose_event), NULL);
  /* No need for gtk's double buffering, since we have our own. */
  gtk_widget_set_double_buffered(playing_area, FALSE);
}

void quit_app (GtkMenuItem *menuitem)
{
  /* If the game isn't over, then it counts as a failure. */
  if (game_in_progress) {
    update_statistics (FALSE, 0);
  }

  gtk_main_quit ();
}

static void create_main_window ()
{
  GConfClient * gconf_client = gconf_client_get_default ();
  gint width, height;

  width = gconf_client_get_int (gconf_client, WIDTH_GCONF_KEY, NULL);
  height = gconf_client_get_int (gconf_client, HEIGHT_GCONF_KEY, NULL);

  app = gnome_app_new ("aisleriot", _("Aisleriot"));
  gtk_window_set_default_size (GTK_WINDOW (app), width, height);

  gtk_widget_realize (app);

  g_signal_connect (GTK_OBJECT (app), "delete_event", 
		      GTK_SIGNAL_FUNC (quit_app), NULL);
  g_signal_connect (GTK_OBJECT (app), "configure_event",
		      GTK_SIGNAL_FUNC (app_configure_event), NULL);
}

gchar* start_game;

static void main_prog(void *closure, int argc, char *argv[])
{
  GtkWidget *score_label, *time_label, *time_box;

  cscm_init();

  create_main_window ();
  vbox = gtk_vbox_new (FALSE, 0);
  gnome_app_set_contents (GNOME_APP (app), vbox);

  load_pixmaps ();

  statusbar = gtk_statusbar_new ();
  gtk_statusbar_set_has_resize_grip (GTK_STATUSBAR (statusbar), FALSE);

  create_menus ();

  gtk_box_pack_start (GTK_BOX (vbox), menubar, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), toolbar, FALSE, FALSE, 0);  
  create_sol_board ();
  gtk_box_pack_end (GTK_BOX (vbox), statusbar, FALSE, FALSE, 0);

  time_box = gtk_hbox_new (0, FALSE);
  score_box = gtk_hbox_new (0, FALSE);
  score_label = gtk_label_new (_("Score:"));
  gtk_box_pack_start (GTK_BOX(score_box), score_label, FALSE, FALSE, 0);
  score_value = gtk_label_new ("   0");
  gtk_box_pack_start (GTK_BOX(score_box), score_value, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX(time_box), score_box, FALSE, FALSE, 0);
  time_label = gtk_label_new (_("Time:"));
  gtk_box_pack_start (GTK_BOX(time_box), time_label, FALSE, FALSE, 0);
  time_value = games_clock_new ();
  gtk_box_pack_start (GTK_BOX(time_box), time_value, FALSE, FALSE, GNOME_PAD_SMALL);

  gtk_box_pack_end (GTK_BOX(statusbar), time_box, FALSE, FALSE, 0);

  new_game (start_game, NULL);

  gtk_widget_show_all (app);

  if (score_is_hidden)
    gtk_widget_hide (score_box);
  else
    gtk_widget_show (score_box);

  gtk_window_set_focus (GTK_WINDOW (app), NULL);

  if (!gconf_client_get_bool (gconf_client,
                             "/apps/aisleriot/show_toolbar", NULL))
    gtk_widget_hide (toolbar);

  click_to_move = gconf_client_get_bool (gconf_client, 
					 "/apps/aisleriot/click_to_move", 
					 NULL);

  create_press_data (playing_area->window);

  gtk_main ();

  gnome_accelerators_sync ();

  free_pixmaps();
  g_object_unref (surface);
}

static void
retrieve_state (GnomeClient *client)
{
  start_game = games_gconf_get_string (gconf_client,
                                       "/apps/aisleriot/game_file",
                                       "klondike.scm");
  card_style = games_gconf_get_string (gconf_client,
				       THEME_GCONF_KEY,
				       "bonded.svg");
}

int main (int argc, char *argv [])
{
  static const GOptionEntry aisleriot_opts[] = {
    {"variation", 'v', 0, G_OPTION_ARG_STRING, &variation,
     N_("Select the game to play"), N_("NAME")},
    {NULL}
  };
  gchar * var_file;
  GOptionContext *option_context;
  GnomeProgram *program;

  bindtextdomain (GETTEXT_PACKAGE, GNOMELOCALEDIR);
  bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
  textdomain (GETTEXT_PACKAGE);

  option_context = g_option_context_new ("");
  g_option_context_add_main_entries (option_context,
                                     aisleriot_opts,
                                     GETTEXT_PACKAGE);
  
  program = gnome_program_init ("aisleriot", VERSION,
				LIBGNOMEUI_MODULE, 
				argc, argv,
				GNOME_PARAM_GOPTION_CONTEXT, option_context,
				GNOME_PARAM_APP_DATADIR, DATADIR, NULL);
  glade_init ();

  gconf_client = gconf_client_get_default ();
  games_gconf_sanity_check_string (gconf_client, "/apps/aisleriot/game_file");
  gconf_client_add_dir (gconf_client, "/apps/aisleriot", 
			GCONF_CLIENT_PRELOAD_ONELEVEL, NULL);
  gconf_client_notify_add (gconf_client, STATISTICS_KEY, 
			   (GConfClientNotifyFunc) load_statistics,
			   NULL, NULL, NULL);
  load_statistics ();
  
  gtk_window_set_default_icon_name ("gnome-aisleriot.png");
  g_signal_connect (GTK_OBJECT (gnome_master_client ()), "save_yourself",
		      GTK_SIGNAL_FUNC (save_state), 
		      (gpointer) g_path_get_basename(argv[0]));
  g_signal_connect (GTK_OBJECT (gnome_master_client ()), "die",
		      GTK_SIGNAL_FUNC (quit_app), NULL);

  retrieve_state (gnome_master_client ());

  gamesdir = gnome_program_locate_file (NULL, GNOME_FILE_DOMAIN_APP_DATADIR,
	                                GAMESDIR, FALSE, NULL);

  /* If we are asked for a specific game, check that it is valid. */
  if (variation && *variation) {
    var_file = game_name_to_file (variation);
    if (g_file_test (var_file, G_FILE_TEST_EXISTS 
                              | G_FILE_TEST_IS_REGULAR)) {
      dont_save = TRUE;
      start_game = g_path_get_basename (var_file);
    }
    g_free (var_file);
  }

  scm_boot_guile (argc, argv, main_prog, NULL);

  g_object_unref (program);
  return 0;
}
