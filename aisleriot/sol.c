/* AisleRiot - sol.c
 * Copyright (C) 1998, 2001 Jonathan Blandford <jrb@alum.mit.edu>
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
#include <time.h>
#include <dirent.h>
#include <libguile.h>
#include <guile/gh.h>
#include <ctype.h>
#include "sol.h"
#include "events.h"
#include "draw.h"
#include <gnome.h>
#include <libgnomeui/gnome-window-icon.h>
#include "slot.h"
#include "card.h"
#include "cscmi.h"
#include "menu.h"
#include "splash.h"

/*
 * Global Variables
 */
GtkWidget        *app;
GtkWidget        *playing_area;
GtkWidget        *option_dialog = NULL;
GdkGC            *draw_gc;
GdkPixmap        *surface;
GdkPixmap        *moving_card_pixmap;
GtkObject*       card_deck;
GdkCardDeckOptions deck_options = NULL;

guint            score;
/* guint            game_seconds;*/
guint            timeout;
guint            seed;
guint            n_games;
struct dirent    **game_dents;
gchar            *game_file = "";
gchar            *game_name;
gboolean         game_over;
gboolean         game_won;
press_data_type* press_data; 

guint            x_spacing = 5;
guint            y_spacing = 15;
guint            x_expanded_offset = 20;
guint            y_expanded_offset = 20;

#define DEFAULT_VARIATION "klondike.scm"
#define GNOME_SESSION_BUG

gchar* game_file_to_name (const gchar* file)
{
  char* p, *buf = g_strdup (g_filename_pointer(file));

  if ((p = strrchr (buf, '.'))) *p = '\0';
  for(p = buf; p = strchr(p, '_'), p && *p;) *p = ' ';
  for(p = buf; p = strchr(p, '-'), p && *p;) *p = ' ';
  buf[0] = toupper(buf[0]);
  p = g_strdup(_(buf));

  g_free(buf);
  return p;
}

/*
 * This function assumes that xxx.scm has a HTML help file xxx.html
 * located in the gnome/help/aisleriot/../ dir (nb '_' -> '-' for DocBook).
 */
gpointer* game_file_to_help_entry (const gchar* file)
{
  GnomeHelpMenuEntry* entry;
  char* p;
  GString* help = g_string_new (g_filename_pointer(file));

  if ((p = strrchr (help->str, '.'))) 
    g_string_truncate (help, p - help->str);
  for(p = help->str; p = strchr(p, '_'), p && *p;) *p = '-';
  g_string_append (help, ".html");

  entry = g_new (GnomeHelpMenuEntry, 1);
  entry->name = g_strdup ("aisleriot");
  entry->path = g_strdup(help->str);

  g_string_free(help, TRUE);
  return (gpointer *) entry;
}

void make_title () 
{
  char *title;

  title = g_strdup_printf ("AisleRiot:  %s  ( %d )", game_name, seed);

  gtk_window_set_title (GTK_WINDOW (app), title); 

  g_free (title);
}

void eval_installed_file (char *file)
{
  char *installed_filename;
  char *relative;
  
  if (g_file_exists (file)){
    gh_eval_file (file);
    return;
  }
  
  relative = g_strconcat (GAMESDIR, file, NULL);
  installed_filename = gnome_unconditional_datadir_file (relative);

  if (g_file_exists(installed_filename)){
    gh_eval_file (installed_filename);
  } else {
    char *message = g_strdup_printf (
         _("Aisleriot can't load the file: \n%s\n\n"
           "Please check your Aisleriot installation"), installed_filename);
    GtkWidget *w = gnome_error_dialog (message);
    gnome_dialog_run_and_close (GNOME_DIALOG(w));
    g_free (message);
    exit (1);
  }
  g_free (installed_filename);
  g_free (relative);
}

GnomeUIInfo rules_help[] = {
  GNOMEUIINFO_ITEM_NONE(NULL, NULL, gnome_help_display),

  GNOMEUIINFO_END
};

void new_game (gchar* file, guint *seedp )
{
  SCM size;
  gint old_w, old_h, min_w, min_h;

  if (file && strcmp (file, game_file)) {
    gchar buf[100];
    GtkWidget *ms;
    guint pos;

    game_file = file;

    /* Although this line slows down game switching by a noticeable amount, we
     * add it here in order to make sure all the original functions are
     * "clean". */
    eval_installed_file ("sol.scm");
    eval_installed_file (file);
    if(game_name) {
      g_snprintf(buf, sizeof (buf), "%s/%s", _("Help"), game_name);
      gnome_app_remove_menus (GNOME_APP (app), buf, 1);
      if(rules_help[0].user_data) {
	GnomeHelpMenuEntry *entry = 
	  (GnomeHelpMenuEntry *) rules_help[0].user_data;
	g_free (entry->name);
	g_free (entry->path);
	g_free (entry);
      }
    }
    game_name = game_file_to_name (file);

    save_state (gnome_master_client ());

    rules_help[0].label = game_name;
    rules_help[0].user_data = game_file_to_help_entry(file);
    g_snprintf(buf, sizeof (buf), "%s/%s", _("Help"), _("Aisleriot"));

    ms = gnome_app_find_menu_pos(GNOME_APP (app)->menubar, buf, &pos);
    if (ms != NULL)
      gnome_app_fill_menu (GTK_MENU_SHELL(ms), rules_help, 
			   GNOME_APP (app)->accel_group, TRUE, pos);
    if(option_dialog) {
      gtk_widget_destroy(option_dialog);
      option_dialog = NULL;
    }
  }

  if (seedp) {
    seed = *seedp;
  }
  else {
    seed = random();
  }
  srandom(seed);
  score = 0;
  set_score();

  if(surface) 
    timer_start();

  size = gh_call0(game_data->start_game_lambda);
  gh_eval_str ("(start-game)");
  min_w = gh_scm2int (gh_car (size))*get_horiz_offset() + 2*get_horiz_start();
  min_h = gh_scm2int (gh_cadr (size))*get_vert_offset() + 2*get_vert_start();

  gdk_window_get_size (playing_area->window, &old_w, &old_h);
  gtk_widget_set_usize (playing_area, min_w, min_h);

  if (surface)
    refresh_screen();

  game_over = FALSE;
  make_title();
  end_of_game_test();
}

GtkWidget *score_value;

void set_score () 
{
  char b [10];
  g_snprintf (b, sizeof (b), "%6d  ", score);

  gtk_label_set(GTK_LABEL(score_value), b);
}

GtkWidget *time_value;

/* void set_time () 
 *{
 *  char b [10];
 *  sprintf (b, "%3d:%02d ", game_seconds / 60, game_seconds % 60);
 *
 *  gtk_label_set(GTK_LABEL(time_value), b);
 *}
 */

gint timer_cb ()
{
#if 0
/*  game_seconds++;
 *  set_time();
 *  if (game_seconds > timeout) {
 *    timeout = 3600;
 *    gh_call0(game_data->timeout_lambda);
 *    end_of_game_test();
 *  }
 *  return game_seconds < 3599; *//* give up at end of one hour */
#endif
  timeout = 3600;
  gh_call0(game_data->timeout_lambda);
  end_of_game_test();
  return 0;	
}

guint timer_timeout = 0;

void timer_start ()
{
  if (timer_timeout)
/*    timer_stop();
 *  game_seconds = 0;
 */
  gtk_clock_stop (GTK_CLOCK (time_value));
  timeout = 3600;
/*  set_time();
 *  timer_timeout = gtk_timeout_add (1000, (GtkFunction) (timer_cb), NULL);
 */
  gtk_clock_set_seconds (GTK_CLOCK (time_value), 0);
  gtk_clock_start (GTK_CLOCK (time_value));
  timer_timeout = gtk_timeout_add (timeout * 1000, (GtkFunction) (timer_cb), NULL);
}

void timer_stop ()
{
  gtk_clock_stop (GTK_CLOCK (time_value));
  gtk_timeout_remove (timer_timeout);
  timer_timeout = 0;
}

/*
 * setup suff
 */

void create_sol_board ()
{
  playing_area = gtk_drawing_area_new ();
  gtk_widget_set_events (playing_area, 
			 gtk_widget_get_events (playing_area) | GAME_EVENTS);
  
  gnome_app_set_contents (GNOME_APP (app), playing_area);
  
  gtk_widget_realize (playing_area);

  draw_gc = gdk_gc_new(playing_area->window);
  if (get_background_pixmap ())
    gdk_gc_set_tile (draw_gc, get_background_pixmap());
  gdk_gc_set_fill (draw_gc, GDK_TILED);
  
  gtk_signal_connect (GTK_OBJECT(playing_area),"button_release_event",
		      GTK_SIGNAL_FUNC (button_release_event), NULL);
  gtk_signal_connect (GTK_OBJECT (playing_area), "motion_notify_event",
		      GTK_SIGNAL_FUNC (motion_notify_event), NULL);
  gtk_signal_connect (GTK_OBJECT (playing_area), "button_press_event",
		      GTK_SIGNAL_FUNC (button_press_event), NULL);
}

void quit_app (GtkWidget *app)
{
  gtk_widget_destroy (app);
  gtk_main_quit ();
}

void create_main_window ()
{
  /* This is the prefix used to retrieve the state when NOT restarted: */
  gchar* prefix = 
    gnome_client_get_global_config_prefix (gnome_master_client ());

  app = gnome_app_new ("sol", _("Aisleriot"));
  /* Use "prefix" as the default config location ... */
  gnome_config_push_prefix (prefix);
  /* ... and use it for the menubar and toolbar aw well: */
  GNOME_APP (app)->prefix = prefix;

  gtk_widget_realize (app);

  gtk_signal_connect (GTK_OBJECT (app), "delete_event", 
		      GTK_SIGNAL_FUNC (quit_app), NULL);
  gtk_signal_connect (GTK_OBJECT (app), "configure_event",
		      GTK_SIGNAL_FUNC (configure_event), NULL);

  gtk_window_set_policy (GTK_WINDOW (app), TRUE, TRUE, FALSE);
}

void create_press_data ()
{
  GdkWindowAttr attributes;

  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.event_mask = 0;
  attributes.width = get_card_width();
  attributes.height = get_card_height();
  attributes.colormap = gdk_window_get_colormap (playing_area->window);
  attributes.visual = gdk_window_get_visual (playing_area->window);
  
  press_data = malloc(sizeof(press_data_type));
  press_data->moving_cards = gdk_window_new(playing_area->window, &attributes,
					    (GDK_WA_VISUAL | GDK_WA_COLORMAP));
  press_data->status = 0;
}

gchar* start_game;

void main_prog(int argc, char *argv[])
{
  GtkWidget *score_label, *time_label, *score_box, *status_bar;

  seed = time(NULL);
  srandom(seed);

  cscm_init();

  splash_update (_("Loading images..."), 0.70);

  create_main_window ();

  load_pixmaps (app, deck_options);
 
  splash_update (_("Dealing game..."), 0.90);

  create_sol_board ();
  create_menus ();

  score_box = gtk_hbox_new(0, FALSE);
  score_label = gtk_label_new (_("Score: "));
  gtk_box_pack_start (GTK_BOX(score_box), score_label, FALSE, FALSE, 0);
  score_value = gtk_label_new ("   0");
  gtk_box_pack_start (GTK_BOX(score_box), score_value, FALSE, FALSE, 0);
  time_label = gtk_label_new (_("Time: "));
  gtk_box_pack_start (GTK_BOX(score_box), time_label, FALSE, FALSE, 0);
  time_value = gtk_clock_new (GTK_CLOCK_INCREASING);
  gtk_box_pack_start (GTK_BOX(score_box), time_value, FALSE, FALSE, GNOME_PAD_SMALL);

  gtk_widget_show (score_label);
  gtk_widget_show (score_value);
  gtk_widget_show (time_label);
  gtk_widget_show (time_value);
  gtk_widget_show (score_box);

  status_bar = gnome_appbar_new (FALSE, TRUE, FALSE);
  gtk_box_pack_end (GTK_BOX(status_bar), score_box, FALSE, FALSE, 0);
  gnome_app_set_statusbar (GNOME_APP (app), status_bar);

  install_menu_hints(GNOME_APP (app));

  new_game (start_game, &seed);

  splash_destroy ();

  gtk_widget_show (app);
  create_press_data ();

  gtk_widget_pop_visual ();
  gtk_widget_pop_colormap ();

  gtk_main ();

  free_pixmaps();
  gdk_pixmap_unref (surface);
  gdk_window_unref (press_data->moving_cards);
}

static int
save_state (GnomeClient *client)
{
  gchar *prefix = gnome_client_get_config_prefix (client);

  gnome_config_push_prefix (prefix);
  gnome_config_set_string ("Variation/Default File", game_file);
  gnome_config_set_string ("Deck/Options", deck_options);
  gnome_config_pop_prefix ();
  gnome_config_sync();

#ifndef GNOME_SESSION_BUG
  argv[0] = "rm";
  argv[1] = gnome_config_get_real_path (prefix);
  gnome_client_set_discard_command (client, 2, argv);
#endif  
  return TRUE;
}

static void
retrieve_state (GnomeClient *client)
{
  gchar *prefix = gnome_client_get_config_prefix (client);

  gnome_config_push_prefix (prefix);
  start_game = gnome_config_get_string_with_default 
    ("Variation/Default File=" DEFAULT_VARIATION, NULL);
  deck_options = gnome_config_get_string ("Deck/Options");
  gnome_config_pop_prefix ();

#ifdef GNOME_SESSION_BUG
  if (strcmp (prefix, gnome_client_get_global_config_prefix (client))) {
    prefix = gnome_config_get_real_path (prefix);
    prefix[ strlen (prefix) - 1] = '\0';
    unlink (prefix);
  }
#endif  
}

int
is_game (const struct dirent* dent)
{
  return (!strcmp (g_extension_pointer (dent->d_name),"scm") &&
	  strcmp (dent->d_name,"sol.scm"));
}

int main (int argc, char *argv [])
{
  gchar* variation = "";
  struct poptOption aisleriot_opts[] = {
    {"variation", '\0', POPT_ARG_STRING, NULL, 0, NULL, NULL},
    {NULL, '\0', 0, NULL, 0, NULL, NULL}
  };
  gint i, records;
  gchar* dir;

  aisleriot_opts[0].arg = &variation;
  aisleriot_opts[0].descrip = N_("Variation on game rules");
  aisleriot_opts[0].argDescrip = N_("NAME");

  gnome_score_init ("Aisleriot");

  bindtextdomain (PACKAGE, GNOMELOCALEDIR);
  textdomain (PACKAGE);

  gnome_init_with_popt_table ("Aisleriot", VERSION, argc, argv,
			      aisleriot_opts, 0, NULL);

  gtk_widget_push_colormap (gdk_imlib_get_colormap ());
  gtk_widget_push_visual (gdk_imlib_get_visual ());

  gnome_window_icon_set_default_from_file (GNOME_ICONDIR"/gnome-aisleriot.png");
  splash_new ();

  splash_update (_("Initializing scheme..."), 0.20);

  gtk_signal_connect (GTK_OBJECT (gnome_master_client ()), "save_yourself",
		      GTK_SIGNAL_FUNC (save_state), 
		      (gpointer) program_invocation_name);
  gtk_signal_connect (GTK_OBJECT (gnome_master_client ()), "die",
		      GTK_SIGNAL_FUNC (quit_app), NULL);

  retrieve_state (gnome_master_client ());

  dir = gnome_unconditional_datadir_file (GAMESDIR);
  records = scandir (dir, &game_dents, is_game, alphasort);
  g_free(dir);

  if (records >= 0)
	  n_games = records;
  else
	  n_games = 0;

  for (i = 0; i < n_games; i++)
    if (!strcasecmp (variation, game_file_to_name (game_dents[i]->d_name))) {
      start_game = g_strdup ((gchar*) game_dents[i]->d_name);
      break;
    }

  gh_enter(argc, argv, main_prog);
  return 0;
}
