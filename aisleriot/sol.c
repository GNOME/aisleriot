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
#include <time.h>
#include <dirent.h>
#include <libguile.h>
#include <guile/gh.h>
#include <ctype.h>
#include "sol.h"
#include "events.h"
#include "draw.h"
#include <gnome.h>
#include <gconf/gconf-client.h>
#include <libgnomeui/gnome-window-icon.h>
#include "slot.h"
#include "card.h"
#include "cscmi.h"
#include "menu.h"
#include "splash.h"
#include "games-clock.h"
#include "games-gconf.h"

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
gboolean         dont_save = FALSE; /* If the game is selected on the
                                     * command line we assume that it is
                                     * special and don't save the state.
                                     * This is essential for Freecell
                                     * emulation.*/

guint            score;
/* guint            game_seconds;*/
guint            timeout;
guint32          seed;
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
GConfClient      *gconf_client = NULL;

#define DEFAULT_VARIATION "klondike.scm"
#define GNOME_SESSION_BUG

gchar* game_file_to_name (const gchar* file)
{
  char* p, *buf = g_path_get_basename(file);

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
static gpointer* game_file_to_help_entry (const gchar* file)
{
#if 0
  GnomeHelpMenuEntry* entry;
  char* p;
  GString* help = g_string_new (g_basename(file));

  if ((p = strrchr (help->str, '.'))) 
    g_string_truncate (help, p - help->str);
  for(p = help->str; p = strchr(p, '_'), p && *p;) *p = '-';
  g_string_append (help, ".html");

  entry = g_new (GnomeHelpMenuEntry, 1);
  entry->name = g_strdup ("aisleriot");
  entry->path = g_strdup(help->str);

  g_string_free(help, TRUE);
  return (gpointer *) entry;
#else
  return NULL;
#endif
}

void make_title () 
{
  char *title;

  title = g_strdup_printf (_("AisleRiot: %s (%d)"), game_name, seed);

  gtk_window_set_title (GTK_WINDOW (app), title); 

  g_free (title);
}

void eval_installed_file (char *file)
{
  char *installed_filename;
  char *relative;
  
  if (g_file_test (file, G_FILE_TEST_EXISTS)){
    gh_eval_file (file);
    return;
  }
  
  relative = g_strconcat (GAMESDIR, file, NULL);
  installed_filename = gnome_program_locate_file (NULL, 
                                                  GNOME_FILE_DOMAIN_APP_DATADIR,
                                                  relative,
                                                  FALSE, NULL);

  if (g_file_test(installed_filename, G_FILE_TEST_EXISTS)){
    gh_eval_file (installed_filename);
  } else {
    char *message = g_strdup_printf (
         _("Aisleriot can't load the file: \n%s\n\n"
           "Please check your Aisleriot installation"), installed_filename);
    GtkWidget *w = gtk_message_dialog_new (GTK_WINDOW(app),
                                       GTK_DIALOG_DESTROY_WITH_PARENT,
                                       GTK_MESSAGE_ERROR,
                                       GTK_BUTTONS_OK,
                                       message);

    gtk_dialog_set_has_separator (GTK_DIALOG (w), FALSE);
    gtk_dialog_run (GTK_DIALOG(w));
		gtk_widget_destroy(w);
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

static int
save_state (GnomeClient *client)
{
  gconf_client_set_string (gconf_client, "/apps/aisleriot/game_file",
                           game_file, NULL);
  gconf_client_set_string (gconf_client, "/apps/aisleriot/card_options",
                           deck_options, NULL);

  return TRUE;
}

void new_game (gchar* file, guint *seedp )
{
  SCM size;
  gint min_w, min_h;

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
/*      gnome_app_remove_menus (GNOME_APP (app), buf, 1); */
      if(rules_help[0].user_data) {
#if 0
	GnomeHelpMenuEntry *entry = 
	  (GnomeHelpMenuEntry *) rules_help[0].user_data;
	g_free (entry->name);
	g_free (entry->path);
	g_free (entry);
#endif
      }
    }
    game_name = game_file_to_name (file);

    if (!dont_save)
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
    seed = g_random_int();
  }
  g_random_set_seed(seed);
  score = 0;
  set_score();

  if(surface) 
    timer_start();

  size = gh_call0(game_data->start_game_lambda);
  gh_eval_str ("(start-game)");

  min_w = gh_scm2int (gh_car (size))*get_horiz_offset() + 2*get_horiz_start();
  min_h = gh_scm2int (gh_cadr (size))*get_vert_offset() + 2*get_vert_start();
  gtk_widget_set_size_request (playing_area, min_w, min_h);

  /* It is possible for some games to not have any moves right from the
   * start. If this happens we redeal. */
  if (!gh_scm2bool (gh_call0 (game_data->game_over_lambda))) {
    new_game (file, seedp);
  } else {
    if (surface)
      refresh_screen();
    
    undo_set_sensitive (FALSE);
    redo_set_sensitive (FALSE);
    
    game_over = FALSE;
    make_title();
  }
}

GtkWidget *score_value;

void set_score () 
{
  char b [10];
  g_snprintf (b, sizeof (b), "%6d  ", score);

  gtk_label_set_text(GTK_LABEL(score_value), b);
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

static gint timer_cb ()
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
  games_clock_stop (GAMES_CLOCK (time_value));
  timeout = 3600;
/*  set_time();
 *  timer_timeout = gtk_timeout_add (1000, (GtkFunction) (timer_cb), NULL);
 */
  games_clock_set_seconds (GAMES_CLOCK (time_value), 0);
  games_clock_start (GAMES_CLOCK (time_value));
  timer_timeout = gtk_timeout_add (timeout * 1000, (GtkFunction) (timer_cb), NULL);
}

void timer_stop ()
{
  games_clock_stop (GAMES_CLOCK (time_value));
  gtk_timeout_remove (timer_timeout);
  timer_timeout = 0;
}

/*
 * setup suff
 */


static void create_sol_board ()
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
  

  g_signal_connect (GTK_OBJECT(playing_area),"button_release_event",
		      GTK_SIGNAL_FUNC (button_release_event), NULL);
  g_signal_connect (GTK_OBJECT (playing_area), "motion_notify_event",
		      GTK_SIGNAL_FUNC (motion_notify_event), NULL);
  g_signal_connect (GTK_OBJECT (playing_area), "button_press_event",
		      GTK_SIGNAL_FUNC (button_press_event), NULL);
}

gboolean quit_app (GtkMenuItem *menuitem)
{
  gtk_main_quit ();
}

static void create_main_window ()
{
  app = gnome_app_new ("aisleriot", _("Aisleriot"));

  gtk_widget_realize (app);
  
  g_signal_connect (GTK_OBJECT (app), "delete_event", 
		      GTK_SIGNAL_FUNC (quit_app), NULL);
  g_signal_connect (GTK_OBJECT (app), "configure_event",
		      GTK_SIGNAL_FUNC (configure_event), NULL);
}

static void create_press_data ()
{
  GdkWindowAttr attributes;

  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.event_mask = 0;
  attributes.width = get_card_width();
  attributes.height = get_card_height();
  attributes.colormap = gdk_drawable_get_colormap (GDK_DRAWABLE(playing_area->window));
  attributes.visual = gdk_drawable_get_visual (GDK_DRAWABLE(playing_area->window));
  
  press_data = malloc(sizeof(press_data_type));
  press_data->moving_cards = gdk_window_new(playing_area->window, &attributes,
					    (GDK_WA_VISUAL | GDK_WA_COLORMAP));
  press_data->status = 0;
}

gchar* start_game;

static void main_prog(int argc, char *argv[])
{
  GtkWidget *score_label, *time_label, *score_box, *status_bar;
  
  seed = time(NULL);
  g_random_set_seed(seed);

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
  time_value = games_clock_new ();
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

  if (!gconf_client_get_bool (gconf_client,
                             "/apps/aisleriot/show_toolbar", NULL))
    toolbar_hide();

  create_press_data ();

  gtk_widget_pop_colormap ();

  gtk_main ();

  free_pixmaps();
  g_object_unref (surface);
  g_object_unref (press_data->moving_cards);
}

static void
retrieve_state (GnomeClient *client)
{
  start_game = games_gconf_get_string (gconf_client,
                                       "/apps/aisleriot/game_file",
                                       "klondike.scm");
  deck_options = games_gconf_get_string (gconf_client,
                                         "/apps/aisleriot/card_options",
                                         "beige.png bonded.png gnome.png bold-09x14.png knuth-09x10.png knuth-18x21.png knuth-21x25.png");
}

static int
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

  bindtextdomain (GETTEXT_PACKAGE, GNOMELOCALEDIR);
  bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
  textdomain (GETTEXT_PACKAGE);

  gnome_program_init ("aisleriot", VERSION,
 		      LIBGNOMEUI_MODULE, 
 		      argc, argv,
 		      GNOME_PARAM_POPT_TABLE, aisleriot_opts,
 		      GNOME_PARAM_APP_DATADIR, DATADIR, NULL);

  gconf_client = gconf_client_get_default ();
  games_gconf_sanity_check_string (gconf_client, "/apps/aisleriot/game_file");
  
  gtk_widget_push_colormap (gdk_rgb_get_colormap ());

  gnome_window_icon_set_default_from_file (GNOME_ICONDIR"/gnome-aisleriot.png");
  splash_new ();

  splash_update (_("Initializing scheme..."), 0.20);

  g_signal_connect (GTK_OBJECT (gnome_master_client ()), "save_yourself",
		      GTK_SIGNAL_FUNC (save_state), 
		      (gpointer) g_path_get_basename(argv[0]));
  g_signal_connect (GTK_OBJECT (gnome_master_client ()), "die",
		      GTK_SIGNAL_FUNC (quit_app), NULL);

  retrieve_state (gnome_master_client ());

  dir = gnome_program_locate_file (NULL, GNOME_FILE_DOMAIN_APP_DATADIR,
	                                        GAMESDIR, FALSE, NULL);

  records = scandir (dir, &game_dents, is_game, alphasort);
  g_free(dir);

  if (records >= 0)
	  n_games = records;
  else
	  n_games = 0;

  for (i = 0; i < n_games; i++) {
    gchar *game_name = game_file_to_name (game_dents[i]->d_name);
    if (!strcasecmp (variation, game_name)) {
      dont_save = TRUE;
      start_game = g_strdup ((gchar*) game_dents[i]->d_name);
      g_free (game_name);
      break;
    }
    g_free (game_name);
  }
  
  gh_enter(argc, argv, main_prog);
  return 0;
}
