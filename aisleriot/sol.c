/* Aisleriot - sol.c
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

#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <dirent.h>
#include <libguile.h>
#include <guile/gh.h>
#include "sol.h"
#include "events.h"
#include "draw.h"
#include "gnome.h"
#include "slot.h"
#include "card.h"
#include "cscmi.h"
#include "menu.h"
/*
 * Global Variables
 */
GtkWidget        *app;
GtkWidget        *playing_area;
GdkGC            *draw_gc;
GdkPixmap        *surface;
GdkPixmap        *moving_card_pixmap;
guint            score;
guint            game_seconds;
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

/*
 * This function does i18n on game files using the convention that
 * filenames are LC_MESSAGES with mechanical changes:
 */

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
  gint old_w, old_h;
  GString* title = g_string_new (game_name);
  g_string_sprintfa (title, "  ( %d )", seed);

  gtk_window_set_title (GTK_WINDOW (app), title->str); 
}

void eval_installed_file (char *file)
{
  char *installed_filename;
  char *relative;
  
  if (g_file_exists (file)){
    gh_eval_file (file);
    return;
  }
  
  relative = g_copy_strings (GAMESDIR, file, NULL);
  installed_filename = gnome_datadir_file (relative);
  gh_eval_file (installed_filename);
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
    eval_installed_file (file);
    if(game_name) {
      sprintf(buf, "%s/%s", N_("Help"), game_name);
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

    rules_help[0].label = game_name;
    rules_help[0].user_data = game_file_to_help_entry(file);
    sprintf(buf, "%s/%s", N_("Help"), N_("Aisleriot"));

    ms = gnome_app_find_menu_pos(GNOME_APP (app)->menubar, buf, &pos);
    gnome_app_fill_menu (GTK_MENU_SHELL(ms), rules_help, 
			 GNOME_APP (app)->accel_group, TRUE, TRUE, pos);
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

  size = gh_apply (game_data->start_game_lambda, SCM_EOL);
  min_w = gh_scm2int (gh_car (size))*get_horiz_offset() + 2*get_horiz_start();
  min_h = gh_scm2int (gh_cadr (size))*get_vert_offset() + 2*get_vert_start();

  gdk_window_get_size (playing_area->window, &old_w, &old_h);
  gtk_widget_set_usize (playing_area, min_w, min_h);

  if(old_w >= min_w && old_h >= min_h)
    refresh_screen();

  game_over = FALSE;
  make_title();
}

GtkWidget *score_value;

void set_score () 
{
  char b [10];
  sprintf (b, "%6d  ", score);

  gtk_label_set(GTK_LABEL(score_value), b);
}

GtkWidget *time_value;

void set_time () 
{
  char b [10];
  sprintf (b, "%3d:%02d ", game_seconds / 60, game_seconds % 60);

  gtk_label_set(GTK_LABEL(time_value), b);
}

gint timer_cb ()
{
  game_seconds++;
  set_time();
  if (game_seconds > timeout) {
    timeout = 3600;
    gh_apply(game_data->timeout_lambda, SCM_EOL);
    end_of_game_test();
  }
  return game_seconds < 3599; /* give up at end of one hour */
}

guint timer_timeout = 0;

void timer_start ()
{
  if (timer_timeout)
    timer_stop();
  game_seconds = 0;
  timeout = 3600;
  set_time();
  timer_timeout = gtk_timeout_add (1000, (GtkFunction) (timer_cb), NULL); 
}

void timer_stop ()
{
  gtk_timeout_remove (timer_timeout);
  timer_timeout = 0;
}

/*
 * setup suff
 */

void create_sol_board ()
{
  gtk_widget_push_colormap (gdk_imlib_get_colormap ());
  gtk_widget_push_visual (gdk_imlib_get_visual ());
  
  playing_area = gtk_drawing_area_new ();
  
  gtk_widget_pop_visual ();
  gtk_widget_pop_colormap ();
  
  gtk_widget_set_events (playing_area, 
			 gtk_widget_get_events (playing_area) | GAME_EVENTS);
  
  gnome_app_set_contents (GNOME_APP (app), playing_area);
  
  gtk_widget_realize (playing_area);

  draw_gc = gdk_gc_new(playing_area->window);
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
  app = gnome_app_new ("AisleRiot", _("AisleRiot"));
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
  press_data->button_pressed = 0;
  press_data->moving_cards = gdk_window_new(playing_area->window, &attributes,
					    (GDK_WA_VISUAL | GDK_WA_COLORMAP));
}

int
is_game (struct dirent* dent)
{
  return (!strcmp (g_extension_pointer (dent->d_name),"scm") &&
	  strcmp (dent->d_name,"sol.scm"));
}

void main_prog(int argc, char *argv[])
{
  GtkWidget *score_label, *time_label, *score_box, *status_bar;
  char* dir;
  
  bindtextdomain (PACKAGE, GNOMELOCALEDIR);
  textdomain (PACKAGE);
  //argp_program_version = VERSION;

  dir = gnome_datadir_file (GAMESDIR);
  n_games = scandir (dir, &game_dents, is_game, alphasort);
  g_free(dir);

  gnome_init ("aisleriot", NULL, argc, argv, 0, NULL);

  seed = time(NULL);
  srandom(seed);

  cscm_init();

  create_main_window ();

  load_pixmaps (app);
 
  create_sol_board ();
  create_menus ();

  score_box = gtk_hbox_new(0, FALSE);
  score_label = gtk_label_new (_("Score:"));
  gtk_box_pack_start (GTK_BOX(score_box), score_label, FALSE, FALSE, 0);
  score_value = gtk_label_new ("     0  ");
  gtk_box_pack_start (GTK_BOX(score_box), score_value, FALSE, FALSE, 0);
  time_label = gtk_label_new (_("Time:"));
  gtk_box_pack_start (GTK_BOX(score_box), time_label, FALSE, FALSE, 0);
  time_value = gtk_label_new ("  0:00 ");
  gtk_box_pack_start (GTK_BOX(score_box), time_value, FALSE, FALSE, 0);

  gtk_widget_show (score_label);
  gtk_widget_show (score_value);
  gtk_widget_show (time_label);
  gtk_widget_show (time_value);
  gtk_widget_show (score_box);

  status_bar = gnome_appbar_new (FALSE, TRUE, TRUE);
  gtk_box_pack_end (GTK_BOX(status_bar), score_box, FALSE, FALSE, 0);
  gnome_app_set_statusbar (GNOME_APP (app), status_bar);

  new_game ("klondike.scm", &seed);

  gtk_widget_show (app);
  create_press_data ();

  gtk_main ();

  free_pixmaps();
  gdk_pixmap_unref (surface);
  gdk_window_unref (press_data->moving_cards);
}

int main (int argc, char *argv [])
{
  gh_enter(argc, argv, main_prog);
  return 0;
}
