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

#define SOL_C

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
 * Variables
 */

GtkWidget *app;
GtkWidget *status_bar;
GtkWidget *playing_area;
GdkGC *draw_gc;  /* needed for tiling operations */
GdkPixmap *surface = NULL;
GdkPixmap *moving_card_pixmap;
gint score;
GtkWidget *score_value;
gint seed;
gchar* game_file;
gchar* game_name;
press_data_type* press_data = NULL; 

/*
 * This function does i18n on game files using the convention that
 * filenames are C locale names with mechanical changes:
 */

gchar* game_file_to_name (const gchar* file)
{
  char* p, *buf = g_strdup (g_filename_pointer(file));

  if ((p = strrchr (buf, '.'))) *p = '\0';
  for(p = buf; p = strchr(p, '_'), p && *p;) *p = ' ';
  /* FIXME : drop next hack by renaming the files */
  buf[0] = toupper(buf[0]);
  p = g_strdup(_(buf));

  g_free(buf);
  return p;
}

void set_score () 
{
  char b [10];
  sprintf (b, "%6d ", score);

  gtk_label_set(GTK_LABEL(score_value), b);
}

void make_title () 
{
  gint old_w, old_h;
  GString* title = g_string_new (game_name);
  g_string_sprintfa (title, "  ( %d )", seed);

  gtk_window_set_title (GTK_WINDOW (app), title->str); 
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
		      (GtkSignalFunc) button_release_event, NULL);
  gtk_signal_connect (GTK_OBJECT (playing_area), "motion_notify_event",
		      (GtkSignalFunc) motion_notify_event, NULL);
  gtk_signal_connect (GTK_OBJECT (playing_area), "button_press_event",
		      (GtkSignalFunc) button_press_event, NULL);
}

void create_main_window ()
{
  app = gnome_app_new ("solitaire", _("Solitaire"));
  gtk_widget_realize (app);

  gtk_signal_connect (GTK_OBJECT(app), "delete_event", 
		      GTK_SIGNAL_FUNC(file_quit_callback), NULL);
  gtk_signal_connect (GTK_OBJECT (app), "configure_event",
		      (GtkSignalFunc) configure_event, NULL);

  gtk_window_set_policy (GTK_WINDOW(app), TRUE, TRUE, FALSE);
}

void create_press_data ()
{
  GdkWindowAttr attributes;

  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.event_mask = 0;
  attributes.width = 1;
  attributes.height = 1;
  attributes.colormap = gdk_window_get_colormap (playing_area->window);
  attributes.visual = gdk_window_get_visual (playing_area->window);
  
  press_data = malloc(sizeof(press_data_type));
  press_data->button_pressed = 0;
  press_data->moving_cards = gdk_window_new(playing_area->window, &attributes,
					    (GDK_WA_VISUAL | GDK_WA_COLORMAP));
}
/*
 * main()
 */

void eval_installed_file (char *file)
{
  char *installed_filename;
  char *relative;
  
  if (g_file_exists (file)){
    gh_eval_file (file);
    return;
  }
  
  relative = g_copy_strings ("sol-games/", file, NULL);
  installed_filename = gnome_datadir_file (relative);
  gh_eval_file (installed_filename);
  g_free (installed_filename);
  g_free (relative);
}

void main_prog(int argc, char *argv[])
{
  GtkWidget *score_label, *score_box;
  
  bindtextdomain (PACKAGE, GNOMELOCALEDIR);
  textdomain (PACKAGE);

  gnome_init ("aisleriot", NULL, argc, argv, 0, NULL);

  seed = time(NULL);
  srandom(seed);

  /* Scheme stuff... */
  /* FIXME: On 1997-11-14, gh_enter stopped loading `icd-9/boot-9.scm'.
     In my copy of guile, the first define in boot-9.scm is for "provide",
     and it looked as good a test as any  */
  gh_eval_str ("(if (not (defined? \'provide))\n"
	       "  (primitive-load-path \"ice-9/boot-9.scm\"))");
  gh_new_procedure0_0("get-card-width", scm_get_card_width);
  gh_new_procedure0_0("get-card-height", scm_get_card_height);
  gh_new_procedure0_0("get-horiz-offset",scm_get_horiz_offset);
  gh_new_procedure0_0("get-vert-offset", scm_get_vert_offset);
  gh_new_procedure0_0("get-horiz-start", scm_get_horiz_start);
  gh_new_procedure0_0("get-vert-start", scm_get_vert_start);
  gh_new_procedure1_0("set-surface-layout", scm_set_surface_layout);
  gh_new_procedure0_0("reset-surface", scm_reset_surface);
  gh_new_procedure1_0("add-slot", scm_add_slot);
  gh_new_procedure1_0("get-slot", scm_get_slot);  
  gh_new_procedure2_0("set-cards!", scm_set_cards);
  gh_new_procedure0_0("get-score", scm_get_score);  
  gh_new_procedure1_0("set-score!", scm_set_score);
  gh_new_procedure1_0("add-to-score!", scm_add_to_score);
  gh_new_procedure("set-lambda", scm_set_lambda, 8, 0, 0);
  gh_new_procedure1_0("random", scm_random);
  eval_installed_file ("sol.scm");

  create_main_window ();

  load_pixmaps (app);
 
  create_sol_board ();
  create_menus (GNOME_APP(app));

  score_box = gtk_hbox_new(0, FALSE);
  score_label = gtk_label_new (_("Score: "));
  gtk_box_pack_start (GTK_BOX(score_box), score_label, FALSE, FALSE, 0);
  score_value = gtk_label_new ("     0 ");
  gtk_box_pack_start (GTK_BOX(score_box), score_value, FALSE, FALSE, 0);
  gtk_widget_show (score_label);
  gtk_widget_show (score_value);
  gtk_widget_show (score_box);

  status_bar = gnome_appbar_new (FALSE, TRUE, TRUE);
  gtk_box_pack_end (GTK_BOX(status_bar), score_box, FALSE, FALSE, 0);
  gnome_app_set_statusbar (GNOME_APP (app), status_bar);

  game_load_game_callback (app, "klondike.scm" );

  gtk_widget_show (app);
  create_press_data ();

  gtk_main ();

  /* clean up (needs some work ): */
  delete_surface();
  free(press_data);
}


int main (int argc, char *argv [])
{
  gh_enter(argc, argv, main_prog);
  return 0;
}
