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

#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <dirent.h>
#include <libguile.h>
#include <gdk_imlib.h>
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

GtkWidget *scorew;
GtkWidget *app, *playing_area,  *vb;
GtkMenuFactory *mf;
GdkPixmap *surface, *blank_surface;
GdkPixmap *snapshot = NULL;
GdkPixmap *moving_card_pixmap;
gint score;

press_data_type* press_data = NULL; 


/* paint functions */




void create_sol_board ()
{
#ifdef DEBUG
printf("create_sol_board\n");
#endif
  /* Here we create the actual playing surface */
  playing_area = gtk_drawing_area_new ();
  gtk_widget_set_events (playing_area, gtk_widget_get_events (playing_area) | GAME_EVENTS);
  
  gtk_box_pack_start_defaults (GTK_BOX(vb), playing_area);

  gtk_widget_realize (playing_area);

  /* Setup the surface */
  gtk_drawing_area_size (GTK_DRAWING_AREA (playing_area),
								 SURFACE_WIDTH,
								 SURFACE_HEIGHT);

  /* Set up the pixmaps */
  surface = gdk_pixmap_new (playing_area->window, SURFACE_WIDTH, SURFACE_HEIGHT, gtk_widget_get_visual (playing_area)->depth);

  blank_surface = gdk_pixmap_new (playing_area->window, SURFACE_WIDTH, SURFACE_HEIGHT, gtk_widget_get_visual (playing_area)->depth);

  refresh_screen();
  
  
  /* Set signals for X events... */
  gtk_signal_connect (GTK_OBJECT (playing_area), "expose_event",
							 (GtkSignalFunc) expose_event, NULL);
  gtk_signal_connect (GTK_OBJECT(playing_area),"button_release_event",
							 (GtkSignalFunc) button_release_event, NULL);
  gtk_signal_connect (GTK_OBJECT (playing_area), "motion_notify_event",
							 (GtkSignalFunc) motion_notify_event, NULL);
  gtk_signal_connect (GTK_OBJECT (playing_area), "button_press_event",
							 (GtkSignalFunc) button_press_event, NULL);
  gtk_signal_connect (GTK_OBJECT (app), "configure_event",
							 (GtkSignalFunc) configure_event, NULL);

  /* and, we're off and running... */
  gtk_widget_show (playing_area);

}



/*
 * setup suff
 */

GtkWidget *
create_main_window ()
{
  GtkWidget* retval;
  retval = gnome_app_new ("solitaire", "Solitaire");

  gtk_widget_realize (retval);
  
  gtk_signal_connect (GTK_OBJECT(retval), "delete_event", GTK_SIGNAL_FUNC(file_quit_callback), NULL);
  gtk_window_set_policy (GTK_WINDOW(retval), 1, 1, 1);
  return retval;
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
  GtkWidget *label, *hb;

  printf("Done.\ninitializing gnome/gdk...\n");
  gnome_init (&argc, &argv);
  gdk_imlib_init();
  printf("Done.\n");
  
  /* generic startup... */
  printf("Creating App...\n");
  srandom(time(NULL));
  app = create_main_window ();
  vb = gtk_vbox_new (FALSE, 0);
  hb = gtk_hbox_new (FALSE, 0);
  gnome_app_set_contents (GNOME_APP (app), vb);
  press_data = malloc(sizeof(press_data_type));

  /* load files as needed */
  printf("loading pixmaps...\n");
  load_pixmaps(app);
  printf("Done.\n");
  /* Scheme stuff... */
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
  gh_new_procedure("set-lambda", scm_set_lambda, 8, 0, 0);
  gh_new_procedure1_0("random", scm_random);
  eval_installed_file ("sol.scm");
  eval_installed_file ("klondike.scm");
  gh_apply(game_data->start_game_lambda, SCM_EOL);
  create_sol_board();

  /* create the menus */
  mf = create_menu ();

  gnome_app_set_menus (GNOME_APP (app), GTK_MENU_BAR (mf->widget));

  /* create the scoring widget */
  label = gtk_label_new (_("Score: "));
  scorew = gtk_label_new ("0");
  
  /* put everything together */
  gtk_box_pack_start_defaults (GTK_BOX(vb), hb);
  gtk_box_pack_end   (GTK_BOX(hb), scorew, 0, 0, 10);
  gtk_box_pack_end   (GTK_BOX(hb), label,  0, 0, 0);
  
  /* and we're up...*/
  gtk_widget_show (app);
  gtk_widget_show (hb);
  gtk_widget_show (vb);
  gtk_widget_show (GTK_WIDGET(label));
  gtk_widget_show (GTK_WIDGET(scorew));

  /* ...and running */
  gtk_main ();

  /* clean up (needs some work ): */
  printf("cleaning up...\n");
  delete_surface();
  free(press_data);
}


int main (int argc, char *argv [])
{
  printf("starting guile...\n");
  gh_enter(argc, argv, main_prog);
  return 0;
}
