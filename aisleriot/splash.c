/* AisleRiot - splash.c
 * Copyright (C) 1998 Felix Bellaby <felix@pooh.u-net.com>
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

#include "splash.h"
#include <gnome.h>
#include <libgnomeui/gnome-window-icon.h>

static GtkWidget* progress = NULL;
static GtkWidget* label = NULL;
static GtkWidget* splash = NULL;
static gboolean waiting_for_expose = FALSE;

void 
splash_update (gchar* text, gfloat percent)
{
	if (label != NULL &&
	    progress != NULL) {
		gtk_label_set (GTK_LABEL (label), text);
		gtk_progress_set_percentage (GTK_PROGRESS (progress), percent); 
		while (gtk_events_pending ())
			gtk_main_iteration ();
	}
}

static void
splash_destroyed (GtkWidget *w)
{
	splash = NULL;
	label = NULL;
	progress = NULL;
	if (waiting_for_expose) {
		waiting_for_expose = FALSE;
		gtk_main_quit ();
	}
}

static gboolean
expose_event (GtkWidget *w, GdkEventExpose *event)
{
	if (waiting_for_expose) {
		waiting_for_expose = FALSE;
		gtk_main_quit ();
	}
}


void 
splash_new ()
{
  gchar* image_file;
  GtkWidget* splash_pixmap = NULL;
  GtkWidget* vbox;

  image_file = gnome_pixmap_file ("cards/splash.png");

  if (image_file != NULL)
	  splash_pixmap = gnome_pixmap_new_from_file (image_file);

  g_free (image_file);

  vbox = gtk_vbox_new(FALSE, GNOME_PAD);

  progress = gtk_progress_bar_new ();
  label = gtk_label_new ("");

  splash = gtk_window_new(gnome_preferences_get_dialog_type());
  gtk_window_position (GTK_WINDOW(splash), 
		       GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (splash), _("AisleRiot"));
  gtk_window_set_policy (GTK_WINDOW (splash), FALSE, FALSE, FALSE);
  gnome_window_icon_set_from_default (GTK_WINDOW (splash));
  gtk_signal_connect (GTK_OBJECT (splash), "destroy",
		      GTK_SIGNAL_FUNC (splash_destroyed),
		      NULL);
  
  gtk_container_border_width (GTK_CONTAINER (vbox), 0);
  gtk_container_add(GTK_CONTAINER(splash), vbox);

  if (splash_pixmap != NULL)
	  gtk_box_pack_start (GTK_BOX (vbox), splash_pixmap, FALSE, FALSE, 
			      GNOME_PAD_SMALL);
  gtk_box_pack_end (GTK_BOX (vbox), progress, FALSE, FALSE, GNOME_PAD_SMALL);
  gtk_box_pack_end (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  
  gtk_widget_show_all (splash);

  /* Give window manager time to map the window */
  if (splash_pixmap != NULL) {
	  gtk_signal_connect (GTK_OBJECT (splash_pixmap), "expose_event",
			      GTK_SIGNAL_FUNC (expose_event), NULL);
	  waiting_for_expose = TRUE;
	  gtk_main ();
	  waiting_for_expose = FALSE;
  }
}

void 
splash_destroy ()
{
	if (splash != NULL) {
		gtk_widget_hide (splash);
		gtk_widget_destroy (splash);
	}
}

