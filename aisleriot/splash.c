/* Aisleriot - sol.c
 * Copyright (C) 1998 Felix Bellaby <felix@pooh.u-net.com>
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

#include "splash.h"
#include <gnome.h>

GtkWidget* progress;
GtkWidget* label;
GtkWidget* splash;

void 
splash_update (gchar* text, gfloat percent)
{
  gtk_label_set (GTK_LABEL (label), text);
  gtk_progress_set_percentage (GTK_PROGRESS (progress), percent); 
  while (gtk_events_pending ()) gtk_main_iteration ();
}

void 
splash_new ()
{
  gchar* image_file = gnome_pixmap_file ("cards/splash.png");
  GtkWidget* splash_pixmap = gnome_pixmap_new_from_file (image_file);
  GtkWidget* vbox = gtk_vbox_new(FALSE, GNOME_PAD);

  progress = gtk_progress_bar_new ();
  label = gtk_label_new ("");

  splash = gtk_window_new(gnome_preferences_get_dialog_type());
  gtk_window_position (GTK_WINDOW(splash), 
		       GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (splash), _("Aisleriot"));
  gtk_window_set_policy (GTK_WINDOW (splash), FALSE, FALSE, FALSE);
  
  gtk_container_border_width (GTK_CONTAINER (vbox), 0);
  gtk_container_add(GTK_CONTAINER(splash), vbox);

  gtk_box_pack_start (GTK_BOX (vbox), splash_pixmap, FALSE, FALSE, 
		      GNOME_PAD_SMALL);
  gtk_box_pack_end (GTK_BOX (vbox), progress, FALSE, FALSE, GNOME_PAD_SMALL);
  gtk_box_pack_end (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  
  gtk_widget_show_all (splash);

  /* Give window manager time to map the window */
  gtk_signal_connect (GTK_OBJECT (splash_pixmap), "expose_event",
		      GTK_SIGNAL_FUNC (gtk_main_quit), NULL);
  gtk_main ();
}

void 
splash_destroy ()
{
  gtk_widget_hide (splash);
  gtk_widget_destroy (splash);
}

