/* score-dialog.c
   Copyright (C) 1997 Ryu Changwoo

   This program is free software; you can redistribute it and'or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Ryu Changwoo <cwryu@eve.kaist.ac.kr>. */


#include <config.h>
#include <stdlib.h>

#include <gtk/gtk.h>
#include <gnome.h>

#include "score.h"


static void
score_dialog_callback_close(GtkWidget *w, gpointer client_data)
{
  gtk_widget_destroy ((GtkWidget *)client_data);
}

static void
score_dialog_callback_clear(GtkWidget *w, gpointer client_data)
{
  score_clear();
  gtk_widget_destroy ((GtkWidget *)client_data);
}


static gint
score_dialog_callback_delete (GtkWidget *w, GdkEvent *e, gpointer client_data)
{
  gtk_widget_destroy((GtkWidget *)client_data);
  return FALSE;
}


GtkWidget *
score_dialog (void)
{
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *button;
  GtkWidget *hbox;
  GtkWidget *all_boxes;

  char *formatstring[20];	/* FIXME: is it enough? */

  int i;

  dialog = gtk_window_new (GTK_WINDOW_DIALOG);
  gtk_container_border_width (GTK_CONTAINER(dialog), 10);
  GTK_WINDOW(dialog)->position = GTK_WIN_POS_MOUSE;
  gtk_window_set_title(GTK_WINDOW(dialog), _("Score"));
  gtk_signal_connect (GTK_OBJECT (dialog), "delete_event",
                      GTK_SIGNAL_FUNC (score_dialog_callback_delete),
                      dialog);

  all_boxes = gtk_vbox_new (FALSE, 5);
  gtk_container_add (GTK_CONTAINER(dialog), all_boxes);
  
  score_formatstring(formatstring);

  for (i = 0; formatstring[i]; i++)
    {
      label = gtk_label_new (formatstring[i]);
      gtk_box_pack_start(GTK_BOX(all_boxes), label, TRUE, TRUE, 0);
      gtk_widget_show(label);
    }

  hbox = gtk_hbox_new (TRUE, 5);
  gtk_box_pack_start (GTK_BOX(all_boxes), hbox, TRUE, TRUE, 0);

  button = gtk_button_new_with_label (_("OK"));
  gtk_signal_connect (GTK_OBJECT(button), "clicked",
		      GTK_SIGNAL_FUNC(score_dialog_callback_close), dialog);
  gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, TRUE, 5);
  gtk_window_set_focus(GTK_WINDOW(dialog), button);
  gtk_widget_show(button);

  button = gtk_button_new_with_label (_("Clear"));
  gtk_signal_connect (GTK_OBJECT(button), "clicked",
		      GTK_SIGNAL_FUNC(score_dialog_callback_clear), dialog);
  gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, TRUE, 5);
  gtk_widget_show(button);

  gtk_widget_show(hbox);
  gtk_widget_show (all_boxes);
  gtk_widget_show(dialog);

  return dialog;
}


