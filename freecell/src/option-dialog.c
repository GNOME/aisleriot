/* option-dialog.c
   Copyright (C) 1998 Free Software Foundation, Inc.

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
#include <gnome.h>
#include <gtk/gtk.h>
#include "option.h"


typedef struct _OPTION_DATA
{
  GtkWidget *dialog;
  GtkWidget *inform_invalid_move_check;
}
OPTION_DATA;


static void
option_dialog_callback_ok(GtkWidget *w, gpointer client_data)
{
  GtkWidget *c;

  c = ((OPTION_DATA *)client_data)->inform_invalid_move_check;

  option_inform_invalid_move
    = (GTK_TOGGLE_BUTTON(c)->active);
  
  gtk_widget_destroy (((OPTION_DATA *)client_data)->dialog);
  free(client_data);
}

static void
option_dialog_callback_cancel(GtkWidget *w, gpointer client_data)
{
  OPTION_DATA *od;
  GtkWidget *d;

  od = ((OPTION_DATA *)client_data);
  d = od->dialog;
  gtk_widget_destroy (d);
  g_free(od);
}



GtkWidget *
option_dialog (void)
{
  GtkWidget *dialog;
  GtkWidget *all_boxes;
  GtkWidget *frame;
  GtkWidget *box;
  GtkWidget *button;
  GtkWidget *check;

  OPTION_DATA *data;

  
  data = (OPTION_DATA *)g_malloc(sizeof(OPTION_DATA));
  
  dialog = gtk_window_new (GTK_WINDOW_DIALOG);
  data->dialog = dialog;
  gtk_container_border_width (GTK_CONTAINER(dialog), 10);
  GTK_WINDOW(dialog)->position = GTK_WIN_POS_MOUSE;
  gtk_window_set_title(GTK_WINDOW(dialog), _("Freecell Setup"));
  gtk_signal_connect (GTK_OBJECT (dialog), "delete_event",
                      GTK_SIGNAL_FUNC (option_dialog_callback_cancel),
                      (gpointer)data);


  all_boxes = gtk_vbox_new (FALSE, 5);
  gtk_container_add (GTK_CONTAINER(dialog), all_boxes);

  frame = gtk_frame_new (_("Options"));
  gtk_box_pack_start(GTK_BOX(all_boxes), frame, TRUE, TRUE, 0);

  box = gtk_vbox_new (TRUE, 5);
  gtk_container_add (GTK_CONTAINER(frame), box);
  
  check = gtk_check_button_new_with_label (_("Inform invalid move"));
  data->inform_invalid_move_check = check;
  if (option_inform_invalid_move)
    gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(check), TRUE);
  gtk_box_pack_start(GTK_BOX(box), check, TRUE, TRUE, 0);
  gtk_widget_show(check);
  
  gtk_widget_show(box);
  gtk_widget_show (frame);
  
  box = gtk_hbox_new (TRUE, 5);
  gtk_box_pack_start (GTK_BOX(all_boxes), box, TRUE, TRUE, 0);

  button = gtk_button_new_with_label (_("OK"));
  gtk_signal_connect(GTK_OBJECT(button), "clicked",
		     GTK_SIGNAL_FUNC(option_dialog_callback_ok),
		     (gpointer)data);
  gtk_box_pack_start(GTK_BOX(box), button, TRUE, TRUE, 5);
  gtk_widget_show (button);

  button = gtk_button_new_with_label (_("Cancel"));
  gtk_signal_connect(GTK_OBJECT(button), "clicked",
		     GTK_SIGNAL_FUNC(option_dialog_callback_cancel),
		     (gpointer)data);
  gtk_box_pack_start(GTK_BOX(box), button, TRUE, TRUE, 5);
  gtk_widget_show (button);

  gtk_widget_show (box);
  gtk_widget_show (all_boxes);
  gtk_widget_show(dialog);

  return dialog;
}
