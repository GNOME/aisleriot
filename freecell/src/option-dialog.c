/* option-dialog.c --
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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
   USA */

/* Written by Changwoo Ryu <cwryu@adam.kaist.ac.kr>. */

#include <config.h>

#include <gnome.h>
#include "option.h"
#include "io-gtk.h"


static void check1_changed_callback (GtkWidget *check, gpointer data);
static void check2_changed_callback (GtkWidget *check, gpointer data);

void
option_dialog (GtkWidget *parent)
{
  static GtkWidget *propbox = NULL;
  GtkWidget *box;
  GtkWidget *check1;
  GtkWidget *check2;

  if (propbox != NULL) {
    gtk_window_present (GTK_WINDOW(propbox));
    return;
  }

  propbox = gtk_dialog_new_with_buttons (_("Freecell Properties"),
		  GTK_WINDOW (parent),
		  0,
		  GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
		  NULL);
  g_signal_connect (G_OBJECT (propbox), "response",
			G_CALLBACK (gtk_widget_destroy), propbox);
  g_signal_connect (G_OBJECT(propbox), "destroy",
                        G_CALLBACK(gtk_widget_destroyed), &propbox);

  box = gtk_vbox_new (TRUE, 4);

  check1 = gtk_check_button_new_with_label (_("Warn on invalid moves"));
  gtk_box_pack_start_defaults (GTK_BOX (box), check1);
  if (option_inform_invalid_move)
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check1), TRUE);
  g_signal_connect (G_OBJECT (check1), "toggled",
			G_CALLBACK (check1_changed_callback),
			NULL);
  gtk_widget_show(check1);

  check2 = gtk_check_button_new_with_label (_("Move stacks one by one"));
  gtk_box_pack_start_defaults (GTK_BOX (box), check2);
  if (option_move_one_by_one)
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check2), TRUE);
  g_signal_connect (G_OBJECT (check2), "toggled",
		      G_CALLBACK (check2_changed_callback),
		      NULL);
  gtk_widget_show(check2);

  gtk_box_pack_start_defaults (GTK_BOX (GTK_DIALOG (propbox)->vbox), box);

  gtk_widget_show (box);
  gtk_widget_show (propbox);
}


static void
check1_changed_callback (GtkWidget *check, gpointer data)
{
  option_inform_invalid_move = gtk_toggle_button_get_active
			(GTK_TOGGLE_BUTTON (check));
  option_write ();
}


static void
check2_changed_callback (GtkWidget *check, gpointer data)
{
  option_move_one_by_one = gtk_toggle_button_get_active
			(GTK_TOGGLE_BUTTON (check));
  option_write ();
}

