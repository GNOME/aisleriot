/* score-dialog.c --
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
#include <stdlib.h>

#include <gtk/gtk.h>
#include <gnome.h>

#include "score.h"


static void
score_dialog_callback(GtkDialog *dialog, gint arg1, gpointer data)
{
  if (arg1 == GTK_RESPONSE_REJECT)
    score_clear();
  else
	  gtk_widget_destroy (GTK_WIDGET(dialog));
}

GtkWidget *
score_dialog (void)
{
  GtkWidget *dialog;
  GtkWidget *label;

  char *formatstring[20];	/* FIXME: is it enough? */
  int i;

  dialog = gtk_dialog_new_with_buttons (_("Score"),
		  NULL,
		  0,
		  GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
		  NULL);
		  
  gtk_dialog_add_buttons (GTK_DIALOG (dialog), GTK_STOCK_CLEAR,
		  GTK_RESPONSE_REJECT, NULL);
  
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);

  g_signal_connect (GTK_OBJECT (dialog), "response",
	              GTK_SIGNAL_FUNC (score_dialog_callback), NULL);
  
  score_formatstring(formatstring);
  for (i = 0; formatstring[i]; i++)
    {
      label = gtk_label_new (formatstring[i]);
      gtk_box_pack_start (GTK_BOX(GTK_DIALOG(dialog)->vbox),
			  label, TRUE, TRUE, 0);
      gtk_widget_show(label);
    }
  gtk_widget_show (dialog);

  return dialog;
}

