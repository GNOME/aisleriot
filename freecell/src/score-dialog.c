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
score_dialog_callback(GnomeDialog *dialog, gint button, gpointer data)
{
  if (button == 1)
    score_clear();
}

GtkWidget *
score_dialog (void)
{
  GtkWidget *dialog;
  GtkWidget *label;

  char *formatstring[20];	/* FIXME: is it enough? */
  int i;

  dialog = gnome_dialog_new (_("Score"), GNOME_STOCK_BUTTON_OK, NULL);
  gnome_dialog_append_button_with_pixmap (GNOME_DIALOG (dialog), _("Clear"),
					  GNOME_STOCK_PIXMAP_CLEAR);
  gnome_dialog_set_default (GNOME_DIALOG (dialog), 0);
  gnome_dialog_set_close (GNOME_DIALOG (dialog), TRUE);

  gtk_signal_connect (GTK_OBJECT (dialog), "clicked",
	              GTK_SIGNAL_FUNC (score_dialog_callback), NULL);
  
  score_formatstring(formatstring);
  for (i = 0; formatstring[i]; i++)
    {
      label = gtk_label_new (formatstring[i]);
      gtk_box_pack_start (GTK_BOX(GNOME_DIALOG(dialog)->vbox),
			  label, TRUE, TRUE, 0);
      gtk_widget_show(label);
    }
  gtk_widget_show (dialog);

  return dialog;
}

