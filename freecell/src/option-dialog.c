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


typedef struct _OPTION_DATA
{
  GtkWidget *inform_invalid_move_check;
  GtkWidget *move_one_by_one_check;
  
}
OPTION_DATA;

static void option_dialog_apply_callback (GtkWidget *w, gpointer data1, gpointer data2);
static void option_dialog_changed_callback (GtkWidget *w, gpointer data);
static gint option_dialog_close_callback (GtkWidget *w, gpointer data1, gpointer data2);

GtkWidget *
option_dialog (void)
{
  GtkWidget *propbox;
  GtkWidget *box;
  GtkWidget *check;
  GtkWidget *label;
  OPTION_DATA *option_data;

  option_data = (OPTION_DATA *) g_malloc (sizeof (OPTION_DATA));
  propbox = gnome_property_box_new ();
  gtk_window_set_title (GTK_WINDOW(&GNOME_PROPERTY_BOX(propbox)->dialog.window),
			_("Freecell Properties"));

  /* the first option frame. */
  box = gtk_vbox_new (TRUE, 4);

  check = gtk_check_button_new_with_label (_("Warn on invalid moves"));
  option_data->inform_invalid_move_check = check;
  gtk_box_pack_start_defaults (GTK_BOX (box), check);
  if (option_inform_invalid_move)
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), TRUE);
  gtk_signal_connect (GTK_OBJECT (check), "toggled",
		      GTK_SIGNAL_FUNC (option_dialog_changed_callback),
		      propbox);
  gtk_widget_show(check);

  check = gtk_check_button_new_with_label (_("Move stacks one by one"));
  option_data->move_one_by_one_check = check;
  gtk_box_pack_start_defaults (GTK_BOX (box), check);
  if (option_move_one_by_one)
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), TRUE);
  gtk_signal_connect (GTK_OBJECT (check), "toggled",
		      GTK_SIGNAL_FUNC (option_dialog_changed_callback),
		      propbox);
  gtk_widget_show(check);

  gtk_widget_show (box);

  label = gtk_label_new (_("Options"));
  gnome_property_box_append_page (GNOME_PROPERTY_BOX (propbox), box, label);

  gtk_signal_connect (GTK_OBJECT (propbox), "delete_event",
		      GTK_SIGNAL_FUNC (option_dialog_close_callback),
		      option_data);
  gtk_signal_connect (GTK_OBJECT (propbox), "apply",
		      GTK_SIGNAL_FUNC (option_dialog_apply_callback),
		      option_data);
  return propbox;
}


static void
option_dialog_apply_callback (GtkWidget *w, gpointer data1, gpointer data2)
{
  GtkWidget *check;

  g_return_if_fail (data2 != NULL);

  switch (GPOINTER_TO_INT (data1))
    {
    case 0:
      check = ((OPTION_DATA *)data2)->inform_invalid_move_check;
      option_inform_invalid_move = GTK_TOGGLE_BUTTON (check)->active;
      check = ((OPTION_DATA *)data2)->move_one_by_one_check;
      option_move_one_by_one = GTK_TOGGLE_BUTTON (check)->active;
      break;
    default:
      break;
    }
}

static void
option_dialog_changed_callback (GtkWidget *w, gpointer data)
{
  g_return_if_fail (data != NULL);

  gnome_property_box_changed (GNOME_PROPERTY_BOX(data));
}

static gint
option_dialog_close_callback (GtkWidget *w, gpointer data1, gpointer data2)
{
  if(data2)
    g_free (data2);
  return FALSE;
}


