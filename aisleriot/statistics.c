/* AisleRiot - statistics.c
 * Copyright (C) 2003 Callum McKenzie <callum@physics.otago.ac.nz>
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

#include <gnome.h>
#include <glade/glade.h>

#include "sol.h"
#include "statistics.h"

GtkWidget * statistics_dialog = NULL;
GtkWidget * name_label;

/* To remove some warnings, these are linked in at run time by libglade. */
gboolean close_statistics_dialog (GtkWidget * widget);
void statistics_dialog_response (GtkWidget * widget, gint response, gpointer data);

void 
statistics_dialog_response (GtkWidget * widget, gint response, gpointer data)
{
  switch (response) {
  case GTK_RESPONSE_CLOSE:
    gtk_widget_hide (statistics_dialog);
    break;
  case GTK_RESPONSE_REJECT:
    g_print ("Clearing statistics\n");
    break;
  }
}

gboolean
close_statistics_dialog (GtkWidget * widget)
{
  statistics_dialog = NULL;

  return FALSE;
}

void update_statistics_display (void)
{
  gchar * name;

  if (!statistics_dialog || !GTK_WIDGET_VISIBLE (statistics_dialog))
    return;

  name = g_strdup_printf ("<b>%s</b>", game_name);
  gtk_label_set_text (GTK_LABEL (name_label), name);
  gtk_label_set_use_markup (GTK_LABEL (name_label), TRUE);
  g_free (name);
}

void
show_statistics_dialog (void)
{
  GladeXML * dialog;
  
  if (!statistics_dialog) {
    dialog = glade_xml_new (GLADEDIR "/statistics.glade", NULL, NULL);
    glade_xml_signal_autoconnect (dialog);
    statistics_dialog = glade_xml_get_widget (dialog, "Statistics");
    name_label = glade_xml_get_widget (dialog, "name_label");
  } else {
    gtk_window_present (GTK_WINDOW (statistics_dialog));
  }

  update_statistics_display ();
}
