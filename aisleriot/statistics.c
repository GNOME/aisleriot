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

typedef struct _game_stats {
  guint wins;
  guint total;
  guint best;
  guint worst;
} game_stats;

GHashTable * stats = NULL;
game_stats * current_stats;

GtkWidget * statistics_dialog = NULL;
GtkWidget * name_label;
GtkWidget * wins_label;
GtkWidget * total_label;
GtkWidget * percentage_label;
GtkWidget * best_label;
GtkWidget * worst_label;

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
  gchar * text;

  if (!statistics_dialog || !GTK_WIDGET_VISIBLE (statistics_dialog))
    return;

  text = g_strdup_printf ("<b>%s</b>", game_name);
  gtk_label_set_text (GTK_LABEL (name_label), text);
  gtk_label_set_use_markup (GTK_LABEL (name_label), TRUE);
  g_free (text);

  if (!stats) {
    stats = g_hash_table_new (g_str_hash, g_str_equal);
  }

  /* Get the current stats from the hash table. Create a new 
   * entry if there are no stats. */
  current_stats = g_hash_table_lookup (stats, game_name);
  if (!current_stats) {
    current_stats = g_new (game_stats, 1);
    current_stats->wins = 0;
    current_stats->total = 0;
    current_stats->best = 0;
    current_stats->worst = 0;
    g_hash_table_insert (stats, game_name, current_stats);
  }

  text = g_strdup_printf ("%d", current_stats->wins);
  gtk_label_set_text (GTK_LABEL (wins_label), text);
  g_free (text);

  text = g_strdup_printf ("%d", current_stats->total);
  gtk_label_set_text (GTK_LABEL (total_label), text);
  g_free (text);

  if (current_stats->total != 0)
    text = g_strdup_printf ("% 3d%%", 
			    (100*current_stats->wins)/current_stats->total);
  else
    /* For translators: N/A means "Not Applicable", use whatever
     * abbreviation you have for a value that has no meaning. */
    text = g_strdup (_("N/A"));
  gtk_label_set_text (GTK_LABEL (percentage_label), text);
  g_free (text);

  /* FIXME: Make these two pritn actual times. */
  text = g_strdup_printf ("%d", current_stats->best);
  gtk_label_set_text (GTK_LABEL (best_label), text);
  g_free (text);

  text = g_strdup_printf ("%d", current_stats->worst);
  gtk_label_set_text (GTK_LABEL (worst_label), text);
  g_free (text);


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
    wins_label = glade_xml_get_widget (dialog, "wins_label");
    total_label = glade_xml_get_widget (dialog, "total_label");
    percentage_label = glade_xml_get_widget (dialog, "percentage_label");
    best_label = glade_xml_get_widget (dialog, "best_label");
    worst_label = glade_xml_get_widget (dialog, "worst_label");
  } else {
    gtk_window_present (GTK_WINDOW (statistics_dialog));
  }

  update_statistics_display ();
}
