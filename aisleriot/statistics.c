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

GSList * stats_list;

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

gboolean
close_statistics_dialog (GtkWidget * widget)
{
  statistics_dialog = NULL;

  return FALSE;
}

static void locate_current_stats (void)
{
  /* Get the current stats from the hash table. Create a new 
   * entry if there are no stats. */
  current_stats = g_hash_table_lookup (stats, game_name);
  if (!current_stats) {
    current_stats = g_new (game_stats, 1);
    current_stats->wins = 0;
    current_stats->total = 0;
    current_stats->best = 0;
    current_stats->worst = 0;
    g_hash_table_insert (stats, g_strdup (game_name), current_stats);
  }
}

static void update_labels (void) {
  gchar * text;

  if (!statistics_dialog)
    return;
  
  text = g_strdup_printf ("%d", current_stats->wins);
  gtk_label_set_text (GTK_LABEL (wins_label), text);
  g_free (text);

  text = g_strdup_printf ("%d", current_stats->total);
  gtk_label_set_text (GTK_LABEL (total_label), text);
  g_free (text);

  if (current_stats->total != 0) {
    text = g_strdup_printf ("% 3d%%", 
			    (100*current_stats->wins)/current_stats->total);
    gtk_label_set_text (GTK_LABEL (percentage_label), text);
    g_free (text);
  } else 
    /* For translators: N/A means "Not Applicable", use whatever
     * abbreviation you have for a value that has no meaning. */
    gtk_label_set_text (GTK_LABEL (percentage_label), _("N/A"));

  if (current_stats->best != 0) {
    /* Translators: this represents minutes:seconds. */
    text = g_strdup_printf (_("%d:%02d"), current_stats->best / 60,
			    current_stats->best % 60);
    gtk_label_set_text (GTK_LABEL (best_label), text);
    g_free (text);
  } else
    gtk_label_set_text (GTK_LABEL (best_label), _("N/A"));    

  if (current_stats->worst != 0) {
    /* Translators: this represents minutes:seconds. */
    text = g_strdup_printf (_("%d:%02d"), current_stats->worst / 60,
			    current_stats->worst % 60);
    gtk_label_set_text (GTK_LABEL (worst_label), text);
    g_free (text);
  } else
    gtk_label_set_text (GTK_LABEL (worst_label), _("N/A"));    
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

  locate_current_stats ();

  update_labels ();
}

static void save_single_stat (gchar * name, game_stats * entry, gpointer data)
{
  /* Everything is pushed onto the list in reverse order. */
  stats_list = g_slist_prepend (stats_list, 
				g_strdup_printf ("%d",entry->worst));
  stats_list = g_slist_prepend (stats_list, 
				g_strdup_printf ("%d",entry->best));
  stats_list = g_slist_prepend (stats_list, 
				g_strdup_printf ("%d",entry->total));
  stats_list = g_slist_prepend (stats_list, 
				g_strdup_printf ("%d",entry->wins));
  stats_list = g_slist_prepend (stats_list, g_strdup (name));
}

static void save_statistics (void)
{
  if (!stats)
    return;

  stats_list = NULL;

  g_hash_table_foreach (stats, (GHFunc) save_single_stat, NULL);

  gconf_client_set_list (gconf_client, STATISTICS_KEY, GCONF_VALUE_STRING,
			 stats_list, NULL);

  g_slist_foreach (stats_list, (GFunc)g_free, NULL);
  g_slist_free (stats_list);
}

void load_statistics (void)
{
  GSList * raw_list;
  game_stats * new_stats;

  raw_list = gconf_client_get_list (gconf_client, STATISTICS_KEY, 
				      GCONF_VALUE_STRING, NULL);
  
  if (!stats) {
    stats = g_hash_table_new (g_str_hash, g_str_equal);    
  }

  while (raw_list) {
    new_stats = g_hash_table_lookup (stats, raw_list->data);

    if (!new_stats) {
      new_stats = g_new (game_stats, 1);
      new_stats->wins = 0; new_stats->total = 0;
      new_stats->best = 0; new_stats->worst = 0;
      g_hash_table_insert (stats, raw_list->data, new_stats);
    } else {
      g_free (raw_list->data);
    }

    raw_list = g_slist_delete_link (raw_list, raw_list);

    if (!raw_list) break;
    new_stats->wins = g_ascii_strtoull (raw_list->data, NULL, 10);
    g_free (raw_list->data);
    raw_list = g_slist_delete_link (raw_list, raw_list);

    if (!raw_list) break;
    new_stats->total = g_ascii_strtoull (raw_list->data, NULL, 10);
    g_free (raw_list->data);
    raw_list = g_slist_delete_link (raw_list, raw_list);

    if (!raw_list) break;
    new_stats->best = g_ascii_strtoull (raw_list->data, NULL, 10);
    g_free (raw_list->data);
    raw_list = g_slist_delete_link (raw_list, raw_list);

    if (!raw_list) break;
    new_stats->worst = g_ascii_strtoull (raw_list->data, NULL, 10);
    g_free (raw_list->data);
    raw_list = g_slist_delete_link (raw_list, raw_list);
  }

  /* We do the whole thing again because we may need to reset the
   * current_stats pointer. */
  update_statistics_display ();
}

/* Yes, there is a gconf race condition here, but since this is all
   per user they're going to have to be either playing impossibly fast
   or using sharing an account. In the later case they won't be caring
   about the statistics. */
void update_statistics (gboolean won, guint time)
{
  locate_current_stats ();

  current_stats->total++;
  if (won) {
    current_stats->wins++;
    if (time > 0) {
      if ((current_stats->best == 0) || (time < current_stats->best))
	current_stats->best = time;
      if (time > current_stats->worst)
	current_stats->worst = time;
    }
  }

  save_statistics ();
  update_labels ();
}

void 
statistics_dialog_response (GtkWidget * widget, gint response, gpointer data)
{
  switch (response) {
  case GTK_RESPONSE_CLOSE:
    gtk_widget_hide (statistics_dialog);
    break;
  case GTK_RESPONSE_REJECT:
    current_stats->wins = 0;
    current_stats->total = 0;
    current_stats->best = 0;
    current_stats->worst = 0;
    save_statistics ();
    update_labels ();
    break;
  }
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
