/*
 *  Copyright © 2003 Callum McKenzie <callum@physics.otago.ac.nz>
 *  Copyright © 2007 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope conf it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include "config.h"

#include <string.h>
#include <errno.h>

#include "util.h"
#include "conf.h"

#ifdef HAVE_GNOME
#include <gconf/gconf-client.h>
#endif

#ifdef HAVE_GNOME
static const char key_name[][18] = {
  "card_style",
  "game_file",
  "recent_games_list",
  "show_toolbar",
  "click_to_move",
  "sound",
};

static const char statistics_key[] = "/apps/aisleriot/statistics";

#else
static const char key_name[][12] = {
  "Theme",
  "Variation",
  "Recent",
  "ShowToolbar",
  "ClickToMove",
  "Sound"
};
#endif /* HAVE_GNOME */

#ifdef HAVE_GNOME
  
static GConfClient *gconf_client;
static GHashTable *stats;

static char *
options_gconf_key (const char *game_file)
{
  static const char basekey[] = "/apps/aisleriot/rules/";

  return g_strconcat (basekey, game_file, NULL);
}

static void
load_statistics (void)
{
  GSList *raw_list;
  AisleriotStatistic *new_stats;
  guint64 value;

  raw_list = gconf_client_get_list (gconf_client, statistics_key,
				    GCONF_VALUE_STRING, NULL);

  while (raw_list) {
    new_stats = g_hash_table_lookup (stats, raw_list->data);

    if (!new_stats) {
      new_stats = g_new (AisleriotStatistic, 1);
      new_stats->wins = 0;
      new_stats->total = 0;
      new_stats->best = 0;
      new_stats->worst = 0;
      g_hash_table_insert (stats, raw_list->data, new_stats);
    } else {
      g_free (raw_list->data);
    }

    raw_list = g_slist_delete_link (raw_list, raw_list);

    if (!raw_list)
      break;
    new_stats->wins = g_ascii_strtoull (raw_list->data, NULL, 10);
    g_free (raw_list->data);
    raw_list = g_slist_delete_link (raw_list, raw_list);

    if (!raw_list)
      break;
    new_stats->total = g_ascii_strtoull (raw_list->data, NULL, 10);
    g_free (raw_list->data);
    raw_list = g_slist_delete_link (raw_list, raw_list);

    if (!raw_list)
      break;
    value = g_ascii_strtoull (raw_list->data, NULL, 10);
    /* Sanitise value to fix statistics from bug #474615 */
    if (value > 0 && value <= 6000) {
      new_stats->best = value;
    } else {
      new_stats->best = 0;
    }
    g_free (raw_list->data);
    raw_list = g_slist_delete_link (raw_list, raw_list);

    if (!raw_list)
      break;
    value = g_ascii_strtoull (raw_list->data, NULL, 10);
    /* Sanitise value to fix statistics from bug #474615 */
    if (value > 0 && value <= 6000) {
      new_stats->worst = value;
    } else {
      new_stats->worst = 0;
    }
    g_free (raw_list->data);
    raw_list = g_slist_delete_link (raw_list, raw_list);
  }
}

static void
save_single_stat (char *name,
                  AisleriotStatistic *entry,
                  GSList **list)
{
  /* Everything is pushed onto the list in reverse order. */
  *list = g_slist_prepend (*list, g_strdup_printf ("%d", entry->worst));
  *list = g_slist_prepend (*list, g_strdup_printf ("%d", entry->best));
  *list = g_slist_prepend (*list, g_strdup_printf ("%d", entry->total));
  *list = g_slist_prepend (*list, g_strdup_printf ("%d", entry->wins));
  *list = g_slist_prepend (*list, g_strdup (name));
}

static void
save_statistics (void)
{
  GSList *stats_list = NULL;

  g_hash_table_foreach (stats, (GHFunc) save_single_stat, &stats_list);

  gconf_client_set_list (gconf_client, statistics_key,
                         GCONF_VALUE_STRING, stats_list, NULL);

  g_slist_foreach (stats_list, (GFunc) g_free, NULL);
  g_slist_free (stats_list);
}

#endif /* HAVE_GNOME */

void
aisleriot_conf_init (void)
{
  if (!games_conf_initialise ("Aisleriot")) {
    /* Set defaults */
    games_conf_set_boolean (NULL, aisleriot_conf_get_key (CONF_SHOW_TOOLBAR), TRUE);

#ifdef HAVE_HILDON
    games_conf_set_boolean (NULL, aisleriot_conf_get_key (CONF_CLICK_TO_MOVE), TRUE);
#endif
  }

#ifdef HAVE_GNOME
  
  gconf_client = gconf_client_get_default ();

  stats = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);

  load_statistics ();
  gconf_client_notify_add (gconf_client, statistics_key,
			   (GConfClientNotifyFunc) load_statistics,
			   NULL, NULL, NULL);

#endif /* HAVE_GNOME */
}

void
aisleriot_conf_shutdown (void)
{
#ifdef HAVE_GNOME
  g_object_unref (gconf_client);
  gconf_client = NULL;

  g_hash_table_destroy (stats);
  stats = NULL;
#endif /* HAVE_GNOME */

  games_conf_shutdown ();
}

const char *
aisleriot_conf_get_key (AisleriotConfKey key)
{
  return key_name[key];
}

gboolean
aisleriot_conf_get_options (const char *game_file,
                            int *options)
{
#ifdef HAVE_GNOME
  GConfEntry *entry;
  GConfValue *value;
  char *gconf_key;

  gconf_key = options_gconf_key (game_file);
  entry = gconf_client_get_entry (gconf_client, gconf_key, NULL, TRUE, NULL);
  g_free (gconf_key);
  if (!entry)
    return FALSE;

  value = gconf_entry_get_value (entry);
  if (!value ||
      value->type != GCONF_VALUE_INT) {
    gconf_entry_unref (entry);
    return FALSE;
  }

  *options = gconf_value_get_int (value);
  gconf_entry_unref (entry);

  return TRUE;
#else
  GError *error = NULL;

  *options = games_conf_get_integer (game_file, "Options", &error);
  if (error) {
    g_error_free (error);
    return FALSE;
  }

  return TRUE;
#endif /* HAVE_GNOME */
}

void
aisleriot_conf_set_options (const char *game_file,
                            int value)
{
#ifdef HAVE_GNOME
  GConfSchema *schema;
  char *gconf_key, *schemas_key;

  gconf_key = options_gconf_key (game_file);

  schemas_key = g_strconcat ("/schemas", gconf_key, NULL);

  /* Check if we have a schema for this key, and make one if we don't */
  schema = gconf_client_get_schema (gconf_client, schemas_key, NULL);
  if (!schema) {
    GConfValue *def;

    schema = gconf_schema_new ();
    gconf_schema_set_type (schema, GCONF_VALUE_INT);
    gconf_schema_set_owner (schema, "aisleriot");
    /* FIXME: Translation - how? */
    gconf_schema_set_short_desc (schema, "A per-game option");
    gconf_schema_set_long_desc (schema,
                                "An integer encoding a list of boolean values (LSB = first item) for use as options in a solitaire game.");

    def = gconf_value_new (GCONF_VALUE_INT);
    /* Not entirely correct, but there's no way to get the default options from
     * the game, and setting this to 0 makes the game options be incorrect.
     */
    gconf_value_set_int (def, value);
    gconf_schema_set_default_value_nocopy (schema, def);

    gconf_client_set_schema (gconf_client, schemas_key, schema, NULL);
    gconf_engine_associate_schema (gconf_engine_get_default (), gconf_key, schemas_key, NULL);
  }
  gconf_schema_free (schema);
  g_free (schemas_key);

  gconf_client_set_int (gconf_client, gconf_key, value, NULL);
  g_free (gconf_key);
#else
  games_conf_set_integer (game_file, "Options", value);
#endif /* HAVE_GNOME */
}

void
aisleriot_conf_get_statistic (const char *game_file,
                              AisleriotStatistic *statistic)
{
#ifdef HAVE_GNOME
  AisleriotStatistic *game_stat;

  game_stat = g_hash_table_lookup (stats, game_file);
  if (game_stat) {
    *statistic = *game_stat;
  } else {
    memset (statistic, 0, sizeof (AisleriotStatistic));
  }

#else

  int *values;
  gsize len = 0;
  GError *error = NULL;

  memset (statistic, 0, sizeof (AisleriotStatistic));

  values = games_conf_get_integer_list (game_file, "Statistic", &len, &error);
  if (error) {
    g_error_free (error);
    return;
  }
  if (len != 4) {
    g_free (values);
    return;
  }

  statistic->wins = values[0];
  statistic->total = values[1];
  /* Sanitise values to fix statistics from bug #474615 */
  if (values[2] > 0 && values[2] <= 6000) {
    statistic->best = values[2];
  } else {
    statistic->best = 0;
  }
  if (values[3] > 0 && values[3] <= 6000) {
    statistic->worst = values[3];
  } else {
    statistic->worst = 0;
  }

  g_free (values);
#endif /* HAVE_GNOME */
}

void
aisleriot_conf_set_statistic (const char *game_file,
                              AisleriotStatistic *statistic)
{
#ifdef HAVE_GNOME
  AisleriotStatistic *game_stat;

  game_stat = g_hash_table_lookup (stats, game_file);
  /* Backward compatibility with buggy old aisleriot versions
   * which stored the localised game name.
   */
  if (!game_stat) {
    char *localised_name;

    localised_name = aisleriot_util_get_display_filename (game_file);
    game_stat = g_hash_table_lookup (stats, localised_name);
    g_free (localised_name);
  }

  if (!game_stat) {
    game_stat = g_new0 (AisleriotStatistic, 1);
    g_hash_table_insert (stats, g_strdup (game_file), game_stat);
  }

  *game_stat = *statistic;

  save_statistics ();

#else

  int values[4];

  values[0] = statistic->wins;
  values[1] = statistic->total;
  values[2] = statistic->best;
  values[3] = statistic->worst;

  games_conf_set_integer_list (game_file, "Statistic", values, G_N_ELEMENTS (values));
#endif /* HAVE_GNOME */
}
