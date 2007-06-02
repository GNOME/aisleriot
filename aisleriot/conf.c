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
 *
 *  $Id$
 */

#include "config.h"

#include <string.h>
#include <errno.h>

#include "util.h"
#include "conf.h"

#ifdef HAVE_GNOME
#include <gconf/gconf-client.h>
#include <libgames-support/games-gconf.h>
#include <libgnomeui/gnome-app-helper.h>
#else
#include <glib/gkeyfile.h>
#include <gtk/gtkaccelmap.h>

#define CONFIG_FILE   "aisleriot"
#define ACCELMAP_FILE "aisleriot.accels"

#endif

#ifdef HAVE_GNOME

static const char gconf_key[][34] = {
  "/apps/aisleriot/card_style",
  "/apps/aisleriot/game_file",
  "/apps/aisleriot/width",
  "/apps/aisleriot/height",
  "/apps/aisleriot/maximized",
  "/apps/aisleriot/fullscreen",
  "/apps/aisleriot/recent_games_list",
  "/apps/aisleriot/show_toolbar",
  "/apps/aisleriot/click_to_move",
  "/apps/aisleriot/statistics",
};

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

  raw_list = gconf_client_get_list (gconf_client, gconf_key[CONF_STATISTICS],
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
    new_stats->best = g_ascii_strtoull (raw_list->data, NULL, 10);
    g_free (raw_list->data);
    raw_list = g_slist_delete_link (raw_list, raw_list);

    if (!raw_list)
      break;
    new_stats->worst = g_ascii_strtoull (raw_list->data, NULL, 10);
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

  gconf_client_set_list (gconf_client, gconf_key[CONF_STATISTICS],
                         GCONF_VALUE_STRING, stats_list, NULL);

  g_slist_foreach (stats_list, (GFunc) g_free, NULL);
  g_slist_free (stats_list);
}

#else /* !HAVE_GNOME */

static GKeyFile *key_file;

static const char CONFIG_GROUP[] = "Aisleriot Config";

static const char key_name[][12] = {
  "Theme",
  "Variation",
  "Width",
  "Height",
  "Maximised",
  "Fullscreen",
  "Recent",
  "ShowToolbar",
  "ClickToMove"
};

#endif /* HAVE_GNOME */

void
aisleriot_conf_init (void)
{
#ifdef HAVE_GNOME
  gconf_client = gconf_client_get_default ();

  gconf_client_add_dir (gconf_client, "/apps/aisleriot",
                        GCONF_CLIENT_PRELOAD_NONE, NULL);

  stats = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);

  load_statistics ();
  gconf_client_notify_add (gconf_client, gconf_key[CONF_STATISTICS],
			   (GConfClientNotifyFunc) load_statistics,
			   NULL, NULL, NULL);

#else /* !HAVE_GNOME */

  char *conf_file;
  GError *error = NULL;

  conf_file = g_build_filename (g_get_user_config_dir (), "gnome-games", CONFIG_FILE, NULL);

  key_file = g_key_file_new ();
  if (!g_key_file_load_from_file (key_file, conf_file, 0, &error)) {
    /* Don't warn on non-existent file */
    if (error->domain != G_FILE_ERROR ||
        error->code != G_FILE_ERROR_NOENT) {
      g_warning ("Failed to read settings from \"%s\": %s",
                  conf_file, error->message);
    }

    g_error_free (error);

    /* This will leave us with a half-initialised key file, but no matter */

    /* Set defaults */
    aisleriot_conf_set_boolean (CONF_SHOW_TOOLBAR, TRUE);

#ifdef HAVE_MAEMO
    aisleriot_conf_set_boolean (CONF_CLICK_TO_MOVE, TRUE);
#endif
  }

  g_free (conf_file);

  /* Load the accel map, which libgnome does for us in the HAVE_GNOME case */
#ifndef HAVE_MAEMO
  conf_file = g_build_filename (g_get_user_config_dir(), "gnome-games", ACCELMAP_FILE, NULL);
  gtk_accel_map_load (conf_file);
  g_free (conf_file);
#endif /* !HAVE_MAEMO */

#endif /* HAVE_GNOME */
}

void
aisleriot_conf_shutdown (void)
{
#ifdef HAVE_GNOME
  gconf_client_remove_dir (gconf_client, "/apps/aisleriot", NULL);

  g_object_unref (gconf_client);
  gconf_client = NULL;

  g_hash_table_destroy (stats);
  stats = NULL;

  /* Save the accel map */
  gnome_accelerators_sync ();

#else /* !HAVE_GNOME */

  char *conf_file, *conf_dir = NULL, *data = NULL;
  gsize len = 0;
  GError *error = NULL;

  conf_file = g_build_filename (g_get_user_config_dir (), "gnome-games", CONFIG_FILE, NULL);

  data = g_key_file_to_data (key_file, &len, &error);
  if (!data) {
    g_warning ("Failed to save settings to \"%s\": %s",
               conf_file, error->message);
    g_error_free (error);
    goto loser;
  }
     
  /* Ensure the directory exists */
  conf_dir = g_build_filename (g_get_user_config_dir (), "gnome-games", NULL);
  /* Mode 0700 per the XDG basedir spec */
  if (g_mkdir_with_parents (conf_dir, 0700) < 0) {
    int err = errno;

    if (err != EEXIST) {
      g_warning ("Failed to create config directory \"%s\": %s\n", conf_dir, g_strerror (err));
      goto loser;
    }
  }

  if (!g_file_set_contents (conf_file, data, len, &error)) {
    g_warning ("Failed to save settings to \"%s\": %s",
              conf_file, error->message);
    g_error_free (error);
  }

loser:
  g_free (data);
  g_free (conf_file);
  g_free (conf_dir);

  /* Save the accel map */
#ifndef HAVE_MAEMO
  conf_file = g_build_filename (g_get_user_config_dir(), "gnome-games", ACCELMAP_FILE, NULL);
  gtk_accel_map_save (conf_file);
  g_free (conf_file);
#endif /* !HAVE_MAEMO */

#endif /* HAVE_GNOME */
}

char *
aisleriot_conf_get_string (AisleriotConfKey key,
                           const char *default_value)
{
  char *value;

#ifdef HAVE_GNOME
  value = games_gconf_get_string (gconf_client, gconf_key[key], default_value);
#else
  GError *error = NULL;

  value = g_key_file_get_string (key_file, CONFIG_GROUP, key_name[key], &error);
  if (error) {
    value = NULL;
    g_error_free (error);
  }
#endif /* HAVE_GNOME */

  return value ? value : g_strdup (default_value);
}

void
aisleriot_conf_set_string (AisleriotConfKey key,
                           const char *value)
{
#ifdef HAVE_GNOME
  gconf_client_set_string (gconf_client, gconf_key[key], value, NULL);
#else
  g_key_file_set_string (key_file, CONFIG_GROUP, key_name[key], value);
#endif /* HAVE_GNOME */
}

char **
aisleriot_conf_get_strings (AisleriotConfKey key,
                            gsize *n_values)
{
#ifdef HAVE_GNOME
  GSList *list, *l;
  char **values = NULL;
  gsize n = 0;

  list = gconf_client_get_list (gconf_client, gconf_key[key], GCONF_VALUE_STRING, NULL);
  if (list != NULL) {
    values = g_new (char *, g_slist_length (list) + 1);

    for (l = list; l != NULL; l = l->next) {
      values[n++] = l->data;
    }

    /* NULL termination */
    values[n] = NULL;

    g_slist_free (list); /* the strings themselves are now owned by the array */
  }

  *n_values = n;
  return values;
#else
  return g_key_file_get_string_list (key_file, CONFIG_GROUP, key_name[key], n_values, NULL);
#endif /* HAVE_GNOME */
}

void
aisleriot_conf_set_strings (AisleriotConfKey key,
                            const char * const *values,
                            gsize n_values)
{
#ifdef HAVE_GNOME
  GSList *list = NULL;
  gsize i;

  for (i = 0; i < n_values; ++i) {
    list = g_slist_prepend (list, (gpointer) values[i]);
  }
  list = g_slist_reverse (list);

  gconf_client_set_list (gconf_client, gconf_key[key], GCONF_VALUE_STRING, list, NULL);

  g_slist_free (list);
#else
  g_key_file_set_string_list (key_file, CONFIG_GROUP, key_name[key], values, n_values);
#endif /* HAVE_GNOME */
}

int
aisleriot_conf_get_int (AisleriotConfKey key)
{
#ifdef HAVE_GNOME
  return gconf_client_get_int (gconf_client, gconf_key[key], NULL);
#else
  int value;
  GError *error = NULL;

  value = g_key_file_get_integer (key_file, CONFIG_GROUP, key_name[key], &error);
  if (error) {
    value = 0;
    g_error_free (error);
  }

  return value;
#endif /* HAVE_GNOME */
}

void
aisleriot_conf_set_int (AisleriotConfKey key,
                        int value)
{
#ifdef HAVE_GNOME
  gconf_client_set_int (gconf_client, gconf_key[key], value, NULL);
#else
  g_key_file_set_integer (key_file, CONFIG_GROUP, key_name[key], value);
#endif /* HAVE_GNOME */
}

gboolean
aisleriot_conf_get_boolean (AisleriotConfKey key)
{
#ifdef HAVE_GNOME
  return gconf_client_get_bool (gconf_client, gconf_key[key], NULL);
#else
  int value;
  GError *error = NULL;

  value = g_key_file_get_boolean (key_file, CONFIG_GROUP, key_name[key], &error);
  if (error) {
    value = FALSE;
    g_error_free (error);
  }

  return value;
#endif /* HAVE_GNOME */
}

void
aisleriot_conf_set_boolean (AisleriotConfKey key,
                            gboolean value)
{
#ifdef HAVE_GNOME
  gconf_client_set_bool (gconf_client, gconf_key[key], value, NULL);
#else
  g_key_file_set_boolean (key_file, CONFIG_GROUP, key_name[key], value);
#endif /* HAVE_GNOME */
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

  *options = g_key_file_get_integer (key_file, game_file, "Options", &error);
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
  g_key_file_set_integer (key_file, game_file, "Options", value);
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

  values = g_key_file_get_integer_list (key_file, game_file, "Statistic", &len, &error);
  if (error) {
    g_error_free (error);
    return;
  }
  if (len != 4) {
    return;
  }

  statistic->wins = values[0];
  statistic->total = values[1];
  statistic->best = values[2];
  statistic->worst = values[3];

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

  g_key_file_set_integer_list (key_file, game_file, "Statistic", values, G_N_ELEMENTS (values));
#endif /* HAVE_GNOME */
}
