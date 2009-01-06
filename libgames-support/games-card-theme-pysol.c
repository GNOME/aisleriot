/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008 Christian Persch

  This programme is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This programme is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this programme.  If not, see <http://www.gnu.org/licenses/>. */

#include <config.h>

#include <errno.h>
#include <string.h>
#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>

#include "games-find-file.h"
#include "games-files.h"
#include "games-preimage.h"
#include "games-runtime.h"
#include "games-string-utils.h"

#include "games-card-theme.h"
#include "games-card-theme-private.h"

struct _GamesCardThemePysolClass {
  GamesCardThemeClass parent_class;
};

struct _GamesCardThemePysol {
  GamesCardTheme parent_instance;
};

#define PYSOL_CONFIG_FILENAME "config.txt"

typedef struct {
  char *name;
  char *base_path;
  char *ext;
  char *back_filename;
  int version;
  int type;
  int n_cards;
  CardSize card_size;
  int card_delta;
} PySolConfigTxtData;

static void
pysol_config_txt_data_free (PySolConfigTxtData *data)
{
  g_free (data->name);
  g_free (data->base_path);
  g_free (data->ext);
  g_free (data->back_filename);
  g_free (data);
}

static gboolean
parse_int (char *string,
           int *value)
{
  char *endptr;

  errno = 0;
  endptr = NULL;
  *value = g_ascii_strtoll (string, &endptr, 10);

  return errno == 0 && endptr != string;
}

static gboolean
pysol_config_txt_parse_line_0 (PySolConfigTxtData *data,
                               const char *line)
{
  char **fields;
  gsize n_fields;
  gboolean retval = FALSE;

  /* FIXMEchpe */
  data->version = 0;
  data->n_cards = 52;
  data->type = 1;

  fields = g_strsplit (line, ";", -1);
  if (!fields)
    return FALSE;

  n_fields = g_strv_length (fields);
  if (n_fields < 2)
    goto out;
  if (!strcmp (g_strstrip (fields[0]), "PySol solitaire cardset") == 0)
    goto out;
  if (!parse_int (g_strstrip (fields[1]), &data->version))
    goto out;

  if (data->version >= 3) {
    if (n_fields < 4)
      goto out;
    data->ext = g_strstrip (g_strdup (fields[2]));
    if (!parse_int (fields[3], &data->type))
      goto out;
    if (!parse_int (fields[4], &data->n_cards))
      goto out;
  }

  retval = TRUE;
out:
  g_strfreev (fields);
  return retval;
}

static gboolean
pysol_config_txt_parse_line_1 (PySolConfigTxtData *data,
                               const char *line)
{
  char **fields;
  gsize n_fields;
  gboolean retval = FALSE;

  fields = g_strsplit (line, ";", -1);
  if (!fields)
    return FALSE;
  n_fields = g_strv_length (fields);
  if (n_fields < 2)
    goto out;

  data->name = g_strstrip (g_strdup (fields[1]));

  retval = TRUE;
out:
  g_strfreev (fields);
  return retval;
}

static gboolean
pysol_config_txt_parse_line_2 (PySolConfigTxtData *data,
                               const char *line)
{
  char **fields;
  gsize n_fields;
  gboolean retval = FALSE;

  fields = g_strsplit (line, " ", -1);
  if (!fields)
    return FALSE;
  n_fields = g_strv_length (fields);
  if (n_fields != 3)
    goto out;
  if (!parse_int (g_strstrip (fields[0]), &data->card_size.width) ||
      !parse_int (g_strstrip (fields[1]), &data->card_size.height) ||
      !parse_int (g_strstrip (fields[2]), &data->card_delta))
    goto out;

  retval = TRUE;
out:
  g_strfreev (fields);
  return retval;
}

static gboolean
pysol_config_txt_parse_line_4 (PySolConfigTxtData *data,
                               const char *line)
{
  data->back_filename = g_strstrip (g_strdup (line));
  return TRUE;
}

static PySolConfigTxtData *
pysol_config_txt_parse (const char *path,
                        const char *subdir)
{
  PySolConfigTxtData *pysol_data = NULL;
  char *config_txt_path;
  char *data = NULL;
  char **lines = NULL;
  gsize len;
  gboolean retval = FALSE;

  config_txt_path = g_build_filename (path, subdir, PYSOL_CONFIG_FILENAME, NULL);
  if (!g_file_get_contents (config_txt_path, &data, &len, NULL) || !len)
    goto out;

  lines = g_strsplit (data, "\n", -1);
  if (!lines || g_strv_length (lines) < 6)
    goto out;

  pysol_data = g_new0 (PySolConfigTxtData, 1);
  if (!pysol_config_txt_parse_line_0 (pysol_data, g_strstrip (lines[0])) ||
        pysol_data->n_cards != 52 ||
      !pysol_config_txt_parse_line_1 (pysol_data, g_strstrip (lines[1])) ||
        !pysol_data->name ||
      !pysol_config_txt_parse_line_2 (pysol_data, g_strstrip (lines[2])) ||
      !pysol_config_txt_parse_line_4 (pysol_data, g_strstrip (lines[4])) ||
        !pysol_data->back_filename)
    goto out;

  pysol_data->base_path = g_build_filename (path, subdir, NULL);
  if (!pysol_data->ext)
    pysol_data->ext = g_strdup (".gif");

  retval = TRUE;

out:
  g_free (config_txt_path);
  g_free (data);
  g_strfreev (lines);

  if (retval)
    return pysol_data;

  if (pysol_data)
    pysol_config_txt_data_free (pysol_data);
    
  return NULL;
}

/* Class implementation */

G_DEFINE_TYPE (GamesCardThemePysol, games_card_theme_pysol, GAMES_TYPE_CARD_THEME);

static gboolean
games_card_theme_pysol_load (GamesCardTheme *card_theme,
                             GError **error)
{
  /* nothing more to do here, we have all the info in our PySolConfigTxtData */
  return TRUE;
}

static GdkPixbuf *
games_card_theme_pysol_load_card (PySolConfigTxtData *data,
                                  int card_id)
{
  GdkPixbuf *pixbuf;
  char *path;
  GError *error = NULL;

  if (G_UNLIKELY (card_id == GAMES_CARD_SLOT)) {
    path = g_build_filename (data->base_path, "bottom01.gif" /* FIXMEchpe ext! */, NULL);
  } else if (G_UNLIKELY (card_id == GAMES_CARD_BACK)) {
    path = g_build_filename (data->base_path, data->back_filename, NULL);
  } else {
    static const char suit_char[] = "cdhs";
    int suit, rank;
    char filename[32];

    suit = card_id / 13;
    rank = card_id % 13;

    if (G_UNLIKELY (suit == 4)) /* Joker */
      return NULL; /* FIXMEchpe */

    g_snprintf (filename, sizeof (filename), "%02d%c%s", rank + 1, suit_char[suit], data->ext);
    path = g_build_filename (data->base_path, filename, NULL);
  }

  pixbuf = gdk_pixbuf_new_from_file (path, &error);
  if (!pixbuf) {
    g_warning ("Failed to load card ID %d: %s\n", card_id, error->message);
    g_error_free (error);
  }

  g_free (path);

  return pixbuf;
}

static void
games_card_theme_pysol_init (GamesCardThemePysol *theme)
{
}

static gboolean
games_card_theme_pysol_set_card_size (GamesCardTheme *card_theme,
                                      int width,
                                      int height,
                                      double proportion)
{
  /* not changing, ever */
  return FALSE;
}

static CardSize
games_card_theme_pysol_get_card_size (GamesCardTheme *card_theme)
{
  GamesCardThemeInfo *theme_info = card_theme->theme_info;
  PySolConfigTxtData *pysol_data = theme_info->data;

  return pysol_data->card_size;
}

static double
games_card_theme_pysol_get_card_aspect (GamesCardTheme *card_theme)
{
  PySolConfigTxtData *pysol_data = card_theme->theme_info->data;

  return ((double) pysol_data->card_size.width) / ((double) pysol_data->card_size.height);
}

static GdkPixbuf *
games_card_theme_pysol_get_card_pixbuf (GamesCardTheme *card_theme,
                                        int card_id)
{
  GdkPixbuf *pixbuf;

  pixbuf = games_card_theme_pysol_load_card (card_theme->theme_info->data, card_id);

  return pixbuf;
}

static GamesCardThemeInfo *
games_card_theme_pysol_class_get_theme_info (GamesCardThemeClass *klass,
                                             const char *path,
                                             const char *filename)
{
  GamesCardThemeInfo *info = NULL;
  PySolConfigTxtData *pysol_data;
  char *display_name;

  if (!g_str_has_prefix (filename, "cardset-"))
    return NULL;

  pysol_data = pysol_config_txt_parse (path, filename);
  if (!pysol_data)
    return NULL;

  display_name = g_strdup_printf ("%s (PySol)", pysol_data->name);

  info = _games_card_theme_info_new (G_OBJECT_CLASS_TYPE (klass),
                                     path,
                                     filename,
                                     display_name,
                                     pysol_data,
                                     (GDestroyNotify) pysol_config_txt_data_free);
  g_free (display_name);

  return info;
}

static void
games_card_theme_pysol_class_get_theme_infos (GamesCardThemeClass *klass,
                                              GList **list)
{
  _games_card_theme_class_append_theme_info_foreach_env
    (klass, "GAMES_CARD_THEME_PATH_PYSOL", list);

  /* FIXMEchpe: is this univeral or ubuntu specific? */
  _games_card_theme_class_append_theme_info_foreach
    (klass, "/usr/share/games/pysol", list);
}

static void
games_card_theme_pysol_class_init (GamesCardThemePysolClass * klass)
{
  GamesCardThemeClass *theme_class = GAMES_CARD_THEME_CLASS (klass);

  theme_class->get_theme_info = games_card_theme_pysol_class_get_theme_info;
  theme_class->get_theme_infos = games_card_theme_pysol_class_get_theme_infos;

  theme_class->load = games_card_theme_pysol_load;
  theme_class->set_card_size = games_card_theme_pysol_set_card_size;
  theme_class->get_card_size = games_card_theme_pysol_get_card_size;
  theme_class->get_card_aspect = games_card_theme_pysol_get_card_aspect;
  theme_class->get_card_pixbuf = games_card_theme_pysol_get_card_pixbuf;
}

/* public API */

/**
 * games_card_theme_pysol_new:
 *
 * Returns: a new #GamesCardThemePysol
 */
GamesCardTheme *
games_card_theme_pysol_new (void)
{
  return g_object_new (GAMES_TYPE_CARD_THEME_PYSOL, NULL);
}
