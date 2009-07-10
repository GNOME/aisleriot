/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008, 2009 Christian Persch

  This library is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

#include <config.h>

#include <string.h>
#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>

#include "games-debug.h"
#include "games-runtime.h"
#include "games-string-utils.h"

#include "games-card-theme.h"
#include "games-card-theme-private.h"

struct _GamesCardThemeFixedClass {
  GamesCardThemeClass parent_class;
};

struct _GamesCardThemeFixed {
  GamesCardTheme parent_instance;

  /* Switched on GamesCardThemeFixed.use_scalable */
  char *themesizepath;
  CardSize *card_sizes;
  guint n_card_sizes;

  CardSize slot_size;
  CardSize card_size;

  guint size_available : 1;
};

/* Class implementation */

G_DEFINE_TYPE (GamesCardThemeFixed, games_card_theme_fixed, GAMES_TYPE_CARD_THEME);

static gboolean
games_card_theme_fixed_load (GamesCardTheme *card_theme,
                             GError **error__)
{
  GamesCardThemeFixed *theme = (GamesCardThemeFixed *) card_theme;
  GamesCardThemeInfo *theme_info = card_theme->theme_info;
  GKeyFile *key_file;
  char *path;
  GError *error = NULL;
  int *sizes = NULL;
  gsize n_sizes, i;
  gboolean retval = FALSE;

  path = g_build_filename (theme_info->path, theme_info->filename, NULL);

  key_file = g_key_file_new ();
  if (!g_key_file_load_from_file (key_file, path, 0, &error)) {
    _games_debug_print (GAMES_DEBUG_CARD_THEME,
                        "Failed to load prerendered card theme from %s: %s\n", path,
                        error->message);
    g_error_free (error);
    goto loser;
  }

  sizes =
    g_key_file_get_integer_list (key_file, "Card Theme", "Sizes", &n_sizes,
                                 &error);
  if (error) {
    _games_debug_print (GAMES_DEBUG_CARD_THEME,
                        "Failed to get card sizes: %s\n", error->message);
    g_error_free (error);
    goto loser;
  }

  if (n_sizes == 0) {
    _games_debug_print (GAMES_DEBUG_CARD_THEME,
                        "Card theme contains no sizes\n");
    goto loser;
  }

  theme->card_sizes = g_new (CardSize, n_sizes);
  theme->n_card_sizes = n_sizes;

  for (i = 0; i < n_sizes; ++i) {
    char group[32];
    GError *err = NULL;
    int width, height;

    g_snprintf (group, sizeof (group), "%d", sizes[i]);

    width = g_key_file_get_integer (key_file, group, "Width", &err);
    if (err) {
      _games_debug_print (GAMES_DEBUG_CARD_THEME,
                          "Error loading width for size %d: %s\n", sizes[i],
                          err->message);
      g_error_free (err);
      goto loser;
    }
    height = g_key_file_get_integer (key_file, group, "Height", &err);
    if (err) {
      _games_debug_print (GAMES_DEBUG_CARD_THEME,
                          "Error loading height for size %d: %s\n", sizes[i],
                          err->message);
      g_error_free (err);
      goto loser;
    }

    theme->card_sizes[i].width = width;
    theme->card_sizes[i].height = height;
  }

  retval = TRUE;
    
loser:

  g_free (sizes);

  g_key_file_free (key_file);
  g_free (path);

  return retval;
}

static void
games_card_theme_fixed_init (GamesCardThemeFixed *theme)
{
  theme->card_size.width = theme->card_size.height = -1;

  theme->card_sizes = NULL;
  theme->n_card_sizes = 0;
  theme->themesizepath = NULL;

  theme->size_available = FALSE;

  theme->card_size.width = theme->card_size.height = theme->slot_size.width =
    theme->slot_size.width = -1;
}

static void
games_card_theme_fixed_finalize (GObject * object)
{
  GamesCardThemeFixed *theme = GAMES_CARD_THEME_FIXED (object);

  g_free (theme->card_sizes);
  g_free (theme->themesizepath);

  G_OBJECT_CLASS (games_card_theme_fixed_parent_class)->finalize (object);
}

static gboolean
games_card_theme_fixed_set_card_size (GamesCardTheme *card_theme,
                                      int width,
                                      int height,
                                      double proportion)
{
  GamesCardThemeFixed *theme = (GamesCardThemeFixed *) card_theme;
  GamesCardThemeInfo *theme_info = card_theme->theme_info;

  if ((width == theme->slot_size.width) &&
      (height == theme->slot_size.height))
    return FALSE;

  theme->slot_size.width = width;
  theme->slot_size.height = height;

  {
    guint i;
    int twidth, theight;
    CardSize size = { -1, -1 }, fit_size = { -1, -1};

    twidth = FLOAT_TO_INT_CEIL (((double) width) * proportion);
    theight = FLOAT_TO_INT_CEIL (((double) height) * proportion);

    /* Find the closest prerendered size */
    for (i = 0; i < theme->n_card_sizes; ++i) {
      CardSize info = theme->card_sizes[i];

      if (info.width > width || info.height > height)
        continue;

      if (info.width > fit_size.width && info.height > fit_size.height)
        fit_size = info;

      /* FIXMEchpe */
      if (info.width <= twidth && info.height <= theight &&
          info.width > size.width && info.height > size.height)
        size = info;
    }

    if (size.width < 0 || size.height < 0)
      size = fit_size;

    if (size.width > 0 && size.height > 0) {
      char *theme_basename;
      char sizestr[16];

      if (size.width == theme->card_size.width &&
          size.height == theme->card_size.height)
        return FALSE;

      g_free (theme->themesizepath);

      theme_basename = g_strdup (theme_info->filename);
      *(strchr (theme_basename, '.')) = '\0';
      g_snprintf (sizestr, sizeof (sizestr), "%d", size.width);
      theme->themesizepath = g_build_filename (theme_info->path,
                                               theme_basename,
                                               sizestr, NULL);
      g_free (theme_basename);

      theme->size_available = TRUE;
      theme->card_size = size;

      _games_debug_print (GAMES_DEBUG_CARD_THEME,
                          "Found prerendered card size %dx%d as nearest available size to %dx%d\n",
                          size.width, size.height, twidth, theight);

    } else {
      _games_debug_print (GAMES_DEBUG_CARD_THEME,
                          "No prerendered size available for %d:%d\n",
                          width, height);
      theme->size_available = FALSE;

      /* FIXMEchpe: at least use the smallest available size here, or
       * programme will surely crash when trying to render NULL pixbufs
       * later on!
       */
      /* FIXMEchpe: emit changed signal here too!! */
      return FALSE;
    }
  }

  _games_card_theme_emit_changed (card_theme);

  return TRUE;
}

static void
games_card_theme_fixed_get_card_size (GamesCardTheme *card_theme,
                                      CardSize *size)
{
  GamesCardThemeFixed *theme = (GamesCardThemeFixed *) card_theme;

  *size = theme->card_size;
}

static double
games_card_theme_fixed_get_card_aspect (GamesCardTheme *card_theme)
{
  GamesCardThemeFixed *theme = (GamesCardThemeFixed *) card_theme;

  return ((double) theme->card_size.width) / ((double) theme->card_size.height);
}

static GdkPixbuf *
games_card_theme_fixed_get_card_pixbuf (GamesCardTheme *card_theme,
                                        int card_id)
{
  GamesCardThemeFixed *theme = (GamesCardThemeFixed *) card_theme;
  GdkPixbuf *pixbuf;
  GError *error = NULL;
  char name[32], filename[36];
  char *path;

  if (!theme->size_available)
    return NULL;

  games_card_get_name_by_id_snprintf (name, sizeof (name), card_id);
  g_snprintf (filename, sizeof (filename), "%s.png", name);
  path = g_build_filename (theme->themesizepath, filename, NULL);

  pixbuf = gdk_pixbuf_new_from_file (path, &error);
  if (!pixbuf) {
    _games_debug_print (GAMES_DEBUG_CARD_THEME,
                        "Failed to load card image %s: %s\n",
                        filename, error->message);
    g_error_free (error);
  }

  g_free (path);

  return pixbuf;
}

static GamesCardThemeInfo *
games_card_theme_fixed_class_get_theme_info (GamesCardThemeClass *klass,
                                             const char *path,
                                             const char *filename)
{
  GamesCardThemeInfo *info;
  char *display_name, *pref_name;

  if (!g_str_has_suffix (filename, ".card-theme"))
    return NULL;

  display_name = games_filename_to_display_name (filename);

#ifdef HAVE_HILDON
  /* On Hildon, fixed is the default. For pref backward compatibility,
   * we don't add the fixed: prefix there.
   */
  pref_name = g_strdup (filename);
  *(strrchr (pref_name, '.')) = '\0'; /* strip extension */
#else
  pref_name = g_strdup_printf ("fixed:%s", filename);
#endif

  info = _games_card_theme_info_new (G_OBJECT_CLASS_TYPE (klass),
                                     path,
                                     filename,
                                     display_name /* adopts */,
                                     pref_name,
                                     NULL, NULL);

  return info;
}

static gboolean
games_card_theme_fixed_class_foreach_theme_dir (GamesCardThemeClass *klass,
                                                GamesCardThemeForeachFunc callback,
                                                gpointer data)
{
  if (!_games_card_theme_class_foreach_env (klass, "GAMES_CARD_THEME_PATH_FIXED", callback, data))
    return FALSE;

  return callback (klass, games_runtime_get_directory (GAMES_RUNTIME_PRERENDERED_CARDS_DIRECTORY), data);
}

static void
games_card_theme_fixed_class_init (GamesCardThemeFixedClass * klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GamesCardThemeClass *theme_class = GAMES_CARD_THEME_CLASS (klass);

  gobject_class->finalize = games_card_theme_fixed_finalize;

  theme_class->get_theme_info = games_card_theme_fixed_class_get_theme_info;
  theme_class->foreach_theme_dir = games_card_theme_fixed_class_foreach_theme_dir;

  theme_class->load = games_card_theme_fixed_load;
  theme_class->set_card_size = games_card_theme_fixed_set_card_size;
  theme_class->get_card_size = games_card_theme_fixed_get_card_size;
  theme_class->get_card_aspect = games_card_theme_fixed_get_card_aspect;
  theme_class->get_card_pixbuf = games_card_theme_fixed_get_card_pixbuf;
}

/* public API */

/**
 * games_card_theme_fixed_new:
 *
 * Returns: a new #GamesCardThemeFixed
 */
GamesCardTheme *
games_card_theme_fixed_new (void)
{
  return g_object_new (GAMES_TYPE_CARD_THEME_FIXED, NULL);
}
