/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008, 2009 Christian Persch

  This library is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

#include <config.h>

#include <string.h>
#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>

#include "ar-debug.h"
#include "ar-runtime.h"
#include "ar-string-utils.h"

#include "ar-card-theme.h"
#include "ar-card-theme-private.h"

struct _ArCardThemeFixedClass {
  ArCardThemeClass parent_class;
};

struct _ArCardThemeFixed {
  ArCardTheme parent_instance;

  /* Switched on ArCardThemeFixed.use_scalable */
  char *themesizepath;
  CardSize *card_sizes;
  guint n_card_sizes;

  CardSize slot_size;
  CardSize card_size;

  guint size_available : 1;
};

/* Class implementation */

G_DEFINE_TYPE (ArCardThemeFixed, ar_card_theme_fixed, AR_TYPE_CARD_THEME);

static gboolean
ar_card_theme_fixed_load (ArCardTheme *card_theme,
                             GError **error__)
{
  ArCardThemeFixed *theme = (ArCardThemeFixed *) card_theme;
  ArCardThemeInfo *theme_info = card_theme->theme_info;
  GKeyFile *key_file;
  char *path;
  GError *error = NULL;
  int *sizes = NULL;
  gsize n_sizes, i;
  gboolean retval = FALSE;

  path = g_build_filename (theme_info->path, theme_info->filename, NULL);

  key_file = g_key_file_new ();
  if (!g_key_file_load_from_file (key_file, path, 0, &error)) {
    ar_debug_print (AR_DEBUG_CARD_THEME,
                        "Failed to load prerendered card theme from %s: %s\n", path,
                        error->message);
    g_error_free (error);
    goto loser;
  }

  sizes =
    g_key_file_get_integer_list (key_file, "Card Theme", "Sizes", &n_sizes,
                                 &error);
  if (error) {
    ar_debug_print (AR_DEBUG_CARD_THEME,
                        "Failed to get card sizes: %s\n", error->message);
    g_error_free (error);
    goto loser;
  }

  if (n_sizes == 0) {
    ar_debug_print (AR_DEBUG_CARD_THEME,
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
      ar_debug_print (AR_DEBUG_CARD_THEME,
                          "Error loading width for size %d: %s\n", sizes[i],
                          err->message);
      g_error_free (err);
      goto loser;
    }
    height = g_key_file_get_integer (key_file, group, "Height", &err);
    if (err) {
      ar_debug_print (AR_DEBUG_CARD_THEME,
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
ar_card_theme_fixed_init (ArCardThemeFixed *theme)
{
  theme->card_size.width = theme->card_size.height = -1;

  theme->card_sizes = NULL;
  theme->n_card_sizes = 0;
  theme->themesizepath = NULL;

  theme->size_available = FALSE;

  theme->card_size.width = theme->card_size.height = theme->slot_size.width =
    theme->slot_size.height = -1;
}

static void
ar_card_theme_fixed_finalize (GObject * object)
{
  ArCardThemeFixed *theme = AR_CARD_THEME_FIXED (object);

  g_free (theme->card_sizes);
  g_free (theme->themesizepath);

  G_OBJECT_CLASS (ar_card_theme_fixed_parent_class)->finalize (object);
}

static gboolean
ar_card_theme_fixed_set_card_size (ArCardTheme *card_theme,
                                      int width,
                                      int height,
                                      double proportion)
{
  ArCardThemeFixed *theme = (ArCardThemeFixed *) card_theme;
  ArCardThemeInfo *theme_info = card_theme->theme_info;

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

      ar_debug_print (AR_DEBUG_CARD_THEME,
                          "Found prerendered card size %dx%d as nearest available size to %dx%d\n",
                          size.width, size.height, twidth, theight);

    } else {
      ar_debug_print (AR_DEBUG_CARD_THEME,
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

  _ar_card_theme_emit_changed (card_theme);

  return TRUE;
}

static void
ar_card_theme_fixed_get_card_size (ArCardTheme *card_theme,
                                      CardSize *size)
{
  ArCardThemeFixed *theme = (ArCardThemeFixed *) card_theme;

  *size = theme->card_size;
}

static double
ar_card_theme_fixed_get_card_aspect (ArCardTheme *card_theme)
{
  ArCardThemeFixed *theme = (ArCardThemeFixed *) card_theme;

  return ((double) theme->card_size.width) / ((double) theme->card_size.height);
}

static GdkPixbuf *
ar_card_theme_fixed_get_card_pixbuf (ArCardTheme *card_theme,
                                        int card_id)
{
  ArCardThemeFixed *theme = (ArCardThemeFixed *) card_theme;
  GdkPixbuf *pixbuf;
  GError *error = NULL;
  char name[32], filename[36];
  char *path;

  if (!theme->size_available)
    return NULL;

  ar_card_get_name_by_id_snprintf (name, sizeof (name), card_id);
  g_snprintf (filename, sizeof (filename), "%s.png", name);
  path = g_build_filename (theme->themesizepath, filename, NULL);

  pixbuf = gdk_pixbuf_new_from_file (path, &error);
  if (!pixbuf) {
    ar_debug_print (AR_DEBUG_CARD_THEME,
                        "Failed to load card image %s: %s\n",
                        filename, error->message);
    g_error_free (error);
  }

  g_free (path);

  return pixbuf;
}

static ArCardThemeInfo *
ar_card_theme_fixed_class_get_theme_info (ArCardThemeClass *klass,
                                             const char *path,
                                             const char *filename)
{
  ArCardThemeInfo *info;
  char *display_name, *pref_name;

  if (!g_str_has_suffix (filename, ".card-theme"))
    return NULL;

  display_name = ar_filename_to_display_name (filename);
  pref_name = g_strdup_printf ("fixed:%s", filename);

  info = _ar_card_theme_info_new (G_OBJECT_CLASS_TYPE (klass),
                                     path,
                                     filename,
                                     display_name /* adopts */,
                                     pref_name,
                                     FALSE /* not scalable */,
                                     NULL, NULL);

  return info;
}

static gboolean
ar_card_theme_fixed_class_foreach_theme_dir (ArCardThemeClass *klass,
                                                ArCardThemeForeachFunc callback,
                                                gpointer data)
{
  if (!_ar_card_theme_class_foreach_env (klass, "AR_CARD_THEME_PATH_FIXED", callback, data))
    return FALSE;

  if (!callback (klass, ar_runtime_get_directory (AR_RUNTIME_PRERENDERED_CARDS_DIRECTORY), data))
    return FALSE;

  /* If we're installed in a non-system prefix, also load the card themes
   * from the system prefix.
   */
  if (!ar_runtime_is_system_prefix ())
    return callback (klass, "/usr/share/gnome-games-common/card-themes", data);

  return TRUE;
}

static void
ar_card_theme_fixed_class_init (ArCardThemeFixedClass * klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  ArCardThemeClass *theme_class = AR_CARD_THEME_CLASS (klass);

  gobject_class->finalize = ar_card_theme_fixed_finalize;

  theme_class->get_theme_info = ar_card_theme_fixed_class_get_theme_info;
  theme_class->foreach_theme_dir = ar_card_theme_fixed_class_foreach_theme_dir;

  theme_class->load = ar_card_theme_fixed_load;
  theme_class->set_card_size = ar_card_theme_fixed_set_card_size;
  theme_class->get_card_size = ar_card_theme_fixed_get_card_size;
  theme_class->get_card_aspect = ar_card_theme_fixed_get_card_aspect;
  theme_class->get_card_pixbuf = ar_card_theme_fixed_get_card_pixbuf;
}

/* public API */

/**
 * ar_card_theme_fixed_new:
 *
 * Returns: a new #ArCardThemeFixed
 */
ArCardTheme *
ar_card_theme_fixed_new (void)
{
  return g_object_new (AR_TYPE_CARD_THEME_FIXED, NULL);
}
