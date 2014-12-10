/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008, 2010, 2014 Christian Persch

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

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

#include <config.h>

#include <string.h>
#include <glib.h>
#include <gtk/gtk.h>

#include "ar-debug.h"
#include "ar-profile.h"
#include "ar-runtime.h"
#include "ar-string-utils.h"

#include "ar-card-theme.h"
#include "ar-card-theme-private.h"
#include "ar-card-theme-qsvg-private.h"

struct _ArCardThemeKDEClass {
  ArCardThemeQSvgClass parent_class;
};

struct _ArCardThemeKDE {
  ArCardThemeQSvg parent_instance;
};

#define KDE_CARDS_GROUP           "KDE Cards"
#define KDE_CARDS_NAME_KEY        "Name"
#define KDE_CARDS_SVG_KEY         "SVG"

#define KDE_BACKDECK_FILENAME     "index.desktop"
#define KDE_BACKDECK_GROUP        "KDE Backdeck"
#define KDE_BACKDECK_BACK_KEY     "Back"
#define KDE_BACKDECK_NAME_KEY     "Name" /* localised */
#define KDE_BACKDECK_PYSOL_KEY    "PySol"
#define KDE_BACKDECK_SVG_KEY      "SVG"

static gboolean
get_is_blacklisted (const char *filename)
{
  static const char *blacklist[] = {
#if defined(HAVE_RSVG) && defined(ENABLE_CARD_THEME_FORMAT_SVG)
    /* We have these as native themes, and ours even render faster */
    "svg-dondorf",
    "svg-gm-paris", /* Bellot */
    "svg-nicu-ornamental",
#endif /* !HAVE_RSVG || !ENABLE_CARD_THEME_FORMAT_SVG */

    /* These are just plaing ugly */
    "svg-konqi-modern",
    "svg-standard",
    "svg-xskat-french",
  };
  guint i;

  for (i = 0; i < G_N_ELEMENTS (blacklist); ++i)
    if (strcmp (filename, blacklist[i]) == 0)
      return TRUE;

  return FALSE;
}

/* Class implementation */

extern "C" {
G_DEFINE_TYPE (ArCardThemeKDE, ar_card_theme_kde, AR_TYPE_CARD_THEME_QSVG);
}

static void
ar_card_theme_kde_init (ArCardThemeKDE *theme)
{
}

static ArCardThemeInfo *
ar_card_theme_kde_class_get_theme_info (ArCardThemeClass *klass,
                                        const char *path,
                                        const char *filename)
{
  ArCardThemeInfo *info = NULL;
  char *base_path = NULL, *key_file_path = NULL;
  GKeyFile *key_file = NULL;
  char *svg_filename = NULL, *name = NULL, *display_name, *pref_name;

  if (get_is_blacklisted (filename)) {
    ar_debug_print (AR_DEBUG_CARD_THEME,
                        "KDE card theme %s is blacklisted\n", filename);
    return NULL;
  }

  base_path = g_build_filename (path, filename, NULL);
  if (!g_file_test (path, G_FILE_TEST_IS_DIR))
    goto out;

  key_file_path = g_build_filename (base_path, KDE_BACKDECK_FILENAME, NULL);
  key_file = g_key_file_new ();
  if (!g_key_file_load_from_file (key_file, key_file_path, GKeyFileFlags(0), NULL))
    goto out;

  if (!g_key_file_has_group (key_file, KDE_BACKDECK_GROUP))
    goto out;

  name = g_key_file_get_locale_string (key_file, KDE_BACKDECK_GROUP, KDE_BACKDECK_NAME_KEY, NULL, NULL);
  svg_filename = g_key_file_get_string (key_file, KDE_BACKDECK_GROUP, KDE_BACKDECK_SVG_KEY, NULL);
  if (!name || !name[0] || !svg_filename || !svg_filename[0])
    goto out;

  display_name = g_strdup_printf ("%s (KDE)", name);
  pref_name = g_strdup_printf ("kde:%s", filename);
  info = _ar_card_theme_info_new (G_OBJECT_CLASS_TYPE (klass),
                                  base_path,
                                  svg_filename,
                                  display_name /* adopts */,
                                  pref_name /* adopts */,
                                  TRUE /* scalable */,
                                  NULL, NULL);

out:
  g_free (base_path);
  g_free (key_file_path);
  g_free (name);
  g_free (svg_filename);
  if (key_file) {
    g_key_file_free (key_file);
  }

  return info;
}

static gboolean
ar_card_theme_kde_class_foreach_theme_dir (ArCardThemeClass *klass,
                                           ArCardThemeForeachFunc callback,
                                           gpointer data)
{
  if (!_ar_card_theme_class_foreach_env (klass, "AR_CARD_THEME_PATH_KDE", callback, data))
    return FALSE;

  return callback (klass, KDE_CARD_THEME_PATH, data);
}

static void
ar_card_theme_kde_class_init (ArCardThemeKDEClass * klass)
{
  ArCardThemeClass *theme_class = AR_CARD_THEME_CLASS (klass);

  theme_class->get_theme_info = ar_card_theme_kde_class_get_theme_info;
  theme_class->foreach_theme_dir = ar_card_theme_kde_class_foreach_theme_dir;
}

/* private API */

/**
 * ar_card_theme_kde_new:
 *
 * Returns: a new #ArCardThemeKDE
 */
ArCardTheme*
ar_card_theme_kde_new (void)
{
  return (ArCardTheme *) g_object_new (AR_TYPE_CARD_THEME_KDE, NULL);
}
