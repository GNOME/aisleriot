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

struct _ArCardThemeNativeClass {
  ArCardThemeQSvgClass parent_class;
};

struct _ArCardThemeNative {
  ArCardThemeQSvg parent_instance;
};

/* QtSvg can't render these correctly */
static gboolean
get_is_blacklisted (const char *filename)
{
  static const char *blacklist[] = {
    "anglo.svgz",
    "anglo_bitmap.svgz",
    "gnomangelo.svgz",
    "gnomangelo_bitmap.svgz",
    "ornamental.svgz",
    "swiss-xvii.svgz", /* trivial to fix */
    "tango.svgz"
  };
  guint i;

  for (i = 0; i < G_N_ELEMENTS (blacklist); ++i)
    if (strcmp (filename, blacklist[i]) == 0)
      return TRUE;

  return FALSE;
}


/* Class implementation */

extern "C" {
G_DEFINE_TYPE (ArCardThemeNative, ar_card_theme_native, AR_TYPE_CARD_THEME_QSVG);
}

static void
ar_card_theme_native_init (ArCardThemeNative *theme)
{
}

static ArCardThemeInfo *
ar_card_theme_native_class_get_theme_info (ArCardThemeClass *klass,
                                           const char *path,
                                           const char *filename)
{
  ArCardThemeInfo *info;
  char *display_name, *real_display_name;

  if (!g_str_has_suffix (filename, ".svgz"))
    return NULL;

  if (get_is_blacklisted (filename))
    return NULL;

  display_name = ar_filename_to_display_name (filename);
  real_display_name = g_strdup_printf ("%s (QtSvg)", display_name);
  g_free (display_name);

  info = _ar_card_theme_info_new (G_OBJECT_CLASS_TYPE (klass),
                                  path,
                                  filename,
                                  real_display_name /* adopts */,
                                  g_strdup_printf("native:%s", filename) /* pref name */,
                                  TRUE /* scalable */,
                                  NULL, NULL);

  return info;
}

static gboolean
ar_card_theme_native_class_foreach_theme_dir (ArCardThemeClass *klass,
                                              ArCardThemeForeachFunc callback,
                                              gpointer data)
{
  if (!_ar_card_theme_class_foreach_env (klass, "AR_CARD_THEME_PATH_SVG", callback, data))
    return FALSE;
  if (!_ar_card_theme_class_foreach_env (klass, "AR_CARD_THEME_PATH_NATIVE", callback, data))
    return FALSE;

  if (!callback (klass, ar_runtime_get_directory (AR_RUNTIME_SCALABLE_CARDS_DIRECTORY), data))
    return FALSE;

  /* If we're installed in a non-system prefix, also load the card themes
   * from the system prefix.
   */
  if (!ar_runtime_is_system_prefix ())
    return callback (klass, "/usr/share/aisleriot/cards", data);

  return TRUE;
}

static void
ar_card_theme_native_class_init (ArCardThemeNativeClass * klass)
{
  ArCardThemeClass *theme_class = AR_CARD_THEME_CLASS (klass);

  theme_class->get_theme_info = ar_card_theme_native_class_get_theme_info;
  theme_class->foreach_theme_dir = ar_card_theme_native_class_foreach_theme_dir;
}

/* private API */

/**
 * ar_card_theme_native_new:
 *
 * Returns: a new #ArCardThemeNative
 */
ArCardTheme*
ar_card_theme_native_new (void)
{
  return (ArCardTheme *) g_object_new (AR_TYPE_CARD_THEME_NATIVE, NULL);
}
