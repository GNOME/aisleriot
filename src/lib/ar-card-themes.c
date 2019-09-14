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

#ifdef GDK_WINDOWING_X11
#include <gdk/gdkx.h>
#endif

#include "ar-debug.h"
#include "ar-profile.h"
#include "ar-runtime.h"

#include "ar-card-themes.h"
#include "ar-card-theme-private.h"

struct _ArCardThemesClass {
  GObjectClass parent_class;
};

struct _ArCardThemes {
  GObject parent;

  GHashTable *theme_infos;
  gboolean theme_infos_loaded;
};

enum {
  N_THEME_TYPES = 5
};

enum {
  CHANGED,
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

/**
 * theme_type_from_string:
 * @type_str:
 * @type_str_len: the length of @type_str
 *
 * Returns: the #GType of the card theme type @type_str
 */
static GType
theme_type_from_string (const char *type_str,
                        gssize type_str_len)
{
  const struct {
    const char name[8];
    GType type;
  } type_strings[] = {
#ifdef HAVE_QTSVG
#if defined(ENABLE_CARD_THEME_FORMAT_NATIVE) && !defined(ENABLE_CARD_THEME_FORMAT_SVG)
    { "svg", AR_TYPE_CARD_THEME_NATIVE },
#endif
#endif  /* HAVE_QTSVG */

#ifdef HAVE_RSVG
#ifdef ENABLE_CARD_THEME_FORMAT_SVG
    { "svg", AR_TYPE_CARD_THEME_SVG },
#endif
#endif /* HAVE_RSVG */

#ifdef HAVE_QTSVG
#ifdef ENABLE_CARD_THEME_FORMAT_NATIVE
    { "native", AR_TYPE_CARD_THEME_NATIVE },
#endif
#ifdef ENABLE_CARD_THEME_FORMAT_KDE
    { "kde", AR_TYPE_CARD_THEME_KDE },
#endif
#endif /* HAVE_QTSVG */

#ifdef ENABLE_CARD_THEME_FORMAT_PYSOL
    { "pysol", AR_TYPE_CARD_THEME_PYSOL },
#endif
#ifdef ENABLE_CARD_THEME_FORMAT_FIXED
    { "fixed", AR_TYPE_CARD_THEME_FIXED }
#endif
  };
  GType type = G_TYPE_INVALID;

  if (type_str_len == 0) {
    static const char default_type_string[] = AR_CARD_THEME_DEFAULT_FORMAT_STRING;

    /* Use the default type */
    type = theme_type_from_string (default_type_string, strlen (default_type_string));
  } else {
    guint i;

    for (i = 0; i < G_N_ELEMENTS (type_strings); ++i) {
      if (strncmp (type_str, type_strings[i].name, type_str_len) == 0) {
        type = type_strings[i].type;
        break;
      }
    }
  }

  return type;
}

/**
 * theme_filename_and_type_from_name:
 * @theme_name: the theme name, or %NULL to get the default theme
 * @type: return location for the card theme type
 *
 * Returns: the filename of the theme @theme_name, and puts the type
 *   in @type, or %NULL if it was not possible to get the type or
 *   filename from @theme_name, or if the requested theme type is not
 *   supported
 */
static char *
theme_filename_and_type_from_name (const char *theme_name,
                                   GType *type)
{
  const char *colon, *filename, *dot;

  g_return_val_if_fail (type != NULL, NULL);

  ar_debug_print (AR_DEBUG_CARD_THEME,
                      "theme_filename_and_type_from_name %s\n",
                      theme_name ? theme_name : "(null)");

  if (!theme_name || !theme_name[0])
    theme_name = AR_CARD_THEME_DEFAULT;

  colon = strchr (theme_name, ':');
  *type = theme_type_from_string (theme_name, colon ? colon - theme_name : 0);
  if (*type == G_TYPE_INVALID)
    return NULL;

  /* Get the filename from the theme name */
  if (colon) {
    filename = colon + 1;
  } else {
    filename = theme_name;
  }

  dot = strrchr (filename, '.');
  if (filename == dot || !filename[0])
    return NULL;

  if (dot == NULL) {
    /* No dot? Try appending the default, for compatibility with old settings */
#if defined(ENABLE_CARD_THEME_FORMAT_FIXED)
    if (*type == AR_TYPE_CARD_THEME_FIXED) {
      return g_strconcat (filename, ".card-theme", NULL);
    }
#elif defined(ENABLE_CARD_THEME_FORMAT_SVG)
    if (*type == AR_TYPE_CARD_THEME_SVG) {
      return g_strconcat (filename, ".svg", NULL);
    }
#endif
  } else {
#if defined(HAVE_GNOME) && defined(ENABLE_CARD_THEME_FORMAT_SVG)
    if (*type == AR_TYPE_CARD_THEME_SVG &&
        g_str_has_suffix (filename, ".png")) {
      char *base_name, *retval;

      /* Very old version; replace .png with .svg */
      base_name = g_strndup (filename, dot - filename);
      retval = g_strconcat (base_name, ".svg", NULL);
      g_free (base_name);

      return retval;
    }
#endif /* HAVE_GNOME && ENABLE_CARD_THEME_FORMAT_SVG */
  }

  return g_strdup (filename);
}

static gboolean
ar_card_themes_foreach_theme_dir (GType type,
                                     ArCardThemeForeachFunc callback,
                                     gpointer data)
{
  ArCardThemeClass *klass;
  gboolean retval;

  klass = g_type_class_ref (type);
  if (!klass)
    return TRUE;

  ar_profilestart ("foreach %s card themes", G_OBJECT_CLASS_NAME (klass));
  retval = _ar_card_theme_class_foreach_theme_dir (klass, callback, data);
  ar_profileend ("foreach %s card themes", G_OBJECT_CLASS_NAME (klass));

  g_type_class_unref (klass);
  return retval;
}

static gboolean
ar_card_themes_foreach_theme_type_and_dir (ArCardThemes *theme_manager,
                                           ArCardThemeForeachFunc callback,
                                           gpointer data)
{
  const GType types[] = {
  /* List of supported theme types, in order of decreasing precedence */
#ifdef HAVE_QTSVG
#if defined(ENABLE_CARD_THEME_FORMAT_NATIVE) && !defined(ENABLE_CARD_THEME_FORMAT_SVG)
    AR_TYPE_CARD_THEME_NATIVE,
#endif
#endif /* HAVE_QTSVG */
#ifdef HAVE_RSVG
#ifdef ENABLE_CARD_THEME_FORMAT_SVG
  AR_TYPE_CARD_THEME_SVG,
#endif
#endif /* HAVE_RSVG */
#ifdef HAVE_QTSVG
#ifdef ENABLE_CARD_THEME_FORMAT_KDE
  AR_TYPE_CARD_THEME_KDE,
#endif
#ifdef ENABLE_CARD_THEME_NATIVE
    AR_TYPE_CARD_THEME_NATIVE,
#endif
#endif /* HAVE_QTSVG */
#ifdef ENABLE_CARD_THEME_FORMAT_PYSOL
  AR_TYPE_CARD_THEME_PYSOL,
#endif
#ifdef ENABLE_CARD_THEME_FORMAT_FIXED
  AR_TYPE_CARD_THEME_FIXED
#endif
  };
  guint i;
  gboolean retval = TRUE;

  for (i = 0; i < G_N_ELEMENTS (types); ++i) {
    retval = ar_card_themes_foreach_theme_dir (types[i], callback, data);
    if (!retval)
      break;
  }

  return retval;
}

static gboolean
ar_card_themes_get_theme_infos_in_dir (ArCardThemeClass *klass,
                                          const char *path,
                                          ArCardThemes *theme_manager)
{
  GDir *iter;
  const char *filename;

  ar_debug_print (AR_DEBUG_CARD_THEME,
                      "Looking for %s themes in %s\n",
                      G_OBJECT_CLASS_NAME (klass),
                      path);

  ar_profilestart ("looking for %s card themes in %s", G_OBJECT_CLASS_NAME (klass), path);

  iter = g_dir_open (path, 0, NULL);
  if (!iter)
    goto out;

  while ((filename = g_dir_read_name (iter)) != NULL) {
    ArCardThemeInfo *info;

    ar_profilestart ("checking for %s card theme in file %s", G_OBJECT_CLASS_NAME (klass), filename);
    info = _ar_card_theme_class_get_theme_info (klass, path, filename);
    ar_profileend ("checking for %s card theme in file %s", G_OBJECT_CLASS_NAME (klass), filename);

    if (info != NULL) {
      /* Don't replace an already existing theme info! */
      if (g_hash_table_lookup (theme_manager->theme_infos, info->pref_name))
        ar_card_theme_info_unref (info);
      else
        g_hash_table_insert (theme_manager->theme_infos, info->pref_name, info);
    }
  }

  g_dir_close (iter);

out:
  ar_profileend ("looking for %s card themes in %s", G_OBJECT_CLASS_NAME (klass), path);

  return TRUE;
}

typedef struct {
  const char *filename;
  ArCardThemeInfo *theme_info;
} LookupData;

static gboolean
ar_card_themes_try_theme_info_by_filename (ArCardThemeClass *klass,
                                              const char *path,
                                              LookupData *data)
{
  ar_debug_print (AR_DEBUG_CARD_THEME,
                      "Looking for theme %s/%s in %s\n",
                      G_OBJECT_CLASS_NAME (klass),
                      data->filename,
                      path);

  /* Try constructing the theme info */
  data->theme_info = _ar_card_theme_class_get_theme_info (klass, path, data->filename);

  /* Continue until found */
  return data->theme_info == NULL;
}

static void
ar_card_themes_load_theme_infos (ArCardThemes *theme_manager)
{
  ar_debug_print (AR_DEBUG_CARD_THEME,
                      "Scanning theme directories\n");

  /* FIXMEchpe: clear the hash table here? */

  ar_profilestart ("looking for card themes");
  ar_card_themes_foreach_theme_type_and_dir (theme_manager,
                                                (ArCardThemeForeachFunc) ar_card_themes_get_theme_infos_in_dir,
                                                theme_manager);
  ar_profileend ("looking for card themes");

  theme_manager->theme_infos_loaded = TRUE;

  g_signal_emit (theme_manager, signals[CHANGED], 0);
}

typedef struct {
  GType type;
  const char *filename;
  ArCardThemeInfo *theme_info;
} ThemesByTypeAndFilenameData;

static void
themes_foreach_by_type_and_filename (gpointer key,
                                     ArCardThemeInfo *theme_info,
                                     ThemesByTypeAndFilenameData *data)
{
  if (data->theme_info)
    return;

  if (theme_info->type == data->type &&
      strcmp (theme_info->filename, data->filename) == 0)
    data->theme_info = theme_info;
}

static void
themes_foreach_add_to_list (gpointer key,
                            ArCardThemeInfo *theme_info,
                            GList **list)
{
  *list = g_list_prepend (*list, ar_card_theme_info_ref (theme_info));
}

typedef struct {
  ArCardThemes *theme_manager;
  ArCardTheme *theme;
} ThemesAnyData;

static void
themes_foreach_any (gpointer key,
                    ArCardThemeInfo *theme_info,
                    ThemesAnyData *data)
{
  if (data->theme)
    return;

  data->theme = ar_card_themes_get_theme (data->theme_manager, theme_info);
}

/* Class implementation */

G_DEFINE_TYPE (ArCardThemes, ar_card_themes, G_TYPE_OBJECT);

static void
ar_card_themes_init (ArCardThemes *theme_manager)
{
  /* Hash table: pref name => theme info */
  theme_manager->theme_infos = g_hash_table_new_full (g_str_hash, g_str_equal,
                                                      NULL /* key is owned by data */,
                                                      (GDestroyNotify) ar_card_theme_info_unref);

  theme_manager->theme_infos_loaded = FALSE;
}

static void
ar_card_themes_finalize (GObject *object)
{
  ArCardThemes *theme_manager = AR_CARD_THEMES (object);

  g_hash_table_destroy (theme_manager->theme_infos);

  G_OBJECT_CLASS (ar_card_themes_parent_class)->finalize (object);
}

static void
ar_card_themes_class_init (ArCardThemesClass * klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  gobject_class->finalize = ar_card_themes_finalize;

  /**
   * ArCardThemes:changed:
   *
   * The ::changed signal is emitted when the list of card themes has
   * changed.
   */
  signals[CHANGED] =
    g_signal_newv ("changed",
                   G_TYPE_FROM_CLASS (klass),
                   (GSignalFlags) (G_SIGNAL_RUN_LAST),
                   NULL,
                   NULL, NULL,
                   g_cclosure_marshal_VOID__VOID,
                   G_TYPE_NONE,
                   0, NULL);
}

/* public API */

/**
 * ar_card_themes_new:
 *
 * Returns: a new #ArCardThemes object
 */
ArCardThemes *
ar_card_themes_new (void)
{
  return g_object_new (AR_TYPE_CARD_THEMES, NULL);
}

/**
 * ar_card_themes_request_themes:
 * @theme_manager:
 *
 * Scans all theme directories for themes, if necessary. If the
 * themes list has changed, emits the "changed" signal synchronously.
 */
void
ar_card_themes_request_themes (ArCardThemes *theme_manager)
{
  g_return_if_fail (AR_IS_CARD_THEMES (theme_manager));

  if (theme_manager->theme_infos_loaded)
    return;

  ar_card_themes_load_theme_infos (theme_manager);
}

/**
 * ar_card_themes_get_theme:
 * @theme_manager:
 * @info: a #ArCardThemeInfo
 *
 * Returns: a new #ArCardTheme for @info, or %NULL if there was an
 *  error while loading the theme.
 */
ArCardTheme *
ar_card_themes_get_theme (ArCardThemes *theme_manager,
                             ArCardThemeInfo *info)
{
  ArCardTheme *theme;
  GError *error = NULL;

  g_return_val_if_fail (AR_IS_CARD_THEMES (theme_manager), NULL);
  g_return_val_if_fail (info != NULL, NULL);

  if (info->type == G_TYPE_INVALID)
    return NULL;

  ar_profilestart ("loading card theme %s/%s", g_type_name (info->type), info->display_name);

  theme = g_object_new (info->type, "theme-info", info, NULL);
  if (!theme->klass->load (theme, &error)) {
    ar_debug_print (AR_DEBUG_CARD_THEME,
                        "Failed to load card theme %s/%s: %s\n",
                        g_type_name (info->type),
                        info->display_name,
                        error ? error->message : "(no error information)");

    g_clear_error (&error);
    g_object_unref (theme);
    theme = NULL;
  } else {
    ar_debug_print (AR_DEBUG_CARD_THEME,
                        "Successfully loaded card theme %s/%s\n",
                        g_type_name (info->type),
                        info->display_name);
  }

  ar_profileend ("loading card theme %s/%s", g_type_name (info->type), info->display_name);

  return theme;
}

/**
 * ar_card_themes_get_theme_by_name:
 * @theme_manager:
 * @theme_name: a theme name, or %NULL to get the default theme
 *
 * Gets a #ArCardTheme by its persistent name. If @theme_name is %NULL,
 * gets the defaul theme.
 *
 * Returns: a new #ArCardTheme for @theme_name, or %NULL if there was an
 *  error while loading the theme
 */
ArCardTheme *
ar_card_themes_get_theme_by_name (ArCardThemes *theme_manager,
                                     const char *theme_name)
{
  GType type;
  char *filename;
  ArCardThemeInfo *theme_info = NULL;

  g_return_val_if_fail (AR_IS_CARD_THEMES (theme_manager), NULL);

  filename = theme_filename_and_type_from_name (theme_name, &type);
  ar_debug_print (AR_DEBUG_CARD_THEME,
                      "Resolved card type=%s filename=%s\n",
                      g_type_name (type),
                      filename);

  if (filename == NULL || type == G_TYPE_INVALID)
    return NULL;

  /* First try to find the theme in our hash table */
  {
    ThemesByTypeAndFilenameData data = { type, filename, NULL };

    g_hash_table_foreach (theme_manager->theme_infos, (GHFunc) themes_foreach_by_type_and_filename, &data);

    theme_info = data.theme_info;
  }

  if (theme_info == NULL &&
      !theme_manager->theme_infos_loaded) {
    LookupData data = { filename, NULL };

    ar_card_themes_foreach_theme_dir (type, (ArCardThemeForeachFunc) ar_card_themes_try_theme_info_by_filename, &data);
    theme_info = data.theme_info;

    if (theme_info)
      g_hash_table_replace (theme_manager->theme_infos, theme_info->pref_name, theme_info);
  }

  g_free (filename);

  if (theme_info == NULL)
    return NULL;

  return ar_card_themes_get_theme (theme_manager, theme_info);
}

/**
 * ar_card_themes_get_theme_any:
 *
 * Loads all card themes until loading one succeeds, and returns it; or
 * %NULL if all card themes fail to load.
 *
 * Returns:
 */
ArCardTheme *
ar_card_themes_get_theme_any (ArCardThemes *theme_manager)
{
  ThemesAnyData data = { theme_manager, NULL };

  g_return_val_if_fail (AR_IS_CARD_THEMES (theme_manager), NULL);

  ar_debug_print (AR_DEBUG_CARD_THEME,
                      "Fallback: trying to load any theme\n");

  ar_card_themes_request_themes (theme_manager);

  g_hash_table_foreach (theme_manager->theme_infos, (GHFunc) themes_foreach_any, &data);

  return data.theme;
}

/**
 * ar_card_themes_get_themes_loaded:
 *
 * Returns: %TRUE iff the themes list has been loaded
 */
gboolean
ar_card_themes_get_themes_loaded (ArCardThemes *theme_manager)
{
  g_return_val_if_fail (AR_IS_CARD_THEMES (theme_manager), FALSE);

  return theme_manager->theme_infos_loaded;
}

/**
 * ar_card_themes_get_themes:
 *
 * Gets the list of known themes. Note that you may need to call
 * ar_card_themes_request_themes() first ensure the themes
 * information has been collected.
 * 
 * Returns: a newly allocated list of referenced #ArCardThemeInfo objects
 */
GList *
ar_card_themes_get_themes (ArCardThemes *theme_manager)
{
  GList *list = NULL;

  g_return_val_if_fail (AR_IS_CARD_THEMES (theme_manager), NULL);

  g_hash_table_foreach (theme_manager->theme_infos, (GHFunc) themes_foreach_add_to_list, &list);

  return g_list_sort (list, (GCompareFunc) _ar_card_theme_info_collate);
}

/**
 * ar_card_themes_install_themes:
 * @theme_manager:
 * @parent_window:
 * @user_time:
 *
 * Try to install more card themes.
 */
void
ar_card_themes_install_themes (ArCardThemes *theme_manager,
                               GtkWidget *parent_window,
                               guint user_time)
{
}
