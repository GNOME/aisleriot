/*
   Copyright © 2004 Callum McKenzie
   Copyright © 2007, 2008, 2009 Christian Persch

   This library is free software; you can redistribute it and'or modify
   it under the terms of the GNU Library General Public License as published 
   by the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

#include <config.h>

#include <string.h>
#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>

#ifdef ENABLE_CARD_THEMES_INSTALLER
#include <dbus/dbus-glib.h>
#endif

#ifdef GDK_WINDOWING_X11
#include <gdk/gdkx.h>
#endif

#include "games-debug.h"
#include "games-profile.h"
#include "games-runtime.h"

#include "games-card-themes.h"
#include "games-card-theme-private.h"

struct _GamesCardThemesClass {
  GObjectClass parent_class;
};

struct _GamesCardThemes {
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

static GType
theme_type_from_string (const char *type_str,
                        gssize type_str_len)
{
  const struct {
    const char name[6];
    GType type;
  } type_strings[] = {
#ifdef HAVE_RSVG
#ifdef ENABLE_CARD_THEME_FORMAT_SVG
    { "svg", GAMES_TYPE_CARD_THEME_SVG },
#endif
#ifdef ENABLE_CARD_THEME_FORMAT_KDE
    { "kde", GAMES_TYPE_CARD_THEME_KDE },
#endif
#endif /* HAVE_RSVG */
#ifndef HAVE_HILDON
#ifdef ENABLE_CARD_THEME_FORMAT_SLICED
    { "sliced", GAMES_TYPE_CARD_THEME_SLICED },
#endif
#ifdef ENABLE_CARD_THEME_FORMAT_PYSOL
    { "pysol", GAMES_TYPE_CARD_THEME_PYSOL },
#endif
#endif /* !HAVE_HILDON */
#ifdef ENABLE_CARD_THEME_FORMAT_FIXED
    { "fixed", GAMES_TYPE_CARD_THEME_FIXED }
#endif
  };
  GType type = G_TYPE_INVALID;

  if (type_str_len == 0) {
    /* Use the default type */
#ifdef HAVE_HILDON
    type = GAMES_TYPE_CARD_THEME_FIXED;
#else
    type = GAMES_TYPE_CARD_THEME_SVG;
#endif
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

static gboolean
games_card_themes_foreach_theme_dir (GType type,
                                     GamesCardThemeForeachFunc callback,
                                     gpointer data)
{
  GamesCardThemeClass *klass;
  gboolean retval;

  klass = g_type_class_ref (type);
  if (!klass)
    return TRUE;

  _games_profile_start ("foreach %s card themes", G_OBJECT_CLASS_NAME (klass));
  retval = _games_card_theme_class_foreach_theme_dir (klass, callback, data);
  _games_profile_end ("foreach %s card themes", G_OBJECT_CLASS_NAME (klass));

  g_type_class_unref (klass);
  return retval;
}

static gboolean
games_card_themes_foreach_theme_type_and_dir (GamesCardThemes *theme_manager,
                                              GamesCardThemeForeachFunc callback,
                                              gpointer data)
{
  const GType types[] = {
  /* List of supported theme types, in order of decreasing precedence */
#ifdef HAVE_RSVG
#ifdef ENABLE_CARD_THEME_FORMAT_SVG
  GAMES_TYPE_CARD_THEME_SVG,
#endif
#ifdef ENABLE_CARD_THEME_FORMAT_KDE
  GAMES_TYPE_CARD_THEME_KDE,
#endif
#endif /* HAVE_RSVG */
#ifndef HAVE_HILDON
#ifdef ENABLE_CARD_THEME_FORMAT_SLICED
  GAMES_TYPE_CARD_THEME_SLICED,
#endif
#ifdef ENABLE_CARD_THEME_FORMAT_PYSOL
  GAMES_TYPE_CARD_THEME_PYSOL,
#endif
#endif /* !HAVE_HILDON */
#ifdef ENABLE_CARD_THEME_FORMAT_FIXED
  GAMES_TYPE_CARD_THEME_FIXED
#endif
  };
  guint i;
  gboolean retval = TRUE;

  for (i = 0; i < G_N_ELEMENTS (types); ++i) {
    retval = games_card_themes_foreach_theme_dir (types[i], callback, data);
    if (!retval)
      break;
  }

  return retval;
}

static gboolean
games_card_themes_get_theme_infos_in_dir (GamesCardThemeClass *klass,
                                          const char *path,
                                          GamesCardThemes *theme_manager)
{
  GDir *iter;
  const char *filename;

  _games_profile_start ("looking for %s card themes in %s", G_OBJECT_CLASS_NAME (klass), path);

  iter = g_dir_open (path, 0, NULL);
  if (!iter)
    goto out;

  while ((filename = g_dir_read_name (iter)) != NULL) {
    GamesCardThemeInfo *info;

    _games_profile_start ("checking for %s card theme in file %s", G_OBJECT_CLASS_NAME (klass), filename);
    info = _games_card_theme_class_get_theme_info (klass, path, filename);
    _games_profile_end ("checking for %s card theme in file %s", G_OBJECT_CLASS_NAME (klass), filename);

    if (info)
      /* Replace existing info with the new one */
      g_hash_table_replace (theme_manager->theme_infos, info->pref_name, info);
  }
      
  g_dir_close (iter);

out:
  _games_profile_end ("looking for %s card themes in %s", G_OBJECT_CLASS_NAME (klass), path);

  return TRUE;
}

typedef struct {
  const char *filename;
  GamesCardThemeInfo *theme_info;
} LookupData;

static gboolean
games_card_themes_try_theme_info_by_filename (GamesCardThemeClass *klass,
                                              const char *path,
                                              LookupData *data)
{
  /* Try constructing the theme info */
  data->theme_info = _games_card_theme_class_get_theme_info (klass, path, data->filename);

  /* Continue until found */
  return data->theme_info == NULL;
}

static void
games_card_themes_load_theme_infos (GamesCardThemes *theme_manager)
{
  _games_profile_start ("looking for card themes");
  games_card_themes_foreach_theme_type_and_dir (theme_manager,
                                                (GamesCardThemeForeachFunc) games_card_themes_get_theme_infos_in_dir,
                                                theme_manager);
  _games_profile_end ("looking for card themes");

  g_signal_emit (theme_manager, signals[CHANGED], 0);
}

typedef struct {
  GType type;
  const char *filename;
  GamesCardThemeInfo *theme_info;
} FindData;

static void
find_by_type_and_name (gpointer key,
                       GamesCardThemeInfo *info,
                       FindData *data)
{
  if (info->type != data->type ||
      strcmp (info->filename, data->filename) != 0)
    return;

  if (data->theme_info == NULL)
    data->theme_info = info;
}

static void
foreach_add_to_list (gpointer key,
                     gpointer data,
                     GList **list)
{
  *list = g_list_prepend (*list, data);
}

static GamesCardThemeInfo *
games_card_themes_get_info_by_type_and_filename (GamesCardThemes *theme_manager,
                                                 GType type,
                                                 const char *filename)
{
  FindData data = { type, filename, NULL };

  g_hash_table_foreach (theme_manager->theme_infos, (GHFunc) find_by_type_and_name, &data);

  return data.theme_info;
}

#ifdef ENABLE_CARD_THEMES_INSTALLER

typedef struct {
  GamesCardThemes *theme_manager;
  DBusGProxy *proxy;
} ThemeInstallData;

static void
theme_install_data_free (ThemeInstallData *data)
{
  g_object_unref (data->theme_manager);
  g_object_unref (data->proxy);
  g_free (data);
}

static void
theme_install_reply_cb (DBusGProxy *proxy,
                        DBusGProxyCall *call,
                        ThemeInstallData *data)
{
  GamesCardThemes *theme_manager = data->theme_manager;
  GError *error = NULL;

  if (!dbus_g_proxy_end_call (proxy, call, &error, G_TYPE_INVALID)) {
    _games_debug_print (GAMES_DEBUG_CARD_THEME,
                        "Failed to call InstallPackages: %s\n",
                        error->message);
    g_error_free (error);
    return;
  }

  /* Installation succeeded. Now re-scan the theme directories */
  games_card_themes_load_theme_infos (theme_manager);
}

#endif /* ENABLE_CARD_THEMES_INSTALLER */

/* Class implementation */

G_DEFINE_TYPE (GamesCardThemes, games_card_themes, G_TYPE_OBJECT);

static void
games_card_themes_init (GamesCardThemes *theme_manager)
{
  /* Hash table: pref name => theme info */
  theme_manager->theme_infos = g_hash_table_new_full (g_str_hash, g_str_equal,
                                                      NULL /* key is owned by data */,
                                                      (GDestroyNotify) games_card_theme_info_unref);

  theme_manager->theme_infos_loaded = FALSE;
}

static void
games_card_themes_finalize (GObject *object)
{
  GamesCardThemes *theme_manager = GAMES_CARD_THEMES (object);

  g_hash_table_destroy (theme_manager->theme_infos);

  G_OBJECT_CLASS (games_card_themes_parent_class)->finalize (object);
}

static void
games_card_themes_class_init (GamesCardThemesClass * klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  gobject_class->finalize = games_card_themes_finalize;

  /**
   * GamesCardThemes:changed:
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
 * games_card_themes_new:
 *
 * Returns: a new #GamesCardThemes object
 */
GamesCardThemes *
games_card_themes_new (void)
{
  return g_object_new (GAMES_TYPE_CARD_THEMES, NULL);
}

/**
 * games_card_themes_request_themes:
 * @theme_manager:
 *
 * Scans all theme directories for themes, if necessary. If the
 * themes list has changed, emits the "changed" signal synchronously.
 */
void
games_card_themes_request_themes (GamesCardThemes *theme_manager)
{
  g_return_if_fail (GAMES_IS_CARD_THEMES (theme_manager));

  if (theme_manager->theme_infos_loaded)
    return;

  games_card_themes_load_theme_infos (theme_manager);
}

/**
 * games_card_themes_get_theme:
 * @theme_manager:
 * @info: a #GamesCardThemeInfo
 *
 * Returns: a new #GamesCardTheme for @info, or %NULL if there was an
 *  error while loading the theme.
 */
GamesCardTheme *
games_card_themes_get_theme (GamesCardThemes *theme_manager,
                             GamesCardThemeInfo *info)
{
  GamesCardTheme *theme;
  GError *error = NULL;

  g_return_val_if_fail (GAMES_IS_CARD_THEMES (theme_manager), NULL);
  g_return_val_if_fail (info != NULL, NULL);

  if (info->type == G_TYPE_INVALID)
    return NULL;

  _games_profile_start ("loading card theme %s/%s", g_type_name (info->type), info->display_name);

  theme = g_object_new (info->type, "theme-info", info, NULL);
  if (!theme->klass->load (theme, &error)) {
    _games_debug_print (GAMES_DEBUG_CARD_THEME,
                        "Failed to load card theme %s/%s: %s\n",
                        g_type_name (info->type),
                        info->display_name,
                        error ? error->message : "(no error information)");

    g_clear_error (&error);
    g_object_unref (theme);
    theme = NULL;
  } else {
    _games_debug_print (GAMES_DEBUG_CARD_THEME,
                        "Successfully loaded card theme %s/%s\n",
                        g_type_name (info->type),
                        info->display_name);
  }

  _games_profile_end ("loading card theme %s/%s", g_type_name (info->type), info->display_name);

  return theme;
}

/**
 * games_card_themes_get_theme_info_by_name:
 * @theme_name: a theme name
 *
 * This function exists only for backward compatibility with
 * older aisleriot versions' preferences.
 * 
 * Returns: a new #GamesCardTheme for @name, or %NULL if there was an
 *  error while loading the theme.
 */
GamesCardThemeInfo *
games_card_themes_get_theme_info_by_name (GamesCardThemes *theme_manager,
                                          const char *theme_name)
{
  const char *colon, *filename, *dot;
  char *free_me = NULL;
  GType type;
  LookupData data;
  GamesCardThemeInfo *theme_info = NULL;

  g_return_val_if_fail (GAMES_IS_CARD_THEMES (theme_manager), NULL);

  if (!theme_name || !theme_name[0])
    return NULL;

  colon = strchr (theme_name, ':');
  type = theme_type_from_string (theme_name, colon ? colon - theme_name : 0);
  if (type == G_TYPE_INVALID)
    return NULL;

  if (colon) {
    /* Get the filename from the theme name */

    filename = colon + 1;
    dot = strrchr (filename, '.');

    if (dot == NULL) {
      /* No dot? Try appending the default, for compatibility with old settings */
#if defined(HAVE_HILDON) && defined(ENABLE_CARD_THEME_FORMAT_FIXED)
      if (type == GAMES_TYPE_CARD_THEME_FIXED) {
        filename = free_me = g_strconcat (filename, ".card-theme", NULL);
      }
#elif defined(HAVE_RSVG) && defined(ENABLE_CARD_THEME_FORMAT_SVG)
      if (type == GAMES_TYPE_CARD_THEME_SVG) {
        filename = free_me = g_strconcat (filename, ".svg", NULL);
      }
#endif
    } else {
#if defined(HAVE_GNOME) && defined(ENABLE_CARD_THEME_FORMAT_SVG)
      if (type == GAMES_TYPE_CARD_THEME_SVG &&
          g_str_has_suffix (filename, ".png")) {
        char *base_name;

        /* Very old version; replace .png with .svg */
        base_name = g_strndup (filename, dot - filename);
        filename = free_me = g_strconcat (base_name, ".svg", NULL);
        g_free (base_name);
      }
#endif /* HAVE_GNOME && ENABLE_CARD_THEME_FORMAT_SVG */
    }
  } else {
    filename = theme_name;
  }
  if (!filename[0])
    goto out;

  /* First try to find the theme in our hash table */
  theme_info = games_card_themes_get_info_by_type_and_filename (theme_manager,
                                                                type,
                                                                filename);
  if (theme_info)
    goto out;

  /* Not in our hash table, and the list is uptodate? No such theme! */
  if (theme_manager->theme_infos_loaded)
    goto out;

  /* Then, try to find it in one of the theme dirs */
  data.filename = filename;
  data.theme_info = NULL;
  games_card_themes_foreach_theme_dir (type, (GamesCardThemeForeachFunc) games_card_themes_try_theme_info_by_filename, &data);
  theme_info = data.theme_info;

out:
  g_free (free_me);

  return theme_info;
}

/**
 * games_card_themes_get_default_theme_info:
 * @theme_manager:
 *
 * Note that you need to call games_card_themes_request_themes() first.
 *
 * Returns: the #GamesCardThemeInfo for the default theme, or %NULL if
 *   the default theme couldn't be found
 */
GamesCardThemeInfo *
games_card_themes_get_default_theme_info (GamesCardThemes *theme_manager)
{
  GType type;
  char *filename;
  GamesCardThemeInfo *theme_info;

#ifdef HAVE_HILDON
  type = GAMES_TYPE_CARD_THEME_FIXED;
  filename = g_strconcat (GAMES_CARD_THEME_DEFAULT, ".card-theme", NULL);
#else
  type = GAMES_TYPE_CARD_THEME_SVG;
  filename = g_strconcat (GAMES_CARD_THEME_DEFAULT, ".svg", NULL);
#endif

  theme_info = games_card_themes_get_info_by_type_and_filename (theme_manager, type, filename);
  g_free (filename);

  return theme_info;
}

/**
 * games_card_themes_get_theme_any:
 *
 * Loads all card themes until loading one succeeds, and returns it; or
 * %NULL if all card themes fail to load.
 *
 * Returns:
 */
GamesCardTheme *
games_card_themes_get_theme_any (GamesCardThemes *theme_manager)
{
//   GList *l;

  g_return_val_if_fail (GAMES_IS_CARD_THEMES (theme_manager), NULL);

/*  if (!theme_infos)
    return NULL;

  for (l = theme_infos; l != NULL; l = l->next) {
    GamesCardThemeInfo *info = (GamesCardThemeInfo *) l->data;
    GamesCardTheme *theme;

    theme = games_card_theme_get (info);
    if (theme)
      return theme;
  }
*/
  return NULL;
}

/**
 * games_card_themes_get_themes:
 *
 * Gets the list of known themes. Note that you may need to call
 * games_card_themes_request_themes() first ensure the themes
 * information has been collected.
 * 
 * Returns: a newly allocated list of referenced #GamesCardThemeInfo objects
 */
GList *
games_card_themes_get_themes (GamesCardThemes *theme_manager)
{
  GList *list = NULL;

  g_return_val_if_fail (GAMES_IS_CARD_THEMES (theme_manager), NULL);

  g_hash_table_foreach (theme_manager->theme_infos, (GHFunc) foreach_add_to_list, &list);

  return g_list_sort (list, (GCompareFunc) _games_card_theme_info_collate);
}


/**
 * games_card_themes_can_install_themes:
 * @theme_manager:
 *
 * Returns: whether the new theme installer is supported
 */
gboolean
games_card_themes_can_install_themes (GamesCardThemes *theme_manager)
{
#ifdef ENABLE_CARD_THEMES_INSTALLER
  return TRUE;
#else
  return FALSE;
#endif
}

/**
 * games_card_themes_install_themes:
 * @theme_manager:
 * @parent_window:
 * @user_time:
 *
 * Try to install more card themes.
 */
void
games_card_themes_install_themes (GamesCardThemes *theme_manager,
                                  GtkWindow *parent_window,
                                  guint user_time)
{
#ifdef ENABLE_CARD_THEMES_INSTALLER
  /* FIXME: more packages, and test with other distros beside ubuntu! */
  const char *packages[] = {
    "gnome-games-extra-data",
    NULL
  };

  DBusGConnection *connection;
  ThemeInstallData *data;
  guint xid = 0;
  GError *error = NULL;

  connection = dbus_g_bus_get (DBUS_BUS_SESSION, &error);
  if (!connection) {
    _games_debug_print (GAMES_DEBUG_CARD_THEME,
                        "Failed to get the session bus: %s\n",
                        error->message);
    g_error_free (error);
    return;
  }

  data = g_new (ThemeInstallData, 1);
  data->theme_manager = g_object_ref (theme_manager);

  /* PackageKit-GNOME interface */
  data->proxy = dbus_g_proxy_new_for_name (connection,
                                           "org.freedesktop.PackageKit",
                                           "/org/freedesktop/PackageKit",
                                           "org.freedesktop.PackageKit");
  g_assert (data->proxy != NULL); /* the call above never fails */

#ifdef GDK_WINDOWING_X11
  if (parent_window) {
    xid = GDK_WINDOW_XID (GTK_WIDGET (parent_window)->window);
  }
#endif

  /* Installing can take a long time; don't do the automatic timeout */
  dbus_g_proxy_set_default_timeout (data->proxy, G_MAXINT);

  if (!dbus_g_proxy_begin_call (data->proxy,
                                "InstallPackageNames",
                                (DBusGProxyCallNotify) theme_install_reply_cb,
                                data,
                                (GDestroyNotify) theme_install_data_free,
                                G_TYPE_UINT, xid,
                                G_TYPE_UINT, user_time,
                                G_TYPE_STRV, packages,
                                G_TYPE_INVALID)) {
    /* Failed; cleanup. FIXME: can this happen at all? */
    _games_debug_print (GAMES_DEBUG_CARD_THEME,
                        "Failed to call the InstallPackages method\n");

    theme_install_data_free (data);
  }
#endif
}
