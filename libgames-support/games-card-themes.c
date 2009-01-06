/*
   Copyright © 2004 Callum McKenzie
   Copyright © 2007, 2008 Christian Persch

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
  gboolean theme_infos_update_needed;
};

enum {
  PROP_0
};

enum {
  CHANGED,
  LAST_SIGNAL
};

/*static guint signals[LAST_SIGNAL];*/

static gboolean
_games_card_themes_get_theme_info_foreach (GamesCardThemeClass *klass,
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
      g_hash_table_insert (theme_manager->theme_infos, info->pref_name, info);
  }
      
  g_dir_close (iter);

out:
  _games_profile_end ("looking for %s card themes in %s", G_OBJECT_CLASS_NAME (klass), path);

  return TRUE;
}

static void
_games_card_theme_ensure_theme_infos (GamesCardThemes *theme_manager)
{
  /* FIXME: take env vars and prefs into account on ordering here */
  GType types[] = {
#ifdef HAVE_RSVG
    GAMES_TYPE_CARD_THEME_SVG,
    GAMES_TYPE_CARD_THEME_KDE,
#endif
#ifndef HAVE_HILDON
    GAMES_TYPE_CARD_THEME_SLICED,
    GAMES_TYPE_CARD_THEME_PYSOL,
#endif
    GAMES_TYPE_CARD_THEME_FIXED
  };
  guint i;

  if (!theme_manager->theme_infos_update_needed)
    return;

  _games_profile_start ("looking for card themes");

  for (i = 0; i < G_N_ELEMENTS (types); ++i) {
    GamesCardThemeClass *klass;

    klass = g_type_class_ref (types[i]);
    if (!klass)
      continue;

    _games_profile_start ("looking for %s card themes", G_OBJECT_CLASS_NAME (klass));
    _games_card_theme_class_foreach_theme_dir (klass,
                                               (GamesCardThemeForeachFunc) _games_card_themes_get_theme_info_foreach,
                                               theme_manager);
    _games_profile_end ("looking for %s card themes", G_OBJECT_CLASS_NAME (klass));

    g_type_class_unref (klass);
  }

  _games_profile_end ("looking for card themes");
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

  data->theme_info = info;
}

static GamesCardThemeInfo *
_games_card_themes_get_info_by_type_and_name (GamesCardThemes *theme_manager,
                                              GType type,
                                              const char *filename)
{
  FindData data = { type, filename, NULL };

  g_hash_table_foreach (theme_manager->theme_infos, (GHFunc) find_by_type_and_name, &data);

  return data.theme_info;
}

static void
foreach_add_to_list (gpointer key,
                     gpointer data,
                     GList **list)
{
  *list = g_list_prepend (*list, data);
}

/* Class implementation */

G_DEFINE_TYPE (GamesCardThemes, games_card_themes, G_TYPE_OBJECT);

static void
games_card_themes_init (GamesCardThemes *theme_manager)
{
  /* Hash table: pref name => theme info */
  theme_manager->theme_infos = g_hash_table_new_full (g_str_hash, g_str_equal,
                                                      NULL /* key is owned by data */,
                                                      (GDestroyNotify) games_card_theme_info_unref);

  theme_manager->theme_infos_update_needed = TRUE;
}

static void
games_card_themes_finalize (GObject *object)
{
  GamesCardThemes *theme_manager = GAMES_CARD_THEMES (object);

  g_hash_table_destroy (theme_manager->theme_infos);

  G_OBJECT_CLASS (games_card_themes_parent_class)->finalize (object);
}

#if 0
static void
games_card_theme_set_property (GObject * object,
                               guint prop_id,
                               const GValue * value, GParamSpec * pspec)
{
  GamesCardTheme *theme = GAMES_CARD_THEME (object);

  switch (prop_id) {
    case PROP_THEME_INFO:
      theme->theme_info = g_value_dup_boxed (value);
      break;
  }
}
#endif

static void
games_card_themes_class_init (GamesCardThemesClass * klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

//  gobject_class->set_property = games_card_themes_set_property;
  gobject_class->finalize = games_card_themes_finalize;

#if 0
  /**
   * GamesCardTheme:changed:
   * @themes: the object on which the signal is emitted
   *
   * The ::changed signal is emitted when the card themes has
   * changed in any way that makes it necessary to re-render
   * any displayed or cached images.
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
#endif
}

/* private API */

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

  g_return_val_if_fail (info != NULL, NULL);

  if (info->type == G_TYPE_INVALID)
    return NULL;

  _games_profile_start ("loading %s card theme %s", g_type_name (info->type), info->display_name);

  theme = g_object_new (info->type, "theme-info", info, NULL);
  if (!theme->klass->load (theme, &error)) {
    _games_debug_print (GAMES_DEBUG_CARD_THEME,
                        "Failed to load card theme %s: %s\n",
                        info->display_name, error ? error->message : "(no error information)");

    g_clear_error (&error);
    g_object_unref (theme);
    theme = NULL;
  } else {
    _games_debug_print (GAMES_DEBUG_CARD_THEME,
                        "Successfully loaded card theme %s\n",
                        info->display_name);
  }

  _games_profile_end ("loading %s card theme %s", g_type_name (info->type), info->display_name);

  return theme;
}

/**
 * games_card_themes_get_theme_by_name:
 * @theme_name: a theme name
 *
 * This function exists only for backward compatibility with
 * older aisleriot versions' preferences.
 * 
 * Returns: a new #GamesCardTheme for @name, or %NULL if there was an
 *  error while loading the theme.
 */
GamesCardTheme *
games_card_themes_get_theme_by_name (GamesCardThemes *theme_manager,
                                     const char *theme_name)
{
  char *colon, *free_me = NULL;
  const char *filename;
  gsize type_str_len;
  GType type = G_TYPE_INVALID;
  GamesCardThemeInfo *theme_info = NULL;

  if (!theme_name || !theme_name[0])
    goto default_fallback;

  _games_card_theme_ensure_theme_infos (theme_manager);

  colon = strchr (theme_name, ':');
  if (colon) {
    type_str_len = colon - theme_name;

    filename = colon + 1;

#ifdef HAVE_RSVG
    if (strncmp (theme_name, "svg", type_str_len) == 0) {
      type = GAMES_TYPE_CARD_THEME_SVG;
    } else if (strncmp (theme_name, "kde", type_str_len) == 0) {
      type = GAMES_TYPE_CARD_THEME_KDE;
    } else
#endif
#ifndef HAVE_HILDON
    if (strncmp (theme_name, "sliced", type_str_len) == 0) {
      type = GAMES_TYPE_CARD_THEME_SLICED;
    } else if (strncmp (theme_name, "pysol", type_str_len) == 0) {
      type = GAMES_TYPE_CARD_THEME_PYSOL;
      filename = free_me = g_strdup_printf ("cardset-%s", filename);
    } else
#endif
    if (strncmp (theme_name, "fixed", type_str_len) == 0)
      type = GAMES_TYPE_CARD_THEME_FIXED;
  } else {
#ifdef HAVE_GNOME
    /* Compatibility with old settings */
#ifdef HAVE_RSVG
    if (g_str_has_suffix (theme_name, ".svg")) {
      type = GAMES_TYPE_CARD_THEME_SVG;
      filename = theme_name;
    } else
#endif
    if (g_str_has_suffix (theme_name, ".png")) {
      type = GAMES_TYPE_CARD_THEME_SLICED;
      filename = theme_name;
    } else
#endif /* HAVE_GNOME */
    {
#ifdef HAVE_HILDON
      type = GAMES_TYPE_CARD_THEME_FIXED;
      filename = free_me = g_strconcat (theme_name, ".card-theme", NULL);
#else
      type = GAMES_TYPE_CARD_THEME_SVG;
      filename = free_me = g_strconcat (theme_name, ".svg", NULL);
#endif
    }
  }
  if (type == G_TYPE_INVALID)
    return NULL;

  theme_info = _games_card_themes_get_info_by_type_and_name (theme_manager, type, filename);
  g_free (free_me);

default_fallback:

  if (!theme_info) {
    /* Try falling back to the default */
#ifdef HAVE_HILDON
    type = GAMES_TYPE_CARD_THEME_FIXED;
    filename = free_me = g_strconcat (GAMES_CARD_THEME_DEFAULT, ".card-theme", NULL);
#else
    type = GAMES_TYPE_CARD_THEME_SVG;
    filename = free_me = g_strconcat (GAMES_CARD_THEME_DEFAULT, ".svg", NULL);
#endif
    theme_info = _games_card_themes_get_info_by_type_and_name (theme_manager, type, filename);
    g_free (free_me);
  }

  if (theme_info)
    return games_card_themes_get_theme (theme_manager, theme_info);

  return NULL;
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

  _games_card_theme_ensure_theme_infos (theme_manager);

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
 * games_card_themes_get_theme_all:
 *
 * Returns:
 */
GList *
games_card_themes_get_theme_all (GamesCardThemes *theme_manager)
{
  GList *list = NULL;

  _games_card_theme_ensure_theme_infos (theme_manager);

  g_hash_table_foreach (theme_manager->theme_infos, (GHFunc) foreach_add_to_list, &list);

  return g_list_sort (list, (GCompareFunc) _games_card_theme_info_collate);
}
