/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008 Christian Persch

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
#include "games-profile.h"
#include "games-runtime.h"

#include "games-card-theme.h"
#include "games-card-theme-private.h"

enum {
  PROP_0,
  PROP_THEME_INFO
};

enum {
  CHANGED,
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

/* Class implementation */

G_DEFINE_ABSTRACT_TYPE (GamesCardTheme, games_card_theme, G_TYPE_OBJECT);

static void
games_card_theme_init (GamesCardTheme * theme)
{
}

static GObject *
games_card_theme_constructor (GType type,
                              guint n_construct_properties,
                              GObjectConstructParam *construct_params)
{
  GObject *object;
  GamesCardTheme *theme;

  object = G_OBJECT_CLASS (games_card_theme_parent_class)->constructor
             (type, n_construct_properties, construct_params);

  theme = GAMES_CARD_THEME (object);

  g_assert (theme->theme_info != NULL);

  /* NOTE! We have to do this here, since it returns the wrong class
   * (GamesCardThemeClass) when called in games_card_theme_init() !
   */
  theme->klass = GAMES_CARD_THEME_GET_CLASS (theme);

  return object;
}

static void
games_card_theme_finalize (GObject *object)
{
  GamesCardTheme *theme = GAMES_CARD_THEME (object);

  games_card_theme_info_unref (theme->theme_info);

  G_OBJECT_CLASS (games_card_theme_parent_class)->finalize (object);
}

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

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static GamesCardThemeInfo *
games_card_theme_class_get_theme_info (GamesCardThemeClass *klass,
                                       const char *dir,
                                       const char *filename)
{
  return NULL;
}

static void
games_card_theme_class_init (GamesCardThemeClass * klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  gobject_class->set_property = games_card_theme_set_property;
  gobject_class->constructor = games_card_theme_constructor;
  gobject_class->finalize = games_card_theme_finalize;

  klass->get_theme_info = games_card_theme_class_get_theme_info;

  g_object_class_install_property
    (gobject_class,
     PROP_THEME_INFO,
     g_param_spec_boxed ("theme-info", NULL, NULL,
                         GAMES_TYPE_CARD_THEME_INFO,
                         G_PARAM_WRITABLE |
                         G_PARAM_STATIC_NAME |
                         G_PARAM_STATIC_NICK |
                         G_PARAM_STATIC_BLURB |
                         G_PARAM_CONSTRUCT_ONLY));

  /**
   * GamesCardTheme:changed:
   * @theme: the object on which the signal is emitted
   *
   * The ::changed signal is emitted when the card theme has
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
}

/* private API */

void
_games_card_theme_emit_changed (GamesCardTheme *theme)
{
  g_signal_emit (theme, signals[CHANGED], 0);
}

GamesCardThemeInfo *
_games_card_theme_class_get_theme_info (GamesCardThemeClass *klass,
                                        const char *dir,
                                        const char *filename)
{
  return klass->get_theme_info (klass, dir, filename);
}

gboolean
_games_card_theme_class_foreach_theme_dir (GamesCardThemeClass *klass,
                                           GamesCardThemeForeachFunc callback,
                                           gpointer data)
{
  return klass->foreach_theme_dir (klass, callback, data);
}

gboolean
_games_card_theme_class_foreach_env (GamesCardThemeClass *klass,
                                     const char *env,
                                     GamesCardThemeForeachFunc callback,
                                     gpointer data)
{
  const char *value;
  char **paths;
  guint i;
  gboolean retval = TRUE;

  value = g_getenv (env);
  if (!value || !value[0])
    return TRUE;

  paths = g_strsplit (value, G_SEARCHPATH_SEPARATOR_S, -1);
  if (!paths)
    return TRUE;

  for (i = 0; paths[i]; ++i) {
    const char *path = paths[i];

    if (!paths[0])
      continue;

    retval = callback (klass, path, data);
    if (!retval)
      break;
  }

  g_strfreev (paths);

  return retval;
}

/* public API */

#if GTK_CHECK_VERSION (2, 10, 0)

/**
 * games_card_theme_set_font_options:
 * @theme:
 * @font_options: the #cairo_font_options_t to use
 *
 * Sets the font options to use when drawing the card images.
 */
void
games_card_theme_set_font_options (GamesCardTheme *theme,
                                   const cairo_font_options_t *font_options)
{
  g_return_if_fail (GAMES_IS_CARD_THEME (theme));

  if (!theme->klass->set_font_options)
    return;

  theme->klass->set_font_options (theme, font_options);
}

#endif /* GTK 2.10.0 */


/**
 * games_card_theme_get_theme_info:
 * @theme:
 *
 * Returns: the #GamesCardThemeInfo corresponding to @theme.
 */
GamesCardThemeInfo *
games_card_theme_get_theme_info (GamesCardTheme *theme)
{
  g_return_val_if_fail (GAMES_IS_CARD_THEME (theme), NULL);

  return theme->theme_info;
}

/**
 * games_card_theme_set_size:
 * @theme:
 * @width: the maximum width
 * @height: the maximum height
 * @proportion: how much of @width and @height to use for the cards
 *
 * Calculates the card size to use. The passed-in dimensions are not used
 * directly; instead the width and height used are calculated using the
 * card theme's aspect ratio and, if using a prerendered card theme, from the
 * available sizes. @theme.card_size will contain the card size afterwards.
 * If the card size was changed, the cards cache will be cleared.
 *
 * Returns: %TRUE iff the card size was changed
 */
gboolean
games_card_theme_set_size (GamesCardTheme *theme,
                           int width,
                           int height,
                           double proportion)
{
  g_return_val_if_fail (GAMES_IS_CARD_THEME (theme), FALSE);

  return theme->klass->set_card_size (theme, width, height, proportion);
}

/**
 * games_card_theme_get_size:
 * @theme:
 * @size: location to store the card size
 *
 * Returns the currently selected card size in @size.
 */
void
games_card_theme_get_size (GamesCardTheme *theme,
                           CardSize *size)
{
  theme->klass->get_card_size (theme, size);
}

/**
 * games_card_theme_get_aspect:
 * @theme:
 *
 * Returns: the aspect ratio of the cards in the currently loaded theme
 */
double
games_card_theme_get_aspect (GamesCardTheme * theme)
{
  g_return_val_if_fail (GAMES_IS_CARD_THEME (theme), 1.0);

  return theme->klass->get_card_aspect (theme);
}

/**
 * games_card_theme_get_card_pixbuf:
 * @theme:
 * @card_id:
 *
 * Returns a #GdkPixbuf for the selected card using the currently loaded
 * theme and the currently selected size.
 *
 * Returns: a new #GdkPixbuf, or %NULL if there was an error
 */
GdkPixbuf *
games_card_theme_get_card_pixbuf (GamesCardTheme *theme,
                                  int card_id)
{
  GdkPixbuf *pixbuf;

  g_return_val_if_fail ((card_id >= 0) && (card_id < GAMES_CARDS_TOTAL), NULL);

  _games_profile_start ("loading card %d from theme %s", card_id, theme->theme_info->display_name);

  pixbuf = theme->klass->get_card_pixbuf (theme, card_id);

  _games_profile_end ("loading card %d from theme %s", card_id, theme->theme_info->display_name);

  return pixbuf;
}

/* GamesCardThemeInfo impl */

static int
theme_type_compare (GType a,
                    GType b)
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
  guint ia, ib;

  for (ia = 0; ia < G_N_ELEMENTS (types); ++ia)
    if (types[ia] == a)
      break;
  for (ib = 0; ib < G_N_ELEMENTS (types); ++ib)
    if (types[ib] == b)
      break;

  return ia - ib;
}

/* private API */

/**
 * _games_card_theme_info_new:
 * @type:
 * @path:
 * @filename:
 * @diplay_name:
 * @pref_name:
 * @data:
 * @destroy_notify:
 *
 * Returns: a new #GamesCardThemeInfo with refcount 1
 */
GamesCardThemeInfo *
_games_card_theme_info_new (GType type,
                            const char *path,
                            const char *filename,
                            char *display_name /* adopts */,
                            char *pref_name /* adopts */,
                            gpointer data /* adoptes */,
                            GDestroyNotify destroy_notify)
{
  GamesCardThemeInfo *info;

#if GLIB_CHECK_VERSION (2, 10, 0)
  info = g_slice_new (GamesCardThemeInfo);
#else
  info = g_new (GamesCardThemeInfo, 1);
#endif

  info->ref_count = 1;
  info->type = type;
  info->path = g_strdup (path);
  info->filename = g_strdup (filename);
  info->display_name = display_name;
  info->pref_name = pref_name;
  info->data = data;
  info->destroy_notify = destroy_notify;

  _games_debug_print (GAMES_DEBUG_CARD_THEME,
                      "Created GamesCardThemeInfo for type=%s path=%s filename=%s display-name=%s\n",
                      g_type_name (type), path, filename, display_name);

  return info;
}

guint
_games_card_theme_info_hash (const GamesCardThemeInfo *a)
{
  return g_int_hash (&a->type) ^
         g_str_hash (a->path) ^
         g_str_hash (a->filename);
}

/**
 * _games_card_theme_info_collate:
 * @a:
 * @b:
 *
 * Compares @a and @b.
 *
 * Returns: %-1 if @a comes before @b, %1 if @b comes before @a, or
 * %0 if @a and @b are equal.
 */
int
_games_card_theme_info_collate (const GamesCardThemeInfo *a,
                                const GamesCardThemeInfo *b)
{
  g_return_val_if_fail (a != NULL && b != NULL, 0);

  if (a->type != b->type)
    return theme_type_compare (a->type, b->type);

  return g_utf8_collate (a->display_name, b->display_name);
}

/* public API */

#if defined(G_DEFINE_BOXED_TYPE)
G_DEFINE_BOXED_TYPE (GamesCardThemeInfo, games_card_theme_info,
                     games_card_theme_info_ref,
                     games_card_theme_info_unref);
#else
GType
games_card_theme_info_get_type (void)
{
  static GType type = 0;

  if (G_UNLIKELY (type == 0)) {
    type = g_boxed_type_register_static ("GamesCardThemeInfo",
                                         (GBoxedCopyFunc) games_card_theme_info_ref,
                                         (GBoxedFreeFunc) games_card_theme_info_unref);
  }

  return type;
}
#endif /* defined(G_DEFINE_BOXED_TYPE) */

/**
 * games_card_theme_info_ref:
 * @info:
 *
 * Refs @info.
 *
 * Returns: @info
 */
GamesCardThemeInfo *
games_card_theme_info_ref (GamesCardThemeInfo *info)
{
  g_return_val_if_fail (info != NULL, NULL);

  info->ref_count++;
  return info;
}

/**
 * games_card_theme_info_unref:
 * @info:
 *
 * Unrefs @info. If the refcount reaches %0, @info is freed.
 */
void
games_card_theme_info_unref (GamesCardThemeInfo *info)
{
  g_return_if_fail (info != NULL);

  if (--info->ref_count > 0)
    return;

  g_free (info->path);
  g_free (info->filename);
  g_free (info->display_name);
  g_free (info->pref_name);

  if (info->data && info->destroy_notify)
    info->destroy_notify (info->data);

#if GLIB_CHECK_VERSION (2, 10, 0)
  g_slice_free (GamesCardThemeInfo, info);
#else
  g_free (info);
#endif
}

/**
 * games_card_theme_info_get_display_name :
 * @info:
 *
 * Returns: the user readable name of @info
 */
const char *
games_card_theme_info_get_display_name (GamesCardThemeInfo *info)
{
  g_return_val_if_fail (info != NULL, NULL);

  return info->display_name;
}

/**
 * games_card_theme_info_get_persistent_name :
 * @info:
 *
 * Returns: the user readable name of @info
 */
const char *
games_card_theme_info_get_persistent_name (GamesCardThemeInfo *info)
{
  g_return_val_if_fail (info != NULL, NULL);

  g_return_val_if_fail (info->pref_name, NULL);

  return info->pref_name;
}

/**
 * games_card_theme_info_equal:
 *
 * Returns: %TRUE iff @a and @b refer to the same card theme
 */
gboolean
games_card_theme_info_equal (const GamesCardThemeInfo *a,
                              const GamesCardThemeInfo *b)
{
  g_return_val_if_fail (a != NULL && b != NULL, FALSE);

  return a->type == b->type &&
         strcmp (a->path, b->path) == 0 &&
         strcmp (a->filename, b->filename) == 0;
}
