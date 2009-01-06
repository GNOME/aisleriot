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

#include "games-find-file.h"
#include "games-files.h"
#include "games-preimage.h"
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

static GList *theme_infos;

/* #defining this prints out the time it takes to render the theme */
/* #define INSTRUMENT_LOADING */


#ifdef INSTRUMENT_LOADING
static long totaltime = 0;
#endif

#if 0
static GType
get_default_theme_type (void)
{
  GType type;
  const char *env;

#if defined(HAVE_RSVG) && !defined(HAVE_HILDON)
  /* Default to scalable */
  type = GAMES_TYPE_CARD_THEME_SVG;
#else
  /* Default to non-scalable */
  type = GAMES_TYPE_CARD_THEME_FIXED;
#endif

#ifndef HAVE_HILDON
  env = g_getenv ("GAMES_CARD_THEME_FORMAT");
  if (env) {
#ifdef HAVE_RSVG
    if (strcmp (env, "svg") == 0)
      type = GAMES_TYPE_CARD_THEME_SVG;
    else if (strcmp (env, "kde") == 0)
      type = GAMES_TYPE_CARD_THEME_KDE;
    else
#endif
    if (strcmp (env, "sliced") == 0)
      type = GAMES_TYPE_CARD_THEME_SLICED;
    else if (strcmp (env, "fixed") == 0)
      type = GAMES_TYPE_CARD_THEME_FIXED;
  }
#endif /* !HAVE_HILDON */

  return type;
}
#endif

static void
_games_card_theme_ensure_theme_infos (void)
{
  if (theme_infos)
    return;

  /* FIXME: take env vars and prefs into account on ordering here */
  GType types[] = {
#ifdef HAVE_RSVG
    GAMES_TYPE_CARD_THEME_SVG,
    GAMES_TYPE_CARD_THEME_KDE,
#endif
#ifndef HAVE_HILDON
    GAMES_TYPE_CARD_THEME_SLICED,
#endif
    GAMES_TYPE_CARD_THEME_FIXED
  };
  guint i;

  theme_infos = NULL;

  for (i = 0; i < G_N_ELEMENTS (types); ++i) {
    GamesCardThemeClass *klass;

    klass = g_type_class_ref (types[i]);
    if (!klass)
      continue;

    klass->get_theme_infos (klass, &theme_infos);
    g_type_class_unref (klass);
  }

  theme_infos = g_list_reverse (theme_infos);
}

#if 0
static gboolean
games_card_theme_load_theme_with_fallback (GamesCardTheme *theme,
                                           GamesCardThemeInfo *info)
{
  const char *env;

  if ((env = g_getenv ("GAMES_CARD_THEME_NAME")))
    theme_name = env;

  if (!theme_dir)
    theme_dir = _games_card_theme_class_get_default_theme_path (theme->klass);

  if (games_card_theme_load_theme (theme, theme_dir, theme_name))
    return TRUE;

  // FIXMEchpe: compare strict dir equality, not just != NULL
  /* Try fallback in default theme directory */
  if (theme_dir != NULL &&
      games_card_theme_load_theme (theme, NULL, theme_name))
    return TRUE;

  g_warning ("Failed to load theme '%s'; trying fallback theme '%s'",
             theme_name, GAMES_CARD_THEME_DEFAULT);

  if (strcmp (theme_name, GAMES_CARD_THEME_DEFAULT) != 0 &&
      (games_card_theme_load_theme (theme, theme_dir, GAMES_CARD_THEME_DEFAULT)))
    return FALSE;
  if (theme_dir != NULL &&
      strcmp (theme_name, GAMES_CARD_THEME_DEFAULT) != 0 &&
      (games_card_theme_load_theme (theme, NULL, GAMES_CARD_THEME_DEFAULT)))
    return FALSE;

  g_warning ("Failed to load fallback theme!");

  return FALSE;
  //FIXMEchpe how to design fallback now...
//  return games_card_theme_load_theme (theme, info);
}
#endif

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
  }
}

static GamesCardThemeInfo *
games_card_theme_class_get_theme_info (GamesCardThemeClass *klass,
                                       const char *dir,
                                       const char *filename)
{
  return NULL;
}

#if 0

static GList *
games_card_theme_get_themes_list (GamesCardThemeClass *klass,
                                  const char *theme_dir)
{
  GamesFileList *files;
  GList *l, *list;
  const char *glob;
  GamesCardThemeInfo * (* get_theme_info) (GamesCardThemeClass *, const char *) = klass->get_theme_info;

  glob = _games_card_theme_class_get_theme_glob (klass);

  if (!theme_dir)
    theme_dir = _games_card_theme_class_get_default_theme_path (klass);

  if (!theme_dir || !glob)
    return NULL;

  files = games_file_list_new (glob, theme_dir, NULL);
  games_file_list_transform_basename (files);

  for (l = files->list; l != NULL; l = l->next) {
    const char *filename = (const char *) l->data;
    char *dot;

    dot = strrchr (filename, '.');
    if (dot) {
      *dot = '\0';
    }
  }

  list = files->list;
  files->list = NULL;
  g_object_unref (files);

  return list;
}
#endif

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

void
_games_card_theme_class_append_theme_info_foreach (GamesCardThemeClass *klass,
                                                   const char *path,
                                                   GList **list)
{
  GDir *iter;
  const char *filename;

  iter = g_dir_open (path, 0, NULL);
  if (!iter)
    return;

  while ((filename = g_dir_read_name (iter)) != NULL) {
    GamesCardThemeInfo *info;

    info = _games_card_theme_class_get_theme_info (klass, path, filename);
    if (info)
      *list = g_list_prepend (*list, info);
  }
      
  g_dir_close (iter);
}

void
_games_card_theme_class_append_theme_info_foreach_env (GamesCardThemeClass *klass,
                                                       const char *env,
                                                       GList **list)
{
  const char *value;
  char **paths;
  guint i;

  value = g_getenv (env);
  if (!value || !value[0])
    return;

  paths = g_strsplit (value, ":", -1);
  if (!paths)
    return;

  for (i = 0; paths[i]; ++i) {
    const char *path = paths[i];

    if (!paths[0])
      continue;

    _games_card_theme_class_append_theme_info_foreach (klass, path, list);
  }

  g_strfreev (paths);
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
 * games_card_theme_get_theme:
 * @theme:
 *
 * Returns: the #GamesCardThemeInfo corresponding to @theme.
 */
GamesCardThemeInfo *
games_card_theme_get_theme (GamesCardTheme *theme)
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
 *
 * Returns: the currently selected card size
 */
CardSize
games_card_theme_get_size (GamesCardTheme *theme)
{
  return theme->klass->get_card_size (theme);
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
games_card_theme_get_card_pixbuf (GamesCardTheme * theme, gint card_id)
{
  GdkPixbuf *pixbuf;

  g_return_val_if_fail ((card_id >= 0)
                        && (card_id < GAMES_CARDS_TOTAL), NULL);

#ifdef INSTRUMENT_LOADING
  clock_t t1, t2;

  t1 = clock ();
#endif

  pixbuf = theme->klass->get_card_pixbuf (theme, card_id);

#ifdef INSTRUMENT_LOADING
  t2 = clock ();
  totaltime += (t2 - t1);
  g_print ("took %.3fs to render card %d (cumulative: %.3fs)\n",
           (t2 - t1) * 1.0 / CLOCKS_PER_SEC, card_id,
           totaltime * 1.0 / CLOCKS_PER_SEC);
#endif

  return pixbuf;
}

/**
 * games_card_theme_get:
 * @info: a #GamesCardThemeInfo
 *
 * Returns: a new #GamesCardTheme for @info, or %NULL if there was an
 *  error while loading the theme.
 */
GamesCardTheme *
games_card_theme_get (GamesCardThemeInfo *info)
{
  GamesCardTheme *theme;
  GError *error = NULL;

  g_return_val_if_fail (info != NULL, NULL);

  if (info->type == G_TYPE_INVALID)
    return NULL;

  theme = g_object_new (info->type, "theme-info", info, NULL);
  if (!theme->klass->load (theme, &error)) {
    g_clear_error (&error);
    g_object_unref (theme);
    return NULL;
  }

  return theme;
}

/**
 * games_card_theme_get_by_name:
 * @theme_name: a theme name
 *
 * This function exists only for backward compatibility with
 * older aisleriot versions' preferences.
 * 
 * Returns: a new #GamesCardTheme for @name, or %NULL if there was an
 *  error while loading the theme.
 */
GamesCardTheme *
games_card_theme_get_by_name (const char *theme_name)
{
  GList *l;

  //XXX FIXMEchpe .svg / .png suffix stripping
  g_return_val_if_fail (theme_name != NULL, NULL);

  _games_card_theme_ensure_theme_infos ();

  for (l = theme_infos; l != NULL; l = l->next) {
    GamesCardThemeInfo *info = (GamesCardThemeInfo *) l->data;

    // FIXMEchpe
    if (g_str_has_prefix (theme_name, info->filename))
      return games_card_theme_get (info);
  }

  return NULL;
}

/**
 * games_card_theme_get_any:
 *
 * FIXMEchpe
 * Loads the card theme @theme_name. If the card theme cannot be loaded,
 * it falls back to the default card theme, if present.
 * After changing the theme, the card size will be undefined; you need
 * to call games_card_theme_set_size() to set it before getting a
 * card from @theme again.
 * 
 * Returns:
 */
GamesCardTheme *
games_card_theme_get_any (void)
{
  _games_card_theme_ensure_theme_infos ();

  if (!theme_infos)
    return NULL;

  // FIXMEchpe obviously
  return games_card_theme_get ((GamesCardThemeInfo *) theme_infos->data);
  return games_card_theme_get_by_name (GAMES_CARD_THEME_DEFAULT);
  // FIXMEchpe put the fallback in here
  return NULL;
}

/**
 * games_card_theme_get_all:
 *
 * Returns:
 */
GList *
games_card_theme_get_all (void)
{
  GList *list;

  _games_card_theme_ensure_theme_infos ();

  list = g_list_copy (theme_infos);
  g_list_foreach (list, (GFunc) games_card_theme_info_ref, NULL);

  return list;
  // FIXMEchpe
//  return g_list_sort (list, (GCompareFunc) _games_card_theme_info_collate, NULL);
}

/* GamesCardThemeInfo impl */

/* private API */

/**
 * _games_card_theme_info_new:
 * @type:
 * @path:
 * @name:
 * @diplay_name:
 *
 * Returns: a new #GamesCardThemeInfo with refcount 1
 */
GamesCardThemeInfo *
_games_card_theme_info_new (GType type,
                            const char *path,
                            const char *filename,
                            const char *display_name,
                            gpointer data /* adopted */,
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
  { char *p; info->theme_name = g_strdup (filename); p = strrchr (info->theme_name, '.'); if (p) *p = '\0'; }
  info->display_name = g_strdup (display_name);
  info->data = data;
  info->destroy_notify = destroy_notify;

  g_print ("Created GamesCardThemeInfo for type=%s path=%s filename=%s display-name=%s\n",
           g_type_name (type), path, filename, display_name);

  return info;
}

/**
 * _games_card_theme_info_equal:
 * @a:
 * @b:
 *
 * Compares @a and @b.
 *
 * Returns: %TRUE iff @a and @b refer to the same card theme
 */
gboolean
_games_card_theme_info_equal (GamesCardThemeInfo *a,
                              GamesCardThemeInfo *b)
{
  g_return_val_if_fail (a != NULL && b != NULL, FALSE);

  return _games_card_theme_info_collate (a, b) == 0;
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
_games_card_theme_info_collate (GamesCardThemeInfo *a,
                                GamesCardThemeInfo *b)
{
  int val;

  g_return_val_if_fail (a != NULL && b != NULL, 0);

  if (a->type != b->type)
    return a->type - b->type;

  val = g_utf8_collate (a->display_name, b->display_name);
  if (val != 0)
    return val;

  val = strcmp (a->display_name, b->display_name);
  if (val != 0)
    return val;

  val = strcmp (a->path, b->path);
  if (val != 0)
    return val;

  val = strcmp (a->filename, b->filename);
  if (val != 0)
    return val;

  return 0;
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

  if (info->data && info->destroy_notify)
    info->destroy_notify (info->data);

#if GLIB_CHECK_VERSION (2, 10, 0)
  g_slice_free (GamesCardThemeInfo, info);
#else
  g_free (info);
#endif
}

/**
 * games_card_theme_info_unref:
 * @info:
 *
 * Unrefs @info. If the refcount reaches %0, frees @info.
 */
const char *
games_card_theme_info_get_display_name (GamesCardThemeInfo *info)
{
  g_return_val_if_fail (info != NULL, NULL);

  return info->display_name;
}
