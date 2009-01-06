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
  CHANGED,
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

/* #defining this prints out the time it takes to render the theme */
/* #define INSTRUMENT_LOADING */

#ifdef INSTRUMENT_LOADING
static long totaltime = 0;
#endif

static gboolean
games_card_theme_load_theme (GamesCardTheme *theme,
                             const char *theme_dir,
                             const char *theme_name)
{
  return theme->klass->load_theme (theme, theme_dir, theme_name, NULL);
}

static gboolean
games_card_theme_load_theme_with_fallback (GamesCardTheme *theme,
                                           const char *theme_dir,
                                           const char *theme_name)
{
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
}

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

  /* NOTE! We have to do this here, since it returns the wrong class
   * (GamesCardThemeClass) when called in games_card_theme_init() !
   */
  theme->klass = GAMES_CARD_THEME_GET_CLASS (theme);

  return object;
}

static void
games_card_theme_class_init (GamesCardThemeClass * klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  gobject_class->constructor = games_card_theme_constructor;

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
 * games_card_theme_set_theme:
 * @theme:
 * @theme_dir: the theme directory, or %NULL to use the default
 * @theme_name: the name of the theme to load
 *
 * Loads the card theme @theme_name. If the card theme cannot be loaded,
 * it falls back to the default card theme, if present.
 * After changing the theme, the card size will be undefined; you need
 * to call games_card_theme_set_size() to set it before getting a
 * card from @theme again.
 * 
 * Returns: %TRUE iff loading the new card theme succeeded
 */
gboolean
games_card_theme_set_theme (GamesCardTheme *theme,
                            const char *theme_dir,
                            const char *theme_name)
{
  g_return_val_if_fail (GAMES_IS_CARD_THEME (theme), FALSE);
  g_return_val_if_fail (theme_name != NULL && theme_name[0] != '\0', FALSE);

  return games_card_theme_load_theme_with_fallback (theme, theme_dir, theme_name);
}

/**
 * games_card_theme_get_theme:
 * @theme:
 *
 * Returns: the name of the currently loaded card theme, or %NULL if no theme
 * is loaded
 */
const char *
games_card_theme_get_theme (GamesCardTheme *theme)
{
  g_return_val_if_fail (GAMES_IS_CARD_THEME (theme), NULL);

  return theme->klass->get_theme_name (theme);
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
 * games_card_theme_sliced_new:
 *
 * Returns: a new #GamesCardThemeSliced
 */
GamesCardTheme *
games_card_theme_new (void)
{
  GType type = G_TYPE_INVALID;
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
    else
#endif
    if (strcmp (env, "sliced") == 0)
      type = GAMES_TYPE_CARD_THEME_SLICED;
    else if (strcmp (env, "fixed") == 0)
      type = GAMES_TYPE_CARD_THEME_FIXED;
  }
#endif /* !HAVE_HILDON */

  return g_object_new (type, NULL);
}
