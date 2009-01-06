/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008 Christian Persch

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
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>

#include "games-find-file.h"
#include "games-files.h"
#include "games-preimage.h"
#include "games-runtime.h"

#include "games-card-theme.h"
#include "games-card-theme-private.h"

struct _GamesCardThemeSlicedClass {
  GamesCardThemePreimageClass parent_class;
};

struct _GamesCardThemeSliced {
  GamesCardThemePreimage parent_instance;

  GdkPixbuf *source;
  CardSize subsize;

  guint prescaled : 1;
};

#define N_ROWS ((double) 5.0)
#define N_COLS ((double) 13.0)

#define DELTA (0.0f)

/* #defining this prints out the time it takes to render the theme */
/* #define INSTRUMENT_LOADING */

#ifdef INSTRUMENT_LOADING
static long totaltime = 0;
#endif

/* Class implementation */

G_DEFINE_TYPE (GamesCardThemeSliced, games_card_theme_sliced, GAMES_TYPE_CARD_THEME_PREIMAGE);

static void
games_card_theme_sliced_clear_sized_theme_data (GamesCardThemePreimage *preimage_card_theme)
{
  GamesCardThemeSliced *theme = GAMES_CARD_THEME_SLICED (preimage_card_theme);

  if (theme->source) {
    g_object_unref (theme->source);
    theme->source = NULL;
  }

#ifdef INSTRUMENT_LOADING
  /* Reset the time */
  totaltime = 0;
#endif
}

static gboolean
games_card_theme_sliced_load (GamesCardTheme *card_theme,
                              GError **error)
{
  GamesCardThemePreimage *preimage_card_theme = (GamesCardThemePreimage *) card_theme;
  GamesCardThemeSliced *theme = (GamesCardThemeSliced *) card_theme;
  gboolean retval = FALSE;

#ifdef INSTRUMENT_LOADING
  clock_t t1, t2;

  t1 = clock ();
#endif

  if (!GAMES_CARD_THEME_CLASS (games_card_theme_sliced_parent_class)->load (card_theme, error))
    goto out;

  /* If we don't have a scalable format, build a scaled pixbuf that we'll cut up later */
  theme->prescaled = games_preimage_is_scalable (preimage_card_theme->cards_preimage);
  if (!theme->prescaled) {
    theme->source = games_preimage_render_unscaled_pixbuf (preimage_card_theme->cards_preimage);
  }

  retval = TRUE;

out:

#ifdef INSTRUMENT_LOADING
  t2 = clock ();
  totaltime += (t2 - t1);
  g_print ("took %.3fs to create preimage (cumulative %.3fs)\n",
           (t2 - t1) * 1.0 / CLOCKS_PER_SEC,
           totaltime * 1.0 / CLOCKS_PER_SEC);
#endif

  return retval;
}

static gboolean
games_card_theme_sliced_prerender_scalable (GamesCardThemeSliced * theme)
{
  GamesCardThemePreimage *preimage_card_theme = (GamesCardThemePreimage *) theme;

#ifdef INSTRUMENT_LOADING
  clock_t t1, t2;

  t1 = clock ();
#endif

  // FIXMEchpe this doesn't look right
  g_return_val_if_fail (theme->prescaled
                        || theme->source != NULL, FALSE);

  theme->source =
    games_preimage_render (preimage_card_theme->cards_preimage,
                           preimage_card_theme->card_size.width * 13,
                           preimage_card_theme->card_size.height * 5);
  if (!theme->source)
    return FALSE;

  theme->subsize.width =
    gdk_pixbuf_get_width (theme->source) / 13;
  theme->subsize.height =
    gdk_pixbuf_get_height (theme->source) / 5;

#ifdef INSTRUMENT_LOADING
  t2 = clock ();
  g_print ("took %.3fs to prerender\n", (t2 - t1) * 1.0 / CLOCKS_PER_SEC);
#endif

  return TRUE;
}

static GdkPixbuf *
games_card_theme_sliced_get_card_pixbuf (GamesCardTheme *card_theme,
                                         int card_id)
{
  GamesCardThemePreimage *preimage_card_theme = (GamesCardThemePreimage *) card_theme;
  GamesCardThemeSliced *theme = (GamesCardThemeSliced *) card_theme;
  GdkPixbuf *subpixbuf, *card_pixbuf;
  int suit, rank;

  if (!preimage_card_theme->theme_loaded)
    return NULL;

  suit = card_id / 13;
  rank = card_id % 13;

  if (G_UNLIKELY (card_id == GAMES_CARD_SLOT)) {
    subpixbuf = games_preimage_render (preimage_card_theme->slot_preimage,
                                       preimage_card_theme->card_size.width,
                                       preimage_card_theme->card_size.height);

    return subpixbuf;
  }

  /* Not using subrendering */
  // FIXMEchpe this doesn't look right for non-scalable
  if (!theme->source &&
      !games_card_theme_sliced_prerender_scalable (theme))
    return NULL;

  subpixbuf = gdk_pixbuf_new_subpixbuf (theme->source,
                                        rank *
                                        theme->subsize.width,
                                        suit *
                                        theme->subsize.height,
                                        theme->subsize.width,
                                        theme->subsize.height);
  if (theme->prescaled) {
    card_pixbuf = subpixbuf;
  } else {
    card_pixbuf = gdk_pixbuf_scale_simple (subpixbuf,
                                           preimage_card_theme->card_size.width,
                                           preimage_card_theme->card_size.height,
                                           GDK_INTERP_BILINEAR);
    g_object_unref (subpixbuf);
  }

  return card_pixbuf;
}

static void
games_card_theme_sliced_init (GamesCardThemeSliced *theme)
{
  theme->subsize.width = theme->subsize.height = -1;
}

static void
games_card_theme_sliced_class_init (GamesCardThemeSlicedClass * klass)
{
  GamesCardThemeClass *theme_class = GAMES_CARD_THEME_CLASS (klass);
  GamesCardThemePreimageClass *preimage_theme_class = GAMES_CARD_THEME_PREIMAGE_CLASS (klass);

  theme_class->load = games_card_theme_sliced_load;
  theme_class->get_card_pixbuf = games_card_theme_sliced_get_card_pixbuf;

  preimage_theme_class->clear_sized_theme_data = games_card_theme_sliced_clear_sized_theme_data;
}

/* private API */

/**
 * games_card_theme_sliced_new:
 *
 * Returns: a new #GamesCardThemeSliced
 */
GamesCardTheme*
games_card_theme_sliced_new (void)
{
  return g_object_new (GAMES_TYPE_CARD_THEME_SLICED, NULL);
}
