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

struct _GamesCardThemeSVGClass {
  GamesCardThemePreimageClass parent_class;
};

struct _GamesCardThemeSVG {
  GamesCardThemePreimage parent_instance;
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

G_DEFINE_TYPE (GamesCardThemeSVG, games_card_theme_svg, GAMES_TYPE_CARD_THEME_PREIMAGE);

static gboolean
games_card_theme_svg_load_theme (GamesCardTheme *card_theme,
                                 const char *theme_dir,
                                 const char *theme_name,
                                 GError **error)
{
  GamesCardThemePreimage *preimage_card_theme = (GamesCardThemePreimage *) card_theme;
  gboolean retval = FALSE;

#ifdef INSTRUMENT_LOADING
  clock_t t1, t2;

  t1 = clock ();
#endif

  if (!GAMES_CARD_THEME_CLASS (games_card_theme_svg_parent_class)->load_theme
    (card_theme, theme_dir, theme_name, error))
    goto out;

  if (!games_preimage_is_scalable (preimage_card_theme->cards_preimage))
    goto out;

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

static GdkPixbuf *
games_card_theme_svg_get_card_pixbuf (GamesCardTheme *card_theme,
                                      int card_id)
{
  GamesCardThemePreimage *preimage_card_theme = (GamesCardThemePreimage *) card_theme;
  GamesPreimage *preimage = preimage_card_theme->cards_preimage;
  GdkPixbuf *subpixbuf;
  int suit, rank;
  double card_width, card_height;
  double width, height;
  double offsetx, offsety;
  double zoomx, zoomy;
  char node[64];

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

  card_width = ((double) games_preimage_get_width (preimage)) / N_COLS;
  card_height = ((double) games_preimage_get_height (preimage)) / N_ROWS;

  width = preimage_card_theme->card_size.width - 2 * DELTA;
  height = preimage_card_theme->card_size.height - 2 * DELTA;

  offsetx = -((double) rank) * card_width + DELTA;
  offsety = -((double) suit) * card_height + DELTA;

  zoomx = width / card_width;
  zoomy = height / card_height;

  games_card_get_node_by_suit_and_rank_snprintf (node, sizeof (node), suit, rank);

  subpixbuf = games_preimage_render_sub (preimage,
                                         node,
                                         preimage_card_theme->card_size.width,
                                         preimage_card_theme->card_size.height,
                                         offsetx, offsety,
                                         zoomx, zoomy);

  return subpixbuf;
}

static void
games_card_theme_svg_init (GamesCardThemeSVG * cardtheme)
{
}

static void
games_card_theme_svg_class_init (GamesCardThemeSVGClass * klass)
{
  GamesCardThemeClass *theme_class = GAMES_CARD_THEME_CLASS (klass);

  theme_class->load_theme = games_card_theme_svg_load_theme;
  theme_class->get_card_pixbuf = games_card_theme_svg_get_card_pixbuf;
}

/* public API */

/**
 * games_card_theme_svg_new:
 *
 * Returns: a new #GamesCardThemeSVG
 */
GamesCardTheme*
games_card_theme_svg_new (void)
{
  return g_object_new (GAMES_TYPE_CARD_THEME_SVG, NULL);
}
