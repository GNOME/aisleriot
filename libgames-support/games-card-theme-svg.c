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
  GamesCardThemeClass parent_class;
};

struct _GamesCardThemeSVG {
  GamesCardTheme parent_instance;

  char *theme_dir;
  char *theme_name;

  GamesPreimage *cards_preimage;
  GamesPreimage *slot_preimage;
  GdkPixbuf *source;
  CardSize subsize;

  CardSize slot_size;
  CardSize card_size;

  guint theme_loaded : 1;
  guint size_available : 1;

  guint subrender : 1;
  guint prescaled : 1;

  cairo_font_options_t *font_options;
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

G_DEFINE_TYPE (GamesCardThemeSVG, games_card_theme_svg, GAMES_TYPE_CARD_THEME);

static void
games_card_theme_svg_clear_source_pixbuf (GamesCardThemeSVG *theme)
{
  if (theme->source) {
    g_object_unref (theme->source);
    theme->source = NULL;
  }

#ifdef INSTRUMENT_LOADING
  /* Reset the time */
  totaltime = 0;
#endif
}

static void
games_card_theme_svg_clear_theme_data (GamesCardThemeSVG *theme)
{
  games_card_theme_svg_clear_source_pixbuf (theme);

  if (theme->cards_preimage != NULL) {
    g_object_unref (theme->cards_preimage);
    theme->cards_preimage = NULL;
  }
  if (theme->slot_preimage != NULL) {
    g_object_unref (theme->slot_preimage);
    theme->slot_preimage = NULL;
  }

  theme->subsize.width = -1;
  theme->subsize.height = -1;

  theme->theme_loaded = FALSE;

  theme->card_size.width = theme->card_size.height = theme->slot_size.width =
    theme->slot_size.width = -1;
}

static gboolean
games_card_theme_svg_load_theme (GamesCardTheme *card_theme,
                                 const char *theme_dir,
                                 const char *theme_name,
                                 GError **error)
{
  GamesCardThemeSVG *theme = (GamesCardThemeSVG *) card_theme;
  GamesPreimage *preimage;
  const char *slot_dir, *env;
  gchar *filename, *path;

#ifdef INSTRUMENT_LOADING
  clock_t t1, t2;

  t1 = clock ();
#endif

  if (theme->theme_name != NULL &&
      strcmp (theme_name, theme->theme_name) == 0)
    return TRUE;

  games_card_theme_svg_clear_theme_data (theme);

  g_free (theme->theme_name);
  theme->theme_name = NULL;

  // FIXMEchpe wtf?
  if (theme->cards_preimage != NULL)
    return TRUE;

  if (!theme_dir)
    theme_dir = games_runtime_get_directory (GAMES_RUNTIME_SCALABLE_CARDS_DIRECTORY);

  /* First try and load the given file. */
  filename = g_strdup_printf ("%s.svg", theme_name);
  path = g_build_filename (theme_dir, filename, NULL);
  preimage = games_preimage_new_from_file (path, NULL);
  g_free (path);

  /* Failing that, try and find a similar file (e.g. a suffix change). */
  if (!preimage) {
    path = games_find_similar_file (filename, theme_dir);
    if (path) {
      preimage = games_preimage_new_from_file (path, NULL);
      g_free (path);
    }
  }

  g_free (filename);

  if (!preimage)
    goto out;

  if (theme->font_options) {
    games_preimage_set_font_options (preimage, theme->font_options);
  }

  theme->cards_preimage = preimage;
  theme->prescaled = games_preimage_is_scalable (preimage);

  /* If we don't have a scalable format, build a scaled pixbuf that we'll cut up later */
  if (!theme->prescaled) {
    theme->source =
      games_preimage_render_unscaled_pixbuf (theme->
                                             cards_preimage);
  }

  /* And the slot image */
  /* FIXMEchpe: use uninstalled data dir for rendering the card theme! */
  slot_dir = games_runtime_get_directory (GAMES_RUNTIME_PIXMAP_DIRECTORY);
  path = g_build_filename (slot_dir, "slot.svg", NULL);
  theme->slot_preimage = games_preimage_new_from_file (path, NULL);
  g_free (path);
  g_return_val_if_fail (theme->slot_preimage != NULL, FALSE);

  if (theme->font_options) {
    games_preimage_set_font_options (theme->slot_preimage,
                                     theme->font_options);
  }

  /* Use subrendering by default, but allow to override with the env var */
  theme->subrender = TRUE;

  env = g_getenv ("GAMES_CARDS_SUBRENDERING");
  if (env != NULL) {
    theme->subrender = g_ascii_strtoull (env, NULL, 0) != 0;
  }
#ifdef GNOME_ENABLE_DEBUG
  if (theme->subrender)
    g_print ("Using subrendering for theme \"%s\"\n", theme_name);
  else
    g_print ("Not using subrendering for theme \"%s\"\n", theme_name);
#endif

out:

#ifdef INSTRUMENT_LOADING
  t2 = clock ();
  totaltime += (t2 - t1);
  g_print ("took %.3fs to create preimage (cumulative %.3fs)\n",
           (t2 - t1) * 1.0 / CLOCKS_PER_SEC,
           totaltime * 1.0 / CLOCKS_PER_SEC);
#endif

  if (theme->cards_preimage != NULL) {
    theme->theme_name = g_strdup (theme_name);

    theme->theme_loaded = TRUE;

    _games_card_theme_emit_changed (card_theme);
    return TRUE;
  }

  games_card_theme_svg_clear_theme_data (theme);
  _games_card_theme_emit_changed (card_theme);

  return FALSE;
}

static gboolean
games_card_theme_svg_prerender_scalable (GamesCardThemeSVG * theme)
{
#ifdef INSTRUMENT_LOADING
  clock_t t1, t2;

  t1 = clock ();
#endif

  g_return_val_if_fail (theme->cards_preimage != NULL, FALSE);
  g_return_val_if_fail (theme->prescaled
                        || theme->source != NULL, FALSE);

  theme->source =
    games_preimage_render (theme->cards_preimage,
                           theme->card_size.width * 13,
                           theme->card_size.height * 5);
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
games_card_theme_svg_render_card (GamesCardThemeSVG * theme, int card_id)
{
  GamesPreimage *preimage = theme->cards_preimage;
  GdkPixbuf *subpixbuf, *card_pixbuf;
  int suit, rank;

  if (!theme->theme_loaded)
    return NULL;

  suit = card_id / 13;
  rank = card_id % 13;

  if (G_UNLIKELY (card_id == GAMES_CARD_SLOT)) {
    subpixbuf = games_preimage_render (theme->slot_preimage,
                                       theme->card_size.width,
                                       theme->card_size.height);

    return subpixbuf;
  }

  if (theme->subrender) {
    double card_width, card_height;
    double width, height;
    double offsetx, offsety;
    double zoomx, zoomy;
    char node[64];

    card_width = ((double) games_preimage_get_width (preimage)) / N_COLS;
    card_height = ((double) games_preimage_get_height (preimage)) / N_ROWS;

    width = theme->card_size.width - 2 * DELTA;
    height = theme->card_size.height - 2 * DELTA;

    offsetx = -((double) rank) * card_width + DELTA;
    offsety = -((double) suit) * card_height + DELTA;

    zoomx = width / card_width;
    zoomy = height / card_height;

    games_card_get_node_by_suit_and_rank_snprintf (node, sizeof (node), suit, rank);

    subpixbuf = games_preimage_render_sub (preimage,
                                           node,
                                           theme->card_size.width, theme->card_size.height,
                                           offsetx, offsety,
                                           zoomx, zoomy);

    return subpixbuf;
  }

  /* Not using subrendering */
  if (!theme->source &&
      !games_card_theme_svg_prerender_scalable (theme))
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
                                           theme->card_size.width,
                                           theme->card_size.height,
                                           GDK_INTERP_BILINEAR);
    g_object_unref (subpixbuf);
  }

  return card_pixbuf;
}

static void
games_card_theme_svg_finalize (GObject * object)
{
  GamesCardThemeSVG *theme = GAMES_CARD_THEME_SVG (object);

  games_card_theme_svg_clear_theme_data (theme);

  g_free (theme->theme_name);
  g_free (theme->theme_dir);

  if (theme->font_options) {
    cairo_font_options_destroy (theme->font_options);
  }

  G_OBJECT_CLASS (games_card_theme_svg_parent_class)->finalize (object);
}

static void
games_card_theme_svg_init (GamesCardThemeSVG * cardtheme)
{
  cardtheme->card_size.width = cardtheme->card_size.height = -1;
  cardtheme->theme_name = NULL;

  cardtheme->prescaled = FALSE;

  cardtheme->subrender = FALSE;
}

static void
games_card_theme_svg_set_font_options (GamesCardTheme *card_theme,
                                       const cairo_font_options_t *font_options)
{
  GamesCardThemeSVG *theme = (GamesCardThemeSVG *) card_theme;

  if (font_options &&
      theme->font_options &&
      cairo_font_options_equal (font_options, theme->font_options))
    return;

  if (theme->font_options) {
    cairo_font_options_destroy (theme->font_options);
  }

  if (font_options) {
    theme->font_options = cairo_font_options_copy (font_options);
  } else {
    theme->font_options = NULL;
  }

  games_card_theme_svg_clear_source_pixbuf (theme);
  _games_card_theme_emit_changed (card_theme);
}

static const gchar *
games_card_theme_svg_get_theme_name (GamesCardTheme *card_theme)
{
  GamesCardThemeSVG *theme = (GamesCardThemeSVG *) card_theme;

  return theme->theme_name;
}

static gboolean
games_card_theme_svg_set_card_size (GamesCardTheme *card_theme,
                                    int width,
                                    int height,
                                    double proportion)
{
  GamesCardThemeSVG *theme = (GamesCardThemeSVG *) card_theme;
  double aspect_ratio, twidth, theight;

  if (!theme->theme_loaded) {
    g_warning ("Theme not loaded yet; cannot set size!");
    return FALSE;
  }

  if ((width == theme->slot_size.width) &&
      (height == theme->slot_size.height))
    return FALSE;

  theme->slot_size.width = width;
  theme->slot_size.height = height;

  /* Now calculate the card size: find the maximum size that fits
   * into the given area, preserving the card's aspect ratio.
   */
  aspect_ratio = games_card_theme_get_aspect (card_theme);

  twidth = proportion * width;
  theight = proportion * height;
  if (twidth / theight < aspect_ratio) {
    theight = twidth / aspect_ratio;
  } else {
    twidth = theight * aspect_ratio;
  }

  if (theme->card_size.width == (int) twidth &&
      theme->card_size.height == (int) theight)
    return FALSE;

  theme->card_size.width = twidth;
  theme->card_size.height = theight;

  games_card_theme_svg_clear_source_pixbuf (theme);
  _games_card_theme_emit_changed (card_theme);

  return TRUE;
}

static CardSize
games_card_theme_svg_get_card_size (GamesCardTheme *card_theme)
{
  GamesCardThemeSVG *theme = (GamesCardThemeSVG *) card_theme;

  return theme->card_size;
}

static double
games_card_theme_svg_get_card_aspect (GamesCardTheme* card_theme)
{
  GamesCardThemeSVG *theme = (GamesCardThemeSVG *) card_theme;
  double aspect;

  g_return_val_if_fail (GAMES_IS_CARD_THEME_SVG (theme), 1.0);

  aspect =
      (((double) games_preimage_get_width (theme->cards_preimage))
       * N_ROWS) /
      (((double) games_preimage_get_height (theme->cards_preimage))
       * N_COLS);

  return aspect;
}

static GdkPixbuf *
games_card_theme_svg_get_card_pixbuf (GamesCardTheme *card_theme,
                                      int card_id)
{
  GamesCardThemeSVG *theme = (GamesCardThemeSVG *) card_theme;
  GdkPixbuf *pixbuf;

#ifdef INSTRUMENT_LOADING
  clock_t t1, t2;

  t1 = clock ();
#endif

  pixbuf = games_card_theme_svg_render_card (theme, card_id);

#ifdef INSTRUMENT_LOADING
  t2 = clock ();
  totaltime += (t2 - t1);
  g_print ("took %.3fs to render card %d (cumulative: %.3fs)\n",
           (t2 - t1) * 1.0 / CLOCKS_PER_SEC, card_id,
           totaltime * 1.0 / CLOCKS_PER_SEC);
#endif

  return pixbuf;
}

static void
games_card_theme_svg_class_init (GamesCardThemeSVGClass * klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GamesCardThemeClass *theme_class = GAMES_CARD_THEME_CLASS (klass);

  gobject_class->finalize = games_card_theme_svg_finalize;

  theme_class->load_theme = games_card_theme_svg_load_theme;
  theme_class->get_theme_name = games_card_theme_svg_get_theme_name;
  theme_class->set_card_size = games_card_theme_svg_set_card_size;
  theme_class->get_card_size = games_card_theme_svg_get_card_size;
  theme_class->get_card_aspect = games_card_theme_svg_get_card_aspect;
  theme_class->get_card_pixbuf = games_card_theme_svg_get_card_pixbuf;
  theme_class->set_font_options = games_card_theme_svg_set_font_options;
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
