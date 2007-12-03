/*
   Copyright © 2004 Callum McKenzie
   Copyright © 2007 Christian Persch

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

#include "games-card-common.h"
#include "games-find-file.h"
#include "games-files.h"
#include "games-preimage.h"

#include "games-card-theme.h"

struct _GamesCardTheme {
  GObject parent;

  gchar *theme_dir;
  gchar *theme_name;

  /* Switched on GamesCardTheme.use_scalable */
  union {
    struct {
      GamesPreimage *cards_preimage;
      GamesPreimage *slot_preimage;
      GdkPixbuf *source;
      CardSize subsize;
    } scalable;
    struct {
      char *themesizepath;
      CardSize *card_sizes;
      guint n_card_sizes;
    } prerendered;
  } theme_data;

  CardSize slot_size;
  CardSize card_size;

  guint use_scalable : 1;
  guint theme_loaded : 1;
  guint size_available : 1;

  guint subrender : 1;
  guint prescaled : 1;

  guint antialias_set : 1;
  guint antialias : 2; /* enough bits for cairo_antialias_t */
  guint subpixel_order : 3; /* enough bits for cairo_subpixel_order_t */
};

static const char extra_cards[][12] = {
  "black_joker",
  "red_joker",
  "back",
  "slot"
};

static const char suites[][8] = {
  "club",
  "diamond",
  "heart",
  "spade"
};

static const char ranks[][6] = {
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
  "10",
  "jack",
  "queen",
  "king"
};

enum {
  PROP_0,
  PROP_SCALABLE,
  PROP_THEME_DIRECTORY
};

/* FIXMEchpe: use uninstalled data dir for rendering the card theme! */
#define SLOTDIR  PKGDATADIR "/pixmaps"

#define N_ROWS ((double) 5.0)
#define N_COLS ((double) 13.0)

#define FLOAT_TO_INT_CEIL(f) ((int) (f + 0.5f))
#define DELTA (0.0f)

/* #defining this prints out the time it takes to render the theme */
/* #define INSTRUMENT_LOADING */

#ifdef INSTRUMENT_LOADING
static long totaltime = 0;
#endif

static inline void
print_card_name (int card_id, char *buffer, gsize _size)
{
  int suit, rank;

  suit = card_id / 13;
  rank = card_id % 13;

  if (G_LIKELY (suit < 4)) {
    g_snprintf (buffer, _size, "%s-%s", suites[suit], ranks[rank]);
  } else {
    g_snprintf (buffer, _size, "%s", extra_cards[rank]);
  }
}

static void
games_card_theme_clear_source_pixbuf (GamesCardTheme * theme)
{
#ifdef HAVE_RSVG
  if (theme->use_scalable) {
    if (theme->theme_data.scalable.source) {
      g_object_unref (theme->theme_data.scalable.source);
    }

    theme->theme_data.scalable.source = NULL;
  }
#endif /* HAVE_RSVG */

#ifdef INSTRUMENT_LOADING
  /* Reset the time */
  totaltime = 0;
#endif
}

static void
games_card_theme_clear_theme_data (GamesCardTheme * theme)
{
#ifdef HAVE_RSVG
  if (theme->use_scalable) {
    if (theme->theme_data.scalable.cards_preimage != NULL) {
      g_object_unref (theme->theme_data.scalable.cards_preimage);
      theme->theme_data.scalable.cards_preimage = NULL;
    }
    if (theme->theme_data.scalable.slot_preimage != NULL) {
      g_object_unref (theme->theme_data.scalable.slot_preimage);
      theme->theme_data.scalable.slot_preimage = NULL;
    }

    theme->theme_data.scalable.subsize.width = -1;
    theme->theme_data.scalable.subsize.height = -1;
  } else
#endif /* HAVE_RSVG */
  {
    g_free (theme->theme_data.prerendered.card_sizes);
    theme->theme_data.prerendered.card_sizes = NULL;
    theme->theme_data.prerendered.n_card_sizes = 0;

    g_free (theme->theme_data.prerendered.themesizepath);
    theme->theme_data.prerendered.themesizepath = NULL;

    theme->size_available = FALSE;
  }

  theme->theme_loaded = FALSE;
}

#ifdef HAVE_RSVG

static gboolean
games_card_theme_load_theme_scalable (GamesCardTheme * theme,
                                      const gchar * theme_name)
{
  GamesPreimage *preimage;
  const char *theme_dir, *env;
  gchar *filename, *path;

#ifdef INSTRUMENT_LOADING
  clock_t t1, t2;

  t1 = clock ();
#endif

  if (theme->theme_data.scalable.cards_preimage != NULL)
    return TRUE;

  theme_dir =
    theme->theme_dir != NULL ? theme->theme_dir : SCALABLE_CARDS_DIR;

  /* First try and load the given file. */
  filename = g_strdup_printf ("%s.svg", theme_name);
  path = games_build_filename (theme_dir, filename);
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

  if (theme->antialias_set) {
    games_preimage_set_antialias (preimage, theme->antialias,
                                  theme->subpixel_order);
  }

  theme->theme_data.scalable.cards_preimage = preimage;
  theme->prescaled = games_preimage_is_scalable (preimage);

  /* If we don't have a scalable format, build a scaled pixbuf that we'll cut up later */
  if (!theme->prescaled) {
    theme->theme_data.scalable.source =
      games_preimage_render_unscaled_pixbuf (theme->theme_data.scalable.
                                             cards_preimage);
  }

  /* And the slot image */
  path = games_build_filename (SLOTDIR, "slot.svg");
  theme->theme_data.scalable.slot_preimage = games_preimage_new_from_file (path, NULL);
  g_free (path);
  g_return_val_if_fail (theme->theme_data.scalable.slot_preimage != NULL, FALSE);

  if (theme->antialias_set)
    games_preimage_set_antialias (theme->theme_data.scalable.slot_preimage,
                                  theme->antialias, theme->subpixel_order);

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

  if (theme->theme_data.scalable.cards_preimage == NULL) {
    games_card_theme_clear_theme_data (theme);
  }

  return theme->theme_data.scalable.cards_preimage != NULL;
}

#endif /* HAVE_RSVG */

static gboolean
games_card_theme_load_theme_prerendered (GamesCardTheme * theme,
                                         const gchar * theme_name)
{
  GKeyFile *key_file;
  char *filename, *path;
  GError *error = NULL;
  int *sizes = NULL;
  gsize n_sizes, i;
  gboolean retval = FALSE;

  filename = g_strdup_printf ("%s.card-theme", theme_name);
  path = games_build_filename (PRERENDERED_CARDS_DIR, filename);
  g_free (filename);

  key_file = g_key_file_new ();
  if (!g_key_file_load_from_file (key_file, path, 0, &error)) {
    g_warning ("Failed to load prerendered card theme from %s: %s\n", path,
               error->message);
    g_error_free (error);
    goto loser;
  }

  sizes =
    g_key_file_get_integer_list (key_file, "Card Theme", "Sizes", &n_sizes,
                                 &error);
  if (error) {
    g_warning ("Failed to get card sizes: %s\n", error->message);
    g_error_free (error);
    goto loser;
  }

  if (n_sizes == 0) {
    g_warning ("Card theme contains no sizes\n");
    goto loser;
  }

  theme->theme_data.prerendered.card_sizes = g_new (CardSize, n_sizes);
  theme->theme_data.prerendered.n_card_sizes = n_sizes;

  for (i = 0; i < n_sizes; ++i) {
    char group[32];
    GError *err = NULL;
    int width, height;

    g_snprintf (group, sizeof (group), "%d", sizes[i]);

    width = g_key_file_get_integer (key_file, group, "Width", &err);
    if (err) {
      g_warning ("Error loading width for size %d: %s\n", sizes[i],
                 err->message);
      g_error_free (err);
      goto loser;
    }
    height = g_key_file_get_integer (key_file, group, "Height", &err);
    if (err) {
      g_warning ("Error loading height for size %d: %s\n", sizes[i],
                 err->message);
      g_error_free (err);
      goto loser;
    }

    theme->theme_data.prerendered.card_sizes[i].width = width;
    theme->theme_data.prerendered.card_sizes[i].height = height;
  }

  retval = TRUE;

loser:

  g_free (sizes);

  g_key_file_free (key_file);
  g_free (path);

  if (!retval) {
    games_card_theme_clear_theme_data (theme);
  }

  return retval;
}

static gboolean
games_card_theme_load_theme (GamesCardTheme * theme, const gchar * theme_name)
{
  gboolean success;

#ifdef HAVE_RSVG
  if (theme->use_scalable) {
    success = games_card_theme_load_theme_scalable (theme, theme_name);
  } else
#endif /* HAVE_RSVG */
  {
    success = games_card_theme_load_theme_prerendered (theme, theme_name);
  }

  if (success) {
    g_free (theme->theme_name);
    theme->theme_name = g_strdup (theme_name);

    theme->theme_loaded = TRUE;
  }

  return success;
}

static gboolean
games_card_theme_load_theme_with_fallback (GamesCardTheme * theme,
                                           const gchar * theme_name)
{
  if (games_card_theme_load_theme (theme, theme_name))
    return TRUE;

  g_warning ("Failed to load theme '%s'; trying fallback theme '%s'",
             theme_name, GAMES_CARD_THEME_DEFAULT);

  if (strcmp (theme_name, GAMES_CARD_THEME_DEFAULT) != 0 &&
      games_card_theme_load_theme (theme, GAMES_CARD_THEME_DEFAULT))
    return FALSE;

  g_warning ("Failed to load fallback theme!");

  return FALSE;
}

#ifdef HAVE_RSVG

static gboolean
games_card_theme_prerender_scalable (GamesCardTheme * theme)
{
#ifdef INSTRUMENT_LOADING
  clock_t t1, t2;

  t1 = clock ();
#endif

  g_return_val_if_fail (theme->use_scalable, FALSE);
  g_return_val_if_fail (theme->theme_data.scalable.cards_preimage != NULL, FALSE);
  g_return_val_if_fail (theme->prescaled
                        || theme->theme_data.scalable.source != NULL, FALSE);

  theme->theme_data.scalable.source =
    games_preimage_render (theme->theme_data.scalable.cards_preimage,
                           theme->card_size.width * 13,
                           theme->card_size.height * 5);
  if (!theme->theme_data.scalable.source)
    return FALSE;

  theme->theme_data.scalable.subsize.width =
    gdk_pixbuf_get_width (theme->theme_data.scalable.source) / 13;
  theme->theme_data.scalable.subsize.height =
    gdk_pixbuf_get_height (theme->theme_data.scalable.source) / 5;

#ifdef INSTRUMENT_LOADING
  t2 = clock ();
  g_print ("took %.3fs to prerender\n", (t2 - t1) * 1.0 / CLOCKS_PER_SEC);
#endif

  return TRUE;
}

static GdkPixbuf *
games_card_theme_render_card (GamesCardTheme * theme, int card_id)
{
  GamesPreimage *preimage = theme->theme_data.scalable.cards_preimage;
  GdkPixbuf *subpixbuf, *card_pixbuf;
  int suit, rank;

  if (!theme->theme_loaded)
    return NULL;

  g_return_val_if_fail (theme->use_scalable, NULL);

  suit = card_id / 13;
  rank = card_id % 13;

  if (G_UNLIKELY (card_id == GAMES_CARD_SLOT)) {
    subpixbuf = games_preimage_render (theme->theme_data.scalable.slot_preimage,
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

    if (G_LIKELY (suit < 4)) {
      g_snprintf (node, sizeof (node), "#%s_%s", ranks[rank], suites[suit]);
    } else {
      g_snprintf (node, sizeof (node), "#%s", extra_cards[rank]);
    }

    subpixbuf = games_preimage_render_sub (preimage,
                                           node,
                                           theme->card_size.width, theme->card_size.height,
                                           offsetx, offsety,
                                           zoomx, zoomy);

    return subpixbuf;
  }

  /* Not using subrendering */
  if (!theme->theme_data.scalable.source &&
      !games_card_theme_prerender_scalable (theme))
    return NULL;

  subpixbuf = gdk_pixbuf_new_subpixbuf (theme->theme_data.scalable.source,
                                        rank *
                                        theme->theme_data.scalable.subsize.width,
                                        suit *
                                        theme->theme_data.scalable.subsize.height,
                                        theme->theme_data.scalable.subsize.width,
                                        theme->theme_data.scalable.subsize.height);
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

#endif /* HAVE_RSVG */

static GdkPixbuf *
games_card_theme_load_card (GamesCardTheme * theme, int card_id)
{
  GdkPixbuf *pixbuf;
  GError *error = NULL;
  char name[64], filename[64];
  char *path;

  if (!theme->theme_loaded || !theme->size_available)
    return NULL;

  g_return_val_if_fail (!theme->use_scalable, NULL);

  print_card_name (card_id, name, sizeof (name));
  g_snprintf (filename, sizeof (filename), "%s.png", name);
  path = games_build_filename (theme->theme_data.prerendered.themesizepath, filename);

  pixbuf = gdk_pixbuf_new_from_file (path, &error);
  if (!pixbuf) {
    g_warning ("Failed to load card image %s: %s\n", filename,
               error->message);
    g_error_free (error);
    return NULL;
  }
  g_free (path);

  return pixbuf;
}

/* Class implementation */

G_DEFINE_TYPE (GamesCardTheme, games_card_theme, G_TYPE_OBJECT);

static void
games_card_theme_finalize (GObject * object)
{
  GamesCardTheme *theme = GAMES_CARD_THEME (object);

  games_card_theme_clear_source_pixbuf (theme);

  games_card_theme_clear_theme_data (theme);

  g_free (theme->theme_name);
  g_free (theme->theme_dir);

  G_OBJECT_CLASS (games_card_theme_parent_class)->finalize (object);
}

static void
games_card_theme_init (GamesCardTheme * cardtheme)
{
  cardtheme->card_size.width = cardtheme->card_size.height = -1;
  cardtheme->theme_name = NULL;

  cardtheme->prescaled = FALSE;

#ifdef HAVE_RSVG
  cardtheme->subrender = FALSE;
  cardtheme->use_scalable = TRUE;
#else
  cardtheme->use_scalable = FALSE;
#endif /* HAVE_RSVG */
}

static void
games_card_theme_set_property (GObject * object,
                               guint prop_id,
                               const GValue * value, GParamSpec * pspec)
{
  GamesCardTheme *theme = GAMES_CARD_THEME (object);

  switch (prop_id) {
  case PROP_SCALABLE:
    theme->use_scalable = g_value_get_boolean (value) != FALSE;
    break;
  case PROP_THEME_DIRECTORY:
    theme->theme_dir = g_value_dup_string (value);
    break;
  }
}

static void
games_card_theme_class_init (GamesCardThemeClass * class)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (class);

  gobject_class->set_property = games_card_theme_set_property;
  gobject_class->finalize = games_card_theme_finalize;

  g_object_class_install_property
    (gobject_class,
     PROP_SCALABLE,
     g_param_spec_boolean ("scalable", NULL, NULL,
                           FALSE,
                           G_PARAM_WRITABLE |
                           G_PARAM_STATIC_NAME |
                           G_PARAM_STATIC_NICK |
                           G_PARAM_STATIC_BLURB | G_PARAM_CONSTRUCT_ONLY));
  g_object_class_install_property
    (gobject_class,
     PROP_THEME_DIRECTORY,
     g_param_spec_string ("theme-directory", NULL, NULL,
                          NULL,
                          G_PARAM_WRITABLE |
                          G_PARAM_STATIC_NAME |
                          G_PARAM_STATIC_NICK |
                          G_PARAM_STATIC_BLURB | G_PARAM_CONSTRUCT_ONLY));
}

/* public API */

/**
 * games_card_theme_new:
 * @theme_dir: the directory to load the theme data from, or %NULL to use
 * the default directory
 * @scalable: whether to use scalable themes, or prerendered themes
 *
 * Returns: a new #GamesCardTheme
 */
GamesCardTheme *
games_card_theme_new (const char *theme_dir, gboolean scalable)
{
  return g_object_new (GAMES_TYPE_CARD_THEME,
                       "theme-directory", theme_dir,
                       "scalable", scalable,
                       NULL);
}

/**
 * games_card_theme_set_antialias:
 * @theme:
 * @antialias: the antialiasing mode to use (see @cairo_antialias_t)
 * @subpixel_order: the subpixel order to use (see @cairo_subpixel_order_t)
 * if @antialias is %CAIRO_ANTIALIAS_SUBPIXEL 
 *
 * Turns on antialising of cards, if using a scalable theme.
 */
void
games_card_theme_set_antialias (GamesCardTheme * theme,
                                guint antialias, guint subpixel_order)
{
  g_return_if_fail (GAMES_IS_CARD_THEME (theme));

  if (theme->antialias_set &&
      theme->antialias == antialias &&
      theme->subpixel_order == subpixel_order)
    return;

  theme->antialias_set = TRUE;
  theme->antialias = antialias;
  theme->subpixel_order = subpixel_order;

  games_card_theme_clear_source_pixbuf (theme);
}

/**
 * games_card_theme_set_theme:
 * @theme:
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
games_card_theme_set_theme (GamesCardTheme * theme, const gchar * theme_name)
{
  g_return_val_if_fail (GAMES_IS_CARD_THEME (theme), FALSE);
  g_return_val_if_fail (theme_name != NULL && theme_name[0] != '\0', FALSE);

  if (theme->theme_name != NULL
      && strcmp (theme_name, theme->theme_name) == 0)
    return TRUE;

  games_card_theme_clear_source_pixbuf (theme);
  games_card_theme_clear_theme_data (theme);

  theme->card_size.width = theme->card_size.height = theme->slot_size.width =
    theme->slot_size.width = -1;

  return games_card_theme_load_theme_with_fallback (theme, theme_name);
}

/**
 * games_card_theme_get_theme:
 * @theme:
 *
 * Returns: the name of the currently loaded card theme, or %NULL if no theme
 * is loaded
 */
const gchar *
games_card_theme_get_theme (GamesCardTheme * theme)
{
  g_return_val_if_fail (GAMES_IS_CARD_THEME (theme), NULL);

  return theme->theme_name;
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
games_card_theme_set_size (GamesCardTheme * theme,
                           gint width, gint height, gdouble proportion)
{
  if (!theme->theme_loaded) {
    g_warning ("Theme not loaded yet; cannot set size!");
    return FALSE;
  }

  if ((width == theme->slot_size.width)
      && (height == theme->slot_size.height))
    return FALSE;

  theme->slot_size.width = width;
  theme->slot_size.height = height;

#ifdef HAVE_RSVG
  if (theme->use_scalable) {
    double aspect_ratio, twidth, theight;

    /* Now calculate the card size: find the maximum size that fits
     * into the given area, preserving the card's aspect ratio.
     */
    aspect_ratio = games_card_theme_get_aspect (theme);

    twidth = proportion * width;
    theight = proportion * height;
    if (twidth / theight < aspect_ratio) {
      theight = twidth / aspect_ratio;
    } else {
      twidth = theight * aspect_ratio;
    }

    if (theme->card_size.width == (int) twidth
        && theme->card_size.height == (int) theight)
      return FALSE;

    theme->card_size.width = twidth;
    theme->card_size.height = theight;

  } else
#endif /* HAVE_RSVG */
  {
    guint i;
    int twidth, theight;
    gchar *spath;
    CardSize size = { -1, -1 }, fit_size = {
    -1, -1};

    twidth = FLOAT_TO_INT_CEIL (((double) width) * proportion);
    theight = FLOAT_TO_INT_CEIL (((double) height) * proportion);

    /* Find the closest prerendered size */
    for (i = 0; i < theme->theme_data.prerendered.n_card_sizes; ++i) {
      CardSize info = theme->theme_data.prerendered.card_sizes[i];

      if (info.width > width || info.height > height)
        continue;

      if (info.width > fit_size.width && info.height > fit_size.height)
        fit_size = info;

      /* FIXMEchpe */
      if (info.width <= twidth && info.height <= theight &&
          info.width > size.width && info.height > size.height)
        size = info;
    }

    if (size.width < 0 || size.height < 0)
      size = fit_size;

    if (size.width > 0 && size.height > 0) {
      char sizestr[32];

      if (size.width == theme->card_size.width &&
          size.height == theme->card_size.height)
        return FALSE;

      g_free (theme->theme_data.prerendered.themesizepath);

      g_snprintf (sizestr, sizeof (sizestr), "%d", size.width);
      spath = g_build_filename (theme->theme_dir !=
                          NULL ? theme->theme_dir : PRERENDERED_CARDS_DIR,
                          theme->theme_name, NULL);
      
      theme->theme_data.prerendered.themesizepath = 
	      				games_build_filename (spath, sizestr);
      g_free (spath);

      theme->size_available = TRUE;
      theme->card_size = size;
    } else {
      g_warning ("No prerendered size available for %d:%d\n", width, height);
      theme->size_available = FALSE;

      /* FIXMEchpe: at least use the smallest available size here, or
       * programme will surely crash when trying to render NULL pixbufs
       * later on!
       */
      return FALSE;
    }
  }

  games_card_theme_clear_source_pixbuf (theme);

  return TRUE;
}

/**
 * games_card_theme_get_size:
 * @theme:
 *
 * Returns: the currently selected card size
 */
CardSize
games_card_theme_get_size (GamesCardTheme * theme)
{
  return theme->card_size;
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
  double aspect;

  g_return_val_if_fail (GAMES_IS_CARD_THEME (theme), 1.0);

#ifdef HAVE_RSVG
  if (theme->use_scalable) {
    aspect =
      (((double) games_preimage_get_width (theme->theme_data.scalable.cards_preimage))
       * N_ROWS) /
      (((double) games_preimage_get_height (theme->theme_data.scalable.cards_preimage))
       * N_COLS);
  } else
#endif
  {
    aspect =
      ((double) theme->card_size.width) / ((double) theme->card_size.height);
  }

  return aspect;
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

#ifdef HAVE_RSVG
  if (theme->use_scalable) {
    pixbuf = games_card_theme_render_card (theme, card_id);
  } else
#endif /* HAVE_RSVG */
  {
    pixbuf = games_card_theme_load_card (theme, card_id);
  }

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
 * games_card_theme_get_card_name:
 * @theme:
 * @cardid:
 *
 * Returns the name of the card @cardid
 *
 * Returns: a newly allocated string containing the card's name
 */
char *
games_card_theme_get_card_name (GamesCardTheme * theme, gint card_id)
{
  char name[128];

  print_card_name (card_id, name, sizeof (name));

  return g_strdup (name);
}
