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

/* Manage a set of pixbufs containing a deck of cards. */

#include <config.h>

#include <string.h>
#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include "games-card-common.h"
#include "games-find-file.h"
#include "games-files.h"
#include "games-preimage.h"
#include "games-pixbuf-utils.h"

#include "games-card-images.h"

enum {
  PROP_0,
  PROP_SCALABLE
};

/* MARK_IS_TRANSFORMED must be the same value as GAMES_CARD_IMAGES_HIGHLIGHTED ! */
enum {
  MARK_IS_PIXMAP = 1U << 0,
  MARK_IS_TRANSFORMED = 1U << 1,
  MARK_MASK = 0x3U
};

#define GET_MARK(ptr) ((gsize) (ptr) & (gsize) MARK_MASK)
#define HAS_MARK(ptr,flags) ((gsize) GET_MARK (ptr) & (gsize) (flags))
#define MARK_POINTER(ptr,flags) ((gpointer) ((gsize) (ptr) | (gsize) (flags)))
#define UNMARK_POINTER(ptr) ((gpointer) ((gsize) (ptr) & ~(gsize) MARK_MASK))

#define CACHE_SIZE  (2 * GAMES_CARDS_TOTAL)

/* Threshold for rendering the pixbuf to card and alpha mask */
#define CARD_ALPHA_THRESHOLD  (127)
#define SLOT_ALPHA_THRESHOLD  (255)

#define GAMES_CARD_ID(suit, rank) ((13*(suit)) + ((rank-1)%13))

static void
games_card_images_clear_cache (GamesCardImages * images)
{
  guint i;

  for (i = 0; i < CACHE_SIZE; i++) {
    gpointer data = UNMARK_POINTER (images->cache[i]);

    if (data != NULL) {
      g_object_unref (data);
    }
  }

  memset (images->cache, 0, CACHE_SIZE * sizeof (gpointer));

  if (images->card_mask != NULL) {
    g_object_unref (images->card_mask);
    images->card_mask = NULL;
  }
  if (images->slot_mask != NULL) {
    g_object_unref (images->slot_mask);
    images->slot_mask = NULL;
  }
}

static inline guint
card_to_index (Card card)
{
  guint card_id;

  if (CARD_GET_FACE_DOWN (card)) {
    card_id = GAMES_CARD_BACK;
  } else if (G_UNLIKELY (CARD_GET_RANK (card) == 0)) {
    /* A joker */
    if (CARD_GET_SUIT (card) == GAMES_CARDS_CLUBS ||
        CARD_GET_SUIT (card) == GAMES_CARDS_SPADES) {
      /* A black joker. */
      card_id = GAMES_CARD_BLACK_JOKER;
    } else {
      /* A red joker. */
      card_id = GAMES_CARD_RED_JOKER;
    }
  } else {
    card_id = GAMES_CARD_ID (CARD_GET_SUIT (card), CARD_GET_RANK (card));
  }

  return card_id;
}

/* Consider the cache as a 2-dimensional array: [0..TOTAL-1 , 0..1]:
 *
 * When creating cache[i][0], we _always_ store a reference to the _pixbuf_ in cache[i][1],
 * even when we're in PIXMAPS mode. In particular, if cache[i][1] == NULL we know that
 * cache[i][0] == NULL too; so we can always look up cache[i][1] first.
 * When accessing cache[i][j], and the stored object isn't of the right format
 * (i.e. is a pixbuf, but we need a pixmap; or is not highlighted, but we need a highlighted
 * image), we transform it, and store the result in cache[i][j].
 */
static GdkPixbuf *
create_pixbuf (GamesCardImages * images, guint card_id)
{
  GdkPixbuf *pixbuf;

  g_assert (images->cache[card_id] == NULL &&
            images->cache[card_id + GAMES_CARDS_TOTAL] == NULL);

  pixbuf = games_card_theme_get_card_pixbuf (images->theme, card_id);
  if (!pixbuf)
    return NULL;

  images->cache[card_id] = pixbuf;
  images->cache[card_id + GAMES_CARDS_TOTAL] = g_object_ref (pixbuf);

  return pixbuf;
}

static GdkPixbuf *
transform_pixbuf (GamesCardImages * images, GdkPixbuf * pixbuf, guint idx)
{
  GdkPixbuf *transformed_pixbuf;

  g_assert (pixbuf != NULL);
  g_assert (pixbuf == UNMARK_POINTER (images->cache[idx]));

  transformed_pixbuf = games_pixbuf_utils_create_highlight (pixbuf,
                                                            &images->
                                                            selection_colour);
  if (!transformed_pixbuf)
    return NULL;

  g_object_unref (pixbuf);
  images->cache[idx] = MARK_POINTER (transformed_pixbuf, MARK_IS_TRANSFORMED);

  return transformed_pixbuf;
}

static GdkBitmap *
create_mask (GdkDrawable * drawable,
             GdkPixbuf * pixbuf, guint alpha_threshold)
{
  GdkPixmap *mask;
  int width, height;

  if (!pixbuf)
    return NULL;

  width = gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);
  mask = gdk_pixmap_new (drawable, width, height, 1);
  if (!mask)
    return NULL;

  gdk_pixbuf_render_threshold_alpha (pixbuf, mask,
                                     0, 0, 0, 0, width, height,
                                     alpha_threshold);

  return mask;
}

/* Class implementation */

G_DEFINE_TYPE (GamesCardImages, games_card_images, G_TYPE_OBJECT);

static void
games_card_images_init (GamesCardImages * images)
{
  const GdkColor background_colour =
    { 0, 0 /* red */ , 0 /* green */ , 0 /* blue */  };
  const GdkColor selection_colour =
    { 0, 0 /* red */ , 0 /* green */ , 0xaa00 /* blue */  };

  images->cache = g_new0 (gpointer, CACHE_SIZE);

  images->background_colour = background_colour;
  images->selection_colour = selection_colour;

  images->cache_mode = CACHE_PIXMAPS;
}

static GObject *
games_card_images_constructor (GType type,
                               guint n_construct_properties,
                               GObjectConstructParam * construct_params)
{
  GObject *object;
  GamesCardImages *images;

  object = G_OBJECT_CLASS (games_card_images_parent_class)->constructor
    (type, n_construct_properties, construct_params);
  images = GAMES_CARD_IMAGES (object);

  images->theme = games_card_theme_new (NULL, images->scalable);

  return object;
}

static void
games_card_images_finalize (GObject * object)
{
  GamesCardImages *images = GAMES_CARD_IMAGES (object);

  games_card_images_clear_cache (images);
  g_free (images->cache);

  g_object_unref (images->theme);

  G_OBJECT_CLASS (games_card_images_parent_class)->finalize (object);
}

static void
games_card_images_set_property (GObject * object,
                                guint prop_id,
                                const GValue * value, GParamSpec * pspec)
{
  GamesCardImages *images = GAMES_CARD_IMAGES (object);

  switch (prop_id) {
  case PROP_SCALABLE:
    images->scalable = g_value_get_boolean (value) != FALSE;
    break;
  }
}

static void
games_card_images_class_init (GamesCardImagesClass * class)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (class);

  gobject_class->constructor = games_card_images_constructor;
  gobject_class->set_property = games_card_images_set_property;
  gobject_class->finalize = games_card_images_finalize;

  g_object_class_install_property
    (gobject_class,
     PROP_SCALABLE,
     g_param_spec_boolean ("scalable", NULL, NULL,
                           TRUE,
                           G_PARAM_WRITABLE | G_PARAM_STATIC_NAME |
                           G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB |
                           G_PARAM_CONSTRUCT_ONLY));
}

/* public API */

/**
 * games_card_images_new:
 * @theme_dir: the directory to load the theme data from, or %NULL to use
 * the default directory
 * @scalable: whether to use scalable themes, or prerendered themes
 *
 * Returns: a new #GamesCardImages
 */
GamesCardImages *
games_card_images_new (gboolean scalable)
{
  return g_object_new (GAMES_TYPE_CARD_IMAGES,
                       "scalable", scalable,
                       NULL);
}

/**
 * games_card_images_set_cache_mode:
 * @images:
 * @mode:
 *
 * Select whether @images stores #GdkPixbuf:s or #GdkPixmap:s.
 * It is an error to use games_card_images_get_*_pixbuf*() while
 * storing pixmaps, and to use games_card_images_get_*_pixmap*() while
 * storing pixbufs.
 * Changing the cache mode invalidates all stored images.
 */
void
games_card_images_set_cache_mode (GamesCardImages * images,
                                  GamesCardImagesCacheMode mode)
{
  g_return_if_fail (GAMES_IS_CARD_IMAGES (images));
  g_return_if_fail (mode >= 0 && mode < LAST_CACHE_MODE);

  if (mode == images->cache_mode)
    return;

  /* If we were storing pixmaps, we need to clear them */
  if (images->cache_mode == CACHE_PIXMAPS) {
    games_card_images_clear_cache (images);
  }

  images->cache_mode = mode;
}

/**
 * games_card_images_set_drawable:
 * @images:
 * @drawable: a #GdkDrawable
 *
 * Sets the drawable used when creating the cards pixmaps; or use
 * %NULL to unset the previously set drawable.
 * The drawable must be set to a non-NULL #GdkDrawable before getting
 * any pixmaps or masks from @images.
 * Changing the drawable invalidates all cached pixmaps and masks.
 */
void
games_card_images_set_drawable (GamesCardImages * images,
                                GdkWindow * drawable)
{
  g_return_if_fail (GAMES_IS_CARD_IMAGES (images));
  g_return_if_fail (drawable == NULL || GDK_IS_DRAWABLE (drawable));

  if (drawable == images->drawable)
    return;

  games_card_images_clear_cache (images);

  images->drawable = drawable;
}

/**
 * games_card_images_set_antialias:
 * @images:
 * @antialias: the antialiasing mode to use (see @cairo_antialias_t)
 * @subpixel_order: the subpixel order to use (see @cairo_subpixel_order_t)
 * if @antialias is %CAIRO_ANTIALIAS_SUBPIXEL 
 *
 * Turns on antialising of cards, if using a scalable theme.
 * Changing the antialias settings invalidates all cached pixbufs and pixmaps.
 */
void
games_card_images_set_antialias (GamesCardImages * images,
                                 guint antialias, guint subpixel_order)
{
  g_return_if_fail (GAMES_IS_CARD_IMAGES (images));

  games_card_images_clear_cache (images);

  games_card_theme_set_antialias (images->theme, antialias, subpixel_order);
}

/**
 * games_card_images_set_theme:
 * @images:
 * @theme_name: the name of the theme to load
 *
 * Loads the card theme @theme_name. If the card theme cannot be loaded,
 * it falls back to the default card theme, if present.
 * Changing the card theme invalidates all cache pixbufs and pixmaps.
 * After changing the theme, the card size will be undefined; you need
 * to call games_card_images_set_size() to set it before getting a
 * card from @images again.
 * 
 * Returns: %TRUE iff loading the new card theme succeeded
 */
gboolean
games_card_images_set_theme (GamesCardImages * images,
                             const gchar * theme_name)
{
  const char *old_theme;

  g_return_val_if_fail (GAMES_IS_CARD_IMAGES (images), FALSE);
  g_return_val_if_fail (theme_name != NULL && theme_name[0] != '\0', FALSE);

  old_theme = games_card_theme_get_theme (images->theme);
  if (old_theme != NULL && strcmp (old_theme, theme_name) == 0)
    return TRUE;

  /* We need to clear the cache even if changing the theme fails! */
  games_card_images_clear_cache (images);

  return games_card_theme_set_theme (images->theme, theme_name);
}

/**
 * games_card_images_get_theme:
 * @images:
 *
 * Returns: the name of the currently loaded card theme, or %NULL if no theme
 * is loaded
 */
const gchar *
games_card_images_get_theme (GamesCardImages * images)
{
  g_return_val_if_fail (GAMES_IS_CARD_IMAGES (images), NULL);

  return games_card_theme_get_theme (images->theme);
}

/**
 * games_card_images_set_size:
 * @images:
 * @width: the maximum width
 * @height: the maximum height
 * @proportion: how much of @width and @height to use for the cards
 *
 * Calculates the card size to use. The passed-in dimensions are not used
 * directly; instead the width and height used are calculated using the
 * card theme's aspect ratio and, if using a prerendered card theme, from the
 * available sizes. You can use games_card_images_get_size() to get
 * the real card size afterwards.
 * If the card size was changed, all cached pixbufs and pixmaps will
 * have been invalidated.
 *
 * Returns: %TRUE iff the card size was changed
 */
gboolean
games_card_images_set_size (GamesCardImages * images,
                            gint width, gint height, gdouble proportion)
{
  gboolean size_changed;

  size_changed = games_card_theme_set_size (images->theme,
                                            width, height, proportion);
  if (size_changed) {
    games_card_images_clear_cache (images);
  }

  return size_changed;
}

/**
 * games_card_images_get_size:
 * @images:
 *
 * Returns: the currently selected card size
 */
CardSize
games_card_images_get_size (GamesCardImages * images)
{
  return games_card_theme_get_size (images->theme);
}

/**
 * games_card_images_get_aspect:
 * @images:
 *
 * Returns: the aspect ratio of the cards in the currently loaded theme
 */
double
games_card_images_get_aspect (GamesCardImages * images)
{
  g_return_val_if_fail (GAMES_IS_CARD_IMAGES (images), 1.0);

  return games_card_theme_get_aspect (images->theme);
}

/**
 * games_card_images_set_background_color:
 * @images:
 * @colour:
 *
 * Sets the colour used for the background colour of card pixmaps.
 * Changing the selection colour invalidates all cached pixmaps.
 */
void
games_card_images_set_background_color (GamesCardImages * images,
                                        const GdkColor * color)
{
  g_return_if_fail (GAMES_IS_CARD_IMAGES (images));
  g_return_if_fail (color != NULL);

  if (memcmp (&images->background_colour, color, sizeof (GdkColor)) == 0)
    return;

  images->background_colour = *color;

  if (images->cache_mode == CACHE_PIXMAPS) {
    games_card_images_clear_cache (images);
  }
}

/**
 * games_card_images_set_selection_color:
 * @images:
 * @colour:
 *
 * Sets the colour used for highlighted card pixbufs.
 * Changing the selection colour invalidates all cached highlighted pixbufs
 * and pixmaps.
 */
void
games_card_images_set_selection_color (GamesCardImages * images,
                                       const GdkColor * color)
{
  g_return_if_fail (GAMES_IS_CARD_IMAGES (images));
  g_return_if_fail (color != NULL);

  if (memcmp (&images->selection_colour, color, sizeof (GdkColor)) == 0)
    return;

  images->selection_colour = *color;

  games_card_images_clear_cache (images);
}

/**
 * games_card_images_get_card_pixbuf_by_id:
 * @images:
 * @card_id:
 *
 * Returns a #GdkPixbuf for @card using the currently loaded
 * theme and the currently selected size.
 * Returns %NULL on failure.
 *
 * Returns: a #GdkPixbuf owned by @images; you must not change or unref it
 */
GdkPixbuf *
games_card_images_get_card_pixbuf_by_id (GamesCardImages * images,
                                         guint card_id, gboolean highlighted)
{
  gpointer data;
  GdkPixbuf *pixbuf;
  guint idx;

  g_return_val_if_fail (GAMES_IS_CARD_IMAGES (images), NULL);
  g_return_val_if_fail ((card_id >= 0)
                        && (card_id < GAMES_CARDS_TOTAL), NULL);
  g_return_val_if_fail (images->cache_mode == CACHE_PIXBUFS, NULL);

  idx = card_id;
  if (G_UNLIKELY (highlighted)) {
    idx += GAMES_CARDS_TOTAL;
  }

  data = images->cache[idx];
  pixbuf = UNMARK_POINTER (data);

  /* If we already have a pixbuf and it's transformed (if necessary), we just return it */
  if (pixbuf != NULL &&
      (!highlighted || HAS_MARK (data, MARK_IS_TRANSFORMED))) {
    g_assert (!HAS_MARK (data, MARK_IS_PIXMAP));
    g_assert (GDK_IS_PIXBUF (pixbuf));
    return pixbuf;
  }

  /* Create the pixbuf */
  if (!pixbuf) {
    pixbuf = create_pixbuf (images, card_id);
    if (!pixbuf)
      return NULL;
  }

  if (G_UNLIKELY (highlighted)) {
    pixbuf = transform_pixbuf (images, pixbuf, idx);
    if (!pixbuf)
      return NULL;
  }

  g_assert (pixbuf == UNMARK_POINTER (images->cache[idx]));
  g_assert (!highlighted
            || HAS_MARK (images->cache[idx], MARK_IS_TRANSFORMED));

  return pixbuf;
}

/**
 * games_card_images_get_card_pixbuf:
 * @images:
 * @card:
 *
 * Returns a #GdkPixbuf for @card using the currently loaded
 * theme and the currently selected size.
 * Returns %NULL on failure.
 *
 * Returns: a #GdkPixbuf owned by @images; you must not change or unref it
 */
GdkPixbuf *
games_card_images_get_card_pixbuf (GamesCardImages * images,
                                   Card card, gboolean highlighted)
{
  return games_card_images_get_card_pixbuf_by_id (images,
                                                  card_to_index (card),
                                                  highlighted);
}

/**
 * games_card_images_get_card_pixmap_by_id:
 * @images:
 * @card_id:
 *
 * Returns a #GdkPixmap for @card_id using the currently loaded
 * theme and the currently selected size.
 * You must set a drawable on @images before using this function,
 * see games_card_images_set_drawable().
 * Returns %NULL on failure.
 *
 * Returns: a #GdkPixmap owned by @images; you must not change or unref it
 */
GdkPixmap *
games_card_images_get_card_pixmap_by_id (GamesCardImages * images,
                                         guint card_id, gboolean highlighted)
{
  gpointer data, pixbuf_or_pixmap;
  GdkPixbuf *pixbuf;
  GdkPixmap *pixmap;
  GdkGC *gc;
  int width, height;
  guint idx;

  g_return_val_if_fail (GAMES_IS_CARD_IMAGES (images), NULL);
  g_return_val_if_fail ((card_id >= 0)
                        && (card_id < GAMES_CARDS_TOTAL), NULL);
  g_return_val_if_fail (images->cache_mode == CACHE_PIXMAPS, NULL);
  g_return_val_if_fail (images->drawable != NULL, NULL);

  idx = card_id;
  if (G_UNLIKELY (highlighted)) {
    idx += GAMES_CARDS_TOTAL;
  }

  data = images->cache[idx];
  pixbuf_or_pixmap = UNMARK_POINTER (data);

  /* If we have a pixmap, it will already be transformed (if necessary); just return it */
  if (pixbuf_or_pixmap != NULL && HAS_MARK (data, MARK_IS_PIXMAP)) {
    g_assert (!highlighted || HAS_MARK (data, MARK_IS_TRANSFORMED));
    g_assert (GDK_IS_PIXMAP (pixbuf_or_pixmap));

    return pixbuf_or_pixmap;
  }

  /* We either have a pixbuf, or need to create it first */
  if (!pixbuf_or_pixmap) {
    pixbuf = create_pixbuf (images, card_id);
    if (!pixbuf)
      return NULL;

  } else {
    pixbuf = pixbuf_or_pixmap;
  }

  g_assert (GDK_IS_PIXBUF (pixbuf));
  g_assert (pixbuf == UNMARK_POINTER (images->cache[idx]));

  if (G_UNLIKELY
      (highlighted && !HAS_MARK (images->cache[idx], MARK_IS_TRANSFORMED))) {
    pixbuf = transform_pixbuf (images, pixbuf, idx);
    if (!pixbuf)
      return NULL;
  }

  g_assert (pixbuf == UNMARK_POINTER (images->cache[idx]));
  g_assert (!highlighted
            || HAS_MARK (images->cache[idx], MARK_IS_TRANSFORMED));

  width = gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);

  /* If we don't have the mask yet, create it now */
  if (G_UNLIKELY (card_id == GAMES_CARD_BACK && images->card_mask == NULL)) {
    images->card_mask =
      create_mask (images->drawable, pixbuf, CARD_ALPHA_THRESHOLD);
  } else
    if (G_UNLIKELY (card_id == GAMES_CARD_SLOT && images->slot_mask == NULL))
  {
    images->slot_mask =
      create_mask (images->drawable, pixbuf, SLOT_ALPHA_THRESHOLD);
  }

  pixmap = gdk_pixmap_new (images->drawable, width, height, -1);
  if (!pixmap)
    return NULL;

  /* Clear the pixmap, to eliminate artifacts from uninitialised
   * pixels when drawing the pixmap with gdk_draw_drawable.
   */
  gc = gdk_gc_new (pixmap);
  gdk_gc_set_foreground (gc, &images->background_colour);
  gdk_draw_rectangle (pixmap, gc, TRUE, 0, 0, width, height);
  g_object_unref (gc);

  gdk_draw_pixbuf (pixmap, NULL, pixbuf,
                   0, 0, 0, 0, width, height, GDK_RGB_DITHER_NORMAL, 0, 0);

  /* Replace the cache entry */
  g_object_unref (pixbuf);

  if (G_UNLIKELY (highlighted)) {
    images->cache[idx] =
      MARK_POINTER (pixmap, MARK_IS_PIXMAP | MARK_IS_TRANSFORMED);
  } else {
    images->cache[idx] = MARK_POINTER (pixmap, MARK_IS_PIXMAP);
  }

  return pixmap;
}

/**
 * games_card_pixmaps_get_card_pixmap_by_id:
 * @images:
 * @card_id:
 *
 * Returns a #GdkPixmap for @card using the currently loaded
 * theme and the currently selected size.
 * You must set a drawable on @images before using this function,
 * see games_card_images_set_drawable().
 * Returns %NULL on failure.
 *
 * Returns: a #GdkPixmap owned by @images; you must not change or unref it
 */
GdkPixmap *
games_card_images_get_card_pixmap (GamesCardImages * images,
                                   Card card, gboolean highlighted)
{
  return games_card_images_get_card_pixmap_by_id (images,
                                                  card_to_index (card),
                                                  highlighted);
}

/**
 * games_card_images_get_card_mask:
 * @images:
 *
 * Returns the card mask for the currently loaded theme at the currently
 * selected size.
 * You must set a drawable on @images before using this function,
 * see games_card_images_set_drawable().
 * Returns %NULL on failure.
 * 
 * Returns: a #GdkBitmap owned by @images; you must not change or unref it
 */
GdkBitmap *
games_card_images_get_card_mask (GamesCardImages * images)
{
  g_return_val_if_fail (GAMES_IS_CARD_IMAGES (images), NULL);
  g_return_val_if_fail (images->drawable != NULL, NULL);

  if (images->card_mask == NULL) {
    if (images->cache_mode == CACHE_PIXMAPS) {
      /* Always use the back of the cards to draw the mask */
      games_card_images_get_card_pixmap_by_id (images, GAMES_CARD_BACK,
                                               FALSE);
    } else {
      images->card_mask = create_mask (images->drawable,
                                       games_card_images_get_card_pixbuf_by_id
                                       (images, GAMES_CARD_BACK, FALSE),
                                       CARD_ALPHA_THRESHOLD);
    }
  }

  return images->card_mask;
}

/**
 * games_card_images_get_slot_pixbuf:
 * @images:
 *
 * Returns a #GdkPixbuf for card slot using the currently loaded
 * theme and the currently selected size.
 * Returns %NULL on failure.
 *
 * Returns: a #GdkPixbuf owned by @images; you must not change or unref it
 */
GdkPixbuf *
games_card_images_get_slot_pixbuf (GamesCardImages * images,
                                   gboolean highlighted)
{
  return games_card_images_get_card_pixbuf_by_id (images,
                                                  GAMES_CARD_SLOT,
                                                  highlighted);
}

/**
 * games_card_images_get_slot_pixmap:
 * @images:
 *
 * Returns a #GdkPixbuf for card slot using the currently loaded
 * theme at the currently selected size.
 * You must set a drawable on @images before using this function,
 * see games_card_images_set_drawable().
 * Returns %NULL on failure.
 * 
 * Returns: a #GdkPixmap owned by @images; you must not change or unref it
 */
GdkPixmap *
games_card_images_get_slot_pixmap (GamesCardImages * images,
                                   gboolean highlighted)
{
  return games_card_images_get_card_pixmap_by_id (images,
                                                  GAMES_CARD_SLOT,
                                                  highlighted);
}

/**
 * games_card_images_get_slot_mask:
 * @images:
 *
 * Returns the slot mask for the currently loaded theme at the currently
 * selected size.
 * You must set a drawable on @images before using this function,
 * see games_card_images_set_drawable().
 * Returns %NULL on failure.
 * 
 * Returns: a #GdkBitmap owned by @images; you must not change or unref it
 */
GdkBitmap *
games_card_images_get_slot_mask (GamesCardImages * images)
{
  g_return_val_if_fail (GAMES_IS_CARD_IMAGES (images), NULL);
  g_return_val_if_fail (images->drawable != NULL, NULL);

  if (images->slot_mask == NULL) {
    if (images->cache_mode == CACHE_PIXMAPS) {
      games_card_images_get_slot_pixmap (images, FALSE);
    } else {
      images->slot_mask = create_mask (images->drawable,
                                       games_card_images_get_slot_pixbuf
                                       (images, FALSE), SLOT_ALPHA_THRESHOLD);
    }
  }

  return images->slot_mask;
}

/* Deprecated, going to remove these! */

/**
 * games_card_images_get_card_pixbuf_by_suit_and_rank:
 * @images:
 * @suit:
 * @rank:
 *
 * Returns a #GdkPixbuf for the selected card using the currently loaded
 * theme and the currently selected size.
 *
 * Returns: a #GdkPixbuf owned by @images; you must not change or unref it
 */
GdkPixbuf *
games_card_images_get_card_pixbuf_by_suit_and_rank (GamesCardImages * images,
                                                    guint suit, guint rank)
{
  g_return_val_if_fail (GAMES_IS_CARD_IMAGES (images), NULL);

  return games_card_images_get_card_pixbuf_by_id (images,
                                                  GAMES_CARD_ID (suit, rank),
                                                  FALSE);
}

/**
 * games_card_images_get_card_pixmap_by_suit_and_rank:
 * @images:
 * @suit:
 * @rank:
 *
 * Returns a #GdkPixmap for the selected card using the currently loaded
 * theme and the currently selected size.
 * You must set a drawable on @images before using this function,
 * see games_card_images_set_drawable().
 * Returns %NULL on failure.
 *
 * Returns: a #GdkPixmap owned by @images; you must not change or unref it
 */
GdkPixmap *
games_card_images_get_card_pixmap_by_suit_and_rank (GamesCardImages * images,
                                                    guint suit, guint rank)
{
  g_return_val_if_fail (((suit >= GAMES_CARDS_CLUBS) &&
                         (suit <= GAMES_CARDS_SPADES)), NULL);
  g_return_val_if_fail (((rank >= GAMES_CARD_ACE) &&
                         (rank <= GAMES_CARD_ACE_HIGH)), NULL);

  return games_card_images_get_card_pixmap_by_id (images,
                                                  GAMES_CARD_ID (suit, rank),
                                                  FALSE);
}
