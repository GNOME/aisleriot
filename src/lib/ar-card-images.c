/* 
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008 Christian Persch

  This library is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

/* Manage a set of pixbufs containing a deck of cards. */

#include <config.h>

#include <string.h>
#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include "ar-debug.h"

#include "ar-card-images.h"

#include "ar-pixbuf-utils.h"
#include "ar-card.h"
#include "ar-card-private.h"

struct _ArCardImagesClass {
  GObjectClass parent_class;
};

struct _ArCardImages {
  GObject parent;

  ArCardTheme *theme;
  GdkDrawable *drawable;
  GdkBitmap *card_mask;
  GdkBitmap *slot_mask;
  gpointer *cache;
  GdkColor background_colour;
  GdkColor selection_colour;

  guint cache_mode;

#ifdef GNOME_ENABLE_DEBUG
  guint n_calls;
  guint cache_hits;
#endif
};

enum {
  PROP_0,
  PROP_THEME
};

/* MARK_IS_TRANSFORMED must be the same value as AR_CARD_IMAGES_HIGHLIGHTED ! */
enum {
  MARK_IS_PIXMAP = 1U << 0,
  MARK_IS_TRANSFORMED = 1U << 1,
  MARK_MASK = 0x3U
};

/* This is an invalid value for a GObject* */
#define FAILED_POINTER ((gpointer) 0x4)

#define GET_MARK(ptr) ((gsize) (ptr) & (gsize) MARK_MASK)
#define HAS_MARK(ptr,flags) ((gsize) GET_MARK (ptr) & (gsize) (flags))
#define MARK_POINTER(ptr,flags) ((gpointer) ((gsize) (ptr) | (gsize) (flags)))
#define UNMARK_POINTER(ptr) ((gpointer) ((gsize) (ptr) & ~(gsize) MARK_MASK))

#define IS_FAILED_POINTER(ptr) (G_UNLIKELY ((ptr) == FAILED_POINTER))

#define CACHE_SIZE  (2 * AR_CARDS_TOTAL)

/* Threshold for rendering the pixbuf to card and alpha mask */
#define CARD_ALPHA_THRESHOLD  (127)
#define SLOT_ALPHA_THRESHOLD  (255)

/* Logging */
#ifdef GNOME_ENABLE_DEBUG
#define LOG_CALL(obj) obj->n_calls++
#define LOG_CACHE_HIT(obj) obj->cache_hits++
#define LOG_CACHE_MISS(obj)
#else
#define LOG_CALL(obj)
#define LOG_CACHE_HIT(obj)
#define LOG_CACHE_MISS(obj)
#endif /* GNOME_ENABLE_DEBUG */

static void
ar_card_images_clear_cache (ArCardImages * images)
{
  guint i;

  ar_debug_print (AR_DEBUG_CARD_CACHE,
                      "ar_card_images_clear_cache\n");

  for (i = 0; i < CACHE_SIZE; i++) {
    gpointer data = UNMARK_POINTER (images->cache[i]);

    if (data != NULL && !IS_FAILED_POINTER (data)) {
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

static void
ar_card_images_theme_changed_cb (ArCardTheme * theme,
                                    ArCardImages * images)
{
  ar_card_images_clear_cache (images);
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
create_pixbuf (ArCardImages * images, guint card_id)
{
  GdkPixbuf *pixbuf;

  g_assert (images->cache[card_id] == NULL &&
            images->cache[card_id + AR_CARDS_TOTAL] == NULL);

  pixbuf = ar_card_theme_get_card_pixbuf (images->theme, card_id);
  if (!pixbuf)
    return NULL;

  images->cache[card_id] = pixbuf;
  images->cache[card_id + AR_CARDS_TOTAL] = g_object_ref (pixbuf);

  return pixbuf;
}

static GdkPixbuf *
transform_pixbuf (ArCardImages * images, GdkPixbuf * pixbuf, guint idx)
{
  GdkPixbuf *transformed_pixbuf;

  g_assert (pixbuf != NULL);
  g_assert (pixbuf == UNMARK_POINTER (images->cache[idx]));

  transformed_pixbuf = ar_pixbuf_utils_create_highlight (pixbuf,
                                                         &images->selection_colour);
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

G_DEFINE_TYPE (ArCardImages, ar_card_images, G_TYPE_OBJECT);

static void
ar_card_images_init (ArCardImages * images)
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

static void
ar_card_images_finalize (GObject * object)
{
  ArCardImages *images = AR_CARD_IMAGES (object);

  ar_card_images_clear_cache (images);
  g_free (images->cache);

  if (images->theme) {
    g_signal_handlers_disconnect_by_func (images->theme,
                                          G_CALLBACK (ar_card_images_theme_changed_cb),
                                          images);
    g_object_unref (images->theme);
  }

#ifdef GNOME_ENABLE_DEBUG
  _AR_DEBUG_IF (AR_DEBUG_CARD_CACHE) {
    ar_debug_print (AR_DEBUG_CARD_CACHE,
                        "ArCardImages %p statistics: %u calls with %u hits and %u misses for a hit/total of %.3f\n",
                        images, images->n_calls, images->cache_hits, images->n_calls - images->cache_hits,
                        images->n_calls > 0 ? (double) images->cache_hits / (double) images->n_calls : 0.0);
  }
#endif

  G_OBJECT_CLASS (ar_card_images_parent_class)->finalize (object);
}

static void
ar_card_images_set_property (GObject *object,
                                guint prop_id,
                                const GValue *value,
                                GParamSpec *pspec)
{
  ArCardImages *images = AR_CARD_IMAGES (object);

  switch (prop_id) {
    case PROP_THEME:
      ar_card_images_set_theme (images, g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
ar_card_images_get_property (GObject *object,
                                guint prop_id,
                                GValue *value,
                                GParamSpec * pspec)
{
  ArCardImages *images = AR_CARD_IMAGES (object);

  switch (prop_id) {
    case PROP_THEME:
      g_value_set_object (value, ar_card_images_get_theme (images));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
ar_card_images_class_init (ArCardImagesClass * class)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (class);

  gobject_class->get_property = ar_card_images_get_property;
  gobject_class->set_property = ar_card_images_set_property;
  gobject_class->finalize = ar_card_images_finalize;

  g_object_class_install_property
    (gobject_class,
     PROP_THEME,
     g_param_spec_object ("theme", NULL, NULL,
                          AR_TYPE_CARD_THEME,
                          G_PARAM_READWRITE |
                          G_PARAM_STATIC_NAME |
                          G_PARAM_STATIC_NICK |
                          G_PARAM_STATIC_BLURB));
}

/* public API */

/**
 * ar_card_images_new:
 *
 * Returns: a new #ArCardImages
 */
ArCardImages *
ar_card_images_new (void)
{
  return g_object_new (AR_TYPE_CARD_IMAGES, NULL);
}

/**
 * ar_card_images_set_theme:
 * @images:
 * @theme: a #ArCardTheme
 *
 * Sets @theme in @images.
 */
void
ar_card_images_set_theme (ArCardImages *images,
                             ArCardTheme *theme)
{
  g_return_if_fail (AR_IS_CARD_IMAGES (images));
  g_return_if_fail (AR_IS_CARD_THEME (theme));

  if (images->theme == theme)
    return;

  if (images->theme) {
    g_signal_handlers_disconnect_by_func (images->theme,
                                          G_CALLBACK (ar_card_images_theme_changed_cb),
                                          images);
    ar_card_images_clear_cache (images);
    g_object_unref (images->theme);
  }

  images->theme = theme;
  if (theme) {
    g_object_ref (images->theme);
    g_signal_connect (images->theme, "changed",
                      G_CALLBACK (ar_card_images_theme_changed_cb), images);
  }

  g_object_notify (G_OBJECT (images), "theme");
}

/**
 * ar_card_images_get_theme:
 * @images:
 *
 * Returns the #ArCardTheme currently in use in @images.
 */
ArCardTheme *
ar_card_images_get_theme (ArCardImages *images)
{
  g_return_val_if_fail (AR_IS_CARD_IMAGES (images), NULL);

  return images->theme;
}

/**
 * ar_card_images_set_cache_mode:
 * @images:
 * @mode:
 *
 * Select whether @images stores #GdkPixbuf:s or #GdkPixmap:s.
 * It is an error to use ar_card_images_get_*_pixbuf*() while
 * storing pixmaps, and to use ar_card_images_get_*_pixmap*() while
 * storing pixbufs.
 * Changing the cache mode invalidates all stored images.
 */
void
ar_card_images_set_cache_mode (ArCardImages * images,
                                  ArCardImagesCacheMode mode)
{
  g_return_if_fail (AR_IS_CARD_IMAGES (images));
  g_return_if_fail (mode < LAST_CACHE_MODE);

  if (mode == images->cache_mode)
    return;

  /* If we were storing pixmaps, we need to clear them */
  if (images->cache_mode == CACHE_PIXMAPS) {
    ar_card_images_clear_cache (images);
  }

  images->cache_mode = mode;
}

/**
 * ar_card_images_drop_cache:
 * @images: a #ArCardImages
 *
 * Clears the image cache.
 */
void
ar_card_images_drop_cache (ArCardImages *images)
{
  g_return_if_fail (AR_IS_CARD_IMAGES (images));

  ar_card_images_clear_cache (images);
}

/**
 * ar_card_images_set_drawable:
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
ar_card_images_set_drawable (ArCardImages * images,
                                GdkWindow * drawable)
{
  g_return_if_fail (AR_IS_CARD_IMAGES (images));
  g_return_if_fail (drawable == NULL || GDK_IS_DRAWABLE (drawable));

  if (drawable == images->drawable)
    return;

  ar_card_images_clear_cache (images);

  images->drawable = drawable;
}

/**
 * ar_card_images_set_size:
 * @images:
 * @width: the maximum width
 * @height: the maximum height
 * @proportion: how much of @width and @height to use for the cards
 *
 * Calculates the card size to use. The passed-in dimensions are not used
 * directly; instead the width and height used are calculated using the
 * card theme's aspect ratio and, if using a prerendered card theme, from the
 * available sizes. You can use ar_card_images_get_size() to get
 * the real card size afterwards.
 * If the card size was changed, all cached pixbufs and pixmaps will
 * have been invalidated.
 *
 * Returns: %TRUE iff the card size was changed
 */
gboolean
ar_card_images_set_size (ArCardImages * images,
                            gint width, gint height, gdouble proportion)
{
  return ar_card_theme_set_size (images->theme,
                                    width, height, proportion);
}

/**
 * ar_card_images_get_size:
 * @images:
 *
 * Returns: the currently selected card size
 */
void
ar_card_images_get_size (ArCardImages *images,
                            CardSize *size)
{
  ar_card_theme_get_size (images->theme, size);
}

/**
 * ar_card_images_set_background_color:
 * @images:
 * @colour:
 *
 * Sets the colour used for the background colour of card pixmaps.
 * Changing the selection colour invalidates all cached pixmaps.
 */
void
ar_card_images_set_background_color (ArCardImages * images,
                                        const GdkColor * color)
{
  g_return_if_fail (AR_IS_CARD_IMAGES (images));
  g_return_if_fail (color != NULL);

  if (gdk_color_equal (&images->background_colour, color))
    return;

  images->background_colour = *color;

  if (images->cache_mode == CACHE_PIXMAPS) {
    ar_card_images_clear_cache (images);
  }
}

/**
 * ar_card_images_set_selection_color:
 * @images:
 * @colour:
 *
 * Sets the colour used for highlighted card pixbufs.
 * Changing the selection colour invalidates all cached highlighted pixbufs
 * and pixmaps.
 */
void
ar_card_images_set_selection_color (ArCardImages * images,
                                       const GdkColor * color)
{
  g_return_if_fail (AR_IS_CARD_IMAGES (images));
  g_return_if_fail (color != NULL);

  if (gdk_color_equal (&images->selection_colour, color))
    return;

  images->selection_colour = *color;

  ar_card_images_clear_cache (images);
}

/**
 * ar_card_images_get_card_pixbuf_by_id:
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
ar_card_images_get_card_pixbuf_by_id (ArCardImages * images,
                                         guint card_id, gboolean highlighted)
{
  gpointer data;
  GdkPixbuf *pixbuf;
  guint idx;

  g_return_val_if_fail (AR_IS_CARD_IMAGES (images), NULL);
  g_return_val_if_fail ((card_id < AR_CARDS_TOTAL), NULL);
  g_return_val_if_fail (images->cache_mode == CACHE_PIXBUFS, NULL);

  LOG_CALL (images);

  idx = card_id;
  if (G_UNLIKELY (highlighted)) {
    idx += AR_CARDS_TOTAL;
  }

  data = images->cache[idx];
  if (IS_FAILED_POINTER (data)) {
    LOG_CACHE_HIT (images);
    return NULL;
  }

  pixbuf = UNMARK_POINTER (data);

  /* If we already have a pixbuf and it's transformed (if necessary), we just return it */
  if (pixbuf != NULL &&
      (!highlighted || HAS_MARK (data, MARK_IS_TRANSFORMED))) {
    g_assert (!HAS_MARK (data, MARK_IS_PIXMAP));
    g_assert (GDK_IS_PIXBUF (pixbuf));
    LOG_CACHE_HIT (images);
    return pixbuf;
  }

  LOG_CACHE_MISS (images);

  /* Create the pixbuf */
  if (!pixbuf) {
    pixbuf = create_pixbuf (images, card_id);
    if (!pixbuf) {
      images->cache[idx] = FAILED_POINTER;
      return NULL;
    }
  }

  if (G_UNLIKELY (highlighted)) {
    pixbuf = transform_pixbuf (images, pixbuf, idx);
    if (!pixbuf) {
      images->cache[idx] = FAILED_POINTER;
      return NULL;
    }
  }

  g_assert (pixbuf == UNMARK_POINTER (images->cache[idx]));
  g_assert (!highlighted
            || HAS_MARK (images->cache[idx], MARK_IS_TRANSFORMED));

  return pixbuf;
}

/**
 * ar_card_images_get_card_pixbuf:
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
ar_card_images_get_card_pixbuf (ArCardImages * images,
                                   Card card, gboolean highlighted)
{
  return ar_card_images_get_card_pixbuf_by_id (images,
                                                  _ar_card_to_index (card),
                                                  highlighted);
}

/**
 * ar_card_images_get_card_pixmap_by_id:
 * @images:
 * @card_id:
 *
 * Returns a #GdkPixmap for @card_id using the currently loaded
 * theme and the currently selected size.
 * You must set a drawable on @images before using this function,
 * see ar_card_images_set_drawable().
 * Returns %NULL on failure.
 *
 * Returns: a #GdkPixmap owned by @images; you must not change or unref it
 */
GdkPixmap *
ar_card_images_get_card_pixmap_by_id (ArCardImages * images,
                                         guint card_id, gboolean highlighted)
{
  gpointer data, pixbuf_or_pixmap;
  GdkPixbuf *pixbuf;
  GdkPixmap *pixmap;
  GdkGC *gc;
  int width, height;
  guint idx;

  g_return_val_if_fail (AR_IS_CARD_IMAGES (images), NULL);
  g_return_val_if_fail ((card_id < AR_CARDS_TOTAL), NULL);
  g_return_val_if_fail (images->cache_mode == CACHE_PIXMAPS, NULL);
  g_return_val_if_fail (images->drawable != NULL, NULL);

  LOG_CALL (images);

  idx = card_id;
  if (G_UNLIKELY (highlighted)) {
    idx += AR_CARDS_TOTAL;
  }

  data = images->cache[idx];
  if (IS_FAILED_POINTER (data)) {
    LOG_CACHE_HIT (images);
    return NULL;
  }

  pixbuf_or_pixmap = UNMARK_POINTER (data);

  /* If we have a pixmap, it will already be transformed (if necessary); just return it */
  if (pixbuf_or_pixmap != NULL && HAS_MARK (data, MARK_IS_PIXMAP)) {
    g_assert (!highlighted || HAS_MARK (data, MARK_IS_TRANSFORMED));
    g_assert (GDK_IS_PIXMAP (pixbuf_or_pixmap));

    LOG_CACHE_HIT (images);
    return pixbuf_or_pixmap;
  }

  LOG_CACHE_MISS (images);

  /* We either have a pixbuf, or need to create it first */
  if (!pixbuf_or_pixmap) {
    pixbuf = create_pixbuf (images, card_id);
    if (!pixbuf) {
      images->cache[idx] = FAILED_POINTER;
      return NULL;
    }

  } else {
    pixbuf = pixbuf_or_pixmap;
  }

  g_assert (GDK_IS_PIXBUF (pixbuf));
  g_assert (pixbuf == UNMARK_POINTER (images->cache[idx]));

  if (G_UNLIKELY
      (highlighted && !HAS_MARK (images->cache[idx], MARK_IS_TRANSFORMED))) {
    pixbuf = transform_pixbuf (images, pixbuf, idx);
    if (!pixbuf) {
      images->cache[idx] = FAILED_POINTER;
      return NULL;
    }
  }

  g_assert (pixbuf == UNMARK_POINTER (images->cache[idx]));
  g_assert (!highlighted
            || HAS_MARK (images->cache[idx], MARK_IS_TRANSFORMED));

  width = gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);

  /* If we don't have the mask yet, create it now */
  if (G_UNLIKELY (card_id == AR_CARD_BACK && images->card_mask == NULL)) {
    images->card_mask =
      create_mask (images->drawable, pixbuf, CARD_ALPHA_THRESHOLD);
  } else
    if (G_UNLIKELY (card_id == AR_CARD_SLOT && images->slot_mask == NULL))
  {
    images->slot_mask =
      create_mask (images->drawable, pixbuf, SLOT_ALPHA_THRESHOLD);
  }

  pixmap = gdk_pixmap_new (images->drawable, width, height, -1);
  if (!pixmap) {
    images->cache[idx] = FAILED_POINTER;
    return NULL;
  }

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
 * ar_card_pixmaps_get_card_pixmap_by_id:
 * @images:
 * @card_id:
 *
 * Returns a #GdkPixmap for @card using the currently loaded
 * theme and the currently selected size.
 * You must set a drawable on @images before using this function,
 * see ar_card_images_set_drawable().
 * Returns %NULL on failure.
 *
 * Returns: a #GdkPixmap owned by @images; you must not change or unref it
 */
GdkPixmap *
ar_card_images_get_card_pixmap (ArCardImages * images,
                                   Card card, gboolean highlighted)
{
  return ar_card_images_get_card_pixmap_by_id (images,
                                                  _ar_card_to_index (card),
                                                  highlighted);
}

/**
 * ar_card_images_get_card_mask:
 * @images:
 *
 * Returns the card mask for the currently loaded theme at the currently
 * selected size.
 * You must set a drawable on @images before using this function,
 * see ar_card_images_set_drawable().
 * Returns %NULL on failure.
 * 
 * Returns: a #GdkBitmap owned by @images; you must not change or unref it
 */
GdkBitmap *
ar_card_images_get_card_mask (ArCardImages * images)
{
  g_return_val_if_fail (AR_IS_CARD_IMAGES (images), NULL);
  g_return_val_if_fail (images->drawable != NULL, NULL);

  if (images->card_mask == NULL) {
    if (images->cache_mode == CACHE_PIXMAPS) {
      /* Always use the back of the cards to draw the mask */
      ar_card_images_get_card_pixmap_by_id (images, AR_CARD_BACK,
                                               FALSE);
    } else {
      images->card_mask = create_mask (images->drawable,
                                       ar_card_images_get_card_pixbuf_by_id
                                       (images, AR_CARD_BACK, FALSE),
                                       CARD_ALPHA_THRESHOLD);
    }
  }

  return images->card_mask;
}

/**
 * ar_card_images_get_slot_pixbuf:
 * @images:
 *
 * Returns a #GdkPixbuf for card slot using the currently loaded
 * theme and the currently selected size.
 * Returns %NULL on failure.
 *
 * Returns: a #GdkPixbuf owned by @images; you must not change or unref it
 */
GdkPixbuf *
ar_card_images_get_slot_pixbuf (ArCardImages * images,
                                   gboolean highlighted)
{
  return ar_card_images_get_card_pixbuf_by_id (images,
                                                  AR_CARD_SLOT,
                                                  highlighted);
}

/**
 * ar_card_images_get_slot_pixmap:
 * @images:
 *
 * Returns a #GdkPixbuf for card slot using the currently loaded
 * theme at the currently selected size.
 * You must set a drawable on @images before using this function,
 * see ar_card_images_set_drawable().
 * Returns %NULL on failure.
 * 
 * Returns: a #GdkPixmap owned by @images; you must not change or unref it
 */
GdkPixmap *
ar_card_images_get_slot_pixmap (ArCardImages * images,
                                   gboolean highlighted)
{
  return ar_card_images_get_card_pixmap_by_id (images,
                                                  AR_CARD_SLOT,
                                                  highlighted);
}

/**
 * ar_card_images_get_slot_mask:
 * @images:
 *
 * Returns the slot mask for the currently loaded theme at the currently
 * selected size.
 * You must set a drawable on @images before using this function,
 * see ar_card_images_set_drawable().
 * Returns %NULL on failure.
 * 
 * Returns: a #GdkBitmap owned by @images; you must not change or unref it
 */
GdkBitmap *
ar_card_images_get_slot_mask (ArCardImages * images)
{
  g_return_val_if_fail (AR_IS_CARD_IMAGES (images), NULL);
  g_return_val_if_fail (images->drawable != NULL, NULL);

  if (images->slot_mask == NULL) {
    if (images->cache_mode == CACHE_PIXMAPS) {
      ar_card_images_get_slot_pixmap (images, FALSE);
    } else {
      images->slot_mask = create_mask (images->drawable,
                                       ar_card_images_get_slot_pixbuf
                                       (images, FALSE), SLOT_ALPHA_THRESHOLD);
    }
  }

  return images->slot_mask;
}

/* Deprecated, going to remove these! */

/**
 * ar_card_images_get_card_pixbuf_by_suit_and_rank:
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
ar_card_images_get_card_pixbuf_by_suit_and_rank (ArCardImages * images,
                                                    guint suit, guint rank)
{
  g_return_val_if_fail (AR_IS_CARD_IMAGES (images), NULL);

  return ar_card_images_get_card_pixbuf_by_id (images,
                                                  AR_CARD_ID (suit, rank),
                                                  FALSE);
}

/**
 * ar_card_images_get_card_pixmap_by_suit_and_rank:
 * @images:
 * @suit:
 * @rank:
 *
 * Returns a #GdkPixmap for the selected card using the currently loaded
 * theme and the currently selected size.
 * You must set a drawable on @images before using this function,
 * see ar_card_images_set_drawable().
 * Returns %NULL on failure.
 *
 * Returns: a #GdkPixmap owned by @images; you must not change or unref it
 */
GdkPixmap *
ar_card_images_get_card_pixmap_by_suit_and_rank (ArCardImages * images,
                                                    guint suit, guint rank)
{
  g_return_val_if_fail (( /* (suit >= AR_CARDS_CLUBS) && */
                         (suit <= AR_CARDS_SPADES)), NULL);
  g_return_val_if_fail (((rank >= AR_CARD_ACE) &&
                         (rank <= AR_CARD_ACE_HIGH)), NULL);

  return ar_card_images_get_card_pixmap_by_id (images,
                                                  AR_CARD_ID (suit, rank),
                                                  FALSE);
}
