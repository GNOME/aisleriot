/*
  Copyright © 2008 Neil Roberts
  Copyright © 2008, 2010 Christian Persch

  This program is free software: you can redistribute it and/or modify
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

#include <config.h>

#include "ar-debug.h"
#include <gdk/gdk.h>

#include "ar-card-surface-cache.h"
#include "ar-card-private.h"
#include "ar-card-theme-private.h"

struct _ArCardSurfaceCachePrivate
{
  ArCardTheme *theme;
  guint theme_changed_id;

  GdkWindow *drawable;
  cairo_surface_t **cards;

#ifdef GNOME_ENABLE_DEBUG
  guint n_calls;
  guint cache_hits;
#endif
};

enum
{
  PROP_0,
  PROP_THEME
};

/* This is an invalid value for a cairo_surface_t *, and distinct from COGL_INVALID_HANDLE */
#define FAILED_SURFACE ((gpointer) 0x1)
#define IS_FAILED_SURFACE(ptr) (G_UNLIKELY ((ptr) == FAILED_SURFACE))

/* Logging */
#ifdef GNOME_ENABLE_DEBUG
#define LOG_CALL(obj) obj->priv->n_calls++
#define LOG_CACHE_HIT(obj) obj->priv->cache_hits++
#define LOG_CACHE_MISS(obj)
#else
#define LOG_CALL(obj)
#define LOG_CACHE_HIT(obj)
#define LOG_CACHE_MISS(obj)
#endif /* GNOME_ENABLE_DEBUG */

static void ar_card_surface_cache_dispose (GObject *object);
static void ar_card_surface_cache_finalize (GObject *object);

G_DEFINE_TYPE_EXTENDED (ArCardSurfaceCache, ar_card_surface_cache, G_TYPE_OBJECT, 0,
                         G_ADD_PRIVATE (ArCardSurfaceCache));

#define AR_CARD_SURFACE_CACHE_GET_PRIVATE(obj) (ar_card_surface_cache_get_instance_private(obj))

/* Helper functions */

static void
ar_card_surface_cache_clear (ArCardSurfaceCache *cache)
{
  ArCardSurfaceCachePrivate *priv = cache->priv;
  int i;

  ar_debug_print (AR_DEBUG_CARD_CACHE,
                      "ar_card_surface_cache_clear\n");

  for (i = 0; i < AR_CARDS_TOTAL; i++) {
    cairo_surface_t *surface = priv->cards[i];

    if (surface != NULL &&
        !IS_FAILED_SURFACE (surface)) {
      cairo_surface_destroy (surface);
    }

    priv->cards[i] = NULL;
  }
}

static void
ar_card_surface_cache_unset_theme (ArCardSurfaceCache *cache)
{
  ArCardSurfaceCachePrivate *priv = cache->priv;

  if (priv->theme) {
    g_signal_handler_disconnect (priv->theme, priv->theme_changed_id);
    g_object_unref (priv->theme);
    priv->theme = NULL;
    priv->theme_changed_id = 0;
  }
}

/* Class implementation */

static void
ar_card_surface_cache_init (ArCardSurfaceCache *self)
{
  ArCardSurfaceCachePrivate *priv;

  priv = self->priv = AR_CARD_SURFACE_CACHE_GET_PRIVATE (self);

  priv->cards = g_malloc0 (sizeof (cairo_surface_t*) * AR_CARDS_TOTAL);
}

static void
ar_card_surface_cache_dispose (GObject *object)
{
  ArCardSurfaceCache *cache = AR_CARD_SURFACE_CACHE (object);

  ar_card_surface_cache_clear (cache);
  ar_card_surface_cache_unset_theme (cache);

  G_OBJECT_CLASS (ar_card_surface_cache_parent_class)->dispose (object);
}

static void
ar_card_surface_cache_finalize (GObject *object)
{
  ArCardSurfaceCache *cache = AR_CARD_SURFACE_CACHE (object);
  ArCardSurfaceCachePrivate *priv = cache->priv;

  g_free (priv->cards);

#ifdef GNOME_ENABLE_DEBUG
  _AR_DEBUG_IF (AR_DEBUG_CARD_CACHE) {
    ar_debug_print (AR_DEBUG_CARD_CACHE,
                        "ArCardSurfaceCache %p statistics: %u calls with %u hits and %u misses for a hit/total of %.3f\n",
                        cache, priv->n_calls, priv->cache_hits, priv->n_calls - priv->cache_hits,
                        priv->n_calls > 0 ? (double) priv->cache_hits / (double) priv->n_calls : 0.0);
  }
#endif

  G_OBJECT_CLASS (ar_card_surface_cache_parent_class)->finalize (object);
}

static void
ar_card_surface_cache_set_property (GObject *self,
                                        guint property_id,
                                        const GValue *value,
                                        GParamSpec *pspec)
{
  ArCardSurfaceCache *cache = AR_CARD_SURFACE_CACHE (self);

  switch (property_id) {
    case PROP_THEME:
      ar_card_surface_cache_set_theme (cache, g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (self, property_id, pspec);
      break;
    }
}

static void
ar_card_surface_cache_get_property (GObject *self,
                                        guint property_id,
                                        GValue *value,
                                        GParamSpec *pspec)
{
  ArCardSurfaceCache *cache = AR_CARD_SURFACE_CACHE (self);

  switch (property_id) {
    case PROP_THEME:
      g_value_set_object (value, ar_card_surface_cache_get_theme (cache));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (self, property_id, pspec);
      break;
    }
}

static void
ar_card_surface_cache_class_init (ArCardSurfaceCacheClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  gobject_class->dispose = ar_card_surface_cache_dispose;
  gobject_class->finalize = ar_card_surface_cache_finalize;
  gobject_class->set_property = ar_card_surface_cache_set_property;
  gobject_class->get_property = ar_card_surface_cache_get_property;

  g_object_class_install_property (gobject_class,
                                   PROP_THEME, 
                                   g_param_spec_object ("theme", NULL, NULL,
                                                        AR_TYPE_CARD_THEME,
                                                        G_PARAM_WRITABLE |
                                                        G_PARAM_CONSTRUCT_ONLY |
                                                        G_PARAM_STATIC_STRINGS));
}

/* Public API */

/**
 * ar_card_surface_cache_new:
 *
 * Returns: a new #ArCardSurfaceCache object
 */
ArCardSurfaceCache *
ar_card_surface_cache_new (void)
{
  return g_object_new (AR_TYPE_CARD_SURFACE_CACHE, NULL);
}

/**
 * ar_card_surface_cache_set_drawable:
 * @cache:
 * @drawable:
 *
 */
void
ar_card_surface_cache_set_drawable (ArCardSurfaceCache *cache,
                                    /* GdkWindow* */ gpointer drawable)
{
  ArCardSurfaceCachePrivate * priv = cache->priv;

  if (priv->drawable != drawable)
    ar_card_surface_cache_drop (cache);

  priv->drawable = drawable;
}

/**
 * ar_card_surface_cache_drop:
 * @images: a #ArCardImages
 *
 * Clears the image cache.
 */
void
ar_card_surface_cache_drop (ArCardSurfaceCache *cache)
{
  g_return_if_fail (AR_IS_CARD_SURFACE_CACHE (cache));

  ar_card_surface_cache_clear (cache);
}

/**
 * ar_card_surface_cache_set_theme:
 * @cache:
 * @theme:
 *
 * Sets the card theme.
 */
void
ar_card_surface_cache_set_theme (ArCardSurfaceCache *cache,
                                 ArCardTheme *theme)
{
  ArCardSurfaceCachePrivate *priv = cache->priv;

  g_return_if_fail (AR_IS_CARD_SURFACE_CACHE (cache));
  g_return_if_fail (theme == NULL || AR_IS_CARD_THEME (theme));

  if (priv->theme == theme)
    return;

  ar_card_surface_cache_clear (cache);
  ar_card_surface_cache_unset_theme (cache);

  priv->theme = theme;
  if (theme) {
    g_object_ref (theme);

    priv->theme_changed_id = g_signal_connect_swapped (theme, "changed",
                                                       G_CALLBACK (ar_card_surface_cache_clear),
                                                       cache);
  }

  g_object_notify (G_OBJECT (cache), "theme");
}

/**
 * ar_card_surface_cache_get_theme:
 * @cache:
 *
 * Returns: the the card theme of @cache
 */
ArCardTheme *
ar_card_surface_cache_get_theme (ArCardSurfaceCache *cache)
{
  g_return_val_if_fail (AR_IS_CARD_SURFACE_CACHE (cache), NULL);

  return cache->priv->theme;
}

/**
 * ar_card_surface_cache_get_card_surface_by_id:
 * @cache:
 * @card_id:
 *
 * Returns: a cached #cairo_surface_t for @card_id.
 */
cairo_surface_t *
ar_card_surface_cache_get_card_surface_by_id (ArCardSurfaceCache *cache,
                                              guint card_id)
{
  ArCardSurfaceCachePrivate *priv = cache->priv;
  cairo_surface_t *surface;

  g_return_val_if_fail (card_id < AR_CARDS_TOTAL , NULL);

  LOG_CALL (cache);

  surface = priv->cards[card_id];
  if (IS_FAILED_SURFACE (surface)) {
    LOG_CACHE_HIT (cache);
    return NULL;
  }

  if (surface == NULL) {
    CardSize card_size;

    LOG_CACHE_MISS (cache);

    ar_card_theme_get_size (priv->theme, &card_size);

    if (priv->drawable != NULL) {
      surface = gdk_window_create_similar_surface (priv->drawable, CAIRO_CONTENT_COLOR_ALPHA,
                                                   card_size.width, card_size.height);
    } else {
      surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                            card_size.width, card_size.height);
    }

    if (!_ar_card_theme_requires_image_surface (priv->theme)) {
      ar_card_theme_paint_card (priv->theme, surface, card_id);
    } else {
      cairo_surface_t *aux_surface;
      cairo_t *cr;

      /* Use an auxiliary image surface and paint to the similar surface afterwards */
      aux_surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                                card_size.width, card_size.height);
      ar_card_theme_paint_card (priv->theme, aux_surface, card_id);
      cr = cairo_create (surface);
      cairo_set_source_surface (cr, aux_surface, 0, 0);
      cairo_paint (cr);
      cairo_destroy (cr);
      cairo_surface_destroy (aux_surface);
    }

    if (cairo_surface_status (surface) != CAIRO_STATUS_SUCCESS) {
      cairo_surface_destroy (surface);
      priv->cards[card_id] = FAILED_SURFACE;
      return NULL;
    }

    priv->cards[card_id] = surface; /* adopts */
  } else {
    LOG_CACHE_HIT (cache);
  }

  return surface;
}

/**
 * ar_card_surface_cache_get_card_surface:
 * @cache:
 * @card:
 * @highlighted:
 *
 * Returns: a cached #cairo_surface_t for @card.
 */
cairo_surface_t *
ar_card_surface_cache_get_card_surface (ArCardSurfaceCache *cache,
                                            Card card)
{
  guint card_id = _ar_card_to_index (card);

  return ar_card_surface_cache_get_card_surface_by_id (cache, card_id);
}

/**
 * ar_card_surface_cache_get_slot_surface:
 * @cache:
 * @highlighted:
 *
 * Returns: a cached #cairo_surface_t * for the slot.
 */
cairo_surface_t *
ar_card_surface_cache_get_slot_surface (ArCardSurfaceCache *cache)
{
  return ar_card_surface_cache_get_card_surface_by_id (cache, AR_CARD_SLOT);
}
