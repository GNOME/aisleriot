/*
  Copyright © 2008 Neil Roberts
  Copyright © 2008 Christian Persch

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

#include <glib-object.h>
#include <gtk/gtk.h>
#include <cogl/cogl.h>

#include "games-card-textures-cache.h"

#include "games-card-private.h"
#include "games-debug.h"

struct _GamesCardTexturesCachePrivate
{
  GamesCardTheme *theme;
  guint theme_changed_handler;

  CoglHandle *cards;

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

/* This is an invalid value for a CoglHandle, and distinct from COGL_INVALID_HANDLE */
#define FAILED_HANDLE ((gpointer) 0x1)
#define IS_FAILED_HANDLE(ptr) (G_UNLIKELY ((ptr) == FAILED_HANDLE))

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

static void games_card_textures_cache_dispose (GObject *object);
static void games_card_textures_cache_finalize (GObject *object);

G_DEFINE_TYPE (GamesCardTexturesCache, games_card_textures_cache, G_TYPE_OBJECT);

#define GAMES_CARD_TEXTURES_CACHE_GET_PRIVATE(obj) (G_TYPE_INSTANCE_GET_PRIVATE ((obj), GAMES_TYPE_CARD_TEXTURES_CACHE, GamesCardTexturesCachePrivate))

/* Helper functions */

static void
games_card_textures_cache_clear (GamesCardTexturesCache *cache)
{
  GamesCardTexturesCachePrivate *priv = cache->priv;
  int i;

  _games_debug_print (GAMES_DEBUG_CARD_CACHE,
                      "games_card_textures_cache_clear\n");

  for (i = 0; i < GAMES_CARDS_TOTAL; i++) {
    CoglHandle handle = priv->cards[i];

    if (handle != COGL_INVALID_HANDLE &&
        !IS_FAILED_HANDLE (handle)) {
      cogl_texture_unref (handle);
    }

    priv->cards[i] = COGL_INVALID_HANDLE;
  }
}

static void
games_card_textures_cache_unset_theme (GamesCardTexturesCache *cache)
{
  GamesCardTexturesCachePrivate *priv = cache->priv;

  if (priv->theme) {
    g_signal_handler_disconnect (priv->theme, priv->theme_changed_handler);
    g_object_unref (priv->theme);
    priv->theme = NULL;
    priv->theme_changed_handler = 0;
  }
}

/* Class implementation */

static void
games_card_textures_cache_init (GamesCardTexturesCache *self)
{
  GamesCardTexturesCachePrivate *priv;

  priv = self->priv = GAMES_CARD_TEXTURES_CACHE_GET_PRIVATE (self);

  priv->cards = g_malloc0 (sizeof (CoglHandle) * GAMES_CARDS_TOTAL);
}

static void
games_card_textures_cache_dispose (GObject *object)
{
  GamesCardTexturesCache *cache = GAMES_CARD_TEXTURES_CACHE (object);

  games_card_textures_cache_clear (cache);
  games_card_textures_cache_unset_theme (cache);

  G_OBJECT_CLASS (games_card_textures_cache_parent_class)->dispose (object);
}

static void
games_card_textures_cache_finalize (GObject *object)
{
  GamesCardTexturesCache *cache = GAMES_CARD_TEXTURES_CACHE (object);
  GamesCardTexturesCachePrivate *priv = cache->priv;

  g_free (priv->cards);

#ifdef GNOME_ENABLE_DEBUG
  _GAMES_DEBUG_IF (GAMES_DEBUG_CARD_CACHE) {
    _games_debug_print (GAMES_DEBUG_CARD_CACHE,
                        "GamesCardTexturesCache %p statistics: %u calls with %u hits and %u misses for a hit/total of %.3f\n",
                        cache, priv->n_calls, priv->cache_hits, priv->n_calls - priv->cache_hits,
                        priv->n_calls > 0 ? (double) priv->cache_hits / (double) priv->n_calls : 0.0);
  }
#endif

  G_OBJECT_CLASS (games_card_textures_cache_parent_class)->finalize (object);
}

static void
games_card_textures_cache_set_property (GObject *self,
                                        guint property_id,
                                        const GValue *value,
                                        GParamSpec *pspec)
{
  GamesCardTexturesCache *cache = GAMES_CARD_TEXTURES_CACHE (self);

  switch (property_id) {
    case PROP_THEME:
      games_card_textures_cache_set_theme (cache, g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (self, property_id, pspec);
      break;
    }
}

static void
games_card_textures_cache_get_property (GObject *self,
                                        guint property_id,
                                        GValue *value,
                                        GParamSpec *pspec)
{
  GamesCardTexturesCache *cache = GAMES_CARD_TEXTURES_CACHE (self);

  switch (property_id) {
    case PROP_THEME:
      g_value_set_object (value, games_card_textures_cache_get_theme (cache));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (self, property_id, pspec);
      break;
    }
}

static void
games_card_textures_cache_class_init (GamesCardTexturesCacheClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GParamSpec *pspec;

  gobject_class->dispose = games_card_textures_cache_dispose;
  gobject_class->finalize = games_card_textures_cache_finalize;
  gobject_class->set_property = games_card_textures_cache_set_property;
  gobject_class->get_property = games_card_textures_cache_get_property;

  g_type_class_add_private (klass, sizeof (GamesCardTexturesCachePrivate));

  pspec = g_param_spec_object ("theme", NULL, NULL,
                               GAMES_TYPE_CARD_THEME,
                               G_PARAM_WRITABLE |
                               G_PARAM_CONSTRUCT_ONLY |
                               G_PARAM_STATIC_NAME |
                               G_PARAM_STATIC_NICK |
                               G_PARAM_STATIC_BLURB);
  g_object_class_install_property (gobject_class, PROP_THEME, pspec);
}

/* Public API */

/**
 * games_card_textures_cache_new:
 *
 * Returns: a new #GamesCardTexturesCache object
 */
GamesCardTexturesCache *
games_card_textures_cache_new (void)
{
  return g_object_new (GAMES_TYPE_CARD_TEXTURES_CACHE, NULL);
}

/**
 * games_card_textures_cache_drop:
 * @images: a #GamesCardImages
 *
 * Clears the image cache.
 */
void
games_card_textures_cache_drop (GamesCardTexturesCache *cache)
{
  g_return_if_fail (GAMES_IS_CARD_TEXTURES_CACHE (cache));

  games_card_textures_cache_clear (cache);
}

/**
 * games_card_textures_cache_set_theme:
 * @cache:
 * @theme:
 *
 * Sets the card theme.
 */
void
games_card_textures_cache_set_theme (GamesCardTexturesCache *cache,
                                     GamesCardTheme *theme)
{
  GamesCardTexturesCachePrivate *priv = cache->priv;

  g_return_if_fail (GAMES_IS_CARD_TEXTURES_CACHE (cache));
  g_return_if_fail (theme == NULL || GAMES_IS_CARD_THEME (theme));

  if (priv->theme == theme)
    return;

  games_card_textures_cache_clear (cache);
  games_card_textures_cache_unset_theme (cache);

  priv->theme = theme;
  if (theme) {
    g_object_ref (theme);

    priv->theme_changed_handler = g_signal_connect_swapped (theme, "changed",
                                                            G_CALLBACK (games_card_textures_cache_clear),
                                                            cache);
  }

  g_object_notify (G_OBJECT (cache), "theme");
}

/**
 * games_card_textures_cache_get_theme:
 * @cache:
 *
 * Returns: the the card theme of @cache
 */
GamesCardTheme *
games_card_textures_cache_get_theme (GamesCardTexturesCache *cache)
{
  g_return_val_if_fail (GAMES_IS_CARD_TEXTURES_CACHE (cache), NULL);

  return cache->priv->theme;
}

/**
 * games_card_textures_cache_get_card_texture_by_id:
 * @cache:
 * @card_id:
 *
 * Returns: a cached #CoglHandle for @card_id.
 */
CoglHandle
games_card_textures_cache_get_card_texture_by_id (GamesCardTexturesCache *cache,
                                                  guint card_id)
{
  GamesCardTexturesCachePrivate *priv = cache->priv;
  CoglHandle handle;

  g_return_val_if_fail (card_id < GAMES_CARDS_TOTAL , NULL);

  LOG_CALL (cache);

  handle = priv->cards[card_id];
  if (IS_FAILED_HANDLE (handle)) {
    LOG_CACHE_HIT (cache);
    return COGL_INVALID_HANDLE;
  }

  if (handle == COGL_INVALID_HANDLE) {
    GdkPixbuf *pixbuf;

    LOG_CACHE_MISS (cache);

    pixbuf = games_card_theme_get_card_pixbuf (priv->theme, card_id);
    if (!pixbuf) {
      priv->cards[card_id] = FAILED_HANDLE;
      return COGL_INVALID_HANDLE;
    }

    handle = cogl_texture_new_from_data (gdk_pixbuf_get_width (pixbuf),
                                         gdk_pixbuf_get_height (pixbuf),
                                         COGL_TEXTURE_NONE,
                                         gdk_pixbuf_get_has_alpha (pixbuf)
                                         ? COGL_PIXEL_FORMAT_RGBA_8888
                                         : COGL_PIXEL_FORMAT_RGB_888,
                                         COGL_PIXEL_FORMAT_ANY,
                                         gdk_pixbuf_get_rowstride (pixbuf),
                                         gdk_pixbuf_get_pixels (pixbuf));
    g_object_unref (pixbuf);

    if (handle == COGL_INVALID_HANDLE) {
      priv->cards[card_id] = FAILED_HANDLE;
      return COGL_INVALID_HANDLE;
    }

    priv->cards[card_id] = handle;
  } else {
    LOG_CACHE_HIT (cache);
  }

  return handle;
}

/**
 * games_card_textures_cache_get_card_texture:
 * @cache:
 * @card:
 * @highlighted:
 *
 * Returns: a cached #CoglHandle for @card.
 */
CoglHandle
games_card_textures_cache_get_card_texture (GamesCardTexturesCache *cache,
                                            Card card)
{
  guint card_id = _games_card_to_index (card);

  return games_card_textures_cache_get_card_texture_by_id (cache, card_id);
}

/**
 * games_card_textures_cache_get_slot_texture:
 * @cache:
 * @highlighted:
 *
 * Returns: a cached #CoglHandle for the slot.
 */
CoglHandle
games_card_textures_cache_get_slot_texture (GamesCardTexturesCache *cache)
{
  return games_card_textures_cache_get_card_texture_by_id (cache, GAMES_CARD_SLOT);
}
