/*
 *  Copyright © 2008 Neil Roberts
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include <config.h>

#include <glib-object.h>
#include <gtk/gtk.h>
#include <cogl/cogl.h>

#include "games-card-textures-cache.h"

struct _GamesCardTexturesCachePrivate
{
  CoglHandle *cards;
  GamesCardImages *card_images;

  guint theme_handler;
};

enum
{
  PROP_0,
  PROP_CARD_IMAGES
};

static void games_card_textures_cache_dispose (GObject *object);
static void games_card_textures_cache_finalize (GObject *object);

G_DEFINE_TYPE (GamesCardTexturesCache, games_card_textures_cache, G_TYPE_OBJECT);

#define GAMES_CARD_TEXTURES_CACHE_GET_PRIVATE(obj) (G_TYPE_INSTANCE_GET_PRIVATE ((obj), GAMES_TYPE_CARD_TEXTURES_CACHE, GamesCardTexturesCachePrivate))

/* Helper functions */

static void
games_card_textures_cache_unref_images (GamesCardTexturesCache *cache)
{
  GamesCardTexturesCachePrivate *priv = cache->priv;

  if (priv->card_images) {
    g_signal_handler_disconnect (priv->card_images, priv->theme_handler);
    g_object_unref (priv->card_images);
    priv->card_images = NULL;
  }
}

static void
games_card_textures_cache_set_card_images (GamesCardTexturesCache *cache,
                                      GamesCardImages *card_images)
{
  GamesCardTexturesCachePrivate *priv = cache->priv;

  if (card_images)
    g_object_ref (card_images);

  games_card_textures_cache_unref_images (cache);

  priv->card_images = card_images;

  if (card_images)
    priv->theme_handler
      = g_signal_connect_swapped (card_images, "notify::theme",
                                  G_CALLBACK (games_card_textures_cache_clear),
                                  NULL);

  games_card_textures_cache_clear (cache);

  g_object_notify (G_OBJECT (cache), "card-images");
}

/* Class implementation */


static void
games_card_textures_cache_init (GamesCardTexturesCache *self)
{
  GamesCardTexturesCachePrivate *priv;

  priv = self->priv = GAMES_CARD_TEXTURES_CACHE_GET_PRIVATE (self);

  priv->cards = g_malloc0 (sizeof (ClutterActor *) * GAMES_CARDS_TOTAL * 2);
}

static void
games_card_textures_cache_dispose (GObject *object)
{
  GamesCardTexturesCache *self = (GamesCardTexturesCache *) object;

  games_card_textures_cache_clear (self);

  games_card_textures_cache_unref_images (self);

  G_OBJECT_CLASS (games_card_textures_cache_parent_class)->dispose (object);
}

static void
games_card_textures_cache_finalize (GObject *object)
{
  GamesCardTexturesCache *self = (GamesCardTexturesCache *) object;
  GamesCardTexturesCachePrivate *priv = self->priv;

  g_free (priv->cards);

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
    case PROP_CARD_IMAGES:
      games_card_textures_cache_set_card_images (cache, g_value_get_object (value));
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
  GamesCardTexturesCachePrivate *priv = cache->priv;

  switch (property_id) {
    case PROP_CARD_IMAGES:
      g_value_set_object (value, priv->card_images);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (self, property_id, pspec);
      break;
    }
}

static void
games_card_textures_cache_class_init (GamesCardTexturesCacheClass *klass)
{
  GObjectClass *gobject_class = (GObjectClass *) klass;
  GParamSpec *pspec;

  gobject_class->dispose = games_card_textures_cache_dispose;
  gobject_class->finalize = games_card_textures_cache_finalize;
  gobject_class->set_property = games_card_textures_cache_set_property;
  gobject_class->get_property = games_card_textures_cache_get_property;

  g_type_class_add_private (klass, sizeof (GamesCardTexturesCachePrivate));

  pspec = g_param_spec_object ("card-images", NULL, NULL,
                               GAMES_TYPE_CARD_IMAGES,
                               G_PARAM_WRITABLE |
                               G_PARAM_CONSTRUCT_ONLY |
                               G_PARAM_STATIC_NAME |
                               G_PARAM_STATIC_NICK |
                               G_PARAM_STATIC_BLURB);
  g_object_class_install_property (gobject_class, PROP_CARD_IMAGES, pspec);
}

/* Public API */

/**
 * games_card_textures_cache_new:
 *
 * Returns: a new #GamesCardTexturesCache object
 */
GamesCardTexturesCache *
games_card_textures_cache_new (GamesCardImages *images)
{
  return g_object_new (GAMES_TYPE_CARD_TEXTURES_CACHE,
                       "card-images", images,
                       NULL);
}

/* FIXME: make this private and automatic */
void
games_card_textures_cache_clear (GamesCardTexturesCache *cache)
{
  GamesCardTexturesCachePrivate *priv = cache->priv;
  int i;

  for (i = 0; i < GAMES_CARDS_TOTAL * 2; i++)
    if (priv->cards[i] != COGL_INVALID_HANDLE) {
      cogl_texture_unref (priv->cards[i]);
      priv->cards[i] = COGL_INVALID_HANDLE;
    }
}

/**
 * games_card_textures_cache_get_card_texture_by_id:
 * @cache:
 * @card_id:
 * @highlighted:
 *
 * Returns: a cached #CoglHandle for @card_id.
 */
CoglHandle
games_card_textures_cache_get_card_texture_by_id (GamesCardTexturesCache *cache,
                                                  guint card_id,
                                                  gboolean highlighted)
{
  GamesCardTexturesCachePrivate *priv;
  guint index;

  g_return_val_if_fail (GAMES_IS_CARD_TEXTURES_CACHE (cache), NULL);
  g_return_val_if_fail (card_id < GAMES_CARDS_TOTAL , NULL);

  priv = cache->priv;

  g_return_val_if_fail (priv->card_images != NULL, NULL);

  index = card_id;
  if (highlighted)
    index += GAMES_CARDS_TOTAL;

  if (priv->cards[index] == COGL_INVALID_HANDLE) {
    CoglHandle tex;
    GdkPixbuf *pixbuf
      = games_card_images_get_card_pixbuf_by_id (priv->card_images,
                                                 card_id,
                                                 highlighted);

    if (!pixbuf)
      return COGL_INVALID_HANDLE;

    tex = cogl_texture_new_from_data (gdk_pixbuf_get_width (pixbuf),
                                      gdk_pixbuf_get_height (pixbuf),
                                      64, FALSE,
                                      gdk_pixbuf_get_has_alpha (pixbuf)
                                      ? COGL_PIXEL_FORMAT_RGBA_8888
                                      : COGL_PIXEL_FORMAT_RGB_888,
                                      COGL_PIXEL_FORMAT_ANY,
                                      gdk_pixbuf_get_rowstride (pixbuf),
                                      gdk_pixbuf_get_pixels (pixbuf));

    priv->cards[index] = tex;
  }

  return priv->cards[index];
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
                                            Card card,
                                            gboolean highlighted)
{
  guint card_id = games_card_images_card_to_index (card);

  return games_card_textures_cache_get_card_texture_by_id (cache, card_id,
                                                           highlighted);
}

/**
 * games_card_textures_cache_get_slot_texture:
 * @cache:
 * @highlighted:
 *
 * Returns: a cached #CoglHandle for the slot.
 */
CoglHandle
games_card_textures_cache_get_slot_texture (GamesCardTexturesCache *cache,
                                            gboolean highlighted)
{
  return games_card_textures_cache_get_card_texture_by_id (cache,
                                                           GAMES_CARD_SLOT,
                                                           highlighted);
}
