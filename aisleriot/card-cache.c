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
#include <clutter/clutter-texture.h>
#include <clutter-gtk/gtk-clutter-util.h>

#include <libgames-support/games-card-images.h>

#include "card-cache.h"

static void aisleriot_card_cache_dispose (GObject *object);
static void aisleriot_card_cache_finalize (GObject *object);

static void aisleriot_card_cache_clear (AisleriotCardCache *cache);

static void aisleriot_card_cache_set_property (GObject      *self,
                                               guint         property_id,
                                               const GValue *value,
                                               GParamSpec   *pspec);
static void aisleriot_card_cache_get_property (GObject    *self,
                                               guint       property_id,
                                               GValue     *value,
                                               GParamSpec *pspec);

static void aisleriot_card_cache_unref_images (AisleriotCardCache *cache);

G_DEFINE_TYPE (AisleriotCardCache, aisleriot_card_cache, G_TYPE_OBJECT);

#define AISLERIOT_CARD_CACHE_GET_PRIVATE(obj) \
  (G_TYPE_INSTANCE_GET_PRIVATE ((obj), AISLERIOT_TYPE_CARD_CACHE, \
                                AisleriotCardCachePrivate))

struct _AisleriotCardCachePrivate
{
  ClutterActor **cards;
  GamesCardImages *card_images;

  guint theme_handler;
};

enum
{
  PROP_0,

  PROP_CARD_IMAGES
};

static void
aisleriot_card_cache_class_init (AisleriotCardCacheClass *klass)
{
  GObjectClass *gobject_class = (GObjectClass *) klass;
  GParamSpec *pspec;

  gobject_class->dispose = aisleriot_card_cache_dispose;
  gobject_class->finalize = aisleriot_card_cache_finalize;
  gobject_class->set_property = aisleriot_card_cache_set_property;
  gobject_class->get_property = aisleriot_card_cache_get_property;

  g_type_class_add_private (klass, sizeof (AisleriotCardCachePrivate));

  pspec = g_param_spec_object ("card-images", NULL, NULL,
                               GAMES_TYPE_CARD_IMAGES,
                               G_PARAM_WRITABLE |
                               G_PARAM_CONSTRUCT_ONLY |
                               G_PARAM_STATIC_NAME |
                               G_PARAM_STATIC_NICK |
                               G_PARAM_STATIC_BLURB);
  g_object_class_install_property (gobject_class, PROP_CARD_IMAGES, pspec);
}

static void
aisleriot_card_cache_init (AisleriotCardCache *self)
{
  AisleriotCardCachePrivate *priv;

  priv = self->priv = AISLERIOT_CARD_CACHE_GET_PRIVATE (self);

  priv->cards = g_malloc0 (sizeof (ClutterActor *) * GAMES_CARDS_TOTAL * 2);
}

static void
aisleriot_card_cache_dispose (GObject *object)
{
  AisleriotCardCache *self = (AisleriotCardCache *) object;

  aisleriot_card_cache_clear (self);

  aisleriot_card_cache_unref_images (self);

  G_OBJECT_CLASS (aisleriot_card_cache_parent_class)->dispose (object);
}

static void
aisleriot_card_cache_finalize (GObject *object)
{
  AisleriotCardCache *self = (AisleriotCardCache *) object;
  AisleriotCardCachePrivate *priv = self->priv;

  g_free (priv->cards);

  G_OBJECT_CLASS (aisleriot_card_cache_parent_class)->finalize (object);
}

static void
aisleriot_card_cache_set_card_images (AisleriotCardCache *cache,
                                      GamesCardImages *card_images)
{
  AisleriotCardCachePrivate *priv = cache->priv;

  if (card_images)
    g_object_ref (card_images);

  aisleriot_card_cache_unref_images (cache);

  priv->card_images = card_images;

  if (card_images)
    priv->theme_handler
      = g_signal_connect_swapped (card_images, "notify::theme",
                                  G_CALLBACK (aisleriot_card_cache_clear),
                                  NULL);

  aisleriot_card_cache_clear (cache);

  g_object_notify (G_OBJECT (cache), "card-images");
}

static void
aisleriot_card_cache_set_property (GObject *self,
                                   guint property_id,
                                   const GValue *value,
                                   GParamSpec *pspec)
{
  AisleriotCardCache *cache = AISLERIOT_CARD_CACHE (self);

  switch (property_id) {
    case PROP_CARD_IMAGES:
      aisleriot_card_cache_set_card_images (cache, g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (self, property_id, pspec);
      break;
    }
}

static void
aisleriot_card_cache_get_property (GObject *self,
                                   guint property_id,
                                   GValue *value,
                                   GParamSpec *pspec)
{
  AisleriotCardCache *cache = AISLERIOT_CARD_CACHE (self);
  AisleriotCardCachePrivate *priv = cache->priv;

  switch (property_id) {
    case PROP_CARD_IMAGES:
      g_value_set_object (value, priv->card_images);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (self, property_id, pspec);
      break;
    }
}

AisleriotCardCache *
aisleriot_card_cache_new (GamesCardImages *images)
{
  AisleriotCardCache *self = g_object_new (AISLERIOT_TYPE_CARD_CACHE,
                                           "card-images", images, NULL);

  return self;
}

static void
aisleriot_card_cache_clear (AisleriotCardCache *cache)
{
  AisleriotCardCachePrivate *priv = cache->priv;
  int i;

  for (i = 0; i < GAMES_CARDS_TOTAL; i++)
    if (priv->cards[i]) {
      g_object_unref (priv->cards[i]);
      priv->cards[i] = NULL;
    }
}

ClutterActor *
aisleriot_card_cache_get_card_texture_by_id (AisleriotCardCache *cache,
                                             guint card_id,
                                             gboolean highlighted)
{
  AisleriotCardCachePrivate *priv;
  guint index;

  g_return_val_if_fail (AISLERIOT_IS_CARD_CACHE (cache), NULL);
  g_return_val_if_fail (card_id < GAMES_CARDS_TOTAL , NULL);

  priv = cache->priv;

  g_return_val_if_fail (priv->card_images != NULL, NULL);

  index = card_id;
  if (highlighted)
    index *= 2;

  if (priv->cards[index] == NULL)
    {
      ClutterActor *tex = g_object_ref_sink (clutter_texture_new ());
      GdkPixbuf *pixbuf
        = games_card_images_get_card_pixbuf_by_id (priv->card_images,
                                                   card_id,
                                                   highlighted);

      gtk_clutter_texture_set_from_pixbuf (CLUTTER_TEXTURE (tex), pixbuf);

      priv->cards[index] = tex;
    }

  return priv->cards[index];
}

ClutterActor *
aisleriot_card_cache_get_card_texture (AisleriotCardCache *cache,
                                       Card card,
                                       gboolean highlighted)
{
  guint card_id = games_card_images_card_to_index (card);

  return aisleriot_card_cache_get_card_texture_by_id (cache, card_id,
                                                      highlighted);
}

static void
aisleriot_card_cache_unref_images (AisleriotCardCache *cache)
{
  AisleriotCardCachePrivate *priv = cache->priv;

  if (priv->card_images) {
    g_signal_handler_disconnect (priv->card_images, priv->theme_handler);
    g_object_unref (priv->card_images);
    priv->card_images = NULL;
  }
}
