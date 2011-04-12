/*
 * Copyright © 2008 Neil Roberts
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <clutter/clutter.h>
#include <gtk/gtk.h>
#include <cogl/cogl.h>

#include "card.h"

static void aisleriot_card_paint (ClutterActor *actor);

static void aisleriot_card_get_preferred_width (ClutterActor *self,
                                                gfloat        for_height,
                                                gfloat       *min_width_p,
                                                gfloat       *natural_width_p);
static void aisleriot_card_get_preferred_height
                                               (ClutterActor *self,
                                                gfloat        for_width,
                                                gfloat       *min_height_p,
                                                gfloat       *natural_height_p);

static void aisleriot_card_dispose (GObject *self);

static void aisleriot_card_set_property (GObject      *self,
                                         guint         property_id,
                                         const GValue *value,
                                         GParamSpec   *pspec);
static void aisleriot_card_get_property (GObject    *self,
                                         guint       property_id,
                                         GValue     *value,
                                         GParamSpec *pspec);

static void aisleriot_card_unref_cache (AisleriotCard *card);

static void aisleriot_card_set_cache (AisleriotCard *card,
                                      ArCardTexturesCache *cache);

#define ANGLE_IS_UPSIDE_DOWN(angle)             \
  ((ABS (angle) + CLUTTER_INT_TO_FIXED (90))    \
   % CLUTTER_INT_TO_FIXED (360)                 \
   >= CLUTTER_INT_TO_FIXED (180))

G_DEFINE_TYPE (AisleriotCard, aisleriot_card, CLUTTER_TYPE_ACTOR);

#define AISLERIOT_CARD_GET_PRIVATE(obj) \
  (G_TYPE_INSTANCE_GET_PRIVATE ((obj), AISLERIOT_TYPE_CARD, \
                                AisleriotCardPrivate))

struct _AisleriotCardPrivate
{
  Card bottom_card, top_card;

  ArCardTexturesCache *cache;
};

enum
{
  PROP_0,

  PROP_CACHE,
  PROP_BOTTOM_CARD,
  PROP_TOP_CARD
};

static void
aisleriot_card_class_init (AisleriotCardClass *klass)
{
  GObjectClass *gobject_class = (GObjectClass *) klass;
  ClutterActorClass *actor_class = (ClutterActorClass *) klass;
  GParamSpec *pspec;

  gobject_class->dispose = aisleriot_card_dispose;
  gobject_class->set_property = aisleriot_card_set_property;
  gobject_class->get_property = aisleriot_card_get_property;

  actor_class->paint = aisleriot_card_paint;
  actor_class->get_preferred_width = aisleriot_card_get_preferred_width;
  actor_class->get_preferred_height = aisleriot_card_get_preferred_height;

  pspec = g_param_spec_uchar ("bottom-card", NULL, NULL,
                              0, 255, 0,
                              G_PARAM_READWRITE |
                              G_PARAM_STATIC_STRINGS);
  g_object_class_install_property (gobject_class, PROP_BOTTOM_CARD, pspec);

  pspec = g_param_spec_uchar ("top-card", NULL, NULL,
                              0, 255, 0,
                              G_PARAM_READWRITE |
                              G_PARAM_STATIC_STRINGS);
  g_object_class_install_property (gobject_class, PROP_TOP_CARD, pspec);

  pspec = g_param_spec_object ("cache", NULL, NULL,
                               AR_TYPE_CARD_TEXTURES_CACHE,
                               G_PARAM_READWRITE |
                               G_PARAM_CONSTRUCT_ONLY |
                               G_PARAM_STATIC_STRINGS);
  g_object_class_install_property (gobject_class, PROP_CACHE, pspec);

  g_type_class_add_private (klass, sizeof (AisleriotCardPrivate));
}

static void
aisleriot_card_init (AisleriotCard *self)
{
  AisleriotCardPrivate *priv;

  priv = self->priv = AISLERIOT_CARD_GET_PRIVATE (self);
}

static void
aisleriot_card_dispose (GObject *self)
{
  AisleriotCard *card = AISLERIOT_CARD (self);

  aisleriot_card_unref_cache (card);

  G_OBJECT_CLASS (aisleriot_card_parent_class)->dispose (self);
}

ClutterActor *
aisleriot_card_new (ArCardTexturesCache *cache,
                    Card bottom_card,
                    Card top_card)
{
  return g_object_new (AISLERIOT_TYPE_CARD,
                       "cache", cache,
                       "bottom-card", bottom_card.value,
                       "top-card", top_card.value,
                       NULL);
}

static void
aisleriot_card_get_preferred_width (ClutterActor *self,
                                    gfloat        for_height,
                                    gfloat       *min_width_p,
                                    gfloat       *natural_width_p)
{
  AisleriotCard *card = AISLERIOT_CARD (self);
  AisleriotCardPrivate *priv = card->priv;
  CoglHandle tex;
  guint width;

  tex = ar_card_textures_cache_get_card_texture (priv->cache,
                                                    priv->top_card);

  if (G_UNLIKELY (tex == COGL_INVALID_HANDLE))
    width = 0;
  else
    width = cogl_texture_get_width (tex);

  if (min_width_p)
    *min_width_p = 0;

  if (natural_width_p)
    *natural_width_p = width;
}

static void
aisleriot_card_get_preferred_height (ClutterActor *self,
                                     gfloat        for_width,
                                     gfloat       *min_height_p,
                                     gfloat       *natural_height_p)
{
  AisleriotCard *card = AISLERIOT_CARD (self);
  AisleriotCardPrivate *priv = card->priv;
  CoglHandle tex;
  guint height;

  tex = ar_card_textures_cache_get_card_texture (priv->cache,
                                                    priv->top_card);

  if (G_UNLIKELY (tex == COGL_INVALID_HANDLE))
    height = 0;
  else
    height = cogl_texture_get_height (tex);

  if (min_height_p)
    *min_height_p = 0;

  if (natural_height_p)
    *natural_height_p = height;
}

static void
aisleriot_card_paint (ClutterActor *actor)
{
  AisleriotCard *card = (AisleriotCard *) actor;
  AisleriotCardPrivate *priv = card->priv;
  CoglHandle top_tex, bottom_tex;
  ClutterActorBox alloc_box;
  gboolean backface_culling_was_enabled = cogl_get_backface_culling_enabled ();

  cogl_set_backface_culling_enabled (TRUE);

  top_tex = ar_card_textures_cache_get_card_texture (priv->cache,
                                                        priv->top_card);
  bottom_tex = ar_card_textures_cache_get_card_texture (priv->cache,
                                                           priv->bottom_card);
  if (G_UNLIKELY (top_tex == COGL_INVALID_HANDLE
                  || bottom_tex == COGL_INVALID_HANDLE))
    return;

  clutter_actor_get_allocation_box (actor, &alloc_box);

  /* Draw both sides of the card. Backface culling is enabled so only
     one side will actually be rendered */

  cogl_set_source_texture (top_tex);

  cogl_rectangle (0.0f, 0.0f,
                  alloc_box.x2 - alloc_box.x1,
                  alloc_box.y2 - alloc_box.y1);

  cogl_set_source_texture (bottom_tex);

  /* Rotate along the y-axis about the center of the card to make the
     bottom of the card face the other way */
  cogl_push_matrix ();
  cogl_translate ((alloc_box.x2 - alloc_box.x1) / 2,
                  0, 0);
  cogl_rotate (180, 0, 1, 0);
  cogl_translate (-(alloc_box.x2 - alloc_box.x1) / 2,
                  0, 0);
  cogl_rectangle (0.0f, 0.0f,
                  alloc_box.x2 - alloc_box.x1,
                  alloc_box.y2 - alloc_box.y1);
  cogl_pop_matrix ();

  if (!backface_culling_was_enabled)
    cogl_set_backface_culling_enabled (FALSE);
}

static void
aisleriot_card_set_property (GObject *self,
                             guint property_id,
                             const GValue *value,
                             GParamSpec *pspec)
{
  AisleriotCard *card = AISLERIOT_CARD (self);

  switch (property_id) {
    case PROP_BOTTOM_CARD:
      {
        Card card_num;

        card_num.value = g_value_get_uchar (value);

        aisleriot_card_set_card (card, card_num, card->priv->top_card);
      }
      break;

    case PROP_TOP_CARD:
      {
        Card card_num;

        card_num.value = g_value_get_uchar (value);

        aisleriot_card_set_card (card, card->priv->bottom_card, card_num);
      }
      break;

    case PROP_CACHE:
      aisleriot_card_set_cache (card, g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (self, property_id, pspec);
      break;
  }
}

static void
aisleriot_card_get_property (GObject *self,
                             guint property_id,
                             GValue *value,
                             GParamSpec *pspec)
{
  AisleriotCard *card = AISLERIOT_CARD (self);
  AisleriotCardPrivate *priv = card->priv;

  switch (property_id) {
    case PROP_BOTTOM_CARD:
      g_value_set_uchar (value, priv->bottom_card.value);
      break;

    case PROP_TOP_CARD:
      g_value_set_uchar (value, priv->top_card.value);
      break;

    case PROP_CACHE:
      g_value_set_object (value, priv->cache);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (self, property_id, pspec);
      break;
  }
}

void
aisleriot_card_set_card (AisleriotCard *card,
                         Card bottom_card,
                         Card top_card)
{
  AisleriotCardPrivate *priv;

  g_return_if_fail (AISLERIOT_IS_CARD (card));

  priv = card->priv;

  priv->bottom_card = bottom_card;
  priv->top_card = top_card;

  clutter_actor_queue_redraw (CLUTTER_ACTOR (card));

  g_object_notify (G_OBJECT (card), "bottom-card");
  g_object_notify (G_OBJECT (card), "top-card");
}

static void
aisleriot_card_unref_cache (AisleriotCard *card)
{
  AisleriotCardPrivate *priv = card->priv;

  if (priv->cache) {
    g_object_unref (priv->cache);
    priv->cache = NULL;
  }
}

static void
aisleriot_card_set_cache (AisleriotCard *card, ArCardTexturesCache *cache)
{
  AisleriotCardPrivate *priv = card->priv;

  if (cache)
    g_object_ref (cache);

  aisleriot_card_unref_cache (card);

  priv->cache = cache;

  clutter_actor_queue_redraw (CLUTTER_ACTOR (card));

  g_object_notify (G_OBJECT (card), "cache");
}
