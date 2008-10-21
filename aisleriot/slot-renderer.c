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

#include <clutter/clutter-actor.h>
#include <gtk/gtk.h>
#include <cogl/cogl.h>

#include "slot-renderer.h"

static void aisleriot_slot_renderer_dispose (GObject *object);
static void aisleriot_slot_renderer_finalize (GObject *object);

static void aisleriot_slot_renderer_set_property (GObject *object,
                                                  guint property_id,
                                                  const GValue *value,
                                                  GParamSpec *pspec);
static void aisleriot_slot_renderer_get_property (GObject *object,
                                                  guint property_id,
                                                  GValue *value,
                                                  GParamSpec *pspec);

static void aisleriot_slot_renderer_paint (ClutterActor *actor);

static void aisleriot_slot_renderer_set_cache (AisleriotSlotRenderer *srend,
                                               AisleriotCardCache *cache);

G_DEFINE_TYPE (AisleriotSlotRenderer, aisleriot_slot_renderer,
               CLUTTER_TYPE_ACTOR);

#define AISLERIOT_SLOT_RENDERER_GET_PRIVATE(obj) \
  (G_TYPE_INSTANCE_GET_PRIVATE ((obj), AISLERIOT_TYPE_SLOT_RENDERER, \
                                AisleriotSlotRendererPrivate))

struct _AisleriotSlotRendererPrivate
{
  AisleriotCardCache *cache;
  Slot *slot;
  gboolean show_highlight;
  guint highlight_start;
};

enum
{
  PROP_0,

  PROP_CACHE,
  PROP_SLOT,
  PROP_HIGHLIGHT
};

static void
aisleriot_slot_renderer_class_init (AisleriotSlotRendererClass *klass)
{
  GObjectClass *gobject_class = (GObjectClass *) klass;
  ClutterActorClass *actor_class = (ClutterActorClass *) klass;
  GParamSpec *pspec;

  gobject_class->dispose = aisleriot_slot_renderer_dispose;
  gobject_class->finalize = aisleriot_slot_renderer_finalize;

  gobject_class->set_property = aisleriot_slot_renderer_set_property;
  gobject_class->get_property = aisleriot_slot_renderer_get_property;

  actor_class->paint = aisleriot_slot_renderer_paint;

  pspec = g_param_spec_object ("cache", NULL, NULL,
                               AISLERIOT_TYPE_CARD_CACHE,
                               G_PARAM_WRITABLE |
                               G_PARAM_CONSTRUCT_ONLY |
                               G_PARAM_STATIC_NAME |
                               G_PARAM_STATIC_NICK |
                               G_PARAM_STATIC_BLURB);
  g_object_class_install_property (gobject_class, PROP_CACHE, pspec);

  pspec = g_param_spec_pointer ("slot", NULL, NULL,
                                G_PARAM_WRITABLE |
                                G_PARAM_CONSTRUCT_ONLY |
                                G_PARAM_STATIC_NAME |
                                G_PARAM_STATIC_NICK |
                                G_PARAM_STATIC_BLURB);
  g_object_class_install_property (gobject_class, PROP_SLOT, pspec);

  pspec = g_param_spec_uint ("highlight", NULL, NULL,
                             0, G_MAXUINT, 0,
                             G_PARAM_WRITABLE |
                             G_PARAM_READABLE |
                             G_PARAM_CONSTRUCT_ONLY |
                             G_PARAM_STATIC_NAME |
                             G_PARAM_STATIC_NICK |
                             G_PARAM_STATIC_BLURB);
  g_object_class_install_property (gobject_class, PROP_HIGHLIGHT, pspec);

  g_type_class_add_private (klass, sizeof (AisleriotSlotRendererPrivate));
}

static void
aisleriot_slot_renderer_init (AisleriotSlotRenderer *self)
{
  AisleriotSlotRendererPrivate *priv;

  priv = self->priv = AISLERIOT_SLOT_RENDERER_GET_PRIVATE (self);

  priv->highlight_start = G_MAXUINT;
}

static void
aisleriot_slot_renderer_dispose (GObject *object)
{
  AisleriotSlotRenderer *self = (AisleriotSlotRenderer *) object;

  aisleriot_slot_renderer_set_cache (self, NULL);

  G_OBJECT_CLASS (aisleriot_slot_renderer_parent_class)->dispose (object);
}

static void
aisleriot_slot_renderer_finalize (GObject *object)
{
  AisleriotSlotRenderer *self = (AisleriotSlotRenderer *) object;

  G_OBJECT_CLASS (aisleriot_slot_renderer_parent_class)->finalize (object);
}

ClutterActor *
aisleriot_slot_renderer_new (AisleriotCardCache *cache, Slot *slot)
{
  ClutterActor *self = g_object_new (AISLERIOT_TYPE_SLOT_RENDERER,
                                     "cache", cache,
                                     "slot", slot,
                                     NULL);

  return self;
}

static void
aisleriot_slot_renderer_set_cache (AisleriotSlotRenderer *srend,
                                   AisleriotCardCache *cache)
{
  AisleriotSlotRendererPrivate *priv = srend->priv;

  if (cache)
    g_object_ref (cache);

  if (priv->cache)
    g_object_unref (priv->cache);

  priv->cache = cache;
}

static void
aisleriot_slot_renderer_set_property (GObject *object,
                                      guint property_id,
                                      const GValue *value,
                                      GParamSpec *pspec)
{
  AisleriotSlotRenderer *srend = AISLERIOT_SLOT_RENDERER (object);
  AisleriotSlotRendererPrivate *priv = srend->priv;

  switch (property_id) {
    case PROP_CACHE:
      aisleriot_slot_renderer_set_cache (srend, g_value_get_object (value));
      break;

    case PROP_SLOT:
      priv->slot = g_value_get_pointer (value);
      break;

    case PROP_HIGHLIGHT:
      aisleriot_slot_renderer_set_highlight (srend,
                                             g_value_get_uint (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

static void
aisleriot_slot_renderer_get_property (GObject *object,
                                      guint property_id,
                                      GValue *value,
                                      GParamSpec *pspec)
{
  AisleriotSlotRenderer *srend = AISLERIOT_SLOT_RENDERER (object);

  switch (property_id) {
    case PROP_HIGHLIGHT:
      g_value_set_uint (value,
                        aisleriot_slot_renderer_get_highlight (srend));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

static void
aisleriot_slot_renderer_paint (ClutterActor *actor)
{
  AisleriotSlotRenderer *srend = (AisleriotSlotRenderer *) actor;
  AisleriotSlotRendererPrivate *priv = srend->priv;
  guint n_cards, first_exposed_card_id, i;
  guint8 *cards;
  int cardx, cardy;

  g_return_if_fail (priv->cache != NULL);
  g_return_if_fail (priv->slot != NULL);

  cards = priv->slot->cards->data;
  n_cards = priv->slot->cards->len;

  g_assert (n_cards >= priv->slot->exposed);
  first_exposed_card_id = n_cards - priv->slot->exposed;

  if (priv->slot->cards->len == 0) {
    CoglHandle cogl_tex;
    guint tex_width, tex_height;

    cogl_tex = aisleriot_card_cache_get_slot_texture (priv->cache,
                                                      priv->show_highlight);
    tex_width = cogl_texture_get_width (cogl_tex);
    tex_height = cogl_texture_get_height (cogl_tex);

    cogl_texture_rectangle (cogl_tex,
                            0, 0,
                            CLUTTER_INT_TO_FIXED (tex_width),
                            CLUTTER_INT_TO_FIXED (tex_height),
                            0, 0, CFX_ONE, CFX_ONE);
  }

  cardx = 0;
  cardy = 0;

  for (i = first_exposed_card_id; i < n_cards; ++i) {
    Card card = CARD (cards[i]);
    gboolean is_highlighted;
    CoglHandle cogl_tex;
    guint tex_width, tex_height;

    is_highlighted = priv->show_highlight && (i >= priv->highlight_start);

    cogl_tex = aisleriot_card_cache_get_card_texture (priv->cache,
                                                      card,
                                                      is_highlighted);

    tex_width = cogl_texture_get_width (cogl_tex);
    tex_height = cogl_texture_get_height (cogl_tex);

    cogl_texture_rectangle (cogl_tex,
                            CLUTTER_INT_TO_FIXED (cardx),
                            CLUTTER_INT_TO_FIXED (cardy),
                            CLUTTER_INT_TO_FIXED (cardx + tex_width),
                            CLUTTER_INT_TO_FIXED (cardy + tex_height),
                            0, 0, CFX_ONE, CFX_ONE);

    cardx += priv->slot->pixeldx;
    cardy += priv->slot->pixeldy;
  }
}

guint
aisleriot_slot_renderer_get_highlight (AisleriotSlotRenderer *srend)
{
  g_return_val_if_fail (AISLERIOT_IS_SLOT_RENDERER (srend), 0);

  return srend->priv->highlight_start;
}

void
aisleriot_slot_renderer_set_highlight (AisleriotSlotRenderer *srend,
                                       guint highlight)
{
  AisleriotSlotRendererPrivate *priv = srend->priv;

  g_return_if_fail (AISLERIOT_IS_SLOT_RENDERER (srend));

  priv->highlight_start = highlight;
  priv->show_highlight = priv->highlight_start != G_MAXUINT;

  clutter_actor_queue_redraw (CLUTTER_ACTOR (srend));
}
