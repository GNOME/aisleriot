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

#include <clutter/clutter.h>
#include <gtk/gtk.h>
#include <cogl/cogl.h>

#include "card.h"

static void aisleriot_card_paint (ClutterActor *actor);

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
                                      GamesCardTexturesCache *cache);

#define ANGLE_IS_UPSIDE_DOWN(angle)             \
  ((ABS (angle) + CLUTTER_INT_TO_FIXED (90))    \
   % CLUTTER_INT_TO_FIXED (360)                 \
   >= CLUTTER_INT_TO_FIXED (180))

static const ClutterColor default_highlight_color = { 0, 0, 0xaa, 0xff };

G_DEFINE_TYPE (AisleriotCard, aisleriot_card, CLUTTER_TYPE_ACTOR);

#define AISLERIOT_CARD_GET_PRIVATE(obj) \
  (G_TYPE_INSTANCE_GET_PRIVATE ((obj), AISLERIOT_TYPE_CARD, \
                                AisleriotCardPrivate))

struct _AisleriotCardPrivate
{
  Card card;
  ClutterColor highlight_color;
  gboolean highlighted;

  GamesCardTexturesCache *cache;
};

enum
{
  PROP_0,

  PROP_CACHE,
  PROP_CARD,
  PROP_HIGHLIGHTED,
  PROP_HIGHLIGHT_COLOR
};

static void
aisleriot_card_set_highlight_color (AisleriotCard *card,
                                    const ClutterColor *color)
{
  AisleriotCardPrivate *priv = card->priv;

  g_return_if_fail (color != NULL);

  if (clutter_color_equal (color, &priv->highlight_color))
    return;

  priv->highlight_color = *color;

  g_object_notify (G_OBJECT (card), "highlight-color");
}

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

  pspec = g_param_spec_uchar ("card", NULL, NULL,
                              0, 255, 0,
                              G_PARAM_WRITABLE |
                              G_PARAM_READABLE |
                              G_PARAM_STATIC_NAME |
                              G_PARAM_STATIC_NICK |
                              G_PARAM_STATIC_BLURB);
  g_object_class_install_property (gobject_class, PROP_CARD, pspec);

  pspec = g_param_spec_object ("cache", NULL, NULL,
                               GAMES_TYPE_CARD_TEXTURES_CACHE,
                               G_PARAM_WRITABLE |
                               G_PARAM_READABLE |
                               G_PARAM_CONSTRUCT_ONLY |
                               G_PARAM_STATIC_NAME |
                               G_PARAM_STATIC_NICK |
                               G_PARAM_STATIC_BLURB);
  g_object_class_install_property (gobject_class, PROP_CACHE, pspec);

  pspec = g_param_spec_boolean ("highlighted", NULL, NULL,
                                FALSE,
                                G_PARAM_WRITABLE |
                                G_PARAM_READABLE |
                                G_PARAM_CONSTRUCT_ONLY |
                                G_PARAM_STATIC_NAME |
                                G_PARAM_STATIC_NICK |
                                G_PARAM_STATIC_BLURB);
  g_object_class_install_property (gobject_class, PROP_HIGHLIGHTED, pspec);

  pspec = clutter_param_spec_color ("highlight-color", NULL, NULL,
                                    &default_highlight_color,
                                    G_PARAM_WRITABLE |
                                    G_PARAM_STATIC_NAME |
                                    G_PARAM_STATIC_NICK |
                                    G_PARAM_STATIC_BLURB |
                                    G_PARAM_CONSTRUCT);
  g_object_class_install_property (gobject_class, PROP_HIGHLIGHT_COLOR, pspec);

  g_type_class_add_private (klass, sizeof (AisleriotCardPrivate));
}

static void
aisleriot_card_init (AisleriotCard *self)
{
  AisleriotCardPrivate *priv;

  priv = self->priv = AISLERIOT_CARD_GET_PRIVATE (self);

  priv->highlighted = FALSE;
  priv->highlight_color = default_highlight_color;
}

static void
aisleriot_card_dispose (GObject *self)
{
  AisleriotCard *card = AISLERIOT_CARD (self);

  aisleriot_card_unref_cache (card);

  G_OBJECT_CLASS (aisleriot_card_parent_class)->dispose (self);
}

ClutterActor *
aisleriot_card_new (GamesCardTexturesCache *cache,
                    Card card,
                    const ClutterColor *highlight_color)
{
  return g_object_new (AISLERIOT_TYPE_CARD,
                       "cache", cache,
                       "card", card.value,
                       "highlight-color", highlight_color,
                       NULL);
}

static void
aisleriot_card_paint (ClutterActor *actor)
{
  AisleriotCard *card = (AisleriotCard *) actor;
  AisleriotCardPrivate *priv = card->priv;
  Card card_num;
  ClutterFixed x_angle, y_angle;
  CoglHandle tex;
  ClutterFixed tex_width, tex_height;
  gboolean x_swapped = FALSE;
  static const ClutterColor white = { 0xff, 0xff, 0xff, 0xff };

  card_num = priv->card;

  tex = games_card_textures_cache_get_card_texture (priv->cache, card_num);
  if (G_UNLIKELY (tex == COGL_INVALID_HANDLE))
    return;

  x_angle = clutter_actor_get_rotationx (actor, CLUTTER_X_AXIS,
                                         NULL, NULL, NULL);
  y_angle = clutter_actor_get_rotationx (actor, CLUTTER_Y_AXIS,
                                         NULL, NULL, NULL);

  /* Show the other side of the card if it is rotated more than 180
     degrees in exactly one of the axes */
  if (ANGLE_IS_UPSIDE_DOWN (x_angle))
    card_num.attr.face_down = !card_num.attr.face_down;
  if (ANGLE_IS_UPSIDE_DOWN (y_angle)) {
    card_num.attr.face_down = !card_num.attr.face_down;
    /* The picture for the back of the card is already drawn as if it
       was rotated so we need to swap the coordinates so it will look
       right when it is rotated again */
    if (card_num.attr.face_down)
      x_swapped = TRUE;
  }

  tex_width = CLUTTER_INT_TO_FIXED (cogl_texture_get_width (tex));
  tex_height = CLUTTER_INT_TO_FIXED (cogl_texture_get_height (tex));

  /* Ideally we would just swap the texture coordinates, but Cogl
     won't let you do this */
  if (x_swapped) {
    cogl_push_matrix ();
    cogl_translate (CLUTTER_FIXED_TO_INT (tex_width) / 2, 0, 0);
    cogl_rotate (180, 0, 1, 0);
    cogl_translate (-CLUTTER_FIXED_TO_INT (tex_width) / 2, 0, 0);
  }

  cogl_color (&white);
  cogl_texture_rectangle (tex, 0, 0,
                          tex_width, tex_height,
                          0, 0, CFX_ONE, CFX_ONE);
  if (priv->highlighted) {
    cogl_color (&priv->highlight_color);
    cogl_texture_rectangle (tex, 0, 0,
                            tex_width, tex_height,
                            0, 0, CFX_ONE, CFX_ONE);
  }

  if (x_swapped)
    cogl_pop_matrix ();
}

static void
aisleriot_card_set_property (GObject *self,
                             guint property_id,
                             const GValue *value,
                             GParamSpec *pspec)
{
  AisleriotCard *card = AISLERIOT_CARD (self);

  switch (property_id) {
    case PROP_CARD:
      {
        Card card_num;

        card_num.value = g_value_get_uchar (value);

        aisleriot_card_set_card (card, card_num);
      }
      break;

    case PROP_CACHE:
      aisleriot_card_set_cache (card, g_value_get_object (value));
      break;

    case PROP_HIGHLIGHTED:
      aisleriot_card_set_highlighted (card, g_value_get_boolean (value));
      break;

    case PROP_HIGHLIGHT_COLOR:
      aisleriot_card_set_highlight_color (card, g_value_get_boxed (value));
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
    case PROP_CARD:
      g_value_set_uchar (value, priv->card.value);
      break;

    case PROP_CACHE:
      g_value_set_object (value, priv->cache);
      break;

    case PROP_HIGHLIGHTED:
      g_value_set_boolean (value, priv->highlighted);
      break;

    case PROP_HIGHLIGHT_COLOR:
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (self, property_id, pspec);
      break;
  }
}

void
aisleriot_card_set_card (AisleriotCard *card, Card card_num)
{
  AisleriotCardPrivate *priv;

  g_return_if_fail (AISLERIOT_IS_CARD (card));

  priv = card->priv;

  priv->card = card_num;

  clutter_actor_queue_redraw (CLUTTER_ACTOR (card));

  g_object_notify (G_OBJECT (card), "card");
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
aisleriot_card_set_cache (AisleriotCard *card, GamesCardTexturesCache *cache)
{
  AisleriotCardPrivate *priv = card->priv;

  if (cache)
    g_object_ref (cache);

  aisleriot_card_unref_cache (card);

  priv->cache = cache;

  clutter_actor_queue_redraw (CLUTTER_ACTOR (card));

  g_object_notify (G_OBJECT (card), "cache");
}

void
aisleriot_card_set_highlighted (AisleriotCard *card, gboolean highlighted)
{
  AisleriotCardPrivate *priv;

  g_return_if_fail (AISLERIOT_IS_CARD (card));

  priv = card->priv;

  priv->highlighted = highlighted;

  clutter_actor_queue_redraw (CLUTTER_ACTOR (card));

  g_object_notify (G_OBJECT (card), "highlighted");
}

gboolean
aisleriot_card_get_highlighted (AisleriotCard *card)
{
  AisleriotCardPrivate *priv;

  g_return_val_if_fail (AISLERIOT_IS_CARD (card), FALSE);

  priv = card->priv;

  return priv->highlighted;
}
