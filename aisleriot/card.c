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
#include <clutter/clutter.h>
#include <gtk/gtk.h>
#include <cogl/cogl.h>

#include "card.h"

static void aisleriot_card_paint (ClutterActor *actor);

static void aisleriot_card_get_preferred_width (ClutterActor *self,
                                                ClutterUnit   for_height,
                                                ClutterUnit  *min_width_p,
                                                ClutterUnit  *natural_width_p);
static void aisleriot_card_get_preferred_height
                                               (ClutterActor *self,
                                                ClutterUnit   for_width,
                                                ClutterUnit  *min_height_p,
                                                ClutterUnit  *natural_height_p);

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
  Card bottom_card, top_card;
  ClutterColor highlight_color;
  gboolean highlighted;

  GamesCardTexturesCache *cache;
};

enum
{
  PROP_0,

  PROP_CACHE,
  PROP_BOTTOM_CARD,
  PROP_TOP_CARD,
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
  actor_class->get_preferred_width = aisleriot_card_get_preferred_width;
  actor_class->get_preferred_height = aisleriot_card_get_preferred_height;

  pspec = g_param_spec_uchar ("bottom-card", NULL, NULL,
                              0, 255, 0,
                              G_PARAM_WRITABLE |
                              G_PARAM_READABLE |
                              G_PARAM_STATIC_NAME |
                              G_PARAM_STATIC_NICK |
                              G_PARAM_STATIC_BLURB);
  g_object_class_install_property (gobject_class, PROP_BOTTOM_CARD, pspec);

  pspec = g_param_spec_uchar ("top-card", NULL, NULL,
                              0, 255, 0,
                              G_PARAM_WRITABLE |
                              G_PARAM_READABLE |
                              G_PARAM_STATIC_NAME |
                              G_PARAM_STATIC_NICK |
                              G_PARAM_STATIC_BLURB);
  g_object_class_install_property (gobject_class, PROP_TOP_CARD, pspec);

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

#if 0
  pspec = clutter_param_spec_color ("highlight-color", NULL, NULL,
                                    &default_highlight_color,
                                    G_PARAM_WRITABLE |
                                    G_PARAM_STATIC_NAME |
                                    G_PARAM_STATIC_NICK |
                                    G_PARAM_STATIC_BLURB |
                                    G_PARAM_CONSTRUCT);
#else
  pspec = g_param_spec_boxed ("highlight-color", NULL, NULL,
                              CLUTTER_TYPE_COLOR,
                              G_PARAM_WRITABLE |
                              G_PARAM_STATIC_NAME |
                              G_PARAM_STATIC_NICK |
                              G_PARAM_STATIC_BLURB |
                              G_PARAM_CONSTRUCT);
#endif
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
                    Card bottom_card,
                    Card top_card,
                    const ClutterColor *highlight_color)
{
  return g_object_new (AISLERIOT_TYPE_CARD,
                       "cache", cache,
                       "bottom-card", bottom_card.value,
                       "top-card", top_card.value,
                       "highlight-color", highlight_color,
                       NULL);
}

static void
aisleriot_card_get_preferred_width (ClutterActor *self,
                                    ClutterUnit   for_height,
                                    ClutterUnit  *min_width_p,
                                    ClutterUnit  *natural_width_p)
{
  AisleriotCard *card = AISLERIOT_CARD (self);
  AisleriotCardPrivate *priv = card->priv;
  CoglHandle tex;
  guint width;

  tex = games_card_textures_cache_get_card_texture (priv->cache,
                                                    priv->top_card);

  if (G_UNLIKELY (tex == COGL_INVALID_HANDLE))
    width = 0;
  else
    width = cogl_texture_get_width (tex);

  if (min_width_p)
    *min_width_p = 0;

  if (natural_width_p)
    *natural_width_p = CLUTTER_UNITS_FROM_DEVICE (width);
}

static void
aisleriot_card_get_preferred_height (ClutterActor *self,
                                     ClutterUnit   for_width,
                                     ClutterUnit  *min_height_p,
                                     ClutterUnit  *natural_height_p)
{
  AisleriotCard *card = AISLERIOT_CARD (self);
  AisleriotCardPrivate *priv = card->priv;
  CoglHandle tex;
  guint height;

  tex = games_card_textures_cache_get_card_texture (priv->cache,
                                                    priv->top_card);

  if (G_UNLIKELY (tex == COGL_INVALID_HANDLE))
    height = 0;
  else
    height = cogl_texture_get_height (tex);

  if (min_height_p)
    *min_height_p = 0;

  if (natural_height_p)
    *natural_height_p = CLUTTER_UNITS_FROM_DEVICE (height);
}

static void
aisleriot_card_paint (ClutterActor *actor)
{
  AisleriotCard *card = (AisleriotCard *) actor;
  AisleriotCardPrivate *priv = card->priv;
  Card card_num;
  ClutterFixed x_angle, y_angle;
  CoglHandle tex;
  ClutterActorBox alloc_box;
  gboolean x_swapped = FALSE;
  static const ClutterColor white = { 0xff, 0xff, 0xff, 0xff };
  gboolean show_top = TRUE;

  x_angle = clutter_actor_get_rotationx (actor, CLUTTER_X_AXIS,
                                         NULL, NULL, NULL);
  y_angle = clutter_actor_get_rotationx (actor, CLUTTER_Y_AXIS,
                                         NULL, NULL, NULL);

  /* Show the other side of the card if it is rotated more than 180
     degrees in exactly one of the axes */
  if (ANGLE_IS_UPSIDE_DOWN (x_angle))
    show_top = !show_top;
  if (ANGLE_IS_UPSIDE_DOWN (y_angle)) {
    show_top = !show_top;
    x_swapped = TRUE;
  }

  card_num = show_top ? priv->top_card : priv->bottom_card;

  tex = games_card_textures_cache_get_card_texture (priv->cache, card_num);
  if (G_UNLIKELY (tex == COGL_INVALID_HANDLE))
    return;

  clutter_actor_get_allocation_box (actor, &alloc_box);

  /* Ideally we would just swap the texture coordinates, but Cogl
     won't let you do this */
  if (x_swapped) {
    cogl_push_matrix ();
    cogl_translate (CLUTTER_UNITS_TO_DEVICE (alloc_box.x2 - alloc_box.x1) / 2,
                    0, 0);
    cogl_rotate (180, 0, 1, 0);
    cogl_translate (-CLUTTER_UNITS_TO_DEVICE (alloc_box.x2 - alloc_box.x1) / 2,
                    0, 0);
  }

  cogl_color (&white);
  cogl_texture_rectangle (tex, 0, 0,
                          CLUTTER_UNITS_TO_FIXED (alloc_box.x2 - alloc_box.x1),
                          CLUTTER_UNITS_TO_FIXED (alloc_box.y2 - alloc_box.y1),
                          0, 0, CFX_ONE, CFX_ONE);
  if (priv->highlighted) {
    cogl_color (&priv->highlight_color);
    cogl_texture_rectangle (tex, 0, 0,
                            CLUTTER_UNITS_TO_FIXED (alloc_box.x2
                                                    - alloc_box.x1),
                            CLUTTER_UNITS_TO_FIXED (alloc_box.y2
                                                    - alloc_box.y1),
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
    case PROP_BOTTOM_CARD:
      g_value_set_uchar (value, priv->bottom_card.value);
      break;

    case PROP_TOP_CARD:
      g_value_set_uchar (value, priv->top_card.value);
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
