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
#include <string.h>
#include <math.h>

#include "slot-renderer.h"
#include "card.h"

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
                                               GamesCardTexturesCache *cache);

static void completed_cb (AisleriotSlotRenderer *srend);


G_DEFINE_TYPE (AisleriotSlotRenderer, aisleriot_slot_renderer,
               CLUTTER_TYPE_ACTOR);

#define AISLERIOT_SLOT_RENDERER_GET_PRIVATE(obj) \
  (G_TYPE_INSTANCE_GET_PRIVATE ((obj), AISLERIOT_TYPE_SLOT_RENDERER, \
                                AisleriotSlotRendererPrivate))

typedef struct _AnimationData AnimationData;

struct _AisleriotSlotRendererPrivate
{
  ArStyle *style;

  GamesCardTexturesCache *cache;

  ArSlot *slot;

  CoglHandle material;

  ClutterColor highlight_color;
  gboolean show_highlight;
  gint highlight_start;

  gint revealed_card;

  ClutterTimeline *timeline;
  guint completed_handler;
  GArray *animations;
  guint n_unexposed_animated_cards;

  ClutterContainer *animation_layer;
};

struct _AnimationData
{
  ClutterActor *card_tex;
  ClutterBehaviour *move, *rotate, *depth;
};

enum
{
  PROP_0,

  PROP_CACHE,
  PROP_SLOT,
  PROP_STYLE,
  PROP_HIGHLIGHT,
  PROP_REVEALED_CARD,
  PROP_ANIMATION_LAYER
};

static void
sync_style_selection_color (ArStyle *style,
                            GParamSpec *pspec,
                            AisleriotSlotRenderer *srend)
{
  AisleriotSlotRendererPrivate *priv = srend->priv;
  ClutterColor color;

  ar_style_get_selection_color (style, &color);
  if (clutter_color_equal (&color, &priv->highlight_color))
    return;

  priv->highlight_color = color;

  if (priv->show_highlight)
    clutter_actor_queue_redraw (CLUTTER_ACTOR (srend));
}

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

  g_object_class_install_property
    (gobject_class,
     PROP_STYLE,
     g_param_spec_object ("style", NULL, NULL,
                          AR_TYPE_STYLE,
                          G_PARAM_WRITABLE |
                          G_PARAM_CONSTRUCT_ONLY |
                          G_PARAM_STATIC_STRINGS));

  pspec = g_param_spec_object ("cache", NULL, NULL,
                               GAMES_TYPE_CARD_TEXTURES_CACHE,
                               G_PARAM_WRITABLE |
                               G_PARAM_CONSTRUCT_ONLY |
                               G_PARAM_STATIC_STRINGS);
  g_object_class_install_property (gobject_class, PROP_CACHE, pspec);

  pspec = g_param_spec_pointer ("slot", NULL, NULL,
                                G_PARAM_WRITABLE |
                                G_PARAM_CONSTRUCT_ONLY |
                                G_PARAM_STATIC_STRINGS);
  g_object_class_install_property (gobject_class, PROP_SLOT, pspec);

  pspec = g_param_spec_int ("highlight", NULL, NULL,
                            -1, G_MAXINT, G_MAXINT,
                            G_PARAM_READWRITE |
                            G_PARAM_STATIC_STRINGS);
  g_object_class_install_property (gobject_class, PROP_HIGHLIGHT, pspec);

  pspec = g_param_spec_int ("revealed-card", NULL, NULL,
                            -1, G_MAXINT, -1,
                            G_PARAM_READWRITE |
                            G_PARAM_STATIC_STRINGS);
  g_object_class_install_property (gobject_class, PROP_REVEALED_CARD, pspec);

  pspec = g_param_spec_object ("animation-layer", NULL, NULL,
                               CLUTTER_TYPE_CONTAINER,
                               G_PARAM_READWRITE |
                               G_PARAM_STATIC_STRINGS);
  g_object_class_install_property (gobject_class, PROP_ANIMATION_LAYER, pspec);

  g_type_class_add_private (klass, sizeof (AisleriotSlotRendererPrivate));
}

static void
aisleriot_slot_renderer_init (AisleriotSlotRenderer *self)
{
  AisleriotSlotRendererPrivate *priv;

  priv = self->priv = AISLERIOT_SLOT_RENDERER_GET_PRIVATE (self);

  priv->revealed_card = -1;
  priv->highlight_start = G_MAXINT;

  priv->animations = g_array_new (FALSE, FALSE, sizeof (AnimationData));
  priv->timeline = clutter_timeline_new (500);
  g_signal_connect_swapped (priv->timeline, "completed",
                            G_CALLBACK (completed_cb), self);
}

static void
aisleriot_slot_renderer_dispose (GObject *object)
{
  AisleriotSlotRenderer *self = (AisleriotSlotRenderer *) object;
  AisleriotSlotRendererPrivate *priv = self->priv;

  aisleriot_slot_renderer_set_cache (self, NULL);

  /* Get rid of any running animations */
  aisleriot_slot_renderer_set_animations (self, 0, NULL, 0);

  if (priv->timeline) {
    g_object_unref (priv->timeline);
    priv->timeline = NULL;
  }

  if (priv->material != COGL_INVALID_HANDLE) {
    cogl_handle_unref (priv->material);
    priv->material = COGL_INVALID_HANDLE;
  }

  aisleriot_slot_renderer_set_animation_layer (self, NULL);

  G_OBJECT_CLASS (aisleriot_slot_renderer_parent_class)->dispose (object);
}

static void
aisleriot_slot_renderer_finalize (GObject *object)
{
  AisleriotSlotRenderer *srend = AISLERIOT_SLOT_RENDERER (object);
  AisleriotSlotRendererPrivate *priv = srend->priv;

  g_array_free (priv->animations, TRUE);

  g_signal_handlers_disconnect_by_func (priv->style,
                                        G_CALLBACK (sync_style_selection_color),
                                        srend);
  g_object_unref (priv->style);

  G_OBJECT_CLASS (aisleriot_slot_renderer_parent_class)->finalize (object);
}

ClutterActor *
aisleriot_slot_renderer_new (ArStyle *style,
                             GamesCardTexturesCache *cache,
                             ArSlot *slot)
{
  return g_object_new (AISLERIOT_TYPE_SLOT_RENDERER,
                       "style", style,
                       "cache", cache,
                       "slot", slot,
                       NULL);
}

static void
aisleriot_slot_renderer_set_cache (AisleriotSlotRenderer *srend,
                                   GamesCardTexturesCache *cache)
{
  AisleriotSlotRendererPrivate *priv = srend->priv;

  if (cache == priv->cache)
    return;

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

    case PROP_STYLE:
      priv->style = g_value_dup_object (value);

      sync_style_selection_color (priv->style, NULL, srend);
      g_signal_connect (priv->style, "notify::" AR_STYLE_PROP_SELECTION_COLOR,
                        G_CALLBACK (sync_style_selection_color), srend);
      break;

    case PROP_HIGHLIGHT:
      aisleriot_slot_renderer_set_highlight (srend,
                                             g_value_get_int (value));
      break;

    case PROP_REVEALED_CARD:
      aisleriot_slot_renderer_set_revealed_card (srend,
                                                 g_value_get_int (value));
      break;

    case PROP_ANIMATION_LAYER:
      aisleriot_slot_renderer_set_animation_layer (srend,
                                                   g_value_get_object (value));
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
      g_value_set_int (value,
                        aisleriot_slot_renderer_get_highlight (srend));
      break;

    case PROP_REVEALED_CARD:
      g_value_set_int (value,
                       aisleriot_slot_renderer_get_revealed_card (srend));
      break;

    case PROP_ANIMATION_LAYER:
      g_value_set_object (value,
                          aisleriot_slot_renderer_get_animation_layer (srend));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

static void
aisleriot_slot_renderer_set_material_for_card (AisleriotSlotRenderer *srend,
                                               CoglHandle tex,
                                               gboolean show_highlight)
{
  AisleriotSlotRendererPrivate *priv = srend->priv;
  guint8 opacity = clutter_actor_get_paint_opacity (CLUTTER_ACTOR (srend));

  if (priv->material == COGL_INVALID_HANDLE)
    priv->material = cogl_material_new ();

  if (show_highlight)
    {
      CoglColor color;

      /* The previous code for drawing the highlight rendered the
         normal card texture and then rendered the card again
         multiplied by the highlight color but with 50%
         transparency. The blend function is alpha*src+(1-alpha*dst)
         where src is effectively the tex times the highlight color
         and the dst is the original tex. Therefore the final color is
         0.5*tex+0.5*tex*highlight which is the same as
         (0.5+highlight/2)*tex. We can precompute that value to avoid
         having to draw the card twice */
      cogl_color_set_from_4ub (&color,
                               MIN (priv->highlight_color.red
                                    / 2 + 128, 0xff),
                               MIN (priv->highlight_color.green
                                    / 2 + 128, 0xff),
                               MIN (priv->highlight_color.blue
                                    / 2 + 128, 0xff),
                               opacity);
      cogl_color_premultiply (&color);
      cogl_material_set_color (priv->material, &color);
    }
  else
    cogl_material_set_color4ub (priv->material,
                                opacity, opacity, opacity, opacity);

  cogl_material_set_layer (priv->material, 0, tex);
  cogl_set_source (priv->material);
}

static void
aisleriot_slot_renderer_paint_card (AisleriotSlotRenderer *srend,
                                    guint card_num)
{
  AisleriotSlotRendererPrivate *priv = srend->priv;
  Card card = CARD (priv->slot->cards->data[card_num]);
  CoglHandle cogl_tex;
  guint tex_width, tex_height;
  int cardx, cardy;

  cogl_tex = games_card_textures_cache_get_card_texture (priv->cache, card);
  if (G_UNLIKELY (cogl_tex == COGL_INVALID_HANDLE))
    return;

  tex_width = cogl_texture_get_width (cogl_tex);
  tex_height = cogl_texture_get_height (cogl_tex);

  aisleriot_game_get_card_offset (priv->slot, card_num,
                                  FALSE,
                                  &cardx, &cardy);

  aisleriot_slot_renderer_set_material_for_card
    (srend, cogl_tex,
     priv->show_highlight && card_num >= priv->highlight_start);

  cogl_rectangle (cardx, cardy, cardx + tex_width, cardy + tex_height);
}

static void
aisleriot_slot_renderer_paint (ClutterActor *actor)
{
  AisleriotSlotRenderer *srend = (AisleriotSlotRenderer *) actor;
  AisleriotSlotRendererPrivate *priv = srend->priv;
  guint n_cards, n_animated_cards;
  guint8 *cards;
  guint i;

  g_return_if_fail (priv->cache != NULL);
  g_return_if_fail (priv->slot != NULL);

  cards = priv->slot->cards->data;
  n_cards = priv->slot->cards->len;
  n_animated_cards = priv->animations->len + priv->n_unexposed_animated_cards;

  g_assert (n_cards >= priv->slot->exposed);
  g_assert (priv->n_unexposed_animated_cards == 0 || priv->animations->len > 0);
  g_assert (n_cards >= n_animated_cards);

  if (n_cards <= n_animated_cards) {
    CoglHandle cogl_tex;
    guint tex_width, tex_height;

    cogl_tex = games_card_textures_cache_get_slot_texture (priv->cache);
    if (G_UNLIKELY (cogl_tex == COGL_INVALID_HANDLE))
      return;

    tex_width = cogl_texture_get_width (cogl_tex);
    tex_height = cogl_texture_get_height (cogl_tex);

    aisleriot_slot_renderer_set_material_for_card (srend, cogl_tex,
                                                   priv->show_highlight);
    cogl_rectangle (0, 0, tex_width, tex_height);
  } else {
    guint first_card, last_card;

    first_card = MIN (n_cards - n_animated_cards - 1,
                      n_cards - priv->slot->exposed);
    last_card = n_cards - n_animated_cards;

    for (i = first_card; i < last_card; i++)
      if (i != priv->revealed_card)
        aisleriot_slot_renderer_paint_card (srend, i);

    /* Paint the revealed card after all of the other cards so that it
     * will appeear on top.
     */
    if (priv->revealed_card >= first_card && priv->revealed_card < last_card)
      aisleriot_slot_renderer_paint_card (srend, priv->revealed_card);
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
                                       gint highlight)
{
  AisleriotSlotRendererPrivate *priv;

  g_return_if_fail (AISLERIOT_IS_SLOT_RENDERER (srend));

  priv = srend->priv;

  priv->highlight_start = highlight;
  priv->show_highlight = priv->highlight_start != G_MAXINT;

  clutter_actor_queue_redraw (CLUTTER_ACTOR (srend));

  g_object_notify (G_OBJECT (srend), "highlight");
}

gint
aisleriot_slot_renderer_get_revealed_card (AisleriotSlotRenderer *srend)
{
  g_return_val_if_fail (AISLERIOT_IS_SLOT_RENDERER (srend), 0);

  return srend->priv->revealed_card;
}

void
aisleriot_slot_renderer_set_revealed_card (AisleriotSlotRenderer *srend,
                                           gint revealed_card)
{
  AisleriotSlotRendererPrivate *priv;

  g_return_if_fail (AISLERIOT_IS_SLOT_RENDERER (srend));

  priv = srend->priv;

  priv->revealed_card = revealed_card;

  clutter_actor_queue_redraw (CLUTTER_ACTOR (srend));

  g_object_notify (G_OBJECT (srend), "revealed-card");
}

ClutterContainer *
aisleriot_slot_renderer_get_animation_layer (AisleriotSlotRenderer *srend)
{
  AisleriotSlotRendererPrivate *priv;

  g_return_val_if_fail (AISLERIOT_IS_SLOT_RENDERER (srend), NULL);

  priv = srend->priv;

  return priv->animation_layer;
}

void
aisleriot_slot_renderer_set_animation_layer (AisleriotSlotRenderer *srend,
                                             ClutterContainer *animation_layer)
{
  AisleriotSlotRendererPrivate *priv;

  g_return_if_fail (AISLERIOT_IS_SLOT_RENDERER (srend));

  priv = srend->priv;

  if (animation_layer)
    g_object_ref (animation_layer);

  if (priv->animation_layer)
    g_object_unref (priv->animation_layer);

  priv->animation_layer = animation_layer;

  g_object_notify (G_OBJECT (srend), "animation-layer");
}

static gdouble
aisleriot_slot_sine_animation_mode (ClutterAlpha *alpha,
                                    gpointer data)
{
  ClutterTimeline *tl = clutter_alpha_get_timeline (alpha);

  return sin (clutter_timeline_get_progress (tl) * G_PI);
}

void
aisleriot_slot_renderer_set_animations (AisleriotSlotRenderer *srend,
                                        guint n_anims,
                                        const AisleriotAnimStart *anims,
                                        guint n_unexposed_animated_cards)
{
  AisleriotSlotRendererPrivate *priv;
  guint i;
  gint card_num;

  g_return_if_fail (AISLERIOT_IS_SLOT_RENDERER (srend));

  priv = srend->priv;

  g_return_if_fail (n_anims <= priv->slot->exposed);

  /* Destroy the current animations */
  for (i = 0; i < priv->animations->len; i++) {
    AnimationData *anim_data;

    anim_data = &g_array_index (priv->animations, AnimationData, i);

    if (anim_data->move)
      g_object_unref (anim_data->move);
    if (anim_data->rotate)
      g_object_unref (anim_data->rotate);
    if (anim_data->depth)
      g_object_unref (anim_data->depth);

    clutter_actor_destroy (anim_data->card_tex);
    g_object_unref (anim_data->card_tex);
  }

  g_array_set_size (priv->animations, 0);

  card_num = priv->slot->cards->len - n_anims;

  for (i = 0; i < n_anims; i++) {
    AnimationData anim_data;
    ClutterAlpha *alpha;
    ClutterKnot knots[2];
    Card card = CARD (priv->slot->cards->data[card_num]);
    guint card_width, card_height;

    memset (&anim_data, 0, sizeof (anim_data));

    anim_data.card_tex = aisleriot_card_new (priv->cache,
                                             anims[i].old_card,
                                             card);

    card_width = clutter_actor_get_width (anim_data.card_tex);
    card_height = clutter_actor_get_height (anim_data.card_tex);

    g_object_ref_sink (anim_data.card_tex);
    if (priv->animation_layer)
      clutter_container_add (priv->animation_layer,
                             CLUTTER_ACTOR (anim_data.card_tex), NULL);

    clutter_actor_set_position (anim_data.card_tex,
                                anims[i].cardx, anims[i].cardy);

    knots[0].x = anims[i].cardx;
    knots[0].y = anims[i].cardy;

    aisleriot_game_get_card_offset (priv->slot, card_num, FALSE,
                                    &knots[1].x, &knots[1].y);
    knots[1].x += priv->slot->rect.x;
    knots[1].y += priv->slot->rect.y;

    alpha = clutter_alpha_new_full (priv->timeline, CLUTTER_LINEAR);

    anim_data.move
      = clutter_behaviour_path_new_with_knots (alpha, knots,
                                               G_N_ELEMENTS (knots));
    clutter_behaviour_apply (anim_data.move, anim_data.card_tex);

    if (anims[i].old_card.value != card.value) {
      int center_x, center_y;

      center_x = card_width / 2;
      center_y = card_height / 2;

      clutter_actor_set_rotation (anim_data.card_tex, CLUTTER_Y_AXIS,
                                  180.0,
                                  center_x, center_y, 0);

      anim_data.rotate = clutter_behaviour_rotate_new (alpha,
                                                       CLUTTER_Y_AXIS,
                                                       CLUTTER_ROTATE_CW,
                                                       180.0, 0.0);
      clutter_behaviour_rotate_set_center (CLUTTER_BEHAVIOUR_ROTATE
                                           (anim_data.rotate),
                                           center_x, center_y, 0);

      clutter_behaviour_apply (anim_data.rotate, anim_data.card_tex);
    }

    if (anims[i].raise) {
      alpha = clutter_alpha_new_with_func (priv->timeline,
                                           aisleriot_slot_sine_animation_mode,
                                           NULL, NULL);

      anim_data.depth = clutter_behaviour_depth_new (alpha,
                                                     0, card_height);
      clutter_behaviour_apply (anim_data.depth, anim_data.card_tex);
    }

    g_array_append_val (priv->animations, anim_data);

    card_num++;
  }

  if (n_anims > 0) {
    clutter_timeline_rewind (priv->timeline);
    clutter_timeline_start (priv->timeline);
  }

  priv->n_unexposed_animated_cards = n_unexposed_animated_cards;

  clutter_actor_queue_redraw (CLUTTER_ACTOR (srend));
}

static void
completed_cb (AisleriotSlotRenderer *srend)
{
  /* Get rid of all animation actors */
  aisleriot_slot_renderer_set_animations (srend, 0, NULL, 0);

  /* Redraw so that the animated actors will be drawn as part of the
     renderer instead */
  clutter_actor_queue_redraw (CLUTTER_ACTOR (srend));
}
