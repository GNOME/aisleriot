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

#ifndef __AISLERIOT_SLOT_RENDERER_H__
#define __AISLERIOT_SLOT_RENDERER_H__

#include <clutter/clutter.h>

#include "game.h"
#include "ar-card-textures-cache.h"
#include "ar-style.h"

G_BEGIN_DECLS

#define AISLERIOT_TYPE_SLOT_RENDERER                                    \
  (aisleriot_slot_renderer_get_type())
#define AISLERIOT_SLOT_RENDERER(obj)                                    \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj),                                   \
                               AISLERIOT_TYPE_SLOT_RENDERER,            \
                               AisleriotSlotRenderer))
#define AISLERIOT_SLOT_RENDERER_CLASS(klass)                            \
  (G_TYPE_CHECK_CLASS_CAST ((klass),                                    \
                            AISLERIOT_TYPE_SLOT_RENDERER,               \
                            AisleriotSlotRendererClass))
#define AISLERIOT_IS_SLOT_RENDERER(obj)                                 \
  (G_TYPE_CHECK_INSTANCE_TYPE ((obj),                                   \
                               AISLERIOT_TYPE_SLOT_RENDERER))
#define AISLERIOT_IS_SLOT_RENDERER_CLASS(klass)                         \
  (G_TYPE_CHECK_CLASS_TYPE ((klass),                                    \
                            AISLERIOT_TYPE_SLOT_RENDERER))
#define AISLERIOT_SLOT_RENDERER_GET_CLASS(obj)                          \
  (G_TYPE_INSTANCE_GET_CLASS ((obj),                                    \
                              AISLERIOT_TYPE_SLOT_RENDERER,             \
                              AisleriotSlotRendererClass))

typedef struct _AisleriotSlotRenderer        AisleriotSlotRenderer;
typedef struct _AisleriotSlotRendererClass   AisleriotSlotRendererClass;
typedef struct _AisleriotSlotRendererPrivate AisleriotSlotRendererPrivate;
typedef struct _AisleriotAnimStart           AisleriotAnimStart;

struct _AisleriotSlotRendererClass
{
  ClutterActorClass parent_class;
};

struct _AisleriotSlotRenderer
{
  ClutterActor parent;

  AisleriotSlotRendererPrivate *priv;
};

struct _AisleriotAnimStart
{
  gint cardx, cardy;
  Card old_card;
  gboolean raise;
};

GType aisleriot_slot_renderer_get_type (void) G_GNUC_CONST;

ClutterActor *aisleriot_slot_renderer_new (ArStyle *style,
                                           ArCardTexturesCache *cache,
                                           ArSlot *slot);

void aisleriot_slot_renderer_set_highlight (AisleriotSlotRenderer *srend,
                                            gint hightlight_start);
guint aisleriot_slot_renderer_get_highlight (AisleriotSlotRenderer *srend);

void aisleriot_slot_renderer_set_revealed_card (AisleriotSlotRenderer *srend,
                                                gint revealed_card);
gint aisleriot_slot_renderer_get_revealed_card (AisleriotSlotRenderer *srend);

ClutterContainer *aisleriot_slot_renderer_get_animation_layer
                                  (AisleriotSlotRenderer *srend);
void aisleriot_slot_renderer_set_animation_layer
                                  (AisleriotSlotRenderer *srend,
                                   ClutterContainer *animation_layer);

void aisleriot_slot_renderer_set_animations (AisleriotSlotRenderer *srend,
                                             guint n_anims,
                                             const AisleriotAnimStart *anims,
                                             guint n_unexposed_animated_cards);

G_END_DECLS

#endif /* __AISLERIOT_SLOT_RENDERER_H__ */
