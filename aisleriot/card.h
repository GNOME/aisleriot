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

#ifndef AISLERIOT_CARD_H
#define AISLERIOT_CARD_H

#include <clutter/clutter.h>
#include "ar-card-textures-cache.h"

G_BEGIN_DECLS

#define AISLERIOT_TYPE_CARD                                             \
  (aisleriot_card_get_type())
#define AISLERIOT_CARD(obj)                                             \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj),                                   \
                               AISLERIOT_TYPE_CARD,                     \
                               AisleriotCard))
#define AISLERIOT_CARD_CLASS(klass)                                     \
  (G_TYPE_CHECK_CLASS_CAST ((klass),                                    \
                            AISLERIOT_TYPE_CARD,                        \
                            AisleriotCardClass))
#define AISLERIOT_IS_CARD(obj)                                          \
  (G_TYPE_CHECK_INSTANCE_TYPE ((obj),                                   \
                               AISLERIOT_TYPE_CARD))
#define AISLERIOT_IS_CARD_CLASS(klass)                                  \
  (G_TYPE_CHECK_CLASS_TYPE ((klass),                                    \
                            AISLERIOT_TYPE_CARD))
#define AISLERIOT_CARD_GET_CLASS(obj)                                   \
  (G_TYPE_INSTANCE_GET_CLASS ((obj),                                    \
                              AISLERIOT_TYPE_CARD,                      \
                              AisleriotCardClass))

typedef struct _AisleriotCard        AisleriotCard;
typedef struct _AisleriotCardClass   AisleriotCardClass;
typedef struct _AisleriotCardPrivate AisleriotCardPrivate;

struct _AisleriotCardClass
{
  ClutterActorClass parent_class;
};

struct _AisleriotCard
{
  ClutterActor parent;

  AisleriotCardPrivate *priv;
};

GType aisleriot_card_get_type (void) G_GNUC_CONST;

ClutterActor *aisleriot_card_new (ArCardTexturesCache *cache,
                                  Card bottom_card,
                                  Card top_card);

void aisleriot_card_set_card (AisleriotCard *card,
                              Card bottom_card,
                              Card top_card);

void aisleriot_card_set_highlighted (AisleriotCard *card,
                                     gboolean highlighted);
gboolean aisleriot_card_get_highlighted (AisleriotCard *card);

G_END_DECLS

#endif /* AISLERIOT_CARD_H */
