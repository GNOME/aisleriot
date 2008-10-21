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

#ifndef AISLERIOT_CARD_CACHE_H
#define AISLERIOT_CARD_CACHE_H

#include <glib-object.h>
#include <cogl/cogl.h>
#include <libgames-support/games-card-images.h>

G_BEGIN_DECLS

#define AISLERIOT_TYPE_CARD_CACHE                                       \
  (aisleriot_card_cache_get_type())
#define AISLERIOT_CARD_CACHE(obj)                                       \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj),                                   \
                               AISLERIOT_TYPE_CARD_CACHE,               \
                               AisleriotCardCache))
#define AISLERIOT_CARD_CACHE_CLASS(klass)                               \
  (G_TYPE_CHECK_CLASS_CAST ((klass),                                    \
                            AISLERIOT_TYPE_CARD_CACHE,                  \
                            AisleriotCardCacheClass))
#define AISLERIOT_IS_CARD_CACHE(obj)                                    \
  (G_TYPE_CHECK_INSTANCE_TYPE ((obj),                                   \
                               AISLERIOT_TYPE_CARD_CACHE))
#define AISLERIOT_IS_CARD_CACHE_CLASS(klass)                            \
  (G_TYPE_CHECK_CLASS_TYPE ((klass),                                    \
                            AISLERIOT_TYPE_CARD_CACHE))
#define AISLERIOT_CARD_CACHE_GET_CLASS(obj)                             \
  (G_TYPE_INSTANCE_GET_CLASS ((obj),                                    \
                              AISLERIOT_TYPE_CARD_CACHE,                \
                              AisleriotCardCacheClass))

typedef struct _AisleriotCardCache        AisleriotCardCache;
typedef struct _AisleriotCardCacheClass   AisleriotCardCacheClass;
typedef struct _AisleriotCardCachePrivate AisleriotCardCachePrivate;

struct _AisleriotCardCacheClass
{
  GObjectClass parent_class;
};

struct _AisleriotCardCache
{
  GObject parent;

  AisleriotCardCachePrivate *priv;
};

GType aisleriot_card_cache_get_type (void) G_GNUC_CONST;

AisleriotCardCache *aisleriot_card_cache_new (GamesCardImages *images);

CoglHandle aisleriot_card_cache_get_card_texture (AisleriotCardCache *cache,
                                                  Card card,
                                                  gboolean highlighted);
CoglHandle aisleriot_card_cache_get_card_texture_by_id
                                                 (AisleriotCardCache *cache,
                                                  guint card_id,
                                                  gboolean highlighted);
CoglHandle aisleriot_card_cache_get_slot_texture (AisleriotCardCache *cache,
                                                  gboolean highlighted);

void aisleriot_card_cache_clear (AisleriotCardCache *cache);

G_END_DECLS

#endif /* AISLERIOT_CARD_CACHE_H */
