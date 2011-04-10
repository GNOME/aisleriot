/*
  Copyright © 2008 Neil Roberts
  Copyright © 2008 Christian Persch

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef AR_CARD_TEXTURES_CACHE_H
#define AR_CARD_TEXTURES_CACHE_H

#include <glib-object.h>
#include <cogl/cogl.h>

#include "ar-card.h"
#include "ar-card-theme.h"

G_BEGIN_DECLS

#define AR_TYPE_CARD_TEXTURES_CACHE            (ar_card_textures_cache_get_type())
#define AR_CARD_TEXTURES_CACHE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_CARD_TEXTURES_CACHE, ArCardTexturesCache))
#define AR_CARD_TEXTURES_CACHE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), AR_TYPE_CARD_TEXTURES_CACHE, ArCardTexturesCacheClass))
#define AR_IS_CARD_TEXTURES_CACHE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_CARD_TEXTURES_CACHE))
#define AR_IS_CARD_TEXTURES_CACHE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), AR_TYPE_CARD_TEXTURES_CACHE))
#define AR_CARD_TEXTURES_CACHE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), AR_TYPE_CARD_TEXTURES_CACHE, ArCardTexturesCacheClass))

typedef struct _ArCardTexturesCache        ArCardTexturesCache;
typedef struct _ArCardTexturesCacheClass   ArCardTexturesCacheClass;
typedef struct _ArCardTexturesCachePrivate ArCardTexturesCachePrivate;

struct _ArCardTexturesCacheClass
{
  GObjectClass parent_class;
};

struct _ArCardTexturesCache
{
  GObject parent;

  ArCardTexturesCachePrivate *priv;
};

GType ar_card_textures_cache_get_type (void);

ArCardTexturesCache *ar_card_textures_cache_new (void);

void ar_card_textures_cache_drop (ArCardTexturesCache *cache);

void ar_card_textures_cache_set_theme (ArCardTexturesCache *cache,
                                          ArCardTheme *theme);

ArCardTheme *ar_card_textures_cache_get_theme (ArCardTexturesCache *cache);

CoglHandle ar_card_textures_cache_get_card_texture (ArCardTexturesCache *cache,
                                                       Card card);

CoglHandle ar_card_textures_cache_get_card_texture_by_id (ArCardTexturesCache *cache,
                                                             guint card_id);

CoglHandle ar_card_textures_cache_get_slot_texture (ArCardTexturesCache *cache);

G_END_DECLS

#endif /* AR_CARD_TEXTURES_CACHE_H */
