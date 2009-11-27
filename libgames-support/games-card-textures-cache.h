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

#ifndef GAMES_CARD_TEXTURES_CACHE_H
#define GAMES_CARD_TEXTURES_CACHE_H

#include <glib-object.h>
#include <cogl/cogl.h>

#include "games-card.h"
#include "games-card-theme.h"

G_BEGIN_DECLS

#define GAMES_TYPE_CARD_TEXTURES_CACHE            (games_card_textures_cache_get_type())
#define GAMES_CARD_TEXTURES_CACHE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_TEXTURES_CACHE, GamesCardTexturesCache))
#define GAMES_CARD_TEXTURES_CACHE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_TEXTURES_CACHE, GamesCardTexturesCacheClass))
#define GAMES_IS_CARD_TEXTURES_CACHE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_TEXTURES_CACHE))
#define GAMES_IS_CARD_TEXTURES_CACHE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_TEXTURES_CACHE))
#define GAMES_CARD_TEXTURES_CACHE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_TEXTURES_CACHE, GamesCardTexturesCacheClass))

typedef struct _GamesCardTexturesCache        GamesCardTexturesCache;
typedef struct _GamesCardTexturesCacheClass   GamesCardTexturesCacheClass;
typedef struct _GamesCardTexturesCachePrivate GamesCardTexturesCachePrivate;

struct _GamesCardTexturesCacheClass
{
  GObjectClass parent_class;
};

struct _GamesCardTexturesCache
{
  GObject parent;

  GamesCardTexturesCachePrivate *priv;
};

GType games_card_textures_cache_get_type (void);

GamesCardTexturesCache *games_card_textures_cache_new (void);

void games_card_textures_cache_drop (GamesCardTexturesCache *cache);

void games_card_textures_cache_set_theme (GamesCardTexturesCache *cache,
                                          GamesCardTheme *theme);

GamesCardTheme *games_card_textures_cache_get_theme (GamesCardTexturesCache *cache);

CoglHandle games_card_textures_cache_get_card_texture (GamesCardTexturesCache *cache,
                                                       Card card);

CoglHandle games_card_textures_cache_get_card_texture_by_id (GamesCardTexturesCache *cache,
                                                             guint card_id);

CoglHandle games_card_textures_cache_get_slot_texture (GamesCardTexturesCache *cache);

G_END_DECLS

#endif /* GAMES_CARD_TEXTURES_CACHE_H */
