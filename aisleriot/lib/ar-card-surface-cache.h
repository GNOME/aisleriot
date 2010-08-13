/*
  Copyright © 2008 Neil Roberts
  Copyright © 2008, 2010 Christian Persch

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

#ifndef AR_CARD_SURFACE_CACHE_H
#define AR_CARD_SURFACE_CACHE_H

#include <glib-object.h>
#include <cairo.h>

#include "ar-card.h"
#include "ar-card-theme.h"

G_BEGIN_DECLS

#define AR_TYPE_CARD_SURFACE_CACHE            (ar_card_surface_cache_get_type())
#define AR_CARD_SURFACE_CACHE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_CARD_SURFACE_CACHE, ArCardSurfaceCache))
#define AR_CARD_SURFACE_CACHE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), AR_TYPE_CARD_SURFACE_CACHE, ArCardSurfaceCacheClass))
#define AR_IS_CARD_SURFACE_CACHE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_CARD_SURFACE_CACHE))
#define AR_IS_CARD_SURFACE_CACHE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), AR_TYPE_CARD_SURFACE_CACHE))
#define AR_CARD_SURFACE_CACHE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), AR_TYPE_CARD_SURFACE_CACHE, ArCardSurfaceCacheClass))

typedef struct _ArCardSurfaceCache        ArCardSurfaceCache;
typedef struct _ArCardSurfaceCacheClass   ArCardSurfaceCacheClass;
typedef struct _ArCardSurfaceCachePrivate ArCardSurfaceCachePrivate;

struct _ArCardSurfaceCacheClass
{
  GObjectClass parent_class;
};

struct _ArCardSurfaceCache
{
  GObject parent;

  ArCardSurfaceCachePrivate *priv;
};

GType ar_card_surface_cache_get_type (void);

ArCardSurfaceCache *ar_card_surface_cache_new (void);

void ar_card_surface_cache_set_drawable (ArCardSurfaceCache *cache,
                                         /* GdkWindow* */ gpointer drawable);

void ar_card_surface_cache_drop (ArCardSurfaceCache *cache);

void ar_card_surface_cache_set_theme (ArCardSurfaceCache *cache,
                                      ArCardTheme *theme);

ArCardTheme *ar_card_surface_cache_get_theme (ArCardSurfaceCache *cache);

cairo_surface_t *ar_card_surface_cache_get_card_surface (ArCardSurfaceCache *cache,
                                                         Card card);

cairo_surface_t *ar_card_surface_cache_get_card_surface_by_id (ArCardSurfaceCache *cache,
                                                               guint card_id);

cairo_surface_t *ar_card_surface_cache_get_slot_surface (ArCardSurfaceCache *cache);

G_END_DECLS

#endif /* AR_CARD_SURFACE_CACHE_H */
