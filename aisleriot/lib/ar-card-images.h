/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007 Christian Persch

  This library is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

/* Manage a set of pixbufs containing a deck of cards. */

#ifndef AR_CARD_IMAGES_H
#define AR_CARD_IMAGES_H

#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include "ar-card.h"
#include "ar-card-theme.h"

G_BEGIN_DECLS

#define AR_TYPE_CARD_IMAGES            (ar_card_images_get_type ())
#define AR_CARD_IMAGES(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_CARD_IMAGES, ArCardImages))
#define AR_CARD_IMAGES_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), AR_TYPE_CARD_IMAGES, ArCardImagesClass))
#define AR_IS_CARD_IMAGES(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_CARD_IMAGES))
#define AR_IS_CARD_IMAGES_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), AR_TYPE_CARD_IMAGES))
#define AR_CARD_IMAGES_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), AR_TYPE_CARD_IMAGES, ArCardImagesClass))

typedef enum
{
  CACHE_PIXBUFS,
  CACHE_PIXMAPS,
  LAST_CACHE_MODE
} ArCardImagesCacheMode;

typedef struct _ArCardImages       ArCardImages;
typedef struct _ArCardImagesClass  ArCardImagesClass;

GType ar_card_images_get_type (void);

ArCardImages *ar_card_images_new (void);

void ar_card_images_set_theme (ArCardImages *images,
                                  ArCardTheme *theme);

ArCardTheme *ar_card_images_get_theme (ArCardImages *images);

void ar_card_images_set_cache_mode (ArCardImages * images,
                                       ArCardImagesCacheMode mode);

void ar_card_images_drop_cache (ArCardImages *images);

void ar_card_images_set_drawable (ArCardImages * images,
                                     GdkWindow * drawable);

gboolean ar_card_images_set_size (ArCardImages * images,
                                     gint width,
                                     gint height, gdouble proportion);

void ar_card_images_get_size (ArCardImages *images,
                                 CardSize *size);

void ar_card_images_set_background_color (ArCardImages * images,
                                             const GdkColor * color);

void ar_card_images_set_selection_color (ArCardImages * images,
                                            const GdkColor * color);

/* Pixbuf routines */
GdkPixbuf *ar_card_images_get_card_pixbuf_by_id (ArCardImages * images,
                                                    guint card_id,
                                                    gboolean highlighted);

GdkPixbuf *ar_card_images_get_card_pixbuf (ArCardImages * images,
                                              Card card,
                                              gboolean highlighted);

/* Pixmap routines */
GdkPixmap *ar_card_images_get_card_pixmap_by_id (ArCardImages * images,
                                                    guint card_id,
                                                    gboolean highlighted);

GdkPixmap *ar_card_images_get_card_pixmap (ArCardImages * images,
                                              Card card,
                                              gboolean highlighted);

GdkBitmap *ar_card_images_get_card_mask (ArCardImages * images);

/* Slot routines */
GdkPixbuf *ar_card_images_get_slot_pixbuf (ArCardImages * images,
                                              gboolean highlighted);

GdkPixmap *ar_card_images_get_slot_pixmap (ArCardImages * images,
                                              gboolean highlighted);

GdkBitmap *ar_card_images_get_slot_mask (ArCardImages * images);

/* Deprecated */
GdkPixbuf *ar_card_images_get_card_pixbuf_by_suit_and_rank (ArCardImages
                                                               * images,
                                                               guint suit,
                                                               guint rank);

GdkPixmap *ar_card_images_get_card_pixmap_by_suit_and_rank (ArCardImages
                                                               * images,
                                                               guint suit,
                                                               guint rank);

G_END_DECLS

#endif /* AR_CARD_IMAGES_H */
