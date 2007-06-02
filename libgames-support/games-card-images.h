/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007 Christian Persch

  This library is free software; you can redistribute it and'or modify
  it under the terms of the GNU Library General Public License as published
  by the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

/* Manage a set of pixbufs containing a deck of cards. */

#ifndef GAMES_CARD_IMAGES_H
#define GAMES_CARD_IMAGES_H

#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include "games-card-common.h"
#include "games-card-theme.h"

G_BEGIN_DECLS

#define GAMES_TYPE_CARD_IMAGES            (games_card_images_get_type ())
#define GAMES_CARD_IMAGES(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CARD_IMAGES, GamesCardImages))
#define GAMES_CARD_IMAGES_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CARD_IMAGES, GamesCardImagesClass))
#define GAMES_IS_CARD_IMAGES(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CARD_IMAGES))
#define GAMES_IS_CARD_IMAGES_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CARD_IMAGES))
#define GAMES_CARD_IMAGES_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CARD_IMAGES))

typedef enum
{
  CACHE_PIXBUFS,
  CACHE_PIXMAPS,
  LAST_CACHE_MODE
} GamesCardImagesCacheMode;

typedef struct _GamesCardImages {
  GObject parent;

  GamesCardTheme *theme;
  GdkDrawable *drawable;
  GdkBitmap *card_mask;
  GdkBitmap *slot_mask;
  gpointer *cache;
  GdkColor background_colour;
  GdkColor selection_colour;

  guint scalable : 1;
  guint cache_mode : 1;
} GamesCardImages;

typedef GObjectClass GamesCardImagesClass;

GType games_card_images_get_type (void);

GamesCardImages *games_card_images_new (gboolean scalable);

void games_card_images_set_cache_mode (GamesCardImages * images,
                                       GamesCardImagesCacheMode mode);

void games_card_images_set_drawable (GamesCardImages * images,
                                     GdkWindow * drawable);

void games_card_images_set_antialias (GamesCardImages * images,
                                      guint antialias, guint subpixel_order);

gboolean games_card_images_set_theme (GamesCardImages * images,
                                      const gchar * name);

const gchar *games_card_images_get_theme (GamesCardImages * images);

gboolean games_card_images_set_size (GamesCardImages * images,
                                     gint width,
                                     gint height, gdouble proportion);

CardSize games_card_images_get_size (GamesCardImages * images);

double games_card_images_get_aspect (GamesCardImages * images);

void games_card_images_set_background_color (GamesCardImages * images,
                                             const GdkColor * color);

void games_card_images_set_selection_color (GamesCardImages * images,
                                            const GdkColor * color);

/* Pixbuf routines */
GdkPixbuf *games_card_images_get_card_pixbuf_by_id (GamesCardImages * images,
                                                    guint card_id,
                                                    gboolean highlighted);

GdkPixbuf *games_card_images_get_card_pixbuf (GamesCardImages * images,
                                              Card card,
                                              gboolean highlighted);

/* Pixmap routines */
GdkPixmap *games_card_images_get_card_pixmap_by_id (GamesCardImages * images,
                                                    guint card_id,
                                                    gboolean highlighted);

GdkPixmap *games_card_images_get_card_pixmap (GamesCardImages * images,
                                              Card card,
                                              gboolean highlighted);

GdkBitmap *games_card_images_get_card_mask (GamesCardImages * images);

/* Slot routines */
GdkPixbuf *games_card_images_get_slot_pixbuf (GamesCardImages * images,
                                              gboolean highlighted);

GdkPixmap *games_card_images_get_slot_pixmap (GamesCardImages * images,
                                              gboolean highlighted);

GdkBitmap *games_card_images_get_slot_mask (GamesCardImages * images);

/* Deprecated */
GdkPixbuf *games_card_images_get_card_pixbuf_by_suit_and_rank (GamesCardImages
                                                               * images,
                                                               guint suit,
                                                               guint rank);

GdkPixmap *games_card_images_get_card_pixmap_by_suit_and_rank (GamesCardImages
                                                               * images,
                                                               guint suit,
                                                               guint rank);

G_END_DECLS

#endif /* GAMES_CARD_IMAGES_H */
