/*
  Copyright © 2004 Richard Hoelscher

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

/* Authors:   Richard Hoelscher <rah@rahga.com> */

/* Cache raster and vector images and render them to a specific size. */

#ifndef AR_PREIMAGE_H
#define AR_PREIMAGE_H

#include <glib.h>
#include <cairo.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

G_BEGIN_DECLS

#define AR_TYPE_PREIMAGE             (ar_preimage_get_type ())
#define AR_PREIMAGE(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj), AR_TYPE_PREIMAGE, ArPreimage))
#define AR_PREIMAGE_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), AR_TYPE_PREIMAGE, ArPreimageClass))
#define GAMES_IS_PREIMAGE(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_PREIMAGE))
#define GAMES_IS_PREIMAGE_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), AR_TYPE_PREIMAGE))
#define GAMES_GET_PREIMAGE_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), AR_TYPE_PREIMAGE, ArPreimageClass))

typedef struct _ArPreimage ArPreimage;

typedef struct {
  GObjectClass parent_class;
} ArPreimageClass;

GType ar_preimage_get_type (void);

ArPreimage *ar_preimage_new (void);

ArPreimage *ar_preimage_new_from_file (const gchar * filename,
                                             GError ** error);

void ar_preimage_set_font_options (ArPreimage * preimage,
                                      const cairo_font_options_t *font_options);

GdkPixbuf *ar_preimage_render (ArPreimage * preimage,
                                  gint width,
                                  gint height);
void ar_preimage_render_cairo (ArPreimage * preimage,
                                  cairo_t *cr,
                                  gint width,
                                  gint height);

GdkPixbuf *ar_preimage_render_sub (ArPreimage * preimage,
                                      const char *node,
                                      int width,
                                      int height,
                                      double xoffset,
                                      double yoffset,
                                      double xzoom, double yzoom);

void ar_preimage_render_cairo_sub (ArPreimage * preimage,
                                      cairo_t *cr,
                                      const char *node,
                                      int width,
                                      int height,
                                      double xoffset,
                                      double yoffset,
                                      double xzoom,
                                      double yzoom);

gboolean ar_preimage_is_scalable (ArPreimage * preimage);

gint ar_preimage_get_width (ArPreimage * preimage);

gint ar_preimage_get_height (ArPreimage * preimage);

GdkPixbuf *ar_preimage_render_unscaled_pixbuf (ArPreimage * preimage);

G_END_DECLS

#endif /* AR_PREIMAGE_H */
