/*
  Copyright © 2004 Richard Hoelscher

  This library is free software; you can redistribute it and'or modify
  it under the terms of the GNU Library General Public License as published
  by the Free Software Foundation; either version 3, or (at your option)
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

#ifndef AR_SVG_H
#define AR_SVG_H

#include <glib.h>
#include <cairo.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

G_BEGIN_DECLS

#define AR_TYPE_SVG             (ar_svg_get_type ())
#define AR_SVG(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj), AR_TYPE_SVG, ArSvg))
#define AR_SVG_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), AR_TYPE_SVG, ArSvgClass))
#define AR_IS_SVG(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_SVG))
#define AR_IS_SVG_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), AR_TYPE_SVG))
#define AR_GET_SVG_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), AR_TYPE_SVG, ArSvgClass))

typedef struct _ArSvg ArSvg;

typedef struct {
  GObjectClass parent_class;
} ArSvgClass;

GType ar_svg_get_type (void);

ArSvg *ar_svg_new (void);

ArSvg *ar_svg_new_from_file (const gchar * filename,
                                             GError ** error);

void ar_svg_set_font_options (ArSvg * preimage,
                                      const cairo_font_options_t *font_options);

void ar_svg_render_cairo (ArSvg * preimage,
                                  cairo_t *cr,
                                  gint width,
                                  gint height);

void ar_svg_render_cairo_sub (ArSvg * preimage,
                                      cairo_t *cr,
                                      const char *node,
                                      int width,
                                      int height,
                                      double xoffset,
                                      double yoffset,
                                      double xzoom,
                                      double yzoom);

gint ar_svg_get_width (ArSvg * preimage);

gint ar_svg_get_height (ArSvg * preimage);

G_END_DECLS

#endif /* AR_SVG_H */
