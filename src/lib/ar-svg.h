/*
 * Copyright © 2004 Richard Hoelscher
 * Copyright © 2011 Christian Persch
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

/* Authors:   Richard Hoelscher <rah@rahga.com> */

/* Cache raster and vector images and render them to a specific size. */

#ifndef AR_SVG_H
#define AR_SVG_H

#include <gio/gio.h>
#include <cairo.h>
#include <librsvg/rsvg.h>

G_BEGIN_DECLS

#define AR_TYPE_SVG             (ar_svg_get_type ())
#define AR_SVG(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj), AR_TYPE_SVG, ArSvg))
#define AR_SVG_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), AR_TYPE_SVG, ArSvgClass))
#define AR_IS_SVG(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_SVG))
#define AR_IS_SVG_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), AR_TYPE_SVG))
#define AR_GET_SVG_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), AR_TYPE_SVG, ArSvgClass))

typedef struct _ArSvg      ArSvg;
typedef struct _ArSvgClass ArSvgClass;

GType ar_svg_get_type (void);

ArSvg *ar_svg_new (void);

ArSvg *ar_svg_new_from_uri_sync (const char *uri,
                                 GCancellable *cancellable,
                                 GError ** error);

ArSvg *ar_svg_new_from_gfile_sync (GFile *file,
                                   GCancellable *cancellable,
                                   GError ** error);

ArSvg *ar_svg_new_from_filename_sync (const gchar * filename,
                                      GCancellable *cancellable,
                                      GError ** error);

cairo_font_options_t *ar_svg_get_font_options (ArSvg *svg);

void ar_svg_set_font_options (ArSvg *svg,
                              const cairo_font_options_t *font_options);

void ar_svg_render_cairo (ArSvg *svg,
                          cairo_surface_t *surface,
                                  gint width,
                                  gint height);

void ar_svg_render_cairo_sub (ArSvg *svg,
                              cairo_surface_t *surface,
                                      const char *node,
                                      int width,
                                      int height,
                                      double xoffset,
                                      double yoffset,
                                      double xzoom,
                                      double yzoom);

gint ar_svg_get_width (ArSvg *svg);

gint ar_svg_get_height (ArSvg *svg);

G_END_DECLS

#endif /* AR_SVG_H */
