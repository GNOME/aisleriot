/*
   Copyright © 2004 Richard Hoelscher
   Copyright © 2007 Christian Persch
   
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

#include <config.h>

#include <string.h>

#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

/* For gdkcairo */
#include <gdk/gdk.h>

#include <librsvg/rsvg.h>
#include <librsvg/rsvg-cairo.h>

#include "ar-profile.h"

#include "ar-svg.h"
#include "ar-svg-private.h"

G_DEFINE_TYPE (ArSvg, ar_svg, G_TYPE_OBJECT);

static void
ar_svg_init (ArSvg *svg)
{
  svg->width = 0;
  svg->height = 0;
}

static void
ar_svg_finalize (GObject * object)
{
  ArSvg *svg = AR_SVG (object);

  if (svg->rsvg_handle != NULL) {
    g_object_unref (svg->rsvg_handle);
  }
  if (svg->font_options) {
    cairo_font_options_destroy (svg->font_options);
  }

  G_OBJECT_CLASS (ar_svg_parent_class)->finalize (object);
}

static void
ar_svg_class_init (ArSvgClass * klass)
{
  GObjectClass *oclass = G_OBJECT_CLASS (klass);

  oclass->finalize = ar_svg_finalize;

  rsvg_init ();
}

/**
 * ar_svg_render_cairo:
 * @preimage:
 * @cr:
 * @width: the desired width
 * @height: the desired height
 *
 * Renders from @preimage's image at the specified
 * @width and @height to @cr.
 **/
void
ar_svg_render_cairo (ArSvg *svg,
                             cairo_t *cr,
                             gint width,
                             gint height)
{
  g_return_if_fail (width > 0 && height > 0);

    ar_svg_render_cairo_sub (svg,
                                     cr,
                                     NULL,
                                     width,
                                     height,
                                     0.0, 0.0,
                                     ((double) width) /
                                     ((double) svg->width),
                                     ((double) height) /
                                     ((double) svg->height));
}

/**
 * ar_svg_render_cairo_sub:
 * @preimage:
 * @cr:
 * @node: a SVG node ID (starting with "#"), or %NULL
 * @width: the width of the clip region
 * @height: the height of the clip region
 * @xoffset: the x offset of the clip region
 * @yoffset: the y offset of the clip region
 * @xzoom: the x zoom factor
 * @yzoom: the y zoom factor
 *
 * Creates a #GdkPixbuf with the dimensions @width by @height,
 * and renders the subimage of @preimage specified by @node to it,
 * transformed by @xzoom, @yzoom and offset by @xoffset and @yoffset,
 * clipped to @width and @height.
 * If @node is NULL, the whole image is rendered into tha clip region.
 *
 * Returns: %TRUE, of %FALSE if there was an error or @preimage
 * isn't a scalable SVG image
 **/
void
ar_svg_render_cairo_sub (ArSvg *svg,
                                 cairo_t *cr,
                                 const char *node,
                                 int width,
                                 int height,
                                 double xoffset,
                                 double yoffset,
                                 double xzoom,
                                 double yzoom)
{
  cairo_matrix_t matrix;

  g_return_if_fail (AR_IS_SVG (svg));

  if (svg->font_options) {
    cairo_set_antialias (cr, cairo_font_options_get_antialias (svg->font_options));

    cairo_set_font_options (cr, svg->font_options);
  }

  cairo_matrix_init_identity (&matrix);
  cairo_matrix_scale (&matrix, xzoom, yzoom);
  cairo_matrix_translate (&matrix, xoffset, yoffset);

  cairo_set_matrix (cr, &matrix);

  rsvg_handle_render_cairo_sub (svg->rsvg_handle, cr, node);
}

/**
 * ar_svg_new_from_file:
 * @filename:
 * @error: a location for a #GError
 *
 * Creates a new #ArSvg from the image in @filename.
 *
 * Returns: (allow-none): a new #ArSvg, or %NULL if there was an error
 */
ArSvg *
ar_svg_new_from_file (const gchar * filename, GError ** error)
{
  ArSvg *svg;

  g_return_val_if_fail (filename != NULL, NULL);

  ar_profilestart ("creating ArSvg from %s", filename);

  svg = g_object_new (AR_TYPE_SVG, NULL);

  svg->rsvg_handle = rsvg_handle_new_from_file (filename, error);
  if (svg->rsvg_handle) {
    RsvgDimensionData data;

    rsvg_handle_get_dimensions (svg->rsvg_handle, &data);

    ar_profileend ("creating ArSvg from %s", filename);

    if (data.width == 0 || data.height == 0) {
      g_set_error (error,
                   GDK_PIXBUF_ERROR,
                   GDK_PIXBUF_ERROR_FAILED, "Image has zero extent");
      g_object_unref (svg);
      return NULL;
    }

    svg->width = data.width;
    svg->height = data.height;

    return svg;
  }

  ar_profileend ("creating ArSvg from %s", filename);

  return NULL;
}

/**
 * ar_svg_set_font_options:
 * @preimage: a #ArSvg
 * @font_options: the font options
 *
 * Turns on antialising of @preimage, if it contains an SVG image.
 */
void
ar_svg_set_font_options (ArSvg *svg,
                         const cairo_font_options_t * font_options)
{
  g_return_if_fail (AR_IS_SVG (svg));

  if (svg->font_options) {
    cairo_font_options_destroy (svg->font_options);
  }

  if (font_options) {
    svg->font_options = cairo_font_options_copy (font_options);
  } else {
    svg->font_options = NULL;
  }
}

/**
 * ar_svg_get_width:
 * @preimage:
 *
 * Returns: the natural width of the image in @preimage
 */
gint
ar_svg_get_width (ArSvg *svg)
{
  g_return_val_if_fail (AR_IS_SVG (svg), 0);

  return svg->width;
}

/**
 * ar_svg_get_height:
 * @preimage:
 *
 * Returns: the natural height of the image in @preimage
 */
gint
ar_svg_get_height (ArSvg *svg)
{
  g_return_val_if_fail (AR_IS_SVG (svg), 0);

  return svg->height;
}
