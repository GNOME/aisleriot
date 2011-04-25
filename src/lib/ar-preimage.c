/*
   Copyright © 2004 Richard Hoelscher
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

#include "ar-preimage.h"
#include "ar-preimage-private.h"

G_DEFINE_TYPE (ArPreimage, ar_preimage, G_TYPE_OBJECT);

static void
ar_preimage_init (ArPreimage * preimage)
{
  preimage->width = 0;
  preimage->height = 0;
}

static void
ar_preimage_finalize (GObject * object)
{
  ArPreimage *preimage = AR_PREIMAGE (object);

  if (preimage->rsvg_handle != NULL) {
    g_object_unref (preimage->rsvg_handle);
  }
  if (preimage->font_options) {
    cairo_font_options_destroy (preimage->font_options);
  }

  G_OBJECT_CLASS (ar_preimage_parent_class)->finalize (object);
}

static void
ar_preimage_class_init (ArPreimageClass * klass)
{
  GObjectClass *oclass = G_OBJECT_CLASS (klass);

  oclass->finalize = ar_preimage_finalize;

  rsvg_init ();
}

/**
 * ar_preimage_render:
 * @preimage: the image to render
 * @width: the desired width
 * @height: the desired height
 *
 * Creates a #GdkPixbuf from @preimage's image at the specified
 * @width and @height.
 *
 * Returns: (transfer full): the new #GdkPixbuf
 **/
GdkPixbuf *
ar_preimage_render (ArPreimage * preimage, gint width, gint height)
{
  GdkPixbuf *pixbuf;

  g_return_val_if_fail (width > 0 && height > 0, NULL);
  g_return_val_if_fail (preimage != NULL, NULL);

    pixbuf = ar_preimage_render_sub (preimage,
                                        NULL,
                                        width,
                                        height,
                                        0.0, 0.0,
                                        ((double) width) /
                                        ((double) preimage->width),
                                        ((double) height) /
                                        ((double) preimage->height));

  return pixbuf;
}

/**
 * ar_preimage_render_cairo:
 * @preimage:
 * @cr:
 * @width: the desired width
 * @height: the desired height
 *
 * Renders from @preimage's image at the specified
 * @width and @height to @cr.
 **/
void
ar_preimage_render_cairo (ArPreimage * preimage,
                             cairo_t *cr,
                             gint width,
                             gint height)
{
  g_return_if_fail (width > 0 && height > 0);
  g_return_if_fail (preimage != NULL);

    ar_preimage_render_cairo_sub (preimage,
                                     cr,
                                     NULL,
                                     width,
                                     height,
                                     0.0, 0.0,
                                     ((double) width) /
                                     ((double) preimage->width),
                                     ((double) height) /
                                     ((double) preimage->height));
}

/* This routine is copied from librsvg:
   Copyright © 2005 Dom Lachowicz <cinamod@hotmail.com>
   Copyright © 2005 Caleb Moore <c.moore@student.unsw.edu.au>
   Copyright © 2005 Red Hat, Inc.
 */
static void
cairo_pixels_to_pixbuf (guint8 * pixels, int rowstride, int height)
{
  int row;

  /* un-premultiply data */
  for (row = 0; row < height; row++) {
    guint8 *row_data = (pixels + (row * rowstride));
    int i;

    for (i = 0; i < rowstride; i += 4) {
      guint8 *b = &row_data[i];
      guint32 pixel;
      guint8 alpha;

      memcpy (&pixel, b, sizeof (guint32));
      alpha = (pixel & 0xff000000) >> 24;
      if (alpha == 0) {
        b[0] = b[1] = b[2] = b[3] = 0;
      } else {
        b[0] = (((pixel & 0xff0000) >> 16) * 255 + alpha / 2) / alpha;
        b[1] = (((pixel & 0x00ff00) >> 8) * 255 + alpha / 2) / alpha;
        b[2] = (((pixel & 0x0000ff) >> 0) * 255 + alpha / 2) / alpha;
        b[3] = alpha;
      }
    }
  }
}

/**
 * ar_preimage_render_cairo_sub:
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
ar_preimage_render_cairo_sub (ArPreimage * preimage,
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

  g_return_if_fail (AR_IS_PREIMAGE (preimage));

  if (preimage->font_options) {
    cairo_set_antialias (cr, cairo_font_options_get_antialias (preimage->font_options));

    cairo_set_font_options (cr, preimage->font_options);
  }

  cairo_matrix_init_identity (&matrix);
  cairo_matrix_scale (&matrix, xzoom, yzoom);
  cairo_matrix_translate (&matrix, xoffset, yoffset);

  cairo_set_matrix (cr, &matrix);

  rsvg_handle_render_cairo_sub (preimage->rsvg_handle, cr, node);
}

/**
 * ar_preimage_render_sub:
 * @preimage:
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
 * Returns: (transfer full) (allow-none): a new #GdkPixbuf, or %NULL if there was an error or @preimage
 * isn't a scalable SVG image
 */
GdkPixbuf *
ar_preimage_render_sub (ArPreimage * preimage,
                           const char *node,
                           int width,
                           int height,
                           double xoffset,
                           double yoffset, double xzoom, double yzoom)
{
  int rowstride;
  guint8 *data;
  cairo_surface_t *surface;
  cairo_t *cr;

  rowstride = cairo_format_stride_for_width (CAIRO_FORMAT_ARGB32, width);

  data = g_try_malloc0 (rowstride * height);
  if (!data)
    return NULL;

  surface = cairo_image_surface_create_for_data (data,
                                                 CAIRO_FORMAT_ARGB32,
                                                 width, height, rowstride);
  cr = cairo_create (surface);
  ar_preimage_render_cairo_sub (preimage, cr, node, width, height,
                                   xoffset, yoffset, xzoom, yzoom);
  cairo_destroy (cr);
  cairo_surface_destroy (surface);
  cairo_pixels_to_pixbuf (data, rowstride, height);

  return gdk_pixbuf_new_from_data (data,
                                   GDK_COLORSPACE_RGB,
                                   TRUE,
                                   8,
                                   width, height,
                                   rowstride,
                                   (GdkPixbufDestroyNotify) g_free, data);
}

/**
 * ar_preimage_new_from_file:
 * @filename:
 * @error: a location for a #GError
 *
 * Creates a new #ArPreimage from the image in @filename.
 *
 * Returns: (allow-none): a new #ArPreimage, or %NULL if there was an error
 */
ArPreimage *
ar_preimage_new_from_file (const gchar * filename, GError ** error)
{
  ArPreimage *preimage;

  g_return_val_if_fail (filename != NULL, NULL);

  ar_profilestart ("creating ArPreimage from %s", filename);

  preimage = g_object_new (AR_TYPE_PREIMAGE, NULL);

  preimage->rsvg_handle = rsvg_handle_new_from_file (filename, error);
  if (preimage->rsvg_handle) {
    RsvgDimensionData data;

    rsvg_handle_get_dimensions (preimage->rsvg_handle, &data);

    ar_profileend ("creating ArPreimage from %s", filename);

    if (data.width == 0 || data.height == 0) {
      g_set_error (error,
                   GDK_PIXBUF_ERROR,
                   GDK_PIXBUF_ERROR_FAILED, "Image has zero extent");
      g_object_unref (preimage);
      return NULL;
    }

    preimage->width = data.width;
    preimage->height = data.height;

    return preimage;
  }

  ar_profileend ("creating ArPreimage from %s", filename);

  return NULL;
}

/**
 * ar_preimage_set_font_options:
 * @preimage: a #ArPreimage
 * @font_options: the font options
 *
 * Turns on antialising of @preimage, if it contains an SVG image.
 */
void
ar_preimage_set_font_options (ArPreimage * preimage,
                                 const cairo_font_options_t * font_options)
{
  g_return_if_fail (AR_IS_PREIMAGE (preimage));

  if (preimage->font_options) {
    cairo_font_options_destroy (preimage->font_options);
  }

  if (font_options) {
    preimage->font_options = cairo_font_options_copy (font_options);
  } else {
    preimage->font_options = NULL;
  }
}

/**
 * ar_preimage_get_width:
 * @preimage:
 *
 * Returns: the natural width of the image in @preimage
 */
gint
ar_preimage_get_width (ArPreimage * preimage)
{
  g_return_val_if_fail (AR_IS_PREIMAGE (preimage), 0);

  return preimage->width;
}

/**
 * ar_preimage_get_height:
 * @preimage:
 *
 * Returns: the natural height of the image in @preimage
 */
gint
ar_preimage_get_height (ArPreimage * preimage)
{
  g_return_val_if_fail (AR_IS_PREIMAGE (preimage), 0);

  return preimage->height;
}
