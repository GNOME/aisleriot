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

#include <math.h>
#include <string.h>

#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#ifdef HAVE_RSVG
#include <librsvg/rsvg.h>
#include <librsvg/rsvg-cairo.h>
#endif /* HAVE_RSVG */

#include "games-preimage.h"

struct _GamesPreimage {
  GObject parent;

  gint width;
  gint height;

#ifdef HAVE_RSVG
  RsvgHandle *rsvg_handle;
#endif

  /* raster pixbuf data */
  GdkPixbuf *pixbuf;

  guint scalable : 1;

  guint antialias_set : 1;
  guint antialias : 2; /* enough bits for cairo_antialias_t */
  guint subpixel_order : 3; /* enough bits for cairo_subpixel_order_t */
};

G_DEFINE_TYPE (GamesPreimage, games_preimage, G_TYPE_OBJECT);

static void
games_preimage_init (GamesPreimage * preimage)
{
  preimage->scalable = FALSE;
  preimage->width = 0;
  preimage->height = 0;
}

static void
games_preimage_finalize (GObject * object)
{
  GamesPreimage *preimage = GAMES_PREIMAGE (object);

#ifdef HAVE_RSVG
  if (preimage->rsvg_handle != NULL) {
    g_object_unref (preimage->rsvg_handle);
  }
#endif

  if (preimage->pixbuf != NULL) {
    g_object_unref (preimage->pixbuf);
  }

  G_OBJECT_CLASS (games_preimage_parent_class)->finalize (object);
}

static void
games_preimage_class_init (GamesPreimageClass * klass)
{
  GObjectClass *oclass = G_OBJECT_CLASS (klass);

  oclass->finalize = games_preimage_finalize;

#ifdef HAVE_RSVG
  rsvg_init ();
#endif
}

/**
 * games_preimage_render:
 * @preimage:
 * @width: the desired width
 * @height: the desired height
 *
 * Creates a #GdkPixbuf from @preimage's image at the specified
 * @width and @height.
 *
 * Returns: a new #GdkPixbuf
*/
GdkPixbuf *
games_preimage_render (GamesPreimage * preimage, gint width, gint height)
{
  GdkPixbuf *pixbuf;

  g_return_val_if_fail (width > 0 && height > 0, NULL);
  g_return_val_if_fail (preimage != NULL, NULL);

#ifdef HAVE_RSVG
  if (preimage->scalable) {     /* Render vector image */
    pixbuf = games_preimage_render_sub (preimage,
                                        NULL,
                                        width,
                                        height,
                                        0.0, 0.0,
                                        ((double) width) /
                                        ((double) preimage->width),
                                        ((double) height) /
                                        ((double) preimage->height));
  } else
#endif /* HAVE_RSVG */
  {
    /* Render raster image */
    pixbuf = gdk_pixbuf_scale_simple (preimage->pixbuf,
                                      width, height, GDK_INTERP_BILINEAR);
  }

  return pixbuf;
}

#ifdef HAVE_RSVG

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
 * games_preimage_render_sub:
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
 * Returns: a new #GdkPixbuf, or %NULL if there was an error or @preimage
 * isn't a scalable SVG image
 */
GdkPixbuf *
games_preimage_render_sub (GamesPreimage * preimage,
                           const char *node,
                           int width,
                           int height,
                           double xoffset,
                           double yoffset, double xzoom, double yzoom)
{
  GdkPixbuf *pixbuf = NULL;
  int rowstride;
  guint8 *data;
  cairo_surface_t *surface;
  cairo_t *cx;
  cairo_matrix_t matrix;

  if (!preimage->scalable)
    return NULL;

  rowstride = width * 4;

  data = g_try_malloc0 (rowstride * height);
  if (!data)
    return NULL;

  surface = cairo_image_surface_create_for_data (data,
                                                 CAIRO_FORMAT_ARGB32,
                                                 width, height, rowstride);
  if (cairo_surface_status (surface) != CAIRO_STATUS_SUCCESS) {
    g_free (data);
    return NULL;
  }

  cx = cairo_create (surface);

  if (preimage->antialias_set) {
    cairo_font_options_t *options;

    cairo_set_antialias (cx, (cairo_antialias_t) preimage->antialias);

    options = cairo_font_options_create ();
    cairo_get_font_options (cx, options);
    cairo_font_options_set_antialias (options,
                                      (cairo_antialias_t) preimage->
                                      antialias);
    cairo_font_options_set_subpixel_order (options,
                                           (cairo_subpixel_order_t) preimage->
                                           subpixel_order);
    cairo_set_font_options (cx, options);
    cairo_font_options_destroy (options);
  }

  cairo_matrix_init_identity (&matrix);
  cairo_matrix_scale (&matrix, xzoom, yzoom);
  cairo_matrix_translate (&matrix, xoffset, yoffset);

  cairo_set_matrix (cx, &matrix);

  rsvg_handle_render_cairo_sub (preimage->rsvg_handle, cx, node);

  cairo_pixels_to_pixbuf (data, rowstride, height);

  if (cairo_status (cx) == CAIRO_STATUS_SUCCESS) {
    pixbuf = gdk_pixbuf_new_from_data (data,
                                       GDK_COLORSPACE_RGB,
                                       TRUE,
                                       8,
                                       width, height,
                                       rowstride,
                                       (GdkPixbufDestroyNotify) g_free, NULL);
    data = NULL;
  }

  cairo_destroy (cx);

  cairo_surface_destroy (surface);

  g_free (data);

  return pixbuf;
}

#endif /* HAVE_RSVG */

/**
 * games_preimage_new_from_file:
 * @filename:
 * @error: a location for a #GError
 *
 * Creates a new #GamesPreimage from the image in @filename.
 *
 * Returns: a new #GamesPreimage, or %NULL if there was an error
 */
GamesPreimage *
games_preimage_new_from_file (const gchar * filename, GError ** error)
{
  GamesPreimage *preimage;
  GdkPixbuf *pixbuf;

  g_return_val_if_fail (filename != NULL, NULL);

  preimage = g_object_new (GAMES_TYPE_PREIMAGE, NULL);

#ifdef HAVE_RSVG
  preimage->rsvg_handle = rsvg_handle_new_from_file (filename, NULL);
  if (preimage->rsvg_handle) {
    RsvgDimensionData data;

    preimage->scalable = TRUE;

    rsvg_handle_get_dimensions (preimage->rsvg_handle, &data);

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
#endif /* HAVE_RSVG */

  /* Not an SVG */
  preimage->scalable = FALSE;

  pixbuf = gdk_pixbuf_new_from_file (filename, error);
  if (!pixbuf) {
    g_object_unref (preimage);
    return NULL;
  }

  preimage->pixbuf = pixbuf;
  preimage->width = gdk_pixbuf_get_width (pixbuf);
  preimage->height = gdk_pixbuf_get_height (pixbuf);

  return preimage;
}

/**
 * games_preimage_set_antialias:
 * @preimage:
 * @antialias: the antialiasing mode to use (see @cairo_antialias_t)
 * @subpixel_order: the subpixel order to use (see @cairo_subpixel_order_t)
 * if @antialias is %CAIRO_ANTIALIAS_SUBPIXEL 
 *
 * Turns on antialising of @preimage, if it contains an SVG image.
 */
void
games_preimage_set_antialias (GamesPreimage * preimage,
                              guint antialias, guint subpixel_order)
{
  g_return_if_fail (GAMES_IS_PREIMAGE (preimage));

  preimage->antialias_set = TRUE;
  preimage->antialias = antialias;
  preimage->subpixel_order = subpixel_order;
}

/**
 * games_preimage_is_scalable:
 * @preimage:
 *
 * Returns: %TRUE iff @preimage contains an SVG image
 */
gboolean
games_preimage_is_scalable (GamesPreimage * preimage)
{
  g_return_val_if_fail (GAMES_IS_PREIMAGE (preimage), FALSE);

  return preimage->scalable;
}

/**
 * games_preimage_get_width:
 * @preimage:
 *
 * Returns: the natural width of the image in @preimage
 */
gint
games_preimage_get_width (GamesPreimage * preimage)
{
  g_return_val_if_fail (GAMES_IS_PREIMAGE (preimage), 0);

  return preimage->width;
}

/**
 * games_preimage_get_height:
 * @preimage:
 *
 * Returns: the natural height of the image in @preimage
 */
gint
games_preimage_get_height (GamesPreimage * preimage)
{
  g_return_val_if_fail (GAMES_IS_PREIMAGE (preimage), 0);

  return preimage->height;
}

/**
 * games_preimage_render_unscaled_pixbuf:
 * @preimage:
 *
 * Renders @preimage onto a new #GdkPixbuf at its natural size
 *
 * Returns: a reference to a #GdkPixbuf possibly owned by @images which
 * you must not modify; or %NULL if there was an error
 */
GdkPixbuf *
games_preimage_render_unscaled_pixbuf (GamesPreimage * preimage)
{
  GdkPixbuf *unscaled_pixbuf;

  g_return_val_if_fail (GAMES_IS_PREIMAGE (preimage), NULL);

  if ((unscaled_pixbuf = preimage->pixbuf)) {
    g_object_ref (unscaled_pixbuf);
  } else {
    unscaled_pixbuf = games_preimage_render (preimage,
                                             preimage->width,
                                             preimage->height);
  }

  return unscaled_pixbuf;
}
