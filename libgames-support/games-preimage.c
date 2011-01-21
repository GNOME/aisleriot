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

#ifdef HAVE_RSVG
#include <librsvg/rsvg.h>
#include <librsvg/rsvg-cairo.h>
#endif /* HAVE_RSVG */

#include "games-profile.h"

#include "games-preimage.h"

G_DEFINE_TYPE (GamesPreimage, games_preimage, G_TYPE_OBJECT);

struct GamesPreimagePrivate {
  gint width;
  gint height;

#ifdef HAVE_RSVG
  RsvgHandle *rsvg_handle;
  cairo_font_options_t *font_options;
#endif

  /* raster pixbuf data */
  GdkPixbuf *pixbuf;

  guint scalable : 1;
};

static void
games_preimage_init (GamesPreimage * preimage)
{
  preimage->priv->scalable = FALSE;
  preimage->priv->width = 0;
  preimage->priv->height = 0;
}

static void
games_preimage_finalize (GObject * object)
{
  GamesPreimage *preimage = GAMES_PREIMAGE (object);

#ifdef HAVE_RSVG
  if (preimage->priv->rsvg_handle != NULL) {
    g_object_unref (preimage->priv->rsvg_handle);
  }
  if (preimage->priv->font_options) {
    cairo_font_options_destroy (preimage->priv->font_options);
  }
#endif

  if (preimage->priv->pixbuf != NULL) {
    g_object_unref (preimage->priv->pixbuf);
  }

  G_OBJECT_CLASS (games_preimage_parent_class)->finalize (object);
}

static void
games_preimage_class_init (GamesPreimageClass * klass)
{
  GObjectClass *oclass = G_OBJECT_CLASS (klass);

  oclass->finalize = games_preimage_finalize;

  g_type_class_add_private (oclass, sizeof (GamesPreimagePrivate));

#ifdef HAVE_RSVG
  rsvg_init ();
#endif
}

/**
 * games_preimage_render:
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
games_preimage_render (GamesPreimage * preimage, gint width, gint height)
{
  GdkPixbuf *pixbuf;

  g_return_val_if_fail (width > 0 && height > 0, NULL);
  g_return_val_if_fail (preimage != NULL, NULL);

#ifdef HAVE_RSVG
  if (preimage->priv->scalable) {     /* Render vector image */
    pixbuf = games_preimage_render_sub (preimage,
                                        NULL,
                                        width,
                                        height,
                                        0.0, 0.0,
                                        ((double) width) /
                                        ((double) preimage->priv->width),
                                        ((double) height) /
                                        ((double) preimage->priv->height));
  } else
#endif /* HAVE_RSVG */
  {
    /* Render raster image */
    pixbuf = gdk_pixbuf_scale_simple (preimage->priv->pixbuf,
                                      width, height, GDK_INTERP_BILINEAR);
  }

  return pixbuf;
}

/**
 * games_preimage_render_cairo:
 * @preimage:
 * @cr:
 * @width: the desired width
 * @height: the desired height
 *
 * Renders from @preimage's image at the specified
 * @width and @height to @cr.
 **/
void
games_preimage_render_cairo (GamesPreimage * preimage,
                             cairo_t *cr,
                             gint width,
                             gint height)
{
  g_return_if_fail (width > 0 && height > 0);
  g_return_if_fail (preimage != NULL);

#ifdef HAVE_RSVG
  if (preimage->priv->scalable) {     /* Render vector image */
    games_preimage_render_cairo_sub (preimage,
                                     cr,
                                     NULL,
                                     width,
                                     height,
                                     0.0, 0.0,
                                     ((double) width) /
                                     ((double) preimage->priv->width),
                                     ((double) height) /
                                     ((double) preimage->priv->height));
  } else
#endif /* HAVE_RSVG */
  {
    GdkPixbuf *pixbuf;

    /* FIXMEchpe: we don't really need this fallback anymore */
    /* Render raster image */
    pixbuf = gdk_pixbuf_scale_simple (preimage->priv->pixbuf,
                                      width, height, GDK_INTERP_BILINEAR);

    cairo_save (cr);
    gdk_cairo_set_source_pixbuf (cr, pixbuf, 0, 0);
    cairo_paint (cr);
    cairo_restore (cr);

    g_object_unref (pixbuf);
  }
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
 * games_preimage_render_cairo_sub:
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
games_preimage_render_cairo_sub (GamesPreimage * preimage,
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

  if (!preimage->priv->scalable)
    return;

  if (preimage->priv->font_options) {
    cairo_set_antialias (cr, cairo_font_options_get_antialias (preimage->priv->font_options));

    cairo_set_font_options (cr, preimage->priv->font_options);
  }

  cairo_matrix_init_identity (&matrix);
  cairo_matrix_scale (&matrix, xzoom, yzoom);
  cairo_matrix_translate (&matrix, xoffset, yoffset);

  cairo_set_matrix (cr, &matrix);

  rsvg_handle_render_cairo_sub (preimage->priv->rsvg_handle, cr, node);
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
 * Returns: (transfer full) (allow-none): a new #GdkPixbuf, or %NULL if there was an error or @preimage
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
  int rowstride;
  guint8 *data;
  cairo_surface_t *surface;
  cairo_t *cr;

  if (!preimage->priv->scalable)
    return NULL;

#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE (1, 6, 0)
  rowstride = cairo_format_stride_for_width (CAIRO_FORMAT_ARGB32, width);
#else
  rowstride = width * 4;
#endif

  data = g_try_malloc0 (rowstride * height);
  if (!data)
    return NULL;

  surface = cairo_image_surface_create_for_data (data,
                                                 CAIRO_FORMAT_ARGB32,
                                                 width, height, rowstride);
  cr = cairo_create (surface);
  games_preimage_render_cairo_sub (preimage, cr, node, width, height,
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

#endif /* HAVE_RSVG */

/**
 * games_preimage_new_from_file:
 * @filename:
 * @error: a location for a #GError
 *
 * Creates a new #GamesPreimage from the image in @filename.
 *
 * Returns: (allow-none): a new #GamesPreimage, or %NULL if there was an error
 */
GamesPreimage *
games_preimage_new_from_file (const gchar * filename, GError ** error)
{
  GamesPreimage *preimage;
  GdkPixbuf *pixbuf;

  g_return_val_if_fail (filename != NULL, NULL);

  _games_profile_start ("creating GamesPreimage from %s", filename);

  preimage = g_object_new (GAMES_TYPE_PREIMAGE, NULL);

#ifdef HAVE_RSVG
  preimage->priv->rsvg_handle = rsvg_handle_new_from_file (filename, NULL);
  if (preimage->priv->rsvg_handle) {
    RsvgDimensionData data;

    preimage->priv->scalable = TRUE;

    rsvg_handle_get_dimensions (preimage->priv->rsvg_handle, &data);

    _games_profile_end ("creating GamesPreimage from %s", filename);

    if (data.width == 0 || data.height == 0) {
      g_set_error (error,
                   GDK_PIXBUF_ERROR,
                   GDK_PIXBUF_ERROR_FAILED, "Image has zero extent");
      g_object_unref (preimage);
      return NULL;
    }

    preimage->priv->width = data.width;
    preimage->priv->height = data.height;

    return preimage;
  }
#endif /* HAVE_RSVG */

  /* Not an SVG */
  preimage->priv->scalable = FALSE;

  pixbuf = gdk_pixbuf_new_from_file (filename, error);
  _games_profile_end ("creating GamesPreimage from %s", filename);

  if (!pixbuf) {
    g_object_unref (preimage);
    return NULL;
  }

  preimage->priv->pixbuf = pixbuf;
  preimage->priv->width = gdk_pixbuf_get_width (pixbuf);
  preimage->priv->height = gdk_pixbuf_get_height (pixbuf);

  return preimage;
}

/**
 * games_preimage_set_font_options:
 * @preimage: a #GamesPreimage
 * @font_options: the font options
 *
 * Turns on antialising of @preimage, if it contains an SVG image.
 */
void
games_preimage_set_font_options (GamesPreimage * preimage,
                                 const cairo_font_options_t * font_options)
{
#ifdef HAVE_RSVG
  g_return_if_fail (GAMES_IS_PREIMAGE (preimage));

  if (preimage->priv->font_options) {
    cairo_font_options_destroy (preimage->priv->font_options);
  }

  if (font_options) {
    preimage->priv->font_options = cairo_font_options_copy (font_options);
  } else {
    preimage->priv->font_options = NULL;
  }
#endif /* HAVE_RSVG */
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

  return preimage->priv->scalable;
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

  return preimage->priv->width;
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

  return preimage->priv->height;
}

/**
 * games_preimage_render_unscaled_pixbuf:
 * @preimage:
 *
 * Renders @preimage onto a new #GdkPixbuf at its natural size
 *
 * Returns: (transfer full) (allow-none): a reference to a #GdkPixbuf possibly owned by @images which
 * you must not modify; or %NULL if there was an error
 */
GdkPixbuf *
games_preimage_render_unscaled_pixbuf (GamesPreimage * preimage)
{
  GdkPixbuf *unscaled_pixbuf;

  g_return_val_if_fail (GAMES_IS_PREIMAGE (preimage), NULL);

  if ((unscaled_pixbuf = preimage->priv->pixbuf)) {
    g_object_ref (unscaled_pixbuf);
  } else {
    unscaled_pixbuf = games_preimage_render (preimage,
                                             preimage->priv->width,
                                             preimage->priv->height);
  }

  return unscaled_pixbuf;
}
