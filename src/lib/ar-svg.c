/*
 * Copyright © 2004 Richard Hoelscher
 * Copyright © 2007, 2011 Christian Persch
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

#include <config.h>

#include <string.h>

#include <glib.h>
#include <gio/gio.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

/* For gdkcairo */
#include <gdk/gdk.h>

#include <librsvg/rsvg.h>

#include "ar-profile.h"

#include "ar-svg.h"

struct _ArSvgClass {
  RsvgHandleClass parent_class;
};

struct _ArSvg {
  RsvgHandle parent_instance;

  cairo_font_options_t *font_options;
  GFile *file;

  gint width;
  gint height;
};

enum {
  PROP_0,
  PROP_FILE
};

static void ar_svg_initable_iface_init (GInitableIface *iface);

G_DEFINE_TYPE_WITH_CODE (ArSvg, ar_svg, RSVG_TYPE_HANDLE,
                         G_IMPLEMENT_INTERFACE (G_TYPE_INITABLE, ar_svg_initable_iface_init))

static void
ar_svg_init (ArSvg *svg)
{
  svg->width = 0;
  svg->height = 0;
}

static gboolean
ar_svg_initable_init (GInitable *initable,
                      GCancellable *cancellable,
                      GError **error)
{
  ArSvg *svg = AR_SVG (initable);
  RsvgHandle *handle = RSVG_HANDLE (initable);
  GFileInfo *info;
  const char *type;
  char *gz_type;
  GInputStream *stream;
  gboolean is_gzip;
  gboolean retval = FALSE;
  gdouble width = 0;
  gdouble height = 0;

//   ar_profilestart ("creating ArSvg from %s", svg->filename);

  if (!(info = g_file_query_info (svg->file,
                                  G_FILE_ATTRIBUTE_STANDARD_FAST_CONTENT_TYPE,
                                  G_FILE_QUERY_INFO_NONE,
                                  cancellable,
                                  error)))
    goto out;

  type = g_file_info_get_attribute_string (info, G_FILE_ATTRIBUTE_STANDARD_FAST_CONTENT_TYPE);
  gz_type = g_content_type_from_mime_type ("application/x-gzip");
  is_gzip = (type != NULL && g_content_type_is_a (type, gz_type));
  g_free (gz_type);
  g_object_unref (info);

  if (!(stream = G_INPUT_STREAM (g_file_read (svg->file, cancellable, error))))
    goto out;

  if (is_gzip) {
    GZlibDecompressor *decompressor;
    GInputStream *converter_stream;

    decompressor = g_zlib_decompressor_new (G_ZLIB_COMPRESSOR_FORMAT_GZIP);
    converter_stream = g_converter_input_stream_new (stream,
                                                     G_CONVERTER (decompressor));
    g_object_unref (stream);
    stream = converter_stream;
  }

  if (!rsvg_handle_read_stream_sync (handle, stream, cancellable, error)) {
    g_object_unref (stream);
    goto out;
  }
  g_object_unref (stream);

  rsvg_handle_get_intrinsic_size_in_pixels (handle, &width, &height);
  if (width == 0 || height == 0) {
    g_set_error_literal (error,
                         GDK_PIXBUF_ERROR,
                         GDK_PIXBUF_ERROR_FAILED,
                         "Image has zero extent");
    goto out;
  }

  svg->width = (gint) width;
  svg->height = (gint) height;

  retval = TRUE;

out:

//   ar_profileend ("creating ArSvg from %s", svg->filename);
  return retval;
}

static void
ar_svg_finalize (GObject * object)
{
  ArSvg *svg = AR_SVG (object);

  if (svg->file)
    g_object_unref (svg->file);
  if (svg->font_options) {
    cairo_font_options_destroy (svg->font_options);
  }

  G_OBJECT_CLASS (ar_svg_parent_class)->finalize (object);
}

static void
ar_svg_set_property (GObject      *object,
                     guint         property_id,
                     const GValue *value,
                     GParamSpec   *pspec)
{
  ArSvg *svg = AR_SVG (object);

  switch (property_id) {
    case PROP_FILE:
      svg->file = g_value_dup_object (value);
      if (svg->file)
        rsvg_handle_set_base_gfile (RSVG_HANDLE (svg), svg->file);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
ar_svg_class_init (ArSvgClass * klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->set_property = ar_svg_set_property;
  object_class->finalize = ar_svg_finalize;

  g_object_class_install_property
    (object_class,
     PROP_FILE,
     g_param_spec_object ("file", NULL, NULL,
                          G_TYPE_FILE,
                          G_PARAM_WRITABLE |
                          G_PARAM_CONSTRUCT_ONLY |
                          G_PARAM_STATIC_STRINGS));
}

static void
ar_svg_initable_iface_init (GInitableIface *iface)
{
  iface->init = ar_svg_initable_init;
}

/**
 * ar_svg_render_cairo:
 * @svg: a #ArSvg
 * @surface: a #cairo_surface_t
 * @width: the desired width
 * @height: the desired height
 *
 * Paints the SVG at the specified @width and @height to @cr.
 **/
void
ar_svg_render_cairo (ArSvg *svg,
                     cairo_surface_t *surface,
                             gint width,
                             gint height)
{
  g_return_if_fail (width > 0 && height > 0);

    ar_svg_render_cairo_sub (svg,
                                     surface,
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
 * @svg: a #ArSvg
 * @surface: a #cairo_surface_t
 * @node: (allow-none): a SVG node ID (starting with "#"), or %NULL
 * @width: the width of the clip region
 * @height: the height of the clip region
 * @xoffset: the x offset of the clip region
 * @yoffset: the y offset of the clip region
 * @xzoom: the x zoom factor
 * @yzoom: the y zoom factor
 *
 * Paints the SVG element @node to @cr, transformed by @xzoom, @yzoom
 * and offset by @xoffset and @yoffset, clipped to @width and @height.
 *
 * If @node is NULL, the whole image is rendered into tha clip region.
 **/
void
ar_svg_render_cairo_sub (ArSvg *svg,
                         cairo_surface_t *surface,
                                 const char *node,
                                 int width,
                                 int height,
                                 double xoffset,
                                 double yoffset,
                                 double xzoom,
                                 double yzoom)
{
  cairo_t *cr;
  cairo_matrix_t matrix;

  g_return_if_fail (AR_IS_SVG (svg));

  cr = cairo_create (surface);

  if (svg->font_options) {
    cairo_set_antialias (cr, cairo_font_options_get_antialias (svg->font_options));

    cairo_set_font_options (cr, svg->font_options);
  }

  cairo_matrix_init_identity (&matrix);
  cairo_matrix_scale (&matrix, xzoom, yzoom);
  cairo_matrix_translate (&matrix, xoffset, yoffset);

  cairo_set_matrix (cr, &matrix);

  rsvg_handle_render_cairo_sub (RSVG_HANDLE (svg), cr, node);

  cairo_destroy (cr);
}

/**
 * ar_svg_new_from_uri_sync:
 * @file: an URI
 * @cancellable: (allow-none): a #GCancellable, or %NULL
 * @error: (allow-none): a location to store a #GError, or %NULL
 *
 * Creates a new #ArSvg and synchronously loads its contents from @uri.
 *
 * Returns: (allow-none): a new #ArSvg, or %NULL on error with @error
 *   filled in
 */
ArSvg *
ar_svg_new_from_uri_sync (const char *uri,
                          GCancellable *cancellable,
                          GError **error)
{
  GFile *file;
  ArSvg *svg;

  g_return_val_if_fail (uri != NULL, NULL);

  file = g_file_new_for_uri (uri);
  svg = ar_svg_new_from_gfile_sync (file, cancellable, error);
  g_object_unref (file);

  return svg;
}


/**
 * ar_svg_new_from_gfile_sync:
 * @file: a #GFile
 * @cancellable: (allow-none): a #GCancellable, or %NULL
 * @error: (allow-none): a location to store a #GError, or %NULL
 *
 * Creates a new #ArSvg and synchronously loads its contents from @file.
 *
 * Returns: (allow-none): a new #ArSvg, or %NULL on error with @error
 *   filled in
 */
ArSvg *
ar_svg_new_from_gfile_sync (GFile *file,
                            GCancellable *cancellable,
                            GError ** error)
{
  g_return_val_if_fail (G_IS_FILE (file), NULL);
  g_return_val_if_fail (cancellable == NULL || G_IS_CANCELLABLE (cancellable), NULL);
  g_return_val_if_fail (error == NULL || *error == NULL, NULL);

  return g_initable_new (AR_TYPE_SVG,
                         cancellable,
                         error,
                         "file", file,
#if LIBRSVG_CHECK_VERSION (2, 40, 3)
                         "flags", RSVG_HANDLE_FLAG_UNLIMITED,
#endif
                         NULL);
}

/**
 * ar_svg_new_from_filename:
 * @filename: the path to the SVG file
 * @cancellable: (allow-none): a #GCancellable, or %NULL
 * @error: (allow-none): a location to store a #GError, or %NULL
 *
 * Creates a new #ArSvg and synchronously loads its contents from @filename.
 *
 * Returns: (allow-none): a new #ArSvg, or %NULL on error with @error
 *   filled in
 */
ArSvg *
ar_svg_new_from_filename_sync (const char *filename,
                               GCancellable *cancellable,
                               GError ** error)
{
  GFile *file;
  ArSvg *svg;

  g_return_val_if_fail (filename != NULL, NULL);

  file = g_file_new_for_path (filename);
  svg = ar_svg_new_from_gfile_sync (file, cancellable, error);
  g_object_unref (file);

  return svg;
}

/**
 * ar_svg_get_font_options:
 * @svg: a #ArSvg
 *
 * Returns the font options set in @svg.
 * 
 * Retursn: (transfer none): a #cairo_font_options_t, or %NULL
 */
cairo_font_options_t *
ar_svg_get_font_options (ArSvg *svg)
{
  g_return_val_if_fail (AR_IS_SVG (svg), NULL);

  return svg->font_options;
}

/**
 * ar_svg_set_font_options:
 * @svg: a #ArSvg
 * @font_options: (allow-none): a #cairo_font_options_t, or %NULL
 *
 * Sets font options to use when painting to cairo using
 * ar_svg_render_cairo_sub(). Use %NULL to unset previously set
 * font options.
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
 * @svg: a #ArSvg
 *
 * Returns: the natural width of the image in @svg
 */
gint
ar_svg_get_width (ArSvg *svg)
{
  g_return_val_if_fail (AR_IS_SVG (svg), 0);

  return svg->width;
}

/**
 * ar_svg_get_height:
 * @svg: a #ArSvg
 *
 * Returns: the natural height of the image in @svg
 */
gint
ar_svg_get_height (ArSvg *svg)
{
  g_return_val_if_fail (AR_IS_SVG (svg), 0);

  return svg->height;
}
