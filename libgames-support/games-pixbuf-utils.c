/*
  Copyright © 1998, 2003 Jonathan Blandford <jrb@mit.edu>
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
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <string.h>

#include "games-pixbuf-utils.h"

/**
 * games_pixbuf_utils_create_highlight:
 * @pixbuf: the original pixbuf
 * @highligh_colour: the colour to use for the highlight
 *
 * Creates a highlighted pixbuf from @pixbuf using @highligh_colour.
 *
 * Returns: a new #GdkPixbuf, or %NULL if there was an error
 */
GdkPixbuf *
games_pixbuf_utils_create_highlight (GdkPixbuf *pixbuf,
                                     const GdkColor *highligh_colour)
{
  GdkPixbuf *comppixbuf;
  guint x, y, width, height, rowstride;
  guint16 red, green, blue;
  guchar *pixels, *p;

  if (!pixbuf)
    return NULL;

  comppixbuf = gdk_pixbuf_new (gdk_pixbuf_get_colorspace (pixbuf),
                               gdk_pixbuf_get_has_alpha (pixbuf),
                               gdk_pixbuf_get_bits_per_sample (pixbuf),
                               gdk_pixbuf_get_width (pixbuf),
                               gdk_pixbuf_get_height (pixbuf));
  if (!comppixbuf)
    return NULL;

  gdk_pixbuf_saturate_and_pixelate (pixbuf, comppixbuf, 0.0, FALSE);

  pixels = gdk_pixbuf_get_pixels (comppixbuf);
  width = gdk_pixbuf_get_width (comppixbuf);
  height = gdk_pixbuf_get_height (comppixbuf);
  rowstride = gdk_pixbuf_get_rowstride (comppixbuf);

  red = highligh_colour->red >> 8;
  green = highligh_colour->green >> 8;
  blue = highligh_colour->blue >> 8;

  /* FIXMEchpe: This could probably use lots of improvements! */
  for (y = 0; y < height; y++) {
    for (x = 0; x < width; x++) {
      p = pixels + y * rowstride + x * 4;
      p[0] += (0xFF - p[0]) * red / 0xFF;
      p[1] += (0xFF - p[1]) * green / 0xFF;
      p[2] += (0xFF - p[2]) * blue / 0xFF;
    }
  }

  return comppixbuf;
}
