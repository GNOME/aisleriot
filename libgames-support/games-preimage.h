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

#ifndef GAMES_PREIMAGE_H
#define GAMES_PREIMAGE_H

#include <glib.h>
#include <cairo.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

G_BEGIN_DECLS

#define GAMES_TYPE_PREIMAGE             (games_preimage_get_type ())
#define GAMES_PREIMAGE(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj), GAMES_TYPE_PREIMAGE, GamesPreimage))
#define GAMES_PREIMAGE_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_PREIMAGE, GamesPreimageClass))
#define GAMES_IS_PREIMAGE(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_PREIMAGE))
#define GAMES_IS_PREIMAGE_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_PREIMAGE))
#define GAMES_GET_PREIMAGE_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_PREIMAGE, GamesPreimageClass))

typedef struct _GamesPreimage GamesPreimage;

typedef struct {
  GObjectClass parent_class;
} GamesPreimageClass;

GType games_preimage_get_type (void);

GamesPreimage *games_preimage_new (void);

GamesPreimage *games_preimage_new_from_file (const gchar * filename,
                                             GError ** error);

void games_preimage_set_font_options (GamesPreimage * preimage,
                                      const cairo_font_options_t *font_options);

GdkPixbuf *games_preimage_render (GamesPreimage * preimage,
                                  gint width,
                                  gint height);
void games_preimage_render_cairo (GamesPreimage * preimage,
                                  cairo_t *cr,
                                  gint width,
                                  gint height);

GdkPixbuf *games_preimage_render_sub (GamesPreimage * preimage,
                                      const char *node,
                                      int width,
                                      int height,
                                      double xoffset,
                                      double yoffset,
                                      double xzoom, double yzoom);

void games_preimage_render_cairo_sub (GamesPreimage * preimage,
                                      cairo_t *cr,
                                      const char *node,
                                      int width,
                                      int height,
                                      double xoffset,
                                      double yoffset,
                                      double xzoom,
                                      double yzoom);

gboolean games_preimage_is_scalable (GamesPreimage * preimage);

gint games_preimage_get_width (GamesPreimage * preimage);

gint games_preimage_get_height (GamesPreimage * preimage);

GdkPixbuf *games_preimage_render_unscaled_pixbuf (GamesPreimage * preimage);

G_END_DECLS

#endif /* GAMES_PREIMAGE_H */
