/*
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
    
#ifndef GAMES_PIXBUF_UTILS_H
#define GAMES_PIXBUF_UTILS_H

#include <gdk/gdk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

G_BEGIN_DECLS

GdkPixbuf *games_pixbuf_utils_create_highlight (GdkPixbuf *pixbuf,
                                                const GdkColor *highligh_colour);

G_END_DECLS

#endif /* !GAMES_PIXBUF_UTILS_H */
