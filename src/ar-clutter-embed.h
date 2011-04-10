/*
 * Copyright © 2009 Christian Persch <chpe@gnome.org>
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

#ifndef __AR_CLUTTER_EMBED_H__
#define __AR_CLUTTER_EMBED_H__

#include <clutter-gtk/clutter-gtk.h>

#include "ar-style.h"
#include "ar-cursor.h"

G_BEGIN_DECLS

#define AR_TYPE_CLUTTER_EMBED            (ar_clutter_embed_get_type())
#define AR_CLUTTER_EMBED(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_CLUTTER_EMBED, ArClutterEmbed))
#define AR_CLUTTER_EMBED_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  AR_TYPE_CLUTTER_EMBED, ArClutterEmbedClass))
#define AR_IS_CLUTTER_EMBED(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_CLUTTER_EMBED))
#define AR_IS_CLUTTER_EMBED_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  AR_TYPE_CLUTTER_EMBED))
#define AR_CLUTTER_EMBED_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  AR_TYPE_CLUTTER_EMBED, ArClutterEmbedClass))

typedef struct _ArClutterEmbed        ArClutterEmbed;
typedef struct _ArClutterEmbedClass   ArClutterEmbedClass;
typedef struct _ArClutterEmbedPrivate ArClutterEmbedPrivate;

struct _ArClutterEmbed
{
  GtkClutterEmbed parent;

  /*< private >*/
  ArClutterEmbedPrivate *priv;
};

struct _ArClutterEmbedClass
{
  GtkClutterEmbedClass parent_class;
};

GType ar_clutter_embed_get_type (void);

ArClutterEmbed* ar_clutter_embed_new (ArStyle *style);

void ar_clutter_embed_set_cursor (ArClutterEmbed *embed,
                                  ArCursorType cursor_type);

G_END_DECLS

#endif /* __AR_CLUTTER_EMBED_H__ */
