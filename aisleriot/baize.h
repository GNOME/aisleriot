/*
 * Copyright © 2008 Neil Roberts
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

#ifndef AISLERIOT_BAIZE_H
#define AISLERIOT_BAIZE_H

#include <clutter/clutter.h>

G_BEGIN_DECLS

#define AISLERIOT_TYPE_BAIZE                                            \
  (aisleriot_baize_get_type())
#define AISLERIOT_BAIZE(obj)                                            \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj),                                   \
                               AISLERIOT_TYPE_BAIZE,                    \
                               AisleriotBaize))
#define AISLERIOT_BAIZE_CLASS(klass)                                    \
  (G_TYPE_CHECK_CLASS_CAST ((klass),                                    \
                            AISLERIOT_TYPE_BAIZE,                       \
                            AisleriotBaizeClass))
#define AISLERIOT_IS_BAIZE(obj)                                         \
  (G_TYPE_CHECK_INSTANCE_TYPE ((obj),                                   \
                               AISLERIOT_TYPE_BAIZE))
#define AISLERIOT_IS_BAIZE_CLASS(klass)                                 \
  (G_TYPE_CHECK_CLASS_TYPE ((klass),                                    \
                            AISLERIOT_TYPE_BAIZE))
#define AISLERIOT_BAIZE_GET_CLASS(obj)                                  \
  (G_TYPE_INSTANCE_GET_CLASS ((obj),                                    \
                              AISLERIOT_TYPE_BAIZE,                     \
                              AisleriotBaizeClass))

typedef struct _AisleriotBaize      AisleriotBaize;
typedef struct _AisleriotBaizeClass AisleriotBaizeClass;

struct _AisleriotBaizeClass
{
  ClutterTextureClass parent_class;
};

struct _AisleriotBaize
{
  ClutterTexture parent;
};

GType aisleriot_baize_get_type (void) G_GNUC_CONST;

ClutterActor *aisleriot_baize_new (void);

G_END_DECLS

#endif /* AISLERIOT_BAIZE_H */
