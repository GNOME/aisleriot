/*
 * Copyright © 2013 William Jon McCann
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

#ifndef AISLERIOT_APPLICATION_H
#define AISLERIOT_APPLICATION_H

#include <gtk/gtk.h>

#include "game.h"

G_BEGIN_DECLS

#define AISLERIOT_TYPE_APPLICATION         (aisleriot_application_get_type ())
#define AISLERIOT_APPLICATION(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), AISLERIOT_TYPE_APPLICATION, AisleriotApplication))
#define AISLERIOT_APPLICATION_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), AISLERIOT_TYPE_APPLICATION, AisleriotApplicationClass))
#define AISLERIOT_IS_APPLICATION(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), AISLERIOT_TYPE_APPLICATION))
#define AISLERIOT_IS_APPLICATION_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), AISLERIOT_TYPE_APPLICATION))
#define AISLERIOT_APPLICATION_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), AISLERIOT_TYPE_APPLICATION, AisleriotApplicationClass))

typedef struct _AisleriotApplication	     AisleriotApplication;
typedef struct _AisleriotApplicationPrivate  AisleriotApplicationPrivate;

struct _AisleriotApplication {
  GtkApplication parent_instance;

  /*< private >*/
  AisleriotApplicationPrivate *priv;
};

typedef GtkApplicationClass AisleriotApplicationClass;

GType aisleriot_application_get_type (void);

GtkApplication *aisleriot_application_new (void);

G_END_DECLS

#endif /* !AISLERIOT_APPLICATION_H */
