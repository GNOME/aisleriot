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

#ifndef AR_APPLICATION_H
#define AR_APPLICATION_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define AR_TYPE_APPLICATION               (ar_application_get_type ())
#define AR_APPLICATION(o)                 (G_TYPE_CHECK_INSTANCE_CAST ((o), AR_TYPE_APPLICATION, ArApplication))
#define AR_APPLICATION_CLASS(k)           (G_TYPE_CHECK_CLASS_CAST((k), AR_TYPE_APPLICATION, ArApplicationClass))
#define AISLERIOT_IS_APPLICATION(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), AR_TYPE_APPLICATION))
#define AISLERIOT_IS_APPLICATION_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), AR_TYPE_APPLICATION))
#define AR_APPLICATION_GET_CLASS(o)       (G_TYPE_INSTANCE_GET_CLASS ((o), AR_TYPE_APPLICATION, ArApplicationClass))

typedef struct _ArApplication	     ArApplication;
typedef struct _ArApplicationPrivate ArApplicationPrivate;
typedef struct _ArApplicationClass   ArApplicationClass;

GType ar_application_get_type (void);

GtkApplication *ar_application_new (const char *variation,
                                    gboolean freecell);

G_END_DECLS

#endif /* !AR_APPLICATION_H */
