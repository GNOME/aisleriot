/*
 *  Copyright © 2007 Christian Persch
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

#ifndef AR_CONF_H
#define AR_CONF_H

#include <glib.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define AR_TYPE_CONF		(ar_conf_get_type ())
#define AR_CONF(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), AR_TYPE_CONF, ArConf))
#define AR_CONF_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST((k), AR_TYPE_CONF, ArConfClass))
#define AR_IS_CONF(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), AR_TYPE_CONF))
#define AR_IS_CONF_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), AR_TYPE_CONF))
#define AR_CONF_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), AR_TYPE_CONF, ArConfClass))

typedef struct ArConfPrivate  ArConfPrivate;

typedef struct {
  GObject parent_instance;
  /*< private >*/
  ArConfPrivate *priv;
} ArConf;

typedef struct {
  GObjectClass parent_class;  
} ArConfClass;

GType      ar_conf_get_type                 (void);
gboolean   ar_conf_initialise               (const char *game_name);
void       ar_conf_shutdown                 (void);
ArConf *ar_conf_get_default              (void);
void       ar_conf_save                     (void);
char      *ar_conf_get_string               (const char *group,
                                                const char *key,
                                                GError ** error);
char      *ar_conf_get_string_with_default  (const char *group,
                                                const char *key,
                                                const char *def_value);
void       ar_conf_set_string               (const char *group,
                                                const char *key,
                                                const char *value);
char     **ar_conf_get_string_list          (const char *group,
                                                const char *key,
                                                gsize * n_values,
                                                GError ** error);
void       ar_conf_set_string_list          (const char *group,
                                                const char *key,
                                                const char * const *values,
                                                gsize n_values);
int        ar_conf_get_integer              (const char *group,
                                                const char *key,
                                                GError ** error);
int        ar_conf_get_integer_with_default (const char *group,
                                                const char *key,
                                                int def_value);
void       ar_conf_set_integer              (const char *group,
                                                const char *key,
                                                int value);
int       *ar_conf_get_integer_list         (const char *group,
                                                const char *key,
                                                gsize * n_values,
                                                GError ** error);
void       ar_conf_set_integer_list         (const char *group,
                                                const char *key,
                                                int *values,
                                                gsize n_values);
gboolean   ar_conf_get_boolean              (const char *group,
                                                const char *key,
                                                GError ** error);
gboolean   ar_conf_get_boolean_with_default (const char *group,
                                                const char *key,
                                                gboolean def_value);
void       ar_conf_set_boolean              (const char *group,
                                                const char *key,
                                                gboolean value);
double     ar_conf_get_double               (const char *group,
                                                const char *key,
                                                GError ** error);
void       ar_conf_set_double               (const char *group,
                                                const char *key,
                                                double value);
guint      ar_conf_get_keyval               (const char *group,
                                                const char *key,
                                                GError ** error);
guint      ar_conf_get_keyval_with_default  (const char *group,
                                                const char *key,
                                                guint default_keyval);
void       ar_conf_set_keyval               (const char *group,
                                                const char *key,
                                                guint value);
void       ar_conf_add_window               (GtkWindow *window,
                                                const char *group);

G_END_DECLS

#endif /* !AR_CONF_H */
