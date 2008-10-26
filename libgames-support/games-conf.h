/*
 *  Copyright © 2007 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope conf it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef GAMES_CONF_H
#define GAMES_CONF_H

#include <glib.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GAMES_TYPE_CONF		(games_conf_get_type ())
#define GAMES_CONF(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GAMES_TYPE_CONF, GamesConf))
#define GAMES_CONF_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST((k), GAMES_TYPE_CONF, GamesConfClass))
#define GAMES_IS_CONF(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GAMES_TYPE_CONF))
#define GAMES_IS_CONF_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GAMES_TYPE_CONF))
#define GAMES_CONF_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), GAMES_TYPE_CONF, GamesConfClass))

typedef  GObjectClass             GamesConfClass;
typedef struct _GamesConf         GamesConf;
typedef struct _GamesConfPrivate  GamesConfPrivate;

struct _GamesConf {
  GObject parent_instance;

  /*< private > */
  GamesConfPrivate *priv;
};

GType games_conf_get_type (void);

gboolean games_conf_initialise (const char *game_name);

void games_conf_shutdown (void);

GamesConf *games_conf_get_default (void);

char *games_conf_get_string (const char *group, const char *key,
                             GError ** error);

char *games_conf_get_string_with_default (const char *group, const char *key,
                                          const char *def_value);

void games_conf_set_string (const char *group, const char *key,
                            const char *value);

char **games_conf_get_string_list (const char *group, const char *key,
                                   gsize * n_values, GError ** error);

void games_conf_set_string_list (const char *group, const char *key,
                                 const char * const *values, gsize n_values);

int games_conf_get_integer (const char *group, const char *key,
                            GError ** error);

int games_conf_get_integer_with_default (const char *group, const char *key,
                                         int def_value);

void games_conf_set_integer (const char *group, const char *key, int value);

int *games_conf_get_integer_list (const char *group, const char *key,
                                  gsize * n_values, GError ** error);

void games_conf_set_integer_list (const char *group, const char *key,
                                  int *values, gsize n_values);

gboolean games_conf_get_boolean (const char *group, const char *key,
                                 GError ** error);

gboolean games_conf_get_boolean_with_default (const char *group, const char *key,
                                              gboolean def_value);

void games_conf_set_boolean (const char *group, const char *key,
                             gboolean value);

double games_conf_get_double (const char *group, const char *key,
                              GError ** error);

void games_conf_set_double (const char *group, const char *key, double value);

guint games_conf_get_keyval (const char *group, const char *key,
                             GError ** error);

guint games_conf_get_keyval_with_default (const char *group, const char *key,
                                          guint default_keyval);

void games_conf_set_keyval (const char *group, const char *key, guint value);

void games_conf_add_window (GtkWindow *window, const char *group);

G_END_DECLS

#endif /* !GAMES_CONF_H */
