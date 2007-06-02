/*
 *  Copyright © 2007 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope conf it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 *  $Id$
 */

#ifndef AISLERIOT_CONF_H
#define AISLERIOT_CONF_H

#include <glib.h>

G_BEGIN_DECLS

typedef enum {
  CONF_THEME,
  CONF_VARIATION,
  CONF_WIDTH,
  CONF_HEIGHT,
  CONF_SHOW_MAXIMISED,
  CONF_SHOW_FULLSCREEN,
  CONF_RECENT_GAMES,
  CONF_SHOW_TOOLBAR,
  CONF_CLICK_TO_MOVE,
  CONF_STATISTICS
} AisleriotConfKey;

typedef struct {
  guint wins;
  guint total;
  guint best;
  guint worst;
} AisleriotStatistic;

void aisleriot_conf_init (void);

void aisleriot_conf_shutdown (void);

char *aisleriot_conf_get_string (AisleriotConfKey key,
                                 const char *default_value);

void aisleriot_conf_set_string (AisleriotConfKey key, const char *value);

char **aisleriot_conf_get_strings (AisleriotConfKey key, gsize * n_values);

void aisleriot_conf_set_strings (AisleriotConfKey key,
                                 const char *const *values, gsize n_values);

int aisleriot_conf_get_int (AisleriotConfKey key);

void aisleriot_conf_set_int (AisleriotConfKey key, int value);

gboolean aisleriot_conf_get_boolean (AisleriotConfKey key);

void aisleriot_conf_set_boolean (AisleriotConfKey key, gboolean value);

gboolean aisleriot_conf_get_options (const char *game_file, int *options);

void aisleriot_conf_set_options (const char *game_file, int options);

void aisleriot_conf_get_statistic (const char *game_file,
                                   AisleriotStatistic * statistic);

void aisleriot_conf_set_statistic (const char *game_file,
                                   AisleriotStatistic * statistic);

G_END_DECLS
#endif /* !AISLERIOT_CONF_H */
