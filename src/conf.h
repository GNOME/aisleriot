/*
 * Copyright © 2007 Christian Persch
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

#ifndef AISLERIOT_CONF_H
#define AISLERIOT_CONF_H

#include <glib.h>
#include "ar-conf.h"

G_BEGIN_DECLS

typedef enum {
  CONF_THEME,
  CONF_VARIATION,
  CONF_RECENT_GAMES,
  CONF_SHOW_TOOLBAR,
  CONF_CLICK_TO_MOVE,
  CONF_SOUND,
  CONF_SHOW_STATUSBAR,
  CONF_ANIMATIONS,
  CONF_STATISTICS /* must be last */
} AisleriotConfKey;

typedef struct {
  guint wins;
  guint total;
  guint best;
  guint worst;
} AisleriotStatistic;

void aisleriot_conf_init (void);

void aisleriot_conf_shutdown (void);

const char *aisleriot_conf_get_key (AisleriotConfKey key);

gboolean aisleriot_conf_get_options (const char *game_module, int *options);

void aisleriot_conf_set_options (const char *game_module, int options);

void aisleriot_conf_get_statistic (const char *game_module,
                                   AisleriotStatistic * statistic);

void aisleriot_conf_set_statistic (const char *game_module,
                                   AisleriotStatistic * statistic);

G_END_DECLS
#endif /* !AISLERIOT_CONF_H */
