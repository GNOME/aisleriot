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

#pragma once

#include <gtk/gtk.h>
#include "conf.h"

G_BEGIN_DECLS

#define AISLERIOT_TYPE_STATS_DIALOG		(aisleriot_stats_dialog_get_type ())
G_DECLARE_FINAL_TYPE(AisleriotStatsDialog, aisleriot_stats_dialog, AISLERIOT, STATS_DIALOG, GtkDialog);


GType                 aisleriot_stats_dialog_get_type (void);
AisleriotStatsDialog *aisleriot_stats_dialog_new      (void);
void                  aisleriot_stats_dialog_update   (AisleriotStatsDialog *dialog,
                                                       AisleriotStatistic   *statistic);
void                  aisleriot_stats_dialog_set_name (AisleriotStatsDialog *dialog,
                                                       const char           *game_name);


G_END_DECLS
