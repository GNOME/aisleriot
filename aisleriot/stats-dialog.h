/*
 *  Copyright © 2007 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope stats_dialog it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef AISLERIOT_STATS_DIALOG_H
#define AISLERIOT_STATS_DIALOG_H

#include <gtk/gtkdialog.h>

#include "conf.h"

G_BEGIN_DECLS

#define AISLERIOT_TYPE_STATS_DIALOG		(aisleriot_stats_dialog_get_type ())
#define AISLERIOT_STATS_DIALOG(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), AISLERIOT_TYPE_STATS_DIALOG, AisleriotStatsDialog))
#define AISLERIOT_STATS_DIALOG_CLASS(k)		(G_TYPE_CHECK_CLASS_CAST((k), AISLERIOT_TYPE_STATS_DIALOG, AisleriotStatsDialogClass))
#define AISLERIOT_IS_STATS_DIALOG(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), AISLERIOT_TYPE_STATS_DIALOG))
#define AISLERIOT_IS_STATS_DIALOG_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), AISLERIOT_TYPE_STATS_DIALOG))
#define AISLERIOT_STATS_DIALOG_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), AISLERIOT_TYPE_STATS_DIALOG, AisleriotStatsDialogClass))

typedef struct _AisleriotStatsDialog		AisleriotStatsDialog;
typedef struct _AisleriotStatsDialogClass	AisleriotStatsDialogClass;
typedef struct _AisleriotStatsDialogPrivate	AisleriotStatsDialogPrivate;

struct _AisleriotStatsDialog {
  GtkDialog parent_instance;

  /*< private >*/
  AisleriotStatsDialogPrivate *priv;
};

struct _AisleriotStatsDialogClass {
  GtkDialogClass parent_class;

  /* Signals */
};

GType aisleriot_stats_dialog_get_type (void);

AisleriotStatsDialog *aisleriot_stats_dialog_new (void);

void aisleriot_stats_dialog_update (AisleriotStatsDialog * dialog,
                                    AisleriotStatistic * statistic);

void aisleriot_stats_dialog_set_name (AisleriotStatsDialog * dialog,
                                      const char *game_name);

G_END_DECLS

#endif /* !AISLERIOT_STATS_DIALOG_H */
