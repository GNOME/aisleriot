/*
 * games-stock.c: games stock items and icon registation
 *
 * Copyright (C) 2005 Richard Hoelscher
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * Authors:
 *      Richard Hoelscher <rah@rahga.com>
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n-lib.h>
#include <gdk/gdkkeysyms.h>

#include "games-stock.h"

static GtkIconFactory *games_icon_factory = NULL;

/* FIXME: Bug #166480, gtk+ will ship with "fullscreen" stock icons eventually. */
/* FIXME: Remove FIXMEs if I find a decent way to completely abandon gtk+ stock icons. */

static GtkStockItem games_stock_items[] =
{
  { GAMES_STOCK_NEW_GAME, N_("_New"), GDK_CONTROL_MASK, 'n', NULL },
  { GAMES_STOCK_PAUSE_GAME, N_("_Pause"), 0, GDK_Pause, NULL },
  { GAMES_STOCK_RESTART_GAME, N_("_Restart"), 0, 0, NULL },
  { GAMES_STOCK_UNDO_MOVE, N_("_Undo Move"), GDK_CONTROL_MASK, 'z', NULL },
  { GAMES_STOCK_REDO_MOVE, N_("_Redo Move"), GDK_CONTROL_MASK | GDK_SHIFT_MASK, 'z', NULL },
  { GAMES_STOCK_HINT, N_("_Hint"), GDK_CONTROL_MASK, 'h', NULL },
  { GAMES_STOCK_SCORES, N_("_Scores"), 0, 0, NULL },
  { GAMES_STOCK_FULLSCREEN, N_("_Fullscreen"), 0, GDK_F11, NULL },
  { GAMES_STOCK_LEAVE_FULLSCREEN, N_("_Leave Fullscreen"), 0, GDK_F11, NULL },
  { GAMES_STOCK_END_GAME, N_("_End Game"), 0, 0, NULL },
  { GAMES_STOCK_CONTENTS, N_("_Contents"), 0, GDK_F1, NULL }
};

typedef struct {
  char *stock_id;
  char *from_data;
  char *from_gtk_stock;
} GamesStockItemIcon;

/* Names of stock intems installed by gtk+ and gnome-icon-theme */
static GamesStockItemIcon stock_item_icon[] = {
  { GAMES_STOCK_NEW_GAME,     NULL,         GTK_STOCK_NEW },
  { GAMES_STOCK_PAUSE_GAME,   "stock_timer_stopped",   NULL },
  { GAMES_STOCK_RESTART_GAME, NULL,         GTK_STOCK_REFRESH },
  { GAMES_STOCK_UNDO_MOVE,    NULL ,        GTK_STOCK_UNDO },
  { GAMES_STOCK_REDO_MOVE,    NULL,         GTK_STOCK_REDO },
  { GAMES_STOCK_HINT,         NULL,         GTK_STOCK_DIALOG_INFO },
  { GAMES_STOCK_SCORES,       "stock_scores", NULL },
  { GAMES_STOCK_FULLSCREEN,   "stock_fullscreen", NULL },
  { GAMES_STOCK_LEAVE_FULLSCREEN,   "stock_leave-fullscreen", NULL },
  { GAMES_STOCK_CONTENTS,     NULL,         GTK_STOCK_HELP}
};

/* FIXME: This is for non-gtk icons. It only seems to go for the hicolor defaults. */
static void
add_stock_icon (const gchar  *stock_id,
                char *from_data)
{
  GtkIconSource *source;
  GtkIconSet    *set;

  source = gtk_icon_source_new ();
  set = gtk_icon_set_new ();
  gtk_icon_source_set_icon_name (source, from_data);

  gtk_icon_set_add_source (set, source);
  gtk_icon_factory_add (games_icon_factory, stock_id, set);
  gtk_icon_set_unref (set);
  gtk_icon_source_free (source);
}

void
games_stock_init (void)
{
  int i;

  games_icon_factory = gtk_icon_factory_new ();

  for (i = 0; i < G_N_ELEMENTS (stock_item_icon); i++) {
    GtkIconSet *icon_set;

  if (stock_item_icon[i].from_data) 
        /* FIXME: Only for non-gtk icons. See above. */
      add_stock_icon (stock_item_icon[i].stock_id, stock_item_icon[i].from_data);
  else {
	/* FIXME: Only for gtk stock icons. 
         * Seems to support theme switching... but not for a11y? */
      icon_set = gtk_icon_factory_lookup_default (stock_item_icon[i].from_gtk_stock);
      gtk_icon_factory_add (games_icon_factory, stock_item_icon[i].stock_id, icon_set);
    }

  }

  gtk_icon_factory_add_default (games_icon_factory);

  gtk_stock_add_static (games_stock_items, G_N_ELEMENTS (games_stock_items));
}
