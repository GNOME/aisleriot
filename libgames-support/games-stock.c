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

#include <string.h>
#include <gtk/gtk.h>
#include <glib/gi18n-lib.h>
#include <gdk/gdkkeysyms.h>

#include "games-stock.h"

static GtkIconFactory *games_icon_factory = NULL;

/* FIXME: Bug #166480, gtk+ will ship with "fullscreen" stock icons eventually. */
/* FIXME: Remove FIXMEs if I find a decent way to completely abandon gtk+ stock icons. */

static GtkStockItem games_stock_items[] =
{
/* i18n: This "_New" is for the menu item 'Game->New', implies "New Game" */
  { GAMES_STOCK_NEW_GAME, N_("_New"), GDK_CONTROL_MASK, 'n', NULL },
  { GAMES_STOCK_PAUSE_GAME, N_("_Pause"), 0, GDK_Pause, NULL },
  { GAMES_STOCK_RESUME_GAME, N_("Res_ume"), 0, GDK_Pause, NULL },
/* i18n: "_Restart" is the menu item 'Game->Restart', implies "Restart Game" */
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
  char *filename;
} GamesStockItemIcon;

/* Names of stock intems installed by gtk+ and gnome-icon-theme */
static GamesStockItemIcon stock_item_icon[] = {
  { GAMES_STOCK_NEW_GAME,         NULL,                     GTK_STOCK_NEW, 
                                  NULL },
  { GAMES_STOCK_PAUSE_GAME,       "stock_timer_stopped",    NULL,
                                  NULL },
  { GAMES_STOCK_RESUME_GAME,      "stock_timer",            NULL,
                                  NULL },
  { GAMES_STOCK_RESTART_GAME,     NULL,                     GTK_STOCK_REFRESH,
                                  NULL },
  { GAMES_STOCK_UNDO_MOVE,        NULL ,                    GTK_STOCK_UNDO,
                                  NULL },
  { GAMES_STOCK_REDO_MOVE,        NULL,                     GTK_STOCK_REDO,
                                  NULL },
  { GAMES_STOCK_HINT,             NULL,                     GTK_STOCK_DIALOG_INFO,
                                  NULL },
  { GAMES_STOCK_SCORES,           "stock_scores",           NULL,
                                  NULL },
  { GAMES_STOCK_FULLSCREEN,       "stock_fullscreen",       NULL,
                                  NULL },
  { GAMES_STOCK_LEAVE_FULLSCREEN, "stock_leave-fullscreen", NULL,
                                  NULL },
  { GAMES_STOCK_CONTENTS,         NULL,                     GTK_STOCK_HELP,
                                  NULL },
  { GAMES_STOCK_TELEPORT,         NULL,                     NULL,
                                  DATADIR "/pixmaps/teleport.png" },
  { GAMES_STOCK_RTELEPORT,        NULL,                     NULL,
                                  DATADIR "/pixmaps/rteleport.png" },
};

typedef struct {
  gchar *stock_id;
  gchar *tooltip;
} GamesStockItemTooltip;

static GamesStockItemTooltip stock_item_tooltip[] = {
  { GAMES_STOCK_NEW_GAME, N_("Start a new game") },
  { GAMES_STOCK_PAUSE_GAME, N_("Pause the game") },
  { GAMES_STOCK_RESUME_GAME, N_("Resume the paused game") },
  { GAMES_STOCK_RESTART_GAME, N_("Restart the game") },
  { GAMES_STOCK_UNDO_MOVE, N_("Undo the last move") }, 
  { GAMES_STOCK_REDO_MOVE, N_("Redo the undone move") },
  { GAMES_STOCK_HINT, N_("Get a hint for your next move") },
  { GAMES_STOCK_SCORES, N_("View the scores") },
  { GAMES_STOCK_FULLSCREEN, N_("Enter fullscreen mode") },
  { GAMES_STOCK_LEAVE_FULLSCREEN, N_("Leave fullscreen mode") },
  { GAMES_STOCK_END_GAME, N_("End the current game") },
  { GAMES_STOCK_CONTENTS, N_("View help for this game") },
  { GTK_STOCK_QUIT, N_("Quit this game") },
  { GTK_STOCK_ABOUT, N_("About this game") },
  { GTK_STOCK_PREFERENCES, N_("Configure the game") }
};


typedef struct {
  GtkWidget *statusbar;
  gchar     *tooltip;
} StatusTip;

static gchar * 
games_stock_get_tooltip_from_stockid(gchar *stockid) {
  gint i;
  gchar *tooltip = NULL;

  if (!stockid)
    return NULL;

  for (i = 0; i < G_N_ELEMENTS (stock_item_tooltip); i++) {	
    if (strcmp (stock_item_tooltip[i].stock_id, stockid) == 0)
      tooltip = stock_item_tooltip[i].tooltip;
  }

  return tooltip;
}


static void
set_statusbar_tooltip (GtkWidget *widget, StatusTip *data)
{
  guint context_id;

  context_id = gtk_statusbar_get_context_id (GTK_STATUSBAR(data->statusbar), "games_stock_tooltip");
  gtk_statusbar_push (GTK_STATUSBAR (data->statusbar), context_id,
                      data->tooltip ? _(data->tooltip): "");
}


static void
unset_statusbar_tooltip (GtkWidget *widget, StatusTip *data)
{
  guint context_id;

  context_id = gtk_statusbar_get_context_id (GTK_STATUSBAR (data->statusbar), "games_stock_tooltip");
  gtk_statusbar_pop (GTK_STATUSBAR (data->statusbar), context_id);
}


static void
connect_proxy (GtkUIManager *ui_manager,
               GtkAction    *action,
               GtkWidget    *proxy,
               GtkWidget    *statusbar)
{
  gchar *tooltip;

  if (!GTK_IS_MENU_ITEM (proxy))
    return;

  g_object_get (G_OBJECT(action), "tooltip", &tooltip, NULL);

  if (!tooltip) {
    gchar *stockid;

    g_object_get (G_OBJECT(action), "stock-id", &stockid, NULL);
    if (stockid)
      tooltip = games_stock_get_tooltip_from_stockid (stockid);
    g_free (stockid);
  }

  if (tooltip){
    StatusTip *statustip;

    statustip = g_new (StatusTip, 1);
    statustip->statusbar = g_object_ref(statusbar);
    statustip->tooltip = tooltip;

    g_signal_connect (proxy, "select",
		      G_CALLBACK (set_statusbar_tooltip), statustip);
    g_signal_connect (proxy, "deselect",
		      G_CALLBACK (unset_statusbar_tooltip), statustip);
  }
}


/** 
 *  Call this once, after creating the UI Manager but before 
 *  you start adding actions. Then, whenever an action is added, 
 *  connect_proxy() will set tooltips to be displayed in the statusbar.
 */
void
games_stock_prepare_for_statusbar_tooltips (GtkUIManager *ui_manager, 
					    GtkWidget *statusbar)
{
  g_signal_connect (ui_manager, "connect-proxy", G_CALLBACK (connect_proxy), statusbar);
}

typedef struct {
  GtkAction *pause_action;
  GtkAction *resume_action;
} PauseActions;

static void
set_pause_actions (GtkAction *active, PauseActions *actions)
{
  gboolean paused;

  paused = (active == actions->pause_action);
  gtk_action_set_visible (actions->pause_action, !paused);
  gtk_action_set_sensitive (actions->pause_action, !paused);
  gtk_action_set_visible (actions->resume_action, paused);
  gtk_action_set_sensitive (actions->resume_action, paused);
}

void
games_stock_set_pause_actions (GtkAction *pause_action, GtkAction *resume_action) 
{
  PauseActions *actions;

  actions = g_new (PauseActions, 1);
  actions->pause_action = pause_action;
  actions->resume_action = resume_action;
  g_signal_connect (pause_action, "activate", G_CALLBACK (set_pause_actions), actions);
  g_signal_connect (resume_action, "activate", G_CALLBACK (set_pause_actions), actions);

  set_pause_actions (resume_action, actions);
}

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

/* The same routine, but for filenames instead. */
static void
add_icon_from_file (const gchar *stock_id, char *filename)
{
  GtkIconSource *source;
  GtkIconSet    *set;

  source = gtk_icon_source_new ();
  set = gtk_icon_set_new ();

  gtk_icon_source_set_filename (source, filename);

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

    if (stock_item_icon[i].filename)
      add_icon_from_file (stock_item_icon[i].stock_id, 
			  stock_item_icon[i].filename);
    else if (stock_item_icon[i].from_data) 
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
