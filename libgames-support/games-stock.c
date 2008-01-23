/*
 * Copyright © 2005 Richard Hoelscher
 * Copyright © 2006 Andreas Røsdal
 * Copyright © 2007 Christian Persch
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

#include <config.h>

#include <string.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtkversion.h>

#include "games-files.h"

#include "games-stock.h"

#ifndef HAVE_HILDON

typedef struct {
  const char *stock_id;
  const char *tooltip;
} GamesStockItemTooltip;

static const char *
games_stock_get_tooltip_from_stockid (const char* stockid)
{
  static const GamesStockItemTooltip stock_item_tooltip[] = {
    { GAMES_STOCK_CONTENTS,         N_("View help for this game") },
    { GAMES_STOCK_END_GAME,         N_("End the current game") },
    { GAMES_STOCK_FULLSCREEN,       N_("Toggle fullscreen mode") },
    { GAMES_STOCK_HINT,             N_("Get a hint for your next move") },
    { GAMES_STOCK_LEAVE_FULLSCREEN, N_("Leave fullscreen mode") },
    { GAMES_STOCK_NETWORK_GAME,     N_("Start a new multiplayer network game") },
    { GAMES_STOCK_NETWORK_LEAVE,    N_("End the current network game and return to network server") },
    { GAMES_STOCK_NEW_GAME,         N_("Start a new game") },
    { GAMES_STOCK_PAUSE_GAME,       N_("Pause the game") },
    { GAMES_STOCK_PLAYER_LIST,      N_("Show a list of players in the network game") },
    { GAMES_STOCK_REDO_MOVE,        N_("Redo the undone move") },
    { GAMES_STOCK_RESTART_GAME,     N_("Restart the game") },
    { GAMES_STOCK_RESUME_GAME,      N_("Resume the paused game") },
    { GAMES_STOCK_SCORES,           N_("View the scores") },
    { GAMES_STOCK_UNDO_MOVE,        N_("Undo the last move") },
    { GTK_STOCK_ABOUT,              N_("About this game") },
    { GTK_STOCK_CLOSE,              N_("Close this window") },
    { GTK_STOCK_PREFERENCES,        N_("Configure the game") },
    { GTK_STOCK_QUIT,               N_("Quit this game") },
  };

  guint i;
  const char *tooltip = NULL;

  if (!stockid)
    return NULL;

  for (i = 0; i < G_N_ELEMENTS (stock_item_tooltip); i++) {
    if (strcmp (stock_item_tooltip[i].stock_id, stockid) == 0) {
      tooltip = stock_item_tooltip[i].tooltip;
      break;
    }
  }

  return tooltip ? _(tooltip) : NULL;
}

static void
menu_item_select_cb (GtkWidget * widget, GtkStatusbar * statusbar)
{
  GtkAction *action;
  gchar *tooltip;
  guint context_id;

  context_id = gtk_statusbar_get_context_id (statusbar, "games-tooltip");

#if GTK_CHECK_VERSION (2, 8, 0)
  action = gtk_widget_get_action (widget);
#else
  action = g_object_get_data (G_OBJECT (widget), "gtk-action");
#endif
  g_return_if_fail (action != NULL);

  g_object_get (action, "tooltip", &tooltip, NULL);

  if (tooltip) {
    gtk_statusbar_push (statusbar, context_id, tooltip);
    g_free (tooltip);
  } else {
    const gchar *stock_tip = NULL;
    gchar *stockid;

    g_object_get (action, "stock-id", &stockid, NULL);
    if (stockid != NULL) {
      stock_tip = games_stock_get_tooltip_from_stockid (stockid);
      g_free (stockid);
    }

    if (stock_tip != NULL) {
      gtk_statusbar_push (statusbar, context_id, stock_tip);
    }
  }
}

static void
menu_item_deselect_cb (GtkWidget * widget, GtkStatusbar * statusbar)
{
  guint context_id;

  context_id = gtk_statusbar_get_context_id (statusbar, "games-tooltip");
  gtk_statusbar_pop (statusbar, context_id);
}

static void
connect_proxy_cb (GtkUIManager * ui_manager,
                  GtkAction * action,
                  GtkWidget * proxy, GtkWidget * statusbar)
{
  if (!GTK_IS_MENU_ITEM (proxy))
    return;

  g_signal_connect (proxy, "select",
                    G_CALLBACK (menu_item_select_cb), statusbar);
  g_signal_connect (proxy, "deselect",
                    G_CALLBACK (menu_item_deselect_cb), statusbar);
}

static void
disconnect_proxy_cb (GtkUIManager * ui_manager,
                     GtkAction * action,
                     GtkWidget * proxy, GtkWidget * statusbar)
{
  if (!GTK_IS_MENU_ITEM (proxy))
    return;

  g_signal_handlers_disconnect_by_func
    (proxy, G_CALLBACK (menu_item_select_cb), statusbar);
  g_signal_handlers_disconnect_by_func
    (proxy, G_CALLBACK (menu_item_deselect_cb), statusbar);
}

/** 
 *  Call this once, after creating the UI Manager but before 
 *  you start adding actions. Then, whenever an action is added, 
 *  connect_proxy() will set tooltips to be displayed in the statusbar.
 */
void
games_stock_prepare_for_statusbar_tooltips (GtkUIManager * ui_manager,
                                            GtkWidget * statusbar)
{
  g_signal_connect (ui_manager, "connect-proxy",
                    G_CALLBACK (connect_proxy_cb), statusbar);
  g_signal_connect (ui_manager, "disconnect-proxy",
                    G_CALLBACK (disconnect_proxy_cb), statusbar);
}

typedef struct {
  GtkAction *pause_action;
  GtkAction *resume_action;
} PauseActions;

static void
set_pause_actions (GtkAction * active, PauseActions * actions)
{
  gboolean paused;

  paused = (active == actions->pause_action);
  gtk_action_set_visible (actions->pause_action, !paused);
  gtk_action_set_sensitive (actions->pause_action, !paused);
  gtk_action_set_visible (actions->resume_action, paused);
  gtk_action_set_sensitive (actions->resume_action, paused);
}

void
games_stock_set_pause_actions (GtkAction * pause_action,
                               GtkAction * resume_action)
{
  PauseActions *actions;

  actions = g_new (PauseActions, 1);
  actions->pause_action = pause_action;
  actions->resume_action = resume_action;
  g_signal_connect (pause_action, "activate", G_CALLBACK (set_pause_actions),
                    actions);
  g_signal_connect (resume_action, "activate", G_CALLBACK (set_pause_actions),
                    actions);

  set_pause_actions (resume_action, actions);
}

/* FIXME: This is for non-gtk icons. It only seems to go for the hicolor defaults. */
static void
add_stock_icon (GtkIconFactory * icon_factory,
                const char *stock_id, const char *icon_name)
{
  GtkIconSource *source;
  GtkIconSet *set;

  source = gtk_icon_source_new ();
  set = gtk_icon_set_new ();
  gtk_icon_source_set_icon_name (source, icon_name);

  gtk_icon_set_add_source (set, source);
  gtk_icon_factory_add (icon_factory, stock_id, set);
  gtk_icon_set_unref (set);
  gtk_icon_source_free (source);
}

/* The same routine, but for filenames instead. */
static void
add_icon_from_file (GtkIconFactory * icon_factory,
                    const char *stock_id, const char *filename)
{
  GtkIconSource *source;
  GtkIconSet *set;
  char *path;

  source = gtk_icon_source_new ();
  set = gtk_icon_set_new ();

  path = games_build_filename (ICONDIR, filename);
  gtk_icon_source_set_filename (source, path);
  g_free (path);

  gtk_icon_set_add_source (set, source);
  gtk_icon_factory_add (icon_factory, stock_id, set);
  gtk_icon_set_unref (set);
  gtk_icon_source_free (source);
}

#endif /* !HAVE_HILDON */

void
games_stock_init (void)
{
  /* These stocks have a gtk stock icon */
  const char *stock_item_with_gtk_stock[][2] = {
    { GAMES_STOCK_CONTENTS,         GTK_STOCK_HELP },
    { GAMES_STOCK_HINT,             GTK_STOCK_DIALOG_INFO },
    { GAMES_STOCK_NEW_GAME,         GTK_STOCK_NEW },
    { GAMES_STOCK_REDO_MOVE,        GTK_STOCK_REDO },
    { GAMES_STOCK_RESET,            GTK_STOCK_CLEAR },
    { GAMES_STOCK_RESTART_GAME,     GTK_STOCK_REFRESH },
    { GAMES_STOCK_UNDO_MOVE,        GTK_STOCK_UNDO },
    { GAMES_STOCK_DEAL_CARDS,       GTK_STOCK_OK } /* FIXMEchpe */,
#ifndef HAVE_HILDON
    { GAMES_STOCK_FULLSCREEN,       GTK_STOCK_FULLSCREEN },
    { GAMES_STOCK_LEAVE_FULLSCREEN, GTK_STOCK_LEAVE_FULLSCREEN },
    { GAMES_STOCK_NETWORK_GAME,     GTK_STOCK_NETWORK },
    { GAMES_STOCK_NETWORK_LEAVE,    GTK_STOCK_STOP },
    { GAMES_STOCK_PLAYER_LIST,      GTK_STOCK_INFO },
#endif /* !HAVE_HILDON */
  };

#ifndef HAVE_HILDON
  /* These stocks has a icon name */
  const char *stock_item_with_icon_name[][2] = {
    { GAMES_STOCK_PAUSE_GAME,   "stock_timer_stopped" },
    { GAMES_STOCK_RESUME_GAME,  "stock_timer" },
    { GAMES_STOCK_SCORES,       "stock_scores" },
  };

  /* These stocks are using a private icon file */
  const char *stock_item_with_file[][2] = {
    { GAMES_STOCK_TELEPORT,   "teleport.png" },
    { GAMES_STOCK_RTELEPORT,  "rteleport.png" },
  };
#endif /* !HAVE_HILDON */

/* Use different accels on GTK/GNOME and Maemo */
#ifdef HAVE_HILDON
#define STOCK_ACCEL(normal,hildon) (hildon)
#else
#define STOCK_ACCEL(normal,hildon) (normal)
#endif

  static const GtkStockItem games_stock_items[] = {
    { GAMES_STOCK_CONTENTS,         N_("_Contents"),          0, STOCK_ACCEL (GDK_F1, 0), NULL },
    { GAMES_STOCK_FULLSCREEN,       N_("_Fullscreen"),        0, STOCK_ACCEL (GDK_F11, GDK_F6), NULL },
    { GAMES_STOCK_HINT,             N_("_Hint"),              STOCK_ACCEL (GDK_CONTROL_MASK, 0), STOCK_ACCEL ('h', GDK_Return), NULL },
    /* Translators: This "_New" is for the menu item 'Game->New', implies "New Game" */
    { GAMES_STOCK_NEW_GAME,         N_("_New"),               STOCK_ACCEL (GDK_CONTROL_MASK, 0), STOCK_ACCEL ('n', 0), NULL },
    /* Translators: This "_New Game" is for the game-over dialogue */
    { GAMES_STOCK_START_NEW_GAME,   N_("_New Game"),          0, 0, NULL },
    { GAMES_STOCK_REDO_MOVE,        N_("_Redo Move"),         STOCK_ACCEL (GDK_CONTROL_MASK | GDK_SHIFT_MASK, 0), STOCK_ACCEL ('z', GDK_F7), NULL },
    /* Translators: this is the "Reset" scores button in a scores dialogue */
    { GAMES_STOCK_RESET,            N_("_Reset"),             0, 0, NULL },
    /* Translators: "_Restart" is the menu item 'Game->Restart', implies "Restart Game" */
    { GAMES_STOCK_RESTART_GAME,     N_("_Restart"),           0, 0, NULL },
    { GAMES_STOCK_UNDO_MOVE,        N_("_Undo Move"),         STOCK_ACCEL (GDK_CONTROL_MASK, 0), STOCK_ACCEL ('z', GDK_F8), NULL },
    { GAMES_STOCK_DEAL_CARDS,       N_("_Deal"),              GDK_CONTROL_MASK, 'd', NULL },
#ifndef HAVE_HILDON
    { GAMES_STOCK_LEAVE_FULLSCREEN, N_("_Leave Fullscreen"),  0, GDK_F11, NULL },
    { GAMES_STOCK_NETWORK_GAME,     N_("Network _Game"),      GDK_CONTROL_MASK, 'g', NULL },
    { GAMES_STOCK_NETWORK_LEAVE,    N_("L_eave Game"),        GDK_CONTROL_MASK, 'e', NULL },
    { GAMES_STOCK_PLAYER_LIST,      N_("Player _List"),       GDK_CONTROL_MASK, 'l', NULL },
    { GAMES_STOCK_PAUSE_GAME,       N_("_Pause"),             0, GDK_Pause, NULL },
    { GAMES_STOCK_RESUME_GAME,      N_("Res_ume"),            0, GDK_Pause, NULL },
    { GAMES_STOCK_SCORES,           N_("_Scores"),            0, 0, NULL },
    { GAMES_STOCK_END_GAME,         N_("_End Game"),          0, 0, NULL },
#endif

    /* Work around maemo brokenness wrt. stock item translations */
    /* FIXMEchpe: this only applies to maemo3; should be fixed in maemo4 */
#ifdef HAVE_HILDON
    { GTK_STOCK_ABOUT,              N_("_About"),             0, 0, NULL },
    { GTK_STOCK_CANCEL,             N_("_Cancel"),            0, 0, NULL },
    { GTK_STOCK_CLOSE,              N_("_Close"),             0, 0, NULL },
    { GTK_STOCK_OK,                 N_("_OK"),                0, 0, NULL },
#endif /* HAVE_HILDON */
  };

#undef STOCK_ACCEL

  GtkIconFactory *icon_factory;
  guint i;

  icon_factory = gtk_icon_factory_new ();

  for (i = 0; i < G_N_ELEMENTS (stock_item_with_gtk_stock); ++i) {
    GtkIconSet *icon_set;

    /* FIXME: Only for gtk stock icons.
     * Seems to support theme switching... but not for a11y?
     */
    icon_set =
      gtk_icon_factory_lookup_default (stock_item_with_gtk_stock[i][1]);
    gtk_icon_factory_add (icon_factory, stock_item_with_gtk_stock[i][0],
                          icon_set);
  }

#ifndef HAVE_HILDON /* only need icons on non-hildon */
  for (i = 0; i < G_N_ELEMENTS (stock_item_with_icon_name); ++i) {
    add_stock_icon (icon_factory,
                    stock_item_with_icon_name[i][0],
                    stock_item_with_icon_name[i][1]);
  }

  for (i = 0; i < G_N_ELEMENTS (stock_item_with_file); i++) {
    add_icon_from_file (icon_factory,
                        stock_item_with_file[i][0],
                        stock_item_with_file[i][1]);
  }
#endif /* !HAVE_HILDON */

  gtk_icon_factory_add_default (icon_factory);
  g_object_unref (icon_factory);

  gtk_stock_add_static (games_stock_items, G_N_ELEMENTS (games_stock_items));
}

/* Returns a GPL license string for a specific game. */
gchar *
games_get_license (const gchar * game_name)
{
  gchar *license_trans, *license_str;

  static const char license0[] =
    /* %s is replaced with the name of the game in gnome-games. */
    N_("%s is free software; you can redistribute it and/or modify "
       "it under the terms of the GNU General Public License as published by "
       "the Free Software Foundation; either version 2 of the License, or "
       "(at your option) any later version.");
  static const char license1[] =
    N_("%s is distributed in the hope that it will be useful, "
       "but WITHOUT ANY WARRANTY; without even the implied warranty of "
       "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the "
       "GNU General Public License for more details.");
  static const char license2[] =
    N_("You should have received a copy of the GNU General Public License "
       "along with %s; if not, write to the Free Software Foundation, Inc., "
       "51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA");

  license_trans = g_strjoin ("\n\n", _(license0), _(license1),
                             _(license2), NULL);

#if !GTK_CHECK_VERSION (2, 8, 0)
  /* We have to manually wrap the text, since the about dialogue cannot
   * do it itself before gtk 2.8.
   */
  {
    char *p;
    gsize line_length;

    line_length = 0;
    for (p = license_trans; *p; ++p) {
      if (*p == ' ' && line_length > 42) {
        *p = '\n';
        line_length = 0;
      } else if (*p == '\n') {
        line_length = 0;
      } else {
        ++line_length;
      }
    }
  }
#endif /* ! GTK+ 2.8.0 */

  license_str =
    g_strdup_printf (license_trans, game_name, game_name, game_name);
  g_free (license_trans);

  return license_str;
}
