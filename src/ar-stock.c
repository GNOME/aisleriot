/*
 * Copyright © 2005 Richard Hoelscher
 * Copyright © 2006 Andreas Røsdal
 * Copyright © 2007 Christian Persch
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Authors:
 *      Richard Hoelscher <rah@rahga.com>
 */

#include <config.h>

#include <string.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "ar-runtime.h"

#include "ar-stock.h"

/* FIXME(porting): Setting this to 1 will turn on the old stock icons,
 * at the expense of lots of disabled warnings. Most of these items
 * will go away once we port to GTK4.
 */
#define ENABLE_STOCK 1

static void
menu_item_select_cb (GtkWidget * widget, GtkStatusbar * statusbar)
{
  GtkAction *action;
  gchar *tooltip;
  guint context_id;

  context_id = gtk_statusbar_get_context_id (statusbar, "games-tooltip");

  action = gtk_activatable_get_related_action (GTK_ACTIVATABLE (widget));
  g_return_if_fail (action != NULL);

  g_object_get (action, "tooltip", &tooltip, NULL);

  if (tooltip) {
    gtk_statusbar_push (statusbar, context_id, tooltip);
    g_free (tooltip);
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
ar_stock_prepare_for_statusbar_tooltips (GtkUIManager * ui_manager,
                                            GtkWidget * statusbar)
{
  g_signal_connect (ui_manager, "connect-proxy",
                    G_CALLBACK (connect_proxy_cb), statusbar);
  g_signal_connect (ui_manager, "disconnect-proxy",
                    G_CALLBACK (disconnect_proxy_cb), statusbar);
}

/* This will become GTK_CHECK_VERSION (2, 15, x) once the patch from gtk+ bug 511332 is committed */
#undef HAVE_GTK_ICON_FACTORY_ADD_ALIAS

#ifndef HAVE_GTK_ICON_FACTORY_ADD_ALIAS

static void
register_stock_icon (GtkIconFactory * icon_factory,
                     const char * stock_id,
                     const char * icon_name)
{
  GtkIconSource *source;
  GtkIconSet *set;

  set = gtk_icon_set_new ();

  source = gtk_icon_source_new ();
  gtk_icon_source_set_icon_name (source, icon_name);
  gtk_icon_set_add_source (set, source);
  gtk_icon_source_free (source);

  gtk_icon_factory_add (icon_factory, stock_id, set);
  gtk_icon_set_unref (set);
}

static void
register_stock_icon_bidi (GtkIconFactory * icon_factory,
                          const char * stock_id,
                          const char * icon_name_ltr,
                          const char * icon_name_rtl)
{
  GtkIconSource *source;
  GtkIconSet *set;

  set = gtk_icon_set_new ();

  source = gtk_icon_source_new ();
  gtk_icon_source_set_icon_name (source, icon_name_ltr);
  gtk_icon_source_set_direction (source, GTK_TEXT_DIR_LTR);
  gtk_icon_source_set_direction_wildcarded (source, FALSE);
  gtk_icon_set_add_source (set, source);
  gtk_icon_source_free (source);

  source = gtk_icon_source_new ();
  gtk_icon_source_set_icon_name (source, icon_name_rtl);
  gtk_icon_source_set_direction (source, GTK_TEXT_DIR_RTL);
  gtk_icon_source_set_direction_wildcarded (source, FALSE);
  gtk_icon_set_add_source (set, source);
  gtk_icon_source_free (source);

  gtk_icon_factory_add (icon_factory, stock_id, set);
  gtk_icon_set_unref (set);
}

#endif /* HAVE_GTK_ICON_FACTORY_ADD_ALIAS */

void
ar_stock_init (void)
{
  /* These stocks have a gtk stock icon */
  const char *stock_icon_aliases[][2] = {
    { AR_STOCK_CONTENTS,         "help-contents" },
    { AR_STOCK_HINT,             "dialog-information" },
    { AR_STOCK_NEW_GAME,         "document-new" },
    { AR_STOCK_START_NEW_GAME,   "document-new" },
    { AR_STOCK_RESET,            "edit-clear" },
    { AR_STOCK_RESTART_GAME,     "view-refresh" },
    { AR_STOCK_FULLSCREEN,       "view-fullscreen" },
    { AR_STOCK_LEAVE_FULLSCREEN, "view-restore" },
    { AR_STOCK_PAUSE_GAME,       "media-playback-pause" },
  };

#ifndef HAVE_GTK_ICON_FACTORY_ADD_ALIAS
  const char *stock_icon_aliases_bidi[][3] = {
    { AR_STOCK_REDO_MOVE, "edit-redo", "edit-undo" },
    { AR_STOCK_UNDO_MOVE, "edit-undo", "edit-redo" },
  };
#endif

  /* Private icon names */
  const char *private_icon_names[][2] = {
    { AR_STOCK_DEAL_CARDS, "cards-deal" }
  };

#if ENABLE_STOCK
  static const GtkStockItem ar_stock_items[] = {
    { AR_STOCK_CONTENTS,         N_("_Contents"),          0, GDK_KEY_F1, NULL },
    { AR_STOCK_FULLSCREEN,       N_("_Fullscreen"),        0, GDK_KEY_F11, NULL },
    { AR_STOCK_HINT,             N_("_Hint"),              GDK_CONTROL_MASK, 'h', NULL },
    /* Translators: This "_New" is for the menu item 'Game->New', implies "New Game" */
    { AR_STOCK_NEW_GAME,         N_("_New"),               GDK_CONTROL_MASK, 'n', NULL },
    /* Translators: This "_New Game" is for the game-over dialogue */
    { AR_STOCK_START_NEW_GAME,   N_("_New Game"),          0, 0, NULL },
    { AR_STOCK_REDO_MOVE,        N_("_Redo Move"),         GDK_CONTROL_MASK | GDK_SHIFT_MASK, 'z', NULL },
    /* Translators: this is the "Reset" scores button in a scores dialogue */
    { AR_STOCK_RESET,            N_("_Reset"),             0, 0, NULL },
    /* Translators: "_Restart" is the menu item 'Game->Restart', implies "Restart Game" */
    { AR_STOCK_RESTART_GAME,     N_("_Restart"),           0, 0, NULL },
    { AR_STOCK_UNDO_MOVE,        N_("_Undo Move"),         GDK_CONTROL_MASK, 'z', NULL },
    { AR_STOCK_DEAL_CARDS,       N_("_Deal"),              GDK_CONTROL_MASK, 'd', NULL },
    { AR_STOCK_LEAVE_FULLSCREEN, N_("_Leave Fullscreen"),  0, GDK_KEY_F11, NULL },
    { AR_STOCK_PAUSE_GAME,       N_("_Pause"),             0, GDK_KEY_Pause, NULL },
  };
#endif

  guint i;
  GtkIconFactory *icon_factory;

  icon_factory = gtk_icon_factory_new ();

#ifdef HAVE_GTK_ICON_FACTORY_ADD_ALIAS
  for (i = 0; i < G_N_ELEMENTS (stock_icon_aliases); ++i) {
    gtk_icon_factory_add_alias (stock_icon_aliases[i][0],
                                stock_icon_aliases[i][1]);
  }

#else
  for (i = 0; i < G_N_ELEMENTS (stock_icon_aliases); ++i) {
    register_stock_icon (icon_factory,
                         stock_icon_aliases[i][0],
                         stock_icon_aliases[i][1]);
  }

  for (i = 0; i < G_N_ELEMENTS (stock_icon_aliases_bidi); ++i) {
    register_stock_icon_bidi (icon_factory,
                              stock_icon_aliases_bidi[i][0],
                              stock_icon_aliases_bidi[i][1],
                              stock_icon_aliases_bidi[i][2]);
  }
#endif /* HAVE_GTK_ICON_FACTORY_ADD_ALIAS */

  /* Register our private themeable icons */
  for (i = 0; i < G_N_ELEMENTS (private_icon_names); i++) {
    register_stock_icon (icon_factory,
                         private_icon_names[i][0],
                         private_icon_names[i][1]);
  }

  gtk_icon_factory_add_default (icon_factory);
  g_object_unref (icon_factory);

  /* GtkIconTheme will then look in our custom hicolor dir
   * for icons as well as the standard search paths.
   */
  /* FIXME: multi-head! */
  gtk_icon_theme_append_search_path (gtk_icon_theme_get_default (),
                                     ar_runtime_get_directory (AR_RUNTIME_ICON_THEME_DIRECTORY));
 #if ENABLE_STOCK
  gtk_stock_add_static (ar_stock_items, G_N_ELEMENTS (ar_stock_items));
#endif
}

/**
 * ar_get_licence:
 *
 * Returns: (transfer full): a newly allocated string with a GPL licence notice
 */
char *
ar_get_licence (const gchar *game_name)
{
  const int version = 3;
  char *license_trans, *license_str;

  static const char license0[] =
    /* %s is replaced with the name of the game in gnome-games. */
    N_("%s is free software; you can redistribute it and/or modify "
       "it under the terms of the GNU General Public License as published by "
       "the Free Software Foundation; either version %d of the License, or "
       "(at your option) any later version.");
  static const char license1[] =
    N_("%s is distributed in the hope that it will be useful, "
       "but WITHOUT ANY WARRANTY; without even the implied warranty of "
       "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the "
       "GNU General Public License for more details.");
  static const char license3[] =
    N_("You should have received a copy of the GNU General Public License "
       "along with this program.  If not, see <http://www.gnu.org/licenses/>.");

  license_trans = g_strjoin ("\n\n", _(license0), _(license1), _(license3), NULL);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-nonliteral"
  license_str =
    g_strdup_printf (license_trans, game_name, version, game_name, game_name);
#pragma GCC diagnostic pop

  g_free (license_trans);

  return license_str;
}
