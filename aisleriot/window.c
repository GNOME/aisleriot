/*
 *  Copyright © 1998, 2003 Jonathan Blandford <jrb@alum.mit.edu>
 *  Copyright © 2003 Callum McKenzie <callum@physics.otago.ac.nz>
 *  Copyright © 2007 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope window it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include "config.h"

#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>

#include <libguile.h>

#ifndef HAVE_GUILE_1_8
#include "guile16-compat.h"
#endif

#include <glib/gi18n.h>

#include <gtk/gtk.h>

#ifdef HAVE_GNOME
#include <libgnomeui/gnome-help.h>
#else
#include <gdk/gdkspawn.h>
#endif

#ifdef HAVE_HILDON
#ifdef HAVE_MAEMO_3
#include <hildon-widgets/hildon-banner.h>
#else
#include <hildon/hildon-banner.h>
#endif
#endif /* HAVE_HILDON */

#include <libgames-support/games-card-common.h>
#include <libgames-support/games-clock.h>
#include <libgames-support/games-files.h>
#include <libgames-support/games-stock.h>
#include <libgames-support/games-sound.h>

#include "board.h"
#include "conf.h"
#include "game.h"
#include "stats-dialog.h"
#include "util.h"

#include "window.h"

#define AISLERIOT_WINDOW_GET_PRIVATE(window)(G_TYPE_INSTANCE_GET_PRIVATE ((window), AISLERIOT_TYPE_WINDOW, AisleriotWindowPrivate))

#define MIN_WIDTH 600
#define MIN_HEIGHT 400

#define MAIN_MENU_UI_PATH       "/MainMenu"
#define RECENT_GAMES_MENU_PATH  MAIN_MENU_UI_PATH "/GameMenu/RecentMenu"
#define OPTIONS_MENU_PATH       MAIN_MENU_UI_PATH "/OptionsMenu"
#define CARD_THEMES_MENU_PATH   MAIN_MENU_UI_PATH "/ViewMenu/ThemeMenu"
#define TOOLBAR_UI_PATH         "/Toolbar"

/* The maximum number of recent games saved */
#define MAX_RECENT 5

#define SELECT_GAME_DIALOG_MIN_HEIGHT (256)

/* define this to enable a debug menu */
/* #undef ENABLE_DEBUG_UI */

enum
{
  ACTION_UNDO_MOVE,
  ACTION_REDO_MOVE,
  ACTION_RESTART_GAME,
  ACTION_FULLSCREEN,
  ACTION_HELP_GAME,
  ACTION_OPTIONS_MENU,
  ACTION_DEAL,
  ACTION_HINT,
#ifdef HAVE_HILDON
  ACTION_ACCEL_UNDO_MOVE,
#endif
#ifndef HAVE_MAEMO
  ACTION_LEAVE_FULLSCREEN,
#endif
  LAST_ACTION
};
  
struct _AisleriotWindowPrivate
{
  AisleriotGame *game;
  AisleriotBoard *board;

#ifdef HAVE_HILDON
  GtkLabel *game_message_label;
  guint game_message_hash;
#else
  GtkStatusbar *statusbar;
  guint game_message_id;
  GtkWidget *score_box;
  GtkWidget *score_label;
  GtkWidget *clock;
#endif

  GtkUIManager *ui_manager;
  GtkActionGroup *action_group;
  GtkWidget *main_menu;
  GtkWidget *toolbar;
  GtkAction *action[LAST_ACTION];

  GtkActionGroup *options_group;
  guint options_merge_id;

  GtkActionGroup *recent_games_group;
  guint recent_games_merge_id;

  GtkActionGroup *card_themes_group;
  guint card_themes_merge_id;

  GtkWidget *game_over_dialog;
  GtkWidget *game_choice_dialog;
  AisleriotStatsDialog *stats_dialog;
  GtkListStore *game_choice_store;
  GtkTreeSelection *game_choice_selection;

#ifndef HAVE_HILDON
  GtkWidget *hint_dialog;
#endif

  guint load_idle_id;

  guint scalable_cards : 1;
  guint use_pixbuf_drawing : 1;
  guint changing_game_type : 1;
  guint freecell_mode : 1;
  guint toolbar_visible : 1;
};

enum {
  OPTION_CHECKMENU,
  OPTION_RADIOMENU
};

/* Game choice dialogue */

enum {
  COL_NAME,
  COL_GAME_FILE
};

static void
select_game_row_activated_cb (GtkWidget *widget,
                              GtkTreePath *path,
                              GtkTreeViewColumn *column,
                              GtkDialog *dialog)
{
  /* Handle a double click by faking a click on the OK button. */
  gtk_dialog_response (dialog, GTK_RESPONSE_OK);
}

static void
select_game_response_cb (GtkWidget *dialog,
                         int response,
                         AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  GtkTreeModel *model;
  GtkTreeIter iter;
  char *game_file = NULL;

  if (response == GTK_RESPONSE_OK &&
      gtk_tree_selection_get_selected (priv->game_choice_selection, &model, &iter)) {
    gtk_tree_model_get (model, &iter,
                        COL_GAME_FILE, &game_file,
                        -1);
    g_assert (game_file != NULL);

    aisleriot_window_set_game (window, game_file, 0);

    /* We'll store the new game in conf when it's been loaded successfully */

    g_free (game_file);
  }

  gtk_widget_destroy (dialog);
}

static void
select_game_cb (GtkAction *action,
                AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  GtkWidget *dialog;
  GtkListStore *list;
  GtkWidget *list_view;
  GtkTreeSelection *selection;
  GtkWidget *scrolled_window;
  GtkTreeViewColumn *column;
  GtkCellRenderer *renderer;
  GamesFileList *files;
  GtkWidget *hbox;
  GtkTreeIter current_iter, selection_iter;
  gboolean current_iter_set = FALSE;
  const char *current_game_file;
  GList *l;

  if (priv->game_choice_dialog) {
    gtk_window_present (GTK_WINDOW (priv->game_choice_dialog));
    return;
  }

  priv->game_choice_store = list = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);
  list_view = gtk_tree_view_new_with_model (GTK_TREE_MODEL (list));
  g_object_unref (list);

  files = games_file_list_new ("*.scm", games_path_runtime_fix (GAMESDIR), NULL);
  games_file_list_transform_basename (files);

  current_game_file = aisleriot_game_get_game_file (priv->game);

  for (l = files->list; l != NULL; l = l->next) {
    const char *game_file = (const char *) l->data;
    char *game_name;
    GtkTreeIter iter;

    if (!game_file ||
        strcmp (game_file, "sol.scm") == 0)
      continue;

    game_name = aisleriot_util_get_display_filename (game_file);

    gtk_list_store_insert_with_values (GTK_LIST_STORE (list), &iter,
                                       -1,
                                       COL_NAME, game_name,
                                       COL_GAME_FILE, game_file,
                                       -1);

    if (current_game_file &&
        strcmp (current_game_file, game_file) == 0) {
      current_iter = iter;
      current_iter_set = TRUE;
    }

    g_free (game_name);
  }

  g_object_unref (files);

  gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (list),
                                        0, GTK_SORT_ASCENDING);

  dialog = gtk_dialog_new_with_buttons (_("Select Game"),
                                        GTK_WINDOW (window),
                                        GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_OK, GTK_RESPONSE_OK,
                                        NULL);
  gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
                                           GTK_RESPONSE_OK,
                                           GTK_RESPONSE_CANCEL,
                                           -1);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);

  gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (dialog), 5);
  gtk_box_set_spacing (GTK_BOX (GTK_DIALOG (dialog)->vbox), 2);

  hbox = gtk_hbox_new (FALSE, 12);
  gtk_container_set_border_width (GTK_CONTAINER (hbox), 5);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox),
                      hbox, TRUE, TRUE, 0);
  g_signal_connect (list_view, "row-activated",
                    G_CALLBACK (select_game_row_activated_cb), dialog);

  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (list_view), FALSE);

  renderer = gtk_cell_renderer_text_new ();
  column = gtk_tree_view_column_new_with_attributes (NULL,
                                                     renderer,
                                                     "text", COL_NAME, NULL);

  gtk_tree_view_append_column (GTK_TREE_VIEW (list_view), column);

  priv->game_choice_selection = selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (list_view));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);

  scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolled_window),
                                       GTK_SHADOW_IN);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                  GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
  gtk_container_add (GTK_CONTAINER (scrolled_window), list_view);

  gtk_box_pack_end (GTK_BOX (hbox), scrolled_window, TRUE, TRUE, 0);

  /* Set initial focus to the list view. Otherwise, if you press Up/Down key,
   * the selection jumps to the first entry in the list as the view gains
   * focus.
   */
  gtk_widget_grab_focus (list_view);

  gtk_widget_show_all (hbox);

  g_signal_connect (dialog, "response",
                    G_CALLBACK (select_game_response_cb), window);

  priv->game_choice_dialog = dialog;
  g_signal_connect (dialog, "destroy",
                    G_CALLBACK (gtk_widget_destroyed), &priv->game_choice_dialog);

  gtk_widget_set_size_request (scrolled_window, -1, SELECT_GAME_DIALOG_MIN_HEIGHT);
  gtk_window_present (GTK_WINDOW (dialog));

  /* Select the row corresponding to the currently loaded game,
   * and scroll to it.
   */
  if (current_iter_set) {
    gtk_tree_selection_select_iter (selection, &current_iter);
  }

  if (gtk_tree_selection_get_selected (selection, NULL, &selection_iter)) {
    GtkTreePath *path;

    path = gtk_tree_model_get_path (GTK_TREE_MODEL (list), &selection_iter);
    gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (list_view), path, NULL,
                                  TRUE,
				  0.5, 0.0);
    gtk_tree_path_free (path);
  }
}

/* Game Over dialogue */

enum {
  RESPONSE_UNDO = 1,
  RESPONSE_RESTART = 2,
  RESPONSE_NEW_GAME = 3
};

static void
game_over_dialog_response_cb (GtkWidget *dialog,
                              int response,
                              AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  gboolean game_won;

  game_won = aisleriot_game_get_state (priv->game) == GAME_WON;

  gtk_widget_destroy (dialog);

  switch (response) {
    case RESPONSE_UNDO:
      aisleriot_game_undo_move (priv->game);
      break;
    case RESPONSE_RESTART:
      aisleriot_game_restart_game (priv->game);
      break;
    case RESPONSE_NEW_GAME:
      aisleriot_game_new_game (priv->game, NULL);
      break;
    case GTK_RESPONSE_CLOSE:
      gtk_widget_destroy (GTK_WIDGET (window)); /* this will quit */
      break;
    default:
      /* The player closed the dialog window from the window border
       * close button.
       * If the game is over, start a new one. Otherwise undo. This latter
       * one isn't actually the default, but it is probably the most reasonable
       * thing to do.
       */
      if (game_won) {
        aisleriot_game_new_game (priv->game, NULL);
      } else {
        aisleriot_game_undo_move (priv->game);
      }
  } 
}

static void
show_game_over_dialog (AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  GtkWidget *dialog;
  const char *message;
  gboolean game_won;

  game_won = aisleriot_game_get_state (priv->game) == GAME_WON;

  if (game_won) {
    message = _("Congratulations, you have won!");
    games_sound_play ("victory");

  } else {
    message =  _("There are no more moves");
    games_sound_play ("splat");
  }

  dialog = gtk_message_dialog_new_with_markup (GTK_WINDOW (window),
					       GTK_DIALOG_DESTROY_WITH_PARENT,
					       GTK_MESSAGE_INFO,
					       GTK_BUTTONS_NONE,
                                               "<b>%s</b>",
                                               message);

#ifdef HAVE_HILDON
  /* Empty title shows up as "<unnamed>" on maemo */
  gtk_window_set_title (GTK_WINDOW (dialog), _("Game Over"));
#else
  gtk_window_set_title (GTK_WINDOW (dialog), "");
#endif /* HAVE_HILDON */

  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);

  if (game_won) {
    gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                            GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE,
                            GAMES_STOCK_START_NEW_GAME, RESPONSE_NEW_GAME,
                            NULL);
    gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
                                             RESPONSE_NEW_GAME,
                                             GTK_RESPONSE_CLOSE,
                                             -1);
  } else {
    gtk_dialog_add_buttons (GTK_DIALOG (dialog),
			    GAMES_STOCK_UNDO_MOVE, RESPONSE_UNDO,
                            GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE,
                            GAMES_STOCK_RESTART_GAME, RESPONSE_RESTART,
                            GAMES_STOCK_START_NEW_GAME, RESPONSE_NEW_GAME,
                            NULL);
    gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
                                             RESPONSE_NEW_GAME,
                                             RESPONSE_RESTART,
                                             GTK_RESPONSE_CLOSE,
                                             RESPONSE_UNDO,
                                             -1);
  }

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), RESPONSE_NEW_GAME);

  priv->game_over_dialog = dialog;
  g_signal_connect (dialog, "response",
                    G_CALLBACK (game_over_dialog_response_cb), window);
  g_signal_connect (dialog, "destroy",
                    G_CALLBACK (gtk_widget_destroyed), &priv->game_over_dialog);

  gtk_widget_show (dialog);
}

/* Statistics dialogue */

static void
update_statistics_display (AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  AisleriotStatistic current_stats;
  char *game_name;

  if (!priv->stats_dialog)
    return;

  game_name = aisleriot_game_get_name (priv->game);
  aisleriot_stats_dialog_set_name (priv->stats_dialog, game_name);
  g_free (game_name);

  aisleriot_conf_get_statistic (aisleriot_game_get_game_file (priv->game),
                                &current_stats);

  aisleriot_stats_dialog_update (priv->stats_dialog, &current_stats);
}

static void
stats_dialog_response_cb (GtkWidget *widget,
                          int response,
                          AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  if (response == GTK_RESPONSE_REJECT) {
    AisleriotStatistic current_stats = { 0, 0, 0, 0 };

    aisleriot_conf_set_statistic (aisleriot_game_get_game_file (priv->game),
                                  &current_stats);
    aisleriot_stats_dialog_update (priv->stats_dialog, &current_stats);

    return;
  }

  gtk_widget_destroy (widget);
}

/* action callbacks */

static void
new_game_cb (GtkAction *action,
             AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  aisleriot_game_new_game (priv->game, NULL);

  gtk_widget_grab_focus (GTK_WIDGET (priv->board));
};

static void
undo_cb (GtkAction *action,
         AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  /* If a move is in progress, cancel it before changing the game! */
  aisleriot_board_abort_move (priv->board);

  aisleriot_game_undo_move (priv->game);
}

static void
redo_cb (GtkAction *action,
         AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  /* If a move is in progress, cancel it before changing the game! */
  aisleriot_board_abort_move (priv->board);

  aisleriot_game_redo_move (priv->game);
}

static void
help_about_cb (GtkAction *action,
               AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  const char *authors[] = {
    _("Main game:"),
    "Jonathan Blandford <jrb@redhat.com>",
    "Felix Bellaby <felix@pooh.u-net.com>",
    "Rosanna Yuen <zana@webwynk.net>",
    "Callum McKenzie <callum@physics.otago.ac.nz>",
    "Christian Persch <chpe" "\100" "gnome.org>",
    "Andreas Røsdal <andreasr" "\100" "gnome.org>",
    "",
    _("Card games:"),
    "Jonathan Blandford <jrb@redhat.com>",
    "W. Borgert <debacle@debian.org>",
    "Robert Brady <rwb197@ecs.soton.ac.uk>",
    "Nick Lamb <njl195@zepler.org.uk>",
    "Changwoo Ryu <cwryu@adam.kaist.ac.kr>",
    "Matthew Wilcox <matthew@wil.cx>",
    "Rosanna Yuen <zana@webwynk.net>",
    "Alan Horkan <horkana@maths.tcd.ie>",
    "Richard Hoelscher <rah@rahga.com>",
    NULL
  };
  const char *artists[] = {
    _("Card themes:"),
     /* Bellot cards */
    "David Bellot http://david.bellot.free.fr/svg-cards",
    /* Ornamental cards */
    "Nicu Buculei http://www.nicubunu.ro/cards",
    /* Dondorf and Paris cards */
    "Richard Hoelscher http://www.rahga.com/svg",
    /* Anglo-American cards */
    "Aike Reyer",
    /* FIXMEchpe: who did the Bonded cards? */
    NULL
  };
  const char *documenters[] = {
    "Rosanna Yuen <zana@webwynk.net>",
    NULL
  };

  char *licence;

  licence = games_get_license (priv->freecell_mode ? _("FreeCell Solitaire") : ("AisleRiot"));

  gtk_show_about_dialog (GTK_WINDOW (window),
#if GTK_CHECK_VERSION (2, 11, 0)
                         "program-name",
#else
                         "name",
#endif /* GTK 2.11.0 */
                            priv->freecell_mode ? _("FreeCell Solitaire")
                                                : _("AisleRiot"),
                         "version", VERSION,
                         "title", priv->freecell_mode ? _("About FreeCell Solitaire")
                                                      : _("About AisleRiot"),
#ifndef HAVE_HILDON
                         /* The long text makes the dialogue too large under maemo */
                         "comments",
                         priv->freecell_mode ?
                           NULL :
                           _("AisleRiot provides a rule-based solitaire "
                             "card engine that allows many different "
                             "games to be played.\n\n"
			     "AisleRiot is a part of GNOME Games."),
#endif
                         "copyright", "Copyright © 1998-2006 Jonathan Blandford\n"
                                      "Copyright © 2007 Christian Persch",
                         "license", licence,
                         "authors", authors,
                         "artists", artists,
                         "documenters", documenters,
                         "translator-credits", _("translator-credits"),
                         "logo-icon-name", priv->freecell_mode ? "gnome-freecell"
                                                               : "gnome-aisleriot",
                         "website", "http://www.gnome.org/projects/gnome-games/",
                         "website-label", _("GNOME Games web site"),
#if GTK_CHECK_VERSION (2, 8, 0)
                         "wrap-license", TRUE,
#endif
                        NULL);
  g_free (licence);
}

static void
restart_game (GtkAction *action,
              AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  aisleriot_game_restart_game (priv->game);
};

static void
help_general_cb (GtkAction *action,
                 AisleriotWindow *window)
{
  aisleriot_display_help (GTK_WINDOW (window), NULL);
}

static void
help_on_game_cb (GtkAction *action,
                 AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  const char *game_file;
  
  game_file = aisleriot_game_get_game_file (priv->game);
  aisleriot_display_help (GTK_WINDOW (window), game_file);
}

static void
close_window_cb (GtkAction *action,
                 AisleriotWindow *window)
{
  gtk_widget_destroy (GTK_WIDGET (window));
}

static void
statistics_cb (GtkAction *action,
               AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  if (!priv->stats_dialog) {
    priv->stats_dialog = aisleriot_stats_dialog_new ();
    gtk_window_set_transient_for (GTK_WINDOW (priv->stats_dialog),
                                  GTK_WINDOW (window));

    g_signal_connect (priv->stats_dialog, "response",
                      G_CALLBACK (stats_dialog_response_cb), window);
    g_signal_connect (priv->stats_dialog, "destroy",
                      G_CALLBACK (gtk_widget_destroyed), &priv->stats_dialog);
  }

  update_statistics_display (window);

  gtk_window_present (GTK_WINDOW (priv->stats_dialog));
}

#ifdef ENABLE_DEBUG_UI

#ifndef HAVE_HILDON

static void
move_to_next_screen_cb (GtkAction *action,
                        GtkWidget *widget)
{
  GdkScreen *screen;
  GdkDisplay *display;
  int number_of_screens, screen_num;

  screen = gtk_widget_get_screen (widget);
  display = gdk_screen_get_display (screen);
  screen_num = gdk_screen_get_number (screen);
  number_of_screens =  gdk_display_get_n_screens (display);

  if ((screen_num + 1) < number_of_screens) {
    screen = gdk_display_get_screen (display, screen_num + 1);
  } else {
    screen = gdk_display_get_screen (display, 0);
  }

  gtk_window_set_screen (GTK_WINDOW (widget), screen);
}

static gboolean
delayed_move_to_next_screen_timeout_cb (GtkWidget *widget)
{
  move_to_next_screen_cb (NULL, widget);
  return FALSE;
}

static void
delayed_move_to_next_screen_cb (GtkAction *action,
                                GtkWidget *widget)
{
  g_timeout_add (10 * 1000, (GSourceFunc) delayed_move_to_next_screen_timeout_cb, widget);
}

static void
debug_exception_cb (GtkAction *action,
                    AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  aisleriot_game_generate_exception (priv->game);
}

typedef struct {
  AisleriotWindow *window;
  GList *games_list;
} DebugCycleData;

static gboolean
debug_cycle_timeout_cb (DebugCycleData *data)
{
  char *game_file;

  if (data->games_list == NULL) {
    g_slice_free (DebugCycleData, data);
    return FALSE;
  }

  game_file = data->games_list->data;
  /* Take the head off of the list */
  data->games_list = g_list_delete_link (data->games_list, data->games_list);

  if (game_file != NULL &&
      strcmp (game_file, "sol.scm") != 0) {
    aisleriot_window_set_game (data->window, game_file, 0);
  }

  g_free (game_file);

  return TRUE;
}

static void
debug_cycle_cb (GtkAction *action,
                AisleriotWindow *window)
{
  GamesFileList *files;
  DebugCycleData *data;

  files = games_file_list_new ("*.scm", games_path_runtime_fix (GAMESDIR), NULL);
  games_file_list_transform_basename (files);

  data = g_slice_new (DebugCycleData);
  data->window = window;
  data->games_list = files->list;

  files->list = NULL;
  g_object_unref (files);

  g_timeout_add (500, (GSourceFunc) debug_cycle_timeout_cb, data);
}

#endif /* !HAVE_HILDON */

static void
debug_choose_seed_response_cb (GtkWidget *dialog,
                               int response,
                               AisleriotWindow *window)
{
  if (response == GTK_RESPONSE_OK) {
    AisleriotWindowPrivate *priv = window->priv;
    GtkEntry *entry;
    const char *text;
    char *endptr;
    guint seed;

    entry = g_object_get_data (G_OBJECT (dialog), "entry");
    text = gtk_entry_get_text (entry);

    errno = 0;
    seed = g_ascii_strtoull (text, &endptr, 10);
    if (errno == 0 && endptr != text) {
      aisleriot_game_new_game (priv->game, &seed);
    }
  }

  gtk_widget_destroy (dialog);
}

static void
debug_choose_seed_cb (GtkAction *action,
                      AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  GtkWidget *dialog, *entry;
  char str[32];

  dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                   GTK_DIALOG_DESTROY_WITH_PARENT |
                                   GTK_DIALOG_MODAL,
                                   GTK_MESSAGE_QUESTION,
                                   GTK_BUTTONS_OK_CANCEL,
                                   "Choose game seed");
  g_signal_connect (dialog, "response",
                    G_CALLBACK (debug_choose_seed_response_cb), window);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

  g_snprintf (str, sizeof (str), "%u", aisleriot_game_get_seed (priv->game));
  entry = gtk_entry_new ();
  gtk_entry_set_text (GTK_ENTRY (entry), str);
  gtk_box_pack_end (GTK_BOX (GTK_MESSAGE_DIALOG (dialog)->label->parent), entry, FALSE, FALSE, 0);
  gtk_widget_show (entry);
  g_object_set_data (G_OBJECT (dialog), "entry", entry);
  gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);

  gtk_window_present (GTK_WINDOW (dialog));
}

static void
debug_pixbuf_drawing_cb (GtkToggleAction *action,
                         AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  gboolean active;

  active = gtk_toggle_action_get_active (action);
  aisleriot_board_set_pixbuf_drawing (priv->board, active);
}

#endif /* ENABLE_DEBUG_UI */

static void
toolbar_toggled_cb (GtkToggleAction *action,
                    AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  gboolean state;

  state = gtk_toggle_action_get_active (action);

  g_object_set (priv->toolbar, "visible", state, NULL);

  priv->toolbar_visible = state != FALSE;

  games_conf_set_boolean (NULL, aisleriot_conf_get_key (CONF_SHOW_TOOLBAR), state);
}

static void
set_fullscreen_actions (AisleriotWindow *window,
                        gboolean is_fullscreen)
{
  AisleriotWindowPrivate *priv = window->priv;

#ifndef HAVE_MAEMO
  g_object_set (priv->main_menu, "visible", !is_fullscreen, NULL);

  gtk_action_set_visible (priv->action[ACTION_LEAVE_FULLSCREEN], is_fullscreen);
#endif

  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (priv->action[ACTION_FULLSCREEN]),
                                is_fullscreen);
}

static void
fullscreen_toggled_cb (GtkToggleAction *action,
                       GtkWindow *window)
{
  if (gtk_toggle_action_get_active (action)) {
    gtk_window_fullscreen (window);
  } else {
    gtk_window_unfullscreen (window);
  }
}

#ifndef HAVE_MAEMO

static void
leave_fullscreen_cb (GtkAction *action,
                     GtkWindow *window)
{
  gtk_window_unfullscreen (window);
}

#endif /* !HAVE_MAEMO */

static void
clickmove_toggle_cb (GtkToggleAction *action,
                     AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  gboolean click_to_move;

  click_to_move = gtk_toggle_action_get_active (action);

  aisleriot_game_set_click_to_move (priv->game, click_to_move);
  aisleriot_board_set_click_to_move (priv->board, click_to_move);
  
  games_conf_set_boolean (NULL, aisleriot_conf_get_key (CONF_CLICK_TO_MOVE), click_to_move);
}

static void
sound_toggle_cb (GtkToggleAction *action,
                 AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  gboolean sound_enabled;

  sound_enabled = gtk_toggle_action_get_active (action);

  games_sound_enable (sound_enabled);
  
  games_conf_set_boolean (NULL, aisleriot_conf_get_key (CONF_SOUND), sound_enabled);
}

static void
show_hint_cb (GtkAction *action,
              AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  SCM hint, string1, string2;
  char *message = NULL;
  char *str1, *str2;
#ifndef HAVE_HILDON
  GtkWidget *dialog;
#endif

  /* If the game hasn't started yet, getting a hint starts it */
  aisleriot_game_start (priv->game);

#ifndef HAVE_HILDON
  if (priv->hint_dialog) {
    gtk_widget_destroy (priv->hint_dialog);
    priv->hint_dialog = NULL;
  }
#endif

  /* Only can show hints for running games */
  if (aisleriot_game_get_state (priv->game) != GAME_RUNNING)
    return;

  hint = aisleriot_game_hint_lambda (priv->game);

  if (!SCM_NFALSEP (hint)) {
    message = g_strdup (_("This game does not have hint support yet."));
  } else {
    switch (scm_num2int (SCM_CAR (hint), SCM_ARG1, NULL)) {

    case 0:
      string1 = SCM_CADR (hint);
      if (!scm_is_string (string1))
        break;

      str1 = scm_to_locale_string (string1);
      if (!str1)
        break;

      message = g_strdup (str1);
      free (str1);
      break;

    case 1:
      string1 = SCM_CADR (hint);
      string2 = SCM_CADDR (hint);

      if (!scm_is_string (string1) || !scm_is_string (string2))
        break;

      str1 = scm_to_locale_string (string1);
      if (!str1)
        break;
      str2 = scm_to_locale_string (string2);
      if (!str2) {
        free (str1);
        break;
      }

      /* Both %s are card names */
      message = g_strdup_printf (_("Move %s onto %s."), str1, str2);
      free (str1);
      free (str2);
      break;

    case 2:
      /* NOTE! This case is exactly the same as case 1, but the strings
        * are different: the first is a card name, the 2nd a sentence fragment.
        * NOTE! FIXMEchpe! This is bad for i18n.
        */
      string1 = SCM_CADR (hint);
      string2 = SCM_CADDR (hint);

      if (!scm_is_string (string1) || !scm_is_string (string2))
        break;

      str1 = scm_to_locale_string (string1);
      if (!str1)
        break;
      str2 = scm_to_locale_string (string2);
      if (!str2) {
        free (str1);
        break;
      }

      /* The first %s is a card name, the 2nd %s a sentence fragment.
        * Yes, we know this is bad for i18n.
        */
      message = g_strdup_printf (_("Move %s onto %s."), str1, str2);
      free (str1);
      free (str2);
      break;

    case 3: /* This is deprecated (due to i18n issues) do not use. */
      g_warning ("This game uses a deprecated hint method (case 3).\n"
                 "Please file a bug at http://bugzilla.gnome.org "
                 "including this message and the name of the game "
                 "you were playing, which is %s.\n",
                 aisleriot_game_get_game_file (priv->game));
      break;

    case 4:
      string1 = SCM_CADR (hint);
      if (!scm_is_string (string1))
        break;

      str1 = scm_to_locale_string (string1);
      if (!str1)
        break;

      message = g_strdup_printf (_("You are searching for a %s."), str1);
      free (str1);
      break;

    default:
      message = g_strdup (_("This game is unable to provide a hint."));
      break;
    }
  }

  if (!message)
    return;

#ifdef HAVE_HILDON
  hildon_banner_show_information (GTK_WIDGET (window),
                                  NULL,
                                  message);
#else

  dialog = gtk_message_dialog_new_with_markup (GTK_WINDOW (window),
                                               GTK_DIALOG_DESTROY_WITH_PARENT,
                                               GTK_MESSAGE_INFO,
                                               GTK_BUTTONS_OK,
                                               "<b>%s</b>", message);
  gtk_window_set_title (GTK_WINDOW (dialog), "");
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);

  gtk_widget_show (dialog);

  priv->hint_dialog = dialog;
  g_signal_connect (dialog, "response",
                    G_CALLBACK (gtk_widget_destroy), NULL);
  g_signal_connect (dialog, "destroy",
                    G_CALLBACK (gtk_widget_destroyed), &priv->hint_dialog);
#endif /* HAVE_HILDON */

  g_free (message);
}

static void
deal_cb (GtkAction *action,
         AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  aisleriot_game_deal_cards (priv->game);
}

/* Make something that's easier to store in conf */
static guint
compress_options_to_int (SCM options_list)
{
  int i;
  guint bits;
  SCM entry;

  bits = 0;
  for (i = scm_to_int (scm_length (options_list)) - 1; i >= 0; i--) {
    entry = scm_list_ref (options_list, scm_from_int (i));
    if (SCM_NFALSEP (scm_list_p (entry))) {
      bits <<= 1;
      bits |= SCM_NFALSEP (scm_list_ref (entry, scm_from_int (1))) ? 1 : 0;
    }
  }

  return bits;
}

/* Take the bit-string value and set the option list from it. */
static void
expand_options_from_int (SCM options_list, guint bits)
{
  gint l, i;
  SCM entry;

  l = scm_to_int (scm_length (options_list));
  for (i = 0; i < l; i++) {
    entry = scm_list_ref (options_list, scm_from_int (i));
    if (SCM_NFALSEP (scm_list_p (entry))) {
      scm_list_set_x (entry, scm_from_int (1),
		      bits & 1 ? SCM_BOOL_T : SCM_BOOL_F);
      bits >>= 1;
    }
  }
}

/* The "Game Options" menu */

static void
option_cb (GtkToggleAction *action,
           AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  SCM options_list;
  SCM entry;
  gboolean active;
  const char *action_name;
  guint n;

  /* Don't change the options if we're just installing the options menu */
  if (priv->changing_game_type)
    return;

  active = gtk_toggle_action_get_active (action);

  action_name = gtk_action_get_name (GTK_ACTION (action));
  n = g_ascii_strtoull (action_name + strlen ("Option"), NULL, 10);

  options_list = aisleriot_game_get_options_lambda (priv->game);

  entry = scm_list_ref (options_list, scm_from_uint (n));

  scm_list_set_x (entry, scm_from_uint (1), active ? SCM_BOOL_T : SCM_BOOL_F);

  aisleriot_conf_set_options (aisleriot_game_get_game_file (priv->game),
                              compress_options_to_int (options_list));
                              
  aisleriot_game_apply_options_lambda (priv->game, options_list);

  /* If we're toggling OFF a radio action, don't redeal now,
   * since we'll get called another time right again when the new option
   * is toggled ON.
   * (Note that we _cannot_ also skip applying the options above, since
   * otherwise the bit from the old option won't get cleared, leading to
   * wrong game behaviour e.g. in Klondike.)
   */
  if (GTK_IS_RADIO_ACTION (action) &&
      !active)
    return;

  /* Now re-deal, so the option is applied */
  aisleriot_game_new_game (priv->game, NULL);
}

static void
install_options_menu (AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  SCM options_list;
  int l, i, options;
  gint mode = OPTION_CHECKMENU;
  GSList *radiogroup = NULL;

  if (priv->options_merge_id != 0) {
    gtk_ui_manager_remove_ui (priv->ui_manager, priv->options_merge_id);
    priv->options_merge_id = 0;
  }

  if (priv->options_group) {
    gtk_ui_manager_remove_action_group (priv->ui_manager, priv->options_group);
    priv->options_group = NULL;
  }

  /* See gtk bug #424448 */
  gtk_ui_manager_ensure_update (priv->ui_manager);

  /* To get radio buttons in the menu insert an atom into the option list
   * in your scheme code. To get back out of radio-button mode insert 
   * another atom. The exact value of the atoms is irrelevant - they merely
   * trigger a toggle - but descriptive names like begin-exclusive and
   * end-exclusive are probably a good idea.
   */
  options_list = aisleriot_game_get_options_lambda (priv->game);

  if (scm_is_false (scm_list_p (options_list)))
    return;
    
  priv->options_group = gtk_action_group_new ("Options");
  gtk_ui_manager_insert_action_group (priv->ui_manager, priv->options_group, -1);
  g_object_unref (priv->options_group);

  priv->options_merge_id = gtk_ui_manager_new_merge_id (priv->ui_manager);

  /* Only apply the options if they exist. Otherwise the options in the menu
    * and the real game options are out of sync until first changed by the user.
    */
  if (aisleriot_conf_get_options (aisleriot_game_get_game_file (priv->game), &options)) {
    expand_options_from_int (options_list, options);
    aisleriot_game_apply_options_lambda (priv->game, options_list);
  }

  l = scm_to_int (scm_length (options_list));
  for (i = 0; i < l; i++) {
    SCM entry;

    /* Each entry in the options list is a list consisting of a name and
     * a variable.
     */
    entry = scm_list_ref (options_list, scm_from_int (i));
    if (!scm_is_false (scm_list_p (entry))) {
      SCM entryname;
      char *entrynamestr;
      gboolean entrystate;
      GtkToggleAction *action;
      gchar actionname[32];

      entryname = scm_list_ref (entry, scm_from_uint (0));
      if (!scm_is_string (entryname))
        continue; /* Shouldn't happen */

      entrynamestr = scm_to_locale_string (entryname);
      if (!entrynamestr)
        continue;

      entrystate = SCM_NFALSEP (scm_list_ref (entry, scm_from_uint (1)));

      g_snprintf (actionname, sizeof (actionname), "Option%d", i);

      if (mode == OPTION_CHECKMENU) {
        action = gtk_toggle_action_new (actionname,
                                        entrynamestr,
                                        NULL,
                                        NULL /* tooltip */);
      } else {
        action = GTK_TOGGLE_ACTION (gtk_radio_action_new (actionname,
                                                          entrynamestr,
                                                          NULL,
                                                          NULL /* tooltip */,
                                                          i));
        gtk_radio_action_set_group (GTK_RADIO_ACTION (action),
                                    radiogroup);
        radiogroup = gtk_radio_action_get_group (GTK_RADIO_ACTION (action));
      }

      free (entrynamestr);

      gtk_toggle_action_set_active (action, entrystate);
      g_signal_connect (action, "toggled",
                        G_CALLBACK (option_cb), window);

      gtk_action_group_add_action (priv->options_group, GTK_ACTION (action));
      g_object_unref (action);

      gtk_ui_manager_add_ui (priv->ui_manager,
                             priv->options_merge_id,
                             OPTIONS_MENU_PATH,
                             actionname, actionname,
                             GTK_UI_MANAGER_MENUITEM, FALSE);
    } else {
      /* If we encounter an atom, change the mode. What the atom is doesn't
      * really matter. */
      if (mode == OPTION_CHECKMENU) {
        mode = OPTION_RADIOMENU;
        radiogroup = NULL;	/* Start a new radio group. */
      } else {
        mode = OPTION_CHECKMENU;
      }
    }
  }
}

/* The "Recent Games" menu */

/*
 * Takes the name of the file that drives the game and
 * stores it as a recently played game
 * Recent games are stored at the end of the list.
 */
static void
add_recently_played_game (AisleriotWindow *window,
                          const char *game_file)
{
  AisleriotWindowPrivate *priv = window->priv;
  char **recent_games, **new_recent;
  gsize i, n_recent = 0, n_new_recent = 0;

  if (!game_file)
    return;

  /* Don't store the game type in freecell mode */
  if (priv->freecell_mode)
    return;

  recent_games = games_conf_get_string_list (NULL, aisleriot_conf_get_key (CONF_RECENT_GAMES), &n_recent, NULL);

  if (recent_games == NULL) {
    new_recent = g_new (char *, 2);
    new_recent[0] = g_strdup (game_file);
    new_recent[1] = NULL;
    n_new_recent = 1;
  } else {
    new_recent = g_new (char *, MIN (n_recent + 1, MAX_RECENT) + 1);
    n_new_recent = 0;

    new_recent[n_new_recent++] = g_strdup (game_file);

    for (i = 0; i < n_recent && n_new_recent < MAX_RECENT; ++i) {
      if (g_ascii_strcasecmp (game_file, recent_games[i]) != 0) {
        new_recent[n_new_recent++] = g_strdup (recent_games[i]);
      }
    }

    /* NULL termination */
    new_recent[n_new_recent] = NULL;

    g_strfreev (recent_games);
  }

  games_conf_set_string_list (NULL, aisleriot_conf_get_key (CONF_RECENT_GAMES),
                              (const char * const *) new_recent, n_new_recent);
  g_strfreev (new_recent);
}

static void
recent_game_cb (GtkAction *action,
                AisleriotWindow *window)
{
  const char *game_file;

  game_file = g_object_get_data (G_OBJECT (action), "game");
  g_return_if_fail (game_file != NULL);

  aisleriot_window_set_game (window, game_file, 0);
  
  games_conf_set_string (NULL, aisleriot_conf_get_key (CONF_VARIATION), game_file);
}

static void
install_recently_played_menu (AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  char **recent_games;
  gsize i, n_recent = 0;

  /* Clean out the old menu */
  if (priv->recent_games_merge_id != 0) {
    gtk_ui_manager_remove_ui (priv->ui_manager, priv->recent_games_merge_id);
  }
  if (priv->recent_games_group != NULL) {
    gtk_ui_manager_remove_action_group (priv->ui_manager, priv->recent_games_group);
  }

  /* See gtk bug #424448 */
  gtk_ui_manager_ensure_update (priv->ui_manager);

  priv->recent_games_group = gtk_action_group_new ("Recent");
  gtk_ui_manager_insert_action_group (priv->ui_manager, priv->recent_games_group, -1);
  g_object_unref (priv->recent_games_group);

  priv->recent_games_merge_id = gtk_ui_manager_new_merge_id (priv->ui_manager);

  recent_games = games_conf_get_string_list (NULL, aisleriot_conf_get_key (CONF_RECENT_GAMES), &n_recent, NULL);

  for (i = 0; i < n_recent; ++i) {
    GtkAction *action;
    char actionname[32];
    char *game_name, *tooltip;

    g_snprintf (actionname, sizeof (actionname), "Recent%d", i);
    game_name = aisleriot_util_get_display_filename (recent_games[i]);
#ifdef HAVE_HILDON
    tooltip = NULL;
#else
    tooltip = g_strdup_printf (_("Play \"%s\""), game_name);
#endif /* HAVE_HILDON */
    action = gtk_action_new (actionname, game_name, tooltip, NULL);
    g_free (game_name);
    g_free (tooltip);
 
    g_object_set_data_full (G_OBJECT (action), "game",
                            recent_games[i], (GDestroyNotify) g_free);
    g_signal_connect (action, "activate",
                      G_CALLBACK (recent_game_cb), window);
    gtk_action_group_add_action (priv->recent_games_group, action);
    g_object_unref (action);

    gtk_ui_manager_add_ui (priv->ui_manager,
                           priv->recent_games_merge_id,
                           RECENT_GAMES_MENU_PATH,
                           actionname, actionname,
                           GTK_UI_MANAGER_MENUITEM, FALSE);
  }

  /* The strings themselves are now owned by gobject data on the action */
  g_free (recent_games);
}

/* Card Theme menu */

static void
card_theme_changed_cb (GtkToggleAction *action,
                       AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  const char *theme;

  if (!gtk_toggle_action_get_active (action))
    return;

  theme = g_object_get_data (G_OBJECT (action), "card-theme");
  g_return_if_fail (theme != NULL);

#ifdef HAVE_GNOME
  /* Compatibility with old settings */
  if (g_str_has_suffix (theme, ".svg")) {
    *g_strrstr (theme, ".svg") = '\0';
  } else if (g_str_has_suffix (theme, ".png")) {
    *g_strrstr (theme, ".png") = '\0';
  }
#endif /* HAVE_GNOME */

  if (aisleriot_board_set_card_theme (priv->board, theme)) {
    games_conf_set_string (NULL, aisleriot_conf_get_key (CONF_THEME), theme);
  }
}

static void
install_card_theme_menu (AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  GamesFileList *themes;
  GList *l;
  GSList *radio_group = NULL;
  const char *card_theme;
  guint i = 0;

  /* Clean out the old menu */
  if (priv->card_themes_merge_id != 0) {
    gtk_ui_manager_remove_ui (priv->ui_manager, priv->card_themes_merge_id);
    priv->card_themes_merge_id = 0;
  }
  if (priv->card_themes_group != NULL) {
    gtk_ui_manager_remove_action_group (priv->ui_manager, priv->card_themes_group);
    priv->card_themes_group = NULL;
  }

  /* See gtk bug #424448 */
  gtk_ui_manager_ensure_update (priv->ui_manager);

  themes = games_file_list_card_themes (priv->scalable_cards);

  /* No need to install the menu when there's only one theme available anyway */
  if (themes->list == NULL || themes->list->next == NULL) {
    g_object_unref (themes);
    return;
  }

  priv->card_themes_group = gtk_action_group_new ("Theme");
  gtk_ui_manager_insert_action_group (priv->ui_manager, priv->card_themes_group, -1);
  g_object_unref (priv->card_themes_group);

  priv->card_themes_merge_id = gtk_ui_manager_new_merge_id (priv->ui_manager);

  card_theme = aisleriot_board_get_card_theme (priv->board);

  for (l = themes->list; l != NULL; l = l->next) {
    const char *theme = (const char *) l->data;
    GtkRadioAction *action;
    char actionname[32];
    char *display_name, *tooltip;

    display_name = aisleriot_util_get_display_filename (theme);

    g_snprintf (actionname, sizeof (actionname), "Theme%d", i);
#ifdef HAVE_HILDON
    tooltip = NULL;
#else
    tooltip = g_strdup_printf (_("Display cards with \"%s\" card theme"), display_name);
#endif
    action = gtk_radio_action_new (actionname, display_name, tooltip, NULL, i);
    g_free (display_name);
    g_free (tooltip);

    gtk_radio_action_set_group (action, radio_group);
    radio_group = gtk_radio_action_get_group (action);

    /* Check if this is the current theme's action. Do this before connecting the callback */
    if (strcmp (card_theme, theme) == 0) {
      gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), TRUE);
    }

    /* We steal the data from the list. A bit hackish, but works fine for now */
    g_object_set_data_full (G_OBJECT (action), "card-theme",
                            l->data, (GDestroyNotify) g_free);
    l->data = NULL;

    g_signal_connect (action, "toggled",
                      G_CALLBACK (card_theme_changed_cb), window);
    gtk_action_group_add_action (priv->card_themes_group, GTK_ACTION (action));
    g_object_unref (action);

    gtk_ui_manager_add_ui (priv->ui_manager,
                           priv->card_themes_merge_id,
                           CARD_THEMES_MENU_PATH,
                           actionname, actionname,
                           GTK_UI_MANAGER_MENUITEM, FALSE);

    ++i;
  }

  g_object_unref (themes);
}

/* Callbacks */

#ifdef HAVE_HILDON

static void
sync_window_topmost_cb (AisleriotWindow *window,
                        GParamSpec *pspec,
                        gpointer user_data)
{
  AisleriotWindowPrivate *priv = window->priv;
  gboolean is_topmost;

  if (aisleriot_game_get_state (priv->game) == GAME_RUNNING) {
    is_topmost = hildon_window_get_is_topmost (HILDON_WINDOW (window));

    aisleriot_game_set_paused (priv->game, !is_topmost);
  }
}

#endif /* HAVE_HILDON */

/* Game state synchronisation */

#ifndef HAVE_HILDON

static void
sync_game_score (AisleriotGame *game,
                 GParamSpec *pspec,
                 AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  guint score = 0;
  char str[64];

  g_object_get (game, "score", &score, NULL);

  /* Translators: if you want to use localised digits for the game score,
   * then translate this string to "%I6d", else to "%6d".
   * Do not translate it to anything else!
   */
  g_snprintf (str, sizeof (str), Q_("score|%6d"), score);
  gtk_label_set_text (GTK_LABEL (priv->score_label), str);
}

#endif /* !HAVE_HILDON */

static void
sync_game_state (AisleriotGame *game,
                 GParamSpec *pspec,
                 AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  guint state;

  state = aisleriot_game_get_state (priv->game);

  /* Can only change options before the game start.
   * Set all the options insensitive, not the menu item in the main menu,
   * since the HIG disapproves of that.
   */
  if (priv->options_group != NULL) {
    gtk_action_group_set_sensitive (priv->options_group, state <= GAME_BEGIN);
  }

  /* Can only get hints while the game is running */
  gtk_action_set_sensitive (priv->action[ACTION_HINT],
                            state == GAME_BEGIN || state == GAME_RUNNING);

#ifndef HAVE_HILDON
  if (state == GAME_RUNNING) {
    games_clock_start (GAMES_CLOCK (priv->clock));
  } else {
    games_clock_stop (GAMES_CLOCK (priv->clock));
  }
#endif

  if (state >= GAME_OVER) {
    update_statistics_display (window);
    show_game_over_dialog (window);
  }
}

static void
sync_game_undoable (AisleriotGame *game,
                    GParamSpec *pspec,
                    AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  gboolean enabled;

  g_object_get (game, "can-undo", &enabled, NULL);

  gtk_action_set_sensitive (priv->action[ACTION_UNDO_MOVE], enabled);
#ifdef HAVE_HILDON
  gtk_action_set_sensitive (priv->action[ACTION_ACCEL_UNDO_MOVE], enabled);
#endif

  /* The restart game validity condition is the same as for undo. */
  gtk_action_set_sensitive (priv->action[ACTION_RESTART_GAME], enabled);
}

static void
sync_game_redoable (AisleriotGame *game,
                    GParamSpec *pspec,
                    AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  gboolean enabled;

  g_object_get (game, "can-redo", &enabled, NULL);

  gtk_action_set_sensitive (priv->action[ACTION_REDO_MOVE], enabled);
}

static void
sync_game_dealable (AisleriotGame *game,
                    GParamSpec *pspec,
                    AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  gboolean enabled;

  g_object_get (game, "can-deal", &enabled, NULL);

  gtk_action_set_sensitive (priv->action[ACTION_DEAL], enabled);
}

static void
game_type_changed_cb (AisleriotGame *game,
                      AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  char *game_name;
  guint features;
  gboolean dealable;
#ifndef HAVE_HILDON
  gboolean show_scores;
#endif /* !HAVE_HILDON */

  priv->changing_game_type = TRUE;

  game_name = aisleriot_game_get_name (game);

  g_object_set (priv->action[ACTION_HELP_GAME], "label", game_name, NULL);
  g_object_set (priv->action[ACTION_OPTIONS_MENU], "label", game_name, NULL);

  gtk_window_set_title (GTK_WINDOW (window), game_name);

  g_free (game_name);

  add_recently_played_game (window, aisleriot_game_get_game_file (game));

  install_recently_played_menu (window);

  install_options_menu (window);

  update_statistics_display (window);

  features = aisleriot_game_get_features (game);

  dealable = (features & FEATURE_DEALABLE) != 0;
  gtk_action_set_visible (priv->action[ACTION_DEAL], dealable);

#ifdef HAVE_HILDON
  gtk_label_set_text (priv->game_message_label, NULL);
#else
  games_clock_set_seconds (GAMES_CLOCK (priv->clock), 0);

  gtk_statusbar_pop (priv->statusbar, priv->game_message_id);

  show_scores = (features & FEATURE_SCORE_HIDDEN) == 0;
  g_object_set (priv->score_box, "visible", show_scores, NULL);
#endif

  priv->changing_game_type = FALSE;
}

static void
game_new_cb (AisleriotGame *game,
             AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  update_statistics_display (window);

#ifdef HAVE_HILDON
  gtk_label_set_text (priv->game_message_label, NULL);
#else
  games_clock_set_seconds (GAMES_CLOCK (priv->clock), 0);

  gtk_statusbar_pop (priv->statusbar, priv->game_message_id);
#endif
}

static void
game_statusbar_message_cb (AisleriotGame *game,
                           const char *message,
                           AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
#ifdef HAVE_HILDON
  gtk_label_set_text (priv->game_message_label, message);

  /* The banners are annoying, but until I get a better idea, use them
   * when the toolbar is hidden.
   */
  if (!priv->toolbar_visible) {
    guint hash;

    if (message != NULL && message[0] != '\0') {
      /* Check that the message is different from the last one;
      * otherwise this becomes too annoying.
      */
      hash = g_str_hash (message);
      if (hash != priv->game_message_hash) {
        priv->game_message_hash = hash;
        hildon_banner_show_information (GTK_WIDGET (window),
                                        NULL,
                                        message);
      }
    }
  }
#else
  guint id = priv->game_message_id;

  gtk_statusbar_pop (priv->statusbar, id);
  if (message != NULL) {
    gtk_statusbar_push (priv->statusbar, id, message);
  }
#endif /* HAVE_HILDON */
}

static void
game_exception_response_cb (GtkWidget *dialog,
                            int response,
                            AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  const char *error_file;

  error_file = g_object_get_data (G_OBJECT (dialog), "error-file");
  g_assert (error_file != NULL);

  if (response == GTK_RESPONSE_ACCEPT) {
    GError *err = NULL;
    char pidstr[64];
    const char * const argv[] = {
      "bug-buddy",
      "--package", "gnome-games",
      "--package-ver", VERSION,
      "--appname", "aisleriot",
      "--pid", pidstr,
      "--include", (const char *) error_file,
      NULL
    };

    g_snprintf (pidstr, sizeof (pidstr), "%d", getpid ());

    if (!gdk_spawn_on_screen (gtk_widget_get_screen (GTK_WIDGET (window)),
                              NULL /* working dir */,
                              (char **) argv,
                              NULL /* envp */,
                              G_SPAWN_SEARCH_PATH,
                              NULL, NULL,
                              NULL,
                              &err)) {
      g_warning ("Failed to launch bug buddy: %s\n", err->message);
      g_error_free (err);
    }

    /* FIXMEchpe: can't unlink now since bug buddy still needs it... what to do? */
    /* unlink (error_file); */
  }

  gtk_widget_destroy (dialog);

  /* Start a new game */
  aisleriot_game_new_game (priv->game, NULL);

  gtk_widget_grab_focus (GTK_WIDGET (priv->board));
}

static void
game_exception_cb (AisleriotGame *game,
                   const char *error_file,
                   AisleriotWindow *window)
{
  GtkWidget *dialog;

  g_return_if_fail (error_file != NULL);

  dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_ERROR,
                                   GTK_BUTTONS_NONE,
                                   _("A scheme exception occurred"));
  gtk_message_dialog_format_secondary_text
    (GTK_MESSAGE_DIALOG (dialog),
     "%s", _("Please report this bug to the developers."));

#ifdef HAVE_HILDON
  /* Empty title shows up as "<unnamed>" on maemo */
  gtk_window_set_title (GTK_WINDOW (dialog), _("Error"));
#else
  gtk_window_set_title (GTK_WINDOW (dialog), "");
#endif /* HAVE_HILDON */

  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);

  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                          _("_Don't report"), GTK_RESPONSE_REJECT, 
                          _("_Report"), GTK_RESPONSE_ACCEPT,
                          NULL);
  gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);

  g_signal_connect (dialog, "response",
                    G_CALLBACK (game_exception_response_cb), window);
  g_object_set_data_full (G_OBJECT (dialog), "error-file",
                          g_strdup (error_file), g_free);

  gtk_widget_show (dialog);
}

/* Class implementation */

#ifdef HAVE_HILDON
G_DEFINE_TYPE (AisleriotWindow, aisleriot_window, HILDON_TYPE_WINDOW);
#else
G_DEFINE_TYPE (AisleriotWindow, aisleriot_window, GTK_TYPE_WINDOW);
#endif

static gboolean
aisleriot_window_state_event (GtkWidget *widget,
                              GdkEventWindowState *event)
{
  AisleriotWindow *window = AISLERIOT_WINDOW (widget);
  AisleriotWindowPrivate *priv = window->priv;

  if (event->changed_mask & (GDK_WINDOW_STATE_FULLSCREEN | GDK_WINDOW_STATE_MAXIMIZED)) {
    gboolean is_fullscreen, is_maximised;

    is_fullscreen = (event->new_window_state & GDK_WINDOW_STATE_FULLSCREEN) != 0;
    is_maximised = (event->new_window_state & GDK_WINDOW_STATE_MAXIMIZED) != 0;

    set_fullscreen_actions (window, is_fullscreen);

#ifndef HAVE_HILDON
#if GTK_CHECK_VERSION (2, 11, 0)
    gtk_statusbar_set_has_resize_grip (priv->statusbar, !is_maximised && !is_fullscreen);
#else
    gtk_statusbar_set_has_resize_grip (priv->statusbar, FALSE);
#endif
#endif /* !HAVE_HILDON */
  }

#ifndef HAVE_HILDON
  if (event->changed_mask & GDK_WINDOW_STATE_ICONIFIED) {
    if (aisleriot_game_get_state (priv->game) == GAME_RUNNING) {
      gboolean is_iconified;

      is_iconified = (event->new_window_state & GDK_WINDOW_STATE_ICONIFIED);

      aisleriot_game_set_paused (priv->game, is_iconified);
      if (is_iconified) {
        games_clock_stop (GAMES_CLOCK (priv->clock));
      } else {
        games_clock_start (GAMES_CLOCK (priv->clock));
      }
    }
  }
#endif /* !HAVE_HILDON */

  if (GTK_WIDGET_CLASS (aisleriot_window_parent_class)->window_state_event) {
    return GTK_WIDGET_CLASS (aisleriot_window_parent_class)->window_state_event (widget, event);
  }

  return FALSE;
}

#ifdef HAVE_HILDON
/* We never show tooltips, no need to put them into the binary */
#define ACTION_TOOLTIP(string)  (NULL)
#define ACTION_ACCEL(string1,string2) (string2)
#else
#define ACTION_TOOLTIP(string)  (string)
#define ACTION_ACCEL(string1,string2) (string1)
#endif

static void
aisleriot_window_init (AisleriotWindow *window)
{
  const GtkActionEntry actions[] = {
    /* Menu actions */
    { "GameMenu", NULL, N_("_Game") },
    { "ViewMenu", NULL, N_("_View") },
    { "ControlMenu", NULL, N_("_Control") },
    { "OptionsMenu", NULL, "Options" },
    { "HelpMenu", NULL, N_("_Help") },

    /* Menu item actions */
    { "NewGame", GAMES_STOCK_NEW_GAME, NULL,
      ACTION_ACCEL ("<ctrl>N", NULL),
      NULL,
      G_CALLBACK (new_game_cb) },
    { "RestartGame", GAMES_STOCK_RESTART_GAME, NULL, NULL, NULL,
      G_CALLBACK (restart_game) },
    { "Select", GTK_STOCK_INDEX, N_("_Select Game..."),
      ACTION_ACCEL ("<ctrl>O", NULL),
      ACTION_TOOLTIP (N_("Play a different game")),
      G_CALLBACK (select_game_cb) },
    { "RecentMenu", NULL, N_("_Recently Played") },
    { "Statistics", GTK_STOCK_ADD, N_("S_tatistics"), NULL,
      ACTION_TOOLTIP (N_("Show gameplay statistics")),
      G_CALLBACK (statistics_cb) },
    { "CloseWindow", GTK_STOCK_CLOSE, NULL, NULL, NULL,
      G_CALLBACK (close_window_cb) },
    { "UndoMove", GAMES_STOCK_UNDO_MOVE, NULL, NULL, NULL,
      G_CALLBACK (undo_cb) },
    { "RedoMove", GAMES_STOCK_REDO_MOVE, NULL, NULL, NULL,
      G_CALLBACK (redo_cb) },
    { "Deal", GTK_STOCK_OK /* FIXMEchpe */, _("_Deal"), "<control>D",
      ACTION_TOOLTIP (N_("Deal next card or cards")),
      G_CALLBACK (deal_cb) },
    { "Hint", GAMES_STOCK_HINT, NULL, NULL, NULL,
      G_CALLBACK (show_hint_cb) },
    { "Contents", GAMES_STOCK_CONTENTS, NULL, NULL,
      ACTION_TOOLTIP (N_("View help for Aisleriot")),
      G_CALLBACK (help_general_cb) },
    { "HelpGame", GAMES_STOCK_CONTENTS, NULL,
      ACTION_ACCEL ("<shift>F1", NULL),
      ACTION_TOOLTIP (N_("View help for this game")),
      G_CALLBACK (help_on_game_cb) },
    { "About", GTK_STOCK_ABOUT, NULL, NULL, NULL,
      G_CALLBACK (help_about_cb) },

    /* Toolbar-only actions */
#ifndef HAVE_MAEMO
    { "LeaveFullscreen", GAMES_STOCK_LEAVE_FULLSCREEN, NULL, NULL, NULL,
      G_CALLBACK (leave_fullscreen_cb) },
#ifndef HAVE_HILDON
    { "ThemeMenu", NULL, N_("_Card Style"), NULL, NULL, NULL },
#endif /* !HAVE_HILDON */
#endif /* !HAVE_MAEMO */

#ifdef ENABLE_DEBUG_UI
    /* Debug UI actions */
    { "DebugMenu", NULL, "_Debug" },
    { "DebugChooseSeed", NULL, "_Choose seed", NULL, NULL,
      G_CALLBACK (debug_choose_seed_cb) },
#ifndef HAVE_HILDON
    { "DebugMoveNextScreen", NULL, "_Move to next screen", NULL, NULL,
       G_CALLBACK (move_to_next_screen_cb) },
    { "DebugDelayedMoveNextScreen", NULL, "_Delayed move to next screen", NULL, NULL,
       G_CALLBACK (delayed_move_to_next_screen_cb) },
    { "DebugException", NULL, "Generate E_xception", NULL, NULL,
      G_CALLBACK (debug_exception_cb) },
    { "DebugCycle", NULL, "Cycle through _all games", NULL, NULL,
      G_CALLBACK (debug_cycle_cb) },
#endif /* !HAVE_HILDON */
#endif /* ENABLE_DEBUG_UI */

    /* Accel actions */
#ifdef HAVE_HILDON
    { "AccelUndoMove", NULL, "AccelUndoMove", "Escape", NULL,
      G_CALLBACK (undo_cb) },
#endif
  };

  const GtkToggleActionEntry toggle_actions[] = {
    { "Fullscreen", GAMES_STOCK_FULLSCREEN, NULL, NULL, NULL,
      G_CALLBACK (fullscreen_toggled_cb),
      FALSE },
    { "Toolbar", NULL, N_("_Toolbar"), NULL,
      ACTION_TOOLTIP (N_("Show or hide the toolbar")),
      G_CALLBACK (toolbar_toggled_cb),
      TRUE /* active by default */
    },
    { "ClickToMove", NULL, N_("_Click to Move"), NULL,
      ACTION_TOOLTIP (N_("Pick up and drop cards by clicking")),
      G_CALLBACK (clickmove_toggle_cb),
      FALSE /* not active by default */ },
   { "Sound", NULL, N_("_Enable sounds"), NULL,
      ACTION_TOOLTIP (N_("Whether or not to play event sounds.")),
      G_CALLBACK (sound_toggle_cb),
      FALSE /* not active by default */ },
#ifdef ENABLE_DEBUG_UI
    { "DebugPixbufDrawing", NULL, "_Pixbuf drawing", NULL, NULL,
      G_CALLBACK (debug_pixbuf_drawing_cb),
      FALSE },
#endif /* ENABLE_DEBUG_UI */
  };

  static const char names[][16] = {
    "UndoMove",
    "RedoMove",
    "RestartGame",
    "Fullscreen",
    "HelpGame",
    "OptionsMenu",
    "Deal",
    "Hint",
#ifdef HAVE_HILDON
    "AccelUndoMove",
#endif /* HAVE_HILDON */
#ifndef HAVE_MAEMO
    "LeaveFullscreen",
#endif /* !HAVE_MAEMO */
  };

  static const char ui_description[] =
    "<ui>"
#ifdef HAVE_HILDON
      "<popup name='MainMenu'>"
        "<menu action='GameMenu'>"
          "<menuitem action='NewGame'/>"
          "<menuitem action='RestartGame'/>"
          "<menuitem action='Statistics'/>"
          "<menuitem action='Select'/>"
          "<separator />"
          "<placeholder name='RecentMenu'/>"
          "<separator/>"
        "</menu>"
        "<menu action='ViewMenu'>"
          "<menuitem action='Fullscreen'/>"
          "<menuitem action='Toolbar'/>"
          "<separator/>"
          "<placeholder name='ThemeMenu'/>"
        "</menu>"
        "<menu action='ControlMenu'>"
          "<menuitem action='UndoMove'/>"
          "<menuitem action='RedoMove'/>"
          "<menuitem action='Deal'/>"
          "<menuitem action='Hint'/>"
          "<separator/>"
          "<menuitem action='ClickToMove'/>"
        "</menu>"
        "<menu action='OptionsMenu'/>"
        "<menu action='HelpMenu'>"
          "<menuitem action='Contents'/>"
          "<menuitem action='HelpGame'/>"
          "<separator/>"
          "<menuitem action='About'/>"
        "</menu>"
#ifdef ENABLE_DEBUG_UI
        "<menu action='DebugMenu'>"
          "<menuitem action='DebugChooseSeed'/>"
          "<menuitem action='DebugPixbufDrawing'/>"
        "</menu>"
#endif /* ENABLE_DEBUG_UI */
        "<menuitem action='CloseWindow'/>"
      "</popup>"
#else /* !HAVE_HILDON */
      "<menubar name='MainMenu'>"
        "<menu action='GameMenu'>"
          "<menuitem action='NewGame'/>"
          "<menuitem action='RestartGame'/>"
          "<menuitem action='Statistics'/>"
          "<menuitem action='Select'/>"
          "<menu action='RecentMenu'/>"
          "<separator/>"
          "<menuitem action='CloseWindow'/>"
        "</menu>"
        "<menu action='ViewMenu'>"
          "<menuitem action='Fullscreen'/>"
          "<menuitem action='Toolbar'/>"
          "<separator/>"
          "<menu action='ThemeMenu'/>"
        "</menu>"
        "<menu action='ControlMenu'>"
          "<menuitem action='UndoMove'/>"
          "<menuitem action='RedoMove'/>"
          "<menuitem action='Deal'/>"
          "<menuitem action='Hint'/>"
          "<separator/>"
          "<menuitem action='ClickToMove'/>"
          "<menuitem action='Sound'/>"
        "</menu>"
        "<menu action='OptionsMenu'/>"
        "<menu action='HelpMenu'>"
          "<menuitem action='Contents'/>"
          "<menuitem action='HelpGame'/>"
          "<menuitem action='About'/>"
        "</menu>"
#ifdef ENABLE_DEBUG_UI
        "<menu action='DebugMenu'>"
          "<menuitem action='DebugPixbufDrawing'/>"
          "<separator/>"
          "<menuitem action='DebugChooseSeed'/>"
          "<menuitem action='DebugMoveNextScreen'/>"
          "<menuitem action='DebugDelayedMoveNextScreen'/>"
          "<menuitem action='DebugException'/>"
          "<menuitem action='DebugCycle'/>"
        "</menu>"
#endif /* ENABLE_DEBUG_UI */
      "</menubar>"
#endif /* HAVE_HILDON */
      "<toolbar name='Toolbar'>"
        "<toolitem action='NewGame'/>"
        "<toolitem action='RestartGame'/>"
        "<toolitem action='Select'/>"
        "<separator/>"
        "<toolitem action='UndoMove'/>"
        "<toolitem action='RedoMove'/>"
        "<toolitem action='Deal'/>"
        "<toolitem action='Hint'/>"
#ifndef HAVE_MAEMO
        "<toolitem action='LeaveFullscreen'/>"
#endif
#ifdef HAVE_MAEMO
        "<separator/>"
#endif
      "</toolbar>"
#ifdef HAVE_HILDON
      "<accelerator action='AccelUndoMove'/>"
#endif
    "</ui>";

  AisleriotWindowPrivate *priv;
  GtkWidget *main_vbox;
  GtkAccelGroup *accel_group;
  GtkAction *action;
  char *theme;
  guint i;
#ifdef HAVE_HILDON
  GtkToolItem *tool_item;
#else
  const char *env;
  GtkStatusbar *statusbar;
  GtkWidget *statusbar_hbox,*label, *time_box;
#endif

  g_assert (G_N_ELEMENTS (names) == LAST_ACTION);

  priv = window->priv = AISLERIOT_WINDOW_GET_PRIVATE (window);

  priv->game = aisleriot_game_new ();

#ifdef HAVE_HILDON 
  priv->scalable_cards = FALSE;
  priv->use_pixbuf_drawing = FALSE;
#else
  /* Default to scalable */
  env = g_getenv ("AISLERIOT_CARDS_SCALABLE");
  priv->scalable_cards = env == NULL || g_ascii_strtoull (env, NULL, 0) != 0;

  /* Default to pixbuf drawing */
  env = g_getenv ("AISLERIOT_PIXBUF_DRAWING");
  priv->use_pixbuf_drawing = env == NULL || g_ascii_strtoull (env, NULL, 10) != 0;

#ifdef GNOME_ENABLE_DEBUG
  if (priv->scalable_cards)
    g_print ("Using scalable cards\n");
  else
    g_print ("Using prerendered card images\n");

  if (priv->use_pixbuf_drawing)
    g_print ("Using pixbuf drawing method\n");
  else
    g_print ("Using pixmap drawing method\n");
#endif
#endif /* HAVE_MAEMO */

  priv->board = AISLERIOT_BOARD (aisleriot_board_new (priv->game, priv->scalable_cards));

  aisleriot_board_set_pixbuf_drawing (priv->board, priv->use_pixbuf_drawing);

  theme = games_conf_get_string_with_default (NULL, aisleriot_conf_get_key (CONF_THEME), GAMES_CARD_THEME_DEFAULT);
#ifdef HAVE_GNOME
  /* Compatibility with old settings */
  if (g_str_has_suffix (theme, ".svg")) {
    *g_strrstr (theme, ".svg") = '\0';
  } else if (g_str_has_suffix (theme, ".png")) {
    *g_strrstr (theme, ".png") = '\0';
  }
#endif

  aisleriot_board_set_card_theme (priv->board, theme);
  g_free (theme);

  priv->action_group = gtk_action_group_new ("MenuActions");
  gtk_action_group_set_translation_domain (priv->action_group, GETTEXT_PACKAGE);
  gtk_action_group_add_actions (priv->action_group,
                                actions,
                                G_N_ELEMENTS (actions),
				window);
  gtk_action_group_add_toggle_actions (priv->action_group,
                                       toggle_actions,
				       G_N_ELEMENTS (toggle_actions),
                                       window);

  priv->ui_manager = gtk_ui_manager_new ();

  gtk_ui_manager_insert_action_group (priv->ui_manager, priv->action_group, -1);

  for (i = 0; i < LAST_ACTION; ++i) {
    priv->action[i] = gtk_action_group_get_action (priv->action_group, names[i]);
    g_assert (priv->action[i]);
  }

  /* Hide the "Deal" action initially, since not all games support it */
  gtk_action_set_visible (priv->action[ACTION_DEAL], FALSE);

  /* Set labels for toolbar items */
  action = gtk_action_group_get_action (priv->action_group, "Select");
  g_object_set (action, "short-label", _("Select Game"), NULL);

#ifndef HAVE_HILDON
  statusbar = priv->statusbar = GTK_STATUSBAR (gtk_statusbar_new ());
  priv->game_message_id = gtk_statusbar_get_context_id (priv->statusbar, "game-message");
  games_stock_prepare_for_statusbar_tooltips (priv->ui_manager,
                                              GTK_WIDGET (priv->statusbar));

#if GTK_CHECK_VERSION (2, 11, 0)
  gtk_statusbar_set_has_resize_grip (priv->statusbar, TRUE);
#else
  gtk_statusbar_set_has_resize_grip (priv->statusbar, FALSE);
#endif

  /* Widget surgery: move the statusbar's label into a hbox
   * which we put in the statusbar's frame instead.
   */
  statusbar_hbox = gtk_hbox_new (FALSE, 24);
  g_object_ref (statusbar->label);
  gtk_container_remove (GTK_CONTAINER (statusbar->frame), statusbar->label);
  gtk_box_pack_start (GTK_BOX (statusbar_hbox), statusbar->label, TRUE, TRUE, 0);
  g_object_unref (statusbar->label);
  gtk_container_add (GTK_CONTAINER (statusbar->frame), statusbar_hbox);
  gtk_widget_show (statusbar_hbox);

  /* Score */
  priv->score_box = gtk_hbox_new (12, FALSE);
  label = gtk_label_new (_("Score:"));
  gtk_widget_show (label);
  gtk_box_pack_start (GTK_BOX (priv->score_box), label, FALSE, FALSE, 0);
  priv->score_label = gtk_label_new ("   0");
  gtk_widget_show (priv->score_label);
  gtk_box_pack_start (GTK_BOX (priv->score_box), priv->score_label, FALSE, FALSE, 0);
  gtk_box_pack_end (GTK_BOX (statusbar_hbox), priv->score_box, FALSE, FALSE, 0);

  time_box = gtk_hbox_new (12, FALSE);
  label = gtk_label_new (_("Time:"));
  gtk_box_pack_start (GTK_BOX (time_box), label, FALSE, FALSE, 0);
  priv->clock = games_clock_new ();
  gtk_box_pack_start (GTK_BOX (time_box), priv->clock, FALSE, FALSE, 0);
  gtk_box_pack_end (GTK_BOX (statusbar_hbox), time_box, FALSE, FALSE, 0);
  gtk_widget_show_all (time_box);
#endif /* !HAVE_HILDON */

  /* Load the UI after we've connected the statusbar,
   * otherwise not all actions will have statusbar help.
   */
  gtk_ui_manager_add_ui_from_string (priv->ui_manager, ui_description,
                                     strlen (ui_description), NULL);

  priv->main_menu = gtk_ui_manager_get_widget (priv->ui_manager, MAIN_MENU_UI_PATH);
  priv->toolbar = gtk_ui_manager_get_widget (priv->ui_manager, TOOLBAR_UI_PATH);

#ifdef HAVE_HILDON
  /* It's a bit ugly to mess with the toolbar like this,
   * but it's simpler than to implement a dedicated action
   * which creates a tool item which ellipsises its label...
   */
  tool_item = gtk_tool_item_new ();
  gtk_tool_item_set_expand (tool_item, TRUE);

  priv->game_message_label = GTK_LABEL (gtk_label_new (NULL));
  gtk_misc_set_alignment (GTK_MISC (priv->game_message_label), 1.0, 0.5);
  gtk_label_set_ellipsize (priv->game_message_label, PANGO_ELLIPSIZE_END);
  gtk_container_add (GTK_CONTAINER (tool_item), GTK_WIDGET (priv->game_message_label));

  gtk_toolbar_insert (GTK_TOOLBAR (priv->toolbar), tool_item, -1);
  gtk_widget_show_all (GTK_WIDGET (tool_item));
#endif

  /* The actions and menus are done. Add the card themes menu; the
   * recent games menu will be updated when the initial game loads.
   */
  install_card_theme_menu (window);

  accel_group = gtk_ui_manager_get_accel_group (priv->ui_manager);
  gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);

  action = gtk_action_group_get_action (priv->action_group, "Toolbar");
  priv->toolbar_visible = games_conf_get_boolean (NULL, aisleriot_conf_get_key (CONF_SHOW_TOOLBAR), NULL) != FALSE;
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
                                priv->toolbar_visible);
  action = gtk_action_group_get_action (priv->action_group, "ClickToMove");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
                                games_conf_get_boolean (NULL, aisleriot_conf_get_key (CONF_CLICK_TO_MOVE), NULL));
  action = gtk_action_group_get_action (priv->action_group, "Sound");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
                                games_conf_get_boolean (NULL, aisleriot_conf_get_key (CONF_SOUND), NULL));
  gtk_action_set_visible (action, games_sound_is_available ());
  action = gtk_action_group_get_action (priv->action_group, "RecentMenu");
  g_object_set (action, "hide-if-empty", FALSE, NULL);

  set_fullscreen_actions (window, FALSE);

  /* Now set up the widgets */
  main_vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), main_vbox);
  gtk_widget_show (main_vbox);

#ifdef HAVE_HILDON
  hildon_window_set_menu (HILDON_WINDOW (window), GTK_MENU (priv->main_menu));
  hildon_window_add_toolbar (HILDON_WINDOW (window), GTK_TOOLBAR (priv->toolbar));
#else
  gtk_box_pack_start (GTK_BOX (main_vbox), priv->main_menu, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (main_vbox), priv->toolbar, FALSE, FALSE, 0);
  gtk_box_pack_end (GTK_BOX (main_vbox), GTK_WIDGET (priv->statusbar), FALSE, FALSE, 0);
  gtk_widget_show (GTK_WIDGET (priv->statusbar));
#endif /* HAVE_HILDON */

  gtk_box_pack_start (GTK_BOX (main_vbox), GTK_WIDGET (priv->board), TRUE, TRUE, 0);
  gtk_widget_show (GTK_WIDGET (priv->board));

  /* Synchronise */
#ifndef HAVE_HILDON
  sync_game_score (priv->game, NULL, window);
  g_signal_connect (priv->game, "notify::score",
                    G_CALLBACK (sync_game_score), window);
#endif /* !HAVE_HILDON */

  sync_game_state (priv->game, NULL, window);
  g_signal_connect (priv->game, "notify::state",
                    G_CALLBACK (sync_game_state), window);
  sync_game_undoable (priv->game, NULL, window);
  g_signal_connect (priv->game, "notify::can-undo",
                    G_CALLBACK (sync_game_undoable), window);
  sync_game_redoable (priv->game, NULL, window);
  g_signal_connect (priv->game, "notify::can-redo",
                    G_CALLBACK (sync_game_redoable), window);
  sync_game_dealable (priv->game, NULL, window);
  g_signal_connect (priv->game, "notify::can-deal",
                    G_CALLBACK (sync_game_dealable), window);

  g_signal_connect (priv->game, "game-type",
                    G_CALLBACK (game_type_changed_cb), window);
  g_signal_connect (priv->game, "game-new",
                    G_CALLBACK (game_new_cb), window);
  g_signal_connect (priv->game, "message",
                    G_CALLBACK (game_statusbar_message_cb), window);
  g_signal_connect (priv->game, "exception",
                    G_CALLBACK (game_exception_cb), window);

  /* Fallback, if there is no saved size */
  gtk_window_set_default_size (GTK_WINDOW (window), MIN_WIDTH, MIN_HEIGHT);

  /* Restore window state */
  games_conf_add_window (GTK_WINDOW (window), NULL);

  /* Initial focus is in the board */
  gtk_widget_grab_focus (GTK_WIDGET (priv->board));

  /* FIXMEchpe: make this #ifdef HAVE_MAEMO_3 after testing that it is indeed fixed */
#ifdef HAVE_HILDON
  /* Bug alert! maemo#615 and maemo#875
   * Thank you, maemo developers! This bug just cost me 2 hours of my life.
   */
  gtk_widget_set_no_show_all (main_vbox, TRUE);
  gtk_widget_show_all (GTK_WIDGET (window));
  if (!priv->toolbar_visible) {
    gtk_widget_hide (GTK_WIDGET (priv->toolbar));
  }
#endif

#ifdef HAVE_HILDON
  g_signal_connect (window, "notify::is-topmost",
                    G_CALLBACK (sync_window_topmost_cb), NULL);
#endif

#ifdef ENABLE_DEBUG_UI
  action = gtk_action_group_get_action (priv->action_group, "DebugPixbufDrawing");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), priv->use_pixbuf_drawing);
#endif /* ENABLE_DEBUG_UI */
}

static void
aisleriot_window_finalize (GObject *object)
{
  AisleriotWindow *window = AISLERIOT_WINDOW (object);
  AisleriotWindowPrivate *priv = window->priv;

#ifndef HAVE_HILDON
  if (priv->hint_dialog) {
    gtk_widget_destroy (priv->hint_dialog);
    g_assert (priv->hint_dialog == NULL);
  }
#endif
  if (priv->game_over_dialog) {
    gtk_widget_destroy (priv->game_over_dialog);
    g_assert (priv->game_over_dialog == NULL);
  }
  if (priv->game_choice_dialog) {
    gtk_widget_destroy (priv->game_choice_dialog);
    g_assert (priv->game_choice_dialog == NULL);
  }
  if (priv->stats_dialog) {
    gtk_widget_destroy (GTK_WIDGET (priv->stats_dialog));
    g_assert (priv->stats_dialog == NULL);
  }

  g_signal_handlers_disconnect_matched (priv->game,
                                        G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL, NULL, window);
  g_object_unref (priv->game);

  if (priv->load_idle_id != 0) {
    g_source_remove (priv->load_idle_id);
  }

  G_OBJECT_CLASS (aisleriot_window_parent_class)->finalize (object);
}

static void
aisleriot_window_class_init (AisleriotWindowClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  gobject_class->finalize = aisleriot_window_finalize;

  widget_class->window_state_event = aisleriot_window_state_event;

  g_type_class_add_private (gobject_class, sizeof (AisleriotWindowPrivate));
}

/* public API */

/**
 * aisleriot_window_new:
 *
 * Returns: a new #AisleriotWindow
 */
GtkWidget *
aisleriot_window_new (void)
{
  return g_object_new (AISLERIOT_TYPE_WINDOW,
                       "title", _("AisleRiot"),
                       NULL);
}

/**
 * aisleriot_window_set_freecell_mode:
 * @window:
 *
 * Sets @window to FreeCell mode. In FreeCell mode,
 * the window is using the FreeCell variation, and doesn't allow
 * changing the game type.
 */
void
aisleriot_window_set_freecell_mode (AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  GtkAction *action;

  priv->freecell_mode = TRUE;

  /* Inhibit game changing */
  action = gtk_action_group_get_action (priv->action_group, "Select");
  gtk_action_set_visible (action, FALSE);
  action = gtk_action_group_get_action (priv->action_group, "RecentMenu");
  gtk_action_set_visible (action, FALSE);
}

typedef struct {
  AisleriotWindow *window;
  char *game_file;
  guint seed;
} LoadIdleData;

static void
load_error_response_cb (GtkWidget *dialog,
                        int response,
                        AisleriotWindow *window)
{
  /* Load the default game */
  aisleriot_window_set_game (window, DEFAULT_VARIATION, 0);

  gtk_widget_destroy (dialog);
}

static gboolean
load_idle_cb (LoadIdleData *data)
{
  AisleriotWindowPrivate *priv = data->window->priv;
  GError *error = NULL;

  if (!aisleriot_game_load_game (priv->game, data->game_file, &error)) {
    GtkWidget *dialog;
    char *name;

    name = aisleriot_util_get_display_filename (data->game_file);

    dialog = gtk_message_dialog_new (GTK_WINDOW (data->window),
                                     GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_OK,
                                     _("Cannot start the game \"%s\""),
                                     name);
    g_free (name);

    if (priv->freecell_mode ||
        error->domain != AISLERIOT_GAME_ERROR ||
        error->code != GAME_ERROR_FALLBACK) {
      /* Loading freecell/the fallback game failed; all we can do is exit */
      g_signal_connect_swapped (dialog, "response",
                                G_CALLBACK (gtk_widget_destroy), data->window);
    } else {
      gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                                                "%s", error->message);

      g_signal_connect (dialog, "response",
                        G_CALLBACK (load_error_response_cb), data->window);
    }

    g_error_free (error);

    gtk_window_present (GTK_WINDOW (dialog));

    return FALSE;
  }

  /* Now that we know we can successfully load this variation,
   * store it in conf, except when we're running in freecell mode.
   */
  if (!priv->freecell_mode) {
    games_conf_set_string (NULL, aisleriot_conf_get_key (CONF_VARIATION), data->game_file);
  }

  aisleriot_game_new_game (priv->game, data->seed != 0 ? &data->seed : NULL);

  gtk_widget_grab_focus (GTK_WIDGET (priv->board));

  return FALSE;
}

static void
free_load_idle_data (LoadIdleData *data)
{
  data->window->priv->load_idle_id = 0;

  g_free (data->game_file);
  g_slice_free (LoadIdleData, data);
}

/**
 * aisleriot_window_set_game:
 * @window:
 * @game_file: a UTF-8 string
 * @seed:
 *
 * Loads the game variation defined in the @game_file file.
 * Note that even though @game_file is used as a filename,
 * it must be in UTF-8!
 */
void
aisleriot_window_set_game (AisleriotWindow *window,
                           const char *game_file,
                           guint seed)
{
  AisleriotWindowPrivate *priv = window->priv;
  LoadIdleData *data;

  /* We'll do this on idle */
  if (priv->load_idle_id != 0) {
    g_source_remove (priv->load_idle_id);
  }

  data = g_slice_new (LoadIdleData);
  data->window = window;
  data->game_file = g_strdup (game_file);
  data->seed = seed;
  
  priv->load_idle_id = g_idle_add_full (G_PRIORITY_LOW,
                                        (GSourceFunc) load_idle_cb,
                                        data,
                                        (GDestroyNotify) free_load_idle_data);
}

/**
 * aisleriot_window_get_game:
 * @window:
 *
 * Returns: the #AisleriotGame running in @window
 */
AisleriotGame *
aisleriot_window_get_game (AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  return priv->game;
}
