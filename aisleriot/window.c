/*
 * Copyright © 1998, 2003 Jonathan Blandford <jrb@alum.mit.edu>
 * Copyright © 2003 Callum McKenzie <callum@physics.otago.ac.nz>
 * Copyright © 2007, 2008, 2009, 2010 Christian Persch
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

#include <config.h>

#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>

#include <glib/gi18n.h>

#include <gdk/gdk.h>
#include <gtk/gtk.h>

#ifdef HAVE_HILDON
#ifdef HAVE_MAEMO_3
#include <hildon-widgets/hildon-banner.h>
#else
#include <hildon/hildon-banner.h>
#endif
#endif /* HAVE_HILDON */

#include <libgames-support/games-card-theme.h>
#include <libgames-support/games-card-themes.h>
#include <libgames-support/games-clock.h>
#include <libgames-support/games-debug.h>
#include <libgames-support/games-glib-compat.h>
#include <libgames-support/games-stock.h>
#include <libgames-support/games-runtime.h>
#include <libgames-support/games-sound.h>
#include <libgames-support/games-string-utils.h>

#ifndef HAVE_HILDON
#include <libgames-support/games-atk-utils.h>
#endif

#ifdef HAVE_CLUTTER
#include "ar-clutter-embed.h"
#include "ar-style.h"
#include "baize.h"
#include "board.h"
#else
#include "board-noclutter.h"
#endif

#include "conf.h"
#include "game.h"
#include "stats-dialog.h"
#include "util.h"
#include "ar-fullscreen-button.h"
#include "ar-game-chooser.h"

#include "window.h"

#define AISLERIOT_WINDOW_GET_PRIVATE(window)(G_TYPE_INSTANCE_GET_PRIVATE ((window), AISLERIOT_TYPE_WINDOW, AisleriotWindowPrivate))

#define MIN_WIDTH 600
#define MIN_HEIGHT 400

#define MAIN_MENU_UI_PATH       "/MainMenu"
#define RECENT_GAMES_MENU_PATH  MAIN_MENU_UI_PATH "/GameMenu/RecentMenu"
#define OPTIONS_MENU_PATH       MAIN_MENU_UI_PATH "/OptionsMenu"
#ifdef HAVE_HILDON
#define CARD_THEMES_MENU_PATH   MAIN_MENU_UI_PATH "/ViewMenu/ThemeMenu"
#else
#define CARD_THEMES_MENU_PATH   MAIN_MENU_UI_PATH "/ViewMenu/ThemeMenu/ThemesPH"
#endif
#define TOOLBAR_UI_PATH         "/Toolbar"

/* The maximum number of recent games saved */
#define MAX_RECENT 5

#if !GLIB_CHECK_VERSION (2, 16, 0)
#define C_(context, string) (_(string))
#endif

/* On maemo5, the toolbar doesn't have enough space to show a
 * significant amount of text, so we always use the banner here.
 */
#if defined(HAVE_HILDON) && (defined(HAVE_MAEMO_3) || defined(HAVE_MAEMO_4))
#define MAEMO_TOOLBAR_BANNER
#endif

/* On maemo5, there's no hardware key to exit the fullscreen mode. So we show
 * an overlay button to restore normal mode, if the toolbar is hidden too.
 */
#if defined(HAVE_HILDON) && defined(HAVE_MAEMO_5)
#define LEAVE_FULLSCREEN_BUTTON
#endif

/* define this to enable a debug menu */
/* #undef ENABLE_DEBUG_UI */

#ifdef ENABLE_DEBUG_UI
#include "prop-editor.h"
#endif

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
#if !defined(HAVE_MAEMO) || !defined(MAEMO_TOOLBAR_BANNER)
  ACTION_LEAVE_FULLSCREEN,
#endif
  LAST_ACTION
};
  
struct _AisleriotWindowPrivate
{
  AisleriotGame *game;
  ArStyle *board_style;
#ifdef HAVE_CLUTTER
  ArClutterEmbed *board;
  ClutterActor *baize_actor;
  ClutterActor *board_actor;
#else
  AisleriotBoard *board;
#endif

  GamesCardThemes *theme_manager;
  GamesCardTheme *theme;

#ifdef HAVE_HILDON
  guint game_message_hash;
#ifdef MAEMO_TOOLBAR_BANNER
  GtkLabel *game_message_label;
#endif /* MAEMO_TOOLBAR_BANNER */
#else
  GtkStatusbar *statusbar;
  guint game_message_id;
  guint board_message_id;
  GtkWidget *score_box;
  GtkWidget *score_label;
  GtkWidget *clock;
#endif /* HAVE_HILDON */

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

#ifndef HAVE_HILDON
  GtkWidget *hint_dialog;
#endif

#ifdef LEAVE_FULLSCREEN_BUTTON
  GtkWidget *fullscreen_button;
#endif

  guint load_idle_id;

  guint changing_game_type : 1;
  guint freecell_mode : 1;
  guint toolbar_visible : 1;
  guint statusbar_visible : 1;
  guint fullscreen : 1;
};

enum {
  OPTION_CHECKMENU,
  OPTION_RADIOMENU
};

enum {
  PROP_0,
  PROP_FREECELL_MODE
};

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
}

static void
undo_cb (GtkAction *action,
         AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  /* If a move is in progress, cancel it before changing the game! */
#ifdef HAVE_CLUTTER
  aisleriot_board_abort_move (AISLERIOT_BOARD (priv->board_actor));
#else
  aisleriot_board_abort_move (priv->board);
#endif

  aisleriot_game_undo_move (priv->game);
}

static void
redo_cb (GtkAction *action,
         AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

#ifdef HAVE_CLUTTER
  aisleriot_board_abort_move (AISLERIOT_BOARD (priv->board_actor));
#else
  aisleriot_board_abort_move (priv->board);
#endif

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
    "Vincent Povirk",
    NULL
  };
  const char *artists[] = {
    _("Card themes:"),
     /* Bellot cards */
    "David Bellot http://david.bellot.free.fr/svg-cards",
    /* Ornamental cards */
    "Nicu Buculei http://www.nicubunu.ro/cards",
    /* "Tango" cards */
    "Frederik Elwert <frederik.elwert@web.de>",
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
                             "games to be played.\n"
			     "AisleRiot is a part of GNOME Games."),
#endif
                         "copyright", "Copyright © 1998-2006 Jonathan Blandford\n"
                                      "Copyright © 2007, 2008, 2009, 2010 Christian Persch",
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
select_game_cb (GtkAction *action,
                AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  if (priv->game_choice_dialog) {
    gtk_window_present (GTK_WINDOW (priv->game_choice_dialog));
    return;
  }

  priv->game_choice_dialog = ar_game_chooser_new (window);
  g_signal_connect (priv->game_choice_dialog, "destroy",
                    G_CALLBACK (gtk_widget_destroyed), &priv->game_choice_dialog);

  gtk_window_present (GTK_WINDOW (priv->game_choice_dialog));
}

static void
help_general_cb (GtkAction *action,
                 AisleriotWindow *window)
{
  aisleriot_show_help (GTK_WIDGET (window), NULL);
}

static void
help_on_game_cb (GtkAction *action,
                 AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  const char *game_file;
  
  game_file = aisleriot_game_get_game_file (priv->game);
  aisleriot_show_help (GTK_WIDGET (window), game_file);
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

#ifdef ENABLE_CARD_THEMES_INSTALLER

static void
install_themes_cb (GtkAction *action,
                   AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  games_card_themes_install_themes (priv->theme_manager,
                                    GTK_WINDOW (window),
                                    gtk_get_current_event_time ());
}

#endif /* ENABLE_CARD_THEMES_INSTALLER */

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
  g_timeout_add_seconds (10, (GSourceFunc) delayed_move_to_next_screen_timeout_cb, widget);
}

static void
debug_exception_cb (GtkAction *action,
                    AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  aisleriot_game_generate_exception (priv->game);
}

#define DEBUG_WINDOW_DATA_KEY "debug-data"

typedef struct {
  AisleriotWindow *window;
  GList *games_list;
  GList *current_game;
} DebugWindowData;

static void
debug_data_free (DebugWindowData *data)
{
  g_list_foreach (data->games_list, (GFunc) g_free, NULL);
  g_list_free (data->games_list);
  g_slice_free (DebugWindowData, data);
}

static DebugWindowData *
debug_ensure_game_list (AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  DebugWindowData *data;
  GDir *dir;
  GList *list = NULL;
  const char *games_dir;

  data = g_object_get_data (G_OBJECT (window), DEBUG_WINDOW_DATA_KEY);
  if (data != NULL)
    return data;

  games_dir = games_runtime_get_directory (GAMES_RUNTIME_GAME_GAMES_DIRECTORY);
  dir = g_dir_open (games_dir, 0, NULL);
  if (dir != NULL) {
    const char *game_file;

    while ((game_file = g_dir_read_name (dir)) != NULL) {
      if (!g_str_has_suffix (game_file, ".scm") ||
          strcmp (game_file, "sol.scm") == 0)
        continue;

      list = g_list_prepend (list, g_strdup (game_file));
    }

    list = g_list_sort (list, (GCompareFunc) strcmp);
    g_dir_close (dir);
  }

  data = g_slice_new (DebugWindowData);
  data->window = window;
  data->games_list = list;
  data->current_game = g_list_find_custom (data->games_list,
                                           aisleriot_game_get_game_file (priv->game),
                                           (GCompareFunc) strcmp);

  g_object_set_data_full (G_OBJECT (window), DEBUG_WINDOW_DATA_KEY,
                          data, (GDestroyNotify) debug_data_free);

  return data;
}

static gboolean
debug_cycle_timeout_cb (AisleriotWindow *window)
{
  DebugWindowData *data;
  char *game_file;

  data = debug_ensure_game_list (window);
  if (data->current_game != NULL) {
    data->current_game = data->current_game->next;
    /* We're done */
    if (!data->current_game)
      return FALSE;
  }
  if (!data->current_game) {
    data->current_game = data->games_list;
  }
  if (!data->current_game)
    return FALSE;

  game_file = data->current_game->data;  
  aisleriot_window_set_game (data->window, game_file, 0);

  return TRUE;
}

static void
debug_cycle_cb (GtkAction *action,
                AisleriotWindow *window)
{
  g_timeout_add (500, (GSourceFunc) debug_cycle_timeout_cb, window);
}

static void
debug_game_first (GtkAction *action,
                  AisleriotWindow *window)
{
  DebugWindowData *data;

  data = debug_ensure_game_list (window);
  data->current_game = data->games_list;
  if (!data->current_game)
    return;

  aisleriot_window_set_game (data->window, (const char *) data->current_game->data, 0);
}

static void
debug_game_last (GtkAction *action,
                 AisleriotWindow *window)
{
  DebugWindowData *data;

  data = debug_ensure_game_list (window);
  data->current_game = g_list_last (data->games_list);
  if (!data->current_game)
    return;

  aisleriot_window_set_game (data->window, (const char *) data->current_game->data, 0);
}

static void
debug_game_next (GtkAction *action,
                 AisleriotWindow *window)
{
  DebugWindowData *data;

  data = debug_ensure_game_list (window);
  if (data->current_game) {
    data->current_game = data->current_game->next;
  }
  if (!data->current_game) {
    data->current_game = data->games_list;
  }
  if (!data->current_game)
    return;

  aisleriot_window_set_game (data->window, (const char *) data->current_game->data, 0);
}

static void
debug_game_prev (GtkAction *action,
                 AisleriotWindow *window)
{
  DebugWindowData *data;

  data = debug_ensure_game_list (window);
  if (data->current_game) {
    data->current_game = data->current_game->prev;
  }
  if (!data->current_game) {
    data->current_game = data->games_list;
  }
  if (!data->current_game)
    return;

  aisleriot_window_set_game (data->window, (const char *) data->current_game->data, 0);
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

      gtk_widget_grab_focus (GTK_WIDGET (priv->board));
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
                                   "%s", "Choose game seed");
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
debug_tweak_cb (GtkAction *action,
                AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  GObject *object;
  GType type;
  GtkWidget *prop_editor;

  object = G_OBJECT (priv->board_style);
  type = G_OBJECT_TYPE (object);

  g_assert (object != NULL);

  prop_editor = create_prop_editor (object, type);
  gtk_window_set_transient_for (GTK_WINDOW (prop_editor), GTK_WINDOW (window));
  gtk_window_present (GTK_WINDOW (prop_editor));
}

#endif /* ENABLE_DEBUG_UI */

static void
set_fullscreen_button_active (AisleriotWindow *window)
{
#ifdef LEAVE_FULLSCREEN_BUTTON
  AisleriotWindowPrivate *priv = window->priv;
  gboolean active;

  active = priv->fullscreen && !priv->toolbar_visible;
  if (!active) {
    if (priv->fullscreen_button != NULL) {
      ar_fullscreen_button_set_active (AR_FULLSCREEN_BUTTON (priv->fullscreen_button), FALSE);
    }

    return;
  }

  if (active && priv->fullscreen_button == NULL) {
    priv->fullscreen_button = ar_fullscreen_button_new (GTK_WINDOW (window),
                                                        GTK_CORNER_TOP_RIGHT);
  }

  ar_fullscreen_button_set_active (AR_FULLSCREEN_BUTTON (priv->fullscreen_button), TRUE);
#endif /* LEAVE_FULLSCREEN_BUTTON */
}

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

  set_fullscreen_button_active (window);
}

#ifndef HAVE_HILDON

static void
statusbar_toggled_cb (GtkToggleAction *action,
                      AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  gboolean state;

  state = gtk_toggle_action_get_active (action);

  g_object_set (priv->statusbar, "visible", state, NULL);

  /* Only update the clock continually if it's visible */
  games_clock_set_update (GAMES_CLOCK (priv->clock), state);

  priv->statusbar_visible = state != FALSE;

  games_conf_set_boolean (NULL, aisleriot_conf_get_key (CONF_SHOW_STATUSBAR), state);
}

#endif /* !HAVE_HILDON */

static void
set_fullscreen_actions (AisleriotWindow *window,
                        gboolean is_fullscreen)
{
  AisleriotWindowPrivate *priv = window->priv;

  priv->fullscreen = is_fullscreen;

#ifndef HAVE_MAEMO
  g_object_set (priv->main_menu, "visible", !is_fullscreen, NULL);
#endif /* HAVE_MAEMO */

#if !defined(HAVE_MAEMO) || !defined(MAEMO_TOOLBAR_BANNER)
  gtk_action_set_visible (priv->action[ACTION_LEAVE_FULLSCREEN], is_fullscreen);
  g_object_set (gtk_ui_manager_get_widget (priv->ui_manager, "/Toolbar/LeaveFullscreenSep"),
                "visible", is_fullscreen,
                "draw", FALSE,
                NULL);
#endif /* !HAVE_MAEMO || !MAEMO_TOOLBAR_BANNER */

  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (priv->action[ACTION_FULLSCREEN]),
                                is_fullscreen);
}

static void
fullscreen_toggled_cb (GtkToggleAction *action,
                       GtkWindow *window)
{
  _games_debug_print (GAMES_DEBUG_WINDOW_STATE,
                      "[window %p] fullscreen_toggled_cb, %s fullscreen\n",
                      window,
                      gtk_toggle_action_get_active (action) ? "going" : "leaving");

  if (gtk_toggle_action_get_active (action)) {
    gtk_window_fullscreen (window);
  } else {
    gtk_window_unfullscreen (window);
  }
}

#if !defined(HAVE_MAEMO) || !defined(MAEMO_TOOLBAR_BANNER)

static void
leave_fullscreen_cb (GtkAction *action,
                     GtkWindow *window)
{
  gtk_window_unfullscreen (window);
}

#endif /* !HAVE_MAEMO || !MAEMO_TOOLBAR_BANNER */

static void
clickmove_toggle_cb (GtkToggleAction *action,
                     AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  gboolean click_to_move;

  click_to_move = gtk_toggle_action_get_active (action);

  aisleriot_game_set_click_to_move (priv->game, click_to_move);
  ar_style_set_click_to_move (priv->board_style, click_to_move);
  
  games_conf_set_boolean (NULL, aisleriot_conf_get_key (CONF_CLICK_TO_MOVE), click_to_move);
}

#ifdef ENABLE_SOUND

static void
sound_toggle_cb (GtkToggleAction *action,
                 AisleriotWindow *window)
{
  gboolean sound_enabled;

  sound_enabled = gtk_toggle_action_get_active (action);

  games_sound_enable (sound_enabled);
  
  games_conf_set_boolean (NULL, aisleriot_conf_get_key (CONF_SOUND), sound_enabled);
}

#endif /* ENABLE_SOUND */

#ifdef HAVE_CLUTTER

static void
animations_toggle_cb (GtkToggleAction *action,
                      AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  gboolean enabled;

  enabled = gtk_toggle_action_get_active (action);

  ar_style_set_enable_animations (priv->board_style, enabled);
  
  games_conf_set_boolean (NULL, aisleriot_conf_get_key (CONF_ANIMATIONS), enabled);
}

#endif /* HAVE_CLUTTER */

static void
show_hint_cb (GtkAction *action,
              AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  char *message;
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

  message = aisleriot_game_get_hint (priv->game);
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

/* The "Game Options" menu */

static void
apply_option (GtkToggleAction *action,
              guint32 *changed_mask,
              guint32 *changed_value)
{
  gboolean active;
  const char *action_name;
  guint32 value;

  active = gtk_toggle_action_get_active (action);

  action_name = gtk_action_get_name (GTK_ACTION (action));
  value = g_ascii_strtoull (action_name + strlen ("Option"), NULL, 16);

  /* g_print ("option %s changed, value=%x set=%d\n", action_name, value, active); */

  *changed_mask |= value;
  if (active)
    *changed_value |= value;
}

static void
option_cb (GtkToggleAction *action,
           AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  gboolean active;
  guint32 changed_mask = 0, changed_value = 0, value;

  /* Don't change the options if we're just installing the options menu */
  if (priv->changing_game_type)
    return;

  active = gtk_toggle_action_get_active (action);

  /* If we're toggling OFF a radio action, don't redeal now,
   * since we'll get called another time right again when the new option
   * is toggled ON.
   * The game options will be updated when we get the toggled signal
   * for the newly active action in this group.
   */
  if (GTK_IS_RADIO_ACTION (action) &&
      !active)
    return;

  if (GTK_IS_RADIO_ACTION (action)) {
    GSList *group, *l;

    /* If toggling ON a radio action, we didn't turn off the other option
     * earlier. So we need to refresh the whole group.
     */

    group = gtk_radio_action_get_group (GTK_RADIO_ACTION (action));

    for (l = group; l; l = l->next) {
      apply_option (GTK_TOGGLE_ACTION (l->data), &changed_mask, &changed_value);
    }
  } else {
    apply_option (action, &changed_mask, &changed_value);
  }

  value = aisleriot_game_change_options (priv->game, changed_mask, changed_value);

  aisleriot_conf_set_options (aisleriot_game_get_game_file (priv->game), (int) value);

  /* Now re-deal, so the option is applied */
  aisleriot_game_new_game (priv->game, NULL);
}

static void
install_options_menu (AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  GList *options, *l;
  int options_value = 0;
  GSList *radiogroup = NULL;
  int radion = 0;

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

  /* Only apply the options if they exist. Otherwise the options in the menu
   * and the real game options are out of sync until first changed by the user.
   */
  if (aisleriot_conf_get_options (aisleriot_game_get_game_file (priv->game), &options_value)) {
    aisleriot_game_change_options (priv->game, AISLERIOT_GAME_OPTIONS_MAX, options_value);
  }

  /* To get radio buttons in the menu insert an atom into the option list
   * in your scheme code. To get back out of radio-button mode insert 
   * another atom. The exact value of the atoms is irrelevant - they merely
   * trigger a toggle - but descriptive names like begin-exclusive and
   * end-exclusive are probably a good idea.
   */
  options = aisleriot_game_get_options (priv->game);
  if (!options)
    return;

  priv->options_group = gtk_action_group_new ("Options");
  gtk_ui_manager_insert_action_group (priv->ui_manager, priv->options_group, -1);
  g_object_unref (priv->options_group);

  priv->options_merge_id = gtk_ui_manager_new_merge_id (priv->ui_manager);

  for (l = options; l != NULL; l = l->next) {
    AisleriotGameOption *option = (AisleriotGameOption *) l->data;
    GtkToggleAction *action;
    gchar actionname[32];

    g_snprintf (actionname, sizeof (actionname), "Option%u", option->value);

    if (option->type == AISLERIOT_GAME_OPTION_CHECK) {
      action = gtk_toggle_action_new (actionname,
                                      option->display_name,
                                      NULL,
                                      NULL /* tooltip */);
      radiogroup = NULL; /* make sure to start a new radio group when the next RADIO option comes */
      radion = 0;
    } else {
      action = GTK_TOGGLE_ACTION (gtk_radio_action_new (actionname,
                                                        option->display_name,
                                                        NULL,
                                                        NULL /* tooltip */,
                                                        radion++));
      gtk_radio_action_set_group (GTK_RADIO_ACTION (action),
                                  radiogroup);
      radiogroup = gtk_radio_action_get_group (GTK_RADIO_ACTION (action));
    }

    gtk_toggle_action_set_active (action, option->set);
    g_signal_connect (action, "toggled",
                      G_CALLBACK (option_cb), window);

    gtk_action_group_add_action (priv->options_group, GTK_ACTION (action));
    g_object_unref (action);

    gtk_ui_manager_add_ui (priv->ui_manager,
                           priv->options_merge_id,
                           OPTIONS_MENU_PATH,
                           actionname, actionname,
                           GTK_UI_MANAGER_MENUITEM, FALSE);

    aisleriot_game_option_free (option);
  }

  g_list_free (options);
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
    game_name = games_filename_to_display_name (recent_games[i]);
#ifdef HAVE_HILDON
    tooltip = NULL;
#else
    tooltip = g_strdup_printf (_("Play “%s”"), game_name);
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
aisleriot_window_take_card_theme (AisleriotWindow *window,
                                  GamesCardTheme *theme /* adopting */)
{
  AisleriotWindowPrivate *priv = window->priv;
#if GTK_CHECK_VERSION (2, 10, 0)
  GtkWidget *widget = GTK_WIDGET (window);
#endif

  if (theme == priv->theme)
    return;

  if (priv->theme) {
    g_object_unref (priv->theme);
  }
  priv->theme = theme;

#if GTK_CHECK_VERSION (2, 10, 0)
  if (gtk_widget_has_screen (widget)) {
    const cairo_font_options_t *font_options;

    font_options = gdk_screen_get_font_options (gtk_widget_get_screen (widget));
    games_card_theme_set_font_options (theme, font_options);
  }
#endif /* GTK+ 2.10.0 */

  ar_style_set_card_theme (priv->board_style, theme);
}    

static void
card_theme_changed_cb (GtkToggleAction *action,
                       AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  GamesCardThemeInfo *current_theme_info = NULL, *new_theme_info;
  GamesCardTheme *theme;
  const char *theme_name;

  if (!gtk_toggle_action_get_active (action))
    return;

  new_theme_info = g_object_get_data (G_OBJECT (action), "theme-info");
  g_assert (new_theme_info != NULL);

  if (priv->theme) {
    current_theme_info = games_card_theme_get_theme_info (priv->theme);
  }

  if (games_card_theme_info_equal (new_theme_info, current_theme_info))
    return;

  theme = games_card_themes_get_theme (priv->theme_manager, new_theme_info);
  if (!theme) {
    GSList *group, *l;

#if GTK_CHECK_VERSION (2, 12, 0) || (defined (HAVE_HILDON) && !defined(HAVE_MAEMO_3))
    gtk_widget_error_bell (GTK_WIDGET (window));
#endif

    /* Set this action insensitive so we don't try again */
    gtk_action_set_sensitive (GTK_ACTION (action), FALSE);

    /* Re-set the radio action of the current theme to active */
    group = gtk_radio_action_get_group (GTK_RADIO_ACTION (action));
    for (l = group; l != NULL; l = l->next) {
      GtkToggleAction *theme_action = GTK_TOGGLE_ACTION (l->data);
      GamesCardThemeInfo *info;

      if (theme_action == action)
        continue;

      info = g_object_get_data (G_OBJECT (theme_action), "theme-info");
      if (!games_card_theme_info_equal (info, current_theme_info))
        continue;

      /* The check at the top will prevent an infinite loop */
      gtk_toggle_action_set_active (theme_action, TRUE);
      break;
    }

    return;
  }

  aisleriot_window_take_card_theme (window, theme);

  theme_name = games_card_theme_info_get_persistent_name (new_theme_info);
  games_conf_set_string (NULL, aisleriot_conf_get_key (CONF_THEME), theme_name);
}

static void
install_card_theme_menu (GamesCardThemes *theme_manager,
                         AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  GList *list, *l;
  GSList *radio_group = NULL;
  GamesCardThemeInfo *current_theme_info;
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

  list = games_card_themes_get_themes (priv->theme_manager);

  /* No need to install the menu when there's only one theme available anyway */
  if (list == NULL || list->next == NULL) {
    g_list_foreach (list, (GFunc) games_card_theme_info_unref, NULL);
    g_list_free (list);
    return;
  }

  priv->card_themes_group = gtk_action_group_new ("Theme");
  gtk_ui_manager_insert_action_group (priv->ui_manager, priv->card_themes_group, -1);
  g_object_unref (priv->card_themes_group);

  priv->card_themes_merge_id = gtk_ui_manager_new_merge_id (priv->ui_manager);

  if (priv->theme) {
    current_theme_info = games_card_theme_get_theme_info (priv->theme);
  } else {
    current_theme_info = NULL;
  }

  for (l = list; l != NULL; l = l->next) {
    GamesCardThemeInfo *info = (GamesCardThemeInfo *) l->data;
    GtkRadioAction *action;
    char actionname[32];
    char *display_name, *tooltip;

    display_name = g_strdup (games_card_theme_info_get_display_name (info));

    g_snprintf (actionname, sizeof (actionname), "Theme%d", i);
#ifdef HAVE_HILDON
    tooltip = NULL;
#else
    tooltip = g_strdup_printf (_("Display cards with “%s” card theme"), display_name);
#endif
    action = gtk_radio_action_new (actionname, display_name, tooltip, NULL, i);
    g_free (display_name);
    g_free (tooltip);

    gtk_radio_action_set_group (action, radio_group);
    radio_group = gtk_radio_action_get_group (action);

    /* Check if this is the current theme's action. Do this before connecting the callback */
    if (games_card_theme_info_equal (info, current_theme_info)) {
      gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), TRUE);
    }

    /* We steal the data from the list */
    g_object_set_data_full (G_OBJECT (action), "theme-info",
                            l->data, (GDestroyNotify) games_card_theme_info_unref);
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

  /* The list elements' data's refcount has been adopted above */
  g_list_free (list);
}

#ifdef HAVE_HILDON
static void
main_menu_show_cb (GtkMenu *menu,
                   AisleriotWindow *window)
#else
static void
view_menu_activate_cb (GtkAction *action,
                       AisleriotWindow *window)
#endif    
{
  AisleriotWindowPrivate *priv = window->priv;

  /* Request the list of themes. If it wasn't updated yet, the "changed"
   * callback will build the card themes submenu.
   */
  games_card_themes_request_themes (priv->theme_manager);
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
  g_snprintf (str, sizeof (str), C_("score", "%6d"), score);
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

  /* In freecell mode, we've already set the title to something different */
  if (!priv->freecell_mode) {
    gtk_window_set_title (GTK_WINDOW (window), game_name);
  }

  g_free (game_name);

  add_recently_played_game (window, aisleriot_game_get_game_file (game));

  install_recently_played_menu (window);

  install_options_menu (window);

  update_statistics_display (window);

  features = aisleriot_game_get_features (game);

  dealable = (features & FEATURE_DEALABLE) != 0;
  gtk_action_set_visible (priv->action[ACTION_DEAL], dealable);

#ifdef HAVE_HILDON
#ifdef MAEMO_TOOLBAR_BANNER
  gtk_label_set_text (priv->game_message_label, NULL);
#endif /* MAEMO_TOOLBAR_BANNER */
#else
  games_clock_reset (GAMES_CLOCK (priv->clock));

  gtk_statusbar_pop (priv->statusbar, priv->game_message_id);
  gtk_statusbar_pop (priv->statusbar, priv->board_message_id);

  show_scores = (features & FEATURE_SCORE_HIDDEN) == 0;
  g_object_set (priv->score_box, "visible", show_scores, NULL);
#endif /* HAVE_HILDON */

  priv->changing_game_type = FALSE;
}

static void
game_new_cb (AisleriotGame *game,
             AisleriotWindow *window)
{
#if defined(MAEMO_TOOLBAR_BANNER) || !defined(HAVE_HILDON)
  AisleriotWindowPrivate *priv = window->priv;
#endif

  update_statistics_display (window);

#ifdef HAVE_HILDON
#ifdef MAEMO_TOOLBAR_BANNER
  gtk_label_set_text (priv->game_message_label, NULL);
#endif /* MAEMO_TOOLBAR_BANNER */
#else
  games_clock_reset (GAMES_CLOCK (priv->clock));

  gtk_statusbar_pop (priv->statusbar, priv->game_message_id);
  gtk_statusbar_pop (priv->statusbar, priv->board_message_id);
#endif /* HAVE_HILDON */
}

static void
game_statusbar_message_cb (AisleriotGame *game,
                           const char *message,
                           AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
#ifdef HAVE_HILDON
#ifdef MAEMO_TOOLBAR_BANNER
  gtk_label_set_text (priv->game_message_label, message);
#endif

  /* The banners are annoying, but until I get a better idea, use them
   * when the toolbar is hidden.
   */
#ifdef MAEMO_TOOLBAR_BANNER
  if (!priv->toolbar_visible)
#endif
  {
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
#if GLIB_CHECK_VERSION (2, 20, 0)
      /* This option was added to bug-buddy 2.25.x (bug 540150). We use the
       * 2.26 glib version as a proxy to detect this, since there's no good
       * other way.
       */
      "--unlink-tempfile",
#endif
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
                                   "%s", _("A scheme exception occurred"));
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

#if defined(HAVE_CLUTTER) || (defined(ENABLE_SOUND) && GTK_CHECK_VERSION (2, 14, 0))

static void
settings_changed_cb (GtkSettings *settings,
                     GParamSpec *pspec,
                     AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  GtkAction *action;
  gboolean enabled;
  const char *name;

  if (pspec)
    name = pspec->name;
  else
    name = NULL;

#ifdef HAVE_CLUTTER
  if (name == NULL || strcmp (name, "gtk-enable-animations") == 0) {
    g_object_get (settings, "gtk-enable-animations", &enabled, NULL);

    action = gtk_action_group_get_action (priv->action_group, "Animations");
    gtk_action_set_visible (action, enabled);
  }
#endif /* HAVE_CLUTTER */

#if defined(ENABLE_SOUND) && GTK_CHECK_VERSION (2, 14, 0)
  if (name == NULL || strcmp (name, "gtk-enable-event-sounds") == 0) {
    g_object_get (settings, "gtk-enable-event-sounds", &enabled, NULL);

    action = gtk_action_group_get_action (priv->action_group, "Sound");
    gtk_action_set_visible (action, enabled);
  }
#endif /* ENABLE_SOUND && GTK >= 2.14 */
}

static void
screen_changed_cb (GtkWidget *widget,
                   GdkScreen *previous_screen,
                   AisleriotWindow *window)
{
  GdkScreen *screen;
  GtkSettings *settings;

  screen = gtk_widget_get_screen (widget);
  if (screen == previous_screen)
    return;

  if (previous_screen) {
    g_signal_handlers_disconnect_by_func (gtk_settings_get_for_screen (previous_screen),
                                          G_CALLBACK (settings_changed_cb),
                                          window);
  }
  
  if (screen == NULL)
    return;

  games_sound_init (screen);

  settings = gtk_widget_get_settings (widget);
  settings_changed_cb (settings, NULL, window);
#ifdef HAVE_CLUTTER
  g_signal_connect (settings, "notify::gtk-enable-animations",
                    G_CALLBACK (settings_changed_cb), window);
#endif
#if defined (ENABLE_SOUND) && GTK_CHECK_VERSION (2, 14, 0)
  g_signal_connect (settings, "notify::gtk-enable-event-sounds",
                    G_CALLBACK (settings_changed_cb), window);
#endif
}

#endif /* HAVE_CLUTTER || ENABLE_SOUND && GTK+ >= 2.14.0 */

/*
 * aisleriot_window_set_freecell_mode:
 * @window:
 *
 * Sets @window to FreeCell mode. In FreeCell mode,
 * the window is using the FreeCell variation, and doesn't allow
 * changing the game type.
 */
static void
aisleriot_window_set_freecell_mode (AisleriotWindow *window,
                                    gboolean freecell_mode)
{
  AisleriotWindowPrivate *priv = window->priv;
  GtkAction *action;

  priv->freecell_mode = freecell_mode != FALSE;

  if (freecell_mode) {
    /* Inhibit game changing */
    action = gtk_action_group_get_action (priv->action_group, "Select");
    gtk_action_set_visible (action, FALSE);
    action = gtk_action_group_get_action (priv->action_group, "RecentMenu");
    gtk_action_set_visible (action, FALSE);

    gtk_window_set_title (GTK_WINDOW (window), _("Freecell Solitaire"));
  } else {
    gtk_window_set_title (GTK_WINDOW (window), _("AisleRiot"));
  }
}

#ifndef HAVE_HILDON

static void
board_status_message_cb (AisleriotBoard *board,
                         const char *status_message,
                         AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  gtk_statusbar_pop (priv->statusbar, priv->board_message_id);

  if (status_message != NULL) {
    gtk_statusbar_push (priv->statusbar, priv->board_message_id, status_message);
  }
}

#endif /* !HAVE_HILDON */

#ifdef HAVE_CLUTTER

static void
board_cursor_cb (AisleriotBoard *board,
                 int cursor_type,
                 ArClutterEmbed *embed)
{
  ar_clutter_embed_set_cursor (embed, (ArCursorType) cursor_type);
}

static void
board_error_bell_cb (AisleriotBoard *board,
                     ArClutterEmbed *embed)
{
#if GTK_CHECK_VERSION (2, 12, 0) || (defined (HAVE_HILDON) && !defined(HAVE_MAEMO_3))
  gtk_widget_error_bell (GTK_WIDGET (embed));
#endif
}

static void
embed_size_allocate_cb (ArClutterEmbed *embed,
                        GtkAllocation *allocation,
                        AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  clutter_actor_set_size (priv->board_actor, allocation->width, allocation->height);
}

#endif /* HAVE_CLUTTER */

/* Class implementation */

#ifdef HAVE_HILDON
G_DEFINE_TYPE (AisleriotWindow, aisleriot_window, HILDON_TYPE_WINDOW);
#else
G_DEFINE_TYPE (AisleriotWindow, aisleriot_window, GTK_TYPE_WINDOW);
#endif

#if GTK_CHECK_VERSION (2, 10, 0)

static void
aisleriot_window_style_set (GtkWidget *widget,
                            GtkStyle *previous_style)
{
  AisleriotWindow *window = AISLERIOT_WINDOW (widget);
  AisleriotWindowPrivate *priv = window->priv;
  const cairo_font_options_t *font_options;
  void (* style_set) (GtkWidget *, GtkStyle *) =
    GTK_WIDGET_CLASS (aisleriot_window_parent_class)->style_set;

  if (style_set)
    style_set (widget, previous_style);

  if (!priv->theme)
    return;

  font_options = gdk_screen_get_font_options (gtk_widget_get_screen (widget));
  games_card_theme_set_font_options (priv->theme, font_options);

  /* FIXMEchpe: clear the cached cards in the slots?? */
}

#endif /* GTK >= 2.10.0 */

static gboolean
aisleriot_window_state_event (GtkWidget *widget,
                              GdkEventWindowState *event)
{
  AisleriotWindow *window = AISLERIOT_WINDOW (widget);
#ifndef HAVE_HILDON
  AisleriotWindowPrivate *priv = window->priv;
#endif

  if (event->changed_mask & (GDK_WINDOW_STATE_FULLSCREEN | GDK_WINDOW_STATE_MAXIMIZED)) {
    gboolean is_fullscreen, is_maximised;

    is_fullscreen = (event->new_window_state & GDK_WINDOW_STATE_FULLSCREEN) != 0;
    is_maximised = (event->new_window_state & GDK_WINDOW_STATE_MAXIMIZED) != 0;

    set_fullscreen_actions (window, is_fullscreen);

    set_fullscreen_button_active (window);

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
      ACTION_TOOLTIP (N_("Start a new game")),
      G_CALLBACK (new_game_cb) },
    { "RestartGame", GAMES_STOCK_RESTART_GAME, NULL, NULL,
       ACTION_TOOLTIP (N_("Restart the game")),
      G_CALLBACK (restart_game) },
    { "Select", GTK_STOCK_INDEX, N_("_Select Game..."),
      ACTION_ACCEL ("<ctrl>O", NULL),
      ACTION_TOOLTIP (N_("Play a different game")),
      G_CALLBACK (select_game_cb) },
    { "RecentMenu", NULL, N_("_Recently Played") },
    { "Statistics", GTK_STOCK_ADD, N_("S_tatistics"), NULL,
      ACTION_TOOLTIP (N_("Show gameplay statistics")),
      G_CALLBACK (statistics_cb) },
    { "CloseWindow", GTK_STOCK_CLOSE, NULL, NULL,
      ACTION_TOOLTIP (N_("Close this window")),
      G_CALLBACK (close_window_cb) },
    { "UndoMove", GAMES_STOCK_UNDO_MOVE, NULL, NULL,
      ACTION_TOOLTIP (N_("Undo the last move")),
      G_CALLBACK (undo_cb) },
    { "RedoMove", GAMES_STOCK_REDO_MOVE, NULL, NULL,
      ACTION_TOOLTIP (N_("Redo the undone move")),
      G_CALLBACK (redo_cb) },
    { "Deal", GAMES_STOCK_DEAL_CARDS, NULL, NULL,
      ACTION_TOOLTIP (N_("Deal next card or cards")),
      G_CALLBACK (deal_cb) },
    { "Hint", GAMES_STOCK_HINT, NULL, NULL,
      ACTION_TOOLTIP (N_("Get a hint for your next move")),
      G_CALLBACK (show_hint_cb) },
    { "Contents", GAMES_STOCK_CONTENTS, NULL, NULL,
      ACTION_TOOLTIP (N_("View help for Aisleriot")),
      G_CALLBACK (help_general_cb) },
    { "HelpGame", GAMES_STOCK_CONTENTS, NULL,
      ACTION_ACCEL ("<shift>F1", NULL),
      ACTION_TOOLTIP (N_("View help for this game")),
      G_CALLBACK (help_on_game_cb) },
    { "About", GTK_STOCK_ABOUT, NULL, NULL,
      ACTION_TOOLTIP (N_("About this game")),
      G_CALLBACK (help_about_cb) },
#ifdef ENABLE_CARD_THEMES_INSTALLER
    { "InstallThemes", NULL, N_("Install card themes…"), NULL,
      ACTION_TOOLTIP (N_("Install new card themes from the distribution packages repositories")),
      G_CALLBACK (install_themes_cb) },
#endif /* ENABLE_CARD_THEMES_INSTALLER */

    /* Toolbar-only actions */
#if !defined(HAVE_MAEMO) || !defined(MAEMO_TOOLBAR_BANNER)
    { "LeaveFullscreen", GAMES_STOCK_LEAVE_FULLSCREEN, NULL, NULL, NULL,
      G_CALLBACK (leave_fullscreen_cb) },
#endif /* !HAVE_MAEMO || !MAEMO_TOOLBAR_BANNER */
#ifndef HAVE_HILDON
    { "ThemeMenu", NULL, N_("_Card Style"), NULL, NULL, NULL },
#endif /* !HAVE_HILDON */

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
    { "DebugGameFirst", GTK_STOCK_GOTO_FIRST, NULL, NULL, NULL,
      G_CALLBACK (debug_game_first) },
    { "DebugGameLast", GTK_STOCK_GOTO_LAST, NULL, NULL, NULL,
      G_CALLBACK (debug_game_last) },
    { "DebugGameNext", GTK_STOCK_GO_FORWARD, NULL, NULL, NULL,
      G_CALLBACK (debug_game_next) },
    { "DebugGamePrev", GTK_STOCK_GO_BACK, NULL, NULL, NULL,
      G_CALLBACK (debug_game_prev) },
#endif /* !HAVE_HILDON */
    { "DebugTweakStyle", NULL, "_Tweak Style", NULL, NULL,
      G_CALLBACK (debug_tweak_cb) },
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
      TRUE /* active by default since the UI manager creates the toolbar visible */
    },
#ifndef HAVE_HILDON
    { "Statusbar", NULL, N_("_Statusbar"), NULL,
      ACTION_TOOLTIP (N_("Show or hide statusbar")),
      G_CALLBACK (statusbar_toggled_cb),
      FALSE
    },
#endif /* !HAVE_HILDON */
    { "ClickToMove", NULL, N_("_Click to Move"), NULL,
      ACTION_TOOLTIP (N_("Pick up and drop cards by clicking")),
      G_CALLBACK (clickmove_toggle_cb),
      FALSE /* not active by default */ },
#ifdef ENABLE_SOUND
   { "Sound", NULL, N_("_Sound"), NULL,
      ACTION_TOOLTIP (N_("Whether or not to play event sounds")),
      G_CALLBACK (sound_toggle_cb),
      FALSE /* not active by default */ },
#endif /* ENABLE_SOUND */
#ifdef HAVE_CLUTTER
   { "Animations", NULL, N_("_Animations"), NULL,
      ACTION_TOOLTIP (N_("Whether or not to animate card moves")),
      G_CALLBACK (animations_toggle_cb),
      FALSE /* not active by default */ },
#endif /* HAVE_CLUTTER */
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
#if !defined(HAVE_MAEMO) || !defined(MAEMO_TOOLBAR_BANNER)
    "LeaveFullscreen",
#endif /* !HAVE_MAEMO || !MAEMO_TOOLBAR_BANNER */
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
#ifdef ENABLE_SOUND
          "<menuitem action='Sound'/>"
#endif
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
          "<menuitem action='DebugTweakStyle'/>"
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
          "<menuitem action='Statusbar'/>"
          "<separator/>"
          "<menu action='ThemeMenu'>"
            "<placeholder name='ThemesPH'/>"
#ifdef ENABLE_CARD_THEMES_INSTALLER
            "<separator/>"
            "<menuitem action='InstallThemes'/>"
#endif
          "</menu>"
        "</menu>"
        "<menu action='ControlMenu'>"
          "<menuitem action='UndoMove'/>"
          "<menuitem action='RedoMove'/>"
          "<menuitem action='Deal'/>"
          "<menuitem action='Hint'/>"
          "<separator/>"
          "<menuitem action='ClickToMove'/>"
#ifdef ENABLE_SOUND
          "<menuitem action='Sound'/>"
#endif
#ifdef HAVE_CLUTTER
          "<menuitem action='Animations'/>"
#endif
        "</menu>"
        "<menu action='OptionsMenu'/>"
        "<menu action='HelpMenu'>"
          "<menuitem action='Contents'/>"
          "<menuitem action='HelpGame'/>"
          "<menuitem action='About'/>"
        "</menu>"
#ifdef ENABLE_DEBUG_UI
        "<menu action='DebugMenu'>"
          "<menuitem action='DebugTweakStyle'/>"
          "<separator/>"
          "<menuitem action='DebugChooseSeed'/>"
          "<menuitem action='DebugMoveNextScreen'/>"
          "<menuitem action='DebugDelayedMoveNextScreen'/>"
          "<menuitem action='DebugException'/>"
          "<separator/>"
          "<menuitem action='DebugCycle'/>"
          "<separator/>"
          "<menuitem action='DebugGameFirst'/>"
          "<menuitem action='DebugGamePrev'/>"
          "<menuitem action='DebugGameNext'/>"
          "<menuitem action='DebugGameLast'/>"
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
#if !defined(HAVE_MAEMO) || !defined(MAEMO_TOOLBAR_BANNER)
        "<separator name='LeaveFullscreenSep' expand='true'/>"
        "<toolitem action='LeaveFullscreen'/>"
#endif /* !HAVE_MAEMO || !MAEMO_TOOLBAR_BANNER */
#ifdef ENABLE_DEBUG_UI
        "<toolitem action='DebugGameFirst'/>"
        "<toolitem action='DebugGamePrev'/>"
        "<toolitem action='DebugGameNext'/>"
        "<toolitem action='DebugGameLast'/>"
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
  char *theme_name;
  GamesCardTheme *theme;
  guint i;
#ifndef HAVE_HILDON
  GtkStatusbar *statusbar;
  GtkWidget *statusbar_hbox, *label, *time_box;
#endif
#ifdef HAVE_CLUTTER
  ClutterContainer *stage;
#endif

  g_assert (G_N_ELEMENTS (names) == LAST_ACTION);

  priv = window->priv = AISLERIOT_WINDOW_GET_PRIVATE (window);

  priv->fullscreen = FALSE;

  priv->game = aisleriot_game_new ();

  priv->theme_manager = games_card_themes_new ();

  priv->board_style = ar_style_new ();

#ifdef HAVE_CLUTTER
  priv->board = ar_clutter_embed_new (priv->board_style);

  priv->baize_actor = aisleriot_baize_new ();

  stage = CLUTTER_CONTAINER (gtk_clutter_embed_get_stage (GTK_CLUTTER_EMBED (priv->board)));
  clutter_container_add (stage, priv->baize_actor, NULL);
  /* FIXMEchpe: how to ensure this is ALWAYS the lowest actor? */
  clutter_actor_lower_bottom (priv->baize_actor);

  priv->board_actor = aisleriot_board_new (priv->board_style, priv->game);
  clutter_container_add (stage, priv->board_actor, NULL);

  /* FIXMEchpe */
  clutter_stage_set_key_focus (CLUTTER_STAGE (stage), priv->board_actor);

  g_signal_connect_after (priv->board, "size-allocate",
                          G_CALLBACK (embed_size_allocate_cb), window);

  g_signal_connect (priv->board_actor, "request-cursor",
                    G_CALLBACK (board_cursor_cb), priv->board);
  g_signal_connect (priv->board_actor, "error-bell",
                    G_CALLBACK (board_error_bell_cb), priv->board);

  /* FIXMEchpe: unref baize & board_actor here? */
#else
  priv->board = AISLERIOT_BOARD (aisleriot_board_new (priv->board_style, priv->game));
#endif /* HAVE_CLUTTER */

  theme_name = games_conf_get_string (NULL, aisleriot_conf_get_key (CONF_THEME), NULL);
  theme = games_card_themes_get_theme_by_name (priv->theme_manager, theme_name);
  g_free (theme_name);
  if (!theme) {
    /* Last-ditch fallback: try getting *any* theme */
    theme = games_card_themes_get_theme_any (priv->theme_manager);
  }
  if (theme) {
    aisleriot_window_take_card_theme (window, theme /* adopts */);
  } else {
    /* FIXMEchpe: FUCK, what now? Panic! */
    /* Put up some UI, and exit! */
    g_assert_not_reached ();
  }

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

  priv->game_message_id = gtk_statusbar_get_context_id (priv->statusbar, "board-message");
  g_signal_connect (priv->board, "status-message",
                    G_CALLBACK (board_status_message_cb), window);

#if GTK_CHECK_VERSION (2, 11, 0)
  gtk_statusbar_set_has_resize_grip (priv->statusbar, TRUE);
#else
  gtk_statusbar_set_has_resize_grip (priv->statusbar, FALSE);
#endif

#if GTK_CHECK_VERSION (2, 19, 1)
  statusbar_hbox = gtk_statusbar_get_message_area (statusbar);
  gtk_box_set_spacing (GTK_BOX (statusbar_hbox), 24);
#else
{
  GtkWidget *statusbar_label;
  GtkContainer *statusbar_frame;
  GList *list;

  /* Widget surgery: move the statusbar's label into a hbox
   * which we put in the statusbar's frame instead.
   */
  statusbar_hbox = gtk_hbox_new (FALSE, 24);
  list = gtk_container_get_children (GTK_CONTAINER (statusbar));
  statusbar_frame = GTK_CONTAINER (list->data);
  g_list_free (list);
  statusbar_label = gtk_bin_get_child (GTK_BIN (statusbar_frame));
  g_object_ref (statusbar_label);
  gtk_container_remove (statusbar_frame, statusbar_label);
  gtk_box_pack_start (GTK_BOX (statusbar_hbox), statusbar_label, TRUE, TRUE, 0);
  g_object_unref (statusbar_label);
  gtk_container_add (statusbar_frame, statusbar_hbox);
  gtk_widget_show (statusbar_hbox);
}
#endif /* GTK+ >= 2.19.1 */

  /* Score */
  priv->score_box = gtk_hbox_new (12, FALSE);
  label = gtk_label_new (_("Score:"));
  gtk_widget_show (label);
  gtk_box_pack_start (GTK_BOX (priv->score_box), label, FALSE, FALSE, 0);
  priv->score_label = gtk_label_new ("   0");
  gtk_widget_show (priv->score_label);
  gtk_box_pack_start (GTK_BOX (priv->score_box), priv->score_label, FALSE, FALSE, 0);
  gtk_box_pack_end (GTK_BOX (statusbar_hbox), priv->score_box, FALSE, FALSE, 0);

  games_atk_util_add_atk_relation (label, priv->score_label, ATK_RELATION_LABEL_FOR);
  games_atk_util_add_atk_relation (priv->score_label, label, ATK_RELATION_LABELLED_BY);

  time_box = gtk_hbox_new (12, FALSE);
  label = gtk_label_new (_("Time:"));
  gtk_box_pack_start (GTK_BOX (time_box), label, FALSE, FALSE, 0);
  priv->clock = games_clock_new ();
  gtk_box_pack_start (GTK_BOX (time_box), priv->clock, FALSE, FALSE, 0);
  gtk_box_pack_end (GTK_BOX (statusbar_hbox), time_box, FALSE, FALSE, 0);
  gtk_widget_show_all (time_box);

  games_atk_util_add_atk_relation (label, priv->clock, ATK_RELATION_LABEL_FOR);
  games_atk_util_add_atk_relation (priv->clock, label, ATK_RELATION_LABELLED_BY);
#endif /* !HAVE_HILDON */

  /* Load the UI after we've connected the statusbar,
   * otherwise not all actions will have statusbar help.
   */
  gtk_ui_manager_add_ui_from_string (priv->ui_manager, ui_description,
                                     strlen (ui_description), NULL);

  priv->main_menu = gtk_ui_manager_get_widget (priv->ui_manager, MAIN_MENU_UI_PATH);
  priv->toolbar = gtk_ui_manager_get_widget (priv->ui_manager, TOOLBAR_UI_PATH);

#ifdef MAEMO_TOOLBAR_BANNER
{
  GtkToolItem *tool_item;

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
}
#endif /* MAEMO_TOOLBAR_BANNER */

  /* Defer building the card themes menu until its parent's menu is opened */
#ifdef HAVE_HILDON
  /* FIXMEchpe check this works! */
  g_signal_connect (priv->main_menu, "show",
                    G_CALLBACK (main_menu_show_cb), window);
#else
  action = gtk_action_group_get_action (priv->action_group, "ViewMenu");
  g_signal_connect (action, "activate",
                    G_CALLBACK (view_menu_activate_cb), window);

  /* FIXMEchpe: this isn't right. We do want to hide the menu if there's no
   * entry in it after the list is updated...
   */
#if 0
  /* So the menu doesn't change size when the theme submenu is installed */
  action = gtk_action_group_get_action (priv->action_group, "ThemeMenu");
  g_object_set (action, "hide-if-empty", GINT_TO_POINTER (FALSE), NULL);
#endif
#endif

  /* It's possible that the themes list has already been loaded (e.g.
   * if the theme loading above involved the fallback); in that case
   * we need to update the menu right now.
   */
  if (games_card_themes_get_themes_loaded (priv->theme_manager))
    install_card_theme_menu (priv->theme_manager, window);

  /* Rebuild the themes menu when the themes list changes */
  g_signal_connect (priv->theme_manager, "changed",
                    G_CALLBACK (install_card_theme_menu), window);

  /* The actions and menus are done. The
   * recent games menu will be updated when the initial game loads.
   */

  accel_group = gtk_ui_manager_get_accel_group (priv->ui_manager);
  gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);

  action = gtk_action_group_get_action (priv->action_group, "Toolbar");
  priv->toolbar_visible = games_conf_get_boolean (NULL, aisleriot_conf_get_key (CONF_SHOW_TOOLBAR), NULL) != FALSE;
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
                                priv->toolbar_visible);
  action = gtk_action_group_get_action (priv->action_group, "ClickToMove");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
                                games_conf_get_boolean (NULL, aisleriot_conf_get_key (CONF_CLICK_TO_MOVE), NULL));

  action = gtk_action_group_get_action (priv->action_group, "RecentMenu");
  g_object_set (action, "hide-if-empty", FALSE, NULL);

#ifdef ENABLE_SOUND
  action = gtk_action_group_get_action (priv->action_group, "Sound");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
                                games_conf_get_boolean (NULL, aisleriot_conf_get_key (CONF_SOUND), NULL));
  gtk_action_set_visible (action, games_sound_is_available ());
#endif /* ENABLE_SOUND */

#ifndef HAVE_HILDON
  action = gtk_action_group_get_action (priv->action_group, "Statusbar");
  priv->statusbar_visible = games_conf_get_boolean (NULL, aisleriot_conf_get_key (CONF_SHOW_STATUSBAR), NULL) != FALSE;
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
                                priv->statusbar_visible);
#endif /* !HAVE_HILDON */

#ifdef ENABLE_CARD_THEMES_INSTALLER
  action = gtk_action_group_get_action (priv->action_group, "InstallThemes");
  gtk_action_set_sensitive (action, games_card_themes_can_install_themes (priv->theme_manager));
#endif /* ENABLE_CARD_THEMES_INSTALLER */

  set_fullscreen_actions (window, FALSE);

#ifdef HAVE_CLUTTER
  action = gtk_action_group_get_action (priv->action_group, "Animations");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
                                games_conf_get_boolean (NULL, aisleriot_conf_get_key (CONF_ANIMATIONS), NULL));

#endif /* HAVE_CLUTTER */

#if defined(HAVE_CLUTTER) || (defined(ENABLE_SOUND) && GTK_CHECK_VERSION (2, 14, 0))
  /* Set the action visibility and listen for animation and sound mode changes */
  screen_changed_cb (GTK_WIDGET (window), NULL, window);
  g_signal_connect (window, "screen-changed",
                    G_CALLBACK (screen_changed_cb), window);
#endif /* HAVE_CLUTTER || ENABLE_SOUND && GTK+ >= 2.14.0 */

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
  /* FIXMEchpe: find out if this is fixed on maemo4 or maemo5 */
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
}

static void
aisleriot_window_dispose (GObject *object)
{
  AisleriotWindow *window = AISLERIOT_WINDOW (object);
  AisleriotWindowPrivate *priv = window->priv;
  
#ifdef HAVE_CLUTTER
  g_signal_handlers_disconnect_by_func (gtk_widget_get_settings (GTK_WIDGET (window)),
                                        G_CALLBACK (settings_changed_cb),
                                        window);
#endif /* HAVE_CLUTTER */

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
  
#ifdef LEAVE_FULLSCREEN_BUTTON
  if (priv->fullscreen_button != NULL) {
    gtk_widget_destroy (GTK_WIDGET (priv->fullscreen_button));
    priv->fullscreen_button = NULL;
  }
#endif

  if (priv->load_idle_id != 0) {
    g_source_remove (priv->load_idle_id);
    priv->load_idle_id = 0;
  }

  G_OBJECT_CLASS (aisleriot_window_parent_class)->dispose (object);
}

static void
aisleriot_window_finalize (GObject *object)
{
  AisleriotWindow *window = AISLERIOT_WINDOW (object);
  AisleriotWindowPrivate *priv = window->priv;

#ifdef HAVE_CLUTTER
  g_object_unref (priv->board_style);
#endif /* HAVE_CLUTTER */

  if (priv->theme) {
    g_object_unref (priv->theme);
  }

  g_object_unref (priv->theme_manager);

  g_signal_handlers_disconnect_matched (priv->game,
                                        G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL, NULL, window);
  g_object_unref (priv->game);

  G_OBJECT_CLASS (aisleriot_window_parent_class)->finalize (object);
}

static void
aisleriot_window_set_property (GObject      *object,
                               guint         property_id,
                               const GValue *value,
                               GParamSpec   *pspec)
{
  AisleriotWindow *window = AISLERIOT_WINDOW (object);

  switch (property_id) {
    case PROP_FREECELL_MODE:
      aisleriot_window_set_freecell_mode (window, g_value_get_boolean (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}
static void
aisleriot_window_class_init (AisleriotWindowClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  gobject_class->dispose = aisleriot_window_dispose;
  gobject_class->finalize = aisleriot_window_finalize;
  gobject_class->set_property = aisleriot_window_set_property;

  widget_class->window_state_event = aisleriot_window_state_event;
#if GTK_CHECK_VERSION (2, 10, 0)
  widget_class->style_set = aisleriot_window_style_set;
#endif

  g_type_class_add_private (gobject_class, sizeof (AisleriotWindowPrivate));

  /**
   * AisleriotWindow:freecell-mode:
   *
   * Whether the window is in freecell mode.
   */
  g_object_class_install_property
    (gobject_class,
     PROP_FREECELL_MODE,
     g_param_spec_boolean ("freecell-mode", NULL, NULL,
                           FALSE,
                           G_PARAM_WRITABLE |
                           G_PARAM_CONSTRUCT_ONLY |
                           G_PARAM_STATIC_STRINGS));
}

/* public API */

/**
 * aisleriot_window_new:
 *
 * Returns: a new #AisleriotWindow
 */
GtkWidget *
aisleriot_window_new (gboolean freecell_mode)
{
  return g_object_new (AISLERIOT_TYPE_WINDOW,
                       "freecell-mode", freecell_mode,
                       NULL);
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

    name = games_filename_to_display_name (data->game_file);

    dialog = gtk_message_dialog_new (GTK_WINDOW (data->window),
                                     GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_OK,
                                     _("Cannot start the game “%s”"),
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
