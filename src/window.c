/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2; tab-width: 2 -*- */
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

#include <glib.h>
#include <glib/gi18n.h>

#include <gdk/gdk.h>
#include <gtk/gtk.h>

#include "ar-clock.h"
#include "ar-debug.h"
#include "ar-stock.h"
#include "ar-runtime.h"
#include "ar-sound.h"
#include "ar-string-utils.h"
#include "ar-gsettings.h"

#include "board-noclutter.h"

#include "ar-card-theme.h"
#include "ar-card-themes.h"
#include "conf.h"
#include "game.h"
#include "stats-dialog.h"
#include "util.h"
#include "ar-game-chooser.h"

#include "window.h"


#define MIN_WIDTH 800
#define MIN_HEIGHT 600

#define MAIN_MENU_UI_PATH       "/MainMenu"
#define RECENT_GAMES_MENU_PATH  MAIN_MENU_UI_PATH "/GameMenu/RecentMenu"
#define OPTIONS_MENU_PATH       MAIN_MENU_UI_PATH "/OptionsMenu"
#define CARD_THEMES_MENU_PATH   MAIN_MENU_UI_PATH "/ViewMenu/ThemeMenu/ThemesPH"
#define TOOLBAR_UI_PATH         "/Toolbar"

/* The maximum number of recent games saved */
#define MAX_RECENT 5

#define AR_SETTINGS_PATH_PREFIX "/org/gnome/solitaire/"
#define AR_SETTINGS_WINDOW_STATE_PATH AR_SETTINGS_PATH_PREFIX "window-state/"


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
  ACTION_LEAVE_FULLSCREEN,
  LAST_ACTION
};

enum {
  OPTION_CHECKMENU,
  OPTION_RADIOMENU
};

/* Game Over dialogue */

enum {
  RESPONSE_UNDO = 1,
  RESPONSE_RESTART = 2,
  RESPONSE_NEW_GAME = 3
};


struct _AisleriotWindow
{
  GtkApplicationWindow parent_instance;

  AisleriotGame *game;
  ArStyle *board_style;
  AisleriotBoard *board;

  ArCardThemes *theme_manager;
  ArCardTheme *theme;

  GtkStatusbar *statusbar;
  guint game_message_id;
  guint board_message_id;
  GtkWidget *score_box;
  GtkWidget *score_label;
  GtkWidget *clock;

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

  GtkWidget *hint_dialog;

#ifdef LEAVE_FULLSCREEN_BUTTON
  GtkWidget *fullscreen_button;
#endif

  guint load_idle_id;

  guint changing_game_type : 1;
  guint toolbar_visible : 1;
  guint statusbar_visible : 1;
  guint fullscreen : 1;
};


G_DEFINE_TYPE (AisleriotWindow, aisleriot_window, GTK_TYPE_APPLICATION_WINDOW);


static void
window_clear_statusbar (AisleriotWindow *window)
{
  /* Need to clear the statusbar *before* changing the game type
   * or starting a new game, since the type change/new game signals
   * are emitted after the new statusbar message has already
   * been set.
   */
  gtk_statusbar_pop (window->statusbar, window->game_message_id);
  gtk_statusbar_pop (window->statusbar, window->board_message_id);
}

static void
window_restart_game(AisleriotWindow* window)
{
  window_clear_statusbar (window);
  aisleriot_game_restart_game (window->game);

  gtk_widget_grab_focus (GTK_WIDGET (window->board));
}

static void
window_new_game_with_rand(AisleriotWindow* window,
                          GRand* rand)
{
  window_clear_statusbar(window);
  aisleriot_game_new_game_with_rand (window->game, rand);

  gtk_widget_grab_focus (GTK_WIDGET (window->board));
}

static void
window_new_game(AisleriotWindow* window)
{
  window_new_game_with_rand(window, NULL);
}

static void
game_over_dialog_response_cb (GtkWidget *dialog,
                              int response,
                              AisleriotWindow *window)
{
  gtk_widget_destroy (dialog);

  switch (response) {
    case RESPONSE_UNDO:
      aisleriot_game_undo_move (window->game);
      break;
    case RESPONSE_RESTART:
      window_restart_game(window);
      break;
    case RESPONSE_NEW_GAME:
      window_new_game(window);
      break;
    case GTK_RESPONSE_CLOSE:
      gtk_widget_destroy (GTK_WIDGET (window)); /* this will quit */
      break;
    default:
      /* The player closed the dialog window from the window border
       * close button. Just start a new game.
       */
      window_new_game(window);
      break;
  } 
}

static void
show_game_over_dialog (AisleriotWindow *window)
{
  GtkWidget *dialog;
  const char *message;
  gboolean game_won;

  game_won = aisleriot_game_get_state (window->game) == GAME_WON;

  if (game_won) {
    message = _("Congratulations, you have won!");
    ar_sound_play ("victory");

  } else {
    message =  _("There are no more moves");
    ar_sound_play ("splat");
  }

  dialog = gtk_message_dialog_new_with_markup (GTK_WINDOW (window),
					       GTK_DIALOG_DESTROY_WITH_PARENT,
					       GTK_MESSAGE_INFO,
					       GTK_BUTTONS_NONE,
                                               "<b>%s</b>",
                                               message);

  gtk_window_set_title (GTK_WINDOW (dialog), "");
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);

  if (game_won) {
    gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                            GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE,
                            AR_STOCK_START_NEW_GAME, RESPONSE_NEW_GAME,
                            NULL);
    gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
                                             RESPONSE_NEW_GAME,
                                             GTK_RESPONSE_CLOSE,
                                             -1);
  } else {
    gtk_dialog_add_buttons (GTK_DIALOG (dialog),
			    AR_STOCK_UNDO_MOVE, RESPONSE_UNDO,
                            GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE,
                            AR_STOCK_RESTART_GAME, RESPONSE_RESTART,
                            AR_STOCK_START_NEW_GAME, RESPONSE_NEW_GAME,
                            NULL);
    gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
                                             RESPONSE_NEW_GAME,
                                             RESPONSE_RESTART,
                                             GTK_RESPONSE_CLOSE,
                                             RESPONSE_UNDO,
                                             -1);
  }

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), RESPONSE_NEW_GAME);

  window->game_over_dialog = dialog;
  g_signal_connect (dialog, "response",
                    G_CALLBACK (game_over_dialog_response_cb), window);
  g_signal_connect (dialog, "destroy",
                    G_CALLBACK (gtk_widget_destroyed), &window->game_over_dialog);

  gtk_widget_show (dialog);
}

/* Statistics dialogue */

static void
update_statistics_display (AisleriotWindow *window)
{
  AisleriotStatistic current_stats;
  char *game_name;

  if (!window->stats_dialog)
    return;

  game_name = aisleriot_game_get_name (window->game);
  aisleriot_stats_dialog_set_name (window->stats_dialog, game_name);
  g_free (game_name);

  aisleriot_conf_get_statistic (aisleriot_game_get_game_module (window->game),
                                &current_stats);

  aisleriot_stats_dialog_update (window->stats_dialog, &current_stats);
}

static void
stats_dialog_response_cb (GtkWidget *widget,
                          int response,
                          AisleriotWindow *window)
{
  if (response == GTK_RESPONSE_REJECT) {
    AisleriotStatistic current_stats = { 0, 0, 0, 0 };

    aisleriot_conf_set_statistic (aisleriot_game_get_game_module (window->game),
                                  &current_stats);
    aisleriot_stats_dialog_update (window->stats_dialog, &current_stats);

    return;
  }

  gtk_widget_destroy (widget);
}

/* action methods */

void
aisleriot_window_new_game (AisleriotWindow *window)
{
  window_new_game(window);
}

void
aisleriot_window_change_game (AisleriotWindow *window)
{
  if (window->game_choice_dialog) {
    gtk_window_present (GTK_WINDOW (window->game_choice_dialog));
    return;
  }

  window->game_choice_dialog = ar_game_chooser_new (window);
  g_signal_connect (window->game_choice_dialog, "destroy",
                    G_CALLBACK (gtk_widget_destroyed), &window->game_choice_dialog);

  gtk_window_present (GTK_WINDOW (window->game_choice_dialog));
}

void
aisleriot_window_show_statistics_dialog (AisleriotWindow *window)
{
  if (!window->stats_dialog) {
    window->stats_dialog = aisleriot_stats_dialog_new ();
    gtk_window_set_transient_for (GTK_WINDOW (window->stats_dialog),
                                  GTK_WINDOW (window));

    g_signal_connect (window->stats_dialog, "response",
                      G_CALLBACK (stats_dialog_response_cb), window);
    g_signal_connect (window->stats_dialog, "destroy",
                      G_CALLBACK (gtk_widget_destroyed), &window->stats_dialog);
  }

  update_statistics_display (window);

  gtk_window_present (GTK_WINDOW (window->stats_dialog));
}

void
aisleriot_window_show_about_dialog (AisleriotWindow * window)
{
  const char *authors[] = {
    _("Main game:"),
    "Jonathan Blandford <jrb@gnome.org>",
    "Felix Bellaby <felix@pooh.u-net.com>",
    "Rosanna Yuen <zana@gnome.org>",
    "Callum McKenzie <callum@physics.otago.ac.nz>",
    "Christian Persch <chpe" "\100" "src.gnome.org>",
    "Andreas Røsdal <andreasr" "\100" "gnome.org>",
    "",
    _("Card games:"),
    "Jonathan Blandford <jrb@redhat.com>",
    "W. Borgert <debacle@debian.org>",
    "Robert Brady <rwb197@ecs.soton.ac.uk>",
    "Nick Lamb <njl195@zepler.org.uk>",
    "Changwoo Ryu <cwryu@adam.kaist.ac.kr>",
    "Matthew Wilcox <matthew@wil.cx>",
    "Rosanna Yuen <zana@gnome.org>",
    "Alan Horkan <horkana@maths.tcd.ie>",
    "Richard Hoelscher <rah@rahga.com>",
    "Vincent Povirk",
    "Sapphire Becker",
    "Gwyneth Morgan",
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
    /* Guyenne Classic and Swiss cards */
    "Mario Frasca <mariotomo@gmail.com>",
    /* FIXMEchpe: who did the Bonded cards? */
    /* Tango Nuevo */
    "\"Thom-10\"",
    /* Tango Nuevo, Ancient French, Simplistic,
     * Атласные (Atlasnye), Atlasnye (International), Atlasnye Skat,
     * Tigullio, Plastic, Neoclassical, Neoclassical 4 Color,
     * fixes and improvements to other decks.
     */
    "Vincent Bermel",
    /* Атласные (Atlasnye), Atlasnye (International), Atlasnye Skat,
     */
    "Дмитрий Фомин",
    /* Tigullio */
    "Luciano Montanaro",
    /* Plastic */
    "Gifford Cheung",
    /* Neoclassical */
    "Charles Esquiaqui",

    NULL
  };
  const char *documenters[] = {
    "Rosanna Yuen <zana@webwynk.net>",
    NULL
  };

  char *licence;

  licence = ar_get_licence ("AisleRiot");

  gtk_show_about_dialog (GTK_WINDOW (window),
                         "program-name",
                         _("AisleRiot"),
                         "version", VERSION,
                         "title", _("About AisleRiot"),
                         "comments",
                         _("AisleRiot provides a rule-based solitaire "
                           "card engine that allows many different "
                           "games to be played."),
                         "copyright", "Copyright © 1998-2006 Jonathan Blandford\n"
                                      "Copyright © 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015 Christian Persch",
                         "license", licence,
                         "authors", authors,
                         "artists", artists,
                         "documenters", documenters,
                         "translator-credits", _("translator-credits"),
                         "logo-icon-name", "gnome-aisleriot",
                         "website", "http://wiki.gnome.org/Apps/Aisleriot",
                         "website-label", _("AisleRiot web site"),
                         "wrap-license", TRUE,
                        NULL);
  g_free (licence);
}

/* action callbacks */

static void
new_game_cb (GtkAction *action,
             AisleriotWindow *window)
{
  aisleriot_window_new_game (window);
}

static void
undo_cb (GtkAction *action,
         AisleriotWindow *window)
{
  /* If a move is in progress, cancel it before changing the game! */
  aisleriot_board_abort_move (window->board);

  aisleriot_game_undo_move (window->game);
}

static void
redo_cb (GtkAction *action,
         AisleriotWindow *window)
{
  aisleriot_board_abort_move (window->board);

  aisleriot_game_redo_move (window->game);
}

static void
help_about_cb (GtkAction *action,
               AisleriotWindow *window)
{
  aisleriot_window_show_about_dialog (window);
}

static void
restart_game (GtkAction *action,
              AisleriotWindow *window)
{
  window_restart_game (window);
};

static void
select_game_cb (GtkAction *action,
                AisleriotWindow *window)
{
  aisleriot_window_change_game (window);
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
  const char *game_module;
  
  game_module = aisleriot_game_get_game_module (window->game);
  aisleriot_show_help (GTK_WIDGET (window), game_module);
}

static void
help_overlay_cb (GSimpleAction *action,
                 AisleriotWindow *window)
{
    g_action_group_activate_action (G_ACTION_GROUP (window), "show-help-overlay", NULL);
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
  aisleriot_window_show_statistics_dialog (window);
}

static void
install_themes_cb (GtkAction *action,
                   AisleriotWindow *window)
{
  ar_card_themes_install_themes (window->theme_manager,
                                 GTK_WIDGET (window),
                                 gtk_get_current_event_time ());
}

#ifdef ENABLE_DEBUG_UI

static void
debug_exception_cb (GtkAction *action,
                    AisleriotWindow *window)
{
  aisleriot_game_generate_exception (window->game);
}

#define DEBUG_WINDOW_DATA_KEY "debug-data"

typedef struct {
  AisleriotWindow *window;
  char **games;
  int n_games;
  int current;
} DebugWindowData;

static void
debug_data_free (DebugWindowData *data)
{
  g_strfreev (data->games);

  g_slice_free (DebugWindowData, data);
}

static DebugWindowData *
debug_ensure_game_list (AisleriotWindow *window)
{
  DebugWindowData *data;
  char **games;
  int n_games;
  const char *current_game_module;
  int i;

  data = g_object_get_data (G_OBJECT (window), DEBUG_WINDOW_DATA_KEY);
  if (data != NULL)
    return data;

  games = ar_get_game_modules ();
  if (games == NULL)
    return NULL;

  n_games = g_strv_length (games);
  if (n_games == 0) {
    g_strfreev (games);
    return NULL;
  }

  data = g_slice_new (DebugWindowData);
  data->window = window;
  data->games = games;
  data->n_games = n_games;
  data->current = -1;

  current_game_module = aisleriot_game_get_game_module (window->game);
  for (i = 0; data->games[i]; ++i) {
    if (strcmp (data->games[i], current_game_module) == 0) {
      data->current = i;
      break;
    }
  }

  g_object_set_data_full (G_OBJECT (window), DEBUG_WINDOW_DATA_KEY,
                          data, (GDestroyNotify) debug_data_free);

  return data;
}

static gboolean
debug_cycle_timeout_cb (DebugWindowData *data)
{
  if (data->current >= -1)
    data->current++;

  /* We're done */
  if (data->current >= data->n_games) {
    data->current = data->n_games - 1;
    return FALSE; /* don't run again */
  }

  aisleriot_window_set_game_module (data->window, data->games[data->current], NULL);

  return TRUE; /* run again */
}

static void
debug_cycle_cb (GtkAction *action,
                AisleriotWindow *window)
{
  DebugWindowData *data;

  data = debug_ensure_game_list (window);
  if (data == NULL)
    return;

  g_timeout_add (500, (GSourceFunc) debug_cycle_timeout_cb, data);
}

static void
debug_game_first (GtkAction *action,
                  AisleriotWindow *window)
{
  DebugWindowData *data;

  data = debug_ensure_game_list (window);
  if (data == NULL)
    return;

  data->current = 0;
  aisleriot_window_set_game_module (data->window, data->games[data->current], NULL);
}

static void
debug_game_last (GtkAction *action,
                 AisleriotWindow *window)
{
  DebugWindowData *data;

  data = debug_ensure_game_list (window);
  if (data == NULL)
    return;

  data->current = data->n_games - 1;
  aisleriot_window_set_game_module (data->window, data->games[data->current], NULL);
}

static void
debug_game_next (GtkAction *action,
                 AisleriotWindow *window)
{
  DebugWindowData *data;

  data = debug_ensure_game_list (window);
  if (data == NULL || data->current + 1 == data->n_games)
    return;

  data->current++;
  aisleriot_window_set_game_module (data->window, data->games[data->current], NULL);
}

static void
debug_game_prev (GtkAction *action,
                 AisleriotWindow *window)
{
  DebugWindowData *data;

  data = debug_ensure_game_list (window);
  if (data == NULL || data->current == 0)
    return;

  data->current--;
  aisleriot_window_set_game_module (data->window, data->games[data->current], NULL);
}

static void
debug_choose_seed_response_cb (GtkWidget *dialog,
                               int response,
                               AisleriotWindow *window)
{
  if (response == GTK_RESPONSE_OK) {
    GtkEntry *entry;
    const char *text;
    char *endptr;
    guint seed;
    GRand *rand;

    entry = g_object_get_data (G_OBJECT (dialog), "entry");
    text = gtk_entry_get_text (entry);

    errno = 0;
    seed = g_ascii_strtoull (text, &endptr, 10);
    if (errno == 0 && endptr != text) {
      rand = g_rand_new_with_seed (seed);

      window_new_game_with_rand (window, rand /* adopts */);
    }
  }

  gtk_widget_destroy (dialog);
}

static void
debug_choose_seed_cb (GtkAction *action,
                      AisleriotWindow *window)
{
  GtkWidget *dialog, *entry;

  dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                   GTK_DIALOG_DESTROY_WITH_PARENT |
                                   GTK_DIALOG_MODAL,
                                   GTK_MESSAGE_QUESTION,
                                   GTK_BUTTONS_OK_CANCEL,
                                   "%s", "Choose game seed");
  g_signal_connect (dialog, "response",
                    G_CALLBACK (debug_choose_seed_response_cb), window);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

  entry = gtk_entry_new ();

  #if 0
{
  char str[32];
  g_snprintf (str, sizeof (str), "%u", aisleriot_game_get_seed (window->game));
  gtk_entry_set_text (GTK_ENTRY (entry), str);
}
#endif

  gtk_box_pack_end (GTK_BOX (gtk_message_dialog_get_message_area (GTK_MESSAGE_DIALOG (dialog))), entry, FALSE, FALSE, 0);
  gtk_widget_show (entry);
  g_object_set_data (G_OBJECT (dialog), "entry", entry);
  gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);

  gtk_window_present (GTK_WINDOW (dialog));
}

#endif /* ENABLE_DEBUG_UI */

static void
set_fullscreen_button_active (AisleriotWindow *window)
{
#ifdef LEAVE_FULLSCREEN_BUTTON
  gboolean active;

  active = window->fullscreen && !window->toolbar_visible;
  if (!active) {
    if (window->fullscreen_button != NULL) {
      ar_fullscreen_button_set_active (AR_FULLSCREEN_BUTTON (window->fullscreen_button), FALSE);
    }

    return;
  }

  if (active && window->fullscreen_button == NULL) {
    window->fullscreen_button = ar_fullscreen_button_new (GTK_WINDOW (window),
                                                        GTK_CORNER_TOP_RIGHT);
  }

  ar_fullscreen_button_set_active (AR_FULLSCREEN_BUTTON (window->fullscreen_button), TRUE);
#endif /* LEAVE_FULLSCREEN_BUTTON */
}

static void
toolbar_toggled_cb (GtkToggleAction *action,
                    AisleriotWindow *window)
{
  gboolean state;

  state = gtk_toggle_action_get_active (action);

  g_object_set (window->toolbar, "visible", state, NULL);

  window->toolbar_visible = state != FALSE;

  ar_conf_set_boolean (NULL, aisleriot_conf_get_key (CONF_SHOW_TOOLBAR), state);

  set_fullscreen_button_active (window);
}

static void
statusbar_toggled_cb (GtkToggleAction *action,
                      AisleriotWindow *window)
{
  gboolean state;

  state = gtk_toggle_action_get_active (action);

  g_object_set (window->statusbar, "visible", state, NULL);

  /* Only update the clock continually if it's visible */
  ar_clock_set_update (AR_CLOCK (window->clock), state);

  window->statusbar_visible = state != FALSE;

  ar_conf_set_boolean (NULL, aisleriot_conf_get_key (CONF_SHOW_STATUSBAR), state);
}

static void
set_fullscreen_actions (AisleriotWindow *window,
                        gboolean is_fullscreen)
{
  window->fullscreen = is_fullscreen;

  g_object_set (window->main_menu, "visible", !is_fullscreen, NULL);

  gtk_action_set_visible (window->action[ACTION_LEAVE_FULLSCREEN], is_fullscreen);
  g_object_set (gtk_ui_manager_get_widget (window->ui_manager, "/Toolbar/LeaveFullscreenSep"),
                "visible", is_fullscreen,
                "draw", FALSE,
                NULL);

  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (window->action[ACTION_FULLSCREEN]),
                                is_fullscreen);
}

static void
fullscreen_toggled_cb (GtkToggleAction *action,
                       GtkWindow *window)
{
  ar_debug_print (AR_DEBUG_WINDOW_STATE,
                      "[window %p] fullscreen_toggled_cb, %s fullscreen\n",
                      window,
                      gtk_toggle_action_get_active (action) ? "going" : "leaving");

  if (gtk_toggle_action_get_active (action)) {
    gtk_window_fullscreen (window);
  } else {
    gtk_window_unfullscreen (window);
  }
}

static void
leave_fullscreen_cb (GtkAction *action,
                     GtkWindow *window)
{
  gtk_window_unfullscreen (window);
}

static void
clickmove_toggle_cb (GtkToggleAction *action,
                     AisleriotWindow *window)
{
  gboolean click_to_move;

  click_to_move = gtk_toggle_action_get_active (action);

  aisleriot_game_set_click_to_move (window->game, click_to_move);
  ar_style_set_click_to_move (window->board_style, click_to_move);
  
  ar_conf_set_boolean (NULL, aisleriot_conf_get_key (CONF_CLICK_TO_MOVE), click_to_move);
}

static void
sound_toggle_cb (GtkToggleAction *action,
                 AisleriotWindow *window)
{
#ifdef ENABLE_SOUND
  gboolean sound_enabled;

  sound_enabled = gtk_toggle_action_get_active (action);

  ar_sound_enable (sound_enabled);
  
  ar_conf_set_boolean (NULL, aisleriot_conf_get_key (CONF_SOUND), sound_enabled);
#endif /* ENABLE_SOUND */
}

static void
show_hint_cb (GtkAction *action,
              AisleriotWindow *window)
{
  char *message;
  GtkWidget *dialog;

  /* If the game hasn't started yet, getting a hint starts it */
  aisleriot_game_start (window->game);

  if (window->hint_dialog) {
    gtk_widget_destroy (window->hint_dialog);
    window->hint_dialog = NULL;
  }

  /* Only can show hints for running games */
  if (aisleriot_game_get_state (window->game) != GAME_RUNNING)
    return;

  message = aisleriot_game_get_hint (window->game);
  if (!message)
    return;

  dialog = gtk_message_dialog_new_with_markup (GTK_WINDOW (window),
                                               GTK_DIALOG_DESTROY_WITH_PARENT,
                                               GTK_MESSAGE_INFO,
                                               GTK_BUTTONS_OK,
                                               "<b>%s</b>", message);
  gtk_window_set_title (GTK_WINDOW (dialog), "");
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);

  gtk_widget_show (dialog);

  window->hint_dialog = dialog;
  g_signal_connect (dialog, "response",
                    G_CALLBACK (gtk_widget_destroy), NULL);
  g_signal_connect (dialog, "destroy",
                    G_CALLBACK (gtk_widget_destroyed), &window->hint_dialog);

  g_free (message);
}

static void
deal_cb (GtkAction *action,
         AisleriotWindow *window)
{

  aisleriot_game_deal_cards (window->game);
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
  value = g_ascii_strtoull (action_name + strlen ("Option"), NULL, 10);

  /* g_print ("option %s changed, value=%x set=%d\n", action_name, value, active); */

  *changed_mask |= value;
  if (active)
    *changed_value |= value;
}

static void
option_cb (GtkToggleAction *action,
           AisleriotWindow *window)
{
  gboolean active;
  guint32 changed_mask = 0, changed_value = 0, value;

  /* Don't change the options if we're just installing the options menu */
  if (window->changing_game_type)
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

  value = aisleriot_game_change_options (window->game, changed_mask, changed_value);

  aisleriot_conf_set_options (aisleriot_game_get_game_module (window->game), (int) value);

  /* Now re-deal, so the option is applied */
  window_new_game (window);
}

static void
install_options_menu (AisleriotWindow *window)
{
  GList *options, *l;
  int options_value = 0;
  GSList *radiogroup = NULL;
  int radion = 0;

  if (window->options_merge_id != 0) {
    gtk_ui_manager_remove_ui (window->ui_manager, window->options_merge_id);
    window->options_merge_id = 0;
  }

  if (window->options_group) {
    gtk_ui_manager_remove_action_group (window->ui_manager, window->options_group);
    window->options_group = NULL;
  }

  /* See gtk bug #424448 */
  gtk_ui_manager_ensure_update (window->ui_manager);

  /* Only apply the options if they exist. Otherwise the options in the menu
   * and the real game options are out of sync until first changed by the user.
   */
  if (aisleriot_conf_get_options (aisleriot_game_get_game_module (window->game), &options_value)) {
    aisleriot_game_change_options (window->game, AISLERIOT_GAME_OPTIONS_MAX, options_value);
  }

  /* To get radio buttons in the menu insert an atom into the option list
   * in your scheme code. To get back out of radio-button mode insert 
   * another atom. The exact value of the atoms is irrelevant - they merely
   * trigger a toggle - but descriptive names like begin-exclusive and
   * end-exclusive are probably a good idea.
   */
  options = aisleriot_game_get_options (window->game);
  if (!options)
    return;

  window->options_group = gtk_action_group_new ("Options");
  gtk_ui_manager_insert_action_group (window->ui_manager, window->options_group, -1);
  g_object_unref (window->options_group);

  window->options_merge_id = gtk_ui_manager_new_merge_id (window->ui_manager);

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

    gtk_action_group_add_action (window->options_group, GTK_ACTION (action));
    g_object_unref (action);

    gtk_ui_manager_add_ui (window->ui_manager,
                           window->options_merge_id,
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
                          const char *game_module)
{
  char **recent_games, **new_recent;
  gsize i, n_recent = 0, n_new_recent = 0;

  if (!game_module)
    return;

  recent_games = ar_conf_get_string_list (NULL, aisleriot_conf_get_key (CONF_RECENT_GAMES), &n_recent, NULL);

  if (recent_games == NULL) {
    new_recent = g_new (char *, 2);
    new_recent[0] = g_strdup (game_module);
    new_recent[1] = NULL;
    n_new_recent = 1;
  } else {
    new_recent = g_new (char *, MIN (n_recent + 1, MAX_RECENT) + 1);
    n_new_recent = 0;

    new_recent[n_new_recent++] = g_strdup (game_module);

    for (i = 0; i < n_recent && n_new_recent < MAX_RECENT; ++i) {
      if (g_ascii_strcasecmp (game_module, recent_games[i]) != 0) {
        new_recent[n_new_recent++] = g_strdup (recent_games[i]);
      }
    }

    /* NULL termination */
    new_recent[n_new_recent] = NULL;

    g_strfreev (recent_games);
  }

  ar_conf_set_string_list (NULL, aisleriot_conf_get_key (CONF_RECENT_GAMES),
                              (const char * const *) new_recent, n_new_recent);
  g_strfreev (new_recent);
}

static void
recent_game_cb (GtkAction *action,
                AisleriotWindow *window)
{
  const char *game_module;

  game_module = g_object_get_data (G_OBJECT (action), "game");
  g_return_if_fail (game_module != NULL);

  aisleriot_window_set_game_module (window, game_module, NULL);

  ar_conf_set_string (NULL, aisleriot_conf_get_key (CONF_VARIATION), game_module);
}

static void
install_recently_played_menu (AisleriotWindow *window)
{
  char **recent_games;
  gsize i, n_recent = 0;

  /* Clean out the old menu */
  if (window->recent_games_merge_id != 0) {
    gtk_ui_manager_remove_ui (window->ui_manager, window->recent_games_merge_id);
  }
  if (window->recent_games_group != NULL) {
    gtk_ui_manager_remove_action_group (window->ui_manager, window->recent_games_group);
  }

  /* See gtk bug #424448 */
  gtk_ui_manager_ensure_update (window->ui_manager);

  window->recent_games_group = gtk_action_group_new ("Recent");
  gtk_ui_manager_insert_action_group (window->ui_manager, window->recent_games_group, -1);
  g_object_unref (window->recent_games_group);

  window->recent_games_merge_id = gtk_ui_manager_new_merge_id (window->ui_manager);

  recent_games = ar_conf_get_string_list (NULL, aisleriot_conf_get_key (CONF_RECENT_GAMES), &n_recent, NULL);

  for (i = 0; i < n_recent; ++i) {
    GtkAction *action;
    char actionname[32];
    char *game_name, *tooltip;

    g_snprintf (actionname, sizeof (actionname), "Recent%"G_GSIZE_FORMAT, i);
    game_name = ar_filename_to_display_name (recent_games[i]);
    tooltip = g_strdup_printf (_("Play “%s”"), game_name);
    action = gtk_action_new (actionname, game_name, tooltip, NULL);
    g_free (game_name);
    g_free (tooltip);
 
    g_object_set_data_full (G_OBJECT (action), "game",
                            ar_filename_to_game_module (recent_games[i]),
                            (GDestroyNotify) g_free);
    g_signal_connect (action, "activate",
                      G_CALLBACK (recent_game_cb), window);
    gtk_action_group_add_action (window->recent_games_group, action);
    g_object_unref (action);

    gtk_ui_manager_add_ui (window->ui_manager,
                           window->recent_games_merge_id,
                           RECENT_GAMES_MENU_PATH,
                           actionname, actionname,
                           GTK_UI_MANAGER_MENUITEM, FALSE);
  }

  g_strfreev (recent_games);
}

/* Card Theme menu */

static void
aisleriot_window_take_card_theme (AisleriotWindow *window,
                                  ArCardTheme *theme /* adopting */)
{
  GtkWidget *widget = GTK_WIDGET (window);

  if (theme == window->theme)
    return;

  if (window->theme) {
    g_object_unref (window->theme);
  }
  window->theme = theme;

  if (gtk_widget_has_screen (widget)) {
    const cairo_font_options_t *font_options;

    font_options = gdk_screen_get_font_options (gtk_widget_get_screen (widget));
    ar_card_theme_set_font_options (theme, font_options);
  }

  ar_style_set_card_theme (window->board_style, theme);
}

static void
card_theme_changed_cb (GtkToggleAction *action,
                       AisleriotWindow *window)
{
  ArCardThemeInfo *current_theme_info = NULL, *new_theme_info;
  ArCardTheme *theme;
  const char *theme_name;

  if (!gtk_toggle_action_get_active (action))
    return;

  new_theme_info = g_object_get_data (G_OBJECT (action), "theme-info");
  g_assert (new_theme_info != NULL);

  if (window->theme) {
    current_theme_info = ar_card_theme_get_theme_info (window->theme);
  }

  if (ar_card_theme_info_equal (new_theme_info, current_theme_info))
    return;

  theme = ar_card_themes_get_theme (window->theme_manager, new_theme_info);
  if (!theme) {
    GSList *group, *l;

    gtk_widget_error_bell (GTK_WIDGET (window));

    /* Set this action insensitive so we don't try again */
    gtk_action_set_sensitive (GTK_ACTION (action), FALSE);

    /* Re-set the radio action of the current theme to active */
    group = gtk_radio_action_get_group (GTK_RADIO_ACTION (action));
    for (l = group; l != NULL; l = l->next) {
      GtkToggleAction *theme_action = GTK_TOGGLE_ACTION (l->data);
      ArCardThemeInfo *info;

      if (theme_action == action)
        continue;

      info = g_object_get_data (G_OBJECT (theme_action), "theme-info");
      if (!ar_card_theme_info_equal (info, current_theme_info))
        continue;

      /* The check at the top will prevent an infinite loop */
      gtk_toggle_action_set_active (theme_action, TRUE);
      break;
    }

    return;
  }

  aisleriot_window_take_card_theme (window, theme);

  theme_name = ar_card_theme_info_get_persistent_name (new_theme_info);
  ar_conf_set_string (NULL, aisleriot_conf_get_key (CONF_THEME), theme_name);
}

static void
list_element_unref_cb (gpointer data,
                      gpointer user_data)
{
  ar_card_theme_info_unref (data);
}
static void
install_card_theme_menu (ArCardThemes *theme_manager,
                         AisleriotWindow *window)
{
  GList *list, *l;
  GSList *radio_group = NULL;
  ArCardThemeInfo *current_theme_info;
  guint i = 0;

  /* Clean out the old menu */
  if (window->card_themes_merge_id != 0) {
    gtk_ui_manager_remove_ui (window->ui_manager, window->card_themes_merge_id);
    window->card_themes_merge_id = 0;
  }
  if (window->card_themes_group != NULL) {
    gtk_ui_manager_remove_action_group (window->ui_manager, window->card_themes_group);
    window->card_themes_group = NULL;
  }

  /* See gtk bug #424448 */
  gtk_ui_manager_ensure_update (window->ui_manager);

  list = ar_card_themes_get_themes (window->theme_manager);

  /* No need to install the menu when there's only one theme available anyway */
  if (list == NULL || list->next == NULL) {
    g_list_foreach (list, list_element_unref_cb, NULL);
    g_list_free (list);
    return;
  }

  window->card_themes_group = gtk_action_group_new ("Theme");
  gtk_ui_manager_insert_action_group (window->ui_manager, window->card_themes_group, -1);
  g_object_unref (window->card_themes_group);

  window->card_themes_merge_id = gtk_ui_manager_new_merge_id (window->ui_manager);

  if (window->theme) {
    current_theme_info = ar_card_theme_get_theme_info (window->theme);
  } else {
    current_theme_info = NULL;
  }

  for (l = list; l != NULL; l = l->next) {
    ArCardThemeInfo *info = (ArCardThemeInfo *) l->data;
    GtkRadioAction *action;
    char actionname[32];
    char *display_name, *tooltip;

    display_name = g_strdup (ar_card_theme_info_get_display_name (info));

    g_snprintf (actionname, sizeof (actionname), "Theme%u", i);
    tooltip = g_strdup_printf (_("Display cards with “%s” card theme"), display_name);
    action = gtk_radio_action_new (actionname, display_name, tooltip, NULL, i);
    g_free (display_name);
    g_free (tooltip);

    gtk_radio_action_set_group (action, radio_group);
    radio_group = gtk_radio_action_get_group (action);

    /* Check if this is the current theme's action. Do this before connecting the callback */
    if (ar_card_theme_info_equal (info, current_theme_info)) {
      gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), TRUE);
    }

    /* We steal the data from the list */
    g_object_set_data_full (G_OBJECT (action), "theme-info",
                            l->data, (GDestroyNotify) ar_card_theme_info_unref);
    l->data = NULL;

    g_signal_connect (action, "toggled",
                      G_CALLBACK (card_theme_changed_cb), window);
    gtk_action_group_add_action (window->card_themes_group, GTK_ACTION (action));
    g_object_unref (action);

    gtk_ui_manager_add_ui (window->ui_manager,
                           window->card_themes_merge_id,
                           CARD_THEMES_MENU_PATH,
                           actionname, actionname,
                           GTK_UI_MANAGER_MENUITEM, FALSE);

    ++i;
  }

  /* The list elements' data's refcount has been adopted above */
  g_list_free (list);
}

static void
view_menu_activate_cb (GtkAction *action,
                       AisleriotWindow *window)
{

  /* Request the list of themes. If it wasn't updated yet, the "changed"
   * callback will build the card themes submenu.
   */
  ar_card_themes_request_themes (window->theme_manager);
}

/* Callbacks */

/* Game state synchronisation */

static void
sync_game_score (AisleriotGame *game,
                 GParamSpec *pspec,
                 AisleriotWindow *window)
{

  gtk_label_set_text (GTK_LABEL (window->score_label),
                      aisleriot_game_get_score (game));
}

static void
sync_game_state (AisleriotGame *game,
                 GParamSpec *pspec,
                 AisleriotWindow *window)
{
  guint state;

  state = aisleriot_game_get_state (window->game);

  /* Can only change options before the game start.
   * Set all the options insensitive, not the menu item in the main menu,
   * since the HIG disapproves of that.
   */
  if (window->options_group != NULL) {
    gtk_action_group_set_sensitive (window->options_group, state <= GAME_BEGIN);
  }

  /* Can only get hints while the game is running */
  gtk_action_set_sensitive (window->action[ACTION_HINT],
                            state == GAME_BEGIN || state == GAME_RUNNING);

  if (state == GAME_RUNNING) {
    ar_clock_start (AR_CLOCK (window->clock));
  } else {
    ar_clock_stop (AR_CLOCK (window->clock));
  }

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
  gboolean enabled;

  g_object_get (game, "can-undo", &enabled, NULL);

  gtk_action_set_sensitive (window->action[ACTION_UNDO_MOVE], enabled);

  /* The restart game validity condition is the same as for undo. */
  gtk_action_set_sensitive (window->action[ACTION_RESTART_GAME], enabled);
}

static void
sync_game_redoable (AisleriotGame *game,
                    GParamSpec *pspec,
                    AisleriotWindow *window)
{
  gboolean enabled;

  g_object_get (game, "can-redo", &enabled, NULL);

  gtk_action_set_sensitive (window->action[ACTION_REDO_MOVE], enabled);
}

static void
sync_game_dealable (AisleriotGame *game,
                    GParamSpec *pspec,
                    AisleriotWindow *window)
{
  gboolean enabled;

  g_object_get (game, "can-deal", &enabled, NULL);

  gtk_action_set_sensitive (window->action[ACTION_DEAL], enabled);
}

static void
game_type_changed_cb (AisleriotGame *game,
                      AisleriotWindow *window)
{
  char *game_name;
  guint features;
  gboolean dealable;
  gboolean show_scores;

  window->changing_game_type = TRUE;

  game_name = aisleriot_game_get_name (game);

  g_object_set (window->action[ACTION_HELP_GAME], "label", game_name, NULL);
  g_object_set (window->action[ACTION_OPTIONS_MENU], "label", game_name, NULL);

  gtk_window_set_title (GTK_WINDOW (window), game_name);

  g_free (game_name);

  add_recently_played_game (window, aisleriot_game_get_game_module (game));

  install_recently_played_menu (window);

  install_options_menu (window);

  update_statistics_display (window);

  features = aisleriot_game_get_features (game);

  dealable = (features & FEATURE_DEALABLE) != 0;
  gtk_action_set_visible (window->action[ACTION_DEAL], dealable);

  ar_clock_reset (AR_CLOCK (window->clock));

  show_scores = (features & FEATURE_SCORE_HIDDEN) == 0;
  g_object_set (window->score_box, "visible", show_scores, NULL);

  window->changing_game_type = FALSE;
}

static void
game_new_cb (AisleriotGame *game,
             AisleriotWindow *window)
{

  update_statistics_display (window);

  ar_clock_reset (AR_CLOCK (window->clock));
}

static void
game_statusbar_message_cb (AisleriotGame *game,
                           const char *message,
                           AisleriotWindow *window)
{
  guint id = window->game_message_id;

  gtk_statusbar_pop (window->statusbar, id);
  if (message != NULL) {
    gtk_statusbar_push (window->statusbar, id, message);
  }
}

static void
game_exception_response_cb (GtkWidget *dialog,
                            int response,
                            AisleriotWindow *window)
{
  GError *error;
  gboolean did_report;

  error = g_object_get_data (G_OBJECT (dialog), "error");
  g_assert (error != NULL);

  did_report = FALSE;
  if (response == GTK_RESPONSE_ACCEPT) {
    GError *err = NULL;
    char pidstr[64];
    int fd;
    char *error_file;

    g_snprintf (pidstr, sizeof (pidstr), "%d", getpid ());

    fd = g_file_open_tmp ("arcrashXXXXXX", &error_file, &err);
    if (fd >= 0) {
      const char * const argv[] = {
        "bug-buddy",
        "--package", "aisleriot",
        "--appname", "sol",
        "--pid", pidstr,
        "--include", (const char *) error_file,
        "--unlink-tempfile",
        NULL
      };

      close (fd);

      g_file_set_contents (error_file, error->message, strlen (error->message), NULL);

      if (g_spawn_async (NULL /* working dir */,
                         (char **) argv,
                         NULL /* envp */,
                         G_SPAWN_SEARCH_PATH,
                         NULL, NULL,
                         NULL,
                         &err)) {
        did_report = TRUE;
      } else {
        g_warning ("Failed to launch bug buddy: %s\n", err->message);
        g_error_free (err);
      }

      g_free (error_file);
    } else {
      g_warning ("Failed to create temp file: %s\n", err->message);
      g_error_free (err);
    }
  }

  if (!did_report) {
    g_printerr ("Aisleriot " VERSION " scheme exception occurred\n-- 8< --\n%s\n-- >8 --\n", error->message);
  }

  gtk_widget_destroy (dialog);

  /* Start a new game */
  window_new_game (window);

  gtk_widget_grab_focus (GTK_WIDGET (window->board));
}

static void
game_exception_cb (AisleriotGame *game,
                   const GError *error,
                   AisleriotWindow *window)
{
  GtkWidget *dialog;

  g_return_if_fail (error != NULL);

  dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_ERROR,
                                   GTK_BUTTONS_NONE,
                                   "%s", _("A scheme exception occurred"));
  gtk_message_dialog_format_secondary_text
    (GTK_MESSAGE_DIALOG (dialog),
     "%s", _("Please report this bug to the developers."));

  gtk_window_set_title (GTK_WINDOW (dialog), "");
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
  g_object_set_data_full (G_OBJECT (dialog), "error",
                          g_error_copy (error),
                          (GDestroyNotify) g_error_free);

  gtk_widget_show (dialog);
}

#if defined(ENABLE_SOUND)

static void
settings_changed_cb (GtkSettings *settings,
                     GParamSpec *pspec,
                     AisleriotWindow *window)
{
  GtkAction *action;
  gboolean enabled;
  const char *name;

  if (pspec)
    name = pspec->name;
  else
    name = NULL;

  if (name == NULL || strcmp (name, "gtk-enable-event-sounds") == 0) {
    g_object_get (settings, "gtk-enable-event-sounds", &enabled, NULL);

    action = gtk_action_group_get_action (window->action_group, "Sound");
    gtk_action_set_visible (action, enabled);
  }
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

  ar_sound_init (screen);

  settings = gtk_widget_get_settings (widget);
  settings_changed_cb (settings, NULL, window);
  g_signal_connect (settings, "notify::gtk-enable-event-sounds",
                    G_CALLBACK (settings_changed_cb), window);
}

#endif /* ENABLE_SOUND */

static void
board_status_message_cb (AisleriotBoard *board,
                         const char *status_message,
                         AisleriotWindow *window)
{

  gtk_statusbar_pop (window->statusbar, window->board_message_id);

  if (status_message != NULL) {
    gtk_statusbar_push (window->statusbar, window->board_message_id, status_message);
  }
}

static void
aisleriot_window_style_set (GtkWidget *widget,
                            GtkStyle *previous_style)
{
  AisleriotWindow *window = AISLERIOT_WINDOW (widget);
  const cairo_font_options_t *font_options;
  void (* style_set) (GtkWidget *, GtkStyle *) =
    GTK_WIDGET_CLASS (aisleriot_window_parent_class)->style_set;

  if (style_set)
    style_set (widget, previous_style);

  if (!window->theme)
    return;

  font_options = gdk_screen_get_font_options (gtk_widget_get_screen (widget));
  ar_card_theme_set_font_options (window->theme, font_options);

  /* FIXMEchpe: clear the cached cards in the slots?? */
}

static gboolean
aisleriot_window_state_event (GtkWidget *widget,
                              GdkEventWindowState *event)
{
  AisleriotWindow *window = AISLERIOT_WINDOW (widget);

  if (event->changed_mask & (GDK_WINDOW_STATE_FULLSCREEN | GDK_WINDOW_STATE_MAXIMIZED)) {
    gboolean is_fullscreen;

    is_fullscreen = (event->new_window_state & GDK_WINDOW_STATE_FULLSCREEN) != 0;

    set_fullscreen_actions (window, is_fullscreen);

    set_fullscreen_button_active (window);
  }

  if (event->changed_mask & GDK_WINDOW_STATE_ICONIFIED) {
    if (aisleriot_game_get_state (window->game) == GAME_RUNNING) {
      gboolean is_iconified;

      is_iconified = (event->new_window_state & GDK_WINDOW_STATE_ICONIFIED);

      aisleriot_game_set_paused (window->game, is_iconified);
      if (is_iconified) {
        ar_clock_stop (AR_CLOCK (window->clock));
      } else {
        ar_clock_start (AR_CLOCK (window->clock));
      }
    }
  }

  if (GTK_WIDGET_CLASS (aisleriot_window_parent_class)->window_state_event) {
    return GTK_WIDGET_CLASS (aisleriot_window_parent_class)->window_state_event (widget, event);
  }

  return FALSE;
}

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
    { "NewGame", AR_STOCK_NEW_GAME, NULL,
      "<ctrl>N",
      N_("Start a new game"),
      G_CALLBACK (new_game_cb) },
    { "RestartGame", AR_STOCK_RESTART_GAME, NULL, NULL,
       N_("Restart the game"),
      G_CALLBACK (restart_game) },
    { "Select", GTK_STOCK_INDEX, N_("_Select Game…"),
      "<ctrl>O",
      N_("Play a different game"),
      G_CALLBACK (select_game_cb) },
    { "RecentMenu", NULL, N_("_Recently Played") },
    { "Statistics", NULL, N_("S_tatistics"), NULL,
      N_("Show gameplay statistics"),
      G_CALLBACK (statistics_cb) },
    { "CloseWindow", GTK_STOCK_CLOSE, NULL, NULL,
      N_("Close this window"),
      G_CALLBACK (close_window_cb) },
    { "UndoMove", AR_STOCK_UNDO_MOVE, NULL, NULL,
      N_("Undo the last move"),
      G_CALLBACK (undo_cb) },
    { "RedoMove", AR_STOCK_REDO_MOVE, NULL, NULL,
      N_("Redo the undone move"),
      G_CALLBACK (redo_cb) },
    { "Deal", AR_STOCK_DEAL_CARDS, NULL, NULL,
      N_("Deal next card or cards"),
      G_CALLBACK (deal_cb) },
    { "Hint", AR_STOCK_HINT, NULL, NULL,
      N_("Get a hint for your next move"),
      G_CALLBACK (show_hint_cb) },
    { "Contents", AR_STOCK_CONTENTS, NULL, NULL,
      N_("View help for Aisleriot"),
      G_CALLBACK (help_general_cb) },
    { "HelpGame", AR_STOCK_CONTENTS, NULL,
      "<shift>F1",
      N_("View help for this game"),
      G_CALLBACK (help_on_game_cb) },
    { "About", GTK_STOCK_ABOUT, NULL, NULL,
      N_("About this game"),
      G_CALLBACK (help_about_cb) },
    { "KeyboardShortcuts", NULL, N_("_Keyboard Shortcuts"),
      NULL,
      NULL,
      G_CALLBACK (help_overlay_cb) },
    { "InstallThemes", NULL, N_("Install card themes…"), NULL,
      N_("Install new card themes from the distribution packages repositories"),
      G_CALLBACK (install_themes_cb) },

    /* Toolbar-only actions */
    { "LeaveFullscreen", AR_STOCK_LEAVE_FULLSCREEN, NULL, NULL, NULL,
      G_CALLBACK (leave_fullscreen_cb) },
    { "ThemeMenu", NULL, N_("_Card Style"), NULL, NULL, NULL },

#ifdef ENABLE_DEBUG_UI
    /* Debug UI actions */
    { "DebugMenu", NULL, "_Debug" },
    { "DebugChooseSeed", NULL, "_Choose seed", NULL, NULL,
      G_CALLBACK (debug_choose_seed_cb) },
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
#endif /* ENABLE_DEBUG_UI */
  };

  const GtkToggleActionEntry toggle_actions[] = {
    { "Fullscreen", AR_STOCK_FULLSCREEN, NULL, NULL, NULL,
      G_CALLBACK (fullscreen_toggled_cb),
      FALSE },
    { "Toolbar", NULL, N_("_Toolbar"), NULL,
      N_("Show or hide the toolbar"),
      G_CALLBACK (toolbar_toggled_cb),
      TRUE /* active by default since the UI manager creates the toolbar visible */
    },
    { "Statusbar", NULL, N_("_Statusbar"), NULL,
      N_("Show or hide statusbar"),
      G_CALLBACK (statusbar_toggled_cb),
      FALSE
    },
    { "ClickToMove", NULL, N_("_Click to Move"), NULL,
      N_("Pick up and drop cards by clicking"),
      G_CALLBACK (clickmove_toggle_cb),
      FALSE /* not active by default */ },
   { "Sound", NULL, N_("_Sound"), NULL,
      N_("Whether or not to play event sounds"),
      G_CALLBACK (sound_toggle_cb),
      FALSE /* not active by default */ },
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
    "LeaveFullscreen",
  };

  GtkWidget *main_vbox;
  GtkAccelGroup *accel_group;
  GtkAction *action;
  char *theme_name;
  ArCardTheme *theme;
  guint i;
  GtkStatusbar *statusbar;
  GtkWidget *statusbar_hbox, *label, *time_box;
  GError *error = NULL;

  g_assert (G_N_ELEMENTS (names) == LAST_ACTION);

  window->fullscreen = FALSE;

  window->game = aisleriot_game_new ();

  window->theme_manager = ar_card_themes_new ();

  window->board_style = ar_style_new ();

  window->board = AISLERIOT_BOARD (aisleriot_board_new (window->board_style, window->game));

  theme_name = ar_conf_get_string (NULL, aisleriot_conf_get_key (CONF_THEME), NULL);
  theme = ar_card_themes_get_theme_by_name (window->theme_manager, theme_name);
  g_free (theme_name);
  if (!theme) {
    /* Last-ditch fallback: try getting *any* theme */
    theme = ar_card_themes_get_theme_any (window->theme_manager);
  }
  if (theme) {
    aisleriot_window_take_card_theme (window, theme /* adopts */);
  } else {
    /* FIXMEchpe: FUCK, what now? Panic! */
    /* Put up some UI, and exit! */
    g_assert_not_reached ();
  }

  window->action_group = gtk_action_group_new ("MenuActions");
  gtk_action_group_set_translation_domain (window->action_group, GETTEXT_PACKAGE);
  gtk_action_group_add_actions (window->action_group,
                                actions,
                                G_N_ELEMENTS (actions),
				window);
  gtk_action_group_add_toggle_actions (window->action_group,
                                       toggle_actions,
				       G_N_ELEMENTS (toggle_actions),
                                       window);

  window->ui_manager = gtk_ui_manager_new ();

  gtk_ui_manager_insert_action_group (window->ui_manager, window->action_group, -1);

  for (i = 0; i < LAST_ACTION; ++i) {
    window->action[i] = gtk_action_group_get_action (window->action_group, names[i]);
    g_assert (window->action[i]);
  }

  /* Hide the "Deal" action initially, since not all games support it */
  gtk_action_set_visible (window->action[ACTION_DEAL], FALSE);

  /* Set labels for toolbar items */
  action = gtk_action_group_get_action (window->action_group, "Select");
  g_object_set (action, "short-label", _("Select Game"), NULL);

  statusbar = window->statusbar = GTK_STATUSBAR (gtk_statusbar_new ());
  window->game_message_id = gtk_statusbar_get_context_id (window->statusbar, "game-message");
  ar_stock_prepare_for_statusbar_tooltips (window->ui_manager,
                                              GTK_WIDGET (window->statusbar));

  window->board_message_id = gtk_statusbar_get_context_id (window->statusbar, "board-message");

  g_signal_connect (window->board, "status-message",
                    G_CALLBACK (board_status_message_cb), window);

  gtk_window_set_has_resize_grip (GTK_WINDOW (window), TRUE);

  statusbar_hbox = gtk_statusbar_get_message_area (statusbar);
  gtk_box_set_spacing (GTK_BOX (statusbar_hbox), 24);

  /* Score */
  window->score_box = gtk_hbox_new (12, FALSE);
  label = gtk_label_new (_("Score:"));
  gtk_widget_show (label);
  gtk_box_pack_start (GTK_BOX (window->score_box), label, FALSE, FALSE, 0);
  window->score_label = gtk_label_new ("   0");
  gtk_widget_show (window->score_label);
  gtk_box_pack_start (GTK_BOX (window->score_box), window->score_label, FALSE, FALSE, 0);
  gtk_box_pack_end (GTK_BOX (statusbar_hbox), window->score_box, FALSE, FALSE, 0);

  ar_atk_util_add_atk_relation (label, window->score_label, ATK_RELATION_LABEL_FOR);
  ar_atk_util_add_atk_relation (window->score_label, label, ATK_RELATION_LABELLED_BY);

  time_box = gtk_hbox_new (12, FALSE);
  label = gtk_label_new (_("Time:"));
  gtk_box_pack_start (GTK_BOX (time_box), label, FALSE, FALSE, 0);
  window->clock = ar_clock_new ();
  gtk_box_pack_start (GTK_BOX (time_box), window->clock, FALSE, FALSE, 0);
  gtk_box_pack_end (GTK_BOX (statusbar_hbox), time_box, FALSE, FALSE, 0);
  gtk_widget_show_all (time_box);

  ar_atk_util_add_atk_relation (label, window->clock, ATK_RELATION_LABEL_FOR);
  ar_atk_util_add_atk_relation (window->clock, label, ATK_RELATION_LABELLED_BY);

  /* Load the UI after we've connected the statusbar,
   * otherwise not all actions will have statusbar help.
   */
  gtk_ui_manager_add_ui_from_resource (window->ui_manager, "/org/gnome/aisleriot/ui/menus.xml", &error);
  g_assert_no_error (error);
#ifdef ENABLE_DEBUG_UI
  gtk_ui_manager_add_ui_from_resource (window->ui_manager, "/org/gnome/aisleriot/ui/debug-menus.xml", &error);
  g_assert_no_error (error);
#endif /* ENABLE_DEBUG_UI */

  window->main_menu = gtk_ui_manager_get_widget (window->ui_manager, MAIN_MENU_UI_PATH);
  window->toolbar = gtk_ui_manager_get_widget (window->ui_manager, TOOLBAR_UI_PATH);

  gtk_style_context_add_class (gtk_widget_get_style_context (window->toolbar),
                               GTK_STYLE_CLASS_PRIMARY_TOOLBAR);

  /* Defer building the card themes menu until its parent's menu is opened */
  action = gtk_action_group_get_action (window->action_group, "ViewMenu");
  g_signal_connect (action, "activate",
                    G_CALLBACK (view_menu_activate_cb), window);

  /* It's possible that the themes list has already been loaded (e.g.
   * if the theme loading above involved the fallback); in that case
   * we need to update the menu right now.
   */
  if (ar_card_themes_get_themes_loaded (window->theme_manager))
    install_card_theme_menu (window->theme_manager, window);

  /* Rebuild the themes menu when the themes list changes */
  g_signal_connect (window->theme_manager, "changed",
                    G_CALLBACK (install_card_theme_menu), window);

  /* The actions and menus are done. The
   * recent games menu will be updated when the initial game loads.
   */

  accel_group = gtk_ui_manager_get_accel_group (window->ui_manager);
  gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);

  action = gtk_action_group_get_action (window->action_group, "Toolbar");
  window->toolbar_visible = ar_conf_get_boolean (NULL, aisleriot_conf_get_key (CONF_SHOW_TOOLBAR), NULL) != FALSE;
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
                                window->toolbar_visible);
  action = gtk_action_group_get_action (window->action_group, "ClickToMove");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
                                ar_conf_get_boolean (NULL, aisleriot_conf_get_key (CONF_CLICK_TO_MOVE), NULL));

  action = gtk_action_group_get_action (window->action_group, "RecentMenu");
  g_object_set (action, "hide-if-empty", FALSE, NULL);

#ifdef ENABLE_SOUND
  action = gtk_action_group_get_action (window->action_group, "Sound");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
                                ar_conf_get_boolean (NULL, aisleriot_conf_get_key (CONF_SOUND), NULL));
  gtk_action_set_visible (action, ar_sound_is_available ());
#endif /* ENABLE_SOUND */

  action = gtk_action_group_get_action (window->action_group, "Statusbar");
  window->statusbar_visible = ar_conf_get_boolean (NULL, aisleriot_conf_get_key (CONF_SHOW_STATUSBAR), NULL) != FALSE;
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
                                window->statusbar_visible);

  set_fullscreen_actions (window, FALSE);

#if defined(ENABLE_SOUND)
  /* Set the action visibility and listen for animation and sound mode changes */
  screen_changed_cb (GTK_WIDGET (window), NULL, window);
  g_signal_connect (window, "screen-changed",
                    G_CALLBACK (screen_changed_cb), window);
#endif /* ENABLE_SOUND */

  /* Now set up the widgets */
  main_vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), main_vbox);
  gtk_widget_show (main_vbox);

  gtk_box_pack_start (GTK_BOX (main_vbox), window->main_menu, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (main_vbox), window->toolbar, FALSE, FALSE, 0);
  gtk_box_pack_end (GTK_BOX (main_vbox), GTK_WIDGET (window->statusbar), FALSE, FALSE, 0);

  gtk_box_pack_start (GTK_BOX (main_vbox), GTK_WIDGET (window->board), TRUE, TRUE, 0);
  gtk_widget_show (GTK_WIDGET (window->board));

  /* Synchronise */
  sync_game_score (window->game, NULL, window);
  g_signal_connect (window->game, "notify::score",
                    G_CALLBACK (sync_game_score), window);

  sync_game_state (window->game, NULL, window);
  g_signal_connect (window->game, "notify::state",
                    G_CALLBACK (sync_game_state), window);
  sync_game_undoable (window->game, NULL, window);
  g_signal_connect (window->game, "notify::can-undo",
                    G_CALLBACK (sync_game_undoable), window);
  sync_game_redoable (window->game, NULL, window);
  g_signal_connect (window->game, "notify::can-redo",
                    G_CALLBACK (sync_game_redoable), window);
  sync_game_dealable (window->game, NULL, window);
  g_signal_connect (window->game, "notify::can-deal",
                    G_CALLBACK (sync_game_dealable), window);

  g_signal_connect (window->game, "game-type",
                    G_CALLBACK (game_type_changed_cb), window);
  g_signal_connect (window->game, "game-new",
                    G_CALLBACK (game_new_cb), window);
  g_signal_connect (window->game, "message",
                    G_CALLBACK (game_statusbar_message_cb), window);
  g_signal_connect (window->game, "exception",
                    G_CALLBACK (game_exception_cb), window);

  /* Fallback, if there is no saved size */
  gtk_window_set_default_size (GTK_WINDOW (window), MIN_WIDTH, MIN_HEIGHT);

  /* Restore window state */
  ar_gsettings_bind_window_state (AR_SETTINGS_WINDOW_STATE_PATH, GTK_WINDOW (window));

  /* Initial focus is in the board */
  gtk_widget_grab_focus (GTK_WIDGET (window->board));
}

static void
aisleriot_window_dispose (GObject *object)
{
  AisleriotWindow *window = AISLERIOT_WINDOW (object);

#ifdef ENABLE_SOUND
  g_signal_handlers_disconnect_by_func (gtk_widget_get_settings (GTK_WIDGET (window)),
                                        G_CALLBACK (settings_changed_cb),
                                        window);
#endif /* ENABLE_SOUND */

  if (window->hint_dialog) {
    gtk_widget_destroy (window->hint_dialog);
    g_assert (window->hint_dialog == NULL);
  }
  if (window->game_over_dialog) {
    gtk_widget_destroy (window->game_over_dialog);
    g_assert (window->game_over_dialog == NULL);
  }
  if (window->game_choice_dialog) {
    gtk_widget_destroy (window->game_choice_dialog);
    g_assert (window->game_choice_dialog == NULL);
  }
  if (window->stats_dialog) {
    gtk_widget_destroy (GTK_WIDGET (window->stats_dialog));
    g_assert (window->stats_dialog == NULL);
  }
  
#ifdef LEAVE_FULLSCREEN_BUTTON
  if (window->fullscreen_button != NULL) {
    gtk_widget_destroy (GTK_WIDGET (window->fullscreen_button));
    window->fullscreen_button = NULL;
  }
#endif

  if (window->load_idle_id != 0) {
    g_source_remove (window->load_idle_id);
    window->load_idle_id = 0;
  }

  G_OBJECT_CLASS (aisleriot_window_parent_class)->dispose (object);
}

static void
aisleriot_window_finalize (GObject *object)
{
  AisleriotWindow *window = AISLERIOT_WINDOW (object);

  if (window->theme) {
    g_object_unref (window->theme);
  }

  g_object_unref (window->theme_manager);

  g_signal_handlers_disconnect_matched (window->game,
                                        G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL, NULL, window);
  g_object_unref (window->game);

  G_OBJECT_CLASS (aisleriot_window_parent_class)->finalize (object);
}

static void
aisleriot_window_class_init (AisleriotWindowClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  gobject_class->dispose = aisleriot_window_dispose;
  gobject_class->finalize = aisleriot_window_finalize;

  widget_class->window_state_event = aisleriot_window_state_event;
  widget_class->style_set = aisleriot_window_style_set;
}

/* public API */

/**
 * aisleriot_window_new:
 *
 * Returns: a new #AisleriotWindow
 */
GtkWidget *
aisleriot_window_new (GtkApplication *application)
{
  return g_object_new (AISLERIOT_TYPE_WINDOW,
                       "application", application,
                       "show-menubar", FALSE,
                       NULL);
}

/**
 * aisleriot_window_get_ui_manager:
 * @window:
 *
 * Returns: (transfer none): the window's #GtkUIManager
 */
GtkUIManager *
aisleriot_window_get_ui_manager (AisleriotWindow *window)
{
  return window->ui_manager;
}

/**
 * aisleriot_window_get_ui_manager:
 * @window:
 *
 * Returns: (transfer none): the window's #GtkUIManager
 */
GtkAction *
aisleriot_window_get_action (AisleriotWindow *window,
                             const char *action_name)
{
  return gtk_action_group_get_action (window->action_group, action_name);
}

typedef struct {
  AisleriotWindow *window;
  char *game_module;
  GRand *rand;
} LoadIdleData;

static void
load_error_response_cb (GtkWidget *dialog,
                        int response,
                        AisleriotWindow *window)
{
  /* Load the default game */
  aisleriot_window_set_game_module (window, DEFAULT_VARIATION, NULL);

  gtk_widget_destroy (dialog);
}

static gboolean
load_idle_cb (LoadIdleData *data)
{
  AisleriotWindow* window = data->window;
  GError *error = NULL;
  GRand *rand;
  char *pref;

  if (!aisleriot_game_load_game (window->game, data->game_module, &error)) {
    GtkWidget *dialog;
    char *name;

    name = ar_filename_to_display_name (data->game_module);

    dialog = gtk_message_dialog_new (GTK_WINDOW (data->window),
                                     GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_OK,
                                     _("Cannot start the game “%s”"),
                                     name);
    g_free (name);

    gtk_message_dialog_format_secondary_text
      (GTK_MESSAGE_DIALOG (dialog),
       "%s\n\n%s",
       _("Aisleriot cannot find the last game you played."),
       _("This usually occurs when you run an older version of Aisleriot "
         "which does not have the game you last played. "
         "The default game, Klondike, is being started instead."));

    /* FIXME: add @error->message to a textview in a Detailed… expander */
    g_printerr ("Scheme exception:\n-- 8< --\n%s\n-- >8 --\n", error->message);

    g_signal_connect (dialog, "response",
                      G_CALLBACK (load_error_response_cb), data->window);

    g_error_free (error);

    gtk_window_present (GTK_WINDOW (dialog));

    return FALSE;
  }

  /* Now that we know we can successfully load this variation,
   * store it in conf
   */
  pref = g_strconcat (data->game_module, ".scm", NULL);
  ar_conf_set_string (NULL, aisleriot_conf_get_key (CONF_VARIATION), pref);
  g_free (pref);

  rand = data->rand;
  data->rand = NULL;

  window_new_game_with_rand (window, rand /* adopted */);

  gtk_widget_grab_focus (GTK_WIDGET (window->board));

  return FALSE;
}

static void
free_load_idle_data (LoadIdleData *data)
{
  data->window->load_idle_id = 0;

  if (data->rand)
    g_rand_free (data->rand);

  g_free (data->game_module);
  g_slice_free (LoadIdleData, data);
}

/**
 * aisleriot_window_set_game:
 * @window:
 * @game_module: a UTF-8 string
 * @rand: (allow-none) (transfer full): a #GRand, or %NULL
 *
 * Loads the game variation defined in the @game_module file.
 * Note that even though @game_module is used as a filename,
 * it must be in UTF-8!
 */
void
aisleriot_window_set_game_module (AisleriotWindow *window,
                                  const char *game_module,
                                  GRand *rand)
{
  LoadIdleData *data;

  /* We'll do this on idle */
  if (window->load_idle_id != 0) {
    g_source_remove (window->load_idle_id);
  }

  data = g_slice_new (LoadIdleData);
  data->window = window;
  data->game_module = g_strdup (game_module);
  data->rand = rand; /* adopted */

  window->load_idle_id = g_idle_add_full (G_PRIORITY_LOW,
                                        (GSourceFunc) load_idle_cb,
                                        data,
                                        (GDestroyNotify) free_load_idle_data);
}

/**
 * aisleriot_window_get_game_module:
 * @window:
 *
 * Returns: the name of the game running in @window
 */
const char *
aisleriot_window_get_game_module (AisleriotWindow *window)
{
  return aisleriot_game_get_game_module (window->game);
}
