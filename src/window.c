/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2; tab-width: 2 -*- */
/*
 * Copyright © 1998, 2003 Jonathan Blandford <jrb@alum.mit.edu>
 * Copyright © 2003 Callum McKenzie <callum@physics.otago.ac.nz>
 * Copyright © 2007, 2008, 2009, 2010, 2013 Christian Persch
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

#include "ar-application.h"
#include "ar-clock.h"
#include "ar-debug.h"
#include "ar-stock.h"
#include "ar-runtime.h"
#include "ar-sound.h"
#include "ar-string-utils.h"
#include "ar-gsettings.h"
#include "ar-prefs.h"

#ifdef HAVE_CLUTTER
#include "ar-clutter-embed.h"
#include "ar-style.h"
#include "baize.h"
#include "board.h"
#else
#include "board-noclutter.h"
#endif

#include "ar-defines.h"
#include "ar-card-theme.h"
#include "ar-card-themes.h"
#include "game.h"
#include "stats-dialog.h"
#include "util.h"
#include "ar-game-chooser.h"

#include "window.h"

#ifdef ENABLE_DEBUG_UI
#include "prop-editor.h"
#endif

#define AISLERIOT_WINDOW_GET_PRIVATE(window)(G_TYPE_INSTANCE_GET_PRIVATE ((window), AISLERIOT_TYPE_WINDOW, AisleriotWindowPrivate))

#define MIN_WIDTH 800
#define MIN_HEIGHT 600

/* The maximum number of recent games saved */
#define MAX_RECENT 5

#define AR_SETTINGS_PATH_PREFIX "/org/gnome/solitaire/"
#define AR_SETTINGS_WINDOW_STATE_PATH AR_SETTINGS_PATH_PREFIX "window-state/"

struct _AisleriotWindowPrivate
{
  GSettings *settings;
  GSettings *state_settings;

  AisleriotGame *game;
  ArStyle *board_style;
#ifdef HAVE_CLUTTER
  ArClutterEmbed *board;
  ClutterActor *baize_actor;
  ClutterActor *board_actor;
#else
  AisleriotBoard *board;
#endif

  ArCardThemes *theme_manager;
  ArCardTheme *theme;

  GtkWidget *main_vbox;
  GtkStatusbar *statusbar;
  guint game_message_id;
  guint board_message_id;
  GtkWidget *score_box;
  GtkWidget *score_label;
  GtkWidget *clock;

  GtkWidget *toolbar;

  GtkWidget *game_over_dialog;
  GtkWidget *game_choice_dialog;
  GtkWidget *prefs_dialog;
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
      aisleriot_game_new_game (priv->game);
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
        aisleriot_game_new_game (priv->game);
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
                            _("_Close"), GTK_RESPONSE_CLOSE,
                            _("_New Game"), RESPONSE_NEW_GAME,
                            NULL);
    gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
                                             RESPONSE_NEW_GAME,
                                             GTK_RESPONSE_CLOSE,
                                             -1);
  } else {
    gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                            _("_Undo Move"), RESPONSE_UNDO,
                            _("_Close"), GTK_RESPONSE_CLOSE,
                            _("_Restart"), RESPONSE_RESTART,
                            _("_New Game"), RESPONSE_NEW_GAME,
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
  char *game_name;

  if (!priv->stats_dialog)
    return;

  game_name = aisleriot_game_get_name (priv->game);
  aisleriot_stats_dialog_set_name (priv->stats_dialog, game_name);
  g_free (game_name);

  aisleriot_stats_dialog_update (priv->stats_dialog,
                                 aisleriot_game_get_scores_settings (priv->game));
}

static void
stats_dialog_response_cb (GtkWidget *widget,
                          int response,
                          AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  if (response == GTK_RESPONSE_REJECT) {
    GSettings *stats_settings;

    stats_settings = aisleriot_game_get_scores_settings (priv->game);
    g_settings_set_uint (stats_settings, AR_SCORES_WINS_KEY, 0);
    g_settings_set_uint (stats_settings, AR_SCORES_TOTAL_KEY, 0);
    g_settings_set_uint (stats_settings, AR_SCORES_BEST_TIME_KEY, 0);
    g_settings_set_uint (stats_settings, AR_SCORES_WORST_TIME_KEY, 0);

    aisleriot_stats_dialog_update (priv->stats_dialog, stats_settings);

    return;
  }

  gtk_widget_destroy (widget);
}

/* action methods */

static void
action_toggle_state_cb (GSimpleAction *saction,
                        GVariant *parameter,
                        gpointer user_data)
{
  GAction *action = G_ACTION (saction);
  GVariant *state;

  state = g_action_get_state (action);
  g_action_change_state (action, g_variant_new_boolean (!g_variant_get_boolean (state)));
  g_variant_unref (state);
}

void
aisleriot_window_new_game (AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  aisleriot_game_new_game (priv->game);

  gtk_widget_grab_focus (GTK_WIDGET (priv->board));
}

void
aisleriot_window_change_game (AisleriotWindow *window)
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

void
aisleriot_window_show_statistics_dialog (AisleriotWindow *window)
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

void
aisleriot_window_show_about_dialog (AisleriotWindow * window)
{
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
    "Sapphire Becker",
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
                                      "Copyright © 2007, 2008, 2009, 2010, 2011, 2012, 2013 Christian Persch",
                         "license", licence,
                         "authors", authors,
                         "artists", artists,
                         "documenters", documenters,
                         "translator-credits", _("translator-credits"),
                         "logo-icon-name", "gnome-aisleriot",
                         "website", "http://www.gnome.org/projects/gnome-games/",
                         "website-label", _("GNOME Games web site"),
                         "wrap-license", TRUE,
                        NULL);
  g_free (licence);
}

/* action callbacks */

static void
action_new_game_cb (GSimpleAction *action,
                    GVariant *parameter,
                    gpointer user_data)
{
  AisleriotWindow *window = user_data;
  AisleriotWindowPrivate *priv = window->priv;

  aisleriot_window_new_game (window);
  gtk_widget_grab_focus (GTK_WIDGET (priv->board));
}

static void
action_undo_cb (GSimpleAction *action,
                GVariant *parameter,
                gpointer user_data)
{
  AisleriotWindow *window = user_data;
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
action_redo_cb (GSimpleAction *action,
                GVariant *parameter,
                gpointer user_data)
{
  AisleriotWindow *window = user_data;
  AisleriotWindowPrivate *priv = window->priv;

#ifdef HAVE_CLUTTER
  aisleriot_board_abort_move (AISLERIOT_BOARD (priv->board_actor));
#else
  aisleriot_board_abort_move (priv->board);
#endif

  aisleriot_game_redo_move (priv->game);
}

static void
action_about_cb (GSimpleAction *action,
                 GVariant *parameter,
                 gpointer user_data)
{
  AisleriotWindow *window = user_data;

  aisleriot_window_show_about_dialog (window);
}

static void
action_preferences_cb (GSimpleAction *action,
                       GVariant *parameter,
                       gpointer user_data)
{
  AisleriotWindow *window = user_data;
  AisleriotWindowPrivate *priv = window->priv;

  if (priv->prefs_dialog) {
    gtk_window_present (GTK_WINDOW (priv->prefs_dialog));
    return;
  }

  priv->prefs_dialog = ar_prefs_new (window);
  g_signal_connect (priv->prefs_dialog, "destroy",
                    G_CALLBACK (gtk_widget_destroyed), &priv->prefs_dialog);

  gtk_window_present (GTK_WINDOW (priv->prefs_dialog));
}

static void
action_restart_game_cb (GSimpleAction *action,
                        GVariant *parameter,
                        gpointer user_data)
{
  AisleriotWindow *window = user_data;
  AisleriotWindowPrivate *priv = window->priv;

  aisleriot_game_restart_game (priv->game);
};

static void
action_select_game_cb (GSimpleAction *action,
                       GVariant *parameter,
                       gpointer user_data)
{
  AisleriotWindow *window = user_data;

  aisleriot_window_change_game (window);
}

static void
action_help_cb (GSimpleAction *action,
                GVariant *parameter,
                gpointer user_data)
{
  AisleriotWindow *window = user_data;
  AisleriotWindowPrivate *priv = window->priv;
  const char *target;

  g_variant_get (parameter, "&s", &target);

  if (g_str_equal (target, "general")) {
    aisleriot_show_help (GTK_WIDGET (window), NULL);
  } else if (g_str_equal (target, "rules")) {
    const char *game_module;
  
    game_module = aisleriot_game_get_game_module (priv->game);
    aisleriot_show_help (GTK_WIDGET (window), game_module);
  }
}

static void
action_close_window_cb (GSimpleAction *action,
                        GVariant *parameter,
                        gpointer user_data)
{
  AisleriotWindow *window = user_data;

  gtk_widget_destroy (GTK_WIDGET (window));
}

static void
action_statistics_cb (GSimpleAction *action,
                      GVariant *parameter,
                      gpointer user_data)
{
  AisleriotWindow *window = user_data;

  aisleriot_window_show_statistics_dialog (window);
}

static void
action_install_themes_cb (GSimpleAction *action,
                          GVariant *parameter,
                          gpointer user_data)
{
  AisleriotWindow *window = user_data;
  AisleriotWindowPrivate *priv = window->priv;

  ar_card_themes_install_themes (priv->theme_manager,
                                 GTK_WIDGET (window),
                                 gtk_get_current_event_time ());
}

#if 0 //def ENABLE_DEBUG_UI

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
  AisleriotWindowPrivate *priv = window->priv;
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

  current_game_module = aisleriot_game_get_game_module (priv->game);
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
    AisleriotWindowPrivate *priv = window->priv;
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

      aisleriot_game_new_game_with_rand (priv->game, rand /* adopts */);

      gtk_widget_grab_focus (GTK_WIDGET (priv->board));
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
  g_snprintf (str, sizeof (str), "%u", aisleriot_game_get_seed (priv->game));
  gtk_entry_set_text (GTK_ENTRY (entry), str);
}
#endif

  gtk_box_pack_end (GTK_BOX (gtk_message_dialog_get_message_area (GTK_MESSAGE_DIALOG (dialog))), entry, FALSE, FALSE, 0);
  gtk_widget_show (entry);
  g_object_set_data (G_OBJECT (dialog), "entry", entry);
  gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);

  gtk_window_present (GTK_WINDOW (dialog));
}

static void
debug_tweak_style_cb (GtkAction *action,
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

static void
debug_tweak_settings_cb (GtkAction *action,
                         AisleriotWindow *window)
{
  GObject *object;
  GType type;
  GtkWidget *prop_editor;

  object = G_OBJECT (gtk_widget_get_settings (GTK_WIDGET (window)));
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
action_show_toolbar_state_cb (GSimpleAction *action,
                              GVariant *state,
                              gpointer user_data)
{
  AisleriotWindow *window = user_data;
  AisleriotWindowPrivate *priv = window->priv;
  gboolean show;

  g_simple_action_set_state (action, state);

  show = g_variant_get_boolean (state);

  g_object_set (priv->toolbar, "visible", show, NULL);

  priv->toolbar_visible = show != FALSE;

  g_settings_set_boolean (priv->settings, AR_SETTINGS_SHOW_TOOLBAR_KEY, show);

  set_fullscreen_button_active (window);
}

static void
action_show_statusbar_state_cb (GSimpleAction *action,
                                GVariant *state,
                                gpointer user_data)
{
  AisleriotWindow *window = user_data;
  AisleriotWindowPrivate *priv = window->priv;
  gboolean show;

  g_simple_action_set_state (action, state);

  show = g_variant_get_boolean (state);

  g_object_set (priv->statusbar, "visible", show, NULL);

  /* Only update the clock continually if it's visible */
  ar_clock_set_update (AR_CLOCK (priv->clock), show);

  priv->statusbar_visible = show != FALSE;

  g_settings_set_boolean (priv->settings, AR_SETTINGS_SHOW_STATUSBAR_KEY, show);
}

static void
set_fullscreen_actions (AisleriotWindow *window,
                        gboolean is_fullscreen)
{
#if 0 // fixme
  AisleriotWindowPrivate *priv = window->priv;

  priv->fullscreen = is_fullscreen;

  g_object_set (priv->main_menu, "visible", !is_fullscreen, NULL);

  gtk_action_set_visible (priv->action[ACTION_LEAVE_FULLSCREEN], is_fullscreen);
  g_object_set (gtk_ui_manager_get_widget (priv->ui_manager, "/Toolbar/LeaveFullscreenSep"),
                "visible", is_fullscreen,
                "draw", FALSE,
                NULL);

  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (priv->action[ACTION_FULLSCREEN]),
                                is_fullscreen);
#endif
}

static void
action_fullscreen_state_cb (GSimpleAction *action,
                            GVariant *state,
                            gpointer user_data)
{
  GtkWindow *window = user_data;

  g_simple_action_set_state (action, state);

  ar_debug_print (AR_DEBUG_WINDOW_STATE,
                  "[window %p] fullscreen_toggled_cb, %s fullscreen\n",
                  window,
                  g_variant_get_boolean (state) ? "going" : "leaving");

  if (g_variant_get_boolean (state)) {
    gtk_window_fullscreen (window);
  } else {
    gtk_window_unfullscreen (window);
  }
}

#if 0
static void
leave_fullscreen_cb (GtkAction *action,
                     GtkWindow *window)
{
  gtk_window_unfullscreen (window);
}
#endif

static void
click_to_move_changed_cb (GSettings *settings,
                         const char *key,
                         gpointer user_data)
{
  AisleriotWindow *window = user_data;
  AisleriotWindowPrivate *priv = window->priv;
  gboolean click_to_move;

  click_to_move = g_settings_get_boolean (settings, key);

  aisleriot_game_set_click_to_move (priv->game, click_to_move);
  ar_style_set_click_to_move (priv->board_style, click_to_move);
}

static void
enable_sound_changed_cb (GSettings *settings,
                         const char *key,
                         gpointer user_data)
{
#ifdef ENABLE_SOUND
  gboolean enabled;

  enabled = g_settings_get_boolean (settings, key);

  ar_sound_enable (enabled);
#endif /* ENABLE_SOUND */
}

#ifdef HAVE_CLUTTER
static void
enable_animations_changed_cb (GSettings *settings,
                              const char *key,
                              gpointer user_data)
{
  AisleriotWindow *window = user_data;
  AisleriotWindowPrivate *priv = window->priv;
  gboolean enabled;

  enabled = g_settings_get_boolean (settings, key);

  ar_style_set_enable_animations (priv->board_style, enabled);
}
#endif /* HAVE_CLUTTER */

static void
action_hint_cb (GSimpleAction *action,
                GVariant *parameter,
                gpointer user_data)
{
  AisleriotWindow *window = user_data;
  AisleriotWindowPrivate *priv = window->priv;
  char *message;
  GtkWidget *dialog;

  /* If the game hasn't started yet, getting a hint starts it */
  aisleriot_game_start (priv->game);

  if (priv->hint_dialog) {
    gtk_widget_destroy (priv->hint_dialog);
    priv->hint_dialog = NULL;
  }

  /* Only can show hints for running games */
  if (aisleriot_game_get_state (priv->game) != GAME_RUNNING)
    return;

  message = aisleriot_game_get_hint (priv->game);
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

  priv->hint_dialog = dialog;
  g_signal_connect (dialog, "response",
                    G_CALLBACK (gtk_widget_destroy), NULL);
  g_signal_connect (dialog, "destroy",
                    G_CALLBACK (gtk_widget_destroyed), &priv->hint_dialog);

  g_free (message);
}

static void
action_deal_cb (GSimpleAction *action,
                GVariant *parameter,
                gpointer user_data)
{
  AisleriotWindow *window = user_data;
  AisleriotWindowPrivate *priv = window->priv;

  aisleriot_game_deal_cards (priv->game);
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
#if 0
  AisleriotWindowPrivate *priv = window->priv;
  char **recent_games, **new_recent;
  gsize i, n_recent = 0, n_new_recent = 0;

  if (!game_module)
    return;

  g_settings_get (priv->state_settings, AR_STATE_RECENT_GAMES_KEY, "^as", &recent_games);

  if (recent_games == NULL) {
    new_recent = g_new (char *, 2);
    new_recent[0] = g_strdup (game_module);
    new_recent[1] = NULL;
    n_new_recent = 1;
  } else {
    n_recent = g_strv_length (recent_games);
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

  g_settings_set_strv (priv->state_settings, AR_STATE_RECENT_GAMES_KEY,
                       (const char * const *) new_recent);
  g_strfreev (new_recent);
#endif
}

/* Card Theme menu */

static void
aisleriot_window_take_card_theme (AisleriotWindow *window,
                                  ArCardTheme *theme /* adopting */)
{
  AisleriotWindowPrivate *priv = window->priv;
  GtkWidget *widget = GTK_WIDGET (window);

  if (theme == priv->theme)
    return;

  if (priv->theme) {
    g_object_unref (priv->theme);
  }
  priv->theme = theme;

  if (gtk_widget_has_screen (widget)) {
    const cairo_font_options_t *font_options;

    font_options = gdk_screen_get_font_options (gtk_widget_get_screen (widget));
    ar_card_theme_set_font_options (theme, font_options);
  }

  ar_style_set_card_theme (priv->board_style, theme);
}

static void
card_theme_changed_cb (GSettings *settings,
                       const char *key,
                       AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  const char *theme_name;
  ArCardTheme *theme;

  g_settings_get (settings, key, "&s", &theme_name);

  theme = ar_card_themes_get_theme_by_name (priv->theme_manager, theme_name);
  if (!theme) {
    /* Last-ditch fallback: try getting *any* theme */
    theme = ar_card_themes_get_theme_any (priv->theme_manager);
  }
  if (theme) {
    aisleriot_window_take_card_theme (window, theme /* adopts */);
  } else {
    /* FIXMEchpe: FUCK, what now? Panic! */
    /* Put up some UI, and exit! */
    g_assert_not_reached ();
  }
}

/* Callbacks */

/* Game state synchronisation */

static void
sync_game_score (AisleriotGame *game,
                 GParamSpec *pspec,
                 AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  gtk_label_set_text (GTK_LABEL (priv->score_label),
                      aisleriot_game_get_score (game));
}

static void
sync_game_state (AisleriotGame *game,
                 GParamSpec *pspec,
                 AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  GAction *action;
  guint state;

  state = aisleriot_game_get_state (priv->game);

  /* Can only get hints while the game is running */
  action = g_action_map_lookup_action (G_ACTION_MAP (window), "hint");
  g_simple_action_set_enabled (G_SIMPLE_ACTION (action),
                               state == GAME_BEGIN || state == GAME_RUNNING);

  if (state == GAME_RUNNING) {
    ar_clock_start (AR_CLOCK (priv->clock));
  } else {
    ar_clock_stop (AR_CLOCK (priv->clock));
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
  GAction *action;
  gboolean enabled;

  g_object_get (game, "can-undo", &enabled, NULL);

  action = g_action_map_lookup_action (G_ACTION_MAP (window), "undo");
  g_simple_action_set_enabled (G_SIMPLE_ACTION (action), enabled);

  /* The restart game validity condition is the same as for undo. */
  action = g_action_map_lookup_action (G_ACTION_MAP (window), "restart-game");
  g_simple_action_set_enabled (G_SIMPLE_ACTION (action), enabled);
}

static void
sync_game_redoable (AisleriotGame *game,
                    GParamSpec *pspec,
                    AisleriotWindow *window)
{
  GAction *action;
  gboolean enabled;

  g_object_get (game, "can-redo", &enabled, NULL);

  action = g_action_map_lookup_action (G_ACTION_MAP (window), "redo");
  g_simple_action_set_enabled (G_SIMPLE_ACTION (action), enabled);
}

static void
sync_game_dealable (AisleriotGame *game,
                    GParamSpec *pspec,
                    AisleriotWindow *window)
{
  GAction *action;
  gboolean enabled;

  g_object_get (game, "can-deal", &enabled, NULL);

  action = g_action_map_lookup_action (G_ACTION_MAP (window), "deal");
  g_simple_action_set_enabled (G_SIMPLE_ACTION (action), enabled);
}

static void
game_type_changed_cb (AisleriotGame *game,
                      AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  char *game_name;
  const char *game_module;
  guint features;
  gboolean dealable;
  gboolean show_scores;

  priv->changing_game_type = TRUE;

  game_name = aisleriot_game_get_name (game);

  gtk_window_set_title (GTK_WINDOW (window), game_name);

  g_free (game_name);

  game_module = aisleriot_game_get_game_module (game);
  add_recently_played_game (window, game_module);

  /* install_options_menu (window); */

  update_statistics_display (window);

  features = aisleriot_game_get_features (game);

  dealable = (features & FEATURE_DEALABLE) != 0;
#if 0 // fixme
  gtk_action_set_visible (priv->action[ACTION_DEAL], dealable);
#endif

  ar_clock_reset (AR_CLOCK (priv->clock));

  gtk_statusbar_pop (priv->statusbar, priv->game_message_id);
  gtk_statusbar_pop (priv->statusbar, priv->board_message_id);

  show_scores = (features & FEATURE_SCORE_HIDDEN) == 0;
  g_object_set (priv->score_box, "visible", show_scores, NULL);

  priv->changing_game_type = FALSE;
}

static void
game_new_cb (AisleriotGame *game,
             AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  update_statistics_display (window);

  ar_clock_reset (AR_CLOCK (priv->clock));

  gtk_statusbar_pop (priv->statusbar, priv->game_message_id);
  gtk_statusbar_pop (priv->statusbar, priv->board_message_id);
}

static void
game_statusbar_message_cb (AisleriotGame *game,
                           const char *message,
                           AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
  guint id = priv->game_message_id;

  gtk_statusbar_pop (priv->statusbar, id);
  if (message != NULL) {
    gtk_statusbar_push (priv->statusbar, id, message);
  }
}

static void
game_exception_response_cb (GtkWidget *dialog,
                            int response,
                            AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;
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
  aisleriot_game_new_game (priv->game);

  gtk_widget_grab_focus (GTK_WIDGET (priv->board));
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
  gtk_widget_error_bell (GTK_WIDGET (embed));
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

G_DEFINE_TYPE (AisleriotWindow, aisleriot_window, GTK_TYPE_APPLICATION_WINDOW)

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
  ar_card_theme_set_font_options (priv->theme, font_options);

  /* FIXMEchpe: clear the cached cards in the slots?? */
}

static gboolean
aisleriot_window_state_event (GtkWidget *widget,
                              GdkEventWindowState *event)
{
  AisleriotWindow *window = AISLERIOT_WINDOW (widget);
  AisleriotWindowPrivate *priv = window->priv;

  if (event->changed_mask & (GDK_WINDOW_STATE_FULLSCREEN | GDK_WINDOW_STATE_MAXIMIZED)) {
    gboolean is_fullscreen;

    is_fullscreen = (event->new_window_state & GDK_WINDOW_STATE_FULLSCREEN) != 0;

    set_fullscreen_actions (window, is_fullscreen);

    set_fullscreen_button_active (window);
  }

  if (event->changed_mask & GDK_WINDOW_STATE_ICONIFIED) {
    if (aisleriot_game_get_state (priv->game) == GAME_RUNNING) {
      gboolean is_iconified;

      is_iconified = (event->new_window_state & GDK_WINDOW_STATE_ICONIFIED);

      aisleriot_game_set_paused (priv->game, is_iconified);
      if (is_iconified) {
        ar_clock_stop (AR_CLOCK (priv->clock));
      } else {
        ar_clock_start (AR_CLOCK (priv->clock));
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
  const GActionEntry gaction_entries[] = {
    { "help",                action_help_cb,           "s",  NULL, NULL },
    { "about",               action_about_cb,          NULL, NULL, NULL },
    { "preferences",         action_preferences_cb,    NULL, NULL, NULL },
    { "quit",                action_close_window_cb,   NULL, NULL, NULL },
    { "install-themes",      action_install_themes_cb, NULL, NULL, NULL },
    { "new-game",            action_new_game_cb,       NULL, NULL, NULL },
    { "restart-game",        action_restart_game_cb,   NULL, NULL, NULL },
    { "select-game",         action_select_game_cb,    NULL, NULL, NULL },
    { "statistics",          action_statistics_cb,     NULL, NULL, NULL },
    { "deal",                action_deal_cb,           NULL, NULL, NULL },
    { "hint",                action_hint_cb,           NULL, NULL, NULL },
    { "undo",                action_undo_cb,           NULL, NULL, NULL },
    { "redo",                action_redo_cb,           NULL, NULL, NULL },

    { "show-toolbar",        action_toggle_state_cb,   NULL, "true",  action_show_toolbar_state_cb      },
    { "show-statusbar",      action_toggle_state_cb,   NULL, "true",  action_show_statusbar_state_cb    },
    { "fullscreen",          action_toggle_state_cb,   NULL, "false", action_fullscreen_state_cb        },
  };
  AisleriotWindowPrivate *priv;
  GAction *action;
  GtkStatusbar *statusbar;
  GtkWidget *statusbar_hbox, *label, *time_box;
#ifdef HAVE_CLUTTER
  ClutterContainer *stage;
#endif
  ArApplication *app = AR_APP;

  priv = window->priv = AISLERIOT_WINDOW_GET_PRIVATE (window);

  gtk_widget_init_template (GTK_WIDGET (window));

  priv->settings = g_settings_new (AR_SETTINGS_SCHEMA);

  priv->state_settings = ar_application_state_settings_new (app, AR_STATE_SCHEMA);

  priv->fullscreen = FALSE;

  priv->game = aisleriot_game_new ();

  priv->theme_manager = ar_application_get_card_themes (app);

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

  /* GAction setup */
  g_action_map_add_action_entries (G_ACTION_MAP (window),
                                   gaction_entries, G_N_ELEMENTS (gaction_entries),
                                   window);

#if 0 // fixme
  /* Hide the "Deal" action initially, since not all games support it */
  gtk_action_set_visible (priv->action[ACTION_DEAL], FALSE);
#endif

#if 0 // fixme
  /* Set labels for toolbar items */
  action = gtk_action_group_get_action (priv->action_group, "Select");
  g_object_set (action, "short-label", _("Select Game"), NULL);
#endif

  statusbar = priv->statusbar;
  priv->game_message_id = gtk_statusbar_get_context_id (priv->statusbar, "game-message");
  priv->board_message_id = gtk_statusbar_get_context_id (priv->statusbar, "board-message");

#ifdef HAVE_CLUTTER
  g_signal_connect (priv->board_actor, "status-message",
                    G_CALLBACK (board_status_message_cb), window);
#else
  g_signal_connect (priv->board, "status-message",
                    G_CALLBACK (board_status_message_cb), window);
#endif

  statusbar_hbox = gtk_statusbar_get_message_area (statusbar);
  gtk_box_set_spacing (GTK_BOX (statusbar_hbox), 24);

  /* Score */
  priv->score_box = gtk_hbox_new (12, FALSE);
  label = gtk_label_new (_("Score:"));
  gtk_widget_show (label);
  gtk_box_pack_start (GTK_BOX (priv->score_box), label, FALSE, FALSE, 0);
  priv->score_label = gtk_label_new ("   0");
  gtk_widget_show (priv->score_label);
  gtk_box_pack_start (GTK_BOX (priv->score_box), priv->score_label, FALSE, FALSE, 0);
  gtk_box_pack_end (GTK_BOX (statusbar_hbox), priv->score_box, FALSE, FALSE, 0);

  ar_atk_util_add_atk_relation (label, priv->score_label, ATK_RELATION_LABEL_FOR);
  ar_atk_util_add_atk_relation (priv->score_label, label, ATK_RELATION_LABELLED_BY);

  time_box = gtk_hbox_new (12, FALSE);
  label = gtk_label_new (_("Time:"));
  gtk_box_pack_start (GTK_BOX (time_box), label, FALSE, FALSE, 0);
  priv->clock = ar_clock_new ();
  gtk_box_pack_start (GTK_BOX (time_box), priv->clock, FALSE, FALSE, 0);
  gtk_box_pack_end (GTK_BOX (statusbar_hbox), time_box, FALSE, FALSE, 0);
  gtk_widget_show_all (time_box);

  ar_atk_util_add_atk_relation (label, priv->clock, ATK_RELATION_LABEL_FOR);
  ar_atk_util_add_atk_relation (priv->clock, label, ATK_RELATION_LABELLED_BY);

  /* The actions and menus are done. The
   * recent games menu will be updated when the initial game loads.
   */

  action = g_action_map_lookup_action (G_ACTION_MAP (window), "show-toolbar");
  priv->toolbar_visible = g_settings_get_boolean (priv->settings, AR_SETTINGS_SHOW_TOOLBAR_KEY);
  g_action_change_state (action, g_variant_new_boolean (priv->toolbar_visible));

#if 0 // fixme
  action = gtk_action_group_get_action (priv->action_group, "RecentMenu");
  g_object_set (action, "hide-if-empty", FALSE, NULL);
#endif

  action = g_action_map_lookup_action (G_ACTION_MAP (window), "show-statusbar");
  priv->statusbar_visible = g_settings_get_boolean (priv->settings, AR_SETTINGS_SHOW_STATUSBAR_KEY);
  g_action_change_state (action, g_variant_new_boolean (priv->statusbar_visible));

  set_fullscreen_actions (window, FALSE);

  /* Now set up the widgets */
  gtk_box_pack_start (GTK_BOX (priv->main_vbox), GTK_WIDGET (priv->board), TRUE, TRUE, 0);
  gtk_widget_show (GTK_WIDGET (priv->board));

  /* Synchronise */
  sync_game_score (priv->game, NULL, window);
  g_signal_connect (priv->game, "notify::score",
                    G_CALLBACK (sync_game_score), window);

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

  /* Prefs */
  click_to_move_changed_cb (priv->settings, AR_SETTINGS_CLICK_TO_MOVE_KEY, window);
  g_signal_connect (priv->settings, "changed::" AR_SETTINGS_CLICK_TO_MOVE_KEY,
                    G_CALLBACK (click_to_move_changed_cb), window);

  enable_sound_changed_cb (priv->settings, AR_SETTINGS_ENABLE_SOUND_KEY, window);
  g_signal_connect (priv->settings, "changed::" AR_SETTINGS_ENABLE_SOUND_KEY,
                    G_CALLBACK (enable_sound_changed_cb), window);

#ifdef HAVE_CLUTTER
  enable_animations_changed_cb (priv->settings, AR_SETTINGS_ENABLE_ANIMATIONSKEY, window);
  g_signal_connect (priv->settings, "changed::" AR_SETTINGS_ENABLE_ANIMATIONS_KEY,
                    G_CALLBACK (enable_animations_changed_cb), window);
#endif

  card_theme_changed_cb (priv->settings, AR_SETTINGS_CARD_THEME_KEY, window);
  g_signal_connect (priv->settings, "changed::" AR_SETTINGS_CARD_THEME_KEY,
                    G_CALLBACK (card_theme_changed_cb), window);

  /* Restore window state */
  ar_gsettings_bind_window_state (AR_SETTINGS_WINDOW_STATE_PATH, GTK_WINDOW (window));

  /* Initial focus is in the board */
  gtk_widget_grab_focus (GTK_WIDGET (priv->board));
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

  if (priv->hint_dialog) {
    gtk_widget_destroy (priv->hint_dialog);
    g_assert (priv->hint_dialog == NULL);
  }
  if (priv->game_over_dialog) {
    gtk_widget_destroy (priv->game_over_dialog);
    g_assert (priv->game_over_dialog == NULL);
  }
  if (priv->prefs_dialog) {
    gtk_widget_destroy (priv->prefs_dialog);
    g_assert (priv->prefs_dialog == NULL);
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

  g_clear_object (&priv->state_settings);

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

  g_clear_object (&priv->theme);

  g_signal_handlers_disconnect_matched (priv->settings,
                                        G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL, NULL, window);
  g_clear_object (&priv->settings);

  g_signal_handlers_disconnect_matched (priv->game,
                                        G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL, NULL, window);
  g_object_unref (priv->game);

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

  g_type_class_add_private (gobject_class, sizeof (AisleriotWindowPrivate));

  gtk_widget_class_set_template_from_resource (widget_class, "/org/gnome/aisleriot/ui/window.ui");
  gtk_widget_class_bind_child (widget_class, AisleriotWindowPrivate, main_vbox);
  gtk_widget_class_bind_child (widget_class, AisleriotWindowPrivate, toolbar);
  gtk_widget_class_bind_child (widget_class, AisleriotWindowPrivate, statusbar);
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
                       "show-menubar", TRUE,
                       NULL);
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
  AisleriotWindowPrivate *priv = data->window->priv;
  GError *error = NULL;
  GRand *rand;

  if (!aisleriot_game_load_game (priv->game, data->game_module, &error)) {
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
  g_settings_set_string (priv->state_settings, AR_STATE_LAST_GAME_KEY, data->game_module);

  rand = data->rand;
  data->rand = NULL;

  aisleriot_game_new_game_with_rand (priv->game, rand /* adopted */);

  gtk_widget_grab_focus (GTK_WIDGET (priv->board));

  return FALSE;
}

static void
free_load_idle_data (LoadIdleData *data)
{
  data->window->priv->load_idle_id = 0;

  if (data->rand)
    g_rand_free (data->rand);

  g_free (data->game_module);
  g_slice_free (LoadIdleData, data);
}

/**
 * aisleriot_window_set_game:
 * @window:
 * @game_module: (allow-none): a UTF-8 string, or %NULL
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
  AisleriotWindowPrivate *priv = window->priv;
  LoadIdleData *data;

  /* We'll do this on idle */
  if (priv->load_idle_id != 0) {
    g_source_remove (priv->load_idle_id);
  }

  data = g_slice_new (LoadIdleData);
  data->window = window;
  data->rand = rand; /* adopted */

  if (game_module) {
    data->game_module = g_strdup (game_module);
  } else {
    char *pref;

    pref = g_settings_get_string (priv->state_settings, AR_STATE_LAST_GAME_KEY);
    data->game_module = ar_filename_to_game_module (pref);
    g_free (pref);
  }

  priv->load_idle_id = g_idle_add_full (G_PRIORITY_LOW,
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
  AisleriotWindowPrivate *priv = window->priv;

  return aisleriot_game_get_game_module (priv->game);
}


AisleriotGame *
ar_window_get_game (AisleriotWindow *window)
{
  AisleriotWindowPrivate *priv = window->priv;

  return priv->game;
}
