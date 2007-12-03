/*
 *  Copyright © 1998, 2003 Jonathan Blandford <jrb@alum.mit.edu>
 *  Copyright © 2007 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope game it will be useful,
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
#include <string.h>
#include <unistd.h>
#include <time.h>

#include <libguile.h>

#ifndef HAVE_GUILE_1_8
#include "guile16-compat.h"
#endif

#include <glib.h>
#include <glib/gi18n.h>

#include "conf.h"
#include "util.h"

#include <libgames-support/games-files.h>

#include "game.h"

#include "board.h"

#define DELAYED_CALLBACK_DELAY (50)

struct _AisleriotGame
{
  GObject parent_instance;

  GPtrArray *slots;

  char *game_file;
  guint timeout;
  guint score;
  guint32 seed;
  guint delayed_call_timeout_id;

  time_t start_time;

  double width;
  double height;

  /* Game callbacks */
  SCM start_game_lambda;
  SCM button_pressed_lambda;
  SCM button_released_lambda;
  SCM button_clicked_lambda;
  SCM button_double_clicked_lambda;
  SCM game_over_lambda;
  SCM winning_game_lambda;
  SCM hint_lambda;
  SCM get_options_lambda;
  SCM apply_options_lambda;
  SCM timeout_lambda;
  SCM droppable_lambda;
  SCM dealable_lambda;

  guint click_to_move : 1;
  guint can_undo : 1;
  guint can_redo : 1;
  guint can_deal : 1;
  guint show_score : 1;
  guint features : 3; /* enough bits for ALL_FEATURES */
  guint state : 3; /* enough bits for LAST_GAME_STATE */
  guint had_exception : 1;
  guint paused : 1;
};

/* The one and only game */
AisleriotGame *app_game;

enum
{
  PROP_0,
  PROP_CAN_UNDO,
  PROP_CAN_REDO,
  PROP_CAN_DEAL,
  PROP_GAME_FILE,
  PROP_SCORE,
  PROP_STATE,
};

enum
{
  GAME_TYPE,
  GAME_CLEARED,
  GAME_NEW,
  GAME_MESSAGE,
  SLOT_CHANGED,
  EXCEPTION,
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

G_DEFINE_TYPE (AisleriotGame, aisleriot_game, G_TYPE_OBJECT);

/* helper functions */

static void
clear_delayed_call (AisleriotGame *game)
{
  if (game->delayed_call_timeout_id != 0) {
    g_source_remove (game->delayed_call_timeout_id);
    game->delayed_call_timeout_id = 0;
  }
}

static void
set_game_state (AisleriotGame *game,
                guint state)
{
  if (game->state != state) {
    game->state = state;

    if (state == GAME_RUNNING) {
      /* Record start time */
      game->start_time = time (NULL);
    }

    g_object_notify (G_OBJECT (game), "state");
  }
}

static void
set_game_score (AisleriotGame *game,
                guint score)
{
  if (score != game->score) {
    game->score = score;

    g_object_notify (G_OBJECT (game), "score");
  }
}

static void
set_game_undoable (AisleriotGame *game,
                   gboolean enabled)
{
  if (enabled != game->can_undo) {
    game->can_undo = enabled;

    g_object_notify (G_OBJECT (game), "can-undo");
  }
}

static void
set_game_redoable (AisleriotGame *game,
                   gboolean enabled)
{
  if (enabled != game->can_redo) {
    game->can_redo = enabled;

    g_object_notify (G_OBJECT (game), "can-redo");
  }
}

static void
set_game_dealable (AisleriotGame *game,
                   gboolean enabled)
{
  if (enabled != game->can_deal) {
    game->can_deal = enabled != FALSE;

    g_object_notify (G_OBJECT (game), "can-deal");
  }
}

/* Yes, there is a race condition here, but since this is all
   per user they're going to have to be either playing impossibly fast
   or using sharing an account. In the later case they won't be caring
   about the statistics. */
static void
update_statistics (AisleriotGame *game)
{
  AisleriotStatistic current_stats;
  time_t t;

  aisleriot_conf_get_statistic (game->game_file, &current_stats);

  current_stats.total++;

  if (game->state == GAME_WON) {
    current_stats.wins++;

    if (game->paused) {
      t = game->start_time;
    } else {
      t = time (NULL) - game->start_time;
    }

    if (t > 0) {
      if ((current_stats.best == 0) || (t < current_stats.best)) {
	current_stats.best = t;
      }
      if (t > current_stats.worst) {
	current_stats.worst = t;
      }
    }
  }

  aisleriot_conf_set_statistic (game->game_file, &current_stats);
}

static void
clear_slots (AisleriotGame *game,
             gboolean notify)
{
  guint i, n_slots;

  n_slots = game->slots->len;
  for (i = 0; i < n_slots; ++i) {
    Slot *slot = game->slots->pdata[i];

    g_byte_array_free (slot->cards, TRUE);
    g_ptr_array_free (slot->card_images, TRUE);

    g_slice_free (Slot, slot);
  }

  g_ptr_array_set_size (game->slots, 0);

  if (notify) {
    g_signal_emit (game, signals[GAME_CLEARED], 0);
  }
}

static Slot *
get_slot (AisleriotGame *game,
          gint slotid)
{
  guint i, n_slots;

  n_slots = game->slots->len;
  for (i = 0; i < n_slots; ++i) {
    Slot *hslot = game->slots->pdata[i];

    if (hslot->id == slotid)
      return hslot;
  }

  return NULL;
}


/* Scheme helpers */

typedef struct {
  SCM lambda;
  guint n_args;
  SCM arg1;
  SCM arg2;
  SCM arg3;
  SCM retval;
} CallData;

#define CALL_DATA_INIT  { 0, 0, 0, 0, 0, 0 };

static void
cscmi_write_exception_details (int error_fd, SCM tag, SCM throw_args)
{
  AisleriotGame *game = app_game;
  char *message;
  SCM port;
  SCM stack;
  char *game_name;
  GPtrArray *slots;
  guint i, n_slots;

  game_name = aisleriot_game_get_name (game);
  message = g_strdup_printf ("Variation: %s\n", game_name);
  g_free (game_name);
  game_name = NULL;

  write (error_fd, message, strlen (message));
  g_free (message);

  message = g_strdup_printf ("Seed: %u\n", game->seed);
  write (error_fd, message, strlen (message));
  g_free (message);

  message = "Scheme error:\n\t";
  write (error_fd, message, strlen (message));

  port = scm_fdopen (scm_from_int (error_fd),
                     scm_mem2string ("w", sizeof (char)));
  scm_display (throw_args, port);
  scm_fsync (port);

  message = "\nScheme tag:\n\t";
  write (error_fd, message, strlen (message));
  scm_display (tag, port);
  scm_fsync (port);

  message = "\n\nBacktrace:\n";
  write (error_fd, message, strlen (message));
  stack = scm_fluid_ref (SCM_VARIABLE_REF (scm_the_last_stack_fluid_var));
  if (!SCM_FALSEP (stack)) {
    scm_display_backtrace (stack, port, SCM_UNDEFINED, SCM_UNDEFINED);
    scm_fsync (port);
  } else {
    message = "\tNo backtrace available.\n";
    write (error_fd, message, strlen (message));
  }

  message = "\n\nDeck State:\n";
  write (error_fd, message, strlen (message));

  slots = aisleriot_game_get_slots (game);

  n_slots = slots->len;
  if (n_slots > 0) {
    for (i = 0; i < n_slots; ++i) {
      Slot *slot = slots->pdata[i];
      GByteArray *cards = slot->cards;
      guint n_cards;

      message = g_strdup_printf ("\tSlot %d\n", slot->id);
      write (error_fd, message, strlen (message));
      g_free (message);

      n_cards = cards->len;
      if (n_cards > 0) {
        guint8 *data = cards->data;
        guint count;

        for (count = 0; count < n_cards; ++count) {
          Card card = CARD (data[count]);

          if (count == 0)
            message = "\t\t";
          else
            message = ", ";
          write (error_fd, message, strlen (message));

          message = g_strdup_printf ("(%d %d %s)",
                                     CARD_GET_SUIT (card),
                                     CARD_GET_RANK (card),
                                     /* See c2scm_card below */
                                     CARD_GET_FACE_DOWN (card) ? "#f" : "#t");
          write (error_fd, message, strlen (message));
          g_free (message);
          count++;
          if (count == 5) {
            message = "\n";
            write (error_fd, message, strlen (message));
            count = 0;
          }
        }
        if (count != 0) {
          message = "\n";
          write (error_fd, message, strlen (message));
        }
      } else {
        message = "\t\t(Empty)\n";
        write (error_fd, message, strlen (message));
      }
    }
  } else {
    message = "\tNo cards in deck\n";
    write (error_fd, message, strlen (message));
  }
}

/* Called when we get an exception from guile.  We launch bug-buddy with the
 * exception information:
 */
static SCM
cscmi_catch_handler (gpointer data G_GNUC_UNUSED,
                     SCM tag,
                     SCM throw_args)
{
  AisleriotGame *game = app_game;
  int error_fd;
  char *error_file = NULL;
  GError *error = NULL;

  if (game->had_exception)
    goto out;

  error_fd = g_file_open_tmp ("arcrashXXXXXX", &error_file, &error);
  if (error_fd >= 0) {
    cscmi_write_exception_details (error_fd, tag, throw_args);
    close (error_fd);

    /* Tell the frontend about the problem */
    g_signal_emit (game, signals[EXCEPTION], 0, error_file);
    g_free (error_file);
  } else {
    g_warning ("A scheme exception occurred, and aisleriot could not create a temporary file to report it: %s",
               error->message);
    g_error_free (error);
  }
  
out:
  /* This game is over, but don't count it in the statistics */
  set_game_state (game, GAME_LOADED);

  return SCM_UNDEFINED;
}

static SCM
cscmi_call_lambda (void *user_data)
{
  CallData *data = (CallData *) user_data;

  /* FIXMEchpe: crash when data->lambda isn't the right type, e.g. "sol --variation sol.scm" */

  switch (data->n_args) {
    case 0:
      data->retval = scm_call_0 (data->lambda);
      break;
    case 1:
      data->retval = scm_call_1 (data->lambda, data->arg1);
      break;
    case 2:
      data->retval = scm_call_2 (data->lambda, data->arg1, data->arg2);
      break;
    case 3:
      data->retval = scm_call_3 (data->lambda, data->arg1, data->arg2, data->arg3);
      break;
    default:
      g_assert_not_reached ();
  }

  return SCM_EOL;
}

static SCM
cscmi_eval_string (const char *string)
{
  return scm_internal_stack_catch (SCM_BOOL_T,
                                   (scm_t_catch_body) scm_c_eval_string, (void *) string,
                                   cscmi_catch_handler, NULL);
}

static SCM
c2scm_card (Card card)
{
  return scm_cons (scm_from_uint (CARD_GET_RANK (card)),
                   scm_cons (scm_from_uint (CARD_GET_SUIT (card)),
                             scm_cons (SCM_BOOL (!CARD_GET_FACE_DOWN (card)),
                                       SCM_EOL)));
}

static Card
scm2c_card (SCM card_data)
{
  Card card;
  guint rank, suit, face_down;

  card.value = 0;

  rank = scm_to_int (SCM_CAR (card_data));
  suit = scm_to_int (SCM_CADR (card_data));
  face_down = !(SCM_NFALSEP (SCM_CADDR (card_data)));

  card.attr.rank = rank;
  card.attr.suit = suit;
  card.attr.face_down = face_down;

  return card;
}

static SCM
c2scm_deck (guint8 *cards,
            guint n_cards)
{
  SCM scm_cards = SCM_EOL;
  guint i;

  for (i = 0; i < n_cards; ++i) {
    scm_cards = scm_cons (c2scm_card (CARD (cards[i])), scm_cards);
  }

  return scm_cards;
}

static void
cscmi_slot_set_cards (Slot *slot,
                      SCM cards)
{
  AisleriotGame *game = app_game;
  SCM list_el;
  guint8 *data = NULL;
  guint i, n_cards = 0;

  if (SCM_NFALSEP (cards)) {
    for (list_el = cards; list_el != SCM_EOL; list_el = SCM_CDR (list_el)) {
      ++n_cards;
    }

    data = g_alloca (n_cards);
    i = n_cards;

    for (list_el = cards; list_el != SCM_EOL; list_el = SCM_CDR (list_el)) {
      data[--i] = CARD_UINT (scm2c_card (SCM_CAR (list_el)));
    }
  }

  /* Don't set the new cards if the same cards are already there.
   * This saves us lots of updates on undo/redo.
   */
  if (slot->cards->len == n_cards &&
      memcmp (slot->cards->data, data, n_cards) == 0)
    return;

  g_byte_array_set_size (slot->cards, 0);

  aisleriot_game_slot_add_cards (game, slot, data, n_cards);
}    

static void
cscmi_add_slot (SCM slot_data)
{
  AisleriotGame *game = app_game;
  Slot *slot;
  gboolean expanded_down = FALSE;
  gboolean expanded_right = FALSE;
  int expansion_depth = 0;

  /* FIXMEchpe check whether this can happen; if it does, fix assumption that it doesn't in Board class. */
  if (game->state > GAME_BEGIN) {
    g_warning ("Trying to add slot after game has started!\n");
  }

#ifdef HAVE_GUILE_1_8
#define CHECK_EXPANSION(string,object) (scm_is_true (scm_equal_p (scm_from_locale_symbol (string), object)))
#else
#define CHECK_EXPANSION(string,object) (!strcmp (string, SCM_CHARS (object)))
#endif

  if (CHECK_EXPANSION ("expanded", SCM_CAR (SCM_CADDR (slot_data)))) {
    expanded_down = TRUE;
  } else if (CHECK_EXPANSION ("expanded-right", SCM_CAR (SCM_CADDR (slot_data)))) {
    expanded_right = TRUE;
  } else if (CHECK_EXPANSION ("partially-expanded", SCM_CAR (SCM_CADDR (slot_data)))) {
    expanded_down = TRUE;
    expansion_depth = scm_to_int (SCM_CADDR (SCM_CADDR (slot_data)));
  } else if (CHECK_EXPANSION ("partially-expanded-right", SCM_CAR (SCM_CADDR (slot_data)))) {
    expanded_right = TRUE;
    expansion_depth = scm_to_int (SCM_CADDR (SCM_CADDR (slot_data)));
  }

#undef CHECK_EXPANSION

  /* create and initialize slot */
  slot = g_slice_new0 (Slot);
  g_ptr_array_add (game->slots, slot);

  slot->id = scm_to_int (SCM_CAR (slot_data));

  slot->cards = g_byte_array_sized_new (SLOT_CARDS_N_PREALLOC);
  slot->exposed = 0;
  slot->x = scm_num2dbl (SCM_CAR (SCM_CADR (SCM_CADDR (slot_data))), NULL);
  slot->y = scm_num2dbl (SCM_CADR (SCM_CADR (SCM_CADDR (slot_data))), NULL);

  slot->expansion_depth = expansion_depth;

  slot->expansion.dx = 0.0;
  slot->expanded_down = expanded_down != FALSE;
  slot->expanded_right = expanded_right != FALSE;

  slot->card_images = g_ptr_array_sized_new (SLOT_CARDS_N_PREALLOC);

  slot->needs_update = TRUE;

  /* this will update the slot length too */
  cscmi_slot_set_cards (slot, SCM_CADR (slot_data));
}

/* Scheme functions */
static SCM
scm_gettext (SCM message)
{
  char *input, *output;
  SCM translated;

  if (!scm_is_string (message))
    return message;

  input = scm_to_locale_string (message);
  if (!input)
    return message;

  output = _(input);

  if (input != output) {
    translated = scm_from_locale_string (output);
  } else {
    translated = message;
  }

  free (input);

  return translated;
}

static SCM
scm_undo_set_sensitive (SCM in_state)
{
  AisleriotGame *game = app_game;
  gboolean state;

  state = SCM_NFALSEP (in_state) ? TRUE : FALSE;
  set_game_undoable (game, state);

  return SCM_EOL;
}

static SCM
scm_redo_set_sensitive (SCM in_state)
{
  AisleriotGame *game = app_game;
  gboolean state;

  state = SCM_NFALSEP (in_state) ? TRUE : FALSE;
  set_game_redoable (game, state);

  return SCM_EOL;
}

static SCM
scm_dealable_set_sensitive (SCM in_state)
{
  AisleriotGame *game = app_game;
  gboolean state;

  state = SCM_NFALSEP (in_state) ? TRUE : FALSE;
  set_game_dealable (game, state);

  return SCM_EOL;
}

static SCM
scm_get_feature_word (void)
{
  AisleriotGame *game = app_game;

  g_return_val_if_fail (game != NULL, SCM_EOL);
  
  return scm_from_uint (game->features);
}

static SCM
scm_set_feature_word (SCM features)
{
  AisleriotGame *game = app_game;

  g_return_val_if_fail (game != NULL, SCM_EOL);

  game->features = scm_to_uint (features);

  return SCM_EOL;
}

static SCM
scm_set_statusbar_message (SCM message)
{
  AisleriotGame *game = app_game;
  char *str, *translated;

  if (!scm_is_string (message))
    return SCM_EOL;

  str = scm_to_locale_string (message);
  if (!str)
    return SCM_EOL;

  translated = g_strstrip (g_strdup (gettext (str)));
  g_signal_emit (game, signals[GAME_MESSAGE], 0, translated);

  free (str);
  g_free (translated);

  return SCM_EOL;
}

static SCM
scm_set_surface_layout (SCM surface)
{
  AisleriotGame *game = app_game;

  if (surface != SCM_EOL) {
    SCM list_el;

    clear_slots (game, TRUE);

    for (list_el = surface; list_el != SCM_EOL; list_el = SCM_CDR (list_el))
      cscmi_add_slot (SCM_CAR (list_el));
  }

  return SCM_EOL;
}


static SCM
scm_reset_surface (void)
{
  AisleriotGame *game = app_game;

  clear_slots (game, TRUE);
  return SCM_EOL;
}

static SCM
ar_scm_add_slot (SCM slot)
{
  cscmi_add_slot (slot);
  return SCM_EOL;
}

static SCM
scm_set_slot_x_expansion (SCM scm_slot_id,
                          SCM new_exp_val)
{
  AisleriotGame *game = app_game;
  Slot *slot;

  slot = get_slot (game, scm_num2int (scm_slot_id, SCM_ARG1, NULL));

  /* We should only set the x expansion for right-expanded slots! */
  g_return_val_if_fail (slot->expanded_right, SCM_EOL);
  /* Cannot set x and y expansion at the same time */
  g_return_val_if_fail (!slot->dy_set, SCM_EOL);

  slot->expansion.dx = scm_num2dbl (new_exp_val, NULL);
  slot->dx_set = TRUE;

  /* We don't need to emit the slot-changed signal here,
   * since we should be here only during game initialisation,
   * which means that there will be a slot-changed later anyway.
   */
  return SCM_EOL;
}

static SCM
scm_set_slot_y_expansion (SCM scm_slot_id,
                          SCM new_exp_val)
{
  AisleriotGame *game = app_game;
  Slot *slot;

  slot = get_slot (game, scm_num2int (scm_slot_id, SCM_ARG1, NULL));

  /* We should only set the y expansion for down-expanded slots! */
  g_return_val_if_fail (slot->expanded_down, SCM_EOL);
  /* Cannot set x and y expansion at the same time */
  g_return_val_if_fail (!slot->dx_set, SCM_EOL);

  slot->expansion.dy = scm_num2dbl (new_exp_val, NULL);
  slot->dy_set = TRUE;

  /* We don't need to emit the slot-changed signal here,
   * since we should be here only during game initialisation,
   * which means that there will be a slot-changed later anyway.
   */
  return SCM_EOL;
}

static SCM
scm_get_slot (SCM scm_slot_id)
{
  AisleriotGame *game = app_game;
  Slot *slot;

  slot = get_slot (game, scm_num2int (scm_slot_id, SCM_ARG1, NULL));

  if (!slot)
    return SCM_EOL;

  return scm_cons (scm_slot_id,
                   scm_cons (c2scm_deck (slot->cards->data, slot->cards->len),
                             SCM_EOL));
}

static SCM
scm_set_cards (SCM scm_slot_id,
               SCM new_cards)
{
  AisleriotGame *game = app_game;
  Slot *slot;

  slot = get_slot (game, scm_num2int (scm_slot_id, SCM_ARG1, NULL));

  cscmi_slot_set_cards (slot, new_cards);

  return SCM_BOOL_T;
}

static SCM
scm_set_lambda (SCM start_game_lambda,
                SCM pressed_lambda,
                SCM released_lambda,
                SCM clicked_lambda,
                SCM dbl_clicked_lambda,
                SCM game_over_lambda,
                SCM winning_game_lambda,
                SCM hint_lambda,
                SCM rest)
{
  AisleriotGame *game = app_game;

  game->start_game_lambda = start_game_lambda;
  game->button_pressed_lambda = pressed_lambda;
  game->button_released_lambda = released_lambda;
  game->button_clicked_lambda = clicked_lambda;
  game->button_double_clicked_lambda = dbl_clicked_lambda;
  game->game_over_lambda = game_over_lambda;
  game->winning_game_lambda = winning_game_lambda;
  game->hint_lambda = hint_lambda;

  game->get_options_lambda = SCM_CAR (rest);
  rest = SCM_CDR (rest);

  game->apply_options_lambda = SCM_CAR (rest);
  rest = SCM_CDR (rest);

  game->timeout_lambda = SCM_CAR (rest);
  rest = SCM_CDR (rest);

  if (game->features & FEATURE_DROPPABLE) {
    game->droppable_lambda = SCM_CAR (rest);
    rest = SCM_CDR (rest);
  } else {
    game->droppable_lambda = SCM_UNDEFINED;
  }

  if (game->features & FEATURE_DEALABLE) {
    game->dealable_lambda = SCM_CAR (rest);
    rest = SCM_CDR (rest);
  } else {
    game->dealable_lambda = SCM_UNDEFINED;
  }

  return SCM_EOL;
}

static SCM
scm_myrandom (SCM range)
{
  return scm_from_uint32 (g_random_int_range (0, scm_to_int (range)));
}

static SCM
scm_click_to_move_p (void)
{
  /* This only affects elevator and escalator games. Their code claims
   * that in click-to-move makes no sense to move the cards away, but that's
   * bogus. Just always return FALSE here instead of
   * game->click_to_move ? SCM_BOOL_T : SCM_BOOL_F
   */
  return SCM_BOOL_F;
}

static SCM
scm_get_score (void)
{
  AisleriotGame *game = app_game;

  return scm_from_uint (game->score);
}

static SCM
scm_set_score (SCM new)
{
  AisleriotGame *game = app_game;

  set_game_score (game, scm_num2int (new, SCM_ARG1, NULL));
  return new;
}

static SCM
scm_add_to_score (SCM delta)
{
  AisleriotGame *game = app_game;
  guint new_score;

  new_score = game->score + scm_num2int (delta, SCM_ARG1, NULL);
  set_game_score (game, new_score);
  return scm_from_uint (new_score);
}

static SCM
scm_set_timeout (SCM new)
{
  AisleriotGame *game = app_game;

  g_warning ("(set-timeout) unimplemented\n");

  game->timeout = scm_num2int (new, SCM_ARG1, NULL);

  return new;
}

static SCM
scm_get_timeout (void)
{
  AisleriotGame *game = app_game;

  g_warning ("(get-timeout) unimplemented\n");

  return scm_from_uint (game->timeout);
}

static void
scm_delayed_call_destroy_data (SCM callback)
{
  AisleriotGame *game = app_game;

  scm_gc_unprotect_object (callback);

  game->delayed_call_timeout_id = 0;
}

static gboolean
scm_execute_delayed_function (SCM callback)
{
  AisleriotGame *game = app_game;
  CallData data = CALL_DATA_INIT;

  /* We set game->delayed_call_timeout_id to 0 _before_ calling |callback|,
   * since it might install a new delayed call.
   */
  game->delayed_call_timeout_id = 0;

  data.lambda = callback;
  data.n_args = 0;
  scm_internal_stack_catch (SCM_BOOL_T,
                            cscmi_call_lambda, &data,
                            cscmi_catch_handler, NULL);

  aisleriot_game_test_end_of_game (game);

  return FALSE;
}

static SCM
scm_delayed_call (SCM callback)
{
  AisleriotGame *game = app_game;

  /* We can only have one pending delayed call! */
  if (game->delayed_call_timeout_id != 0) {
    return scm_throw (scm_from_locale_symbol ("invalid-call"),
                      scm_cons (scm_from_locale_string ("Already have a delayed callback pending."), SCM_EOL));
  }

  /* We need to protect the callback data from being GC'd until the
   * timeout has run.
   */
  scm_gc_protect_object (callback);

  g_timeout_add_full (G_PRIORITY_LOW,
                      DELAYED_CALLBACK_DELAY,
                      (GSourceFunc) scm_execute_delayed_function,
                      callback,
                      (GDestroyNotify) scm_delayed_call_destroy_data);

  return SCM_BOOL_T;
}

static void
cscm_init (void)
{
  /* Enable useful debugging options. */
  SCM_DEVAL_P = 1;
  SCM_BACKTRACE_P = 1;
  SCM_RECORD_POSITIONS_P = 1;
  SCM_RESET_DEBUG_MODE;

  /* Let the scheme side of things know about our C functions. */
  scm_c_define_gsubr ("set-feature-word!", 1, 0, 0, scm_set_feature_word);
  scm_c_define_gsubr ("get-feature-word", 0, 0, 0, scm_get_feature_word);
  scm_c_define_gsubr ("set-statusbar-message", 1, 0, 0,
                      scm_set_statusbar_message);
  scm_c_define_gsubr ("set-surface-layout", 1, 0, 0, scm_set_surface_layout);
  scm_c_define_gsubr ("reset-surface", 0, 0, 0, scm_reset_surface);
  scm_c_define_gsubr ("add-slot", 1, 0, 0, ar_scm_add_slot);
  scm_c_define_gsubr ("get-slot", 1, 0, 0, scm_get_slot);
  scm_c_define_gsubr ("set-cards-c!", 2, 0, 0, scm_set_cards);
  scm_c_define_gsubr ("set-slot-y-expansion!", 2, 0, 0,
                      scm_set_slot_y_expansion);
  scm_c_define_gsubr ("set-slot-x-expansion!", 2, 0, 0,
                      scm_set_slot_x_expansion);
  scm_c_define_gsubr ("set-lambda", 8, 0, 1, scm_set_lambda);
  scm_c_define_gsubr ("random", 1, 0, 0, scm_myrandom);
  scm_c_define_gsubr ("click-to-move?", 0, 0, 0, scm_click_to_move_p);
  scm_c_define_gsubr ("get-score", 0, 0, 0, scm_get_score);
  scm_c_define_gsubr ("set-score!", 1, 0, 0, scm_set_score);
  scm_c_define_gsubr ("get-timeout", 0, 0, 0, scm_get_timeout);
  scm_c_define_gsubr ("set-timeout!", 1, 0, 0, scm_set_timeout);
  scm_c_define_gsubr ("delayed-call", 1, 0, 0, scm_delayed_call);
  scm_c_define_gsubr ("add-to-score!", 1, 0, 0, scm_add_to_score);
  scm_c_define_gsubr ("_", 1, 0, 0, scm_gettext);
  scm_c_define_gsubr ("undo-set-sensitive", 1, 0, 0, scm_undo_set_sensitive);
  scm_c_define_gsubr ("redo-set-sensitive", 1, 0, 0, scm_redo_set_sensitive);
  scm_c_define_gsubr ("dealable-set-sensitive", 1, 0, 0, scm_dealable_set_sensitive);
}

static void
update_game_dealable (AisleriotGame *game)
{
  CallData data = CALL_DATA_INIT;

  if ((game->features & FEATURE_DEALABLE) == 0)
    return;

  data.lambda = game->dealable_lambda;
  data.n_args = 0;
  scm_internal_stack_catch (SCM_BOOL_T,
                            cscmi_call_lambda, &data,
                            cscmi_catch_handler, NULL);

  set_game_dealable (game, SCM_NFALSEP (data.retval));
}

static void
cscmi_start_game_lambda (double *width,
                         double *height)
{
  AisleriotGame *game = app_game;
  CallData data = CALL_DATA_INIT;

  data.lambda = game->start_game_lambda;
  data.n_args = 0;
  scm_internal_stack_catch (SCM_BOOL_T,
                            cscmi_call_lambda, &data,
                            cscmi_catch_handler, NULL);

  *width = scm_num2double (SCM_CAR (data.retval), 0, NULL);
  *height = scm_num2double (SCM_CADR (data.retval), 0, NULL);
}

static gboolean
cscmi_game_over_lambda (void)
{
  AisleriotGame *game = app_game;
  CallData data = CALL_DATA_INIT;

  data.lambda = game->game_over_lambda;
  data.n_args = 0;
  scm_internal_stack_catch (SCM_BOOL_T,
                            cscmi_call_lambda, &data,
                            cscmi_catch_handler, NULL);

  return SCM_NFALSEP (data.retval);
}

static gboolean
cscmi_winning_game_lambda (void)
{
  AisleriotGame *game = app_game;
  CallData data = CALL_DATA_INIT;

  data.lambda = game->winning_game_lambda;
  data.n_args = 0;
  scm_internal_stack_catch (SCM_BOOL_T,
                            cscmi_call_lambda, &data,
                            cscmi_catch_handler, NULL);

  return SCM_NFALSEP (data.retval);
}

static gboolean
cscmi_eval_installed_file (const char *filename,
                           GError **error)
{
  char *path;

  g_return_val_if_fail (filename != NULL, FALSE);

  /* FIXMEchpe: install exception handlers and set error if an exception occurs while loading the file? */

  if (!g_str_has_suffix (filename, ".scm")) {
    g_set_error (error, AISLERIOT_GAME_ERROR, GAME_ERROR_GENERIC,
                 "%s is not a scheme file", filename);
    return FALSE;
  }

  path = games_build_filename (GAMESDIR, filename);
  if (g_file_test (path, G_FILE_TEST_EXISTS) &&
      g_file_test (path, G_FILE_TEST_IS_REGULAR)) {
    scm_c_primitive_load (path);
    g_free (path);
    return TRUE;
  }

  g_set_error (error, AISLERIOT_GAME_ERROR, GAME_ERROR_GENERIC,
               _("Aisleriot cannot load the file \"%s\". "
                 "Please check your Aisleriot installation."),
               path);
  g_free (path);

  return FALSE;
}

/* Class implementation */

static void
aisleriot_game_init (AisleriotGame *game)
{
  game->state = GAME_UNINITIALISED;

  game->slots = g_ptr_array_sized_new (SLOT_CARDS_N_PREALLOC);

  game->timeout = 60 * 60;
}

static GObject *
aisleriot_game_constructor (GType type,
                            guint n_construct_properties,
                            GObjectConstructParam *construct_params)
{
  GObject *object;

  g_assert (app_game == NULL);

  object = G_OBJECT_CLASS (aisleriot_game_parent_class)->constructor
             (type, n_construct_properties, construct_params);

  app_game = AISLERIOT_GAME (object);

  return object;
}

static void
aisleriot_game_finalize (GObject *object)
{
  AisleriotGame *game = AISLERIOT_GAME (object);

  /* Update the statistics */
  /* FIXMEchpe unless we're saving to session and exiting because of session logout! */
  if (game->state >= GAME_RUNNING) {
    update_statistics (game);
  }

  clear_delayed_call (game);
  clear_slots (game, FALSE);
  g_ptr_array_free (game->slots, TRUE);

  g_free (game->game_file);

  app_game = NULL;

  G_OBJECT_CLASS (aisleriot_game_parent_class)->finalize (object);
}

static void
aisleriot_game_get_property (GObject *object,
			     guint prop_id,
			     GValue *value,
			     GParamSpec *pspec)
{
  AisleriotGame *game = AISLERIOT_GAME (object);

  switch (prop_id) {
    case PROP_CAN_UNDO:
      g_value_set_boolean (value, game->can_undo);
      break;
    case PROP_CAN_REDO:
      g_value_set_boolean (value, game->can_redo);
      break;
    case PROP_CAN_DEAL:
      g_value_set_boolean (value, game->can_deal);
      break;
    case PROP_GAME_FILE:
      g_value_set_string (value, game->game_file);
      break;
    case PROP_SCORE:
      g_value_set_uint (value, game->score);
      break;
  }
}

static void
aisleriot_game_class_init (AisleriotGameClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GType param_types[] = { G_TYPE_STRING | G_SIGNAL_TYPE_STATIC_SCOPE };
  GType ptr_types[] = { G_TYPE_POINTER };

  gobject_class->constructor = aisleriot_game_constructor;
  gobject_class->finalize = aisleriot_game_finalize;
  gobject_class->get_property = aisleriot_game_get_property;

  signals[GAME_TYPE] =
    g_signal_newv ("game-type",
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   (GSignalFlags) (G_SIGNAL_RUN_LAST),
                   NULL,
                   NULL, NULL,
                   g_cclosure_marshal_VOID__VOID,
                   G_TYPE_NONE,
                   0, NULL);

  signals[GAME_CLEARED] =
    g_signal_newv ("game-cleared",
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   (GSignalFlags) (G_SIGNAL_RUN_LAST),
                   NULL,
                   NULL, NULL,
                   g_cclosure_marshal_VOID__VOID,
                   G_TYPE_NONE,
                   0, NULL);

  signals[GAME_NEW] =
    g_signal_newv ("game-new",
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   (GSignalFlags) (G_SIGNAL_RUN_LAST),
                   NULL,
                   NULL, NULL,
                   g_cclosure_marshal_VOID__VOID,
                   G_TYPE_NONE,
                   0, NULL);

  signals[SLOT_CHANGED] =
    g_signal_newv ("slot-changed",
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   (GSignalFlags) (G_SIGNAL_RUN_LAST),
                   NULL,
                   NULL, NULL,
                   g_cclosure_marshal_VOID__POINTER,
                   G_TYPE_NONE,
                   1, ptr_types);

 signals[GAME_MESSAGE] =
    g_signal_newv ("message",
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   (GSignalFlags) (G_SIGNAL_RUN_LAST),
                   NULL,
                   NULL, NULL,
                   g_cclosure_marshal_VOID__STRING,
                   G_TYPE_NONE,
                   1, param_types);

 signals[EXCEPTION] =
    g_signal_newv ("exception",
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   (GSignalFlags) (G_SIGNAL_RUN_LAST),
                   NULL,
                   NULL, NULL,
                   g_cclosure_marshal_VOID__STRING,
                   G_TYPE_NONE,
                   1, param_types);

  g_object_class_install_property
    (gobject_class,
     PROP_CAN_UNDO,
     g_param_spec_boolean ("can-undo", NULL, NULL,
                           FALSE,
                           G_PARAM_READABLE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB));

  g_object_class_install_property
    (gobject_class,
     PROP_CAN_REDO,
     g_param_spec_boolean ("can-redo", NULL, NULL,
                           FALSE,
                           G_PARAM_READABLE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB));

  g_object_class_install_property
    (gobject_class,
     PROP_CAN_DEAL,
     g_param_spec_boolean ("can-deal", NULL, NULL,
                           FALSE,
                           G_PARAM_READABLE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB));

  g_object_class_install_property
    (gobject_class,
     PROP_GAME_FILE,
     g_param_spec_string ("game-file", NULL, NULL,
                          NULL,
                          G_PARAM_READABLE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB));

  g_object_class_install_property
    (gobject_class,
     PROP_SCORE,
     g_param_spec_uint ("score", "", "",
                        0, G_MAXUINT, 0,
                        G_PARAM_READABLE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB));

  g_object_class_install_property
    (gobject_class,
     PROP_STATE,
     g_param_spec_uint ("state", "", "",
                        0, LAST_GAME_STATE, 0, 
                        G_PARAM_READABLE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB));

  /* Initialise our scheme interfaces */
  cscm_init ();
}

/* public API */

/**
 * aisleriot_game_error_quark:
 *
 * Returns: the #GQuark used for errors
 */
GQuark
aisleriot_game_error_quark (void)
{
  static GQuark quark = 0;

  if (G_UNLIKELY (quark == 0)) {
    quark = g_quark_from_static_string ("AisleRiotGameError");
  }

  return quark;
}

/**
 * aisleriot_game_new:
 *
 * Returns: a new #AisleriotGame
 */
AisleriotGame *
aisleriot_game_new (void)
{
  return g_object_new (AISLERIOT_TYPE_GAME, NULL);
}

/**
 * aisleriot_game_get_slots:
 * @game:
 *
 * Returns an array of #Slot.
 *
 * Returns: a #GPtrArray; it is owned by @game and must not be modified or freed
 */
GPtrArray *
aisleriot_game_get_slots (AisleriotGame *game)
{
  return game->slots;
}

/**
 * aisleriot_game_slot_add_cards:
 * @game:
 * @slot:
 * @cards:
 * @n_cards:
 *
 * Adds @n_cards from @cards to the top of @slot.
 */
void
aisleriot_game_slot_add_cards (AisleriotGame *game,
                               Slot *slot,
                               guint8 *cards,
                               guint n_cards)
{
  g_byte_array_append (slot->cards, cards, n_cards);

  g_signal_emit (game, signals[SLOT_CHANGED], 0, slot);
}

/**
 * aisleriot_game_get_state:
 * @game:
 *
 * Returns the current game state.
 *
 * Returns: a #AisleriotGameState
 */
guint
aisleriot_game_get_state (AisleriotGame *game)
{
  return game->state;
}

/**
 * aisleriot_game_get_features:
 * @game:
 *
 * Returns the game features.
 *
 * Returns: a #AisleriotGameFeatures
 */
guint
aisleriot_game_get_features (AisleriotGame *game)
{
  return game->features;
}

/**
 * aisleriot_game_start:
 * @game:
 *
 * Starts the game, if it hasn't started already.
 */
void
aisleriot_game_start (AisleriotGame *game)
{
  if (game->state == GAME_BEGIN) {
    set_game_state (game, GAME_RUNNING);
  }
}

/**
 * aisleriot_game_set_paused:
 * @game:
 * @paused: whether to pause the game
 *
 * Stops or resumes the game timer.
 */
void
aisleriot_game_set_paused (AisleriotGame *game,
                           gboolean paused)
{
  g_return_if_fail (game->state == GAME_RUNNING);

  paused = paused != FALSE;
  if (paused == game->paused)
    return;

  game->paused = paused;

  /* (Re)store the start time */
  game->start_time = time (NULL) - game->start_time;
}

/**
 * aisleriot_game_undo_move:
 * @game:
 *
 * Undoes the last move.
 */
void
aisleriot_game_undo_move (AisleriotGame *game)
{
  /* If the game had ended, reset to RUNNING */
  if (game->state >= GAME_OVER) {
    set_game_state (game, GAME_RUNNING);
  }

  cscmi_eval_string ("(undo)");

  update_game_dealable (game);
}

/**
 * aisleriot_game_redo_move:
 * @game:
 *
 * Redoes the last undone move.
 */
void
aisleriot_game_redo_move (AisleriotGame *game)
{
  cscmi_eval_string ("(redo)");

  /* We need this now that you can undo a losing move. */
  aisleriot_game_test_end_of_game (game);
}

/**
 * aisleriot_game_load_game:
 * @game:
 * @game_file: the game file to load
 * @error: a location for a #GError
 *
 * Loads the game @game_file into @game. If there is an error,
 * @error is filled in and %FALSE returned.
 *
 * Returns: %TRUE iff loading the game succeeded
 */
gboolean
aisleriot_game_load_game (AisleriotGame *game,
                          const char *game_file,
                          GError **error)
{
  GObject *object = G_OBJECT (game);
  GError *err = NULL;
  gboolean retval = FALSE;

  g_return_val_if_fail (game_file != NULL && game_file[0] != '\0', FALSE);

  if (game->game_file != NULL &&
      strcmp (game_file, game->game_file) == 0)
    return TRUE;

  /* We use @game_file as a filename, but since it'll be used in configuration
   * as a key, we also require it to be valid UTF-8 !
   */
  if (!g_utf8_validate (game_file, -1, NULL)) {
    g_set_error (error, AISLERIOT_GAME_ERROR, GAME_ERROR_GENERIC,
                 "Invalid UTF-8");
    return FALSE;
  }

  /* If we're aborting an old game count it as a failure for
   * statistical purposes.
   */
  if (game->state >= GAME_RUNNING) {
    update_statistics (game);
  }

  g_object_freeze_notify (object);

  clear_delayed_call (game);
  set_game_state (game, GAME_UNINITIALISED);
  set_game_score (game, 0);
  set_game_undoable (game, FALSE);
  set_game_redoable (game, FALSE);
  set_game_dealable (game, FALSE);
  game->features = 0;
  game->had_exception = FALSE;

  /* Although this line slows down game switching by a noticeable amount, we
    * add it here in order to make sure all the original functions are
    * "clean". */
  if (!cscmi_eval_installed_file ("sol.scm", error))
    goto out;

  /* Now try to load the game rules */
  if (!cscmi_eval_installed_file (game_file, &err)) {
    if (strcmp (game_file, DEFAULT_VARIATION) == 0) {
      g_propagate_error (error, err);
    } else {
      g_error_free (err);

      g_set_error (error, AISLERIOT_GAME_ERROR, GAME_ERROR_FALLBACK,
                   "%s %s",
                   _("Aisleriot cannot find the last game you played."),
                   _("This usually occurs when you run an older version of Aisleriot "
                     "which does not have the game you last played. "
                     "The default game, Klondike, is being started instead."));
    }

    goto out;
  }

  g_free (game->game_file);
  game->game_file = g_strdup (game_file);

  set_game_state (game, GAME_LOADED);

  g_object_notify (object, "game-file");

  retval = TRUE;

out:

  g_object_thaw_notify (object);
    
  if (retval) {
    g_signal_emit (game, signals[GAME_TYPE], 0);
  }

  return retval;
}

/**
 * aisleriot_game_new_game:
 * @game:
 * @seed: a pointer to a #guint, or %NULL
 *
 * Starts a new game of the currently loaded game type.
 * If @seed is non-%NULL, the value it points to is used
 * as the game number; otherwise a random number is used.
 */
void
aisleriot_game_new_game (AisleriotGame *game,
                         guint *seed)
{
  GObject *object = G_OBJECT (game);

  g_return_if_fail (game->state > GAME_UNINITIALISED);

  g_object_freeze_notify (object);

  /* Clear exception */
  game->had_exception = FALSE;
  game->paused = FALSE;

  /* If we're aborting an old game count it as a failure for
   * statistical purposes.
   * But treat a restart as part of the same game. Eventually either
   * the player will win or lose and then it gets counted.
   */
  /* FIXMEchpe: this allows cheating the statistics by doing
   * Restart, then New Game.
   */
  if (game->state >= GAME_RUNNING &&
      (!seed || *seed != game->seed)) {
    update_statistics (game);
  }

  clear_delayed_call (game);
  /* The game isn't actually in progress until the user makes a move */
  set_game_state (game, GAME_BEGIN);
  set_game_score (game, 0);
  set_game_undoable (game, FALSE);
  set_game_redoable (game, FALSE);

  /* It is possible for some games to not have any moves right from the
   * start. If this happens we redeal.
   */
  /* FIXMEchpe we should have a maximum number of tries, and then bail out! */
  do {
    if (seed) {
      game->seed = *seed;
    } else {
      game->seed = g_random_int ();
    }
    seed = NULL;

    /* FIXMEchpe: We should NOT re-seed here, but just get another random number! */
    g_random_set_seed (game->seed);

    cscmi_start_game_lambda (&game->width, &game->height);
    scm_c_eval_string ("(start-game)");
  } while (!cscmi_game_over_lambda ());

  update_game_dealable (game);

  g_object_thaw_notify (object);

  g_signal_emit (game, signals[GAME_NEW], 0);
}

/**
 * aisleriot_game_restart_game:
 * @game:
 *
 * Restarts the current game from the beginning.
 */
void
aisleriot_game_restart_game (AisleriotGame *game)
{
  guint seed = game->seed;

  aisleriot_game_new_game (game, &seed);
}

/**
 * aisleriot_game_get_game_file:
 * @game:
 *
 * Returns the game filename for the currently loaded game type.
 *
 * Returns: a string owned by @game; you must not modify or free it
 */
const char *
aisleriot_game_get_game_file (AisleriotGame *game)
{
  return game->game_file;
}

/**
 * aisleriot_game_get_name:
 * @game:
 *
 * Returns the name of the currently loaded game type in a form
 * suitable for presentation to the user.
 *
 * Returns: a newly allocated string
 */
char *
aisleriot_game_get_name (AisleriotGame *game)
{
  return aisleriot_util_get_display_filename (game->game_file);
}

/**
 * aisleriot_game_get_game_file:
 * @game:
 * @width: a location to store the width in
 * @height: a location to store the height in
 *
 * Returns the board size to use for the game, in game coordinates.
 */
void
aisleriot_game_get_geometry (AisleriotGame *game,
                             double *width,
                             double *height)
{
  *width = game->width;
  *height = game->height;
}

/**
 * aisleriot_game_get_seed:
 * @game:
 *
 * Returns the seed of the current game.
 *
 * Returns: a number
 */
guint
aisleriot_game_get_seed (AisleriotGame *game)
{
  return game->seed;
}

/**
 * aisleriot_game_drag_valid:
 * @game:
 * @slot_id:
 * @cards:
 * 
 * Checks whether the cards @cards can be moved off of slot @slot.
 */
gboolean
aisleriot_game_drag_valid (AisleriotGame *game,
                           int slot_id,
                           guint8 *cards,
                           guint n_cards)
{
  CallData data = CALL_DATA_INIT;

  data.lambda = game->button_pressed_lambda;
  data.n_args = 2;
  data.arg1 = scm_from_int (slot_id);
  data.arg2 = c2scm_deck (cards, n_cards);
  scm_gc_protect_object (data.arg2);
  scm_internal_stack_catch (SCM_BOOL_T,
                            cscmi_call_lambda, &data,
                            cscmi_catch_handler, NULL);
  scm_gc_unprotect_object (data.arg2);

  return SCM_NFALSEP (data.retval);
}

/**
 * aisleriot_game_drop_valid:
 * @game:
 * @start_slot:
 * @end_slot:
 * @cards:
 *
 * Checks whether the game allows moving cards @cards from @start_slot
 * to @end_slot.
 */
gboolean
aisleriot_game_drop_valid (AisleriotGame *game,
                           int start_slot,
                           int end_slot,
                           guint8 *cards,
                           guint n_cards)
{
  CallData data = CALL_DATA_INIT;

  if ((game->features & FEATURE_DROPPABLE) == 0)
    return FALSE;

  data.lambda = game->droppable_lambda;
  data.n_args = 3;
  data.arg1 = scm_from_int (start_slot);
  data.arg2 = c2scm_deck (cards, n_cards);
  scm_gc_protect_object (data.arg2);
  data.arg3 = scm_from_int (end_slot);
  scm_internal_stack_catch (SCM_BOOL_T,
                            cscmi_call_lambda, &data,
                            cscmi_catch_handler, NULL);
  scm_gc_unprotect_object (data.arg2);

  return SCM_NFALSEP (data.retval);
}

/**
 * aisleriot_game_drop_cards:
 * @game:
 * @start_slot:
 * @end_slot:
 * @cards:
 *
 * Moves cards @cards from @start_slot to @end_slot.
 */
gboolean
aisleriot_game_drop_cards (AisleriotGame *game,
                           int start_slot,
                           int end_slot,
                           guint8 *cards,
                           guint n_cards)
{
  CallData data = CALL_DATA_INIT;

  data.lambda = game->button_released_lambda;
  data.n_args = 3;
  data.arg1 = scm_from_int (start_slot);
  data.arg2 = c2scm_deck (cards, n_cards);
  scm_gc_protect_object (data.arg2);
  data.arg3 = scm_from_int (end_slot);
  scm_internal_stack_catch (SCM_BOOL_T,
                            cscmi_call_lambda, &data,
                            cscmi_catch_handler, NULL);
  scm_gc_unprotect_object (data.arg2);

  return SCM_NFALSEP (data.retval);
}

/**
 * aisleriot_game_button_clicked_lambda:
 * @game:
 * @slot_id:
 *
 * Performs the "click" action on @slot.
 * 
 * Returns: %TRUE iff any action was taken
 */
gboolean
aisleriot_game_button_clicked_lambda (AisleriotGame *game,
                                      int slot_id)
{
  CallData data = CALL_DATA_INIT;

  data.lambda = game->button_clicked_lambda;
  data.n_args = 1;
  data.arg1 = scm_from_int (slot_id);
  scm_internal_stack_catch (SCM_BOOL_T,
                            cscmi_call_lambda, &data,
                            cscmi_catch_handler, NULL);

  return SCM_NFALSEP (data.retval);
}

/**
 * aisleriot_game_button_double_clicked_lambda:
 * @game:
 * @slot_id:
 *
 * Performs the "double click" action on @slot.
 * 
 * Returns: %TRUE iff any action was taken
 */
gboolean
aisleriot_game_button_double_clicked_lambda (AisleriotGame *game,
                                             int slot_id)
{
  CallData data = CALL_DATA_INIT;

  data.lambda = game->button_double_clicked_lambda;
  data.n_args = 1;
  data.arg1 = scm_from_int (slot_id);
  scm_internal_stack_catch (SCM_BOOL_T,
                            cscmi_call_lambda, &data,
                            cscmi_catch_handler, NULL);

  return SCM_NFALSEP (data.retval);
}

/**
 * aisleriot_game_hint_lambda:
 * @game:
 *
 * Gets a hint.
 *
 * Returns: a #SCM containing the hint
 */
SCM
aisleriot_game_hint_lambda (AisleriotGame *game)
{
  CallData data = CALL_DATA_INIT;

  data.lambda = game->hint_lambda;
  data.n_args = 0;
  scm_internal_stack_catch (SCM_BOOL_T,
                            cscmi_call_lambda, &data,
                            cscmi_catch_handler, NULL);

  return data.retval;
}

/**
 * aisleriot_game_get_options_lambda:
 * @game:
 *
 * Returns: a #SCM containing the game options
 */
SCM
aisleriot_game_get_options_lambda (AisleriotGame *game)
{
  CallData data = CALL_DATA_INIT;

  data.lambda = game->get_options_lambda;
  data.n_args = 0;
  scm_internal_stack_catch (SCM_BOOL_T,
                            cscmi_call_lambda, &data,
                            cscmi_catch_handler, NULL);

  return data.retval;
}

/**
 * aisleriot_game_apply_options_lambda:
 * @game:
 * @options:
 *
 * Applies @options.
 *
 * Returns: a #SCM with status information
 */
SCM
aisleriot_game_apply_options_lambda (AisleriotGame *game, SCM options)
{
  CallData data = CALL_DATA_INIT;

  data.lambda = game->apply_options_lambda;
  data.n_args = 1;
  data.arg1 = options;
  scm_internal_stack_catch (SCM_BOOL_T,
                            cscmi_call_lambda, &data,
                            cscmi_catch_handler, NULL);

  return data.retval;
}

/**
 * aisleriot_game_timeout_lambda:
 * @game:
 *
 * Checks whether @game has timed out
 *
 * Returns: %TRUE iff the game has timed out
 */
gboolean
aisleriot_game_timeout_lambda (AisleriotGame *game)
{
  CallData data = CALL_DATA_INIT;

  data.lambda = game->timeout_lambda;
  data.n_args = 0;
  scm_internal_stack_catch (SCM_BOOL_T,
                            cscmi_call_lambda, &data,
                            cscmi_catch_handler, NULL);

  return SCM_NFALSEP (data.retval);
}

/**
 * aisleriot_game_record_move:
 * @game:
 * @slot_id:
 * @cards:
 * @n_cards:
 *
 * Records the state of @slot and the and cards on it.
 */
void
aisleriot_game_record_move (AisleriotGame *game,
                            int slot_id,
                            guint8 *cards,
                            guint n_cards)
{
  SCM cardlist;

  cardlist = c2scm_deck (cards, n_cards);
  scm_gc_protect_object (cardlist);

  scm_call_2 (scm_c_eval_string ("record-move"),
              scm_from_int (slot_id), cardlist);

  scm_gc_unprotect_object (cardlist);
}

/**
 * aisleriot_game_end_move:
 * @game:
 *
 * Adds the state information stored with aisleriot_game_record_move
 * to the undo history.
 */
void
aisleriot_game_end_move (AisleriotGame *game)
{
  scm_call_0 (scm_c_eval_string ("end-move"));
}

/**
 * aisleriot_game_discard_move:
 * @game:
 *
 * Discards the state information stored with aisleriot_game_record_move.
 */
void
aisleriot_game_discard_move (AisleriotGame *game)
{
  scm_call_0 (scm_c_eval_string ("discard-move"));
}

/**
 * aisleriot_game_update_game_state:
 * @game:
 *
 * Tests whether the game is over.
 */
void
aisleriot_game_test_end_of_game (AisleriotGame *game)
{
  aisleriot_game_end_move (game);

  update_game_dealable (game);

  if (game->state < GAME_OVER) {
    if (!cscmi_game_over_lambda ()) {
      guint new_state;

      if (cscmi_winning_game_lambda ()) {
        new_state = GAME_WON;
      } else {
        new_state = GAME_OVER;
      }

      set_game_state (game, new_state);

      /* Don't update the statistics here; we'll do that when
       * starting the next game (or on finalize).
       */
    }
  }
}

/**
 * aisleriot_game_set_click_to_move:
 * @game:
 *
 * Sets whether the game is using clicks to move, or drag-and-drop.
 */
void
aisleriot_game_set_click_to_move (AisleriotGame *game,
                                  gboolean enabled)
{
  game->click_to_move = enabled != FALSE;
}

/**
 * aisleriot_game_generate_exception:
 * @game:
 *
 * Generates an artificial scheme exception.
 */
void
aisleriot_game_generate_exception (AisleriotGame *game)
{
  cscmi_eval_string ("(how-about-a-nice-crash?)");
}

/**
 * aisleriot_game_deal_cards:
 * @game:
 *
 * Deals the next card or cards, if possible.
 *
 * Returns: %TRUE iff cards were dealt
 */
void
aisleriot_game_deal_cards (AisleriotGame *game)
{
  aisleriot_game_record_move (game, -1, NULL, 0);

  cscmi_eval_string ("(do-deal-next-cards)");
    
  aisleriot_game_end_move (game);
  aisleriot_game_test_end_of_game (game);
}
