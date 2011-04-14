/*
 * Copyright © 1998, 2003 Jonathan Blandford <jrb@alum.mit.edu>
 * Copyright © 2007 Christian Persch
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
#include <time.h>

#include <libguile.h>

#include <glib.h>
#include <glib/gi18n.h>

#ifdef HAVE_CLUTTER
#include <clutter/clutter.h>
#endif

#include "ar-debug.h"
#include "ar-runtime.h"
#include "ar-string-utils.h"

#include "conf.h"
#include "util.h"

#include "game.h"

#define DELAYED_CALLBACK_DELAY (50)

#define I_(string) g_intern_static_string (string)

struct _AisleriotGame
{
  GObject parent_instance;

  GPtrArray *slots;

  char *game_file;
  guint32 seed;
  guint delayed_call_timeout_id;

  GTimer *timer;

  int timeout;
  int score;

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
      /* Reset the timer */
      g_timer_start (game->timer);
    } else if (state >= GAME_OVER) {
      /* Stop the timer now so we will record the right time. See bug #514239. */
      g_timer_stop (game->timer);
    }

    g_object_notify (G_OBJECT (game), "state");
  }
}

static void
set_game_score (AisleriotGame *game,
                int score)
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

    t = (time_t) (g_timer_elapsed (game->timer, NULL) + 0.5);
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
    ArSlot *slot = game->slots->pdata[i];

#ifdef HAVE_CLUTTER
    if (slot->slot_renderer) {
      clutter_actor_destroy (slot->slot_renderer);
      g_object_unref (slot->slot_renderer);
    }

    g_byte_array_free (slot->old_cards, TRUE);
#else
    g_ptr_array_free (slot->card_images, TRUE);
#endif /* HAVE_CLUTTER */
    g_byte_array_free (slot->cards, TRUE);

    g_slice_free (ArSlot, slot);
  }

  g_ptr_array_set_size (game->slots, 0);

  if (notify) {
    g_signal_emit (game, signals[GAME_CLEARED], 0);
  }
}

static ArSlot *
get_slot (AisleriotGame *game,
          gint slotid)
{
  guint i, n_slots;

  n_slots = game->slots->len;
  for (i = 0; i < n_slots; ++i) {
    ArSlot *hslot = game->slots->pdata[i];

    if (hslot->id == slotid)
      return hslot;
  }

  return NULL;
}


/* Scheme helpers */

typedef struct {
  SCM lambda;
  SCM *args;
  gsize n_args;
  SCM retval;
  gboolean exception;
} CallData;

static char *
cscmi_exception_get_backtrace (SCM tag, SCM throw_args)
{
  AisleriotGame *game = app_game;
  SCM port;
  SCM stack;
  GPtrArray *slots;
  guint i, n_slots;
  GString *message;
  char *string;

  scm_dynwind_begin (0);

  message = g_string_sized_new (1024);

  g_string_append_printf (message, "Variation: %s\n", aisleriot_game_get_game_file (game));
  g_string_append_printf (message, "Seed: %u\n", game->seed);

  g_string_append (message, "Scheme error:\n\t");

  port = scm_open_output_string ();
  scm_display (throw_args, port);
  string = scm_to_locale_string (scm_get_output_string (port));
  scm_dynwind_free (string);
  scm_close_output_port (port);
  g_string_append (message, string);

  port = scm_open_output_string ();
  g_string_append (message, "\nScheme tag:\n\t");
  scm_display (tag, port);
  string = scm_to_locale_string (scm_get_output_string (port));
  scm_dynwind_free (string);
  scm_close_output_port (port);
  g_string_append (message, string);

  g_string_append (message, "\n\nBacktrace:\n");
  stack = scm_make_stack (SCM_BOOL_T, SCM_EOL);
  if (!scm_is_false (stack)) {
    port = scm_open_output_string ();
    scm_display_backtrace (stack, port, SCM_UNDEFINED, SCM_UNDEFINED);
    string = scm_to_locale_string (scm_get_output_string (port));
    scm_dynwind_free (string);
    scm_close_output_port (port);
    g_string_append (message, string);
  } else {
    g_string_append (message, "\tNo backtrace available.\n");
  }

  g_string_append (message, "\n\nDeck State:\n");

  slots = aisleriot_game_get_slots (game);

  n_slots = slots->len;
  if (n_slots > 0) {
    for (i = 0; i < n_slots; ++i) {
      ArSlot *slot = slots->pdata[i];
      GByteArray *cards = slot->cards;
      guint n_cards;

      g_string_append_printf (message, "\tSlot %d\n", slot->id);

      n_cards = cards->len;
      if (n_cards > 0) {
        guint8 *data = cards->data;
        guint count;

        for (count = 0; count < n_cards; ++count) {
          Card card = CARD (data[count]);

          if (count == 0) {
            g_string_append_c (message, '\t');
            g_string_append_c (message, '\t');
          } else {
            g_string_append_c (message, ' ');
            g_string_append_c (message, ',');
          }

          g_string_append_printf (message,
                                  "(%d %d %s)",
                                  CARD_GET_SUIT (card),
                                  CARD_GET_RANK (card),
                                  /* See c2scm_card below */
                                  CARD_GET_FACE_DOWN (card) ? "#f" : "#t");

          count++;
          if (count == 5) {
            g_string_append_c (message, '\n');
            count = 0;
          }
        }
        if (count != 0) {
          g_string_append_c (message, '\n');
        }
      } else {
        g_string_append (message,"\t\t(Empty)\n");
      }
    }
  } else {
    g_string_append (message, "\tNo cards in deck\n");
  }

  scm_dynwind_end ();

  return g_string_free (message, FALSE);
}

/* Called when we get an exception from guile.  We launch bug-buddy with the
 * exception information:
 */
static SCM
game_scm_pre_unwind_handler (void *user_data,
                             SCM tag,
                             SCM throw_args)
{
  CallData *data = (CallData *) user_data;
  AisleriotGame *game = app_game;
  char *message = NULL;
  int error_fd;
  char *error_file = NULL;
  GError *error = NULL;

  if (data)
    data->exception = TRUE;

  g_print ("preunwind handler\n");

  if (game->had_exception)
    goto out;

  message = cscmi_exception_get_backtrace (tag, throw_args);
  if (!message) {
    g_warning ("A scheme exception occurred, but there was no exception info\n");
    goto out;
  }

  g_print ("scheme exception: \n\n%s\n\n", message);
    goto out;

  error_fd = g_file_open_tmp ("arcrashXXXXXX", &error_file, &error);
  if (error_fd >= 0) {
    close (error_fd);
    g_file_set_contents (error_file, message, strlen (message), NULL);

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

  g_free (message);

  return SCM_UNDEFINED;
}

static SCM
game_scm_catch_handler (void *user_data,
                        SCM tag,
                        SCM throw_args)
{
  return SCM_UNDEFINED;
}

static SCM
game_scm_call_lambda (void *user_data)
{
  CallData *data = (CallData *) user_data;

#if SCM_MAJOR_VERSION >= 2
  return scm_call_n (data->lambda, data->args, data->n_args);
#else
  /* Guile 1.8 lacks the scm_call_n function */
  switch (data->n_args) {
    case 0:
      return scm_call_0 (data->lambda);
    case 1:
      return scm_call_1 (data->lambda, data->args[0]);
    case 2:
      return scm_call_2 (data->lambda, data->args[0], data->args[1]);
    case 3:
      return scm_call_3 (data->lambda, data->args[0], data->args[1], data->args[2]);
    default:
      g_assert_not_reached ();
  }
#endif
}

static gboolean
game_scm_call (SCM lambda,
               SCM *args,
               gsize n_args,
               SCM *retval)
{
  CallData data = { lambda, args, n_args, FALSE };
  SCM rv;

  rv = scm_c_catch (SCM_BOOL_T,
                    game_scm_call_lambda, &data,
                    game_scm_catch_handler, &data,
                    game_scm_pre_unwind_handler, &data);
  if (data.exception)
    return FALSE;

  if (retval)
    *retval = rv;

  return TRUE;
}

static gboolean
game_scm_call_by_name (const char *name,
                       SCM *args,
                       gsize n_args,
                       SCM *retval)
{
  SCM lambda;

  lambda = scm_c_eval_string (name);

  if (!game_scm_call (lambda, args, n_args, retval))
    return FALSE;

  scm_remember_upto_here_1 (lambda);
  return TRUE;
}

static SCM
c2scm_card (Card card)
{
  return scm_cons (scm_from_uint (CARD_GET_RANK (card)),
                   scm_cons (scm_from_uint (CARD_GET_SUIT (card)),
                             scm_cons (SCM_BOOL (!CARD_GET_FACE_DOWN (card)),
                                       SCM_EOL)));
}

static void
scm2c_card (SCM card_data,
            Card *card)
{
  guint rank, suit, face_down;

  card->value = 0;

  rank = scm_to_int (SCM_CAR (card_data));
  suit = scm_to_int (SCM_CADR (card_data));
  face_down = !(scm_is_true (SCM_CADDR (card_data)));

  card->attr.rank = rank;
  card->attr.suit = suit;
  card->attr.face_down = face_down;
}

static SCM
c2scm_deck (guint8 *cards,
            guint n_cards)
{
  SCM scm_cards;
  guint i;

  scm_cards = SCM_EOL;
  for (i = 0; i < n_cards; ++i) {
    scm_cards = scm_cons (c2scm_card (CARD (cards[i])), scm_cards);
  }

  return scm_cards;
}

static void
cscmi_slot_set_cards (ArSlot *slot,
                      SCM cards)
{
  AisleriotGame *game = app_game;
  SCM list_el;
  guint8 *data = NULL;
  guint i, n_cards = 0;

  if (scm_is_true (scm_list_p (cards))) {
    for (list_el = cards; list_el != SCM_EOL; list_el = SCM_CDR (list_el)) {
      ++n_cards;
    }

    data = g_alloca (n_cards);
    i = n_cards;

    for (list_el = cards; list_el != SCM_EOL; list_el = SCM_CDR (list_el)) {
      Card card;

      scm2c_card (SCM_CAR (list_el), &card);
      data[--i] = CARD_UINT (card);
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

static SCM
cscmi_add_slot (SCM slot_data)
{
  AisleriotGame *game = app_game;
  ArSlot *slot;
  gboolean expanded_down = FALSE;
  gboolean expanded_right = FALSE;
  int expansion_depth = 0;
  ArSlotType type = AR_SLOT_UNKNOWN;
  SCM slot_placement, slot_type;

  if (game->state > GAME_BEGIN) {
    return scm_throw (scm_from_locale_symbol ("game-started"),
                      scm_cons (scm_from_locale_string ("Cannot add a new slot after the game has started."),
                                SCM_EOL));
  }

#define EQUALS_SYMBOL(string,object) (scm_is_true (scm_equal_p (scm_from_locale_symbol (string), object)))

  slot_placement = SCM_CADDR (slot_data);
  if (EQUALS_SYMBOL ("expanded", SCM_CAR (slot_placement))) {
    expanded_down = TRUE;
  } else if (EQUALS_SYMBOL ("expanded-right", SCM_CAR (slot_placement))) {
    expanded_right = TRUE;
  } else if (EQUALS_SYMBOL ("partially-expanded", SCM_CAR (slot_placement))) {
    expanded_down = TRUE;
    expansion_depth = scm_to_int (SCM_CADDR (slot_placement));
  } else if (EQUALS_SYMBOL ("partially-expanded-right", SCM_CAR (slot_placement))) {
    expanded_right = TRUE;
    expansion_depth = scm_to_int (SCM_CADDR (slot_placement));
  }

#undef CHECK_EXPANSION

  /* 3rd argument is the slot type (optionally) */
  slot_type = SCM_CDDDR (slot_data);
  if (slot_type != SCM_EOL) {
    if (EQUALS_SYMBOL ("foundation", SCM_CAR (slot_type))) {
      type = AR_SLOT_FOUNDATION;
    } else if (EQUALS_SYMBOL ("reserve", SCM_CAR (slot_type))) {
      type = AR_SLOT_RESERVE;
    } else if (EQUALS_SYMBOL ("stock", SCM_CAR (slot_type))) {
      type = AR_SLOT_STOCK;
    } else if (EQUALS_SYMBOL ("tableau", SCM_CAR (slot_type))) {
      type = AR_SLOT_TABLEAU;
    } else if (EQUALS_SYMBOL ("waste", SCM_CAR (slot_type))) {
      type = AR_SLOT_WASTE;
    }
  }

#undef EQUALS_SYMBOL

#ifdef GNOME_ENABLE_DEBUG
  _AR_DEBUG_IF (AR_DEBUG_SCHEME) {
    static const char *types[] = { "unknown", "foundation", "reserve", "stock", "tableau", "waste" };

    ar_debug_print (AR_DEBUG_SCHEME,
                        "Adding new slot %d type %s\n",
                        scm_to_int (SCM_CAR (slot_data)), types[type]);
  }
#endif /* GNOME_ENABLE_DEBUG */

  /* create and initialize slot */
  slot = g_slice_new0 (ArSlot);

  g_ptr_array_add (game->slots, slot);

  slot->id = scm_to_int (SCM_CAR (slot_data));
  slot->type = type;

  slot->cards = g_byte_array_sized_new (SLOT_CARDS_N_PREALLOC);
  slot->exposed = 0;
  slot->x = scm_to_double (SCM_CAR (SCM_CADR (SCM_CADDR (slot_data))));
  slot->y = scm_to_double (SCM_CADR (SCM_CADR (SCM_CADDR (slot_data))));

  slot->expansion_depth = expansion_depth;

  slot->expansion.dx = 0.0;
  slot->expanded_down = expanded_down != FALSE;
  slot->expanded_right = expanded_right != FALSE;

#ifdef HAVE_CLUTTER
  slot->old_cards = g_byte_array_sized_new (SLOT_CARDS_N_PREALLOC);
#else
  slot->card_images = g_ptr_array_sized_new (SLOT_CARDS_N_PREALLOC);
#endif

  slot->needs_update = TRUE;

  /* this will update the slot length too */
  cscmi_slot_set_cards (slot, SCM_CADR (slot_data));

  return SCM_EOL;
}

/* Scheme functions */
static SCM
scm_gettext (SCM message)
{
  char *input, *output;
  SCM translated;

  if (!scm_is_string (message))
    return message;

  scm_dynwind_begin (0);

  input = scm_to_locale_string (message);
  scm_dynwind_free (input);
  if (!input)
    goto out;

  output = _(input);

  if (input != output) {
    translated = scm_from_locale_string (output);
  } else {
    translated = message;
  }

out:
  scm_dynwind_end ();

  return translated;
}

static SCM
scm_undo_set_sensitive (SCM in_state)
{
  AisleriotGame *game = app_game;
  gboolean state;

  state = scm_is_true (in_state) ? TRUE : FALSE;
  set_game_undoable (game, state);

  return SCM_EOL;
}

static SCM
scm_redo_set_sensitive (SCM in_state)
{
  AisleriotGame *game = app_game;
  gboolean state;

  state = scm_is_true (in_state) ? TRUE : FALSE;
  set_game_redoable (game, state);

  return SCM_EOL;
}

static SCM
scm_dealable_set_sensitive (SCM in_state)
{
  AisleriotGame *game = app_game;
  gboolean state;

  state = scm_is_true (in_state) ? TRUE : FALSE;
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

  scm_dynwind_begin (0);

  str = scm_to_locale_string (message);
  scm_dynwind_free (str);
  if (!str)
    goto out;

  /* FIXMEchpe: this looks bogus; the string is already translated on the scheme side */
  translated = g_strstrip (g_strdup (_(str)));
  g_signal_emit (game, signals[GAME_MESSAGE], 0, translated);

  g_free (translated);

out:
  scm_dynwind_end ();

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
scm_set_slot_x_expansion (SCM scm_slot_id,
                          SCM new_exp_val)
{
  AisleriotGame *game = app_game;
  ArSlot *slot;

  slot = get_slot (game, scm_to_int (scm_slot_id));

  /* We should only set the x expansion for right-expanded slots! */
  g_return_val_if_fail (slot->expanded_right, SCM_EOL);
  /* Cannot set x and y expansion at the same time */
  g_return_val_if_fail (!slot->dy_set, SCM_EOL);

  slot->expansion.dx = scm_to_double (new_exp_val);
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
  ArSlot *slot;

  slot = get_slot (game, scm_to_int (scm_slot_id));

  /* We should only set the y expansion for down-expanded slots! */
  g_return_val_if_fail (slot->expanded_down, SCM_EOL);
  /* Cannot set x and y expansion at the same time */
  g_return_val_if_fail (!slot->dx_set, SCM_EOL);

  slot->expansion.dy = scm_to_double (new_exp_val);
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
  ArSlot *slot;

  slot = get_slot (game, scm_to_int (scm_slot_id));

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
  ArSlot *slot;

  slot = get_slot (game, scm_to_int (scm_slot_id));

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

  return scm_from_int (game->score);
}

static SCM
scm_set_score (SCM new_score)
{
  AisleriotGame *game = app_game;

  set_game_score (game, scm_to_int (new_score));
  return new_score;
}

static SCM
scm_add_to_score (SCM delta)
{
  AisleriotGame *game = app_game;
  int new_score;

  new_score = game->score + scm_to_int (delta);
  set_game_score (game, new_score);
  return scm_from_int (new_score);
}

static SCM
scm_set_timeout (SCM new)
{
  AisleriotGame *game = app_game;

  g_warning ("(set-timeout) unimplemented\n");

  game->timeout = scm_to_int (new);

  return new;
}

static SCM
scm_get_timeout (void)
{
  AisleriotGame *game = app_game;

  g_warning ("(get-timeout) unimplemented\n");

  return scm_from_int (game->timeout);
}

static void
scm_delayed_call_destroy_data (SCM callback)
{
  AisleriotGame *game = app_game;

  scm_gc_unprotect_object (callback);

  game->delayed_call_timeout_id = 0;
}

/* @callback is GC protected during this call! */
static gboolean
scm_execute_delayed_function (SCM callback)
{
  AisleriotGame *game = app_game;

  /* We set game->delayed_call_timeout_id to 0 _before_ calling |callback|,
   * since it might install a new delayed call.
   */
  game->delayed_call_timeout_id = 0;

  if (!game_scm_call (callback, NULL, 0, NULL))
    return FALSE;

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
  /* Let the scheme side of things know about our C functions. */
  scm_c_define_gsubr ("set-feature-word!", 1, 0, 0, scm_set_feature_word);
  scm_c_define_gsubr ("get-feature-word", 0, 0, 0, scm_get_feature_word);
  scm_c_define_gsubr ("set-statusbar-message", 1, 0, 0,
                      scm_set_statusbar_message);
  scm_c_define_gsubr ("reset-surface", 0, 0, 0, scm_reset_surface);
  scm_c_define_gsubr ("add-slot", 1, 0, 0, cscmi_add_slot);
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
  SCM retval;

  if ((game->features & FEATURE_DEALABLE) == 0)
    return;

  if (!game_scm_call (game->dealable_lambda, NULL, 0, &retval))
    return;

  set_game_dealable (game, scm_is_true (retval));
}

static gboolean
cscmi_game_over_lambda (void)
{
  AisleriotGame *game = app_game;
  SCM retval;

  if (!game_scm_call (game->game_over_lambda, NULL, 0, &retval))
    return TRUE;

  return scm_is_true (retval);
}

static gboolean
cscmi_winning_game_lambda (void)
{
  AisleriotGame *game = app_game;
  SCM retval;

  if (!game_scm_call (game->winning_game_lambda, NULL, 0, &retval))
    return FALSE;

  return scm_is_true (retval);
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

  path = ar_runtime_get_file (AR_RUNTIME_GAMES_DIRECTORY, filename);
  if (g_file_test (path, G_FILE_TEST_EXISTS) &&
      g_file_test (path, G_FILE_TEST_IS_REGULAR)) {
    scm_c_primitive_load (path);
    g_free (path);
    return TRUE;
  }

  g_set_error (error, AISLERIOT_GAME_ERROR, GAME_ERROR_GENERIC,
               _("Aisleriot cannot load the file “%s”. "
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

  game->timer = g_timer_new ();

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

  g_timer_destroy (game->timer);

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
      g_value_set_int (value, game->score);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
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
    g_signal_newv (I_("game-type"),
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   (GSignalFlags) (G_SIGNAL_RUN_LAST),
                   NULL,
                   NULL, NULL,
                   g_cclosure_marshal_VOID__VOID,
                   G_TYPE_NONE,
                   0, NULL);

  signals[GAME_CLEARED] =
    g_signal_newv (I_("game-cleared"),
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   (GSignalFlags) (G_SIGNAL_RUN_LAST),
                   NULL,
                   NULL, NULL,
                   g_cclosure_marshal_VOID__VOID,
                   G_TYPE_NONE,
                   0, NULL);

  signals[GAME_NEW] =
    g_signal_newv (I_("game-new"),
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   (GSignalFlags) (G_SIGNAL_RUN_LAST),
                   NULL,
                   NULL, NULL,
                   g_cclosure_marshal_VOID__VOID,
                   G_TYPE_NONE,
                   0, NULL);

  signals[SLOT_CHANGED] =
    g_signal_newv (I_("slot-changed"),
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   (GSignalFlags) (G_SIGNAL_RUN_LAST),
                   NULL,
                   NULL, NULL,
                   g_cclosure_marshal_VOID__POINTER,
                   G_TYPE_NONE,
                   1, ptr_types);

 signals[GAME_MESSAGE] =
    g_signal_newv (I_("message"),
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   (GSignalFlags) (G_SIGNAL_RUN_LAST),
                   NULL,
                   NULL, NULL,
                   g_cclosure_marshal_VOID__STRING,
                   G_TYPE_NONE,
                   1, param_types);

 signals[EXCEPTION] =
    g_signal_newv (I_("exception"),
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
                           G_PARAM_READABLE |
                           G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (gobject_class,
     PROP_CAN_REDO,
     g_param_spec_boolean ("can-redo", NULL, NULL,
                           FALSE,
                           G_PARAM_READABLE |
                           G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (gobject_class,
     PROP_CAN_DEAL,
     g_param_spec_boolean ("can-deal", NULL, NULL,
                           FALSE,
                           G_PARAM_READABLE |
                           G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (gobject_class,
     PROP_GAME_FILE,
     g_param_spec_string ("game-file", NULL, NULL,
                          NULL,
                          G_PARAM_READABLE |
                          G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (gobject_class,
     PROP_SCORE,
     g_param_spec_int ("score", NULL, NULL,
                       G_MININT, G_MAXINT, 0,
                       G_PARAM_READABLE |
                       G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (gobject_class,
     PROP_STATE,
     g_param_spec_uint ("state", NULL, NULL,
                        0, LAST_GAME_STATE, 0, 
                        G_PARAM_READABLE |
                        G_PARAM_STATIC_STRINGS));

  /* Initialise our scheme interfaces */
  cscm_init ();
}

/* public API */

/**
 * ar_slot_get_slot_type:
 * @slot: a #ArSlot
 *
 * Returns: the slot type of @slot
 */
ArSlotType
ar_slot_get_slot_type (ArSlot *slot)
{
  g_return_val_if_fail (slot != NULL, AR_SLOT_UNKNOWN);

  return slot->type;
}

/**
 * ar_slot_get_type_string:
 * @slot: a #ArSlot
 *
 * Returns: a string describing the slot type
 */
const char *
ar_slot_get_type_string (ArSlot *slot)
{
  const char *text = NULL;

  g_return_val_if_fail (slot != NULL, NULL);

  switch (slot->type) {
    case AR_SLOT_UNKNOWN:
      text = NULL;
      break;
    case AR_SLOT_FOUNDATION:
      /* Translators: this is the name of a type of card slot */
      text = C_("slot type", "foundation");
      break;
    case AR_SLOT_RESERVE:
      /* Translators: this is the name of a type of card slot */
      text = C_("slot type", "reserve");
      break;
    case AR_SLOT_STOCK:
      /* Translators: this is the name of a type of card slot */
      text = C_("slot type", "stock");
      break;
    case AR_SLOT_TABLEAU:
      /* Translators: this is the name of a type of card slot */
      text = C_("slot type", "tableau");
      break;
    case AR_SLOT_WASTE:
      /* Translators: this is the name of a type of card slot */
      text = C_("slot type", "waste");
      break;
  }

  return text;
}

/**
 * ar_slot_get_hint_string:
 * @slot: a #ArSlot
 *
 * Returns: a string describing the slot type
 */
char *
ar_slot_get_hint_string (ArSlot *slot,
                         int cardid)
{
  const char *card_name;

  g_return_val_if_fail (slot != NULL, NULL);

  if (slot->type == AR_SLOT_UNKNOWN)
    return NULL;

  if (cardid < 0)
    return g_strdup (ar_slot_get_type_string (slot));

  card_name = ar_card_get_locale_name (CARD (slot->cards->data[cardid]));

  switch (slot->type) {
    case AR_SLOT_FOUNDATION:
      /* Translators: %s is the name of the card; "foundation" is the name of a type of card slot */
      return g_strdup_printf (C_("slot hint", "%s on foundation"), card_name);

    case AR_SLOT_RESERVE:
      /* Translators: %s is the name of the card; "reserve" is the name of a type of card slot */
      return g_strdup_printf (C_("slot hint", "%s on reserve"), card_name);

    case AR_SLOT_STOCK:
      /* Translators: %s is the name of the card; "stock" is the name of a type of card slot */
      return g_strdup_printf (C_("slot hint", "%s on stock"), card_name);

    case AR_SLOT_TABLEAU:
      /* Translators: %s is the name of the card; "tableau" is the name of a type of card slot */
      return g_strdup_printf (C_("slot hint", "%s on tableau"), card_name);

    case AR_SLOT_WASTE:
      /* Translators: %s is the name of the card; "waste" is the name of a type of card slot */
      return g_strdup_printf (C_("slot hint", "%s on waste"), card_name);

    default:
      g_assert_not_reached ();
  }

  return NULL;
}

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
                               ArSlot *slot,
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

  if (paused) {
    g_timer_stop (game->timer);
  } else {
    g_timer_continue (game->timer);
  }
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

  if (!game_scm_call_by_name ("undo", NULL, 0, NULL))
    return;

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
  if (!game_scm_call_by_name ("redo", NULL, 0, NULL))
    return;

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
    SCM retval;

    if (seed) {
      game->seed = *seed;
    } else {
      game->seed = g_random_int ();
    }
    seed = NULL;

    /* FIXMEchpe: We should NOT re-seed here, but just get another random number! */
    g_random_set_seed (game->seed);

    if (!game_scm_call (game->start_game_lambda, NULL, 0, &retval))
      goto out;

    game->width = scm_to_double (SCM_CAR (retval));
    game->height = scm_to_double (SCM_CADR (retval));

    scm_remember_upto_here (retval);

    if (!game_scm_call_by_name ("start-game", NULL, 0, NULL))
      goto out;

  } while (!cscmi_game_over_lambda ());

  update_game_dealable (game);

out:
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
  return ar_filename_to_display_name (game->game_file);
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
  SCM retval;
  SCM args[2];

  args[0] = scm_from_int (slot_id);
  args[1] = c2scm_deck (cards, n_cards);

  if (!game_scm_call (game->button_pressed_lambda, args, 2, &retval))
    return FALSE;

  scm_remember_upto_here_2 (args[0], args[1]);

  return scm_is_true (retval);
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
  SCM retval;
  SCM args[3];

  if ((game->features & FEATURE_DROPPABLE) == 0)
    return FALSE;

  args[0] = scm_from_int (start_slot);
  args[1] = c2scm_deck (cards, n_cards);
  args[2] = scm_from_int (end_slot);
  if (!game_scm_call (game->droppable_lambda, args, 3, &retval))
    return FALSE;

  scm_remember_upto_here (args[0], args[1], args[2]);

  return scm_is_true (retval);
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
  SCM retval;
  SCM args[3];

  args[0] = scm_from_int (start_slot);
  args[1] = c2scm_deck (cards, n_cards);
  args[2] = scm_from_int (end_slot);
  if (!game_scm_call (game->button_released_lambda, args, 3, &retval))
    return FALSE;

  scm_remember_upto_here (args[0], args[1], args[2]);

  return scm_is_true (retval);
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
  SCM retval;
  SCM args[1];

  args[0] = scm_from_int (slot_id);
  if (!game_scm_call (game->button_clicked_lambda, args, 1, &retval))
    return FALSE;

  scm_remember_upto_here_1 (args[0]);

  return scm_is_true (retval);
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
  SCM retval;
  SCM args[1];

  args[0] = scm_from_int (slot_id);
  if (!game_scm_call (game->button_double_clicked_lambda, args, 1, &retval))
    return FALSE;

  scm_remember_upto_here_1 (args[0]);

  return scm_is_true (retval);
}

/**
 * aisleriot_game_get_hint:
 * @game:
 *
 * Gets a hint.
 *
 * Returns: a newly allocated string containing the hint message
 */
char *
aisleriot_game_get_hint (AisleriotGame *game)
{
  SCM hint;
  SCM string1, string2;
  char *message = NULL;
  char *str1, *str2;

  if (!game_scm_call (game->hint_lambda, NULL, 0, &hint))
    return NULL;

  scm_dynwind_begin (0);

  if (scm_is_false (hint)) {
    message = g_strdup (_("This game does not have hint support yet."));
  } else {
    switch (scm_to_int (SCM_CAR (hint))) {

    case 0:
      string1 = SCM_CADR (hint);
      if (!scm_is_string (string1))
        break;

      str1 = scm_to_locale_string (string1);
      scm_dynwind_free (str1);
      if (!str1)
        break;

      message = g_strdup (str1);
      break;

    case 1:
      string1 = SCM_CADR (hint);
      string2 = SCM_CADDR (hint);

      if (!scm_is_string (string1) || !scm_is_string (string2))
        break;

      str1 = scm_to_locale_string (string1);
      scm_dynwind_free (str1);
      if (!str1)
        break;

      str2 = scm_to_locale_string (string2);
      scm_dynwind_free (str2);
      if (!str2)
        break;

      /* Both %s are card names */
      message = g_strdup_printf (_("Move %s onto %s."), str1, str2);
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
      scm_dynwind_free (str1);
      if (!str1)
        break;
      str2 = scm_to_locale_string (string2);
      scm_dynwind_free (str2);
      if (!str2)
        break;

      /* The first %s is a card name, the 2nd %s a sentence fragment.
        * Yes, we know this is bad for i18n.
        */
      message = g_strdup_printf (_("Move %s onto %s."), str1, str2);
      break;

    case 3: /* This is deprecated (due to i18n issues) do not use. */
      g_warning ("This game uses a deprecated hint method (case 3).\n"
                 "Please file a bug at http://bugzilla.gnome.org "
                 "including this message and the name of the game "
                 "you were playing, which is %s.\n",
                 aisleriot_game_get_game_file (game));
      break;

    case 4:
      string1 = SCM_CADR (hint);
      if (!scm_is_string (string1))
        break;

      str1 = scm_to_locale_string (string1);
      scm_dynwind_free (str1);
      if (!str1)
        break;

      message = g_strdup_printf (_("You are searching for a %s."), str1);
      break;

    default:
      message = g_strdup (_("This game is unable to provide a hint."));
      break;
    }
  }

  scm_dynwind_end ();

  return message;
}

/**
 * aisleriot_game_option_free:
 * @option: a #AisleriotGameOption
 *
 * Frees @options.
 */
void
aisleriot_game_option_free (AisleriotGameOption *option)
{
  g_return_if_fail (option != NULL);

  g_free (option->display_name);
  g_slice_free (AisleriotGameOption, option);
}

/**
 * aisleriot_game_get_options:
 * @game:
 *
 * Returns: a newly allocated list containing newly allocated
 * #AisleriotGameOption structs. 
 */
GList *
aisleriot_game_get_options (AisleriotGame *game)
{
  SCM options_list;
  int l, i;
  guint32 bit = 1;
  AisleriotGameOptionType type = AISLERIOT_GAME_OPTION_CHECK;
  GList *options = NULL;

  if (!game_scm_call (game->get_options_lambda, NULL, 0, &options_list))
    return NULL;

  if (scm_is_false (scm_list_p (options_list)))
    return NULL;

  scm_dynwind_begin (0);

  l = scm_to_int (scm_length (options_list));
  bit = 1;
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
      AisleriotGameOption *option;

      entryname = scm_list_ref (entry, scm_from_uint (0));
      if (!scm_is_string (entryname))
        continue; /* Shouldn't happen */

      entrynamestr = scm_to_locale_string (entryname);
      scm_dynwind_free (entrynamestr);
      if (!entrynamestr)
        continue;

      entrystate = scm_is_true (scm_list_ref (entry, scm_from_uint (1)));

      option = g_slice_new (AisleriotGameOption);
      option->display_name = g_strdup (entrynamestr);
      option->type = type;
      option->value = bit;
      option->set = entrystate != FALSE;

      options = g_list_prepend (options, option);

      bit <<= 1;
    } else {
      /* If we encounter an atom, change the mode. What the atom is doesn't
      * really matter. */
      if (type == AISLERIOT_GAME_OPTION_CHECK) {
        type = AISLERIOT_GAME_OPTION_RADIO;
      } else {
        type = AISLERIOT_GAME_OPTION_CHECK;
      }
    }
  }

  scm_dynwind_end ();

  return g_list_reverse (options);
}

/**
 * aisleriot_game_apply_options_lambda:
 * @game:
 * @changed_mask:
 * @changed_value:
 *
 * Applies options.
 *
 * Returns: the new options value
 */
guint32
aisleriot_game_change_options (AisleriotGame *game,
                               guint32 changed_mask,
                               guint32 changed_value)
{
  SCM options_list;
  guint32 bit, value;
  int l, i;

  if (!game_scm_call (game->get_options_lambda, NULL, 0, &options_list))
    return 0;

  if (scm_is_false (scm_list_p (options_list)))
    return 0;

  value = 0;
  bit = 1;
  l = scm_to_int (scm_length (options_list));
  for (i = 0; i < l; i++) {
    SCM entry;

    entry = scm_list_ref (options_list, scm_from_uint (i));
    if (scm_is_false (scm_list_p (entry)))
      continue;

    if (changed_mask & bit)
      scm_list_set_x (entry, scm_from_uint (1), (changed_value & bit) ? SCM_BOOL_T : SCM_BOOL_F);
  
    if (scm_is_true (scm_list_ref (entry, scm_from_uint (1))))
      value |= bit;

    bit <<= 1;
  }

  game_scm_call (game->apply_options_lambda, &options_list, 1, NULL);

  scm_remember_upto_here_1 (options_list);
  return value;
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
  SCM retval;

  if (game_scm_call (game->timeout_lambda, NULL, 0, &retval))
    return FALSE;

  return scm_is_true (retval);
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
  SCM args[2];

  args[0] = scm_from_int (slot_id);
  args[1] = c2scm_deck (cards, n_cards);

  if (!game_scm_call_by_name ("record-move", args, 2, NULL))
    return;

  scm_remember_upto_here_2 (args[0], args[1]);
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
  if (!game_scm_call_by_name ("end-move", NULL, 0, NULL))
    return;
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
  if (!game_scm_call_by_name ("discard-move", NULL, 0, NULL))
    return;
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
  CallData data = { SCM_EOL, NULL, 0, FALSE };

  scm_c_catch (SCM_BOOL_T,
               (scm_t_catch_body) scm_c_eval_string, (void *) "(/ 1 0)",
               game_scm_catch_handler, &data,
               game_scm_pre_unwind_handler, &data);
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
  /* If the game hasn't started yet, start it now */
  aisleriot_game_start (game);

  aisleriot_game_record_move (game, -1, NULL, 0);

  if (!game_scm_call_by_name ("do-deal-next-cards", NULL, 0, NULL))
    return;

  aisleriot_game_end_move (game);
  aisleriot_game_test_end_of_game (game);
}

#ifdef HAVE_CLUTTER

void
aisleriot_game_get_card_offset (ArSlot *slot,
                                guint card_num,
                                gboolean old_cards,
                                gint *xoff, gint *yoff)
{
  gint n_cards, exposed;

  if (old_cards) {
    n_cards = (gint) slot->old_cards->len;
    exposed = (gint) slot->old_exposed;
  } else {
    n_cards = (gint) slot->cards->len;
    exposed = (gint) slot->exposed;
  }

  if (card_num >= n_cards - exposed) {
    gint idx = card_num + exposed - n_cards;
    *xoff = slot->pixeldx * idx;
    *yoff = slot->pixeldy * idx;
  } else {
    *xoff = 0;
    *yoff = 0;
  }
}

void
aisleriot_game_reset_old_cards (ArSlot *slot)
{
  g_byte_array_set_size (slot->old_cards, 0);
  g_byte_array_append (slot->old_cards, slot->cards->data, slot->cards->len);
  slot->old_exposed = slot->exposed;
}

#endif /* HAVE_CLUTTER */
