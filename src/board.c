/*
 * Copyright © 1998, 2003 Jonathan Blandford <jrb@mit.edu>
 * Copyright © 2007, 2008, 2009, 2010 Christian Persch
 *
 * Some code copied from gtk+/gtk/gtkiconview (LGPL2+):
 * Copyright © 2002, 2004  Anders Carlsson <andersca@gnu.org>
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

#include "board.h"

#include <string.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include <clutter/clutter.h>

#include "ar-debug.h"
#include "ar-marshal.h"
#include "ar-sound.h"

#include "conf.h"

#include "game.h"
#include "card.h"
#include "slot-renderer.h"
#include "ar-card-textures-cache.h"
#include "ar-cursor.h"

#define AISLERIOT_BOARD_GET_PRIVATE(board)(G_TYPE_INSTANCE_GET_PRIVATE ((board), AISLERIOT_TYPE_BOARD, AisleriotBoardPrivate))

/* Enable keynav by default */
#ifndef DISABLE_KEYNAV
#define ENABLE_KEYNAV
#endif

/* The limits for how much overlap there is between cards and
 * how much is allowed to slip off the bottom or right.
 */
#define MIN_DELTA (0.05)

/* The minimum size for the playing area. Almost completely arbitrary. */
#define BOARD_MIN_WIDTH 300
#define BOARD_MIN_HEIGHT 200

#define DOUBLE_TO_INT_CEIL(d) ((int) (d + 0.5))

#define I_(string) g_intern_static_string (string)
  
/* FIXMEchpe: file a bug to get an exported function like gtk_accelerator_get_default_mod_mask() for this? */
/* Copied from clutter-binding-pool.c */
#define CLUTTER_DEFAULT_MOD_MASK ((CLUTTER_SHIFT_MASK   | \
                                   CLUTTER_CONTROL_MASK | \
                                   CLUTTER_MOD1_MASK    | \
                                   CLUTTER_SUPER_MASK   | \
                                   CLUTTER_HYPER_MASK   | \
                                   CLUTTER_META_MASK)   | \
                                  CLUTTER_RELEASE_MASK)

#pragma GCC poison GtkWidget
#pragma GCC poison widget

typedef enum {
  STATUS_NONE,
  STATUS_MAYBE_DRAG,
  STATUS_NOT_DRAG,
  STATUS_IS_DRAG,
  STATUS_SHOW,
  LAST_STATUS
} MoveStatus;

#ifdef ENABLE_KEYNAV

#define MOVE_CURSOR_LEFT_RIGHT    'h'
#define MOVE_CURSOR_LEFT_RIGHT_S  "h"
#define MOVE_CURSOR_UP_DOWN       'v'
#define MOVE_CURSOR_UP_DOWN_S     "v"
#define MOVE_CURSOR_PAGES         'p'
#define MOVE_CURSOR_PAGES_S       "p"
#define MOVE_CURSOR_START_END     'e'
#define MOVE_CURSOR_START_END_S   "e"

#define MOVE_LEFT     'l'
#define MOVE_LEFT_S   "l"
#define MOVE_RIGHT    'r'
#define MOVE_RIGHT_S  "r"

#endif /* ENABLE_KEYNAV */

struct _AisleriotBoardPrivate
{
  AisleriotGame *game;

  ArStyle *style;

  ClutterActorBox allocation;

  /* Card theme */
  CardSize card_size;

  /* Cards cache */
  ArCardTexturesCache *textures;

  double width;
  double height;

  /* The size of a slot in pixels. */
  double xslotstep;
  double yslotstep;

  /* How much of the slot the card should take up */
  double card_slot_ratio;

  /* The offset of the cards within the slot. */
  int xoffset, yoffset;

  /* The offset within the window. */
  int xbaseoffset;

  /* Button press */
  int last_click_x;
  int last_click_y;
  guint32 last_click_time;

  /* Moving cards */
  ArSlot *moving_cards_origin_slot;
  int moving_cards_origin_card_id; /* The index of the card that was clicked on in hslot->cards; or -1 if the click wasn't on a card */
  ClutterActor *moving_cards_group;
  GByteArray *moving_cards;

  /* A group to put animated cards above the slots */
  ClutterActor *animation_layer;

  /* The 'reveal card' action's slot and card link */
  ArSlot *show_card_slot;
  int show_card_id;

  /* Click data */
  ArSlot *last_clicked_slot;
  int last_clicked_card_id;

  /* Focus handling */
  ArSlot *focus_slot;
  int focus_card_id; /* -1 for focused empty slot */
  int focus_line_width;
  int focus_padding;
  GdkRectangle focus_rect;

  /* Selection */
  ArSlot *selection_slot;
  int selection_start_card_id;

  /* Highlight */
  ArSlot *highlight_slot;

  /* Array RemovedCards to be dropped in animations */
  GArray *removed_cards;

  /* Idle handler where the slots will be compared for changes to
     trigger animations */
  guint check_animations_handler;

  /* Status message */
  const char *status_message; /* interned */

  /* Bit field */
  guint droppable_supported : 1;
  guint touchscreen_mode : 1;
  guint show_focus : 1; /* whether the focus is drawn */
  guint interior_focus : 1;

  guint click_to_move : 1;

  guint geometry_set : 1;
  guint is_rtl : 1;

  guint last_click_left_click : 1;
  guint click_status : 4; /* enough bits for MoveStatus */

  guint show_selection : 1;
  guint show_highlight : 1;
  guint show_status_messages : 1;

  guint force_geometry_update : 1;
};

typedef struct _RemovedCard RemovedCard;

struct _RemovedCard
{
  Card card;
  gint cardx, cardy;
  gboolean from_drag;
};

G_STATIC_ASSERT (LAST_STATUS < 16 /* 2^4 */);

enum
{
  PROP_0,
  PROP_GAME,
  PROP_STYLE
};

enum
{
  REQUEST_CURSOR,
  ERROR_BELL,
  STATUS_MESSAGE,
  FOCUS,
#ifdef ENABLE_KEYNAV
  ACTIVATE,
  MOVE_CURSOR,
  TOGGLE_SELECTION,
  SELECT_ALL,
  DESELECT_ALL,
#endif /* ENABLE_KEYNAV */
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

static void get_slot_and_card_from_point (AisleriotBoard *board,
                                          int x,
                                          int y,
                                          ArSlot **slot,
                                          int *_cardid);
static void slot_update_card_images      (AisleriotBoard *board,
                                          ArSlot *slot);
static void slot_update_card_images_full (AisleriotBoard *board,
                                          ArSlot *slot,
                                          gint highlight_start_card_id);

static void aisleriot_board_setup_geometry (AisleriotBoard *board);

static void
set_cursor (AisleriotBoard *board,
            ArCursorType cursor)
{
  g_signal_emit (board, signals[REQUEST_CURSOR], 0, (int) cursor);
}

/* If we are over a slot, set the cursor to the given cursor,
 * otherwise use the default cursor. */
static void
set_cursor_by_location (AisleriotBoard *board,
                        int x,
                        int y)
{
  AisleriotBoardPrivate *priv = board->priv;
  ArSlot *selection_slot = priv->selection_slot;
  int selection_start_card_id = priv->selection_start_card_id;
  ArSlot *slot;
  int card_id;
  gboolean drop_valid = FALSE;
  ArCursorType cursor = AR_CURSOR_DEFAULT;

  get_slot_and_card_from_point (board, x, y, &slot, &card_id);

  if (priv->click_to_move &&
      slot != NULL &&
      selection_slot != NULL &&
      slot != selection_slot &&
      selection_start_card_id >= 0) {
    g_return_if_fail (selection_slot->cards->len > selection_start_card_id);

    drop_valid = aisleriot_game_drop_valid (priv->game,
                                            selection_slot->id,
                                            slot->id,
                                            selection_slot->cards->data + selection_start_card_id,
                                            selection_slot->cards->len - selection_start_card_id);
  }
  /* FIXMEchpe: special cursor when _drag_ is possible? */

  if (drop_valid) {
    cursor = AR_CURSOR_DROPPABLE;
  } else if (slot != NULL &&
             card_id >= 0 &&
             !CARD_GET_FACE_DOWN (CARD (slot->cards->data[card_id]))) {
    if (priv->click_status == STATUS_NONE) {
      cursor = AR_CURSOR_OPEN;
    } else {
      cursor = AR_CURSOR_CLOSED;
    }
  }

  set_cursor (board, cursor);
}

/* status message */

static void
set_status_message (AisleriotBoard *board,
                    const char *message)
{
  AisleriotBoardPrivate *priv = board->priv;

  if (g_strcmp0 (priv->status_message, message) == 0)
    return;

  priv->status_message = g_intern_string (message);

  g_signal_emit (board, signals[STATUS_MESSAGE], 0, priv->status_message);
}

/* Slot helpers */

static void
get_slot_and_card_from_point (AisleriotBoard *board,
                              int x,
                              int y,
                              ArSlot **slot,
                              int *_cardid)
{
  AisleriotBoardPrivate *priv = board->priv;
  GPtrArray *slots;
  gboolean got_slot = FALSE;
  int num_cards;
  int i, n_slots;
  int cardid;

  *slot = NULL;
  cardid = -1;

  slots = aisleriot_game_get_slots (priv->game);

  n_slots = slots->len;
  for (i = n_slots - 1; i >= 0; --i) {
    ArSlot *hslot = slots->pdata[i];

    /* if point is within our rectangle */
    if (hslot->rect.x <= x && x <= hslot->rect.x + hslot->rect.width &&
        hslot->rect.y <= y && y <= hslot->rect.y + hslot->rect.height) {
      num_cards = hslot->cards->len;

      if (got_slot == FALSE || num_cards > 0) {
        /* if we support exposing more than one card,
         * find the exact card  */

        gint depth = 1;

        if (hslot->pixeldx > 0)
          depth += (x - hslot->rect.x) / hslot->pixeldx;
        else if (hslot->pixeldx < 0)
          depth += (hslot->rect.x + hslot->rect.width - x) / -hslot->pixeldx;
        else if (hslot->pixeldy > 0)
          depth += (y - hslot->rect.y) / hslot->pixeldy;

        /* account for the last card getting much more display area
         * or no cards */

        if (depth > hslot->exposed)
          depth = hslot->exposed;
        *slot = hslot;

        /* card = #cards in slot + card chosen (indexed in # exposed cards) - # exposed cards */

        cardid = num_cards + depth - hslot->exposed;

        /* this is the topmost slot with a card */
        /* take it and run */
        if (num_cards > 0)
          break;

        got_slot = TRUE;
      }
    }
  }

  *_cardid = cardid > 0 ? cardid - 1 : -1;
}

#ifdef ENABLE_KEYNAV

static gboolean
test_slot_projection_intersects_x (ArSlot *slot,
                                   int x_start,
                                   int x_end)
{
  return slot->rect.x <= x_end &&
         slot->rect.x + slot->rect.width >= x_start;
}

static int
get_slot_index_from_slot (AisleriotBoard *board,
                          ArSlot *slot)
{
  AisleriotBoardPrivate *priv = board->priv;
  GPtrArray *slots;
  guint n_slots;
  int slot_index;

  g_assert (slot != NULL);

  slots = aisleriot_game_get_slots (priv->game);
  n_slots = slots->len;
  g_assert (n_slots > 0);

  for (slot_index = 0; slot_index < n_slots; ++slot_index) {
    if (g_ptr_array_index (slots, slot_index) == slot)
      break;
  }

  g_assert (slot_index < n_slots); /* the slot EXISTS after all */

  return slot_index;
}

#endif /* ENABLE_KEYNAV */

static void
get_rect_by_slot_and_card (AisleriotBoard *board,
                           ArSlot *slot,
                           int card_id,
                           int num_cards,
                           GdkRectangle *rect)
{
  AisleriotBoardPrivate *priv = board->priv;
  guint delta;
  int first_card_id, num;

  g_return_if_fail (slot != NULL && card_id >= -1);

  first_card_id = ((int) slot->cards->len) - ((int) slot->exposed);

  if (card_id >= first_card_id) {
    delta = card_id - first_card_id;
    num = num_cards - 1;

    rect->x = slot->rect.x + delta * slot->pixeldx;
    rect->y = slot->rect.y + delta * slot->pixeldy;
    rect->width = priv->card_size.width + num * slot->pixeldx;
    rect->height = priv->card_size.height + num * slot->pixeldy;
        
    if (priv->is_rtl &&
        slot->expanded_right) {
      rect->x += slot->rect.width - priv->card_size.width;
    }

  } else {
    /* card_id == -1 or no card available, return the slot rect.
     * Its size should be card_size.
     */
    *rect = slot->rect;
  }
}

/* Focus handling */

static void
widen_rect (GdkRectangle *rect,
            int delta)
{
  int x, y;

  x = rect->x - delta;
  y = rect->y - delta;

  rect->x = MAX (x, 0);
  rect->y = MAX (y, 0);
  rect->width = rect->width + 2 * delta;
  rect->height = rect->height + 2 * delta;
}

static void
get_focus_rect (AisleriotBoard *board,
                GdkRectangle *rect)
{
  AisleriotBoardPrivate *priv = board->priv;

  if (!priv->focus_slot)
    return;

  get_rect_by_slot_and_card (board,
                             priv->focus_slot,
                             priv->focus_card_id,
                             1, rect);
  widen_rect (rect, priv->focus_line_width + priv->focus_padding);
}

static void
set_focus (AisleriotBoard *board,
           ArSlot *slot,
           int card_id,
           gboolean show_focus)
{
  AisleriotBoardPrivate *priv = board->priv;
//   GtkWidget *widget = GTK_WIDGET (board);
  int top_card_id;

  /* Sanitise */
  top_card_id = slot ? ((int) slot->cards->len) - 1 : -1;
  card_id = MIN (card_id, top_card_id);

  if (priv->focus_slot == slot &&
      priv->focus_card_id == card_id &&
      priv->show_focus == show_focus)
    return;

  if (priv->focus_slot != NULL) {
#ifdef FIXMEchpe
    if (priv->show_focus &&
        gtk_widget_has_focus (widget)) {
      gdk_window_invalidate_rect (widget->window, &priv->focus_rect, FALSE);
    
      priv->show_focus = FALSE;
    }
#endif

    priv->focus_slot = NULL;
    priv->focus_card_id = -1;
  }

  priv->show_focus = show_focus;

  if (!slot)
    return;

  priv->focus_slot = slot;
  priv->focus_card_id = card_id;

#ifdef FIXMEchpe
  if (show_focus &&
      gtk_widget_has_focus (widget)) {
    get_focus_rect (board, &priv->focus_rect);
    gdk_window_invalidate_rect (widget->window, &priv->focus_rect, FALSE);
  }
#endif
}

/* Selection handling */

static void
set_selection (AisleriotBoard *board,
               ArSlot *slot,
               int card_id,
               gboolean show_selection)
{
  AisleriotBoardPrivate *priv = board->priv;

  if (priv->selection_slot == slot &&
      priv->selection_start_card_id == card_id &&
      priv->show_selection == show_selection)
    return;

  if (priv->selection_slot != NULL) {
    if (priv->show_selection) {
      /* Clear selection card images */
      slot_update_card_images_full (board, priv->selection_slot, G_MAXINT);
    }

    priv->selection_slot = NULL;
    priv->selection_start_card_id = -1;
  }

  priv->show_selection = show_selection;
  priv->selection_slot = slot;
  priv->selection_start_card_id = card_id;
  g_assert (slot != NULL || card_id == -1);

  if (!slot)
    return;

  g_assert (card_id < 0 || card_id < slot->cards->len);

  if (priv->show_selection) {
    slot_update_card_images_full (board, slot, card_id);
  }
}

/* Slot functions */

static void
slot_update_geometry (AisleriotBoard *board,
                      ArSlot *slot)
{
  AisleriotBoardPrivate *priv = board->priv;
//   GtkWidget *widget = GTK_WIDGET (board);
  GdkRectangle old_rect;
  GByteArray *cards;
  int delta, xofs, yofs, pixeldx;
  double card_step;

  if (!priv->geometry_set)
    return;

  cards = slot->cards;
  old_rect = slot->rect;

  card_step = ar_style_get_card_step (priv->style);

  xofs = priv->xoffset;
  yofs = priv->yoffset;

  /* FIXMEchpe: what exactly is the purpose of the following lines? */
  if (slot->expanded_right)
    xofs = yofs;
  if (slot->expanded_down)
    yofs = xofs;

  if (priv->is_rtl) {
    slot->rect.x = priv->xslotstep * (priv->width - slot->x) - priv->card_size.width - xofs + priv->xbaseoffset;
  } else {
    slot->rect.x = priv->xslotstep * slot->x + xofs + priv->xbaseoffset;
  }

  slot->rect.y = priv->yslotstep * slot->y + yofs; /* FIXMEchpe + priv->ybaseoffset; */

  /* We need to make sure the cards fit within the board, even
   * when there are many of them. See bug #171417.
   */
  /* FIXMEchpe: check |slot->exposed| instead of cards->len? */
  pixeldx = 0;
  if (cards->len > 1) {
    double dx = 0, dy = 0;
    double n_cards = cards->len - 1; /* FIXMEchpe: slot->exposed - 1 ? */

    if (slot->expanded_down) {
      double y_from_bottom, max_dy = card_step;
      float allocation_height = priv->allocation.y2 - priv->allocation.y1;

      if (slot->dy_set)
        max_dy = slot->expansion.dy;

      /* Calculate the compressed_dy that will let us fit within the board */
#ifdef FIXMEchpe
      y_from_bottom = ((double) (widget->allocation.height - slot->rect.y)) / ((double) priv->card_size.height);
#else
      y_from_bottom = ((double) (allocation_height - slot->rect.y)) / ((double) priv->card_size.height);
#endif
      dy = (y_from_bottom - (1.0 - ar_style_get_card_overhang (priv->style))) / n_cards;
      dy = CLAMP (dy, MIN_DELTA, max_dy);
    } else if (slot->expanded_right) {
      if (priv->is_rtl) {
        double x_from_left, max_dx = card_step;

        if (slot->dx_set)
          max_dx = slot->expansion.dx;

        x_from_left = ((double) slot->rect.x) / ((double) priv->card_size.width) + 1.0;
        dx = (x_from_left - (1.0 - ar_style_get_card_overhang (priv->style))) / n_cards;
        dx = CLAMP (dx, MIN_DELTA, max_dx);

        slot->pixeldx = DOUBLE_TO_INT_CEIL (- dx * priv->card_size.width);
        pixeldx = -slot->pixeldx;
      } else {
        double x_from_right, max_dx = card_step;
        float allocation_width = priv->allocation.x2 - priv->allocation.x1;

        if (slot->dx_set)
          max_dx = slot->expansion.dx;

#ifdef FIXMEchpe
        x_from_right = ((double) (widget->allocation.width - slot->rect.x)) / ((double) priv->card_size.width);
#else
        x_from_right = ((double) (allocation_width - slot->rect.x)) / ((double) priv->card_size.width);
#endif
        dx = (x_from_right - (1.0 - ar_style_get_card_overhang (priv->style))) / n_cards;
        dx = CLAMP (dx, MIN_DELTA, max_dx);

        pixeldx = slot->pixeldx = DOUBLE_TO_INT_CEIL (dx * priv->card_size.width);        
      }
    }

    slot->pixeldy = DOUBLE_TO_INT_CEIL (dy * priv->card_size.height);
  } else {
    slot->pixeldx = slot->pixeldy = 0;
  }

  slot->exposed = cards->len;
  if (0 < slot->expansion_depth &&
      slot->expansion_depth < slot->exposed) {
    slot->exposed = slot->expansion_depth;
  }

  if ((slot->pixeldx == 0 &&
       slot->pixeldy == 0 &&
       slot->exposed > 1) ||
      (cards->len > 0 &&
       slot->exposed < 1)) {
    slot->exposed = 1;
  }

  delta = slot->exposed > 0 ? slot->exposed - 1 : 0;

  slot->rect.width = priv->card_size.width + delta * pixeldx;
  slot->rect.height = priv->card_size.height + delta * slot->pixeldy;

  if (priv->is_rtl) {
    slot->rect.x -= slot->rect.width - priv->card_size.width;
  }

  if (slot->slot_renderer)
    clutter_actor_set_position (slot->slot_renderer,
                                slot->rect.x, slot->rect.y);

  slot->needs_update = FALSE;
}

static gboolean
check_animations_cb (gpointer user_data)
{
  AisleriotBoard *board = user_data;
  AisleriotBoardPrivate *priv = board->priv;
  GPtrArray *slots;
  int slot_num, i;
  ArSlot *slot;
  GArray *animations = g_array_new (FALSE, FALSE, sizeof (AisleriotAnimStart));

  slots = aisleriot_game_get_slots (priv->game);

  /* Find any cards that have been removed from the top of the
     slots */
  for (slot_num = 0; slot_num < slots->len; slot_num++) {
    slot = slots->pdata[slot_num];

    if (slot->old_cards->len > slot->cards->len) {
      for (i = 0; i < slot->cards->len; i++) {
        Card old_card = CARD (slot->old_cards->data[i]);
        Card new_card = CARD (slot->cards->data[i]);

        if (old_card.attr.suit != new_card.attr.suit
            || old_card.attr.rank != new_card.attr.rank)
          break;
      }

      if (i >= slot->cards->len) {
        for (; i < slot->old_cards->len; i++) {
          RemovedCard removed_card;

          removed_card.card = CARD (slot->old_cards->data[i]);
          aisleriot_game_get_card_offset (slot, i,
                                          TRUE,
                                          &removed_card.cardx,
                                          &removed_card.cardy);
          removed_card.cardx += slot->rect.x;
          removed_card.cardy += slot->rect.y;
          removed_card.from_drag = FALSE;
          g_array_append_val (priv->removed_cards, removed_card);
        }
      }
    }
  }

  for (slot_num = 0; slot_num < slots->len; slot_num++) {
    /* Number of extra cards that aren't visible to include in the
       animation */
    guint n_unexposed_animated_cards = 0;

    slot = slots->pdata[slot_num];

    g_array_set_size (animations, 0);

    /* Check if the top card has been flipped over */
    if (slot->old_cards->len >= slot->cards->len
        && slot->cards->len >= 1
        && !memcmp (slot->old_cards->data, slot->cards->data,
                    slot->cards->len - 1)) {
      Card old_card = CARD (slot->old_cards->data[slot->cards->len - 1]);
      Card new_card = CARD (slot->cards->data[slot->cards->len - 1]);

      if (old_card.attr.suit == new_card.attr.suit
          && old_card.attr.rank == new_card.attr.rank
          && old_card.attr.face_down != new_card.attr.face_down) {
        AisleriotAnimStart anim;

        aisleriot_game_get_card_offset (slot, slot->cards->len - 1,
                                        FALSE,
                                        &anim.cardx,
                                        &anim.cardy);
        anim.cardx += slot->rect.x;
        anim.cardy += slot->rect.y;
        anim.old_card = old_card;
        anim.raise = TRUE;

        g_array_append_val (animations, anim);
      }
      /* Check if any cards have been added from the removed cards
         pile */
    } else if (slot->old_cards->len < slot->cards->len
               && !memcmp (slot->old_cards->data, slot->cards->data,
                           slot->old_cards->len)) {
      for (i = MAX (slot->old_cards->len, slot->cards->len - slot->exposed);
           i < slot->cards->len;
           i++) {
        Card added_card = CARD (slot->cards->data[i]);
        int j;

        for (j = 0; j < priv->removed_cards->len; j++) {
          RemovedCard *removed_card = &g_array_index (priv->removed_cards,
                                                      RemovedCard, j);

          if (added_card.attr.suit == removed_card->card.attr.suit
              && added_card.attr.rank == removed_card->card.attr.rank) {
            AisleriotAnimStart anim;

            anim.cardx = removed_card->cardx;
            anim.cardy = removed_card->cardy;
            anim.old_card = removed_card->card;
            anim.raise = !removed_card->from_drag;

            g_array_append_val (animations, anim);

            g_array_remove_index (priv->removed_cards, j);

            break;
          }
        }
      }

      /* Check if any extra unexposed cards are included in the
         animation. There's no point in drawing these because they
         will be hidden by the exposed cards but we don't want to draw
         them at the slot either. This will for example happen in
         Canfield when the discard pile is flipped over into the draw
         pile */
      if (animations->len > 0 && animations->len == slot->exposed)
        {
          AisleriotAnimStart *anim = &g_array_index (animations,
                                                     AisleriotAnimStart, 0);

          n_unexposed_animated_cards = (slot->cards->len - slot->old_cards->len
                                        - slot->exposed);

          if (n_unexposed_animated_cards > 0)
            {
              /* Set the bottom card of the first animation to be the
                 lowest unexposed card */
              anim->old_card
                = CARD (slot->cards->data[slot->cards->len
                                          - animations->len
                                          - n_unexposed_animated_cards]);
              anim->old_card.attr.face_down = !anim->old_card.attr.face_down;
            }
        }
    }

    aisleriot_slot_renderer_set_animations
      (AISLERIOT_SLOT_RENDERER (slot->slot_renderer),
       animations->len, (const AisleriotAnimStart *) animations->data,
       n_unexposed_animated_cards);

    if (slot->cards->len == 0) {
      clutter_actor_lower_bottom (slot->slot_renderer);
    } else {
      ClutterActor *animation_layer;

      clutter_actor_raise_top (slot->slot_renderer);
      animation_layer = CLUTTER_ACTOR(aisleriot_slot_renderer_get_animation_layer(AISLERIOT_SLOT_RENDERER(slot->slot_renderer)));
      clutter_actor_raise_top (animation_layer);
    }

    /* Set the old cards back to the new cards */
    aisleriot_game_reset_old_cards (slot);
  }

  g_array_set_size (priv->removed_cards, 0);

  g_array_free (animations, TRUE);

  priv->check_animations_handler = 0;

  return FALSE;
}

static void
queue_check_animations (AisleriotBoard *board)
{
  AisleriotBoardPrivate *priv = board->priv;

  if (!ar_style_get_enable_animations (priv->style))
    return;

  /* The animations are checked for in an idle handler so that this
     function can be called whenever the scheme script makes changes
     to the board but it won't actually check until control has been
     given back to the glib main loop */

  if (priv->check_animations_handler == 0)
    /* Check for animations with a high priority to ensure that the
       animations are setup before the stage is repainted. Clutter's
       redraw priority is unfortunately higher than all of the idle
       priorities so we need to use G_PRIORITY_DEFAULT */
    priv->check_animations_handler =
      clutter_threads_add_idle_full (G_PRIORITY_DEFAULT,
                                     check_animations_cb,
                                     board, NULL);
}

static void
slot_update_card_images_full (AisleriotBoard *board,
                              ArSlot *slot,
                              gint highlight_start_card_id)
{
  AisleriotBoardPrivate *priv = board->priv;

  if (!priv->geometry_set)
    return;

  if (slot->slot_renderer == NULL) {
    slot->slot_renderer = aisleriot_slot_renderer_new (priv->style, priv->textures, slot);
    g_object_ref_sink (slot->slot_renderer);

    aisleriot_slot_renderer_set_animation_layer
      (AISLERIOT_SLOT_RENDERER (slot->slot_renderer),
       CLUTTER_CONTAINER (priv->animation_layer));

    clutter_actor_set_position (slot->slot_renderer,
                                slot->rect.x, slot->rect.y);

    clutter_container_add (CLUTTER_CONTAINER (board),
                           slot->slot_renderer, NULL);

    clutter_actor_raise_top (priv->animation_layer);
  }

  aisleriot_slot_renderer_set_animations
    (AISLERIOT_SLOT_RENDERER (slot->slot_renderer), 0, NULL, 0);

  aisleriot_slot_renderer_set_highlight
    (AISLERIOT_SLOT_RENDERER (slot->slot_renderer),
     priv->show_highlight ? highlight_start_card_id : G_MAXINT);
}

static void
slot_update_card_images (AisleriotBoard *board,
                         ArSlot *slot)
{
  AisleriotBoardPrivate *priv = board->priv;
  int highlight_start_card_id = G_MAXINT;

  if (G_UNLIKELY (slot == priv->highlight_slot &&
                  priv->show_highlight)) {
    highlight_start_card_id = slot->cards->len - 1;
  } else if (G_UNLIKELY (slot == priv->selection_slot &&
                         priv->selection_start_card_id >= 0 &&
                         priv->show_selection)) {
    highlight_start_card_id = priv->selection_start_card_id;
  }

  slot_update_card_images_full (board, slot, highlight_start_card_id);
}

/* helper functions */

static void
aisleriot_board_error_bell (AisleriotBoard *board)
{
  g_signal_emit (board, signals[ERROR_BELL], 0);
}

/* Work out new sizes and spacings for the cards. */
static void
aisleriot_board_setup_geometry (AisleriotBoard *board)
{
  AisleriotBoardPrivate *priv = board->priv;
  ClutterActor *actor = CLUTTER_ACTOR (board);
  ArCardTheme *theme;
  GPtrArray *slots;
  guint i, n_slots;
  CardSize card_size;

  if (!CLUTTER_ACTOR_IS_REALIZED (actor))
    return;

  /* Nothing to do yet */
  if (aisleriot_game_get_state (priv->game) <= GAME_LOADED)
    return;

  theme = ar_style_get_card_theme (priv->style);
  if (theme == NULL)
    return;

  g_return_if_fail (priv->width > 0 && priv->height > 0);

  // FIXMEchpe
  priv->xslotstep = ((double) priv->allocation.x2 - priv->allocation.x1) / priv->width;
  priv->yslotstep = ((double) priv->allocation.y2 - priv->allocation.y1) / priv->height;

  ar_card_theme_set_size (theme,
                             priv->xslotstep,
                             priv->yslotstep,
                             priv->card_slot_ratio);

  ar_card_theme_get_size (theme, &card_size);
  priv->card_size = card_size;

  /* If the cards are too far apart, bunch them in the middle. */
  priv->xbaseoffset = 0;
  if (priv->xslotstep > (card_size.width * 3) / 2) {
    priv->xslotstep = (card_size.width * 3) / 2;
    /* FIXMEchpe: if there are expand-right slots, reserve the space for them instead? */
#ifdef FIXMEchpe
    priv->xbaseoffset = (widget->allocation.width - priv->xslotstep * priv->width) / 2;
#else
    priv->xbaseoffset = (priv->allocation.x2 - priv->allocation.x1 - priv->xslotstep * priv->width) / 2;
#endif
  }
  if (priv->yslotstep > (card_size.height * 3) / 2) {
    priv->yslotstep = (card_size.height * 3) / 2;
    /* FIXMEchpe: if there are expand-down slots, reserve the space for them instead?
       priv->ybaseoffset = (widget->allocation.height - priv->yslotstep * priv->height) / 2;
    */
  }

  priv->xoffset = (priv->xslotstep - card_size.width) / 2;
  priv->yoffset = (priv->yslotstep - card_size.height) / 2;

  /* NOTE! Updating the slots checks that geometry is set, so
   * we set it to TRUE already.
   */
  priv->geometry_set = TRUE;

  /* Now recalculate the slot locations. */
  slots = aisleriot_game_get_slots (priv->game);

  n_slots = slots->len;
  for (i = 0; i < n_slots; ++i) {
    ArSlot *slot = slots->pdata[i];

    slot_update_geometry (board, slot);
    slot_update_card_images (board, slot);
  }

  /* Update the focus and selection rects */
  get_focus_rect (board, &priv->focus_rect);
}

static void
drag_begin (AisleriotBoard *board)
{
  AisleriotBoardPrivate *priv = board->priv;
  ArSlot *hslot;
  int delta;
  int x, y;
  int num_moving_cards;
  guint i;
  GByteArray *cards;

  if (!priv->selection_slot ||
      priv->selection_start_card_id < 0) {
    priv->click_status = STATUS_NONE;
    return;
  }

  priv->click_status = STATUS_IS_DRAG;

  hslot = priv->moving_cards_origin_slot = priv->selection_slot;
  priv->moving_cards_origin_card_id = priv->selection_start_card_id;

  num_moving_cards = hslot->cards->len - priv->moving_cards_origin_card_id;

  cards = hslot->cards;

  /* Save game state */
  aisleriot_game_record_move (priv->game, hslot->id,
                              cards->data, cards->len);

  /* Unset the selection and focus. It'll be re-set if the drag is aborted */
  set_selection (board, NULL, -1, FALSE);
  set_focus (board, NULL, -1, FALSE);

  delta = hslot->exposed - num_moving_cards;

  /* (x,y) is the upper left edge of the topmost dragged card */
  x = hslot->rect.x + delta * hslot->pixeldx;
  if (priv->is_rtl &&
      hslot->expanded_right) {
    x += hslot->rect.width - priv->card_size.width;
  }

  priv->last_click_x -= x;
  priv->last_click_y -= y = hslot->rect.y + delta * hslot->pixeldy;;

  g_byte_array_set_size (priv->moving_cards, 0);
  g_byte_array_append (priv->moving_cards,
                       cards->data + priv->moving_cards_origin_card_id,
                       cards->len - priv->moving_cards_origin_card_id);


  priv->moving_cards_group = g_object_ref_sink (clutter_group_new ());
  clutter_actor_set_position (priv->moving_cards_group, x, y);

  /* FIXMEchpe: RTL issue: this doesn't work right when we allow dragging of
   * more than one card from a expand-right slot. (But right now no game .scm
   * does allow that.)
   */
  x = y = 0;

  for (i = 0; i < priv->moving_cards->len; ++i) {
    Card hcard = CARD (priv->moving_cards->data[i]);
    ClutterActor *card_tex;
    RemovedCard removed_card;

    removed_card.cardx = x;
    removed_card.cardy = y;
    removed_card.card = hcard;
    removed_card.from_drag = TRUE;

    g_array_append_val (priv->removed_cards, removed_card);

    card_tex = aisleriot_card_new (priv->textures, hcard, hcard);
    clutter_actor_set_position (card_tex, x, y);
    clutter_container_add (CLUTTER_CONTAINER (priv->moving_cards_group),
                           card_tex, NULL);

    x += hslot->pixeldx;
    y += hslot->pixeldy;
  }

  /* Take the cards off of the stack */
  g_byte_array_set_size (cards, priv->moving_cards_origin_card_id);

  slot_update_geometry (board, hslot);
  slot_update_card_images (board, hslot);
  aisleriot_game_reset_old_cards (hslot);

  clutter_container_add (CLUTTER_CONTAINER (board),
                         priv->moving_cards_group, NULL);

  if (hslot->cards->len == 0) {
    clutter_actor_lower_bottom (hslot->slot_renderer);
  }

  set_cursor (board, AR_CURSOR_CLOSED);
}

static void
drag_end (AisleriotBoard *board,
          gboolean moved)
{
  AisleriotBoardPrivate *priv = board->priv;

  if (priv->moving_cards_group != NULL) {
    clutter_actor_destroy (priv->moving_cards_group);
    g_object_unref (priv->moving_cards_group);
    priv->moving_cards_group = NULL;
  }

  /* FIXMEchpe: check that slot->cards->len == moving_cards_origin_card_id !!! FIXMEchpe what to do if not, abort the game? */
  /* Add the origin cards back to the origin slot */
  if (!moved &&
      priv->moving_cards_origin_slot != NULL &&
      priv->moving_cards->len > 0) {
    ClutterActor *animation_layer;

    aisleriot_game_slot_add_cards (priv->game,
                                   priv->moving_cards_origin_slot,
                                   priv->moving_cards->data,
                                   priv->moving_cards->len);
    clutter_actor_raise_top (priv->moving_cards_origin_slot->slot_renderer);
    animation_layer = CLUTTER_ACTOR(aisleriot_slot_renderer_get_animation_layer(AISLERIOT_SLOT_RENDERER(priv->moving_cards_origin_slot->slot_renderer)));
    clutter_actor_raise_top (animation_layer);
  }

  priv->click_status = STATUS_NONE;
  priv->moving_cards_origin_slot = NULL;
  priv->moving_cards_origin_card_id = -1;
}

static gboolean
cards_are_droppable (AisleriotBoard *board,
                     ArSlot *slot)
{
  AisleriotBoardPrivate *priv = board->priv;

  return slot != NULL &&
         priv->moving_cards_origin_slot &&
         aisleriot_game_drop_valid (priv->game,
                                    priv->moving_cards_origin_slot->id,
                                    slot->id,
                                    priv->moving_cards->data,
                                    priv->moving_cards->len);
}

static ArSlot *
find_drop_target (AisleriotBoard *board,
                  gint x,
                  gint y)
{
  AisleriotBoardPrivate *priv = board->priv;
  ArSlot *new_hslot;
  ArSlot *retval = NULL;
  gint i, new_cardid;
  gint min_distance = G_MAXINT;

  /* Find a target directly under the center of the card. */
  get_slot_and_card_from_point (board,
                                x + priv->card_size.width / 2,
                                y + priv->card_size.height / 2,
                                &new_hslot, &new_cardid);

  if (cards_are_droppable (board, new_hslot))
    return new_hslot;

  /* If that didn't work, look for a target at all 4 corners of the card. */
  for (i = 0; i < 4; i++) {
    get_slot_and_card_from_point (board,
                                  x + priv->card_size.width * (i / 2),
                                  y + priv->card_size.height * (i % 2),
                                  &new_hslot, &new_cardid);

    if (!new_hslot)
      continue;

    /* This skips corners we know are not droppable. */
    if (!priv->droppable_supported || cards_are_droppable (board, new_hslot)) {
      gint dx, dy, distance_squared;

      dx = new_hslot->rect.x + (new_cardid - 1) * new_hslot->pixeldx - x;
      dy = new_hslot->rect.y + (new_cardid - 1) * new_hslot->pixeldy - y;

      distance_squared = dx * dx + dy * dy;

      if (distance_squared <= min_distance) {
	retval = new_hslot;
	min_distance = distance_squared;
      }
    }
  }

  return retval;
}

static void
drop_moving_cards (AisleriotBoard *board,
                   gint x,
                   gint y)
{
  AisleriotBoardPrivate *priv = board->priv;
  ArSlot *hslot;
  gboolean moved = FALSE;
  guint i;

  hslot = find_drop_target (board,
                            x - priv->last_click_x,
                            y - priv->last_click_y);

  /* Reposition the removed cards so that they are relative to the
     cursor position */
  for (i = 0; i < priv->removed_cards->len; i++) {
    RemovedCard *removed_card = &g_array_index (priv->removed_cards,
                                                RemovedCard, i);

    if (removed_card->from_drag) {
      removed_card->cardx += x - priv->last_click_x;
      removed_card->cardy += y - priv->last_click_y;
    }
  }

  if (hslot) {
    moved = aisleriot_game_drop_cards (priv->game,
                                       priv->moving_cards_origin_slot->id,
                                       hslot->id,
                                       priv->moving_cards->data,
                                       priv->moving_cards->len);
  }

  if (moved) {
    aisleriot_game_end_move (priv->game);
    ar_sound_play ("click");
  } else {
    aisleriot_game_discard_move (priv->game);
    ar_sound_play ("slide");
  }

  drag_end (board, moved);

  if (moved)
    aisleriot_game_test_end_of_game (priv->game);
}

static void
highlight_drop_target (AisleriotBoard *board,
                       ArSlot *slot)
{
  AisleriotBoardPrivate *priv = board->priv;
  ArSlot *old_slot = priv->highlight_slot;

  if (slot == old_slot)
    return;

  /* Need to set the highlight slot even when we the cards aren't droppable
   * since that can happen when the game doesn't support FEATURE_DROPPABLE.
   */
  priv->highlight_slot = slot;

  /* Invalidate the old highlight rect */
  if (old_slot != NULL &&
      priv->show_highlight) {
    /* FIXMEchpe only update the topmost card? */
    /* It's ok to call this directly here, since the old highlight_slot cannot
     * have been the same as the current selection_slot!
     */
    slot_update_card_images_full (board, old_slot, G_MAXINT);
  }

  if (!cards_are_droppable (board, slot))
    return;

  if (!priv->show_highlight)
    return;

  /* Prepare the highlight pixbuf/pixmaps */

  /* FIXMEchpe only update the topmost card? */
  /* It's ok to call this directly, since the highlight slot is always
   * different from the selection slot!
   */
  slot_update_card_images_full (board, slot, ((int) slot->cards->len) - 1);
}

static void
reveal_card (AisleriotBoard *board,
             ArSlot *slot,
             int cardid)
{
  AisleriotBoardPrivate *priv = board->priv;
  Card card;
  AisleriotSlotRenderer *renderer;

  if (priv->show_card_slot == slot)
    return;

  if (priv->show_card_slot != NULL) {
    if (priv->show_card_slot->slot_renderer) {
      renderer = AISLERIOT_SLOT_RENDERER (priv->show_card_slot->slot_renderer);
      aisleriot_slot_renderer_set_revealed_card (renderer, -1);
    }
    priv->show_card_slot = NULL;
    priv->show_card_id = -1;
    priv->click_status = STATUS_NONE;
  }

  if (!slot || cardid < 0 || cardid >= slot->cards->len - 1)
    return;

  card = CARD (slot->cards->data[cardid]);
  if (CARD_GET_FACE_DOWN (card))
    return;

  priv->show_card_slot = slot;
  priv->show_card_id = cardid;
  priv->click_status = STATUS_SHOW;

  renderer = AISLERIOT_SLOT_RENDERER (slot->slot_renderer);
  aisleriot_slot_renderer_set_revealed_card (renderer, cardid);
}

static void
clear_state (AisleriotBoard *board)
{
  AisleriotBoardPrivate *priv = board->priv;

  highlight_drop_target (board, NULL);
  drag_end (board, FALSE /* FIXMEchpe ? */);

  reveal_card (board, NULL, -1);

  priv->click_status = STATUS_NONE;
  priv->last_clicked_slot = NULL;
  priv->last_clicked_card_id = -1;
}

/* Note: this unsets the selection! */
static gboolean
aisleriot_board_move_selected_cards_to_slot (AisleriotBoard *board,
                                             ArSlot *hslot)
{
  AisleriotBoardPrivate *priv = board->priv;
  ArSlot *selection_slot = priv->selection_slot;
  int selection_start_card_id = priv->selection_start_card_id;
  gboolean moved;
  guint8 *cards;
  guint n_cards;

  if (!selection_slot ||
      priv->selection_start_card_id < 0)
    return FALSE;

  /* NOTE: We cannot use aisleriot_game_drop_valid here since the
   * game may not support the "droppable" feature.
   */

  set_selection (board, NULL, -1, FALSE);

  priv->click_status = STATUS_NONE;

  aisleriot_game_record_move (priv->game,
                              selection_slot->id,
                              selection_slot->cards->data,
                              selection_slot->cards->len);

  /* Store the cards, since the move could alter slot->cards! */
  g_assert (selection_slot->cards->len >= selection_start_card_id);
  n_cards = selection_slot->cards->len - selection_start_card_id;

  cards = g_alloca (n_cards);
  memcpy (cards,
          selection_slot->cards->data + selection_start_card_id,
          n_cards);

  /* Now take the cards off of the origin slot. We'll update the slot geometry later */
  g_byte_array_set_size (selection_slot->cards, selection_start_card_id);
  selection_slot->needs_update = TRUE;

  moved = aisleriot_game_drop_cards (priv->game,
                                      selection_slot->id,
                                      hslot->id,
                                      cards,
                                      n_cards);
  if (moved) {
    aisleriot_game_end_move (priv->game);

    ar_sound_play ("click");

    if (selection_slot->needs_update)
      g_signal_emit_by_name (priv->game, "slot-changed", selection_slot); /* FIXMEchpe! */

    aisleriot_game_test_end_of_game (priv->game);
  } else {
    /* Not moved; discard the move add the cards back to the origin slot */
    aisleriot_game_discard_move (priv->game);
    aisleriot_game_slot_add_cards (priv->game, selection_slot, cards, n_cards);
  }

  return moved;
}

/* Keynav */

#ifdef ENABLE_KEYNAV

/** Keynav specification:
 *
 * Focus state consists of the slot that has the focus, and the card
 * in that slot of the card that has the focus; if the slot has no cards
 * the focus is on the empty slot itself.
 *
 * Without modifiers, for focus movement:
 * Left (Right): For right-extended slots, moves the focus to the card
 *   under (over) the currently focused card on the same slot.
 *   If the focused card is already the bottommost (topmost)
 *   card of the slot, or the slot is not right-extended, moves the focus
 *   to the topmost (bottommost) card on the slot left (right) to the currently
 *   focused slot (wraps around from first to last slot and vv.)
 * Up (Down): For down-extended slots, moves the focus to the card
 *   under (over) the currently focused card on the same slot.
 *   If the focused card is already the bottommost (topmost) card
 *   of the slot, or the slot is not down-extended, moves the focus to
 *   the topmost (bottommost) card of the slot over (under) the currently
 *   focused slot (wraps around vertically)
 * Home (End): For down- or right-extended slots, moves the focus to the
 *   start (end) of the stack of face-up cards on the currently focused slot.
 *   If the focus is already on that card, moves the focus over a stack of
 *   face-down cards into the next stack of face-up cards (e.g. Athena), or
 *   the bottommost (topmost) card on the slot if there are no such cards.
 * PageUp, PageDown: Acts like <control>Up, <control>Down
 * Space: selects the cards from the currently focused card to the topmost
 *   card of the slot, if this is allowed by the game. If the focused card is
 *   the first selected card already, unsets the selection.
 * Return: Performs the button press action on the focused card.
 *   If no action was performed by that, moves the selected card(s) to the top
 *   of the focused slot, if this is allowed by the game.
 *
 * With <control>, for focus movement:
 * Left (Right): Moves the focus to the bottommost (topmost) card on the
 *   slot; if that card is already focused, moves the focus to the
 *   topmost/bottommost card on the slot left/right to the currently
 *   focused slot (wraps around from first to last slot and vv.)
 * Up (Down): Moves the focus to the topmost (bottommost) card of the slot
 *   over (under) the currently focused slot (wraps around vertically)
 * Home (End): moves the focus to the bottommost (topmost) card on the
 *   first (last) slot
 * Return: Performs the double-click action on the focused card
 *
 * With <shift>: extends the selection; focus movement itself occurs
 *   like for the same key without modifiers.
 * Left (Right): for right-extended slots, extends (shrinks) the selection
 *   by one card, if this is allowed by the game
 * Up (Down): for down-extended slots, extends (shrinks) the selection
 *   by one card, if this is allowed by the game
 * Home: extends the selection maximally in the focused slot, as allowed
 *   by the game
 * End: shrinks the selection into nonexistence
 * 
 * With <control><shift>: extends selection like with <shift> alone,
 *   and moves focus like with <control> alone
 *
 * Other keyboard shortcuts:
 * <control>A: extends the selection maximally in the focused slot, as allowed
 *   by the game
 * <shift><control>A: unsets the selection
 *
 * Notes:
 * If no slot is currently focused:
 * Right, Up, Down, PgUp, PgDown, Home: moves the focus to the bottommost card on the first
 *   slot
 * Left, End: moves the focus to the topmost card on the last slot
 */

static void
aisleriot_board_add_move_binding (ClutterBindingPool *binding_pool,
                                  GClosure           *closure,
                                  const char         *action,
                                  guint               keyval,
                                  ClutterModifierType modifiers)
{
  clutter_binding_pool_install_closure (binding_pool,
                                        action,
                                        keyval,
                                        modifiers,
                                        closure);

  if (modifiers & CLUTTER_CONTROL_MASK)
    return;

  clutter_binding_pool_install_closure (binding_pool,
                                        action,
                                        keyval,
                                        modifiers | CLUTTER_CONTROL_MASK,
                                        closure);
}

static void
aisleriot_board_add_move_and_select_binding (ClutterBindingPool *binding_pool,
                                             GClosure           *closure,
                                             const char         *action,
                                             guint               keyval,
                                             ClutterModifierType modifiers)
{
  aisleriot_board_add_move_binding (binding_pool, closure, action, keyval, modifiers);
  aisleriot_board_add_move_binding (binding_pool, closure, action, keyval, modifiers | CLUTTER_SHIFT_MASK);
}

static void
aisleriot_board_add_activate_binding (ClutterBindingPool *binding_pool,
                                      GClosure           *closure,
                                      guint               keyval,
                                      ClutterModifierType modifiers)
{
  clutter_binding_pool_install_closure (binding_pool,
                                        I_("activate"),
                                        keyval,
                                        modifiers,
                                        closure);

  if (modifiers & CLUTTER_CONTROL_MASK)
    return;

  clutter_binding_pool_install_closure (binding_pool,
                                        I_("activate"),
                                        keyval,
                                        modifiers | CLUTTER_CONTROL_MASK,
                                        closure);
}

static gboolean
aisleriot_board_move_cursor_in_slot (AisleriotBoard *board,
                                     int count)
{
  AisleriotBoardPrivate *priv = board->priv;
  ArSlot *focus_slot;
  int new_focus_card_id, first_card_id;

  focus_slot = priv->focus_slot;
  first_card_id = ((int) focus_slot->cards->len) - ((int) focus_slot->exposed);
  new_focus_card_id = priv->focus_card_id + count;
  if (new_focus_card_id < first_card_id || new_focus_card_id >= (int) focus_slot->cards->len)
    return FALSE;

  set_focus (board, focus_slot, new_focus_card_id, TRUE);
  return TRUE;
}

static gboolean
aisleriot_board_move_cursor_start_end_in_slot (AisleriotBoard *board,
                                               int count)
{
  AisleriotBoardPrivate *priv = board->priv;
  ArSlot *focus_slot = priv->focus_slot;
  int first_card_id, top_card_id, new_focus_card_id;
  guint8 *cards;

  if (focus_slot->cards->len == 0)
    return FALSE;

  g_assert (priv->focus_card_id >= 0);

  /* Moves the cursor to the first/last card above/below a face-down
   * card, or the start/end of the slot if there are no face-down cards
   * between the currently focused card and the slot start/end.
   * (Jumping over face-down cards and landing on a non-face-down card
   * happens e.g. in Athena.)
   */
  cards = focus_slot->cards->data;
  top_card_id = ((int) focus_slot->cards->len) - 1;
  first_card_id = ((int) focus_slot->cards->len) - ((int) focus_slot->exposed);
  new_focus_card_id = priv->focus_card_id;

  /* Set new_focus_card_id to the index of the last face-down card
    * in the run of face-down cards.
    */
  do {
    new_focus_card_id += count;
  } while (new_focus_card_id >= first_card_id &&
           new_focus_card_id <= top_card_id &&
           CARD_GET_FACE_DOWN (((Card) cards[new_focus_card_id])));

  /* We went one too far */
  new_focus_card_id -= count;

  /* Now get to the start/end of the run of face-up cards */
  do {
    new_focus_card_id += count;
  } while (new_focus_card_id >= first_card_id &&
           new_focus_card_id <= top_card_id &&
           !CARD_GET_FACE_DOWN (((Card) cards[new_focus_card_id])));

  if (new_focus_card_id < first_card_id ||
      new_focus_card_id > top_card_id ||
      CARD_GET_FACE_DOWN (((Card) cards[new_focus_card_id]))) {
    /* We went one too far */
    new_focus_card_id -= count;
  }

  new_focus_card_id = CLAMP (new_focus_card_id, first_card_id, top_card_id);
  set_focus (board, focus_slot, new_focus_card_id, TRUE);

  return TRUE;
}

static gboolean
aisleriot_board_extend_selection_in_slot (AisleriotBoard *board,
                                          int count)
{
  AisleriotBoardPrivate *priv = board->priv;
  ArSlot *focus_slot, *selection_slot;
  int new_selection_start_card_id, first_card_id;

  focus_slot = priv->focus_slot;
  selection_slot = priv->selection_slot;
  first_card_id = ((int) focus_slot->cards->len) - ((int) focus_slot->exposed);

  if (selection_slot == focus_slot) {
    new_selection_start_card_id = priv->selection_start_card_id + count;

    /* Can only extend the selection if the focus is adjacent to the selection */
    if (priv->focus_card_id - 1 > new_selection_start_card_id ||
        priv->focus_card_id + 1 < new_selection_start_card_id)
      return FALSE;
  } else {
    /* No selection yet */
    new_selection_start_card_id = ((int) focus_slot->cards->len) + count;

    /* Must have the topmost card focused */
    if (new_selection_start_card_id != priv->focus_card_id)
      return FALSE;
  }

  if (new_selection_start_card_id < first_card_id)
    return FALSE;

  /* If it's the top card, unselect all */
  if (new_selection_start_card_id >= focus_slot->cards->len) {
    set_selection (board, NULL, -1, FALSE);
    return TRUE;
  }
    
  if (!aisleriot_game_drag_valid (priv->game,
                                  focus_slot->id,
                                  focus_slot->cards->data + new_selection_start_card_id,
                                  focus_slot->cards->len - new_selection_start_card_id))
    return FALSE;

  set_selection (board, focus_slot, new_selection_start_card_id, TRUE);

  /* Try to move the cursor too, but don't beep if that fails */
  aisleriot_board_move_cursor_in_slot (board, count);
  return TRUE;
}

static gboolean
aisleriot_board_extend_selection_in_slot_maximal (AisleriotBoard *board)
{
  AisleriotBoardPrivate *priv = board->priv;
  ArSlot *focus_slot = priv->focus_slot;
  int new_selection_start_card_id, n_selected;

  n_selected = 0;
  new_selection_start_card_id = ((int) focus_slot->cards->len) - 1;
  while (new_selection_start_card_id >= 0) {
    if (!aisleriot_game_drag_valid (priv->game,
                                    focus_slot->id,
                                    focus_slot->cards->data + new_selection_start_card_id,
                                    focus_slot->cards->len - new_selection_start_card_id))
      break;

    ++n_selected;
    --new_selection_start_card_id;
  }

  if (n_selected == 0)
    return FALSE;

  set_selection (board, focus_slot, new_selection_start_card_id + 1, TRUE);
  return TRUE;
}

static gboolean
aisleriot_board_move_cursor_left_right_by_slot (AisleriotBoard *board,
                                                int count,
                                                gboolean wrap)
{
  AisleriotBoardPrivate *priv = board->priv;
  GPtrArray *slots;
  guint n_slots;
  ArSlot *focus_slot, *new_focus_slot;
  int focus_slot_index, new_focus_slot_index;
  int new_focus_slot_topmost_card_id, new_focus_card_id;
  gboolean is_rtl;

  slots = aisleriot_game_get_slots (priv->game);
  if (!slots || slots->len == 0)
    return FALSE;

  n_slots = slots->len;

  focus_slot = priv->focus_slot;
  g_assert (focus_slot != NULL);

  focus_slot_index = get_slot_index_from_slot (board, focus_slot);

  /* Move visually */
  is_rtl = priv->is_rtl;
  if (priv->is_rtl) {
    new_focus_slot_index = focus_slot_index - count;
  } else {
    new_focus_slot_index = focus_slot_index + count;
  }

  /* Wrap-around? */
  if (new_focus_slot_index < 0 ||
      new_focus_slot_index >= n_slots) {
    if (!wrap)
      return FALSE;

#ifdef FIXMEchpe
{
    GtkDirectionType direction;

    if (count > 0) {
      direction = GTK_DIR_RIGHT;
    } else {
      direction = GTK_DIR_LEFT;
    }

    if (!gtk_widget_keynav_failed (widget, direction)) {
       return gtk_widget_child_focus (gtk_widget_get_toplevel (widget), direction);
    }
}
#endif // FIXMEchpe

    if (new_focus_slot_index < 0) {
      new_focus_slot_index = ((int) n_slots) - 1;
    } else {
      new_focus_slot_index = 0;
    }
  }

  g_assert (new_focus_slot_index >= 0 && new_focus_slot_index < n_slots);

  new_focus_slot = slots->pdata[new_focus_slot_index];
  new_focus_slot_topmost_card_id = ((int) new_focus_slot->cards->len) - 1;

  if (new_focus_slot->expanded_right) {
    if ((is_rtl && count < 0) ||
        (!is_rtl && count > 0)) {
      if (new_focus_slot->cards->len > 0) {
        new_focus_card_id = ((int) new_focus_slot->cards->len) - ((int) new_focus_slot->exposed);
      } else {
        new_focus_card_id = -1;
      }
    } else {
      new_focus_card_id = new_focus_slot_topmost_card_id;
    }
  } else {
    /* Just take the topmost card */
    new_focus_card_id = new_focus_slot_topmost_card_id;
  }

  set_focus (board, new_focus_slot, new_focus_card_id, TRUE);
  return TRUE;
}

static gboolean
aisleriot_board_move_cursor_up_down_by_slot (AisleriotBoard *board,
                                             int count)
{
  AisleriotBoardPrivate *priv = board->priv;
  GPtrArray *slots;
  guint n_slots;
  ArSlot *focus_slot, *new_focus_slot;
  int focus_slot_index, new_focus_slot_index;
  int new_focus_slot_topmost_card_id, new_focus_card_id;
  int x_start, x_end;

  slots = aisleriot_game_get_slots (priv->game);
  if (!slots || slots->len == 0)
    return FALSE;

  n_slots = slots->len;

  focus_slot = priv->focus_slot;
  g_assert (focus_slot != NULL);

  x_start = focus_slot->rect.x;
  x_end = x_start + focus_slot->rect.width;

  focus_slot_index = get_slot_index_from_slot (board, focus_slot);

  new_focus_slot_index = focus_slot_index;
  do {
    new_focus_slot_index += count;
  } while (new_focus_slot_index >= 0 &&
           new_focus_slot_index < n_slots &&
           !test_slot_projection_intersects_x (slots->pdata[new_focus_slot_index], x_start, x_end));

  if (new_focus_slot_index < 0 || new_focus_slot_index == n_slots) {
#ifdef FIXMEchpe
    GtkWidget *widget = GTK_WIDGET (board);
    GtkDirectionType direction;

    if (count > 0) {
      direction = GTK_DIR_DOWN;
    } else {
      direction = GTK_DIR_UP;
    }

    if (!gtk_widget_keynav_failed (widget, direction)) {
       return gtk_widget_child_focus (gtk_widget_get_toplevel (widget), direction);
    }
#endif // FIXMEchpe

    /* Wrap around */
    if (count > 0) {
      new_focus_slot_index = -1;
    } else {
      new_focus_slot_index = n_slots;
    }

    do {
      new_focus_slot_index += count;
    } while (new_focus_slot_index != focus_slot_index &&
             !test_slot_projection_intersects_x (slots->pdata[new_focus_slot_index], x_start, x_end));
  }

  g_assert (new_focus_slot_index >= 0 && new_focus_slot_index < n_slots);

  new_focus_slot = slots->pdata[new_focus_slot_index];
  new_focus_slot_topmost_card_id = ((int) new_focus_slot->cards->len) - 1;

  if (new_focus_slot->expanded_down) {
    if (count > 0) {
      if (new_focus_slot->cards->len > 0) {
        new_focus_card_id = ((int) new_focus_slot->cards->len) - ((int) new_focus_slot->exposed);
      } else {
        new_focus_card_id = -1;
      }
    } else {
      new_focus_card_id = new_focus_slot_topmost_card_id;
    }
  } else {
    /* Just take the topmost card */
    new_focus_card_id = new_focus_slot_topmost_card_id;
  }

  set_focus (board, new_focus_slot, new_focus_card_id, TRUE);
  return TRUE;
}

static gboolean
aisleriot_board_move_cursor_start_end_by_slot (AisleriotBoard *board,
                                               int count)
{
  AisleriotBoardPrivate *priv = board->priv;
  GPtrArray *slots;
  ArSlot *new_focus_slot;
  int new_focus_card_id;

  slots = aisleriot_game_get_slots (priv->game);
  if (!slots || slots->len == 0)
    return FALSE;

  if (count > 0) {
    new_focus_slot = (ArSlot *) slots->pdata[slots->len - 1];
    new_focus_card_id = ((int) new_focus_slot->cards->len) - 1;
  } else {
    new_focus_slot = (ArSlot *) slots->pdata[0];
    if (new_focus_slot->cards->len > 0) {
      new_focus_card_id = ((int) new_focus_slot->cards->len) - ((int) new_focus_slot->exposed);
    } else {
      new_focus_card_id = -1;
    }
  }

  g_assert (new_focus_slot != NULL);
  g_assert (new_focus_card_id >= -1);

  set_focus (board, new_focus_slot, new_focus_card_id, TRUE);
  return TRUE;
}

static gboolean
aisleriot_board_move_cursor_left_right (AisleriotBoard *board,
                                        int count,
                                        gboolean is_control)
{
  AisleriotBoardPrivate *priv = board->priv;

  /* First try in-slot focus movement */
  if (!is_control &&
      priv->focus_slot->expanded_right &&
      aisleriot_board_move_cursor_in_slot (board, priv->is_rtl ? -count : count))
    return TRUE;

  /* Cannot move in-slot; move focused slot */
  return aisleriot_board_move_cursor_left_right_by_slot (board, count, TRUE);
}

static gboolean
aisleriot_board_move_cursor_up_down (AisleriotBoard *board,
                                     int count,
                                     gboolean is_control)
{
  AisleriotBoardPrivate *priv = board->priv;

  g_assert (priv->focus_slot != NULL);

  /* First try in-slot focus movement */
  if (!is_control &&
      priv->focus_slot->expanded_down &&
      aisleriot_board_move_cursor_in_slot (board, count))
    return TRUE;

  /* Cannot move in-slot; move focused slot */
  return aisleriot_board_move_cursor_up_down_by_slot (board, count);
}

static gboolean
aisleriot_board_extend_selection_left_right (AisleriotBoard *board,
                                             int count)
{
  AisleriotBoardPrivate *priv = board->priv;

  if (!priv->focus_slot->expanded_right)
    return FALSE;

  return aisleriot_board_extend_selection_in_slot (board, count);
}

static gboolean
aisleriot_board_extend_selection_up_down (AisleriotBoard *board,
                                          int count)
{
  AisleriotBoardPrivate *priv = board->priv;

  if (!priv->focus_slot->expanded_down)
    return FALSE;

  return aisleriot_board_extend_selection_in_slot (board, count);
}

static gboolean
aisleriot_board_extend_selection_start_end (AisleriotBoard *board,
                                            int count)
{
  AisleriotBoardPrivate *priv = board->priv;
  ArSlot *focus_slot = priv->focus_slot;
  int new_focus_card_id;

  if (count > 0) {
    /* Can only shrink the selection if the focus is on the selected slot,
     * and the focused card is on or below the start of the selection.
     */
    if (priv->selection_slot == focus_slot &&
        priv->selection_start_card_id >= priv->focus_card_id) {
      set_selection (board, NULL, -1, FALSE);
      new_focus_card_id = ((int) focus_slot->cards->len);
    } else {
      aisleriot_board_error_bell (board);
      return FALSE;
    }

  } else {
    if (!aisleriot_board_extend_selection_in_slot_maximal (board)) {
      set_selection (board, NULL, -1, FALSE);
      aisleriot_board_error_bell (board);
      return FALSE;
    }

    new_focus_card_id = priv->selection_start_card_id;
  }

  set_focus (board, focus_slot, new_focus_card_id, TRUE);
  return TRUE;
}

#endif /* ENABLE_KEYNAV */

/* Game state handling */

static void
game_type_changed_cb (AisleriotGame *game,
                      AisleriotBoard *board)
{
  AisleriotBoardPrivate *priv = board->priv;
  guint features;

  features = aisleriot_game_get_features (game);

  priv->droppable_supported = ((features & FEATURE_DROPPABLE) != 0);
  priv->show_highlight = priv->droppable_supported;
}

static void
game_cleared_cb (AisleriotGame *game,
                 AisleriotBoard *board)
{
  AisleriotBoardPrivate *priv = board->priv;

  priv->geometry_set = FALSE;

  /* So we don't re-add the cards to the now-dead slot */
  priv->highlight_slot = NULL;
  priv->last_clicked_slot = NULL;
  priv->moving_cards_origin_slot = NULL;
  priv->selection_slot = NULL;
  priv->show_card_slot = NULL;

  clear_state (board);
}

static void
game_new_cb (AisleriotGame *game,
             AisleriotBoard *board)
{
  AisleriotBoardPrivate *priv = board->priv;

  clear_state (board);

  set_focus (board, NULL, -1, FALSE);
  set_selection (board, NULL, -1, FALSE);

  aisleriot_game_get_geometry (game, &priv->width, &priv->height);

  aisleriot_board_setup_geometry (board);

#if 0
  g_print ("{ %.3f , %.3f /* %s */ },\n",
           priv->width, priv->height,
           aisleriot_game_get_game_module (priv->game));
#endif

  /* Check for animations so that the differences will be reset */
  queue_check_animations (board);
}

static void
slot_changed_cb (AisleriotGame *game,
                 ArSlot *slot,
                 AisleriotBoard *board)
{
  AisleriotBoardPrivate *priv = board->priv;

  slot_update_geometry (board, slot);
  slot_update_card_images (board, slot);

  if (slot == priv->moving_cards_origin_slot) {
    /* PANIC! */
    /* FIXMEchpe */
  }
  if (slot == priv->selection_slot) {
    set_selection (board, NULL, -1, FALSE);

    /* If this slot changes while we're in a click cycle, abort the action.
     * That prevents a problem where the cards that were selected and
     * about to be dragged have vanished because of autoplay; c.f. bug #449767.
     * Note: we don't use clear_state() here since we only want to disable
     * the highlight and revealed card if that particular slot changed, see
     * the code above. And we don't clear last_clicked_slot/card_id either, so
     * a double-click will still work.
     */
    priv->click_status = STATUS_NONE;
  }
  if (slot == priv->focus_slot) {
    /* Try to keep the focus intact. If the focused card isn't there
     * anymore, this will set the focus to the topmost card of there
     * same slot, or the slot itself if there are no cards on it.
     * If the slot was empty but now isn't, we set the focus to the
     * topmost card.
     */
    if (priv->focus_card_id < 0) {
      set_focus (board, slot, ((int) slot->cards->len) - 1, priv->show_focus);
    } else {
      set_focus (board, slot, priv->focus_card_id, priv->show_focus);
    }
  }
  if (slot == priv->highlight_slot) {
    highlight_drop_target (board, NULL);
  }

  queue_check_animations (board);
}

/* Style handling */

static void
aisleriot_board_sync_style (ArStyle *style,
                            GParamSpec *pspec,
                            AisleriotBoard *board)
{
  AisleriotBoardPrivate *priv = board->priv;
  const char *pspec_name;
  gboolean update_geometry = FALSE, redraw_focus = FALSE;

  g_assert (style == priv->style);

  if (pspec != NULL) {
    pspec_name = pspec->name;
  } else {
    pspec_name = NULL;
  }

  if (pspec_name == NULL || pspec_name == I_(AR_STYLE_PROP_CARD_THEME)) {
    ArCardTheme *theme;

    theme = ar_style_get_card_theme (style);
    if (theme != NULL) {
      ar_card_textures_cache_set_theme (priv->textures, theme);

      priv->geometry_set = FALSE;

      update_geometry |= TRUE;
    }
  }

  if (pspec_name == NULL || pspec_name == I_(AR_STYLE_PROP_CARD_SLOT_RATIO)) {
    double card_slot_ratio;

    card_slot_ratio = ar_style_get_card_slot_ratio (style);

    update_geometry |= (card_slot_ratio != priv->card_slot_ratio);

    priv->card_slot_ratio = card_slot_ratio;
  }

  if (pspec_name == NULL || pspec_name == I_(AR_STYLE_PROP_CARD_OVERHANG)) {
    update_geometry |= TRUE;
  }

  if (pspec_name == NULL || pspec_name == I_(AR_STYLE_PROP_CARD_STEP)) {
    update_geometry |= TRUE;
  }

  if (pspec_name == NULL || pspec_name == I_(AR_STYLE_PROP_INTERIOR_FOCUS)) {
    gboolean interior_focus;

    interior_focus = ar_style_get_interior_focus (style);

    redraw_focus = (interior_focus != priv->interior_focus);

    priv->interior_focus = interior_focus;
  }

  if (pspec_name == NULL || pspec_name == I_(AR_STYLE_PROP_FOCUS_LINE_WIDTH)) {
    int focus_line_width;

    focus_line_width = ar_style_get_focus_line_width (style);

    redraw_focus = (focus_line_width != priv->focus_line_width);

    priv->focus_line_width = focus_line_width;
  }

  if (pspec_name == NULL || pspec_name == I_(AR_STYLE_PROP_FOCUS_PADDING)) {
    int focus_padding;

    focus_padding = ar_style_get_focus_padding (style);

    redraw_focus = (focus_padding != priv->focus_padding);

    priv->focus_padding = focus_padding;
  }

  if (pspec_name == NULL || pspec_name == I_(AR_STYLE_PROP_RTL)) {
    gboolean is_rtl;

    is_rtl = ar_style_get_rtl (style);

    update_geometry |= (is_rtl != priv->is_rtl);

    priv->is_rtl = is_rtl;

    /* FIXMEchpe: necessary? */
    priv->force_geometry_update = TRUE;
  }

  if (pspec_name == NULL || pspec_name == I_(AR_STYLE_PROP_ENABLE_ANIMATIONS)) {
    /* FIXMEchpe: abort animations-in-progress if the setting is now OFF */
  }

  if (pspec_name == NULL || pspec_name == I_(AR_STYLE_PROP_CLICK_TO_MOVE)) {
    gboolean click_to_move;

    click_to_move = ar_style_get_click_to_move (style);
    if (click_to_move != priv->click_to_move) {
      /* Clear the selection. Do this before setting the new value,
      * since otherwise selection won't get cleared correctly.
      */
      set_selection (board, NULL, -1, FALSE);

      priv->click_to_move = click_to_move;

      /* FIXMEchpe: we used to queue a redraw here. WHY?? Check that it's safe not to. */
    }
  }

  if (pspec_name == NULL || pspec_name == I_(AR_STYLE_PROP_SHOW_STATUS_MESSAGES)) {
    gboolean show_status_messages;

    show_status_messages = ar_style_get_show_status_messages (priv->style);

    if (show_status_messages != priv->show_status_messages) {
      priv->show_status_messages = show_status_messages;

      if (!show_status_messages) {
        /* Clear message */
        set_status_message (board, NULL);
      }
    }
  }

  /* FIXMEchpe: queue a relayout instead? */
  if (update_geometry) {
    aisleriot_board_setup_geometry (board);
  }

  if (redraw_focus) {
    /* FIXMEchpe: do redraw the focus! */
  }
}

/* Class implementation */

G_DEFINE_TYPE (AisleriotBoard, aisleriot_board, CLUTTER_TYPE_GROUP);

/* AisleriotBoardClass methods */

#ifdef ENABLE_KEYNAV

static void
aisleriot_board_activate (AisleriotBoard *board,
                          const char *action,
                          guint keyval,
                          ClutterModifierType modifiers)
{
  AisleriotBoardPrivate *priv = board->priv;  ArSlot *focus_slot = priv->focus_slot;
  ArSlot *selection_slot = priv->selection_slot;
  int selection_start_card_id = priv->selection_start_card_id;

#ifdef FIXMEchpe
  if (!gtk_widget_has_focus (widget))
    return;
#endif

  ar_debug_print (AR_DEBUG_GAME_KEYNAV,
                      "board ::activate keyval %x modifiers %x\n",
                      keyval, modifiers);

  if (!focus_slot) {
    aisleriot_board_error_bell (board);
    return;
  }

  /* Focus not shown? Show it, and do nothing else */
  if (!priv->show_focus) {
    set_focus (board, focus_slot, priv->focus_card_id, TRUE);
    return;
  }

  /* Control-Activate is double-click */
  if (modifiers & CLUTTER_CONTROL_MASK) {
    aisleriot_game_record_move (priv->game, -1, NULL, 0);
    if (aisleriot_game_button_double_clicked_lambda (priv->game, focus_slot->id)) {
      aisleriot_game_end_move (priv->game);
    } else {
      aisleriot_game_discard_move (priv->game);
      aisleriot_board_error_bell (board);
    }

    aisleriot_game_test_end_of_game (priv->game);

    return;
  }

  /* Try single click action */
  aisleriot_game_record_move (priv->game, -1, NULL, 0);

  if (aisleriot_game_button_clicked_lambda (priv->game, focus_slot->id)) {
    aisleriot_game_end_move (priv->game);
    ar_sound_play ("click");
    aisleriot_game_test_end_of_game (priv->game);

    return;
  }

  aisleriot_game_discard_move (priv->game);

  /* If we have a selection, and the topmost card of a slot is focused,
   * try to move the selected cards to the focused slot.
   * Note that this needs to be tested even if selection_slot == focus_slot !
   *
   * NOTE: We cannot use aisleriot_game_drop_valid here since the
   * game may not support the "droppable" feature.
   */
  if (selection_slot != NULL &&
      selection_start_card_id >= 0 &&
      priv->focus_card_id == ((int) focus_slot->cards->len) - 1) {
    if (aisleriot_board_move_selected_cards_to_slot (board, focus_slot)) {
      /* Select the new topmost card */
      set_focus (board, focus_slot, ((int) focus_slot->cards->len - 1), TRUE);

      return;
    }

    /* Trying to move the cards has unset the selection; re-select them */
    set_selection (board, selection_slot, selection_start_card_id, TRUE);
  }

  aisleriot_board_error_bell (board);
}

static gboolean
aisleriot_board_move_cursor (AisleriotBoard *board,
                             const char *action,
                             guint keyval,
                             ClutterModifierType modifiers)
{
  AisleriotBoardPrivate *priv = board->priv;
  gboolean is_control, is_shift, moved = FALSE;
  char step;
  int count;

#ifdef FIXMEchpe
  if (!gtk_widget_has_focus (widget))
    return FALSE;
#endif

  step = action[0];
  count = (action[1] == MOVE_LEFT ? -1 : 1);

  ar_debug_print (AR_DEBUG_GAME_KEYNAV,
                      "board ::move-cursor keyval %x modifiers %x step '%c' count %d\n",
                      keyval, modifiers,
                      step, count);

  /* No focus? Set focus to the first/last slot */
  /* This will always return TRUE, no need for keynav-failed handling */
  if (!priv->focus_slot) {
    switch (step) {
      case MOVE_CURSOR_UP_DOWN:
      case MOVE_CURSOR_PAGES:
        /* Focus the first slot */
        return aisleriot_board_move_cursor_start_end_by_slot (board, -1);
      case MOVE_CURSOR_LEFT_RIGHT:
        /* Move as if we'd been on the last/first slot */
        if (!priv->is_rtl) {
          return aisleriot_board_move_cursor_start_end_by_slot (board, -count);
        }
        /* fall-through */
      default:
        return aisleriot_board_move_cursor_start_end_by_slot (board, count);
    }
  }

  g_assert (priv->focus_slot != NULL);

  is_shift = (modifiers & CLUTTER_SHIFT_MASK) != 0;
  is_control = (modifiers & CLUTTER_CONTROL_MASK) != 0;

  switch (step) {
    case MOVE_CURSOR_LEFT_RIGHT:
      if (is_shift) {
        moved = aisleriot_board_extend_selection_left_right (board, count);
      } else {
        moved = aisleriot_board_move_cursor_left_right (board, count, is_control);
      }
      break;
    case MOVE_CURSOR_UP_DOWN:
      if (is_shift) {
        moved = aisleriot_board_extend_selection_up_down (board, count);
      } else {
        moved = aisleriot_board_move_cursor_up_down (board, count, is_control);
      }
      break;
    case MOVE_CURSOR_PAGES:
      if (!is_shift) {
        moved = aisleriot_board_move_cursor_up_down (board, count, TRUE);
      }
      break;
    case MOVE_CURSOR_START_END:
      if (is_shift) {
        moved = aisleriot_board_extend_selection_start_end (board, count);
      } else if (is_control) {
        moved = aisleriot_board_move_cursor_start_end_by_slot (board, count);
      } else {
        moved = aisleriot_board_move_cursor_start_end_in_slot (board, count);
      }
      break;
    default:
      g_assert_not_reached ();
  }

  /* Show focus */
  if (!moved &&
      !priv->show_focus) {
    set_focus (board, priv->focus_slot, priv->focus_card_id, TRUE);
  }

  return moved;
}

static void
aisleriot_board_select_all (AisleriotBoard *board,
                            const char *action,
                            guint keyval,
                            ClutterModifierType modifiers)
{
  AisleriotBoardPrivate *priv = board->priv;
  ArSlot *focus_slot = priv->focus_slot;

  ar_debug_print (AR_DEBUG_GAME_KEYNAV,
                      "board ::select-all keyval %x modifiers %x\n",
                      keyval, modifiers);

  if (!focus_slot ||
      focus_slot->cards->len == 0 ||
      !aisleriot_board_extend_selection_in_slot_maximal (board)) {
    set_selection (board, NULL, -1, FALSE);
    aisleriot_board_error_bell (board);
  }
}

static void
aisleriot_board_deselect_all (AisleriotBoard *board,
                              const char *action,
                              guint keyval,
                              ClutterModifierType modifiers)
{
  ar_debug_print (AR_DEBUG_GAME_KEYNAV,
                      "board ::deselect-all keyval %x modifiers %x\n",
                      keyval, modifiers);

  set_selection (board, NULL, -1, FALSE);
}

static void
aisleriot_board_toggle_selection (AisleriotBoard *board,
                                  const char *action,
                                  guint keyval,
                                  ClutterModifierType modifiers)
{
  AisleriotBoardPrivate *priv = board->priv;
  ArSlot *focus_slot;
  int focus_card_id;

  ar_debug_print (AR_DEBUG_GAME_KEYNAV,
                      "board ::toggle-selection keyval %x modifiers %x\n",
                      keyval, modifiers);

  focus_slot = priv->focus_slot;
  if (!focus_slot)
    return;

  focus_card_id = priv->focus_card_id;

  /* Focus not shown? Show it, and proceed */
  if (!priv->show_focus) {
    set_focus (board, focus_slot, focus_card_id, TRUE);
  }

  if (focus_card_id < 0) {
    aisleriot_board_error_bell (board);
    return;
  }

  /* If the selection isn't currently showing, don't truncate it.
   * Otherwise we get unexpected results when clicking on some cards
   * (which selects them but doesn't show the selection) and then press
   * Space or Shift-Up/Down etc.
   */
  if (priv->selection_slot == focus_slot &&
      priv->selection_start_card_id == focus_card_id &&
      priv->show_selection) {
    set_selection (board, NULL, -1, FALSE);
    return;
  }

  if (!aisleriot_game_drag_valid (priv->game,
                                  focus_slot->id,
                                  focus_slot->cards->data + focus_card_id,
                                  focus_slot->cards->len - focus_card_id)) {
    aisleriot_board_error_bell (board);
    return;
  }

  set_selection (board, focus_slot, focus_card_id, TRUE);
}

#endif /* ENABLE_KEYNAV */


/* ClutterActorClass impl */

#if 0
static void
aisleriot_board_realize (GtkWidget *widget)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
//   AisleriotBoardPrivate *priv = board->priv;

  GTK_WIDGET_CLASS (aisleriot_board_parent_class)->realize (widget);

  aisleriot_board_setup_geometry (board);
}

static void
aisleriot_board_unrealize (GtkWidget *widget)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
  AisleriotBoardPrivate *priv = board->priv;

  priv->geometry_set = FALSE;

  clear_state (board);

  GTK_WIDGET_CLASS (aisleriot_board_parent_class)->unrealize (widget);
}
#endif

static void
aisleriot_board_allocate (ClutterActor *actor,
                          const ClutterActorBox *box,
                          ClutterAllocationFlags flags)
{
  AisleriotBoard *board = AISLERIOT_BOARD (actor);
  AisleriotBoardPrivate *priv = board->priv;
  gboolean is_same;

  is_same = clutter_actor_box_equal (box, &priv->allocation);

  ar_debug_print (AR_DEBUG_GAME_SIZING,
                      "board ::allocate (%f / %f)-(%f / %f) => %f x %f is-same %s force-update %s\n",
                      box->x1, box->y1, box->x2, box->y2,
                      box->x2 - box->x1, box->y2 - box->y1,
                      is_same ? "t" : "f",
                      priv->force_geometry_update ? "t" : "f");

  CLUTTER_ACTOR_CLASS (aisleriot_board_parent_class)->allocate (actor, box, flags);

  priv->allocation = *box;

  if (is_same && !priv->force_geometry_update)
    return;

  priv->force_geometry_update = FALSE;

  /* FIXMEchpe: just queue this instead maybe? */
  aisleriot_board_setup_geometry (board);
}

static void
aisleriot_board_get_preferred_width (ClutterActor *actor,
                                     float for_height,
                                     float *min_width_p,
                                     float *natural_width_p)
{
  ar_debug_print (AR_DEBUG_GAME_SIZING,
                      "board ::get-preferred-width\n");

  *min_width_p = BOARD_MIN_WIDTH;
  *natural_width_p = 3 * BOARD_MIN_WIDTH;
}

static void
aisleriot_board_get_preferred_height (ClutterActor *actor,
                                      float for_width,
                                      float *min_height_p,
                                      float *natural_height_p)
{
  ar_debug_print (AR_DEBUG_GAME_SIZING,
                      "board ::get-preferred-height\n");

  *min_height_p = BOARD_MIN_HEIGHT;
  *natural_height_p = 3 * BOARD_MIN_HEIGHT;
}

#ifdef ENABLE_KEYNAV

static gboolean
aisleriot_board_focus (AisleriotBoard *board,
                       int count)
{
  AisleriotBoardPrivate *priv = board->priv;

  if (!priv->focus_slot) {
    return aisleriot_board_move_cursor_start_end_by_slot (board, -count);
  }

#if 0
  if (aisleriot_board_move_cursor_left_right_by_slot (board, count, FALSE))
    return TRUE;
#endif

  return FALSE;
}

static gboolean
aisleriot_board_key_press (ClutterActor *actor,
                           ClutterKeyEvent *event)
{
  ClutterBindingPool *pool;

  ar_debug_print (AR_DEBUG_GAME_EVENTS,
                      "board ::key-press keyval %x modifiers %x\n",
                      event->keyval, event->modifier_state);

  pool = clutter_binding_pool_get_for_class (CLUTTER_ACTOR_GET_CLASS (actor));
  g_assert (pool != NULL);

  return clutter_binding_pool_activate (pool,
                                        event->keyval,
                                        event->modifier_state,
                                        G_OBJECT (actor));
}

#endif /* ENABLE_KEYNAV */

#ifdef FIXMEchpe
/* The gtkwidget.c focus in/out handlers queue a shallow draw;
 * that's ok for us but maybe we want to optimise this a bit to
 * only do it if we have a focus to draw/erase?
 */
static gboolean
aisleriot_board_focus_in (GtkWidget *widget,
                          GdkEventFocus *event)
{
#ifdef ENABLE_KEYNAV
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
  AisleriotBoardPrivate *priv = board->priv;

  /* Paint focus */
  if (priv->show_focus &&
      priv->focus_slot != NULL) {
    gdk_window_invalidate_rect (widget->window, &priv->focus_rect, FALSE);
  }
#endif /* ENABLE_KEYNAV */

  return FALSE;
}

static gboolean
aisleriot_board_focus_out (GtkWidget *widget,
                           GdkEventFocus *event)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
#ifdef ENABLE_KEYNAV
  AisleriotBoardPrivate *priv = board->priv;
#endif /* ENABLE_KEYNAV */

  clear_state (board);

#ifdef ENABLE_KEYNAV
  /* Hide focus */
  if (priv->show_focus &&
      priv->focus_slot != NULL) {
    gdk_window_invalidate_rect (widget->window, &priv->focus_rect, FALSE);
  }
#endif /* ENABLE_KEYNAV */

  return FALSE;
}
#endif /* FIXMEchpe */

static gboolean
aisleriot_board_button_press (ClutterActor *actor,
                              ClutterButtonEvent *event)
{
  AisleriotBoard *board = AISLERIOT_BOARD (actor);
  AisleriotBoardPrivate *priv = board->priv;
  ArSlot *hslot;
  int cardid;
  guint32 button;
  gboolean drag_valid;
  guint state;
  gboolean is_double_click, show_focus;

  ar_debug_print (AR_DEBUG_GAME_EVENTS,
                      "board ::button-press @(%f / %f) button %d click-count %d modifiers %x\n",
                      event->x, event->y,
                      event->button, event->click_count,
                      event->modifier_state);

  /* NOTE: It's ok to just return instead of chaining up, since the
   * parent classes have no class closure for this event.
   */

  /* FIXMEchpe: check event coordinate handling (float vs int!) */

#ifdef FIXMEchpe
  /* FIXMEchpe: we might be able to use ClutterButtonEvent::click_count for double-click detection! */
  /* ignore the gdk synthetic double/triple click events */
  if (event->type != GDK_BUTTON_PRESS)
    return FALSE;
#endif

  /* Don't do anything if a modifier is pressed */
  /* FIXMEchpe: is there anything like gtk_accelerator_get_default_mod_mask() in clutter? */
  state = event->modifier_state & CLUTTER_DEFAULT_MOD_MASK;
  if (state != 0)
    return FALSE;

  button = event->button;

  /* We're only interested in left, middle and right-clicks */
  if (button < 1 || button > 3)
    return FALSE;

  /* If we already have a click, ignore this new one */
  if (priv->click_status != STATUS_NONE) {
    return FALSE;
  }

  /* If the game hasn't started yet, start it now */
  aisleriot_game_start (priv->game);

  get_slot_and_card_from_point (board, event->x, event->y, &hslot, &cardid);

  is_double_click = button == 2 ||
                    (priv->last_click_left_click &&
                     (event->time - priv->last_click_time <= ar_style_get_double_click_time (priv->style)) &&
                     priv->last_clicked_slot == hslot &&
                     priv->last_clicked_card_id == cardid);

  priv->last_click_x = event->x;
  priv->last_click_y = event->y;
  priv->last_clicked_slot = hslot;
  priv->last_clicked_card_id = cardid;
  priv->last_click_time = event->time;
  priv->last_click_left_click = button == 1;

  if (!hslot) {
    set_focus (board, NULL, -1, FALSE);
    set_selection (board, NULL, -1, FALSE);

    priv->click_status = STATUS_NONE;

    return FALSE;
  }

  set_cursor (board, AR_CURSOR_CLOSED);

  /* First check if it's a right-click: if so, we reveal the card and do nothing else */
  if (button == 3) {
    /* Don't change the selection here! */
    reveal_card (board, hslot, cardid);

    return TRUE;
  }

  /* Clear revealed card */
  reveal_card (board, NULL, -1);

  /* We can't let Gdk do the double-click detection; since the entire playing
   * area is one big widget it can't distinguish between single-clicks on two
   * different cards and a double-click on one card.
   */
  if (is_double_click) {
    ArSlot *clicked_slot = hslot;

    priv->click_status = STATUS_NONE;

    /* Reset this since otherwise 3 clicks will be interpreted as 2 double-clicks */
    priv->last_click_left_click = FALSE;

    aisleriot_game_record_move (priv->game, -1, NULL, 0);
    if (aisleriot_game_button_double_clicked_lambda (priv->game, clicked_slot->id)) {
      aisleriot_game_end_move (priv->game);
    } else {
      aisleriot_game_discard_move (priv->game);
    }

    aisleriot_game_test_end_of_game (priv->game);

    set_cursor (board, AR_CURSOR_OPEN);

    return TRUE;
  }

  /* button == 1 from now on */

  if (priv->selection_slot == NULL)
    goto set_selection;

  /* In click-to-move mode, we need to test whether moving the selected cards
   * to this slot does a move. Note that it is necessary to do this both if
   * the clicked slot is the selection_slot (and the clicked card the topmost
   * card below the selection), and if it's not the selection_slot, since some
   * games depend on this behaviour (e.g. Treize). See bug #565560.
   *
   * Note that aisleriot_board_move_selected_cards_to_slot unsets the selection,
   * so we need to fall through to set_selection if no move was done.
    */
  if (priv->click_to_move &&
      priv->selection_start_card_id >= 0 &&
      (hslot != priv->selection_slot || cardid + 1 == priv->selection_start_card_id)) {

    /* Try to move the selected cards to the clicked slot */
    if (aisleriot_board_move_selected_cards_to_slot (board, hslot))
      return TRUE;

    /* Move failed if this wasn't the selection_slot slot */
    if (hslot != priv->selection_slot) {
      aisleriot_board_error_bell (board);
    }
  }

  if (hslot != priv->selection_slot ||
      cardid != priv->selection_start_card_id)
    goto set_selection;
    
  /* Single click on the selected slot & card, we take that to mean to deselect,
   * but only in click-to-move mode.
   */
  if (priv->click_to_move) {
    set_selection (board, NULL, -1, FALSE);

    /* Reveal the card on left click */
    reveal_card (board, hslot, cardid);

    return TRUE;
  }

set_selection:

  if (cardid >= 0) {
    drag_valid = aisleriot_game_drag_valid (priv->game,
                                            hslot->id,
                                            hslot->cards->data + cardid,
                                            hslot->cards->len - cardid);
  } else {
    drag_valid = FALSE;
  }

  if (drag_valid) {
    set_selection (board, hslot, cardid, priv->click_to_move);
    priv->click_status = priv->click_to_move ? STATUS_NOT_DRAG : STATUS_MAYBE_DRAG;
  } else {
    set_selection (board, NULL, -1, FALSE);
    priv->click_status = STATUS_NOT_DRAG;
  }

  /* If we're already showing focus or just clicked on the
   * card with the (hidden) focus, show the focus on the
   * clicked card.
   */
  show_focus = priv->show_focus ||
               (hslot == priv->focus_slot &&
                cardid == priv->focus_card_id);
  set_focus (board, hslot, cardid, show_focus);

  /* Reveal the card on left click */
  if (priv->click_to_move) {
    reveal_card (board, hslot, cardid);
  }

  return FALSE;
}

static gboolean
aisleriot_board_button_release (ClutterActor *actor,
                                ClutterButtonEvent *event)
{
  AisleriotBoard *board = AISLERIOT_BOARD (actor);
  AisleriotBoardPrivate *priv = board->priv;
  /* guint state; */

  ar_debug_print (AR_DEBUG_GAME_EVENTS,
                      "board ::button-release @(%f / %f) button %d click-count %d modifiers %x\n",
                      event->x, event->y,
                      event->button, event->click_count,
                      event->modifier_state);

  /* FIXMEchpe: check event coordinate handling (float vs int!) */

  /* NOTE: It's ok to just return instead of chaining up, since the
   * parent classes have no class closure for this event.
   */

  /* We just abort any action on button release, even if the button-up
   * is not the one that started the action. This greatly simplifies the code,
   * and is also the right thing to do, anyway.
   */

  /* state = event->state & gtk_accelerator_get_default_mod_mask (); */

  switch (priv->click_status) {
    case STATUS_SHOW:
      reveal_card (board, NULL, -1);
      break;

    case STATUS_IS_DRAG:
      highlight_drop_target (board, NULL);
      drop_moving_cards (board, event->x, event->y);
      break;

    case STATUS_MAYBE_DRAG:
    case STATUS_NOT_DRAG: {
      ArSlot *slot;
      int card_id;

      /* Don't do the action if the mouse moved away from the clicked slot; see bug #329183 */
      get_slot_and_card_from_point (board, event->x, event->y, &slot, &card_id);
      if (!slot || slot != priv->last_clicked_slot)
        break;

      aisleriot_game_record_move (priv->game, -1, NULL, 0);
      if (aisleriot_game_button_clicked_lambda (priv->game, slot->id)) {
        aisleriot_game_end_move (priv->game);
	ar_sound_play_for_event ("click", (GdkEvent *) event);
      } else {
        aisleriot_game_discard_move (priv->game);
      }

      aisleriot_game_test_end_of_game (priv->game);

      break;
    }

    case STATUS_NONE:
      break;
  }

  priv->click_status = STATUS_NONE;

  set_cursor_by_location (board, event->x, event->y);

  return TRUE;
}

static gboolean
aisleriot_board_motion (ClutterActor *actor,
                        ClutterMotionEvent *event)
{
  AisleriotBoard *board = AISLERIOT_BOARD (actor);
  AisleriotBoardPrivate *priv = board->priv;

  ar_debug_print (AR_DEBUG_GAME_EVENTS,
                      "board ::motion @(%f / %f) modifiers %x\n",
                      event->x, event->y,
                      event->modifier_state);

  /* FIXMEchpe: check event coordinate handling (float vs int!) */

  /* NOTE: It's ok to just return instead of chaining up, since the
   * parent classes have no class closure for this event.
   */

  if (priv->show_status_messages) {
    ArSlot *slot = NULL;
    int cardid = -1;

    get_slot_and_card_from_point (board, event->x, event->y, &slot, &cardid);
    if (slot != NULL && ar_slot_get_slot_type (slot) != AR_SLOT_UNKNOWN) {
      char *text;

      text = ar_slot_get_hint_string (slot, cardid);
      set_status_message (board, text);
      g_free (text);
    } else {
      set_status_message (board, NULL);
    }
  }

  if (priv->click_status == STATUS_IS_DRAG) {
    ArSlot *slot;
    int x, y;

    x = event->x - priv->last_click_x;
    y = event->y - priv->last_click_y;

    slot = find_drop_target (board, x, y);
    highlight_drop_target (board, slot);

    clutter_actor_set_position (priv->moving_cards_group, x, y);
    clutter_actor_raise_top (priv->moving_cards_group);

    set_cursor (board, AR_CURSOR_CLOSED);
  } else if (priv->click_status == STATUS_MAYBE_DRAG &&
             ar_style_check_dnd_drag_threshold (priv->style,
                                                priv->last_click_x,
                                                priv->last_click_y,
                                                event->x,
                                                event->y)) {
    drag_begin (board);
  } else {
    set_cursor_by_location (board, event->x, event->y);
  }

  return FALSE;
}

static gboolean
aisleriot_board_enter (ClutterActor *actor,
                       ClutterCrossingEvent *event)
{
  ar_debug_print (AR_DEBUG_GAME_EVENTS,
                      "board ::enter @(%f / %f)\n",
                      event->x, event->y);

  /* NOTE: It's ok to just return instead of chaining up, since the
   * parent classes have no class closure for this event.
   */

  return FALSE;
}

static gboolean
aisleriot_board_leave (ClutterActor *actor,
                       ClutterCrossingEvent *event)
{
  AisleriotBoard *board = AISLERIOT_BOARD (actor);

  ar_debug_print (AR_DEBUG_GAME_EVENTS,
                      "board ::leave @(%f / %f)\n",
                      event->x, event->y);

  /* NOTE: It's ok to just return instead of chaining up, since the
   * parent classes have no class closure for this event.
   */

  set_cursor (board, AR_CURSOR_DEFAULT);

  return FALSE;
}

static void
aisleriot_board_key_focus_in (ClutterActor *actor)
{
  ar_debug_print (AR_DEBUG_GAME_EVENTS,
                      "board ::key-focus-in\n");
}

static void
aisleriot_board_key_focus_out (ClutterActor *actor)
{
  ar_debug_print (AR_DEBUG_GAME_EVENTS,
                      "board ::key-focus-out\n");
}

/* GObjectClass methods */

static void
aisleriot_board_init (AisleriotBoard *board)
{
  ClutterActor *actor = CLUTTER_ACTOR (board);
  AisleriotBoardPrivate *priv;

  priv = board->priv = AISLERIOT_BOARD_GET_PRIVATE (board);

  priv->textures = ar_card_textures_cache_new ();

  memset (&priv->allocation, 0, sizeof (ClutterActorBox));

  /* We want to receive events! */
  clutter_actor_set_reactive (actor, TRUE);

  priv->force_geometry_update = FALSE;

  priv->click_to_move = FALSE;
  priv->show_selection = FALSE;
  priv->show_status_messages = FALSE;

  priv->show_card_id = -1;

  priv->moving_cards = g_byte_array_sized_new (SLOT_CARDS_N_PREALLOC);

  priv->removed_cards = g_array_new (FALSE, FALSE, sizeof (RemovedCard));

  priv->animation_layer = g_object_ref_sink (clutter_group_new ());
  clutter_container_add (CLUTTER_CONTAINER (board),
                         priv->animation_layer, NULL);
}

static void
aisleriot_board_finalize (GObject *object)
{
  AisleriotBoard *board = AISLERIOT_BOARD (object);
  AisleriotBoardPrivate *priv = board->priv;

  g_signal_handlers_disconnect_matched (priv->game,
                                        G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL, NULL, board);
  g_object_unref (priv->game);

  g_array_free (priv->removed_cards, TRUE);

  g_byte_array_free (priv->moving_cards, TRUE);

  if (priv->style != NULL) {
    g_signal_handlers_disconnect_by_func (priv->style,
                                          G_CALLBACK (aisleriot_board_sync_style),
                                          board);

    g_object_unref (priv->style);
  }

#if 0
  screen = gtk_widget_get_settings (widget);
  g_signal_handlers_disconnect_matched (settings, G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL, NULL,
                                        widget);
#endif

  G_OBJECT_CLASS (aisleriot_board_parent_class)->finalize (object);
}

static void
aisleriot_board_dispose (GObject *object)
{
  AisleriotBoard *board = AISLERIOT_BOARD (object);
  AisleriotBoardPrivate *priv = board->priv;

  if (priv->textures) {
    g_object_unref (priv->textures);
    priv->textures = NULL;
  }

  if (priv->animation_layer) {
    g_object_unref (priv->animation_layer);
    priv->animation_layer = NULL;
  }

  if (priv->check_animations_handler) {
    g_source_remove (priv->check_animations_handler);
    priv->check_animations_handler = 0;
  }

  G_OBJECT_CLASS (aisleriot_board_parent_class)->dispose (object);
}

static void
aisleriot_board_set_property (GObject *object,
                              guint prop_id,
                              const GValue *value,
                              GParamSpec *pspec)
{
  AisleriotBoard *board = AISLERIOT_BOARD (object);
  AisleriotBoardPrivate *priv = board->priv;

  switch (prop_id) {
    case PROP_GAME:
      priv->game = AISLERIOT_GAME (g_value_dup_object (value));

      g_signal_connect (priv->game, "game-type",
                        G_CALLBACK (game_type_changed_cb), board);
      g_signal_connect (priv->game, "game-cleared",
                        G_CALLBACK (game_cleared_cb), board);
      g_signal_connect (priv->game, "game-new",
                        G_CALLBACK (game_new_cb), board);
      g_signal_connect (priv->game, "slot-changed",
                        G_CALLBACK (slot_changed_cb), board);

      break;

    case PROP_STYLE:
      priv->style = g_value_dup_object (value);

      aisleriot_board_sync_style (priv->style, NULL, board);
      g_signal_connect (priv->style, "notify",
                        G_CALLBACK (aisleriot_board_sync_style), board);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

static void
aisleriot_board_class_init (AisleriotBoardClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  ClutterActorClass *actor_class = CLUTTER_ACTOR_CLASS (klass);
#ifdef ENABLE_KEYNAV
  ClutterBindingPool *binding_pool;
  GClosure *closure;
#endif

  g_type_class_add_private (gobject_class, sizeof (AisleriotBoardPrivate));

  gobject_class->dispose = aisleriot_board_dispose;
  gobject_class->finalize = aisleriot_board_finalize;
  gobject_class->set_property = aisleriot_board_set_property;

#ifdef FIXMEchpe
  widget_class->realize = aisleriot_board_realize;
  widget_class->unrealize = aisleriot_board_unrealize;
  widget_class->focus_in_event = aisleriot_board_focus_in;
  widget_class->focus_out_event = aisleriot_board_focus_out;
#endif // FIXMEchpe

  actor_class->allocate = aisleriot_board_allocate;
  actor_class->get_preferred_width = aisleriot_board_get_preferred_width;
  actor_class->get_preferred_height = aisleriot_board_get_preferred_height;
#ifdef ENABLE_KEYNAV
  actor_class->key_press_event = aisleriot_board_key_press;
#endif /* ENABLE_KEYNAV */
  actor_class->button_press_event = aisleriot_board_button_press;
  actor_class->button_release_event = aisleriot_board_button_release;
  actor_class->motion_event = aisleriot_board_motion;
  actor_class->enter_event = aisleriot_board_enter;
  actor_class->leave_event = aisleriot_board_leave;
  actor_class->key_focus_in = aisleriot_board_key_focus_in;
  actor_class->key_focus_out = aisleriot_board_key_focus_out;


  signals[REQUEST_CURSOR] =
    g_signal_new (I_("request-cursor"),
                  G_TYPE_FROM_CLASS (gobject_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                  G_STRUCT_OFFSET (AisleriotBoardClass, request_cursor),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__INT,
                  G_TYPE_NONE,
                  1,
                  G_TYPE_INT);

  signals[ERROR_BELL] =
    g_signal_new (I_("error-bell"),
                  G_TYPE_FROM_CLASS (gobject_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                  G_STRUCT_OFFSET (AisleriotBoardClass, error_bell),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE,
                  0);

  signals[STATUS_MESSAGE] =
    g_signal_new (I_("status-message"),
                  G_TYPE_FROM_CLASS (gobject_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                  G_STRUCT_OFFSET (AisleriotBoardClass, status_message),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__STRING,
                  G_TYPE_NONE,
                  1,
                  G_TYPE_STRING | G_SIGNAL_TYPE_STATIC_SCOPE);

  signals[FOCUS] =
    g_signal_new (I_("focus"),
                  G_TYPE_FROM_CLASS (gobject_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                  G_STRUCT_OFFSET (AisleriotBoardClass, focus),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__INT,
                  G_TYPE_BOOLEAN,
                  1,
                  G_TYPE_INT);

#ifdef ENABLE_KEYNAV
  klass->focus = aisleriot_board_focus;
  klass->activate = aisleriot_board_activate;
  klass->move_cursor = aisleriot_board_move_cursor;
  klass->select_all = aisleriot_board_select_all;
  klass->deselect_all = aisleriot_board_deselect_all;
  klass->toggle_selection = aisleriot_board_toggle_selection;

  /* Keybinding signals */
#ifdef FIXMEchpe
  widget_class->activate_signal =
#endif
  signals[ACTIVATE] =
    g_signal_new (I_("activate"),
                  G_TYPE_FROM_CLASS (gobject_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                  G_STRUCT_OFFSET (AisleriotBoardClass, activate),
                  NULL, NULL,
                  ar_marshal_BOOLEAN__STRING_UINT_ENUM,
                  G_TYPE_BOOLEAN,
                  3,
                  G_TYPE_STRING | G_SIGNAL_TYPE_STATIC_SCOPE,
                  G_TYPE_UINT,
                  CLUTTER_TYPE_MODIFIER_TYPE);

  signals[MOVE_CURSOR] =
    g_signal_new (I_("move-cursor"),
                  G_TYPE_FROM_CLASS (gobject_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                  G_STRUCT_OFFSET (AisleriotBoardClass, move_cursor),
                  NULL, NULL,
                  ar_marshal_BOOLEAN__STRING_UINT_ENUM,
                  G_TYPE_BOOLEAN,
                  3,
                  G_TYPE_STRING | G_SIGNAL_TYPE_STATIC_SCOPE,
                  G_TYPE_UINT,
                  CLUTTER_TYPE_MODIFIER_TYPE);

  signals[TOGGLE_SELECTION] =
    g_signal_new (I_("toggle-selection"),
                  G_TYPE_FROM_CLASS (gobject_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                  G_STRUCT_OFFSET (AisleriotBoardClass, toggle_selection),
                  NULL, NULL,
                  ar_marshal_BOOLEAN__STRING_UINT_ENUM,
                  G_TYPE_BOOLEAN,
                  3,
                  G_TYPE_STRING | G_SIGNAL_TYPE_STATIC_SCOPE,
                  G_TYPE_UINT,
                  CLUTTER_TYPE_MODIFIER_TYPE);

  signals[SELECT_ALL] =
    g_signal_new (I_("select-all"),
                  G_TYPE_FROM_CLASS (gobject_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                  G_STRUCT_OFFSET (AisleriotBoardClass, select_all),
                  NULL, NULL,
                  ar_marshal_BOOLEAN__STRING_UINT_ENUM,
                  G_TYPE_BOOLEAN,
                  3,
                  G_TYPE_STRING | G_SIGNAL_TYPE_STATIC_SCOPE,
                  G_TYPE_UINT,
                  CLUTTER_TYPE_MODIFIER_TYPE);

  signals[DESELECT_ALL] =
    g_signal_new (I_("deselect-all"),
                  G_TYPE_FROM_CLASS (gobject_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                  G_STRUCT_OFFSET (AisleriotBoardClass, deselect_all),
                  NULL, NULL,
                  ar_marshal_BOOLEAN__STRING_UINT_ENUM,
                  G_TYPE_BOOLEAN,
                  3,
                  G_TYPE_STRING | G_SIGNAL_TYPE_STATIC_SCOPE,
                  G_TYPE_UINT,
                  CLUTTER_TYPE_MODIFIER_TYPE);
#endif /* ENABLE_KEYNAV */

  /* Properties */
  g_object_class_install_property
    (gobject_class,
     PROP_GAME,
     g_param_spec_object ("game", NULL, NULL,
                          AISLERIOT_TYPE_GAME,
                          G_PARAM_WRITABLE |
                          G_PARAM_CONSTRUCT_ONLY |
                          G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (gobject_class,
     PROP_STYLE,
     g_param_spec_object ("style", NULL, NULL,
                          AR_TYPE_STYLE,
                          G_PARAM_WRITABLE |
                          G_PARAM_CONSTRUCT_ONLY |
                          G_PARAM_STATIC_STRINGS));

#ifdef ENABLE_KEYNAV
  /* Keybindings */
  binding_pool = clutter_binding_pool_get_for_class (klass);

  /* Cursor movement */
  closure = g_signal_type_cclosure_new (G_TYPE_FROM_CLASS (klass),
                                        G_STRUCT_OFFSET (AisleriotBoardClass, move_cursor));

  aisleriot_board_add_move_and_select_binding (binding_pool, closure,
                                               I_(MOVE_CURSOR_LEFT_RIGHT_S MOVE_LEFT_S),
                                               CLUTTER_Left, 0);
  aisleriot_board_add_move_and_select_binding (binding_pool, closure,
                                               I_(MOVE_CURSOR_LEFT_RIGHT_S MOVE_LEFT_S),
                                               CLUTTER_KP_Left, 0);

  aisleriot_board_add_move_and_select_binding (binding_pool, closure,
                                               I_(MOVE_CURSOR_LEFT_RIGHT_S MOVE_RIGHT_S),
                                               CLUTTER_Right, 0);
  aisleriot_board_add_move_and_select_binding (binding_pool, closure,
                                               I_(MOVE_CURSOR_LEFT_RIGHT_S MOVE_RIGHT_S),
                                               CLUTTER_KP_Right, 0);
  
  aisleriot_board_add_move_and_select_binding (binding_pool, closure,
                                               I_(MOVE_CURSOR_UP_DOWN_S MOVE_LEFT_S),
                                               CLUTTER_Up, 0);
  aisleriot_board_add_move_and_select_binding (binding_pool, closure,
                                               I_(MOVE_CURSOR_UP_DOWN_S MOVE_LEFT_S),
                                               CLUTTER_KP_Up, 0);

  aisleriot_board_add_move_and_select_binding (binding_pool, closure,
                                               I_(MOVE_CURSOR_UP_DOWN_S MOVE_RIGHT_S),
                                               CLUTTER_Down, 0);
  aisleriot_board_add_move_and_select_binding (binding_pool, closure,
                                               I_(MOVE_CURSOR_UP_DOWN_S MOVE_RIGHT_S),
                                               CLUTTER_KP_Down, 0);

  aisleriot_board_add_move_and_select_binding (binding_pool, closure,
                                               I_(MOVE_CURSOR_START_END_S MOVE_LEFT_S),
                                               CLUTTER_Home, 0);
  aisleriot_board_add_move_and_select_binding (binding_pool, closure,
                                               I_(MOVE_CURSOR_START_END_S MOVE_LEFT_S),
                                               CLUTTER_KP_Home, 0);
  aisleriot_board_add_move_and_select_binding (binding_pool, closure,
                                               I_(MOVE_CURSOR_START_END_S MOVE_LEFT_S),
                                               CLUTTER_Begin, 0);

  aisleriot_board_add_move_and_select_binding (binding_pool, closure,
                                               I_(MOVE_CURSOR_START_END_S MOVE_RIGHT_S),
                                               CLUTTER_End, 0);
  aisleriot_board_add_move_and_select_binding (binding_pool, closure,
                                               I_(MOVE_CURSOR_START_END_S MOVE_RIGHT_S),
                                               CLUTTER_KP_End, 0);

  aisleriot_board_add_move_binding (binding_pool, closure,
                                    I_(MOVE_CURSOR_PAGES_S MOVE_LEFT_S),
                                    CLUTTER_Page_Up, 0);
  aisleriot_board_add_move_binding (binding_pool, closure,
                                    I_(MOVE_CURSOR_PAGES_S MOVE_LEFT_S),
                                    CLUTTER_KP_Page_Up, 0);

  aisleriot_board_add_move_binding (binding_pool, closure,
                                    I_(MOVE_CURSOR_PAGES_S MOVE_RIGHT_S),
                                    CLUTTER_Page_Down, 0);
  aisleriot_board_add_move_binding (binding_pool, closure,
                                    I_(MOVE_CURSOR_PAGES_S MOVE_RIGHT_S),
                                    CLUTTER_KP_Page_Down, 0);

  g_closure_unref (closure);

  /* Selection */
  closure = g_signal_type_cclosure_new (G_TYPE_FROM_CLASS (klass),
                                        G_STRUCT_OFFSET (AisleriotBoardClass, toggle_selection));

  clutter_binding_pool_install_closure (binding_pool,
                                        I_("toggle-selection"),
                                        CLUTTER_space,
                                        0,
                                       closure);
  clutter_binding_pool_install_closure (binding_pool,
                                        I_("toggle-selection"),
                                        CLUTTER_KP_Space,
                                        0,
                                        closure);
  g_closure_unref (closure);

  closure = g_signal_type_cclosure_new (G_TYPE_FROM_CLASS (klass),
                                        G_STRUCT_OFFSET (AisleriotBoardClass, select_all));
  clutter_binding_pool_install_closure (binding_pool,
                                        I_("select-all"),
                                        CLUTTER_a,
                                        CLUTTER_CONTROL_MASK,
                                        closure);
  g_closure_unref (closure);

  closure = g_signal_type_cclosure_new (G_TYPE_FROM_CLASS (klass),
                                        G_STRUCT_OFFSET (AisleriotBoardClass, deselect_all));
  clutter_binding_pool_install_closure (binding_pool,
                                        I_("deselect-all"),
                                        CLUTTER_a,
                                        CLUTTER_CONTROL_MASK | CLUTTER_SHIFT_MASK,
                                        closure);
  g_closure_unref (closure);

  /* Activate */
  closure = g_signal_type_cclosure_new (G_TYPE_FROM_CLASS (klass),
                                        G_STRUCT_OFFSET (AisleriotBoardClass, activate));

  aisleriot_board_add_activate_binding (binding_pool, closure, CLUTTER_Return, 0);
  aisleriot_board_add_activate_binding (binding_pool, closure, CLUTTER_ISO_Enter, 0);
  aisleriot_board_add_activate_binding (binding_pool, closure, CLUTTER_KP_Enter, 0);

  g_closure_unref (closure);
#endif /* ENABLE_KEYNAV */
}

/* public API */

ClutterActor *
aisleriot_board_new (ArStyle *style,
                     AisleriotGame *game)
{
  return g_object_new (AISLERIOT_TYPE_BOARD,
                       "style", style,
                       "game", game,
                       NULL);
}

void
aisleriot_board_abort_move (AisleriotBoard *board)
{
  clear_state (board);
}
