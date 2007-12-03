/*
 *  Copyright © 1998, 2003 Jonathan Blandford <jrb@mit.edu>
 *  Copyright © 2007 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
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

#include <gtk/gtk.h>

#include <libgames-support/games-card-images.h>
#include <libgames-support/games-files.h>
#include <libgames-support/games-pixbuf-utils.h>
#include <libgames-support/games-sound.h>

#include "conf.h"

#include "game.h"

#include "board.h"

#define AISLERIOT_BOARD_GET_PRIVATE(board)(G_TYPE_INSTANCE_GET_PRIVATE ((board), AISLERIOT_TYPE_BOARD, AisleriotBoardPrivate))

/* The minimum size for the playing area. Almost completely arbitrary. */
#define BOARD_MIN_WIDTH 300
#define BOARD_MIN_HEIGHT 200

/* The limits for how much overlap there is between cards and
 * how much is allowed to slip off the bottom or right.
 */
#define MIN_DELTA (0.05)
#define MAX_DELTA (0.2)
#define MAX_OVERHANG (0.2)

/* The proportion of a slot dedicated to the card (horiz or vert). */
#ifdef HAVE_HILDON
#define CARD_SLOT_PROP (0.9)
#else
#define CARD_SLOT_PROP (0.8)
#endif

#define DOUBLE_TO_INT_CEIL(d) ((int) (d + 0.5))

#define STATIC_ASSERT(condition) STATIC_ASSERT_IMPL(condition, __LINE__)
#define STATIC_ASSERT_IMPL(condition, line) STATIC_ASSERT_IMPL2(condition, line)
#define STATIC_ASSERT_IMPL2(condition, line) typedef int _static_assert_line_##line[(condition) ? 1 : -1]

#ifdef HAVE_HILDON 
#define PIXBUF_DRAWING_LIKELIHOOD(cond) G_UNLIKELY (cond)
#else
#define PIXBUF_DRAWING_LIKELIHOOD(cond) (cond)
#endif /* HAVE_HILDON */

typedef enum {
  CURSOR_DEFAULT,
  CURSOR_OPEN,
  CURSOR_CLOSED,
  CURSOR_DROPPABLE,
  LAST_CURSOR
} CursorType;

typedef enum {
  STATUS_NONE,
  STATUS_MAYBE_DRAG,
  STATUS_NOT_DRAG,
  STATUS_IS_DRAG,
  STATUS_SHOW,
  LAST_STATUS
} MoveStatus;

struct _AisleriotBoardPrivate
{
  AisleriotGame *game;

  GdkGC *draw_gc;
  GdkGC *bg_gc;
  GdkGC *slot_gc;
  GdkCursor *cursor[LAST_CURSOR];

  char *card_theme;
  CardSize card_size;

  double width;
  double height;

  /* The size of a slot in pixels. */
  double xslotstep;
  double yslotstep;

  /* The offset of the cards within the slot. */
  int xoffset, yoffset;

  /* The offset within the window. */
  int xbaseoffset;

  /* Cards cache */
  GamesCardImages *images;

  /* Slot */
  gpointer slot_image; /* either a GdkPixbuf or GdkPixmap, depending on drawing mode */

  /* Button press */
  int last_click_x;
  int last_click_y;
  int double_click_time;
  guint32 last_click_time;

  /* Moving cards */
  Slot *moving_cards_origin_slot;
  int moving_cards_origin_card_id; /* The index of the card that was clicked on in hslot->cards; or -1 if the click wasn't on a card */
  GdkWindow *moving_cards_window;
  GByteArray *moving_cards;

  /* The 'reveal card' action's slot and card link */
  Slot *show_card_slot;
  int show_card_id;

  /* Click data */
  Slot *last_clicked_slot;
  int last_clicked_card_id;

  /* Focus handling */
  Slot *focus_slot;
  int focus_card_id; /* -1 for focused empty slot */
  int focus_line_width;
  GdkRectangle focus_rect;

  /* Selection */
  Slot *selection_slot;
  int selection_start_card_id;
  GdkRectangle selection_rect;
  GdkColor selection_colour;

  /* Highlight */
  Slot *highlight_slot;

#ifdef HAVE_MAEMO
  /* Tap-and-Hold */
  Slot *tap_and_hold_slot;
  int tap_and_hold_card_id;
#endif

  /* Bit field */
  guint droppable_supported : 1;
  guint touchscreen_mode : 1;
  guint use_pixbuf_drawing : 1;
  guint show_focus; /* whether the focus is drawn */

  guint click_to_move : 1;

  guint geometry_set : 1;
  guint is_rtl : 1;

  guint last_click_left_click : 1;
  guint click_status : 4; /* enough bits for MoveStatus */

  guint show_selection : 1;
  guint show_highlight : 1;
  guint scalable_cards : 1;
};

STATIC_ASSERT (LAST_STATUS < 16 /* 2^4 */);

enum
{
  PROP_0,
  PROP_GAME,
  PROP_SCALABLE_CARDS,
  PROP_THEME
};

static void get_slot_and_card_from_point (AisleriotBoard *board,
                                          int x,
                                          int y,
                                          Slot **slot,
                                          int *_cardid);
static void slot_update_card_images      (AisleriotBoard *board,
                                          Slot *slot);
static void slot_update_card_images_full (AisleriotBoard *board,
                                          Slot *slot,
                                          int highlight_start_card_id);

#ifndef HAVE_HILDON 

/* These cursors borrowed from EOG */
/* FIXMEchpe use themeable cursors here! */
#define hand_closed_data_width 20
#define hand_closed_data_height 20
static const char hand_closed_data_bits[] = {
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x80, 0x3f, 0x00,
  0x80, 0xff, 0x00, 0x80, 0xff, 0x00, 0xb0, 0xff, 0x00, 0xf0, 0xff, 0x00,
  0xe0, 0xff, 0x00, 0xe0, 0x7f, 0x00, 0xc0, 0x7f, 0x00, 0x80, 0x3f, 0x00,
  0x00, 0x3f, 0x00, 0x00, 0x3f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

#define hand_closed_mask_width 20
#define hand_closed_mask_height 20
static const char hand_closed_mask_bits[] = {
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x80, 0x3f, 0x00, 0xc0, 0xff, 0x00,
  0xc0, 0xff, 0x01, 0xf0, 0xff, 0x01, 0xf8, 0xff, 0x01, 0xf8, 0xff, 0x01,
  0xf0, 0xff, 0x01, 0xf0, 0xff, 0x00, 0xe0, 0xff, 0x00, 0xc0, 0x7f, 0x00,
  0x80, 0x7f, 0x00, 0x80, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

#define hand_open_data_width 20
#define hand_open_data_height 20
static const char hand_open_data_bits[] = {
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00,
  0x60, 0x36, 0x00, 0x60, 0x36, 0x00, 0xc0, 0x36, 0x01, 0xc0, 0xb6, 0x01,
  0x80, 0xbf, 0x01, 0x98, 0xff, 0x01, 0xb8, 0xff, 0x00, 0xf0, 0xff, 0x00,
  0xe0, 0xff, 0x00, 0xe0, 0x7f, 0x00, 0xc0, 0x7f, 0x00, 0x80, 0x3f, 0x00,
  0x00, 0x3f, 0x00, 0x00, 0x3f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

#define hand_open_mask_width 20
#define hand_open_mask_height 20
static const char hand_open_mask_bits[] = {
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x60, 0x3f, 0x00,
  0xf0, 0x7f, 0x00, 0xf0, 0x7f, 0x01, 0xe0, 0xff, 0x03, 0xe0, 0xff, 0x03,
  0xd8, 0xff, 0x03, 0xfc, 0xff, 0x03, 0xfc, 0xff, 0x01, 0xf8, 0xff, 0x01,
  0xf0, 0xff, 0x01, 0xf0, 0xff, 0x00, 0xe0, 0xff, 0x00, 0xc0, 0x7f, 0x00,
  0x80, 0x7f, 0x00, 0x80, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

#endif /* !HAVE_HILDON */

/* Cursor */

#ifndef HAVE_HILDON 

static GdkCursor *
make_cursor (GtkWidget *widget,
             const char *data,
             const char *mask_data)
{
  const GdkColor fg = { 0, 65535, 65535, 65535 };
  const GdkColor bg = { 0, 0, 0, 0 };
  GdkPixmap *source;
  GdkPixmap *mask;
  GdkCursor *cursor;

  /* Yeah, hard-coded sizes are bad. */
  source = gdk_bitmap_create_from_data (widget->window, data, 20, 20);
  mask = gdk_bitmap_create_from_data (widget->window, mask_data, 20, 20);

  cursor = gdk_cursor_new_from_pixmap (source, mask, &fg, &bg, 10, 10);

  g_object_unref (source);
  g_object_unref (mask);

  return cursor;
}

#endif /* !HAVE_HILDON */

static void
set_cursor (AisleriotBoard *board,
            CursorType cursor)
{
#ifndef HAVE_HILDON 
  AisleriotBoardPrivate *priv = board->priv;

  gdk_window_set_cursor (GTK_WIDGET (board)->window,
                         priv->cursor[cursor]);
#endif /* !HAVE_HILDON */
}

/* If we are over a slot, set the cursor to the given cursor,
 * otherwise use the default cursor. */
static void
set_cursor_by_location (AisleriotBoard *board,
                        int x,
                        int y)
{
#ifndef HAVE_HILDON 
  AisleriotBoardPrivate *priv = board->priv;
  Slot *selection_slot = priv->selection_slot;
  int selection_start_card_id = priv->selection_start_card_id;
  Slot *slot;
  int card_id;
  gboolean drop_valid = FALSE;
  CursorType cursor = CURSOR_DEFAULT;

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
    cursor = CURSOR_DROPPABLE;
  } else if (slot != NULL &&
             card_id >= 0 &&
             !CARD_GET_FACE_DOWN (CARD (slot->cards->data[card_id]))) {
    if (priv->click_status == STATUS_NONE) {
      cursor = CURSOR_OPEN;
    } else {
      cursor = CURSOR_CLOSED;
    }
  }

  set_cursor (board, cursor);
#endif /* !HAVE_HILDON */
}

/* card drawing functions */

static void
set_background_from_baize (GtkWidget *widget,
                           GdkGC *gc)
{
  GError *error = NULL;
  GdkPixbuf *pixbuf;
  GdkPixmap *pixmap;
  char *path;
  int width, height;

  path = games_build_filename (PIXMAPDIR, "baize.png");

  pixbuf = gdk_pixbuf_new_from_file (path, &error);
  g_free (path);
  if (error) {
    g_warning ("Failed to load the baize pixbuf: %s\n", error->message);
    g_error_free (error);
    return;
  }

  g_assert (pixbuf != NULL);

  width = gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);
  pixmap = gdk_pixmap_new (widget->window, width, height, -1);
  if (!pixmap) {
    g_object_unref (pixbuf);
    return;
  }

  /* FIXMEchpe: if we ever use a baize with an alpha channel,
   * clear the pixmap first!
   */
  gdk_draw_pixbuf (pixmap, NULL, pixbuf,
                   0, 0, 0, 0, width, height,
                   GDK_RGB_DITHER_NORMAL, 0, 0);

  gdk_gc_set_tile (gc, pixmap);
  gdk_gc_set_fill (gc, GDK_TILED);

  gdk_pixbuf_unref (pixbuf);
  g_object_unref (pixmap);
}

/* Slot helpers */

static void
get_slot_and_card_from_point (AisleriotBoard *board,
                              int x,
                              int y,
                              Slot **slot,
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
    Slot *hslot = slots->pdata[i];

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

static void
get_rect_by_slot_and_card (AisleriotBoard *board,
                           Slot *slot,
                           int card_id,
                           int num_cards,
                           GdkRectangle *rect)
{
  AisleriotBoardPrivate *priv = board->priv;
  guint delta;
  int first_card_id, num;

  g_return_if_fail (slot != NULL && card_id >= -1);

  first_card_id = slot->cards->len - slot->exposed;

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

  g_return_if_fail (priv->focus_slot != NULL);

  get_rect_by_slot_and_card (board,
                             priv->focus_slot,
                             priv->focus_card_id,
                             1, rect);
  widen_rect (rect, 2 * priv->focus_line_width);
}

static void
set_focus (AisleriotBoard *board,
           Slot *slot,
           int card_id,
           gboolean show_focus)
{
  AisleriotBoardPrivate *priv = board->priv;
  GtkWidget *widget = GTK_WIDGET (board);
  int top_card_id;

  /* Sanitise */
  top_card_id = slot ? ((int) slot->cards->len) - 1 : -1;
  card_id = MAX (card_id, top_card_id);

  if (priv->focus_slot == slot &&
      priv->focus_card_id == card_id)
    return;

  if (priv->focus_slot != NULL) {
    if (priv->show_focus) {
      gdk_window_invalidate_rect (widget->window, &priv->focus_rect, FALSE);
    
      priv->show_focus = FALSE;
    }

    priv->focus_slot = NULL;
    priv->focus_card_id = -1;
  }

  if (!slot)
    return;

  /* FIXMEchpe: check that if cardid >= 0, it's either face-up or the topmost face-down card */

  priv->focus_slot = slot;
  priv->focus_card_id = card_id;

  priv->show_focus = show_focus;
  if (show_focus) {
    get_focus_rect (board, &priv->focus_rect);
    gdk_window_invalidate_rect (widget->window, &priv->focus_rect, FALSE);
  }
}

/* Selection handling */

static void
get_selection_rect (AisleriotBoard *board,
                    GdkRectangle *rect)
{
  AisleriotBoardPrivate *priv = board->priv;
  int n_cards;

  g_return_if_fail (priv->selection_slot != NULL);

  n_cards = priv->selection_slot->cards->len - priv->selection_start_card_id;

  get_rect_by_slot_and_card (board,
                             priv->selection_slot,
                             priv->selection_start_card_id,
                             n_cards, rect);
  widen_rect (rect, 4 * priv->focus_line_width);
}

static void
set_selection (AisleriotBoard *board,
               Slot *slot,
               int card_id)
{
  AisleriotBoardPrivate *priv = board->priv;
  GtkWidget *widget = GTK_WIDGET (board);

  if (priv->selection_slot == slot &&
      priv->selection_start_card_id == card_id)
    return;

  if (priv->selection_slot != NULL) {
    if (priv->show_selection) {
      gdk_window_invalidate_rect (widget->window, &priv->selection_rect, FALSE);

      /* Clear selection card images */
      slot_update_card_images_full (board, priv->selection_slot, G_MAXINT);
    }

    priv->selection_slot = NULL;
    priv->selection_start_card_id = -1;
  }

  if (!slot)
    return;

  priv->selection_slot = slot;
  priv->selection_start_card_id = card_id;

  g_assert (card_id < 0 || card_id < slot->cards->len);

  if (priv->show_selection) {
    get_selection_rect (board, &priv->selection_rect);
    gdk_window_invalidate_rect (widget->window, &priv->selection_rect, FALSE);
  
    slot_update_card_images_full (board, slot, card_id);
  }
}

/* If a prefix of @cards is on top of @slot, set the selection to that prefix;
 * else set selection to NULL.
 */
static void
set_selection_with_cards (AisleriotBoard *board,
                          Slot *slot,
                          guint8 *cards,
                          guint n_cards)
{
  AisleriotBoardPrivate *priv = board->priv;

  while (n_cards > 0 &&
         slot->cards->len >= n_cards) {
    if (memcmp (slot->cards->data + slot->cards->len - n_cards,
                cards,
                n_cards) == 0 &&
        aisleriot_game_drag_valid (priv->game,
                                   slot->id,
                                   cards,
                                   n_cards)) {
      set_selection (board, slot, slot->cards->len - n_cards);
      return;
    }

    --n_cards;
  }

  set_selection (board, NULL, -1);
}

/* Slot functions */

static void
slot_update_geometry (AisleriotBoard *board,
                      Slot *slot)
{
  AisleriotBoardPrivate *priv = board->priv;
  GtkWidget *widget = GTK_WIDGET (board);
  GdkRectangle old_rect;
  GByteArray *cards;
  int delta, xofs, yofs, pixeldx;

  if (!priv->geometry_set)
    return;

  cards = slot->cards;
  old_rect = slot->rect;

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
  /* FIXMEchpe: check slot->exposed instead of cards->len? */
  pixeldx = 0;
  if (cards->len > 1) {
    double dx = 0, dy = 0;
    double n_cards = cards->len - 1; /* FIXMEchpe: slot->exposed - 1 ? */

    if (slot->expanded_down) {
      double y_from_bottom, max_dy = MAX_DELTA;

      if (slot->dy_set)
        max_dy = slot->expansion.dy;

      /* Calculate the compressed_dy that will let us fit within the board */
      y_from_bottom = ((double) (widget->allocation.height - slot->rect.y)) / ((double) priv->card_size.height);
      dy = (y_from_bottom - MAX_OVERHANG) / n_cards;
      dy = CLAMP (dy, MIN_DELTA, max_dy);
    } else if (slot->expanded_right) {
      if (priv->is_rtl) {
        double x_from_left, max_dx = MAX_DELTA;

        if (slot->dx_set)
          max_dx = slot->expansion.dx;

        x_from_left = ((double) slot->rect.x) / ((double) priv->card_size.width) + 1.0;
        dx = (x_from_left - MAX_OVERHANG) / n_cards;
        dx = CLAMP (dx, MIN_DELTA, max_dx);

        slot->pixeldx = DOUBLE_TO_INT_CEIL (- dx * priv->card_size.width);
        pixeldx = -slot->pixeldx;
      } else {
        double x_from_right, max_dx = MAX_DELTA;

        if (slot->dx_set)
          max_dx = slot->expansion.dx;

        x_from_right = ((double) (widget->allocation.width - slot->rect.x)) / ((double) priv->card_size.width);
        dx = (x_from_right - MAX_OVERHANG) / n_cards;
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

  if (GTK_WIDGET_REALIZED (widget)) {
    GdkRectangle damage = slot->rect;

    if (old_rect.width > 0 && old_rect.height > 0) {
      gdk_rectangle_union (&damage, &old_rect, &damage);
    }

    gdk_window_invalidate_rect (widget->window, &damage, FALSE);
  }

  slot->needs_update = FALSE;
}

static void
slot_update_card_images_full (AisleriotBoard *board,
                              Slot *slot,
                              int highlight_start_card_id)
{
  AisleriotBoardPrivate *priv = board->priv;
  GamesCardImages *images = priv->images;
  GPtrArray *card_images;
  guint n_cards, first_exposed_card_id, i;
  guint8 *cards;

  card_images = slot->card_images;
  g_ptr_array_set_size (card_images, 0);

  if (!priv->geometry_set)
    return;

  cards = slot->cards->data;
  n_cards = slot->cards->len;

  g_assert (n_cards >= slot->exposed);
  first_exposed_card_id = n_cards - slot->exposed;

  /* No need to get invisible cards from cache, which will just
   * slow us down!
   */
  for (i = 0; i < first_exposed_card_id; ++i) {
    g_ptr_array_add (card_images, NULL);
  }

  if (PIXBUF_DRAWING_LIKELIHOOD (priv->use_pixbuf_drawing)) {
    for (i = first_exposed_card_id; i < n_cards; ++i) {
      Card card = CARD (cards[i]);
      gboolean is_highlighted;

      is_highlighted = (i >= highlight_start_card_id);

      g_ptr_array_add (card_images,
                       games_card_images_get_card_pixbuf (images, card, is_highlighted));
    }
  } else {
    for (i = first_exposed_card_id; i < n_cards; ++i) {
      Card card = CARD (cards[i]);
      gboolean is_highlighted;

      is_highlighted = (i >= highlight_start_card_id);

      g_ptr_array_add (card_images,
                       games_card_images_get_card_pixmap (images, card, is_highlighted));
    }
  }
}

static void
slot_update_card_images (AisleriotBoard *board,
                         Slot *slot)
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

/* Work out new sizes and spacings for the cards. */
static void
aisleriot_board_setup_geometry (AisleriotBoard *board)
{
  AisleriotBoardPrivate *priv = board->priv;
  GtkWidget *widget = GTK_WIDGET (board);
  GPtrArray *slots;
  guint i, n_slots;
  CardSize card_size;
  gboolean size_changed;

  /* Nothing to do yet */
  if (aisleriot_game_get_state (priv->game) <= GAME_LOADED)
    return;

  g_return_if_fail (GTK_WIDGET_REALIZED (widget));
  g_return_if_fail (priv->width > 0 && priv->height > 0);

  priv->xslotstep = ((double) widget->allocation.width) / priv->width;
  priv->yslotstep = ((double) widget->allocation.height) / priv->height;

  size_changed = games_card_images_set_size (priv->images,
                                             priv->xslotstep,
                                             priv->yslotstep,
                                             CARD_SLOT_PROP);

  card_size = priv->card_size = games_card_images_get_size (priv->images);

  /* If the cards are too far apart, bunch them in the middle. */
  priv->xbaseoffset = 0;
  if (priv->xslotstep > (card_size.width * 3) / 2) {
    priv->xslotstep = (card_size.width * 3) / 2;
    /* FIXMEchpe: if there are expand-right slots, reserve the space for them instead? */
    priv->xbaseoffset = (widget->allocation.width - priv->xslotstep * priv->width) / 2;
  }
  if (priv->yslotstep > (card_size.height * 3) / 2) {
    priv->yslotstep = (card_size.height * 3) / 2;
    /* FIXMEchpe: if there are expand-down slots, reserve the space for them instead?
       priv->ybaseoffset = (widget->allocation.height - priv->yslotstep * priv->height) / 2;
    */
  }

  priv->xoffset = (priv->xslotstep - card_size.width) / 2;
  priv->yoffset = (priv->yslotstep - card_size.height) / 2;

  if (PIXBUF_DRAWING_LIKELIHOOD (priv->use_pixbuf_drawing)) {
    priv->slot_image = games_card_images_get_slot_pixbuf (priv->images, FALSE);
  } else {
    priv->slot_image = games_card_images_get_slot_pixmap (priv->images, FALSE);
  }

  gdk_gc_set_clip_mask (priv->slot_gc, games_card_images_get_slot_mask (priv->images));
  gdk_gc_set_clip_mask (priv->draw_gc, games_card_images_get_card_mask (priv->images));

  /* NOTE! Updating the slots checks that geometry is set, so
   * we set it to TRUE already.
   */
  priv->geometry_set = TRUE;

  /* Now recalculate the slot locations. */
  slots = aisleriot_game_get_slots (priv->game);

  n_slots = slots->len;
  for (i = 0; i < n_slots; ++i) {
    Slot *slot = slots->pdata[i];

    slot_update_geometry (board, slot);
    slot_update_card_images (board, slot);
  }
}

static void
drag_begin (AisleriotBoard *board)
{
  AisleriotBoardPrivate *priv = board->priv;
  GtkWidget *widget = GTK_WIDGET (board);
  Slot *hslot;
  int delta, height, width;
  int x, y;
  GdkPixmap *moving_pixmap;
  GdkBitmap *card_mask;
  GdkBitmap *moving_mask;
  GdkGC *gc1, *gc2;
  const GdkColor masked = { 0, 0, 0, 0 };
  const GdkColor unmasked = { 1, 0xffff, 0xffff, 0xffff };
  int num_moving_cards;
  gboolean use_pixbuf_drawing = priv->use_pixbuf_drawing;
  guint i;
  GByteArray *cards;
  GdkDrawable *drawable;
  GdkWindowAttr attributes;

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

  /* Unset the selection. It'll be re-set if the drag is aborted */
  set_selection (board, NULL, -1);

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

  /* Take the cards off of the stack */
  g_byte_array_set_size (cards, priv->moving_cards_origin_card_id);
  g_ptr_array_set_size (hslot->card_images, priv->moving_cards_origin_card_id);

  width = priv->card_size.width + (num_moving_cards - 1) * hslot->pixeldx;
  height = priv->card_size.height + (num_moving_cards - 1) * hslot->pixeldy;

  drawable = GDK_DRAWABLE (widget->window);

  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.event_mask = 0;
  attributes.x = x;
  attributes.y = y;
  attributes.width = width;
  attributes.height = height;
  attributes.colormap = gdk_drawable_get_colormap (drawable);
  attributes.visual = gdk_drawable_get_visual (drawable);

  priv->moving_cards_window = gdk_window_new (widget->window, &attributes,
                                              GDK_WA_VISUAL | GDK_WA_COLORMAP | GDK_WA_X | GDK_WA_Y);

  moving_pixmap = gdk_pixmap_new (priv->moving_cards_window,
                                  width, height,
                                  gdk_drawable_get_visual (priv->moving_cards_window)->depth);
  moving_mask = gdk_pixmap_new (priv->moving_cards_window,
                                width, height,
                                1);

  gc1 = gdk_gc_new (moving_pixmap);
  gc2 = gdk_gc_new (moving_mask);

  gdk_draw_rectangle (moving_pixmap, gc1, TRUE,
                      0, 0, width, height);

  gdk_gc_set_foreground (gc2, &masked);
  gdk_draw_rectangle (moving_mask, gc2, TRUE,
                      0, 0, width, height);
  gdk_gc_set_foreground (gc2, &unmasked);

  card_mask = games_card_images_get_card_mask (priv->images);
  gdk_gc_set_clip_mask (gc1, card_mask);
  gdk_gc_set_clip_mask (gc2, card_mask);

  /* FIXMEchpe: RTL issue: this doesn't work right when we allow dragging of
   * more than one card from a expand-right slot. (But right now no game .scm
   * does allow that.)
   */
  x = y = 0;
  width = priv->card_size.width;
  height = priv->card_size.height;

  for (i = 0; i < priv->moving_cards->len; ++i) {
    Card hcard = CARD (priv->moving_cards->data[i]);

    gdk_gc_set_clip_origin (gc1, x, y);
    gdk_gc_set_clip_origin (gc2, x, y);

    if (PIXBUF_DRAWING_LIKELIHOOD (use_pixbuf_drawing)) {
      GdkPixbuf *pixbuf;

      pixbuf = games_card_images_get_card_pixbuf (priv->images, hcard, FALSE);
      if (!pixbuf)
        goto next;

      gdk_draw_pixbuf (moving_pixmap, gc1,
                       pixbuf,
                       0, 0, x, y, width, height,
                       GDK_RGB_DITHER_NONE, 0, 0);
      gdk_draw_rectangle (moving_mask, gc2, TRUE,
                          x, y, width, height);
    } else {
      GdkPixmap *pixmap;

      pixmap = games_card_images_get_card_pixmap (priv->images, hcard, FALSE);
      if (!pixmap)
        goto next;

      gdk_draw_drawable (moving_pixmap, gc1, pixmap,
                          0, 0, x, y, width, height);
      gdk_draw_rectangle (moving_mask, gc2, TRUE,
                          x, y, width, height);
    }

  next:

    x += hslot->pixeldx;
    y += hslot->pixeldy;
  }

  g_object_unref (gc1);
  g_object_unref (gc2);

  gdk_window_set_back_pixmap (priv->moving_cards_window,
			      moving_pixmap, 0);
  gdk_window_shape_combine_mask (priv->moving_cards_window,
				 moving_mask, 0, 0);

  g_object_unref (moving_pixmap);
  g_object_unref (moving_mask);

  gdk_window_show (priv->moving_cards_window);

  slot_update_geometry (board, hslot);
  slot_update_card_images (board, hslot);
      
  set_cursor (board, CURSOR_CLOSED);
}

static void
drag_end (AisleriotBoard *board,
          gboolean moved)
{
  AisleriotBoardPrivate *priv = board->priv;

  if (priv->moving_cards_window != NULL) {
    gdk_window_hide (priv->moving_cards_window);
    gdk_window_destroy (priv->moving_cards_window);
    priv->moving_cards_window = NULL;
  }

  /* FIXMEchpe: check that slot->cards->len == moving_cards_origin_card_id !!! FIXMEchpe what to do if not, abort the game? */
  /* Add the origin cards back to the origin slot */
  if (!moved &&
      priv->moving_cards_origin_slot != NULL &&
      priv->moving_cards->len > 0) {
    aisleriot_game_slot_add_cards (priv->game,
                                   priv->moving_cards_origin_slot,
                                   priv->moving_cards->data,
                                   priv->moving_cards->len);
  }

  priv->click_status = STATUS_NONE;
  priv->moving_cards_origin_slot = NULL;
  priv->moving_cards_origin_card_id = -1;
}

static gboolean
cards_are_droppable (AisleriotBoard *board,
                     Slot *slot)
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

static Slot *
find_drop_target (AisleriotBoard *board,
                  gint x,
                  gint y)
{
  AisleriotBoardPrivate *priv = board->priv;
  Slot *new_hslot;
  Slot *retval = NULL;
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
  Slot *hslot;
  gboolean moved = FALSE;

  hslot = find_drop_target (board,
                            x - priv->last_click_x,
                            y - priv->last_click_y);

  if (hslot) {
    moved = aisleriot_game_drop_cards (priv->game,
                                       priv->moving_cards_origin_slot->id,
                                       hslot->id,
                                       priv->moving_cards->data,
                                       priv->moving_cards->len);
  }

  if (moved) {
    aisleriot_game_end_move (priv->game);
    games_sound_play ("click");
  } else {
    aisleriot_game_discard_move (priv->game);
    games_sound_play ("slide");
  }

  drag_end (board, moved);

  if (moved)
    aisleriot_game_test_end_of_game (priv->game);
}

static void
highlight_drop_target (AisleriotBoard *board,
                       Slot *slot)
{
  AisleriotBoardPrivate *priv = board->priv;
  GtkWidget *widget = GTK_WIDGET (board);
  GdkRectangle rect;

  if (slot == priv->highlight_slot)
    return;

  /* Invalidate the old highlight rect */
  if (priv->highlight_slot != NULL &&
      priv->show_highlight) {
    get_rect_by_slot_and_card (board,
                               priv->highlight_slot,
                               priv->highlight_slot->cards->len - 1 /* it's ok if this is == -1 */,
                               1, &rect);
    gdk_window_invalidate_rect (widget->window, &rect, FALSE);

    /* FIXMEchpe only update the topmost card? */
    /* It's ok to call this directly here, since the old highlight_slot cannot
     * have been the same as the current selection_slot!
     */
    slot_update_card_images_full (board, priv->highlight_slot, G_MAXINT);
  }

  /* Need to set the highlight slot even when we the cards aren't droppable
   * since that can happen when the game doesn't support FEATURE_DROPPABLE.
   */
  priv->highlight_slot = slot;
  
  if (!cards_are_droppable (board, slot))
    return;

  if (!priv->show_highlight)
    return;

  /* Prepare the highlight pixbuf/pixmaps and invalidate the new highlight rect */
  get_rect_by_slot_and_card (board,
                             slot,
                             slot->cards->len - 1 /* it's ok if this is == -1 */,
                             1, &rect);
  gdk_window_invalidate_rect (widget->window, &rect, FALSE);

  /* FIXMEchpe only update the topmost card? */
  /* It's ok to call this directly, since the highlight slot is always
   * different from the selection slot!
   */
  slot_update_card_images_full (board, slot, slot->cards->len - 1);
}

static void
reveal_card (AisleriotBoard *board,
             Slot *slot,
             int cardid)
{
  AisleriotBoardPrivate *priv = board->priv;
  GtkWidget *widget = GTK_WIDGET (board);
  Card card;
  GdkRectangle rect;

  if (priv->show_card_slot == slot)
    return;

  if (priv->show_card_slot != NULL) {
    get_rect_by_slot_and_card (board,
                               priv->show_card_slot,
                               priv->show_card_id,
                               1, &rect);
    gdk_window_invalidate_rect (widget->window, &rect, FALSE);
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

  get_rect_by_slot_and_card (board,
                            priv->show_card_slot,
                            priv->show_card_id,
                            1, &rect);
  gdk_window_invalidate_rect (widget->window, &rect, FALSE);
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
  set_selection (board, NULL, -1);

  aisleriot_game_get_geometry (game, &priv->width, &priv->height);

  aisleriot_board_setup_geometry (board);

#if 0
  g_print ("{ %.3f , %.3f /* %s */ },\n",
           priv->width, priv->height,
           aisleriot_game_get_game_file (priv->game));
#endif

  gtk_widget_queue_draw (GTK_WIDGET (board));
}

static void
slot_changed_cb (AisleriotGame *game,
                 Slot *slot,
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
    set_selection (board, NULL, -1);

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
    set_focus (board, slot, priv->focus_card_id, priv->show_focus);
  }
  if (slot == priv->highlight_slot) {
    highlight_drop_target (board, NULL);
  }
}

/* Class implementation */

G_DEFINE_TYPE (AisleriotBoard, aisleriot_board, GTK_TYPE_DRAWING_AREA);

static void
aisleriot_board_realize (GtkWidget *widget)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
  AisleriotBoardPrivate *priv = board->priv;
  GdkDisplay *display;
  GtkSettings *settings;
#ifndef HAVE_HILDON 
  gboolean touchscreen_mode;
#ifdef GDK_WINDOWING_X11
  char *xft_rgba = NULL;
#endif /* GDK_WINDOWING_X11 */
#endif /* !HAVE_HILDON */

  GTK_WIDGET_CLASS (aisleriot_board_parent_class)->realize (widget);

  display = gtk_widget_get_display (widget);

  games_card_images_set_drawable (priv->images, widget->window);

  priv->draw_gc = gdk_gc_new (widget->window);

  priv->bg_gc = gdk_gc_new (widget->window);
  set_background_from_baize (widget, priv->bg_gc);
  
  priv->slot_gc = gdk_gc_new (widget->window);

#ifndef HAVE_HILDON 
  /* Create cursors */
  priv->cursor[CURSOR_DEFAULT] = gdk_cursor_new_for_display (display, GDK_LEFT_PTR);
  priv->cursor[CURSOR_OPEN] = make_cursor (widget, hand_open_data_bits, hand_open_mask_bits);
  priv->cursor[CURSOR_CLOSED] = make_cursor (widget, hand_closed_data_bits, hand_closed_mask_bits);
  priv->cursor[CURSOR_DROPPABLE] = gdk_cursor_new_for_display (display, GDK_DOUBLE_ARROW); /* FIXMEchpe: better cursor */
#endif /* !HAVE_HILDON */

   /* Set up the double-click detection. */
  settings = gtk_widget_get_settings (widget);
  /* FIXMEchpe listen to changes! */
  g_object_get (settings,
                "gtk-double-click-time", &priv->double_click_time,
#ifndef HAVE_HILDON 
                "gtk-touchscreen-mode", &touchscreen_mode,
#ifdef GDK_WINDOWING_X11
                "gtk-xft-rgba", &xft_rgba,
#endif /* GDK_WINDOWING_X11 */
#endif /* !HAVE_HILDON */
                NULL);

#if defined (GDK_WINDOWING_X11) && !defined (HAVE_HILDON)
  if (xft_rgba != NULL) {
    gboolean antialias_set = TRUE;
    cairo_antialias_t antialias_mode = CAIRO_ANTIALIAS_DEFAULT;
    cairo_subpixel_order_t subpixel_order = CAIRO_SUBPIXEL_ORDER_DEFAULT;

    if (strcmp (xft_rgba, "none") == 0) {
      antialias_set = FALSE;
    } else if (strcmp (xft_rgba, "rgb") == 0) {
      antialias_mode = CAIRO_ANTIALIAS_SUBPIXEL;
      subpixel_order = CAIRO_SUBPIXEL_ORDER_RGB;
    } else if (strcmp (xft_rgba, "bgr") == 0) {
      antialias_mode = CAIRO_ANTIALIAS_SUBPIXEL;
      subpixel_order = CAIRO_SUBPIXEL_ORDER_BGR;
    } else if (strcmp (xft_rgba, "vrgb") == 0) {
      antialias_mode = CAIRO_ANTIALIAS_SUBPIXEL;
      subpixel_order = CAIRO_SUBPIXEL_ORDER_VRGB;
    } else if (strcmp (xft_rgba, "vbgr") == 0) {
      antialias_mode = CAIRO_ANTIALIAS_SUBPIXEL;
      subpixel_order = CAIRO_SUBPIXEL_ORDER_VBGR;
    }

    g_free (xft_rgba);

    if (antialias_set) {
      games_card_images_set_antialias (priv->images,
                                       antialias_mode,
                                       subpixel_order);
    }
  }
#endif /* GDK_WINDOWING_X11 && !HAVE_HILDON */
#ifndef HAVE_HILDON 
  priv->touchscreen_mode = touchscreen_mode != FALSE;
#endif /* !HAVE_HILDON */

  aisleriot_board_setup_geometry (board);
}

static void
aisleriot_board_unrealize (GtkWidget *widget)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
  AisleriotBoardPrivate *priv = board->priv;
#ifndef HAVE_HILDON 
  guint i;
#endif

  priv->geometry_set = FALSE;

  g_object_unref (priv->draw_gc);
  priv->draw_gc = NULL;
  g_object_unref (priv->bg_gc);
  priv->bg_gc = NULL;
  g_object_unref (priv->slot_gc);
  priv->slot_gc = NULL;

#ifndef HAVE_HILDON 
  for (i = 0; i < LAST_CURSOR; ++i) {
    gdk_cursor_unref (priv->cursor[i]);
    priv->cursor[i] = NULL;
  }
#endif /* !HAVE_HILDON*/

  games_card_images_set_drawable (priv->images, NULL);

  clear_state (board);

  priv->slot_image = NULL;

  GTK_WIDGET_CLASS (aisleriot_board_parent_class)->unrealize (widget);
}

static void
aisleriot_board_style_set (GtkWidget *widget,
                           GtkStyle *previous_style)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
  AisleriotBoardPrivate *priv = board->priv;
  GdkColor *colour = NULL;

  gtk_widget_style_get (widget,
                        "focus-line-width", &priv->focus_line_width,
                        "selection-color", &colour,
                        NULL);

  if (colour != NULL) {
    priv->selection_colour = *colour;
    gdk_color_free (colour);
  } else {
    const GdkColor default_colour = { 0, 0 /* red */, 0 /* green */, 0xaa00 /* blue */};

    priv->selection_colour = default_colour;
  }

  games_card_images_set_selection_color (priv->images,
                                         &priv->selection_colour);

  /* FIXMEchpe: is this the right place? */
  priv->is_rtl = gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL;

  GTK_WIDGET_CLASS (aisleriot_board_parent_class)->style_set (widget, previous_style);
}

static void
aisleriot_board_size_allocate (GtkWidget *widget,
                               GtkAllocation *allocation)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
  gboolean is_same;

  is_same = (memcmp (&widget->allocation, allocation, sizeof (GtkAllocation)) == 0);
  
  GTK_WIDGET_CLASS (aisleriot_board_parent_class)->size_allocate (widget, allocation);

  if (is_same)
    return;

  if (GTK_WIDGET_REALIZED (widget)) {
    aisleriot_board_setup_geometry (board);
  }
}

/* The gtkwidget.c focus in/out handlers queue a shallow draw;
 * that's ok for us but maybe we want to optimise this a bit to
 * only do it if we have a focus to draw/erase?
 */
static gboolean
aisleriot_board_focus_in (GtkWidget *widget,
                          GdkEventFocus *event)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
  AisleriotBoardPrivate *priv = board->priv;

  /* If we're not showing focus, no need to redraw on focus change */
  if (!priv->show_focus)
    return FALSE;

  return GTK_WIDGET_CLASS (aisleriot_board_parent_class)->focus_in_event (widget, event);
}

static gboolean
aisleriot_board_focus_out (GtkWidget *widget,
                           GdkEventFocus *event)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
  AisleriotBoardPrivate *priv = board->priv;

  clear_state (board);

  /* If we're not showing focus, no need to redraw on focus change */
  if (!priv->show_focus)
    return FALSE;

  return GTK_WIDGET_CLASS (aisleriot_board_parent_class)->focus_out_event (widget, event);
}

static gboolean
aisleriot_board_button_press (GtkWidget *widget,
                              GdkEventButton *event)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
  AisleriotBoardPrivate *priv = board->priv;
  Slot *hslot;
  int cardid;
  guint button;
  gboolean drag_valid;
  guint state;
  gboolean is_double_click;

  /* NOTE: It's ok to just return instead of chaining up, since the
   * parent class has no class closure for this event.
   */

  /* ignore the gdk synthetic double/triple click events */
  if (event->type != GDK_BUTTON_PRESS)
    return FALSE;

  /* Don't do anything if a modifier is pressed */
  state = event->state & gtk_accelerator_get_default_mod_mask ();
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
                     (event->time - priv->last_click_time <= priv->double_click_time) &&
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
    set_selection (board, NULL, -1);

    priv->click_status = STATUS_NONE;

    return FALSE;
  }

  set_cursor (board, CURSOR_CLOSED);

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
    Slot *clicked_slot = hslot;

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

    set_cursor (board, CURSOR_OPEN);

    return TRUE;
  }

  /* button == 1 from now on */

  if (priv->selection_slot == NULL)
    goto set_selection;

  /* If clicked on a non-selected slot, we either drop the selected cards there
   * (in click-to-select mode only), or we set the selection.
   */
  if (hslot != priv->selection_slot) {
    Slot *selection_slot = priv->selection_slot;
    int selection_start_card_id = priv->selection_start_card_id;
    gboolean moved;
    guint8 *cards;
    guint n_cards;

    /* NOTE: We cannot use aisleriot_game_drop_valid here since the
     * game may not support the "droppable" feature.
     */
    if (!priv->click_to_move ||
        selection_start_card_id < 0)
      goto set_selection;

    /* Remove the old selection. If the move doesn't succeed, we'll select
     * the clicked-on cards instead.
     */
    set_selection (board, NULL, -1);

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

      if (selection_slot->needs_update)
        g_signal_emit_by_name (priv->game, "slot-changed", selection_slot); /* FIXMEchpe! */

      aisleriot_game_test_end_of_game (priv->game);

      return TRUE;
    }

    /* Not moved; discard the move and select the new cards */
    aisleriot_game_discard_move (priv->game);

    aisleriot_game_slot_add_cards (priv->game, selection_slot, cards, n_cards);

    /* FIXMEchpe: maybe beep? */

    goto set_selection;
  }

  if (cardid != priv->selection_start_card_id)
    goto set_selection;
    
  /* Single click on the selected slot & card, we take that to mean to deselect,
   * but only in click-to-move mode.
   */
  if (priv->click_to_move) {
    set_selection (board, NULL, -1);

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
    set_selection (board, hslot, cardid);
    priv->click_status = priv->click_to_move ? STATUS_NOT_DRAG : STATUS_MAYBE_DRAG;
  } else {
    set_selection (board, NULL, -1);
    priv->click_status = STATUS_NOT_DRAG;
  }

  set_focus (board, hslot, cardid, FALSE);

  /* Reveal the card on left click */
  if (priv->click_to_move) {
    reveal_card (board, hslot, cardid);
  }

  return FALSE;
}

static gboolean
aisleriot_board_button_release (GtkWidget *widget,
                                GdkEventButton *event)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
  AisleriotBoardPrivate *priv = board->priv;
  /* guint state; */

  /* NOTE: It's ok to just return instead of chaining up, since the
   * parent class has no class closure for this event.
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
      Slot *slot;
      int card_id;

      /* Don't do the action if the mouse moved away from the clicked slot; see bug #329183 */
      get_slot_and_card_from_point (board, event->x, event->y, &slot, &card_id);
      if (!slot || slot != priv->last_clicked_slot)
        break;

      aisleriot_game_record_move (priv->game, -1, NULL, 0);
      if (aisleriot_game_button_clicked_lambda (priv->game, slot->id)) {
        aisleriot_game_end_move (priv->game);
	games_sound_play ("click");
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
aisleriot_board_motion_notify (GtkWidget *widget,
                               GdkEventMotion * event)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
  AisleriotBoardPrivate *priv = board->priv;

  /* NOTE: It's ok to just return instead of chaining up, since the
   * parent class has no class closure for this event.
   */

  if (priv->click_status == STATUS_IS_DRAG) {
    Slot *slot;
    int x, y;

    x = event->x - priv->last_click_x;
    y = event->y - priv->last_click_y;

    slot = find_drop_target (board, x, y);
    highlight_drop_target (board, slot);

    gdk_window_move (priv->moving_cards_window, x, y);
    /* FIXMEchpe: why? */
    gdk_window_clear (priv->moving_cards_window);

    set_cursor (board, CURSOR_CLOSED);
  } else if (priv->click_status == STATUS_MAYBE_DRAG &&
             gtk_drag_check_threshold (widget,
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
aisleriot_board_key_press (GtkWidget *widget,
                           GdkEventKey* event)
{
  return GTK_WIDGET_CLASS (aisleriot_board_parent_class)->key_press_event (widget, event);
}

#ifdef HAVE_MAEMO

static gboolean
aisleriot_board_tap_and_hold_query (GtkWidget *widget,
                                    GdkEvent *_event)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
  AisleriotBoardPrivate *priv = board->priv;
  /* BadDocs! should mention that this is a GdkEventButton! */
  GdkEventButton *event = (GdkEventButton *) _event;
  Slot *slot;
  int card_id;

  /* In click-to-move mode, we always reveal with just the left click */
  if (priv->click_to_move)
    return TRUE;

  get_slot_and_card_from_point (board, event->x, event->y, &slot, &card_id);
  if (!slot ||
      card_id < 0 ||
      card_id == slot->cards->len - 1 ||
      CARD_GET_FACE_DOWN (CARD (slot->cards->data[card_id])))
    return TRUE;

  priv->tap_and_hold_slot = slot;
  priv->tap_and_hold_card_id = card_id;
  
  return GTK_WIDGET_CLASS (aisleriot_board_parent_class)->tap_and_hold_query (widget, _event);
}

static void
aisleriot_board_tap_and_hold (GtkWidget *widget)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
  AisleriotBoardPrivate *priv = board->priv;

  g_assert (priv->tap_and_hold_slot != NULL && priv->tap_and_hold_card_id >= 0);

  reveal_card (board, priv->tap_and_hold_slot, priv->tap_and_hold_card_id);

  GTK_WIDGET_CLASS (aisleriot_board_parent_class)->tap_and_hold (widget);
}

#endif /* HAVE_MAEMO */

static gboolean
aisleriot_board_expose_event (GtkWidget *widget,
                              GdkEventExpose *event)
{
  AisleriotBoard *board = AISLERIOT_BOARD (widget);
  AisleriotBoardPrivate *priv = board->priv;
  GdkRegion *region = event->region;
  GdkRectangle *rects;
  int i, n_rects;
  GPtrArray *slots;
  guint n_slots;
  Slot **exposed_slots;
  Slot *highlight_slot;
  guint n_exposed_slots;
  gboolean use_pixbuf_drawing = priv->use_pixbuf_drawing;

  /* NOTE: It's ok to just return instead of chaining up, since the
   * parent class has no class closure for this event.
   */

  if (event->window != widget->window)
    return FALSE;

  if (gdk_region_empty (region))
    return FALSE;

  /* First paint the background */

  gdk_region_get_rectangles (region, &rects, &n_rects);
  if (n_rects == 0)
    return FALSE;

#if 0
  {
    g_print ("Exposing area %d:%d@(%d,%d) ", event->area.width, event->area.height,
             event->area.x, event->area.y);
    for (i = 0; i < n_rects; ++i) {
      g_print ("[Rect %d:%d@(%d,%d)] ", rects[i].width, rects[i].height, rects[i].x, rects[i].y);
    }
    g_print ("\n");
  }
#endif

  for (i = 0; i < n_rects; ++i) {
    gdk_draw_rectangle (widget->window, priv->bg_gc,
                        TRUE,
                        rects[i].x, rects[i].y,
                        rects[i].width, rects[i].height);
  }
  g_free (rects);

  /* Only draw the the cards when the geometry is set, and we're in a resize */
  if (!priv->geometry_set)
    return TRUE;

  /* Now draw the slots and cards */
  slots = aisleriot_game_get_slots (priv->game);

  n_slots = slots->len;

  /* First check which slots are exposed */
  /* It's fine to allocate on the stack, since there'll never be very many slots */
  exposed_slots = g_newa (Slot*, n_slots);
  n_exposed_slots = 0;

  for (i = 0; i < n_slots; ++i) {
    Slot *slot = slots->pdata[i];

    /* Check whether this slot needs to be drawn */
    if (gdk_region_rect_in (region, &slot->rect) == GDK_OVERLAP_RECTANGLE_OUT)
      continue;

    exposed_slots[n_exposed_slots++] = slot;
  }

  highlight_slot = priv->highlight_slot;

  /* First draw the slots. Otherwise they'll overlap on cards
   * (e.g. in Elevator, after a card was removed).
   */
  for (i = 0; i < n_exposed_slots; ++i) {
    Slot *hslot = exposed_slots[i];
    int x, y;

    /* FIXMEchpe: if ((hslot->length - hslot->exposed) >= 0) ?? */
    if (hslot->cards->len > 0)
      continue;

    /* If the slot it empty, draw the slot image */
    x = hslot->rect.x;
    y = hslot->rect.y;

    gdk_gc_set_clip_origin (priv->slot_gc, x, y);

    if (PIXBUF_DRAWING_LIKELIHOOD (use_pixbuf_drawing)) {
      GdkPixbuf *pixbuf;

      if (G_LIKELY (hslot != highlight_slot)) {
        pixbuf = priv->slot_image;
      } else {
        pixbuf = games_card_images_get_slot_pixbuf (priv->images,
                                                    priv->show_highlight);
      }

      if (!pixbuf)
        continue;

      gdk_draw_pixbuf (widget->window, priv->slot_gc, pixbuf,
                       0, 0, x, y,
                       priv->card_size.width, priv->card_size.height,
                       GDK_RGB_DITHER_NONE, 0, 0);
    } else {
      GdkPixmap *pixmap;

      if (G_LIKELY (hslot != highlight_slot)) {
        pixmap = priv->slot_image;
      } else {
        pixmap = games_card_images_get_slot_pixmap (priv->images,
                                                    priv->show_highlight);
      }

      if (!pixmap)
        continue;

      gdk_draw_drawable (widget->window, priv->slot_gc, pixmap,
                         0, 0, x, y,
                         priv->card_size.width, priv->card_size.height);
    }
  }

  /* Now draw the cards */
  for (i = 0; i < n_exposed_slots; ++i) {
    Slot *hslot = exposed_slots[i];
    GByteArray *cards = hslot->cards;
    gpointer *card_images = hslot->card_images->pdata;
    GdkRectangle card_rect;
    guint i, n_cards;

    n_cards = cards->len;
    if (n_cards == 0)
      continue;

    card_rect.x = hslot->rect.x;
    card_rect.y = hslot->rect.y;
    card_rect.width = priv->card_size.width;
    card_rect.height = priv->card_size.height;

    if (priv->is_rtl &&
        hslot->expanded_right) {
      card_rect.x += hslot->rect.width - priv->card_size.width;
    }

    for (i = n_cards - hslot->exposed; i < n_cards; ++i) {
      /* Check whether this card needs to be drawn */
      /* FIXMEchpe: we can be even smarter here, by checking
       * with the rect of the part of the card that's not going 
       * to be obscured by later drawn cards anyway.
       */
      if (gdk_region_rect_in (region, &card_rect) == GDK_OVERLAP_RECTANGLE_OUT)
        goto next;

      if (PIXBUF_DRAWING_LIKELIHOOD (use_pixbuf_drawing)) {
        GdkPixbuf *pixbuf;

        pixbuf = card_images[i];
        if (!pixbuf)
          goto next;

        gdk_gc_set_clip_origin (priv->draw_gc, card_rect.x, card_rect.y);
        gdk_draw_pixbuf (widget->window, priv->draw_gc, pixbuf,
                         0, 0, card_rect.x, card_rect.y, -1, -1,
                         GDK_RGB_DITHER_NONE, 0, 0);
      } else {
        GdkPixmap *pixmap;

        pixmap = card_images[i];
        if (!pixmap)
          goto next;

        gdk_gc_set_clip_origin (priv->draw_gc, card_rect.x, card_rect.y);
        gdk_draw_drawable (widget->window, priv->draw_gc, pixmap,
                           0, 0, card_rect.x, card_rect.y, -1, -1);
      }

    next:

      card_rect.x += hslot->pixeldx;
      card_rect.y += hslot->pixeldy;
    }
  }

  /* Draw the revealed card */
  if (priv->show_card_slot != NULL &&
      gdk_region_rect_in (region, &priv->show_card_slot->rect) != GDK_OVERLAP_RECTANGLE_OUT) {
    GdkRectangle card_rect;

    get_rect_by_slot_and_card (board,
                               priv->show_card_slot,
                               priv->show_card_id,
                               1, &card_rect);

    if (PIXBUF_DRAWING_LIKELIHOOD (use_pixbuf_drawing)) {
      GdkPixbuf *pixbuf;

      pixbuf = priv->show_card_slot->card_images->pdata[priv->show_card_id];
      if (!pixbuf)
        goto draw_focus;

      gdk_gc_set_clip_origin (priv->draw_gc, card_rect.x, card_rect.y);
      gdk_draw_pixbuf (widget->window, priv->draw_gc, pixbuf,
                       0, 0, card_rect.x, card_rect.y, -1, -1,
                       GDK_RGB_DITHER_NONE, 0, 0);
    } else {
      GdkPixmap *pixmap;

      pixmap = priv->show_card_slot->card_images->pdata[priv->show_card_id];
      if (!pixmap)
        goto draw_focus;

      gdk_gc_set_clip_origin (priv->draw_gc, card_rect.x, card_rect.y);
      gdk_draw_drawable (widget->window, priv->draw_gc, pixmap,
                          0, 0, card_rect.x, card_rect.y, -1, -1);
    }
  }

draw_focus:

  /* FIXMEchpe: Once we support focus, draw the focus here */

expose_done:

  /* Parent class has no expose handler, no need to chain up */
  return TRUE;
}

static void
aisleriot_board_init (AisleriotBoard *board)
{
  GtkWidget *widget = GTK_WIDGET (board);
  AisleriotBoardPrivate *priv;

  priv = board->priv = AISLERIOT_BOARD_GET_PRIVATE (board);

  GTK_WIDGET_SET_FLAGS (widget, GTK_CAN_FOCUS);

  priv->click_to_move = FALSE;
  priv->show_selection = FALSE;

  priv->show_card_id = -1;

  priv->moving_cards = g_byte_array_sized_new (SLOT_CARDS_N_PREALLOC);

  gtk_widget_set_events (widget,
			 gtk_widget_get_events (widget) |
                         GDK_EXPOSURE_MASK |
                         GDK_BUTTON_PRESS_MASK |
                         GDK_POINTER_MOTION_MASK |
                         GDK_BUTTON_RELEASE_MASK);
  /* FIMXEchpe: no need for motion events on maemo, unless we explicitly activate drag mode */

  /* This only enforces the minimum size. It is actually set using the
   * window size.
   */
  gtk_widget_set_size_request (widget, BOARD_MIN_WIDTH, BOARD_MIN_HEIGHT);

#ifdef HAVE_MAEMO
  gtk_widget_tap_and_hold_setup (widget, NULL, NULL, GTK_TAP_AND_HOLD_PASS_PRESS);
#endif
}

static GObject *
aisleriot_board_constructor (GType type,
			     guint n_construct_properties,
			     GObjectConstructParam *construct_params)
{
  GObject *object;
  AisleriotBoard *board;
  AisleriotBoardPrivate *priv;

  object = G_OBJECT_CLASS (aisleriot_board_parent_class)->constructor
            (type, n_construct_properties, construct_params);

  board = AISLERIOT_BOARD (object);
  priv = board->priv;

  g_assert (priv->game != NULL);

  /* Create this down here since we need to have the scalable_cards value */
  priv->images = games_card_images_new (priv->scalable_cards);

  return object;
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

  g_free (priv->card_theme);

  g_byte_array_free (priv->moving_cards, TRUE);

  g_object_unref (priv->images);

  G_OBJECT_CLASS (aisleriot_board_parent_class)->finalize (object);
}

static void
aisleriot_board_get_property (GObject *object,
			   guint prop_id,
			   GValue *value,
			   GParamSpec *pspec)
{
  AisleriotBoard *board = AISLERIOT_BOARD (object);
  AisleriotBoardPrivate *priv = board->priv;

  switch (prop_id) {
    case PROP_THEME:
      g_value_set_string (value, aisleriot_board_get_card_theme (board));
      break;
    case PROP_SCALABLE_CARDS:
      g_value_set_boolean (value, priv->scalable_cards);
  }
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
    case PROP_THEME:
      aisleriot_board_set_card_theme (board, g_value_get_string (value));
      break;
    case PROP_SCALABLE_CARDS:
      priv->scalable_cards = g_value_get_boolean (value) != FALSE;
      break;
  }
}

static void
aisleriot_board_class_init (AisleriotBoardClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);
  GtkBindingSet *binding_set;

  gobject_class->constructor = aisleriot_board_constructor;
  gobject_class->finalize = aisleriot_board_finalize;
  gobject_class->set_property = aisleriot_board_set_property;
  gobject_class->get_property = aisleriot_board_get_property;

  widget_class->realize = aisleriot_board_realize;
  widget_class->unrealize = aisleriot_board_unrealize;
  widget_class->style_set = aisleriot_board_style_set;
  widget_class->size_allocate = aisleriot_board_size_allocate;
  widget_class->focus_in_event = aisleriot_board_focus_in;
  widget_class->focus_out_event = aisleriot_board_focus_out;
  widget_class->button_press_event = aisleriot_board_button_press;
  widget_class->button_release_event = aisleriot_board_button_release;
  widget_class->motion_notify_event = aisleriot_board_motion_notify;
  widget_class->key_press_event = aisleriot_board_key_press;
  widget_class->expose_event = aisleriot_board_expose_event;
#ifdef HAVE_MAEMO
  widget_class->tap_and_hold_query = aisleriot_board_tap_and_hold_query;
  widget_class->tap_and_hold = aisleriot_board_tap_and_hold;
#endif /* HAVE_MAEMO */

  g_object_class_install_property
    (gobject_class,
     PROP_GAME,
     g_param_spec_object ("game", NULL, NULL,
                          AISLERIOT_TYPE_GAME,
                          G_PARAM_WRITABLE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB | G_PARAM_CONSTRUCT_ONLY));

  g_object_class_install_property
    (gobject_class,
     PROP_SCALABLE_CARDS,
     g_param_spec_boolean ("scalable-cards", NULL, NULL,
                           FALSE,
                           G_PARAM_WRITABLE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB | G_PARAM_CONSTRUCT_ONLY));

  g_object_class_install_property
    (gobject_class,
     PROP_THEME,
     g_param_spec_string ("theme", NULL, NULL,
                          NULL,
                          G_PARAM_READWRITE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB));

  gtk_widget_class_install_style_property
    (widget_class,
     g_param_spec_boxed ("selection-color", NULL, NULL,
                         GDK_TYPE_COLOR,
                         G_PARAM_READABLE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB));

  binding_set = gtk_binding_set_by_class (klass);

  g_type_class_add_private (gobject_class, sizeof (AisleriotBoardPrivate));
}

/* public API */

GtkWidget *
aisleriot_board_new (AisleriotGame *game,
                     gboolean scalable_cards)
{
  return g_object_new (AISLERIOT_TYPE_BOARD,
                       "game", game,
                       "scalable-cards", scalable_cards,
                       NULL);
}

gboolean
aisleriot_board_set_card_theme (AisleriotBoard *board,
                                const char *card_theme)
{
  AisleriotBoardPrivate *priv = board->priv;
  gboolean retval;

  g_return_val_if_fail (card_theme != NULL && card_theme[0] != '\0', FALSE);

  priv->geometry_set = FALSE;
  priv->slot_image = NULL;

  retval = games_card_images_set_theme (priv->images, card_theme);

  /* NOTE! We need to do this even if setting the theme failed, since
   * the attempt will have wiped out the old theme data!
   */
  if (GTK_WIDGET_REALIZED (board)) {
    /* Update card size and slot locations for new card theme (might have changed aspect!)*/
    aisleriot_board_setup_geometry (board);

    gtk_widget_queue_draw (GTK_WIDGET (board));
  }

  g_object_notify (G_OBJECT (board), "theme");

  return retval;
}

const char *
aisleriot_board_get_card_theme (AisleriotBoard *board)
{
  AisleriotBoardPrivate *priv = board->priv;
  const char *theme;

  theme = games_card_images_get_theme (priv->images);

  return theme != NULL ? theme : GAMES_CARD_THEME_DEFAULT;
}

void
aisleriot_board_set_click_to_move (AisleriotBoard *board,
                                   gboolean click_to_move)
{
  AisleriotBoardPrivate *priv = board->priv;
  GtkWidget *widget = GTK_WIDGET (board);

  click_to_move = click_to_move != FALSE;
  if (priv->click_to_move == click_to_move)
    return;

  /* Clear the selection. Do this before setting the new value,
   * since otherwise selection won't get cleared correctly.
   */
  set_selection (board, NULL, -1);

  priv->click_to_move = click_to_move;
  priv->show_selection = click_to_move;

  if (GTK_WIDGET_REALIZED (widget)) {
    gtk_widget_queue_draw (widget);
  }
}

void
aisleriot_board_abort_move (AisleriotBoard *board)
{
  clear_state (board);
}

void
aisleriot_board_set_pixbuf_drawing (AisleriotBoard *board,
                                    gboolean use_pixbuf_drawing)
{
  AisleriotBoardPrivate *priv = board->priv;
  GtkWidget *widget = GTK_WIDGET (board);

  use_pixbuf_drawing = use_pixbuf_drawing != FALSE;

  if (use_pixbuf_drawing == priv->use_pixbuf_drawing)
    return;

  priv->use_pixbuf_drawing = use_pixbuf_drawing;

  games_card_images_set_cache_mode (priv->images,
                                    use_pixbuf_drawing ? CACHE_PIXBUFS : CACHE_PIXMAPS);

  if (GTK_WIDGET_REALIZED (widget) &&
      priv->geometry_set) {
    /* Need to update the geometry, so we update the cached card images! */
    aisleriot_board_setup_geometry (board);

    gtk_widget_queue_draw (widget);
  }
}
