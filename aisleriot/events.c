/* AisleRiot - events.c
 * Copyright (C) 1998, 2003 Jonathan Blandford <jrb@mit.edu>
 *
 * This game is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 */

#include "events.h"
#include "sol.h"
#include "cscmi.h"
#include "draw.h"
#include "dialog.h"
#include "slot.h"
#include "statistics.h"

/* These have been lifted straight from eog. */

#define hand_closed_data_width 20
#define hand_closed_data_height 20
static char hand_closed_data_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x80, 0x3f, 0x00,
   0x80, 0xff, 0x00, 0x80, 0xff, 0x00, 0xb0, 0xff, 0x00, 0xf0, 0xff, 0x00,
   0xe0, 0xff, 0x00, 0xe0, 0x7f, 0x00, 0xc0, 0x7f, 0x00, 0x80, 0x3f, 0x00,
   0x00, 0x3f, 0x00, 0x00, 0x3f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

#define hand_closed_mask_width 20
#define hand_closed_mask_height 20
static char hand_closed_mask_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x80, 0x3f, 0x00, 0xc0, 0xff, 0x00,
   0xc0, 0xff, 0x01, 0xf0, 0xff, 0x01, 0xf8, 0xff, 0x01, 0xf8, 0xff, 0x01,
   0xf0, 0xff, 0x01, 0xf0, 0xff, 0x00, 0xe0, 0xff, 0x00, 0xc0, 0x7f, 0x00,
   0x80, 0x7f, 0x00, 0x80, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

#define hand_open_data_width 20
#define hand_open_data_height 20
static char hand_open_data_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00,
   0x60, 0x36, 0x00, 0x60, 0x36, 0x00, 0xc0, 0x36, 0x01, 0xc0, 0xb6, 0x01,
   0x80, 0xbf, 0x01, 0x98, 0xff, 0x01, 0xb8, 0xff, 0x00, 0xf0, 0xff, 0x00,
   0xe0, 0xff, 0x00, 0xe0, 0x7f, 0x00, 0xc0, 0x7f, 0x00, 0x80, 0x3f, 0x00,
   0x00, 0x3f, 0x00, 0x00, 0x3f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

#define hand_open_mask_width 20
#define hand_open_mask_height 20
static char hand_open_mask_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x60, 0x3f, 0x00,
   0xf0, 0x7f, 0x00, 0xf0, 0x7f, 0x01, 0xe0, 0xff, 0x03, 0xe0, 0xff, 0x03,
   0xd8, 0xff, 0x03, 0xfc, 0xff, 0x03, 0xfc, 0xff, 0x01, 0xf8, 0xff, 0x01,
   0xf0, 0xff, 0x01, 0xf0, 0xff, 0x00, 0xe0, 0xff, 0x00, 0xc0, 0x7f, 0x00,
   0x80, 0x7f, 0x00, 0x80, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

hslot_type last_hslot;
gint last_cardid;
GTimer * click_timer = NULL;
gdouble dbl_click_time;
gint clicked = FALSE; /* Auxilary status for click-to-move. */

GdkCursor * hand_closed_cursor = NULL;
GdkCursor * hand_open_cursor = NULL;
GdkCursor * default_cursor = NULL;

enum {
  CURSOR_NONE = 0,
  CURSOR_OPEN,
  CURSOR_CLOSED
};

static void set_cursor (int cursor)
{
  if (cursor == CURSOR_NONE)
    gdk_window_set_cursor (playing_area->window, default_cursor);
  if (cursor == CURSOR_OPEN)
    gdk_window_set_cursor (playing_area->window, hand_open_cursor);
  if (cursor == CURSOR_CLOSED)
    gdk_window_set_cursor (playing_area->window, hand_closed_cursor);
}

/* If we are over a slot, set the cursor to the given cursor,
 * otherwise use the default cursor. */
static void set_cursor_by_location (int cursor, int x, int y)
{
  hslot_type hslot;
  gint cardid;

  slot_pressed(x, y, &hslot, &cardid);
  if (hslot)
    set_cursor (cursor);
  else
    set_cursor (CURSOR_NONE);
}

int waiting_for_mouse_up(void) {
  if (!press_data) return 0;
  if (press_data->status == STATUS_IS_DRAG) return 1;
  if (press_data->status == STATUS_SHOW) return 1;
  return 0;
}

void end_of_game_test() {

  gh_eval_str ("(end-move)");
  if(!game_over) { 
    game_over = !gh_scm2bool (gh_call0 (game_data->game_over_lambda));
    
    if (game_over) {
      timer_stop ();
      game_in_progress = FALSE;
      game_won = gh_scm2bool (gh_call0 (game_data->winning_game_lambda));
      refresh_screen ();
      update_statistics (game_won, timer_get ());
      show_game_over_dialog ();
    }
  }  
}

void drop_moving_cards(gint x, gint y) {
  GList* temp;
  SCM cardlist = SCM_EOL;
  hslot_type hslot;
  gint cardid, moved = 0;
  gint width, height;

  slot_pressed(x + card_width / 2 - press_data->xoffset, 
	       y + card_height / 2 - press_data->yoffset, 
	       &hslot, &cardid);

  if (hslot) {

    for (temp = press_data->cards; temp; temp = temp->next)
      cardlist = gh_cons(make_card(temp->data), cardlist);
    moved = gh_scm2bool (gh_call3 (game_data->button_released_lambda, 
				   gh_long2scm (press_data->hslot->id),
				   cardlist, 
				   gh_long2scm (hslot->id)));
  }

  if (!moved) {
    hslot = press_data->hslot;
    add_cards_to_slot(press_data->cards, hslot);

    gh_eval_str ("(discard-move)");
  }

  update_slot_length(hslot);

  gdk_drawable_get_size(press_data->moving_cards, &width, &height);
  gdk_window_move(press_data->moving_cards, 
		  hslot->pixelx + hslot->width - width, 
		  hslot->pixely + hslot->height - height);

  refresh_screen();

  free_press_data ();

  if(moved) end_of_game_test();
}

gint button_press_event (GtkWidget *widget, GdkEventButton *event, void *d)
{
  hslot_type hslot;
  gint cardid;
  gboolean double_click;

  if (!game_in_progress) {
    game_in_progress = TRUE;
    timer_start ();
  }

  if (event->button != 1 && event->button != 3)
    return TRUE;
  
  /* ignore the gdk synthetic click events */
  if (event->type != GDK_BUTTON_PRESS)
    return TRUE;
  
  if (press_data->status == STATUS_SHOW || press_data->status == STATUS_IS_DRAG)
    return TRUE;

  slot_pressed(event->x, event->y, &hslot, &cardid);

  if (!hslot)
    return TRUE;

  if (event->button == 1)
    set_cursor (CURSOR_CLOSED);

  /* We can't let Gdk do the double-click detection since the entire playing
   * area is one big widget it can't distinguish between single-clicks on two
   * cards and a double-click on one. */
  double_click = (g_timer_elapsed(click_timer,NULL) < dbl_click_time)
    && (last_cardid == cardid)
    && (last_hslot->id == hslot->id)
    && (event->button != 3);
  g_timer_start(click_timer);
  slot_pressed(event->x, event->y, &last_hslot, &last_cardid);

  press_data->xoffset = event->x;
  press_data->yoffset = event->y;
  press_data->hslot = hslot;
  press_data->cardid = cardid;

  /* The right-button-reveals-card action. */
  if (cardid > 0 && (press_data->status == STATUS_NONE || press_data->status == STATUS_CLICK)
      && event->button == 3) {
    hcard_type card = g_list_nth(hslot->cards, cardid - 1)->data;

    if (card->direction == UP) {      
      guint delta = hslot->exposed - (hslot->length - cardid) - 1;
      int x = hslot->pixelx + delta * hslot->pixeldx;
      int y = hslot->pixely + delta * hslot->pixeldy;

      press_data->status = STATUS_SHOW;
      press_data->moving_pixmap = get_card_picture (card->suit, card->value);
      press_data->moving_mask = mask;

      if (press_data->moving_pixmap != NULL)
        gdk_window_set_back_pixmap (press_data->moving_cards, 
				    press_data->moving_pixmap, 0);
      gdk_window_shape_combine_mask (press_data->moving_cards, 
				     press_data->moving_mask, 0, 0);
      gdk_window_move(press_data->moving_cards, x, y); 
      gdk_window_show(press_data->moving_cards);
    }
    return TRUE;
  } 

  clicked = TRUE;

  if (!(press_data->status == STATUS_CLICK && double_click)) {
    hslot_type hslot = press_data->hslot;
    GList* glist = g_list_nth(hslot->cards, press_data->cardid - 1); 
    SCM cardlist = SCM_EOL;

    press_data->status = cardid > 0 ? STATUS_MAYBE_DRAG : STATUS_NOT_DRAG;
    
    /* Check if the cards are draggable, assuming we have any cards. */
    if (hslot->cards) {
      for (; glist; glist = glist->next)
	cardlist = gh_cons(make_card((hcard_type)glist->data), cardlist);
      
      if (!gh_scm2bool (gh_call2 (game_data->button_pressed_lambda,
				  gh_long2scm (press_data->hslot->id), 
				  cardlist))) {
	press_data->status = STATUS_NOT_DRAG;
	clicked = FALSE;
      }
    }
  } 

  if (double_click) {
    press_data->status = STATUS_NONE; 
    gh_call2 (gh_eval_str ("record-move"), gh_long2scm (-1),
	      SCM_EOL);
    if (gh_scm2bool (gh_call1 (game_data->button_double_clicked_lambda,
                               gh_long2scm (hslot->id)))) 
      gh_call0 (gh_eval_str ("end-move"));
    else {
      gh_call0 (gh_eval_str ("discard-move"));
      /* Allow for a drag on the second click if nothing else happened. */
      if (cardid > 0)
	press_data->status = STATUS_MAYBE_DRAG; 
    }
    set_cursor (CURSOR_OPEN);
    refresh_screen ();
    end_of_game_test ();
    return TRUE;
  }
  return TRUE;
}

gint button_release_event (GtkWidget *widget, GdkEventButton *event, void *d)
{
  if (event->button == 3 && press_data->status == STATUS_SHOW) {
    press_data->status = STATUS_NONE;
    gdk_window_hide(press_data->moving_cards);
    press_data->moving_pixmap = NULL;
    press_data->moving_mask = NULL;
    return TRUE;
  }

  if (click_to_move && clicked && (press_data->status != STATUS_NOT_DRAG)) {
    clicked = FALSE;
    return TRUE;
  } else
    set_cursor_by_location (CURSOR_OPEN, event->x, event->y);

  if (event->button != 1)
    return TRUE;
  switch (press_data->status) {
  case STATUS_IS_DRAG:
    press_data->status = STATUS_NONE;
    drop_moving_cards(event->x, event->y);
    break;
    
  case STATUS_MAYBE_DRAG:
  case STATUS_NOT_DRAG:
    press_data->status = STATUS_CLICK;
    gh_call2 (gh_eval_str ("record-move"), gh_long2scm (-1),
	      SCM_EOL);
    if (gh_scm2bool (gh_call1 (game_data->button_clicked_lambda, 
			       gh_long2scm (press_data->hslot->id))))
	    gh_call0 (gh_eval_str ("end-move"));
    else
	    gh_call0 (gh_eval_str ("discard-move"));
    refresh_screen();
    end_of_game_test();
    break;
    
  case STATUS_CLICK:
  case STATUS_NONE:
    break;
  }

  return TRUE;
}

gint motion_notify_event (GtkWidget *widget, GdkEventMotion *event)
{
  if (press_data->status == STATUS_IS_DRAG) {
    gdk_window_move(press_data->moving_cards,  
		    event->x - press_data->xoffset,
		    event->y - press_data->yoffset);
    gdk_window_clear(press_data->moving_cards);
    set_cursor (CURSOR_CLOSED);
  }
  else if (press_data->status == STATUS_MAYBE_DRAG &&
	   (abs(press_data->xoffset - event->x) > 2 ||
	    abs(press_data->yoffset - event->y) > 2)) {
    press_data->status = STATUS_IS_DRAG;
    generate_press_data ();
    take_snapshot();
    set_cursor (CURSOR_CLOSED);
  } else {
    set_cursor_by_location (CURSOR_OPEN, event->x, event->y);
  }
  return FALSE;
}

gint app_configure_event (GtkWidget *widget, GdkEventConfigure *event) {
  GConfClient * gconf_client = gconf_client_get_default ();

  gconf_client_set_int (gconf_client, WIDTH_GCONF_KEY, event->width, NULL);
  gconf_client_set_int (gconf_client, HEIGHT_GCONF_KEY, event->height, NULL);

  return FALSE;
}

static GdkCursor * make_cursor (char * data, char * mask_data)
{
  GdkColor fg = {0, 65535, 65535, 65535};
  GdkColor bg = {0, 0, 0, 0};
  GdkPixmap *source;
  GdkPixmap *mask;
  GdkCursor * cursor;

  /* Yeah, hard-coded sizes are bad. */
  source = gdk_bitmap_create_from_data (NULL, data, 20, 20);
  mask = gdk_bitmap_create_from_data (NULL, mask_data, 20, 20);

  cursor = gdk_cursor_new_from_pixmap (source, mask, &fg, &bg, 10, 10);

  g_object_unref (source);
  g_object_unref (mask);
  
  return cursor;
}

gint configure_event (GtkWidget *widget, GdkEventConfigure *event) {
  gint tmptime;
  GtkSettings * settings;

  if (!default_cursor)
    default_cursor = gdk_cursor_new(GDK_LEFT_PTR);

  if (!hand_closed_cursor)
    hand_closed_cursor = make_cursor (hand_closed_data_bits,
				      hand_closed_mask_bits);

  if (!hand_open_cursor)
    hand_open_cursor = make_cursor (hand_open_data_bits,
				    hand_open_mask_bits);

  if(surface) {
    if(window_width == event->width && window_height == event->height)
      return TRUE;
    g_object_unref(surface);
  }

  window_width = event->width;
  window_height = event->height;

  surface =
    gdk_pixmap_new (playing_area->window, window_width, window_height, -1);

  rescale_cards ();
  refresh_screen();

  /* Set up the double-click detection.*/
  if (!click_timer)
    click_timer = g_timer_new();
  settings = gtk_settings_get_default();
  g_object_get(G_OBJECT(settings),"gtk-double-click-time",&tmptime,NULL);
  dbl_click_time = tmptime/1000.0;
 
  return FALSE;
}

