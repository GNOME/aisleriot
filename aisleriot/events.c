/* AisleRiot - events.c
 * Copyright (C) 1998 Jonathan Blandford <jrb@mit.edu>
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

int waiting_for_mouse_up(void) {
  if (!press_data) return 0;
  if (press_data->status == STATUS_IS_DRAG) return 1;
  if (press_data->status == STATUS_SHOW) return 1;
  return 0;
}

/* Defining SINGLE_ACTION prevents scheme from being called twice when
 * a double click occurs but delays the single_click action by 250ms */
/*#define SINGLE_ACTION */
/* I don't like this, so it's commented out -jrb */

/* Button press statuses */

void end_of_game_test() {

  gh_eval_str ("(end-move)");
  if(!game_over) { 
    game_over = !gh_scm2bool (gh_call0 (game_data->game_over_lambda));
    
    if (game_over) {
      timer_stop ();
      game_won = gh_scm2bool (gh_call0 (game_data->winning_game_lambda));
      refresh_screen ();
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

  slot_pressed(x + get_card_width() / 2 - press_data->xoffset, 
	       y + get_card_height() / 2 - press_data->yoffset, 
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
  press_data->cards = NULL;

  gdk_window_get_size(press_data->moving_cards, &width, &height);
  gdk_window_move(press_data->moving_cards, 
		  hslot->x + hslot->width - width, 
		  hslot->y + hslot->height - height);

  refresh_screen();

  gdk_window_hide(press_data->moving_cards);

  if (press_data->moving_pixmap)
    gdk_pixmap_unref(press_data->moving_pixmap);
  if (press_data->moving_mask)
    gdk_pixmap_unref(press_data->moving_mask);
  press_data->moving_pixmap = NULL;
  press_data->moving_mask = NULL;

  if(moved) end_of_game_test();
}

#ifdef SINGLE_ACTION
guint timer_click = 0;

gint single_click ()
{
  if (press_data->status == STATUS_CLICK) {
    press_data->status = STATUS_NONE;
    gh_call1 (game_data->button_clicked_lambda, 
	      gh_long2scm (press_data->hslot->id));
    refresh_screen();
    end_of_game_test();
  }
  return FALSE;
}
#endif

gint button_press_event (GtkWidget *widget, GdkEventButton *event, void *d)
{
  hslot_type hslot;
  gint cardid;
  static guint dbl_click_time = 250;
  static int first_press;

  /* ignore the gdk synthetic click events */
  if (event->type != GDK_BUTTON_PRESS)
    return TRUE;
  
  if ((event->button == 2) || 
      (event->button == 3 && (press_data->status == STATUS_IS_DRAG)) ||
      (event->button == 1 && (press_data->status == STATUS_SHOW || press_data->status == STATUS_IS_DRAG)))
    return TRUE;

#ifndef SINGLE_ACTION
  if (gdk_time_get() > first_press + dbl_click_time)
    press_data->status = STATUS_NONE;
#endif
  slot_pressed(event->x, event->y, &hslot, &cardid);

  if (!hslot)
    return TRUE;
  press_data->xoffset = event->x;
  press_data->yoffset = event->y;
  press_data->hslot = hslot;
  press_data->cardid = cardid;
  if (event->button == 1) {
    if (press_data->status == STATUS_CLICK) {
      /* double click */
      press_data->status = STATUS_NONE;
#ifdef SINGLE_ACTION
      gtk_timeout_remove (timer_click);
#endif
      gh_call2 (gh_eval_str ("record-move"), gh_long2scm (-1),
		SCM_EOL);
      if (gh_scm2bool (gh_call1 (game_data->button_double_clicked_lambda, 
				 gh_long2scm (hslot->id))))
	      gh_call0 (gh_eval_str ("end-move"));
      else
	      gh_call0 (gh_eval_str ("discard-move"));
      refresh_screen ();
      end_of_game_test ();
      return TRUE;
    }
    else {
      first_press = gdk_time_get();
      press_data->status = cardid > 0 ? STATUS_MAYBE_DRAG : STATUS_NOT_DRAG;
#ifdef SINGLE_ACTION
      timer_click = 
	gtk_timeout_add (dbl_click_time, (GtkFunction) (single_click), NULL);
#endif
    }
  }
  else if (event->button == 3 && cardid > 0 && press_data->status == STATUS_NONE) {
    hcard_type card = g_list_nth(hslot->cards, cardid - 1)->data;
    
    if (card->direction == UP) {      
      guint delta = hslot->exposed - (hslot->length - cardid) - 1;
      int x = hslot->x + delta * hslot->dx;
      int y = hslot->y + delta * hslot->dy;
      
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
  }
  return TRUE;
}

gint button_release_event (GtkWidget *widget, GdkEventButton *event, void *d)
{
  if (event->button == 2)
    return TRUE;

  switch (press_data->status) {
  case STATUS_IS_DRAG:
    if (event->button != 1) return TRUE;
    press_data->status = STATUS_NONE;
    drop_moving_cards(event->x, event->y);
    break;
    
  case STATUS_SHOW:
    if (event->button != 3) return TRUE;
    press_data->status = STATUS_NONE;
    gdk_window_hide(press_data->moving_cards);
    press_data->moving_pixmap = NULL;
    press_data->moving_mask = NULL;
    break;
    
  case STATUS_MAYBE_DRAG:
  case STATUS_NOT_DRAG:
    if (event->button != 1) return TRUE;
    press_data->status = STATUS_CLICK;
#ifndef SINGLE_ACTION
    gh_call2 (gh_eval_str ("record-move"), gh_long2scm (-1),
	      SCM_EOL);
    if (gh_scm2bool (gh_call1 (game_data->button_clicked_lambda, 
			       gh_long2scm (press_data->hslot->id))))
	    gh_call0 (gh_eval_str ("end-move"));
    else
	    gh_call0 (gh_eval_str ("discard-move"));
    refresh_screen();
    end_of_game_test();
#endif
    break;
    
  case STATUS_CLICK:
  case STATUS_NONE:
    break;
  }
  return TRUE;
}

gint motion_notify_event (GtkWidget *widget, GdkEventMotion *event)
{
/*g_print ("move\n");*/
  if (press_data->status == STATUS_IS_DRAG) {
/*g_print ("keep movin\n");*/
    gdk_window_move(press_data->moving_cards,  
		    event->x - press_data->xoffset,
		    event->y - press_data->yoffset);
  }
  else if (press_data->status == STATUS_MAYBE_DRAG &&
	   (abs(press_data->xoffset - event->x) > 2 ||
	    abs(press_data->yoffset - event->y) > 2)) {

    hslot_type hslot = press_data->hslot;
    GList* glist = g_list_nth(hslot->cards, press_data->cardid - 1); 
    SCM cardlist = SCM_EOL;
/*g_print ("maybe move\n");*/
    
    for (; glist; glist = glist->next)
      cardlist = gh_cons(make_card((hcard_type)glist->data), cardlist);
    
    if (gh_scm2bool (gh_call2 (game_data->button_pressed_lambda,
			       gh_long2scm (press_data->hslot->id), 
			       cardlist))) {
      press_data->status = STATUS_IS_DRAG;
#ifdef SINGLE_ACTION
      gtk_timeout_remove (timer_click);
#endif
      generate_press_data ();
      take_snapshot();
    }
    else
      press_data->status = STATUS_NOT_DRAG;      
  }
  return TRUE;
}

gint configure_event (GtkWidget *widget, GdkEventConfigure *event) {

  if(surface) {
    gint old_w, old_h;

    gdk_window_get_size(surface, &old_w, &old_h);
    if(old_w == event->width && old_h == event->height)
      return TRUE;
    gdk_pixmap_unref(surface);
  }
  else {
    timer_start();
  }

  surface =
    gdk_pixmap_new (playing_area->window, event->width, event->height,
		    gdk_window_get_visual (playing_area->window)->depth);
  
  refresh_screen();

  return TRUE;
}

