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

hslot_type last_hslot;
gint last_cardid;
GTimer * click_timer = NULL;
gdouble dbl_click_time;

int waiting_for_mouse_up(void) {
  if (!press_data) return 0;
  if (press_data->status == STATUS_IS_DRAG) return 1;
  if (press_data->status == STATUS_SHOW) return 1;
  return 0;
}

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

  gdk_drawable_get_size(press_data->moving_cards, &width, &height);
  gdk_window_move(press_data->moving_cards, 
		  hslot->x + hslot->width - width, 
		  hslot->y + hslot->height - height);

  refresh_screen();

  gdk_window_hide(press_data->moving_cards);

  if (press_data->moving_pixmap)
    g_object_unref(press_data->moving_pixmap);
  if (press_data->moving_mask)
    g_object_unref(press_data->moving_mask);
  press_data->moving_pixmap = NULL;
  press_data->moving_mask = NULL;

  if(moved) end_of_game_test();
}

gint button_press_event (GtkWidget *widget, GdkEventButton *event, void *d)
{
  hslot_type hslot;
  gint cardid;
  gboolean double_click;

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

  if (cardid > 0 && (press_data->status == STATUS_NONE || press_data->status == STATUS_CLICK)
      && event->button == 3) {
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
    return TRUE;
  } 
  
  if (!(press_data->status == STATUS_CLICK && double_click)) {
    press_data->status = cardid > 0 ? STATUS_MAYBE_DRAG : STATUS_NOT_DRAG;
  }
  if (double_click) {
    press_data->status = STATUS_NONE;
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
  }
  else if (press_data->status == STATUS_MAYBE_DRAG &&
	   (abs(press_data->xoffset - event->x) > 2 ||
	    abs(press_data->yoffset - event->y) > 2)) {

    hslot_type hslot = press_data->hslot;
    GList* glist = g_list_nth(hslot->cards, press_data->cardid - 1); 
    SCM cardlist = SCM_EOL;
    
    for (; glist; glist = glist->next)
      cardlist = gh_cons(make_card((hcard_type)glist->data), cardlist);
    
    if (gh_scm2bool (gh_call2 (game_data->button_pressed_lambda,
			       gh_long2scm (press_data->hslot->id), 
			       cardlist))) {
      press_data->status = STATUS_IS_DRAG;
      generate_press_data ();
      take_snapshot();
    }
    else
      press_data->status = STATUS_NOT_DRAG;      
  }
  return FALSE;
}

gint configure_event (GtkWidget *widget, GdkEventConfigure *event) {
  gint tmptime;
  GtkSettings * settings;

  if(surface) {
    gint old_w, old_h;

    gdk_drawable_get_size(surface, &old_w, &old_h);
    if(old_w == event->width && old_h == event->height)
      return TRUE;
    g_object_unref(surface);
  }
  else {
    timer_start();
  }

  surface =
    gdk_pixmap_new (playing_area->window, event->width, event->height,
		    gdk_drawable_get_visual (playing_area->window)->depth);
  
  refresh_screen();

  /* Set up the double-click detection.*/
  if (!click_timer)
    click_timer = g_timer_new();
  settings = gtk_settings_get_default();
  g_object_get(G_OBJECT(settings),"gtk-double-click-time",&tmptime,NULL);
  dbl_click_time = tmptime/1000.0;
  
  return FALSE;
}

