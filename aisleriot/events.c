/* Aisleriot - events.c
 * Copyright (C) 1998 Jonathan Blandford <jrb@mit.edu>
 *
 * This game is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "events.h"
#include "sol.h"
#include "cscmi.h"
#include "draw.h"
#include "dialog.h"

void end_of_game_test() {
  SCM testval;

  testval = gh_apply(game_data->game_over_lambda, gh_cons(SCM_EOL,SCM_EOL));

  if (!gh_scm2bool(testval)) {
	 if (gh_scm2bool(gh_apply(game_data->winning_game_lambda, gh_cons(SCM_EOL,SCM_EOL))))
		show_game_over_dialog(TRUE);
	 else
		show_game_over_dialog(FALSE);
  }  
}


void drop_moving_cards(gint x, gint y) {
  GList* temp;
  SCM arglist;
  SCM cardlist;
  hslot_type slot;
  gint slotid, cardid;
 
#ifdef DEBUG
  printf("drop_moving_cards\n");
#endif
  cardlist = SCM_EOL;

  slot_pressed(x, y, &slotid, &cardid);
 
  if (slotid == -1) {
	 add_cards_to_slot(press_data->cards, press_data->slot_id);
	 slot=get_slot(press_data->slot_id);
	 slot->expansion_depth = press_data->temporary_partial_hack;
	 update_slot_length(press_data->slot_id);
  }
  else {
	 for (temp = press_data->cards; temp; temp = temp->next)
		cardlist = gh_cons(make_card(temp->data), cardlist);

	 arglist =  gh_cons(gh_long2scm(press_data->slot_id), gh_cons(cardlist, gh_cons(gh_long2scm(slotid), SCM_EOL)));
	 if (gh_scm2bool(gh_apply(game_data->button_released_lambda, arglist))) {
		slot=get_slot(slotid);
		slot->expansion_depth = press_data->temporary_partial_hack;
		update_slot_length(slotid);
		end_of_game_test();
	 }
	 else {
		add_cards_to_slot(press_data->cards, press_data->slot_id);	 
		slot=get_slot(press_data->slot_id);
		slot->expansion_depth = press_data->temporary_partial_hack;
		update_slot_length(press_data->slot_id);
	 }
  }
  press_data->cards = NULL;
  if (press_data->moving_pixmap)
	 gdk_pixmap_unref(press_data->moving_pixmap);
  press_data->moving_pixmap = NULL;
  return;

}

void button_up_not_moved(gint x, gint y) {
  SCM arglist;
  gint slotid, cardid;
 
#ifdef DEBUG
  printf("button_up\n");
#endif

  slot_pressed(x, y, &slotid, &cardid);
 
  if (slotid == press_data->slot_id) {
	 arglist =  gh_cons(gh_long2scm(slotid), SCM_EOL);
	 gh_apply(game_data->button_clicked_lambda, arglist);
	 end_of_game_test();
  }
  return;
}

/* event handlers */
gint expose_event (GtkWidget *widget, GdkEventExpose *event, void *d)
{
#ifdef DEBUG
  printf("expose_event\n");
#endif
  gdk_draw_pixmap(widget->window,
						widget->style->fg_gc[GTK_WIDGET_STATE (widget)],
						surface,
						event->area.x, event->area.y,
						event->area.x, event->area.y,
						event->area.width, event->area.height);
  return TRUE;
}

gint button_press_event (GtkWidget *widget, GdkEventButton *event, void *d)
{
  GList* temp;
  SCM arglist;
  SCM templist;
  SCM cardlist = SCM_EOL;
  gint slotid, cardid;
  hslot_type slot;

#ifdef DEBUG
  printf("button_press_event\n");
#endif
  if (event->button == 1) {
	 if (press_data->button_pressed == 1) {
		slot_pressed(event->x,event->y, &slotid, &cardid);
		if (slotid != -1) {
		  templist =  gh_cons(gh_long2scm(slotid), SCM_EOL);
		  /* gh_apply(game_data->button_double_clicked_lambda, templist); */
		}
		return TRUE;
	 }
	 else if (press_data->button_pressed == 3)
		stop_show_card();
	 
	 press_data->button_pressed = 1;
	 
	 /* figure out which (if any) card was pressed... */
	 slot_pressed(event->x,event->y, &slotid, &cardid);

	 if (slotid == -1) {
		press_data->button_pressed = 0;
		return TRUE;
	 }

	 /* ask scheme if we want to drag... */
	 slot = get_slot(slotid);
	 if (slot->cards)
		for (temp = g_list_nth(slot->cards, cardid - 1); temp; temp = temp->next) {
		  cardlist = gh_cons(make_card((hcard_type)temp->data), cardlist);
		}
	 else
		cardlist = SCM_BOOL_F;
	 arglist =  gh_cons(gh_long2scm(slotid), gh_cons(cardlist, SCM_EOL));
	 if (!gh_scm2bool(gh_apply(game_data->button_pressed_lambda, arglist))) {
		press_data->slot_id = slotid;
		press_data->moving = FALSE;
		return TRUE;
	 }

	 
	 /* we've found the card -- prepare to draw it */
	 generate_press_data(event->x, event->y, slotid, cardid);
	 press_data->moving = TRUE;

	 take_snapshot();
	 gdk_draw_pixmap(surface, playing_area->style->black_gc,snapshot,0,0,0,0,-1,-1);
	 gdk_draw_pixmap(surface, playing_area->style->black_gc,press_data->moving_pixmap,0,0,
						  event->x - press_data->xoffset,
						  event->y - press_data->yoffset,-1,-1);
	 gdk_draw_pixmap(playing_area->window, playing_area->style->black_gc,surface,0,0,0,0,-1,-1);
	 

	 
  }
  else if (event->button == 3) {
	 if (press_data->button_pressed == 3)
		;
	 else if (press_data->button_pressed == 3)
		drop_moving_cards(event->x, event->y);
  }
  
  return TRUE;
}

gint button_release_event (GtkWidget *widget, GdkEventButton *event, void *d)
{

#ifdef DEBUG
printf("button_release_event\n");
#endif


  if (event->button == 1) {
	 if (press_data->button_pressed == 1) {
		press_data->button_pressed = 0;
		if (press_data->moving) {
		  press_data->moving = FALSE;
		  drop_moving_cards(event->x, event->y);
		  refresh_screen();
		}
		else {
		  button_up_not_moved(event->x, event->y);
		  refresh_screen();
		} 
	 }
  }
  else if (event->button == 3) {
	 if (press_data->button_pressed == 3)
		press_data->button_pressed = 0;
	 stop_show_card();
  }
  return TRUE;
}

gint configure_event (GtkWidget *widget, GdkEventConfigure *event) {
#ifdef DEBUG
printf("configure_event\n");
#endif
  if (blank_surface) {
	 gdk_pixmap_unref(blank_surface);
  }
  blank_surface = gdk_pixmap_new(playing_area->window,
											event->width,
											event->height,
											gtk_widget_get_visual (playing_area)->depth);
  if (surface) {
	 gdk_pixmap_unref(surface);
  }
  surface = gdk_pixmap_new(playing_area->window,
									event->width,
									event->height,
									gtk_widget_get_visual (playing_area)->depth);
  
  refresh_screen();
  return TRUE;

}

gint motion_notify_event (GtkWidget *widget, GdkEventMotion *event) {
  if ((press_data->button_pressed == 1) && (press_data->moving)) {
	 gdk_draw_pixmap(surface, playing_area->style->black_gc,snapshot,0,0,0,0,-1,-1);
	 gdk_draw_pixmap(surface, playing_area->style->black_gc,press_data->moving_pixmap,0,0,
						  event->x - press_data->xoffset,
						  event->y - press_data->yoffset,-1,-1);
	 gdk_draw_pixmap(playing_area->window, playing_area->style->black_gc,surface,0,0,0,0,-1,-1);
  }

	 
  return TRUE;
}
