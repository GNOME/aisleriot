/* Aisleriot - draw.c
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

#include "draw.h"
#include "sol.h"
#include "slot.h"
#include "card.h"

void draw_cards(GdkPixmap* pix) {
  GList* listptr;
  GdkPixmap* tempPix;
  card_type* temp_card;
  gint vert_offset, horiz_offset;
  GList* expanded_card_list;
  GdkGC* gc;

#ifdef DEBUG
  printf("draw_cards\n");
#endif
  
  gc = playing_area->style->fg_gc[GTK_STATE_NORMAL];
  
  gdk_gc_set_clip_mask(gc,mask); 
  
  for (listptr = get_slot_list(); listptr; listptr = listptr->next) {
	 /*  are we drawing a normal slot? */
	 if (((hslot_type)listptr->data)->type == NORMAL_SLOT) {
		/*...and are there cards in the slot? */
		if (((hslot_type)listptr->data)->cards) {
		  /* then get the last card on the slot */
		  temp_card =( g_list_last(((hslot_type)listptr->data)->cards) )->data;
		  
		  if (temp_card->direction == DOWN) {
			 gdk_gc_set_clip_origin(gc,((hslot_type)listptr->data)->x,((hslot_type)listptr->data)->y); 
			 gdk_draw_pixmap(pix, playing_area->style->black_gc, get_card_back_pixmap(), 0,0,
								  ((hslot_type)listptr->data)->x, ((hslot_type)listptr->data)->y, -1, -1);
		  }
		  else {
			 tempPix = get_card_picture(temp_card->suit, temp_card->value);
			 gdk_gc_set_clip_origin(gc,((hslot_type)listptr->data)->x,((hslot_type)listptr->data)->y); 
			 gdk_draw_pixmap(pix, playing_area->style->black_gc, tempPix, 0,0,
								  ((hslot_type)listptr->data)->x, ((hslot_type)listptr->data)->y, -1, -1);
		  }
		}
	 }
	 else if (((hslot_type)listptr->data)->type == EXPANDING_SLOT){
		/* we are drawing an expanding slot */
		/*...and are there cards in the slot? */
	  if (((hslot_type)listptr->data)->cards) {
		 
		 vert_offset = 0;
		 for (expanded_card_list = (((hslot_type)listptr->data)->cards); expanded_card_list; 
				expanded_card_list = expanded_card_list->next) {
			
			
			temp_card = expanded_card_list->data;
			
			/* print it the right direction */
			if (temp_card->direction == DOWN) { 
			  gdk_gc_set_clip_origin(gc,((hslot_type)listptr->data)->x,((hslot_type)listptr->data)->y + vert_offset); 
			  gdk_draw_pixmap(pix, playing_area->style->black_gc, get_card_back_pixmap(), 0,0,
									((hslot_type)listptr->data)->x, ((hslot_type)listptr->data)->y + vert_offset, -1, -1);
			}
			else {
			  tempPix = get_card_picture(temp_card->suit, temp_card->value);
			  gdk_gc_set_clip_origin(gc,((hslot_type)listptr->data)->x,((hslot_type)listptr->data)->y + vert_offset); 
			  gdk_draw_pixmap(pix, playing_area->style->black_gc, tempPix, 0,0,
									((hslot_type)listptr->data)->x, ((hslot_type)listptr->data)->y + vert_offset, -1, -1);
			}
			vert_offset += EXPANDED_VERT_OFFSET;
			
		 }
	  }
	 }
	else if (((hslot_type)listptr->data)->type == EXPANDING_SLOT_RIGHT){
	  /*...and are there cards in the slot? */
	  if (((hslot_type)listptr->data)->cards) {
		 
		 horiz_offset = 0;
		 for (expanded_card_list = (((hslot_type)listptr->data)->cards); expanded_card_list; 
				expanded_card_list = expanded_card_list->next) {
			
			
			temp_card = expanded_card_list->data;
			
			/* print it the right direction */
			if (temp_card->direction == DOWN) {
			  gdk_gc_set_clip_origin(gc,((hslot_type)listptr->data)->x + horiz_offset,((hslot_type)listptr->data)->y); 
			  gdk_draw_pixmap(pix, playing_area->style->black_gc, get_card_back_pixmap(), 0,0,
									((hslot_type)listptr->data)->x + horiz_offset, ((hslot_type)listptr->data)->y, -1, -1);
			}
			else {
			  tempPix = get_card_picture(temp_card->suit, temp_card->value);
			  gdk_gc_set_clip_origin(gc,((hslot_type)listptr->data)->x + horiz_offset,((hslot_type)listptr->data)->y); 
			  gdk_draw_pixmap(pix, playing_area->style->black_gc, tempPix, 0,0,
									((hslot_type)listptr->data)->x + horiz_offset, ((hslot_type)listptr->data)->y, -1, -1);
			}
			horiz_offset += EXPANDED_HORIZ_OFFSET;
			
		 }
	  }
	}
	 else if (((hslot_type)listptr->data)->type == PARTIALLY_EXPANDING_SLOT){
		/* hmm this is ugly.. I should be able to compact this muchly */

	  if (((hslot_type)listptr->data)->cards) {
		 
		 if (g_list_length(((hslot_type)listptr->data)->cards) <= ((hslot_type)listptr->data)->expansion_depth)
			  expanded_card_list = (((hslot_type)listptr->data)->cards);
		 else
			  expanded_card_list = g_list_nth(((hslot_type)listptr->data)->cards, 
														 g_list_length(((hslot_type)listptr->data)->cards)
														 - ((hslot_type)listptr->data)->expansion_depth);
		 vert_offset = 0;
		 for (; expanded_card_list; expanded_card_list = expanded_card_list->next) {
			
			
			temp_card = expanded_card_list->data;
			
			/* print it the right direction */
			if (temp_card->direction == DOWN) {
			  gdk_gc_set_clip_origin(gc,((hslot_type)listptr->data)->x,((hslot_type)listptr->data)->y + vert_offset); 
			  gdk_draw_pixmap(pix, playing_area->style->black_gc, get_card_back_pixmap(), 0,0,
									((hslot_type)listptr->data)->x, ((hslot_type)listptr->data)->y + vert_offset, -1, -1);
			}
			else {
			  tempPix = get_card_picture(temp_card->suit, temp_card->value);
			  gdk_gc_set_clip_origin(gc,((hslot_type)listptr->data)->x,((hslot_type)listptr->data)->y + vert_offset); 
			  gdk_draw_pixmap(pix, playing_area->style->black_gc, tempPix, 0,0,
									((hslot_type)listptr->data)->x, ((hslot_type)listptr->data)->y + vert_offset, -1, -1);
			}
			vert_offset += EXPANDED_VERT_OFFSET;
			
		 }
	  }
	 }
	else if (((hslot_type)listptr->data)->type == PARTIALLY_EXPANDING_SLOT_RIGHT){
	  /*...and are there cards in the slot? */
	  if (((hslot_type)listptr->data)->cards) {
		 
		 if (g_list_length(((hslot_type)listptr->data)->cards) <= ((hslot_type)listptr->data)->expansion_depth)
			  expanded_card_list = (((hslot_type)listptr->data)->cards);
		 else
			  expanded_card_list = g_list_nth(((hslot_type)listptr->data)->cards, 
														 g_list_length(((hslot_type)listptr->data)->cards)
														 - ((hslot_type)listptr->data)->expansion_depth);
		 horiz_offset = 0;
		 for (; expanded_card_list; expanded_card_list = expanded_card_list->next) {
			
			
			temp_card = expanded_card_list->data;
			
			/* print it the right direction */
			if (temp_card->direction == DOWN) {
			  gdk_gc_set_clip_origin(gc,((hslot_type)listptr->data)->x + horiz_offset,((hslot_type)listptr->data)->y); 
			  gdk_draw_pixmap(pix, playing_area->style->black_gc, get_card_back_pixmap(), 0,0,
									((hslot_type)listptr->data)->x + horiz_offset, ((hslot_type)listptr->data)->y, -1, -1);
			}
			else {
			  tempPix = get_card_picture(temp_card->suit, temp_card->value);
			  gdk_gc_set_clip_origin(gc,((hslot_type)listptr->data)->x + horiz_offset,((hslot_type)listptr->data)->y); 
			  gdk_draw_pixmap(pix, playing_area->style->black_gc, tempPix, 0,0,
									((hslot_type)listptr->data)->x + horiz_offset, ((hslot_type)listptr->data)->y, -1, -1);
			}
			horiz_offset += EXPANDED_HORIZ_OFFSET;
			
		 }
	  }
	}
  }
  gdk_gc_set_clip_mask(gc,NULL); 
}




void draw_normal_slot(int x, int y, GdkPixmap* pix) {
  GdkGC* gc;
#ifdef DEBUG
printf("draw_normal_slot\n");
#endif

  gc = playing_area->style->fg_gc[GTK_STATE_NORMAL];
  gdk_gc_set_clip_mask(gc,mask); 
  gdk_gc_set_clip_origin(gc,x,y);
  gdk_draw_pixmap(pix,
						playing_area->style->black_gc,
						get_slot_pixmap(),
						0,0,
						x, y,
						-1, -1);
  gdk_gc_set_clip_mask(gc,NULL); 
  return;
}

void draw_expanding_slot (int x, int y, GdkPixmap* pix) {
  GdkGC* gc;
#ifdef DEBUG
  printf("draw_expanding_slot\n");
#endif
  gc = playing_area->style->fg_gc[GTK_STATE_NORMAL];
  gdk_gc_set_clip_mask(gc,mask); 
  gdk_gc_set_clip_origin(gc,x,y);
  gdk_draw_pixmap(pix,
						playing_area->style->black_gc,
						get_slot_pixmap(),
						0,0,
						x,y,
						-1, -1);
  gdk_gc_set_clip_mask(gc,NULL); 
  return;
}

void draw_slot_placements(GdkPixmap* pix) {
  GList* listptr;
#ifdef DEBUG
printf("draw_slot_placements\n");
#endif
  for (listptr = get_slot_list(); listptr; listptr = listptr->next) {
	 if (((hslot_type)listptr->data)->type ==NORMAL_SLOT)
		draw_normal_slot(((hslot_type)listptr->data)->x, ((hslot_type)listptr->data)->y, pix);
	 else if (((hslot_type)listptr->data)->type ==EXPANDING_SLOT)
		draw_expanding_slot(((hslot_type)listptr->data)->x, ((hslot_type)listptr->data)->y, pix);
	 else if (((hslot_type)listptr->data)->type ==EXPANDING_SLOT_RIGHT)
		draw_expanding_slot(((hslot_type)listptr->data)->x, ((hslot_type)listptr->data)->y, pix);
	 else if (((hslot_type)listptr->data)->type ==PARTIALLY_EXPANDING_SLOT)
		draw_expanding_slot(((hslot_type)listptr->data)->x, ((hslot_type)listptr->data)->y, pix);
	 else if (((hslot_type)listptr->data)->type ==PARTIALLY_EXPANDING_SLOT_RIGHT)
		draw_expanding_slot(((hslot_type)listptr->data)->x, ((hslot_type)listptr->data)->y, pix);

  }
}

void take_snapshot() {
  int snapshot_width;
  int snapshot_height;
  int surface_width;
  int surface_height;
#ifdef DEBUG
printf("take_snapshot\n");
#endif

  gdk_window_get_size (surface, &surface_width, &surface_height);

  if (snapshot) {
	 gdk_window_get_size (snapshot, &snapshot_width, &snapshot_height);
	 if (!((snapshot_width == surface_width) && (snapshot_height == surface_height))) {
		gdk_pixmap_unref(snapshot);
		snapshot = gdk_pixmap_new(playing_area->window, surface_width, surface_height, gtk_widget_get_visual (playing_area)->depth);
	 }
  }
  else 
	 snapshot = gdk_pixmap_new(playing_area->window, surface_width, surface_height, gtk_widget_get_visual (playing_area)->depth);		

  gdk_draw_pixmap(snapshot, playing_area->style->black_gc,blank_surface,0,0,0,0,-1,-1);  
  draw_cards(snapshot);
}

void paint_blank_surface() 
{
  int dbp_width, dbp_height;
  int bs_width, bs_height;
  int i, j;
#ifdef DEBUG
printf("paint_blank_surface\n");
#endif

  gdk_window_get_size (get_background_pixmap(), &dbp_width, &dbp_height);
  gdk_window_get_size (blank_surface, &bs_width, &bs_height);

  /* tile the surface with the default background */
  for (i = 0; i < bs_width; i += dbp_width)
	 for (j = 0; j < bs_height; j += dbp_height)
		gdk_draw_pixmap(blank_surface,
							 playing_area->style->black_gc,
							 get_background_pixmap(),
							 0,0,
							 i,j,
							 -1, -1);
  
  
}

void refresh_screen() {
#ifdef DEBUG
printf("refresh_screen\n");
#endif
  paint_blank_surface();
  draw_slot_placements(blank_surface);
  gdk_draw_pixmap(surface,	playing_area->style->black_gc,blank_surface,0,0,0,0,-1,-1);
  draw_cards(surface);
  gdk_draw_pixmap(playing_area->window, playing_area->style->black_gc,surface,0,0,0,0,-1,-1);
}

/* show_card stuff */
void stop_show_card() {
  refresh_screen();
}
