/* AisleRiot - draw.c
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

#include <glib.h>
#include <math.h>

#include "draw.h"
#include "sol.h"
#include "slot.h"
#include "card.h"

/* The ratio of height to width for a card. */
#define CARD_HW_RATIO (0.65)

/* The proportion of a slot dedicated to the card (horiz or vert). */
#define CARD_SLOT_PROP (0.8)

/* The size of the drawing area. */
int window_width = 0;
int window_height = 0;

/* The size of a slot in pixels. */
static double xslotstep = 0;
static double yslotstep = 0;

/* The size of a slot in scaled units. */
static double width = 0.0;
static double height = 0.0;

/* The size of a card. */
int card_width;
int card_height;

/* The offset of the cards within the slot. */ 
int xoffset, yoffset;

static void calculate_card_location (hslot_type hslot)
{
  int xofs, yofs;

  xofs = xoffset;
  yofs = yoffset;

  /* If this is an extended slot, position the cards to one side. */
  if (hslot->dx > 0.0)
    xofs = yofs;
  if (hslot->dy > 0.0)
    yofs = xofs;

  hslot->pixelx = xslotstep*hslot->x + xofs;
  hslot->pixely = yslotstep*hslot->y + yofs;
  hslot->pixeldx = hslot->dx*card_width;
  hslot->pixeldy = hslot->compressed_dy*card_height;
 
  update_slot_length (hslot);
}

/* Work out new sizes and spacings for the cards. */
void set_geometry (double new_width, double new_height) {
  double twidth, theight;

  width = new_width;
  height = new_height;

  /* We are called for two reasons: if the logical size of the board
   * has changed and if the physical size of the board has changed.
   * This catches the case where the logical size is set before the
   * physical size. In that case we ignore anything that needs
   * knowledge of the physical size. Yes, this probably is a sign that
   * the code needs reorganising. */
  if ((window_height == 0) || (window_width == 0))
    return;

  xslotstep = window_width/width;
  yslotstep = window_height/height;

  twidth = CARD_SLOT_PROP*xslotstep;
  theight = CARD_SLOT_PROP*yslotstep;
  if (twidth/theight < CARD_HW_RATIO) {
    card_height = twidth/CARD_HW_RATIO;
    card_width = twidth;
  } else {
    card_width = CARD_HW_RATIO*theight;
    card_height = theight;
  }
  xoffset = (xslotstep - twidth)/2;
  yoffset = (yslotstep - theight)/2;

  set_card_size (card_width, card_height);

  /* Recalculate the slot locations. */
  g_list_foreach (get_slot_list (), (GFunc) calculate_card_location, NULL);
}

void rescale_cards (void) {
  set_geometry (width, height);
}

/* get the actual height of the slot */
static double get_slot_height (hslot_type hslot) {
  double cardtops_height;
  double rest_height_last_card;

  cardtops_height = hslot->pixeldy*g_list_length (hslot->cards);
  rest_height_last_card = (1-hslot->compressed_dy)*card_height;

  return cardtops_height + rest_height_last_card;
}

/* get the maximum size of the slot allowing
 * it to fit in the window */
static double get_slot_max_height (hslot_type hslot) {
  int surface_height;

  gdk_drawable_get_size(surface, NULL, &surface_height);

  return surface_height - yoffset - hslot->pixely;
}

void draw_cards () {
  GList* slot;
  gint x, y;
  GList* card_list;
  GdkPixmap *image;
  double slot_height;
  double slot_max_height;

  for (slot = get_slot_list (); slot; slot = slot->next) {
    hslot_type hslot = (hslot_type)slot->data;

    hslot->compressed_dy = hslot->dy;
    hslot->pixeldy = hslot->compressed_dy * card_height;

    slot_height = get_slot_height (hslot);
    slot_max_height = get_slot_max_height (hslot);

    /* if the slot is too high to fit, reduce dy to make it fit */
    while (slot_height > slot_max_height) {
      hslot->compressed_dy = 0.8 * hslot->compressed_dy;
      hslot->pixeldy = hslot->compressed_dy * card_height;
      slot_height = get_slot_height (hslot);
      slot_max_height = get_slot_max_height (hslot);
      /* Give up if the result would be unusably small. */
      if (hslot->pixeldy < 4) {
	hslot->pixeldy = 4;
	break;
      }
    }
    calculate_card_location (hslot);

    if ((card_list = hslot->cards)) {

      card_list = g_list_nth (card_list, hslot->length - hslot->exposed);

      x = hslot->pixelx;
      y = hslot->pixely;

      for (; card_list; card_list = card_list->next) {
	card_type *card = card_list->data;

	if (card->direction == DOWN) 
	  image = get_card_back_pixmap ();
	else 
	  image = get_card_picture (card->suit, card->value);
	
	gdk_gc_set_clip_origin (draw_gc, x, y);
	if (image != NULL)
	  gdk_draw_drawable (surface, draw_gc, image, 0, 0, x, y, -1, -1);

	x += hslot->pixeldx; y += hslot->pixeldy;
      }
    }
  }
}

void take_snapshot() {
  GList* slot;
  GdkPixmap *slot_pixmap;
  gint x,y;

  gdk_draw_rectangle (surface, bg_gc, TRUE, 0, 0, -1, -1);
  slot_pixmap = get_slot_pixmap ();

  if (slot_pixmap != NULL)
    for (slot = get_slot_list (); slot; slot = slot->next) {
      x = ((hslot_type)slot->data)->pixelx;
      y = ((hslot_type)slot->data)->pixely;
      gdk_gc_set_clip_origin (slot_gc, x, y);
      gdk_draw_drawable (surface, slot_gc, slot_pixmap, 0, 0, x, y,
			 card_width, card_height);
    }
  draw_cards ();
}

void refresh_screen () {
  take_snapshot ();
  gtk_widget_queue_draw (playing_area);
}
