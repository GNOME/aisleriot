/* card-draw.c --
   Copyright (C) 1998 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and'or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
   USA */

/* Written by Changwoo Ryu <cwryu@adam.kaist.ac.kr>. */

#include <sys/param.h>
#include <gtk/gtk.h>
#include <gdk/gdkprivate.h>
#include "card.h"
#include "gdk-card-image.h"

/* used for double buffering. */
static GdkPixmap *d_pixmap;
static GdkPixmap *c_pixmap;
static int card_width = 79;
static int card_height = 123;

static GtkObject* card_deck;

static void draw_card (GdkWindow *window, GdkGC *gc, int x, int y, CARD *card);
static void draw_hash(GdkWindow *window, GdkGC *gc, int x, int y);
static void draw_background(GdkWindow *window, GdkGC *gc);

#define CARD_ID(c) ((card_suit(c)-1)*13 + (card_rank(c) -1 ))
#define W_WIDTH(w) ((GdkWindowPrivate *)w)->width
#define W_HEIGHT(w) ((GdkWindowPrivate *)w)->height

int
card_image_width(void)
{
  return card_width;
}

int
card_image_height(void)
{
  return card_height;
}

int
card_image_top_height(void)
{
  return card_height/5;
}

     
static void
draw_card (GdkWindow *window, GdkGC *gc, int x, int y, CARD *card)
{
  GdkPixmap *pixmap;
  GdkBitmap *clip;

  pixmap = gdk_card_deck_face (GDK_CARD_DECK (card_deck), 
			       card_suit (card), card_rank (card));
  clip = gdk_card_deck_mask (GDK_CARD_DECK (card_deck));

  gdk_gc_set_clip_origin (gc, x, y);
  gdk_gc_set_clip_mask(gc, clip);
  gdk_draw_pixmap(window, gc,
		  pixmap,
		  0, 0,
		  x, y,
		  card_width, card_height);
  gdk_gc_set_clip_mask(gc, NULL);
}

static void
draw_hash(GdkWindow *window, GdkGC *gc, int x, int y)
{
  GdkBitmap *clip;
  int w, h;
  int i;

  clip = gdk_card_deck_mask (GDK_CARD_DECK (card_deck));
  w = card_width;
  h = card_height;

  gdk_gc_set_clip_origin (gc, x, y);
  gdk_gc_set_clip_mask(gc, clip);

  for (i = x; i < h+w; i+=8)
    {
      gdk_draw_line ((GdkDrawable *)window, gc,
		     x, y+i, x+w-1, y+i-w);
      gdk_draw_line ((GdkDrawable *)window, gc,
		     x+w-1, y+i, x, y+i-w);
    }
  
  gdk_gc_set_clip_mask(gc, NULL);
}

static void
draw_background(GdkWindow *window, GdkGC *bg_gc)
{
  gdk_draw_rectangle((GdkDrawable *)window, bg_gc, TRUE,
		     0, 0,
		     ((GdkWindowPrivate *)window)->width, 
		     ((GdkWindowPrivate *)window)->height);
}

void
card_draw_init(GtkWidget *widget)
{
  GdkPixmap *clip;

  card_deck = gdk_card_deck_new(widget->window, NULL);
  clip = gdk_card_deck_mask (GDK_CARD_DECK (card_deck));
  card_width = W_WIDTH(clip);
  card_height = W_HEIGHT(clip);
  
  d_pixmap = gdk_pixmap_new(widget->window,
			    card_width + 10, 5 * card_height,
			    -1);
  c_pixmap = gdk_pixmap_new(widget->window,
			    card_width , card_height,
			    -1);
}

static int
gap_between_cards(GtkWidget *widget, DECK *deck)
{
  int height;

  height = widget->allocation.height;

  if (deck_number(deck) > 1)
    return MIN(card_image_top_height(),
	       (height - card_image_height())/(deck_number(deck) - 1));
  else
    return card_image_top_height();
}

int
card_draw_get_index_from_deck(GtkWidget *widget, int x, int y, DECK *deck)
{
  int gap;

  gap = gap_between_cards(widget, deck);

  if (y < 0 || y > gap * deck_number(deck))
    return -1;

  if (y/gap < deck_number(deck))
    return deck_number(deck)-1-(y/gap);
  else
    return 0;
}

void
card_draw_card_general (GtkWidget *widget, int x, int y,
			      CARD *card, int is_selected)
{
  if (!card)
    {
      draw_background(widget->window, widget->style->bg_gc[GTK_STATE_NORMAL]);
      return;
    }

  draw_background((GdkWindow *)c_pixmap,
		  widget->style->bg_gc[GTK_STATE_NORMAL]);
  draw_card((GdkWindow *)c_pixmap,
	    widget->style->fg_gc[GTK_STATE_NORMAL],
	    x, y, card);

  if(is_selected)
    {
      draw_hash((GdkWindow *)c_pixmap,
		widget->style->fg_gc[GTK_STATE_NORMAL], x, y);
    }

  gdk_gc_set_clip_mask(widget->style->fg_gc[GTK_STATE_NORMAL],
		       NULL);
  gdk_window_copy_area(widget->window, widget->style->fg_gc[GTK_STATE_NORMAL],
		       0, 0,
		       (GdkWindow *)c_pixmap, 0, 0,
		       card_image_width(), card_image_height());
}


void
card_draw_deck_general(GtkWidget *widget, int x, int y,
			     DECK *deck, int is_selected, int view)
{
  CARD *card;
  int gap;
  int i;
  int ypos;
  int w, h;

  w = widget->allocation.width;
  h = widget->allocation.height;
  
  gap = gap_between_cards(widget, deck);

  draw_background((GdkWindow *)d_pixmap,
		  widget->style->bg_gc[GTK_STATE_NORMAL]);
  for (i = (deck_number(deck) - 1); i >= 0; i--)
    {
      card = deck_view(deck, i);
      ypos = (deck_number(deck) - i - 1) * gap;

      draw_card ((GdkWindow *)d_pixmap,
		 widget->style->fg_gc[GTK_STATE_NORMAL],
		 0, ypos, card);
    }

  if (is_selected)
    {
      ypos = (deck_number(deck) - 1) * gap;
      draw_hash ((GdkWindow *)d_pixmap,
		 widget->style->fg_gc[GTK_STATE_NORMAL],
		 0, ypos);
    }

  if (view >= 0)
    {
      card = deck_view(deck, view);
      ypos = (deck_number(deck) - view - 1) * gap;
      draw_card ((GdkWindow *)d_pixmap,
		 widget->style->fg_gc[GTK_STATE_NORMAL],
		 0, ypos, card);
    }

  gdk_gc_set_clip_mask(widget->style->fg_gc[GTK_STATE_NORMAL],
		       NULL);
  gdk_window_copy_area(widget->window, widget->style->fg_gc[GTK_STATE_NORMAL],
		       0, 0,
		       (GdkWindow *)d_pixmap, 0, 0, w, h);
}


		       
