/* card-pixmap.c
   Copyright (C) 1997 Ryu Changwoo

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Ryu Changwoo <cwryu@eve.kaist.ac.kr>. */

#include <config.h>

#include <gnome.h>
#include <gtk/gtk.h>
#include <gdk/gdkprivate.h>

#include <stdio.h>
#include "card.h"


static GdkPixmap *card_pixmaps[4][52];
static GdkBitmap *card_clip;
static int card_width;
static int card_height;
static int card_top_height;


typedef struct CARD_BASE
{
  int w, h;

  GdkPixmap *background;
  GdkBitmap *card_clip;

  GdkPixmap *ranks;
  GdkBitmap *ranks_clip;
  int rw, rh;
  int r1w, r1h;
  int rx, ry;

  GdkPixmap *suits;
  GdkBitmap *suits_clip;
  int sw, sh;
  int s1size, s1off;
  int s2size, s2off;
  int s3size, s3off;
  int s4size, s4off;
  int s4x, s4y;
  
  GdkPixmap *pictures;
  GdkBitmap *pictures_clip;
  int pw, ph;
  int p1w, p1h;

  int ydelta;
  int x0, x1, x2;	/* x-positions of the large suit symbols */
  int y0, y1, y2, y3, y4, y5, y6, y7, y8;	/* y-positions */
}
CARD_BASE;
CARD_BASE card_base;


static void read_base_image (GdkWindow *w, GdkColor *transcolor);
static void mirror_card (GdkGC *gc, GdkPixmap *p, int w, int h);
static void destroy_base_image(void);
static void draw_background (GdkGC *gc, GdkPixmap *p);
static void draw_rank (GdkGC *gc, GdkPixmap *p, int suit, int rank);
static void draw_small_suit (GdkGC *gc, GdkPixmap *p, int suit);
static void draw_suit (GdkGC *gc, GdkPixmap *p, int suit, int rank);
static void draw_picture (GdkGC *gc, GdkPixmap *p, int suit, int rank);

     
static void
read_base_image (GdkWindow *w, GdkColor *transcolor)
{
  char *filename;

  filename = gnome_pixmap_file ("cards/Background.xpm");
  card_base.background
    = gdk_pixmap_create_from_xpm (w, &card_base.card_clip,
				  transcolor,
				  filename);
  g_free(filename);
  card_base.w = ((GdkWindowPrivate *)card_base.background)->width;
  card_base.h = ((GdkWindowPrivate *)card_base.background)->height;
  
  filename = gnome_pixmap_file ("cards/Ranks.xpm");
  card_base.ranks
    = gdk_pixmap_create_from_xpm (w, &card_base.ranks_clip,
				  transcolor,
				  filename);
  g_free(filename);
  card_base.rw = ((GdkWindowPrivate *)card_base.ranks)->width;
  card_base.rh = ((GdkWindowPrivate *)card_base.ranks)->height;
  card_base.r1w = card_base.rw/12;
  card_base.r1h = card_base.rh/5;
  card_base.rx = 4;
  card_base.ry = 6;
  
  filename = gnome_pixmap_file ("cards/Suits.xpm");
  card_base.suits
    = gdk_pixmap_create_from_xpm (w, &card_base.suits_clip,
				  transcolor,
				  filename);
  g_free(filename);
  card_base.sw = ((GdkWindowPrivate *)card_base.suits)->width;
  card_base.sh = ((GdkWindowPrivate *)card_base.suits)->height;

  /* FIXME: Xpat's X-gfx2.c assumes width equals with height.  */
  card_base.s1size = 41;
  card_base.s1off = 0;
  card_base.s2size = 21;
  card_base.s2off = card_base.s1off + card_base.s1size;
  card_base.s3size = 15;
  card_base.s3off = card_base.s2off + card_base.s2size;
  card_base.s4size = 11;
  card_base.s4off = card_base.s3off + card_base.s3size;

  filename = gnome_pixmap_file ("cards/Pictures.xpm");
  card_base.pictures
    = gdk_pixmap_create_from_xpm (w, &card_base.pictures_clip,
				  transcolor,
				  filename);
  g_free(filename);
  card_base.pw = ((GdkWindowPrivate *)card_base.pictures)->width;
  card_base.ph = ((GdkWindowPrivate *)card_base.pictures)->height;
  card_base.p1w = card_base.pw/4;
  card_base.p1h = card_base.ph/3;

  card_base.x0 = (card_base.w - 1 * card_base.s2size) / 2
    - 2 * card_base.w / 9 + 1;
  card_base.x1 = (card_base.w - 1 * card_base.s2size) / 2;
  card_base.x2 = (card_base.w - 1 * card_base.s2size) / 2
    + 2 * card_base.w / 9 - 1;

  card_base.ydelta = (((card_base.h - 2 - 4 * card_base.s2size) / 7) | 1);

  card_base.y0 = (card_base.h - 3 * card_base.ydelta
		  - 4 * card_base.s2size) / 2;
  card_base.y1 = (card_base.h - 2 * card_base.ydelta
		  - 3 * card_base.s2size) / 2;
  card_base.y2 = (card_base.h - 1 * card_base.ydelta
		  - 2 * card_base.s2size) / 2;
  card_base.y3 = (card_base.h - 0 * card_base.ydelta
		  - 1 * card_base.s2size) / 2;
  card_base.y4 = (card_base.h + 1 * card_base.ydelta
		  - 0 * card_base.s2size) / 2;
  card_base.y5 = (card_base.h + 2 * card_base.ydelta
		  + 1 * card_base.s2size) / 2;
  card_base.y6 = (card_base.h + 3 * card_base.ydelta
		  + 2 * card_base.s2size) / 2;
  card_base.y7 = (card_base.y0 + card_base.y3) / 2;
  card_base.y8 = (card_base.y6 + card_base.y3) / 2;

  card_base.s4x = 3;
  card_base.s4y = card_base.y0 + card_base.s2size - card_base.s4size;
}


static void
destroy_base_image(void)
{
  /* not destroy card_clip which is used in drawing.  */

  gdk_pixmap_unref (card_base.background);
  gdk_pixmap_unref (card_base.ranks);
  gdk_pixmap_unref ((GdkPixmap *)card_base.ranks_clip);
  gdk_pixmap_unref (card_base.suits);
  gdk_pixmap_unref ((GdkPixmap *)card_base.suits_clip);
  gdk_pixmap_unref (card_base.pictures);
  gdk_pixmap_unref ((GdkPixmap *)card_base.pictures_clip);
}

static void
draw_background (GdkGC *gc, GdkPixmap *p)
{
  gdk_window_copy_area((GdkWindow *)p,
		       gc,
		       0, 0,
		       (GdkWindow *)card_base.background,
		       0, 0, card_base.w, card_base.h);
}


static void
draw_rank (GdkGC *gc, GdkPixmap *p, int suit, int rank)
{
  int x, y, dl, cx, cy;

  x = 3 * (suit/2) * card_base.r1w + (rank / 5) * card_base.r1w;
  y = (rank % 5) * card_base.r1h;

  gdk_gc_set_clip_mask(gc,
		       card_base.ranks_clip);

  gdk_gc_set_clip_origin(gc, card_base.rx-x, card_base.ry-y);
  gdk_window_copy_area((GdkWindow *)p, gc,
		       card_base.rx, card_base.ry,
		       (GdkWindow *)card_base.ranks,
		       x, y, card_base.r1w, card_base.r1h);
  
  x = (8 + 3 * (suit/2) - (rank / 5)) * card_base.r1w;
  y = (4 - rank % 5) * card_base.r1h;
  dl = card_base.w - card_base.r1w;
  cx = dl-card_base.rx;
  cy = card_base.h - card_base.ry - card_base.r1h;

  gdk_gc_set_clip_origin(gc,
			 cx-x, cy-y);
  gdk_window_copy_area((GdkWindow *)p, gc,
		       cx, cy,
		       (GdkWindow *)card_base.ranks,
		       x, y, card_base.r1w, card_base.r1h);
  gdk_gc_set_clip_mask(gc, NULL);
}

static void
draw_small_suit (GdkGC *gc, GdkPixmap *p, int suit)
{
  int x, y, dl, cx, cy;

  x = card_base.s4x;
  y = card_base.s4y;

  gdk_gc_set_clip_mask(gc,
		       card_base.suits_clip);
  gdk_gc_set_clip_origin(gc, x-suit*card_base.s4size, y-card_base.s4off);
  gdk_window_copy_area((GdkWindow *)p, gc,
		       x, y,
		       (GdkWindow *)card_base.suits,
		       suit * card_base.s4size,
		       card_base.s4off,
		       card_base.s4size, card_base.s4size);

  x = (suit+4) * card_base.s4size;
  y = card_base.s4off;
  dl = card_base.w - card_base.s4size;
  cx = dl-card_base.s4x;
  cy = card_base.h - card_base.s4size - card_base.s4y;

  gdk_gc_set_clip_origin(gc, cx-x, cy-y);
  gdk_window_copy_area((GdkWindow *)p, gc,
		       cx, cy,
		       (GdkWindow *)card_base.suits,
		       x, y, card_base.s4size, card_base.s4size);
  gdk_gc_set_clip_mask(gc, NULL);
}

#define PAINT_AT(xx, yy) \
{ \
  gdk_gc_set_clip_origin(gc, (xx)-x, (yy)-y); \
  gdk_window_copy_area((GdkWindow *)p, gc, (xx), (yy), \
		       (GdkWindow *)card_base.suits, x, y, \
		       card_base.s2size, card_base.s2size); \
}							    
		       

static void
draw_suit (GdkGC *gc, GdkPixmap *p, int suit, int rank)
{
  static const unsigned char suitflags[] =
  {  /* 25 => 03 for different 8 */
    0x80, 0x08, 0x88, 0x01, 0x81, 0x05, 0x45, 0x25, 0x83, 0x13, 0, 0, 0, 0
  };
  int x, y;

  x = suit * card_base.s2size;
  y = card_base.s2off;

  gdk_gc_set_clip_mask(gc, card_base.suits_clip);

  if (suitflags[rank] & 0x01)
    {
      PAINT_AT(card_base.x0, card_base.y0);
      PAINT_AT(card_base.x2, card_base.y0);
    }
  if (suitflags[rank] & 0x02)
    {
      PAINT_AT(card_base.x0, card_base.y2);
      PAINT_AT(card_base.x2, card_base.y2);
    }
  if (suitflags[rank] & 0x08)
    {
      PAINT_AT(card_base.x1, card_base.y0);
    }
  if (suitflags[rank] & 0x10)
    {
      PAINT_AT(card_base.x1, card_base.y1);
    }
  if (suitflags[rank] & 0x20)
    {
      PAINT_AT(card_base.x1, card_base.y7);
    }
  gdk_gc_set_clip_mask(gc, NULL);
  mirror_card (gc, p, card_base.w, card_base.h);
  gdk_gc_set_clip_mask(gc, card_base.suits_clip);
  if (suitflags[rank] & 0x04)
    {
      PAINT_AT(card_base.x0, card_base.y3);
      PAINT_AT(card_base.x2, card_base.y3);
    }
  if (suitflags[rank] & 0x40)
    {
      PAINT_AT(card_base.x1, card_base.y7);
    }
  if (suitflags[rank] & 0x80)
    {
      PAINT_AT(card_base.x1, card_base.y3);
    }
  gdk_gc_set_clip_mask(gc, NULL);
}

static void
mirror_card (GdkGC *gc, GdkPixmap *p, int w, int h)
{
  int x, y;
  int H = h/2;

  /* first, build a left-right mirror */
  for (x = 0; x < w; ++x)
    gdk_window_copy_area((GdkWindow *)p, gc, w-1-x, H+2,
			 (GdkWindow *)p, x, 1, 1, H-1);

  /* now mirror the lower half of the card upside-down */
  for (y = 0; y < H/2; ++y)
    {
      gdk_window_copy_area((GdkWindow *)p, gc, 0, H+1+y,
			   (GdkWindow *)p, 0, h-1-y, w, 1);
      gdk_window_copy_area((GdkWindow *)p, gc, 0, h-1-y,
			   (GdkWindow *)p, 0, H+2+y, w, 1);
    }
  
  /* shift one scan line */
  gdk_window_copy_area((GdkWindow *)p, gc, 0, H+1+H/2,
		       (GdkWindow *)p, 0, H+2+H/2, w, H/2);
    
  /* restore the bottom line which was overwritten */
  gdk_window_copy_area((GdkWindow *)p, gc, 0, h-1,
		       (GdkWindow *)p, 0, 0, w, 1);
}


static void
draw_picture (GdkGC *gc, GdkPixmap *p, int suit, int rank)
{
  int x, y;

  x = (card_base.w - card_base.p1w)/2;
  y = card_base.h/2 - card_base.p1h;

  /* horizontal line in the midst of the card: */
  gdk_draw_line((GdkDrawable *)p, gc, x, card_base.h/2,
		card_base.w-1-x, card_base.h/2);
  
  gdk_gc_set_clip_mask(gc, card_base.pictures_clip);

  gdk_gc_set_clip_origin (gc,
			  x-(suit*card_base.p1w), y-((rank-10)*card_base.p1h));
  gdk_window_copy_area ((GdkWindow *)p, gc, x, y,
			(GdkWindow *)card_base.pictures,
			suit*card_base.p1w, (rank-10)*card_base.p1h,
			card_base.p1w, card_base.p1h);
  gdk_gc_set_clip_mask(gc, NULL);

  mirror_card (gc, p, card_base.w, card_base.h);
}


void
card_image_init (GtkWidget *w)
{
  GdkPixmap *pixmap;
  GdkGC *gc;

  int i, j;
  
  read_base_image (w->window, &w->style->bg[GTK_STATE_NORMAL]);
  gc = w->style->fg_gc[GTK_STATE_NORMAL];
  
  for (i = 0; i < 4; i++)
    for (j = 0; j < 13; j++)
      {
	pixmap = gdk_pixmap_new (w->window,
				 card_base.w, card_base.h,
				 -1);

	draw_background(gc, pixmap);
	draw_rank(gc, pixmap, i, j);
	draw_small_suit(gc, pixmap, i);

	if ((j >= 0) && (j <= 9))
	  draw_suit(gc, pixmap, i, j);
	else
	  draw_picture(gc, pixmap, i, j);
	  
	card_pixmaps[i][j] = pixmap;
      }
  card_clip = card_base.card_clip;
  card_width = card_base.w;
  card_height = card_base.h;
  card_top_height = 30;
  
  destroy_base_image();
}

GdkPixmap *
card_image (CARD *card)
{
  return card_pixmaps[card_suit(card)-1][card_rank(card)-1];
}

GdkBitmap *
card_image_clip (void)
{
  return card_clip;
}

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
  return card_top_height;
}


#ifdef TEST
#include <gtk/gtk.h>

int
main (int argc, char **argv)
{
  GtkWidget *window;
  GtkWidget *pixmap;
  GdkPixmap *p;
  int i, j;

  GdkBitmap *mask;
  GdkColor transcolor;
  CARD *card;

  gtk_init (&argc, &argv);

  i = atoi(argv[1]);
  j = atoi(argv[2]);

  /* create a new window */
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_widget_realize(window);

  card_image_init(window);
  card = card_new (i, j);
  p = card_image(card);

  pixmap = gtk_pixmap_new (p, NULL);
  gtk_container_add (GTK_CONTAINER (window), pixmap);

  gtk_widget_show(pixmap);
  gtk_widget_draw_default(pixmap);
  gtk_widget_show(window);

  gtk_main();

}
#endif /* TEST */
