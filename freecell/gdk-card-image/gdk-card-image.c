/* card-image.c
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

#include <gdk/gdk.h>
#include <gdk_imlib.h>

#include <string.h>
#include <stdio.h>

static GdkPixmap **faces[4];
static GdkPixmap *back;

typedef struct CARD_BASE
{
  GdkPixmap *background;
  GdkBitmap *card_clip;
  int w, h;

  GdkPixmap *back;
  GdkBitmap *back_clip;
  int bw, bh;

  GdkPixmap *ranks;
  GdkBitmap *ranks_clip;
  int rw, rh;
  int r1w, r1h;
  int rx, ry;

  GdkPixmap *suits;
  GdkBitmap *suits_clip;
  int sw, sh;
  int s1w, s1h, s1off;
  int s2w, s2h, s2off;
  int s3w, s3h, s3off;
  int s4w, s4h, s4off;
  int s4x, s4y;
  
  GdkPixmap *pictures;
  GdkBitmap *pictures_clip;
  int pw, ph;
  int p1w, p1h;

  int xdelta;
  int ydelta;
  int x0, x1, x2;	/* x-positions of the large suit symbols */
  int y0, y1, y2, y3, y4, y5, y6, y7, y8;	/* y-positions */
}
CARD_BASE;
static CARD_BASE card_base;

static void read_base_image ();
static void mirror_image (GdkGC *gc, GdkPixmap *p, int x, int y, int w, int h);
static void draw_background (GdkGC *gc, GdkPixmap *p);
static void draw_rank (GdkGC *gc, GdkPixmap *p, int suit, int rank);
static void draw_small_suit (GdkGC *gc, GdkPixmap *p, int suit);
static void draw_suit (GdkGC *gc, GdkPixmap *p, int suit, int rank);
static void draw_picture (GdkGC *gc, GdkPixmap *p, int suit, int rank);
static char *image_filename (char *basename);
static GdkPixmap *load_image (GdkBitmap **mask, char *filename);

  /* FIX ME: CARDIMAGEDIR should be passed as a parameter
   * OR the images should be hardcoded into the library. Hard coding
   * a directory is absurd. Linking libgnome is not much better */
static char *
image_filename (char *basename)
{
  char *str;

  str = (char *)g_malloc(strlen(CARDIMAGEDIR) + 1 + strlen(basename) + 1);
  sprintf (str, "%s/%s", CARDIMAGEDIR, basename);
  return str;
}

static GdkPixmap* 
load_image (GdkBitmap** mask, char* filename)
{
  GdkPixmap* ret;
  GdkImlibImage *im;
  char* fullname = image_filename (filename);

  im = gdk_imlib_load_image (fullname);
  gdk_imlib_render (im, im->rgb_width, im->rgb_height);
  ret = gdk_imlib_copy_image (im);
  if(mask)
    *mask = gdk_imlib_copy_mask (im);
  gdk_imlib_destroy_image (im);
  g_free (fullname);

  return ret;
}


static void
read_base_image ()
{
  char *filename;

  card_base.ranks = load_image (&card_base.ranks_clip, "Ranks.xpm");

  gdk_window_get_size (card_base.ranks, &card_base.rw, &card_base.rh);
  card_base.r1w = card_base.rw/13;
  card_base.r1h = card_base.rh/4;
  
  card_base.background = load_image (&card_base.card_clip, "Background.xpm");

  gdk_window_get_size (card_base.background, &card_base.w, &card_base.h);

  card_base.back = load_image (&card_base.back_clip, "Cardback1.xpm");

  gdk_window_get_size (card_base.back, &card_base.bw, &card_base.bh);

  card_base.suits = load_image (&card_base.suits_clip, "Suits.xpm");

  gdk_window_get_size (card_base.suits, &card_base.sw, &card_base.sh);

  card_base.s1w = 21;
  card_base.s1h = 25;
  card_base.s1off = 0;
  card_base.s2w = 18;
  card_base.s2h = 21;
  card_base.s2off = card_base.s1off + card_base.s1h;
  card_base.s3w = 15;
  card_base.s3h = 19;
  card_base.s3off = card_base.s2off + card_base.s2h;
  card_base.s4w = 9;
  card_base.s4h = 10;
  card_base.s4off = card_base.s3off + card_base.s3h;

  card_base.pictures = load_image (&card_base.pictures_clip, "Pictures.xpm");

  gdk_window_get_size (card_base.pictures, &card_base.pw, &card_base.ph);
  card_base.p1w = card_base.pw/4;
  card_base.p1h = card_base.ph/3;

  card_base.xdelta = card_base.w / 5;
  card_base.ydelta = card_base.h / 10;  

  card_base.x1 = (card_base.w - card_base.s2w) / 2;
  card_base.y3 = (card_base.h - card_base.s2h) / 2;

  card_base.x0 = card_base.x1 - card_base.xdelta; 
  card_base.x2 = card_base.x1 + card_base.xdelta; 

  card_base.y0 = card_base.y3 - 3 * card_base.ydelta;
  card_base.y1 = card_base.y3 - 2 * card_base.ydelta;
  card_base.y2 = card_base.y3 - 1 * card_base.ydelta;
  card_base.y4 = card_base.y3 + 1 * card_base.ydelta;
  card_base.y5 = card_base.y3 + 2 * card_base.ydelta;
  card_base.y6 = card_base.y3 + 3 * card_base.ydelta;

  card_base.y7 = card_base.y3 - (3 * card_base.ydelta) / 2;
  card_base.y8 = card_base.y3 + (3 * card_base.ydelta) / 2;

  card_base.s4x = card_base.x0 + (card_base.s2w - card_base.s4w) / 2
    - card_base.xdelta; 
  card_base.s4y = card_base.y0 + (card_base.s2h - card_base.s4h) / 2;
  card_base.rx = card_base.s4x + (card_base.s4w - card_base.r1w) / 2;
  card_base.ry = card_base.s4y - card_base.r1h - 1;
  if (card_base.ry < 6) {
    card_base.ry = 6;
    card_base.s4y = card_base.ry + card_base.r1h + 1;
  }
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
  int x, y, cx, cy;

  cx = card_base.w - card_base.rx - card_base.r1w;
  cy = card_base.h - card_base.ry - card_base.r1h;

  x = rank * card_base.r1w;
  y = ((suit >> 1) ^ (suit & 1)) * card_base.r1h;

  gdk_gc_set_clip_mask(gc, card_base.ranks_clip);

  gdk_gc_set_clip_origin(gc, card_base.rx-x, card_base.ry-y);
  gdk_window_copy_area((GdkWindow *)p, gc,
		       card_base.rx, card_base.ry,
		       (GdkWindow *)card_base.ranks,
		       x, y, card_base.r1w, card_base.r1h);
  
  gdk_gc_set_clip_origin(gc, cx-x, card_base.ry-y);
  gdk_window_copy_area((GdkWindow *)p, gc,
		       cx, card_base.ry,
		       (GdkWindow *)card_base.ranks,
		       x, y, card_base.r1w, card_base.r1h);

  x = card_base.rw - card_base.r1w - x;
  y = card_base.rh - card_base.r1h - y;

  gdk_gc_set_clip_origin(gc, card_base.rx-x, cy-y);
  gdk_window_copy_area((GdkWindow *)p, gc,
		       card_base.rx, cy,
		       (GdkWindow *)card_base.ranks,
		       x, y, card_base.r1w, card_base.r1h);

  gdk_gc_set_clip_origin(gc, cx-x, cy-y);
  gdk_window_copy_area((GdkWindow *)p, gc,
		       cx, cy,
		       (GdkWindow *)card_base.ranks,
		       x, y, card_base.r1w, card_base.r1h);

  gdk_gc_set_clip_mask(gc, NULL);
}

static void
draw_small_suit (GdkGC *gc, GdkPixmap *p, int suit)
{
  int x, y, cx, cy;

  cx = card_base.w - card_base.s4w - card_base.s4x;
  cy = card_base.h - card_base.s4h - card_base.s4y;

  x = suit*card_base.s4w;
  y = card_base.s4off;

  gdk_gc_set_clip_mask(gc, card_base.suits_clip);

  gdk_gc_set_clip_origin(gc, card_base.s4x-x, card_base.s4y-y);
  gdk_window_copy_area((GdkWindow *)p, gc,
		       card_base.s4x, card_base.s4y,
		       (GdkWindow *)card_base.suits,
		       x, y, card_base.s4w, card_base.s4h);

  gdk_gc_set_clip_origin(gc, cx-x, card_base.s4y-y);
  gdk_window_copy_area((GdkWindow *)p, gc,
		       cx, card_base.s4y,
		       (GdkWindow *)card_base.suits,
		       x, y, card_base.s4w, card_base.s4h);

  x = x + 4 * card_base.s4w;

  gdk_gc_set_clip_origin(gc, cx-x, cy-y);
  gdk_window_copy_area((GdkWindow *)p, gc,
		       cx, cy,
		       (GdkWindow *)card_base.suits,
		       x, y, card_base.s4w, card_base.s4h);

  gdk_gc_set_clip_origin(gc, card_base.s4x-x, cy-y);
  gdk_window_copy_area((GdkWindow *)p, gc,
		       card_base.s4x, cy,
		       (GdkWindow *)card_base.suits,
		       x, y, card_base.s4w, card_base.s4h);

  gdk_gc_set_clip_mask(gc, NULL);
}

#define PAINT_AT(xx, yy) \
{ \
  gdk_gc_set_clip_origin(gc, (xx)-x, (yy)-y); \
  gdk_window_copy_area((GdkWindow *)p, gc, (xx), (yy), \
		       (GdkWindow *)card_base.suits, x, y, \
		       card_base.s2w, card_base.s2h); \
}							    
#define PAINT_RV(xx, yy) \
{ \
  gdk_gc_set_clip_origin(gc, (xx)-x, (yy)-y); \
  gdk_window_copy_area((GdkWindow *)p, gc, (xx), (yy), \
		       (GdkWindow *)card_base.suits, x, y, \
		       card_base.s2w, card_base.s2h); \
}							    
		       

static void
draw_suit (GdkGC *gc, GdkPixmap *p, int suit, int rank)
{
  static const unsigned char suitflags[] =
  {  /* 25 => 03 for different 8 */
    0x80, 0x08, 0x88, 0x01, 0x81, 0x05, 0x45, 0x25, 0x83, 0x13, 0, 0, 0, 0
  };
  int x, y;

  x = suit * card_base.s2w;
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
  /*  mirror_image (gc, p, 0, 0, card_base.w, card_base.h); */
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

  /* draw all symbols which are upside-down */
  x = (suit+4) * card_base.s2w;
  if (suitflags[rank] & 0x01) {
    PAINT_RV(card_base.x0, card_base.y6);
    PAINT_RV(card_base.x2, card_base.y6);
  }
  if (suitflags[rank] & 0x02) {
    PAINT_RV(card_base.x0, card_base.y4);
    PAINT_RV(card_base.x2, card_base.y4);
  }
  if (suitflags[rank] & 0x08) {
    PAINT_RV(card_base.x1, card_base.y6);
  }
  if (suitflags[rank] & 0x10) {
    PAINT_RV(card_base.x1, card_base.y5);
  }
  if (suitflags[rank] & 0x20) {
    PAINT_RV(card_base.x1, card_base.y8);
  }
  
  gdk_gc_set_clip_mask(gc, NULL);
}

static void
mirror_image (GdkGC *gc, GdkPixmap *p, int x, int y, int w, int h)
{
  int xi, yi;
  int H = h/2;

  /* first, build a left-right mirror */
  for (xi = x; xi < x+w; ++xi)
    gdk_window_copy_area((GdkWindow *)p, gc, (x+w)-1-xi, H+2,
			 (GdkWindow *)p, xi, y+1, 1, H-1);

  /* now mirror the lower half of the card upside-down */
  for (yi = 0; yi < H/2; ++yi)
    {
      gdk_window_copy_area((GdkWindow *)p, gc, 0, H+1+yi,
			   (GdkWindow *)p, 0, h-1-yi, w, 1);
      gdk_window_copy_area((GdkWindow *)p, gc, 0, h-1-yi,
			   (GdkWindow *)p, 0, H+2+yi, w, 1);
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
  y = (card_base.h + 1)/2 - card_base.p1h;

  gdk_gc_set_clip_mask(gc, card_base.pictures_clip);

  gdk_gc_set_clip_origin (gc,
			  x-(suit*card_base.p1w), y-((rank-10)*card_base.p1h));
  gdk_window_copy_area ((GdkWindow *)p, gc, x, y,
			(GdkWindow *)card_base.pictures,
			suit*card_base.p1w, (rank-10)*card_base.p1h,
			card_base.p1w, card_base.p1h);
  gdk_gc_set_clip_mask(gc, NULL);

  mirror_image (gc, p, 0, 0, card_base.w, card_base.h);
}

void
gdk_card_image_init (GdkWindow *window)
{
  GdkPixmap * pixmap;
  GdkGC *gc;
  int i, j;
  
  /* This costs nothing when imlib has already been initialized and
     makes the library easier to use: */
  gdk_imlib_init ();

  read_base_image ();

  gc = gdk_gc_new (card_base.background);

  for (i = 0; i < 4; i++) {
    faces[i] = (GdkPixmap**) calloc(13, sizeof(GdkPixmap*));

    for (j = 0; j < 13; j++) {
      pixmap =
	gdk_pixmap_new (window, card_base.w, card_base.h, -1);

      draw_background(gc, pixmap);
      draw_rank(gc, pixmap, i, j);
      draw_small_suit(gc, pixmap, i);
      
      if (j == 0) {
	int x = i * card_base.s1w;
	int y = card_base.s1off;
	
	int xx = (card_base.w - card_base.s1w) / 2;
	int yy = (card_base.h - card_base.s1h) / 2;
	
	gdk_gc_set_clip_mask(gc, card_base.suits_clip);
	gdk_gc_set_clip_origin(gc, (xx)-x, (yy)-y);
	gdk_window_copy_area((GdkWindow *)pixmap, gc, (xx), (yy),
			     (GdkWindow *)card_base.suits, x, y,
			     card_base.s1w, card_base.s1h);
	gdk_gc_set_clip_mask(gc, NULL);
      }
      else if (j <= 9)
	draw_suit(gc, pixmap, i, j);
      else
	draw_picture(gc, pixmap, i, j);
	  
      faces[i][j] = pixmap;
    }
  }

  {
    int xx = (card_base.w - card_base.bw) / 2;
    int yy = (card_base.h - card_base.bh) / 2;

    back = gdk_pixmap_new (window, card_base.w, card_base.h, -1);
    draw_background(gc, back);
    gdk_gc_set_clip_mask(gc, card_base.back_clip);
    gdk_gc_set_clip_origin(gc, (xx), (yy));
    gdk_window_copy_area((GdkWindow *)back , gc, (xx), (yy),
			 (GdkWindow *)card_base.back, 0, 0,
			 card_base.bw, card_base.bh);
    gdk_gc_set_clip_mask(gc, NULL);  
  }

  gdk_gc_unref(gc);
}

GdkPixmap*
gdk_card_image_face (int suit, int value)
{
  return faces[suit][value];
}

GdkPixmap*
gdk_card_image_back ()
{
  return back;
}

GdkBitmap*
gdk_card_image_mask ()
{
  return card_base.card_clip;
}

void
gdk_card_image_unref ()
{
  int i, j;
  
  gdk_pixmap_unref (card_base.ranks);
  gdk_bitmap_unref (card_base.ranks_clip);
  gdk_pixmap_unref (card_base.background);
  gdk_bitmap_unref (card_base.card_clip);
  gdk_pixmap_unref (card_base.suits);
  gdk_bitmap_unref (card_base.suits_clip);
  gdk_pixmap_unref (card_base.pictures);
  gdk_bitmap_unref (card_base.pictures_clip);

  for (i = 0; i < 4; i++)
    for (j = 0; j < 13; j++)
      gdk_pixmap_unref (faces[i][j]);

  gdk_pixmap_unref (card_base.back);
  gdk_bitmap_unref (card_base.back_clip);
}

/* Obsolete */
void
gdk_card_image (int old_id, GdkPixmap **pixmap, GdkBitmap **clip)
{
  if (pixmap)
    *pixmap = faces[(4-(old_id/13))%4][old_id%13];
  if (clip)
    *clip = card_base.card_clip;
}

#ifdef TEST
#include <gtk/gtk.h>

int
main (int argc, char **argv)
{
  GtkWidget *window;
  GtkWidget *pixmap;
  GdkPixmap *p;
  GdkBitmap *mask;
  int i;

  gtk_init (&argc, &argv);

  i = atoi(argv[1]);

  /* create a new window */
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_widget_realize(window);

  card_image_init(window->window); 
  
  card_image(i, &p, &mask); 

  pixmap = gtk_pixmap_new (p, mask);
  gtk_container_add (GTK_CONTAINER (window), pixmap);

  gtk_widget_show(pixmap);
  gtk_widget_draw_default(pixmap);
  gtk_widget_show(window);

  gtk_main();

  return 0;
}
#endif /* TEST */
