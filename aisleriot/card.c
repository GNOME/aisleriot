/* AisleRiot - card.c
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

#include "sol.h"
#include "card.h"
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <games-card-images.h>

GdkPixmap *default_background_pixmap; 
GdkPixbuf *slot_pixbuf;
GdkPixmap *slot_pixmap = NULL;
GdkBitmap *mask = NULL;
GdkBitmap *slot_mask = NULL;

GamesCardImages * images = NULL;
GdkPixmap * pixmaps[GAMES_CARDS_TOTAL];

GdkPixmap* get_card_picture (gint suit, gint rank ) 
{
  return pixmaps[GAMES_CARD_ID (suit, rank)];
}

GdkPixmap* get_background_pixmap () {
  
  return default_background_pixmap;
}

GdkPixmap* get_slot_pixmap () {
  return slot_pixmap;
}

GdkPixmap* get_card_back_pixmap () {
  return pixmaps[GAMES_CARD_BACK];
}

static GdkPixmap* get_pixmap (const char* filename)
{
  GdkPixmap* ret;
  GdkPixbuf *im;
  char* fullname = gnome_program_locate_file (NULL,
		  GNOME_FILE_DOMAIN_APP_PIXMAP,
		  filename, TRUE, NULL);

  if (fullname == NULL)
    return NULL; 

  im = gdk_pixbuf_new_from_file (fullname, NULL);
  if (im != NULL) {
    gdk_pixbuf_render_pixmap_and_mask (im, &ret, NULL, 127);
    gdk_pixbuf_unref (im);
  } else {
    ret = NULL;
  } 
  g_free (fullname);

  return ret;
}

static GdkPixbuf* get_pixbuf (const char* filename)
{
  GdkPixbuf *im;
  char* fullname = gnome_program_locate_file (NULL,
                                              GNOME_FILE_DOMAIN_APP_PIXMAP,
                                              filename, TRUE, NULL);

  if (fullname == NULL)
    return NULL; 

  im = gdk_pixbuf_new_from_file (fullname, NULL);
  g_free (fullname);

  return im;
}

void load_pixmaps (GtkWidget* app, GdkCardDeckOptions deck_options) 
{
  card_deck = gdk_card_deck_new (app->window, deck_options);
  if (card_deck == NULL) {
    GtkWidget * error_dialog;
    
    error_dialog = gtk_message_dialog_new(GTK_WINDOW (app), GTK_DIALOG_MODAL,
                                          GTK_MESSAGE_ERROR,
                                          GTK_BUTTONS_CLOSE,
                                          N_("AisleRiot could not load its deck images.\n\nAisleRiot will now exit."));
    gtk_dialog_run (GTK_DIALOG (error_dialog));
    gtk_widget_destroy(error_dialog);
    exit(1);
  }
  
  mask = gdk_card_deck_mask (GDK_CARD_DECK (card_deck)); 
  slot_pixbuf = get_pixbuf ("cards/slots/plain.png");
  default_background_pixmap = get_pixmap ("cards/baize.png");
}

void free_pixmaps () 
{
  gtk_object_destroy (card_deck);
  if (slot_pixbuf != NULL)
    gdk_pixbuf_unref (slot_pixbuf);
  if (default_background_pixmap != NULL)
    g_object_unref (default_background_pixmap);
}

void add_card(GList** card_list, hcard_type temp_card) {
  if (*card_list)
	 g_list_append(*card_list, temp_card);
  else {
	 *card_list = g_list_alloc();
	 (*card_list)->data = temp_card;
  }
}

void set_card_size (gint width, gint height)
{
  GdkPixbuf *scaled;
  int i;

  if (slot_pixmap)
    g_object_unref (slot_pixmap);
  if (mask)
    g_object_unref (mask);
  if (slot_mask)
    g_object_unref (slot_mask);

  scaled = gdk_pixbuf_scale_simple (slot_pixbuf, width, height, 
				    GDK_INTERP_BILINEAR);

  gdk_pixbuf_render_pixmap_and_mask (scaled, &slot_pixmap, &slot_mask, 255);

  gdk_gc_set_clip_mask (slot_gc, slot_mask); 

  g_object_unref (scaled);

  if (!images) {
    images = games_card_images_new ();
    games_card_images_set_size (images, width, height);
    games_card_images_set_theme (images, "dondorf-new.png"); 
  } else {
    for (i=0; i<GAMES_CARDS_TOTAL; i++)
      g_object_unref (pixmaps[i]);
    games_card_images_set_size (images, width, height);
  }

  gdk_pixbuf_render_pixmap_and_mask (games_card_images_get_card_by_id (images, 0),
				     &pixmaps[0], &mask, 255);
  gdk_gc_set_clip_mask (draw_gc, mask);  

  for (i=1; i<GAMES_CARDS_TOTAL; i++) {
    pixmaps[i] = gdk_pixmap_new (playing_area->window, width, height, -1);
    gdk_draw_pixbuf (pixmaps[i], NULL, 
		     games_card_images_get_card_by_id (images, i),
		     0, 0, 0, 0, width, height, GDK_RGB_DITHER_NORMAL, 0, 0);
  }
  
}
