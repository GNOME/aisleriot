/* gdk-card-image.h
   Copyright 1998 Free Software Foundation, Inc.

   This library is free software; you can redistribute it and'or modify
   it under the terms of the GNU Library General Public License as published 
   by the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Authors:   Felix Bellaby <felix@pooh.u-net.com>
              Ryu Changwoo <cwryu@eve.kaist.ac.kr> */

/* NB: You must initialize the gnome libraries before using this library. */

#ifndef __GDK_CARD_IMAGE_H
#define __GDK_CARD_IMAGE_H

#include <gtk/gtk.h>

typedef enum
{
  CLUB  =  0, DIAMOND, HEART, SPADE
} GdkCardSuit;

typedef enum
{
  JOKER =  0, 
  ACE   =  1, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN,
  JACK  = 11, QUEEN, KING
} GdkCardRank;

/* The user options supplied to the library to build the deck:
 * These remain opaque to an application using the deck. */
typedef gchar* GdkCardDeckOptions;

/* This GtkObject adds pages to a GtkNotebook to allow 
 * the user to edit a GdkCardDeckOptions value:  
 * A "changed" signal is emitted when the user changes an option */
typedef struct _GdkCardDeckOptionsEdit GdkCardDeckOptionsEdit;
typedef struct _GdkCardDeckOptionsEditClass GdkCardDeckOptionsEditClass;

guint gdk_card_deck_options_edit_get_type  (void);

#define GDK_CARD_DECK_OPTIONS_EDIT(obj)         GTK_CHECK_CAST (obj, \
   gdk_card_deck_options_edit_get_type (), GdkCardDeckOptionsEdit)
#define GDK_CARD_DECK_OPTIONS_EDIT_CLASS(klass) GTK_CHECK_CLASS_CAST (klass, \
   gdk_card_deck_options_edit_get_type (), GdkCardDeckOptionsEditClass)
#define GDK_IS_CARD_DECK_OPTIONS_EDIT(obj)      GTK_CHECK_TYPE (obj, \
   gdk_card_deck_options_edit_get_type ())

/* Add the edit pages to the notebook */
GtkObject*    gdk_card_deck_options_edit_new (GtkNotebook* notebook);

/* Set/Get the currently displayed deck options */
void          gdk_card_deck_options_edit_set (GdkCardDeckOptionsEdit* w,
					      GdkCardDeckOptions deck_options);
GdkCardDeckOptions gdk_card_deck_options_edit_get (GdkCardDeckOptionsEdit* w);

/* True when the user has made changes since the options were last set */
gboolean      gdk_card_deck_options_edit_dirty (GdkCardDeckOptionsEdit* w);

/* This GdkObject contains GdkPixmaps representing a deck of cards */
typedef struct _GdkCardDeck GdkCardDeck;
typedef struct _GdkCardDeckClass GdkCardDeckClass;

guint gdk_card_deck_get_type  (void);

#define GDK_CARD_DECK(obj)         GTK_CHECK_CAST (obj, \
   gdk_card_deck_get_type (), GdkCardDeck)
#define GDK_CARD_DECK_CLASS(klass) GTK_CHECK_CLASS_CAST (klass, \
   gdk_card_deck_get_type (), GdkCardDeckClass)
#define GDK_IS_CARD_DECK(obj)      GTK_CHECK_TYPE (obj, \
   gdk_card_deck_get_type ())

/* Passing NULL deck_options will return the default deck.
 * NB A GdkCardDeck uses considerable X server resources and 
 *    should be destroyed when it is no longer required. */
GtkObject* gdk_card_deck_new (GdkWindow *window, 
			      GdkCardDeckOptions deck_options);

/* Access the GdkCardDeckOptions for the deck */
GdkCardDeckOptions gdk_card_deck_get_options (GdkCardDeck* deck); 

/* Access the rendered cards in a deck:
 * NB Do not unref these pixmaps individually */ 
GdkPixmap* gdk_card_deck_face (GdkCardDeck* deck, 
			       GdkCardSuit suit, GdkCardRank rank);
GdkPixmap* gdk_card_deck_joker (GdkCardDeck* deck, GdkCardSuit suit); 
GdkPixmap* gdk_card_deck_back (GdkCardDeck* deck);
GdkBitmap* gdk_card_deck_mask (GdkCardDeck* deck);

/* This library is distributed with a collection of image files
 * (under cards/.../...) which it uses to build the card images.
 * You may add further, similar image files to these directories
 * to extend the range of decks that the library can produce... */

#endif /* __GDK_CARD_IMAGE_H */
