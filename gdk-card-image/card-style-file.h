/* card-style-file.h
   Copyright 2003 Free Software Foundation, Inc.

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  

   Author: Callum McKenzie

   This file contains the routines for parsing a file of card styles.
*/

#ifndef CARD_STYLE_FILE_H
#define CARD_STYLE_FILE_H

G_BEGIN_DECLS

#include <glib.h>

/* This must match OPT_NUM in gdk-card-image.h */
#define NUM_COMPONENTS 7

typedef struct _CardDeckStyle {
  gchar * name;
  gchar * components[NUM_COMPONENTS];
} CardDeckStyle;

/* This routine parses filename and returns a list of any styles found 
 * Disposal of the list is up to the caller. The data elements can be safely
 * g_free()ed. */
GList * card_style_file_parse (gchar * filename);

/* To make sorting the card styles easy. */
gint card_style_compare (gconstpointer a, gconstpointer b);

G_END_DECLS

#endif /* CARD_STYLE_FILE_H */
