/* card-image.h
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

#ifndef __FREECELL_CARD_IMAGE_H
#define __FREECELL_CARD_IMAGE_H

void card_image_init (GtkWidget *w);
GdkPixmap *card_image (CARD *card);
GdkBitmap *card_image_clip (void);
int card_image_width (void);
int card_image_height (void);
int card_image_top_height (void);


#endif /* __FREECELL_CARD_IMAGE_H */
