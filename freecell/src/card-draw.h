/* card-draw.h --
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

#ifndef __FREECELL_CARD_DRAW_H
#define __FREECELL_CARD_DRAW_H

#include <gtk/gtk.h>

extern void card_draw_init(GtkWidget *widget);

extern int card_draw_get_index_from_deck(GtkWidget *w, int x, int y, DECK *d);

#define card_draw_deck(w, d) \
	card_draw_deck_general(w, 0, 0, d, 0, -1)
#define card_draw_selected_deck(w, d) \
	card_draw_deck_general(w, 0, 0, d, 1, -1)
#define card_draw_deck_with_view(w, d, i) \
	card_draw_deck_general(w, 0, 0, d, 0, i)
#define card_draw_selected_deck_with_view(w, d, i) \
	card_draw_deck_general(w, 0, 0, d, 1, i) 

extern void card_draw_deck_general(GtkWidget *widget, int x, int y, DECK *deck, int is_selected, int view);

#define card_draw_card(w, c) \
	card_draw_card_general(w, 0, 0, c, 0)
#define card_draw_selected_card(w, c) \
	card_draw_card_general(w, 0, 0, c, 1)

extern void card_draw_card_general(GtkWidget *widget, int x, int y, CARD *card, int is_selected);

int card_image_width(void);
int card_image_height(void);
#endif /* __FREECELL_CARD_DRAW_H */
