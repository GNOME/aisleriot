/* Aisleriot - cscmi.h
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

#define CSCMI_C

#include "cscmi.h"


lambda_data* game_data = NULL;



/* Scheme to C functions... */
hslot_type new_slot(SCM slot_data) {
  hslot_type retval;
  retval = malloc(sizeof(slot_type));

  retval->id = SCM_INUM(SCM_CAR(slot_data));
  retval->cards = new_deck(SCM_CADR(slot_data));
  if (!strcmp("expanded", SCM_CHARS(SCM_CAR(SCM_CADDR(slot_data))))) {
    retval->type = EXPANDING_SLOT;
  }
  else if (!strcmp("expanded-right", SCM_CHARS(SCM_CAR(SCM_CADDR(slot_data))))) {
    retval->type = EXPANDING_SLOT_RIGHT;
  }
  else if (!strcmp("partially-expanded", SCM_CHARS(SCM_CAR(SCM_CADDR(slot_data))))) {
    retval->type = PARTIALLY_EXPANDING_SLOT;
    retval->expansion_depth = SCM_INUM(SCM_CADDR(SCM_CADDR(slot_data)));
  }
  else if (!strcmp("partially-expanded-right", SCM_CHARS(SCM_CAR(SCM_CADDR(slot_data))))) {
    retval->type = PARTIALLY_EXPANDING_SLOT_RIGHT;
    retval->expansion_depth = SCM_INUM(SCM_CADDR(SCM_CADDR(slot_data)));
  }
  else if (!strcmp("normal", SCM_CHARS(SCM_CAR(SCM_CADDR(slot_data))))) {
    retval->type = NORMAL_SLOT;
  }
  //if we don't recognize
  else {
	 retval->type = NORMAL_SLOT;
  }
  
  retval->x = SCM_INUM(SCM_CAR(SCM_CADR(SCM_CADDR(slot_data))));
  retval->y = SCM_INUM(SCM_CADR(SCM_CADR(SCM_CADDR(slot_data))));

  //hmmm, this is sorta messy... I assume that update_slot_length will be called soon... 
  retval->height = get_card_height();
  retval->width = get_card_width();

  return retval;
}

hcard_type new_card(SCM card_data) {
  hcard_type temp_card;
  
  temp_card = malloc(sizeof(card_type));
  temp_card->value = SCM_INUM(SCM_CAR(card_data));
  temp_card->suit = SCM_INUM(SCM_CADR(card_data));
  temp_card->direction = !(SCM_NFALSEP(SCM_CADDR(card_data)));

  return temp_card;
}

GList* new_deck(SCM deck_data) {
  SCM list_el;
  GList* temp_deck;

  temp_deck = NULL;
  if (!SCM_NFALSEP(deck_data)) {
	 return NULL;
  }
 
  if (deck_data == SCM_EOL) {
	 return NULL;
  }

  for (list_el = deck_data; list_el != SCM_EOL; list_el = SCM_CDR(list_el)) {
	 add_card(&temp_deck,new_card(SCM_CAR(list_el)));
  }
  
  return g_list_reverse(temp_deck);
}

/* C to Scheme functions... */
SCM make_card(hcard_type card) {
  SCM retval;

  if (card->direction)
	 retval = gh_cons(gh_long2scm(card->value),gh_cons(gh_long2scm(card->suit),gh_cons(SCM_BOOL_F,SCM_EOL)));
  else
	 retval = gh_cons(gh_long2scm(card->value),gh_cons(gh_long2scm(card->suit),gh_cons(SCM_BOOL_T,SCM_EOL)));
  return retval;
}

/* Scheme functions */
SCM scm_get_card_width() {
  return gh_long2scm(get_card_width());
}
SCM scm_get_card_height() {
  return gh_long2scm(get_card_height());
}
SCM scm_get_horiz_offset() {
  return gh_long2scm(get_horiz_offset());
}
SCM scm_get_vert_offset() {
  return gh_long2scm(get_vert_offset());
}
SCM scm_get_horiz_start() {
  return gh_long2scm(get_horiz_start());
}
SCM scm_get_vert_start() {
  return gh_long2scm(get_vert_start());
}

SCM scm_set_surface_layout(SCM surface) {
  SCM list_el;
  int i;
  
  if (surface == SCM_EOL)
	 return SCM_EOL;
  delete_surface();

  for (list_el = surface; list_el != SCM_EOL; list_el = SCM_CDR(list_el))
	 add_slot(new_slot(SCM_CAR(list_el)));

  for (i = g_list_length(slot_list) - 1; i >= 0; i--)
	 update_slot_length(i);
  return SCM_EOL;
}


SCM scm_reset_surface() {
  delete_surface();
  return SCM_EOL;
}

SCM scm_add_slot(SCM slot) {
  add_slot(new_slot(slot));

  update_slot_length(g_list_length(slot_list) - 1);
  return SCM_EOL;
}

SCM scm_get_slot(SCM scm_slot_id) {
  SCM cards = SCM_EOL;
  hslot_type slot = NULL;
  GList* tempcard;
  gint slot_id = gh_scm2int(scm_slot_id);

  slot = get_slot(slot_id);
  
  for (tempcard = slot->cards; tempcard; tempcard = tempcard->next) {
		cards = gh_cons(make_card(tempcard->data),cards);

  }
  return gh_cons(scm_slot_id, gh_cons(cards, SCM_EOL));

}

SCM scm_set_cards(SCM scm_slot_id, SCM new_cards) {
  gint i;
  hslot_type slot;
  GList* temptr;
  gint slot_id = gh_scm2int(scm_slot_id);
  
  slot = get_slot(slot_id);
  
  for (temptr = slot->cards; temptr; temptr = temptr->next)
	 free(temptr->data);
  
  slot->cards = new_deck(new_cards);

  for (i = g_list_length(slot_list) - 1; i >= 0; i--)
	 update_slot_length(i);

  return SCM_BOOL_T;
}

SCM scm_set_lambda(SCM start_game_lambda, 
						 SCM pressed_lambda, 
						 SCM released_lambda, 
						 SCM clicked_lambda, 
						 SCM dbl_clicked_lambda,
						 SCM game_over_lambda,
						 SCM winning_game_lambda,
						 SCM hint_lambda) {
  if (!game_data)
	 game_data = malloc(sizeof(lambda_data));
  game_data->start_game_lambda = start_game_lambda;
  game_data->button_pressed_lambda = pressed_lambda;
  game_data->button_released_lambda = released_lambda;
  game_data->button_clicked_lambda = clicked_lambda;
  game_data->button_double_clicked_lambda = dbl_clicked_lambda;
  game_data->game_over_lambda = game_over_lambda;
  game_data->winning_game_lambda = winning_game_lambda;
  game_data->hint_lambda = hint_lambda;
  return SCM_EOL;
}

SCM scm_random(SCM range) {
  return gh_long2scm(random()%SCM_INUM(range));
}
